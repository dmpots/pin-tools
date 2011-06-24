/*BEGIN_LEGAL 
Intel Open Source License 

Copyright (c) 2002-2010 Intel Corporation. All rights reserved.
 
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.  Redistributions
in binary form must reproduce the above copyright notice, this list of
conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.  Neither the name of
the Intel Corporation nor the names of its contributors may be used to
endorse or promote products derived from this software without
specific prior written permission.
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE INTEL OR
ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
END_LEGAL */

/* ===================================================================== */
/*
  @ORIGINAL_AUTHOR: Robert Muth
  @AUTOR: David Peixotto
*/

#include "pin.H"
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <set>
#include <map>
#include <stddef.h>

#include <boost/tr1/unordered_map.hpp>
#include <boost/tr1/unordered_set.hpp>
#include <boost/circular_buffer.hpp>
using namespace std::tr1;

/* ===================================================================== */
/* Commandline Switches */
/* ===================================================================== */

KNOB<string> KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool",
    "o", "ibdetails.out", "specify output file name");

KNOB<UINT32> KnobCutoff(KNOB_MODE_WRITEONCE, "pintool",
    "c", "5", "cutoff for displaying branch targets");

KNOB<UINT32> KnobLocalitySize(KNOB_MODE_WRITEONCE, "pintool",
    "b", "5", "buffer size for locality measurement");

/* ===================================================================== */
/* Print Help Message                                                    */
/* ===================================================================== */

INT32 Usage()
{
    cerr <<
        "This tool produces a count of the number of distinct targets\n"
        "for indirect branches, calls, and return instructions\n"
        "and gives the source and targets for branches when the\n"
        "number of targets is above -c <cutoff>\n"
        "\n";
    cerr << KNOB_BASE::StringKnobSummary();
    cerr << endl;
    return -1;
}

/* ===================================================================== */
/* Global Variables */
/* ===================================================================== */

// image name data
set<string> imageNames;
string unknownImage = "<UNKNOWN>";

// buffer data
BUFFER_ID bufTypeId;
TLS_KEY jlog_key;
#define NUM_BUF_PAGES 1024
//      ^^ Number of OS pages for the buffer

// function prototypes
const string *lookupImageName(ADDRINT addr);

/* ===================================================================== */
/* Trace Types */
/* ===================================================================== */
typedef
enum BranchKind {
  INVALID,
  ICALL,
  IJUMP,
  RET,
}
BranchKind;

typedef
struct JumpEntry {
  ADDRINT    source;
  ADDRINT    dest;
  BranchKind arrivedBy;
}
JumpEntry;

struct JumpRecord {
  typedef unordered_set<ADDRINT> AddrSet;
  typedef boost::circular_buffer<ADDRINT> LocalitySet;
  UINT64       hits;
  UINT64       misses;
  AddrSet targets;
  LocalitySet locality;


  JumpRecord() : hits(0), misses(0), targets(), locality(KnobLocalitySize.Value()) { }

  void insert(ADDRINT target) {
    targets.insert(target);
    hits++;

    if(find(locality.begin(), locality.end(), target) == locality.end()) {
      misses++;
    }
    locality.push_back(target);
  }

  private:
  JumpRecord(const JumpRecord&);
  JumpRecord& operator=(const JumpRecord&);
};

class HistRecord {
  public:
  typedef set<ADDRINT> AddrSet;

  HistRecord() : hits_(0), misses_(0), sources_(), targets_() { }


  void Update(ADDRINT source, 
              const JumpRecord::AddrSet& targets, 
              UINT64 hits, 
              UINT64 misses) {
    sources_.insert(source);
    targets_.insert(targets.begin(), targets.end());
    hits_   += hits;
    misses_ += misses;
  }
  
  double MissRatio() {
    return static_cast<double>(misses_) / static_cast<double>(hits_);
  }

  UINT64 hits()  {return hits_;}
  UINT64 misses() {return misses_;}
  const AddrSet& sources() {return sources_;}
  const AddrSet& targets() {return targets_;}

  private:
  UINT64  hits_;
  UINT64  misses_;
  AddrSet sources_;
  AddrSet targets_;

  HistRecord(const HistRecord&);
  HistRecord& operator=(const HistRecord&);
};

/* ===================================================================== */
/* JumpLog Class */
/* ===================================================================== */
class JumpLog
{
  public:
    JumpLog(THREADID tid);
    ~JumpLog();

    // Map: Source -> {Target}
    typedef unordered_map<ADDRINT, JumpRecord* > JumpMap;
    // Map: Taken Count -> Occurances
    typedef map<UINT64, HistRecord* > Histogram;
    
    void ConsolidateEntries(JumpEntry* traces,
                            UINT64     numElements,
                            THREADID tid );
    void inline InsertEntry(JumpMap& map, ADDRINT src, ADDRINT dest);
    void DumpHistograms();
    void DumpHistogram(const string& name, const Histogram& hist, bool weighted);
    void Hist(const JumpMap& jumps, Histogram* hist, bool weighted);
    void ClearHist(Histogram*);


  private:
    ofstream _ofile;
    JumpMap jumpmap;
    JumpMap callmap;
    JumpMap retnmap;
};

JumpLog::JumpLog(THREADID tid)
{
    string filename = KnobOutputFile.Value();
    string tidStr    = decstr(tid);
    size_t len = filename.length();

    // Special case for files that end in ".LOG".
    // For these files we change the filename to end in ".tid.LOG".
    // We do this to make it easy for the other scripts we wrote to gather
    // all the log files.
    if(len >= 4 && filename[len-4] == '.'
                && filename[len-3] == 'L'
                && filename[len-2] == 'O'
                && filename[len-1] == 'G')
    {
      filename.insert(len-4, "." + tidStr);
    }
    else
    {
      filename.append(tidStr);
    }

    _ofile.open(filename.c_str());

    if ( ! _ofile )
    {
        cerr << "Error: could not open output file." << endl;
        PIN_ExitProcess(tid);
    }
    _ofile << setw(12);
}

JumpLog::~JumpLog()
{
    _ofile.close();
}

VOID JumpLog::ConsolidateEntries(
                JumpEntry *entries,
                UINT64 numElements,
                THREADID tid)
{
    for(UINT64 i=0; i<numElements; i++, entries++)
    {
      switch(entries->arrivedBy) {
        case ICALL: 
          InsertEntry(callmap, entries->source, entries->dest);
          break;
        case IJUMP: 
          InsertEntry(jumpmap, entries->source, entries->dest);
          break;
        case RET:   
          InsertEntry(retnmap, entries->source, entries->dest);
          break;
        case INVALID:;
      }
    }
}

VOID inline JumpLog::InsertEntry(JumpMap& map, ADDRINT src, ADDRINT dest)
{
  JumpMap::iterator it = map.find(src);
  if (it == map.end()) {
    JumpRecord *record = new JumpRecord();
    record->insert(dest);
    map.insert(pair<ADDRINT, JumpRecord*>(src, record));
  }
  else {
    it->second->insert(dest);
  }
}

/* */
void JumpLog::DumpHistograms() {
  string sep = " ";
  _ofile << "# Indirect Branch Measurements\n"
            "# Counts the number of dynamic targets for each\n"
            "#    indirect branch\n"
            "#    indirect call\n"
            "#    return       \n"
            "# Output is a histogram counting how many times that\n"
            "# number of distinct targets occurs.\n"
            "# Output format:\n"
            "#\n"
            "# dynamic_targets occurrences" "\n"
            "#\n";

  Histogram hist;
  for(int weighted = 0; weighted <= 1; weighted++) {
    ClearHist(&hist);
    Hist(jumpmap, &hist, weighted);
    DumpHistogram(string("jumps"), hist, weighted);
    
    ClearHist(&hist);
    Hist(callmap, &hist, weighted);
    DumpHistogram(string("calls"), hist, weighted);

    ClearHist(&hist);
    Hist(retnmap, &hist, weighted);
    DumpHistogram(string("returns"), hist, weighted);
  }

}

void JumpLog::DumpHistogram(const string& name, const Histogram& hist, bool weighted) {
  _ofile << "@" << name;
  if (weighted) {_ofile << "-weighted";}
  _ofile << "\n";
  

  // Copy to a sorted map for pretty output
  Histogram::const_iterator hit;
  Histogram::const_iterator end = hist.end();

  for(hit = hist.begin(); hit != end; ++hit) {
    UINT64 num_targets = hit->first;
    HistRecord *hr = hit->second;

    _ofile << setw(5) << setiosflags(ios::left)
           << num_targets
           << "|"
           << setw(20)
           << hr->hits();

    _ofile << "|"
           << hr->MissRatio();


    _ofile << "|";
    if(num_targets >= KnobCutoff.Value()) {
        HistRecord::AddrSet::iterator sit = hr->sources().begin();
        HistRecord::AddrSet::iterator sit_end = hr->sources().end();
        for(; sit != sit_end; ++sit) {
          _ofile << "0x" << setw(12) << hex << *sit << " ";
        }
        _ofile << dec;
    }

    _ofile << "|";
    if(num_targets >= KnobCutoff.Value()) {
        HistRecord::AddrSet::iterator sit = hr->targets().begin();
        HistRecord::AddrSet::iterator sit_end = hr->targets().end();
        for(; sit != sit_end; ++sit) {
          _ofile << "0x" << setw(12) << hex << *sit << " ";
        }
        _ofile << dec;
    }

    _ofile << "\n";
  }
}

void JumpLog::Hist(const JumpMap& jumps, Histogram* hist, bool weighted) {
  JumpMap::const_iterator it;
  JumpMap::const_iterator end = jumps.end();
  for(it = jumps.begin() ; it != end; ++it) {
    ADDRINT target     = it->first;
    JumpRecord *jr     = it->second;
    UINT64 num_targets = jr->targets.size();
    
    UINT64 hits = 1;
    if(weighted) {
      hits = jr->hits;
    }

    // insert new entry if needed
    Histogram::iterator hit = hist->find(num_targets);
    HistRecord *entry;
    if(hit == hist->end()) {
      entry = new HistRecord();
      hist->insert(make_pair(num_targets, entry));
    }
    else {
      entry = hit->second;
    }

    // update entry
    entry->Update(target, jr->targets, hits, jr->misses);
  }
}

void JumpLog::ClearHist(Histogram* hist) {
  Histogram::iterator hit = hist->begin();
  Histogram::iterator end = hist->end();
  for(; hit != end; ++hit) {
    delete hit->second;
  }
  hist->clear();
}


/* ===================================================================== */
/* Utility Functions */
/* ===================================================================== */
const string *lookupImageName(ADDRINT addr) {
    const string *imgName = &unknownImage;

    IMG img = IMG_FindByAddress(addr);
    if(IMG_Valid(img)) {
      string name = IMG_Name(img);
      set<string>::iterator it = imageNames.find(name);
      if(it == imageNames.end()) {
        imgName = new string(name);
        imageNames.insert(*imgName);
      }
      else {
        imgName = &(*it);
      }
    }

    return imgName;
}

/* ===================================================================== */
/* Instrumentation Functions */
/* ===================================================================== */

// This routine is executed every time a thread is created.
VOID ThreadStart(THREADID tid, CONTEXT *ctxt, INT32 flags, VOID *v)
{
    JumpLog *jlog = new JumpLog(tid);
    PIN_SetThreadData(jlog_key, jlog, tid);
}

// This routine is executed every time a thread is destroyed.
VOID ThreadFini(THREADID tid, const CONTEXT *ctxt, INT32 code, VOID *v)
{
    JumpLog *jlog = static_cast<JumpLog*>(PIN_GetThreadData(jlog_key, tid));
    jlog->DumpHistograms();
    delete jlog;
    PIN_SetThreadData(jlog_key, NULL, tid);
}


// This routine is executed every time a trace is created
VOID Trace(TRACE trace, VOID *v)
{
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl))
    {
        INS ins = BBL_InsTail(bbl);

        if(INS_Valid(ins)) {
        if(INS_IsBranchOrCall(ins)) {
          BranchKind kind = INVALID;
          if(INS_IsIndirectBranchOrCall(ins)) {
            if     (INS_IsBranch(ins))  { kind = IJUMP; }
            else if(INS_IsRet(ins))     { kind = RET;   }
            else                        { kind = ICALL; }
          }

          INS_InsertFillBuffer(ins, IPOINT_BEFORE, bufTypeId,
            IARG_ADDRINT, INS_Address(ins), offsetof(JumpEntry, source),
            IARG_BRANCH_TARGET_ADDR, offsetof(JumpEntry, dest),
            IARG_UINT32, kind, offsetof(JumpEntry, arrivedBy),
            IARG_END
          );
        }
        }
    }
}

// This routine is executed when the application completes
VOID Fini(INT32 code, VOID *v)
{
}

// This routine is executed when the buffer fills up
VOID * BufferFull(BUFFER_ID id, THREADID tid, const CONTEXT *ctxt, VOID *buf,
                  UINT64 numElements, VOID *v)
{
    JumpEntry *entries = static_cast<JumpEntry*>(buf);
    JumpLog   *jlog    = static_cast<JumpLog*>(PIN_GetThreadData(jlog_key,tid));
    jlog->ConsolidateEntries(entries, numElements, tid);

    return buf;
}


/* ===================================================================== */
/* Main                                                                  */
/* ===================================================================== */

int  main(int argc, char *argv[])
{
    
    if( PIN_Init(argc,argv) )
    {
        return Usage();
    }
    
    // Initialize buffering data
    bufTypeId = PIN_DefineTraceBuffer(
                  sizeof(JumpEntry), NUM_BUF_PAGES, BufferFull, 0
                );
    if(bufTypeId == BUFFER_ID_INVALID)
    {
        cerr << "Error: could not allocate initial buffer" << endl;
        return 1;
    }

    // Initialize thread-specific data not handled by buffering api.
    jlog_key = PIN_CreateThreadDataKey(0);
    
    TRACE_AddInstrumentFunction(Trace, 0);
    PIN_AddThreadStartFunction(ThreadStart, 0);
    PIN_AddThreadFiniFunction(ThreadFini, 0);
    PIN_AddFiniFunction(Fini, 0);

    // Never returns
    PIN_StartProgram();
    
    return 0;
}

/* ===================================================================== */
/* eof */
/* ===================================================================== */
