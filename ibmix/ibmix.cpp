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
  @AUTOR: David Peixottor
*/

#include "pin.H"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <set>
#include <map>

/* ===================================================================== */
/* Commandline Switches */
/* ===================================================================== */

KNOB<string> KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool",
    "o", "ibmix.out", "specify output file name");

/* ===================================================================== */
/* Print Help Message                                                    */
/* ===================================================================== */

INT32 Usage()
{
    cerr <<
        "This tool produces a count of the number of distinct targets\n"
        "for indirect branches, calls, and return instructions\n"
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
const char *BranchKindName(BranchKind kind) {
  switch(kind){
    case ICALL: return "ICALL  ";
    case IJUMP: return "IJUMP  ";
    case RET:   return "RET    ";
    default:    return "INVALID";
  }
}

typedef
struct JumpEntry {
  ADDRINT    source;
  ADDRINT    dest;
  BranchKind arrivedBy;
}
JumpEntry;

/* ===================================================================== */
/* JumpLog Class */
/* ===================================================================== */
class JumpLog
{
  public:
    JumpLog(THREADID tid);
    ~JumpLog();

    // Map: Source -> {Target}
    typedef map<ADDRINT, set<ADDRINT> > JumpMap;
    // Map: Taken Count -> Occurances
    typedef map<UINT64, UINT64 > Histogram;
    
    void ConsolidateEntries(JumpEntry* traces,
                            UINT64     numElements,
                            THREADID tid );
    void DumpHistograms();
    void DumpHistogram(const string& name, Histogram& hist);


  private:
    ofstream _ofile;
    JumpMap jumpmap;
    JumpMap callmap;
    JumpMap retnmap;

    void Hist(JumpMap& jumps, Histogram& hist);
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
        case ICALL: callmap[entries->source].insert(entries->dest);
        case IJUMP: jumpmap[entries->source].insert(entries->dest);
        case RET:   retnmap[entries->source].insert(entries->dest);
        case INVALID:;
      }
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
            "# dynamic_targets occurences" "\n"
            "#\n";

  Histogram hist;
  Hist(jumpmap, hist);
  DumpHistogram(string("jumps"), hist);
  
  hist.clear();
  Hist(callmap, hist);
  DumpHistogram(string("calls"), hist);

  hist.clear();
  Hist(retnmap, hist);
  DumpHistogram(string("returns"), hist);

}

void JumpLog::DumpHistogram(const string& name, Histogram& hist) {
  _ofile << "@" << name << "\n";
  
  Histogram::iterator it;
  Histogram::iterator end = hist.end();
  for(it = hist.begin(); it != end; ++it) {
    _ofile << setw(5) << setiosflags(ios::left)
           << it->first
           << " "
           << it->second
           << "\n";
  }
}

void JumpLog::Hist(JumpMap& jumps, Histogram& hist) {
  JumpMap::iterator it;
  JumpMap::iterator end = jumps.end();
  for(it = jumps.begin() ; it != end; ++it) {
    hist[it->second.size()]++;
  }
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
