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
*/

/* ===================================================================== */
/*! @file
 *  This file contains an ISA-portable PIN tool for tracing instructions
 */



#include "pin.H"
#include <iostream>
#include <fstream>
#include <sstream>
#include <set>

/* ===================================================================== */
/* Commandline Switches */
/* ===================================================================== */

KNOB<string> KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool",
    "o", "trace.out", "specify trace file name");
KNOB<BOOL>   KnobNoCompress(KNOB_MODE_WRITEONCE, "pintool",
    "no_compress", "0", "Do not compress");

/* ===================================================================== */
/* Print Help Message                                                    */
/* ===================================================================== */

INT32 Usage()
{
    cerr <<
        "This tool produces a compressed (dynamic) instruction trace.\n"
        "The trace is still in textual form but repeated sequences\n"
        "of the same code are abbreviated with a number which dramatically\n"
        "reduces the output size and the overhead of the tool.\n"
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
TLS_KEY tlog_key;
#define NUM_BUF_PAGES 1024
//      ^^ Number of OS pages for the buffer

// function prototypes
const string *lookupImageName(ADDRINT addr);

/* ===================================================================== */
/* Trace Types */
/* ===================================================================== */
typedef
enum BranchKind {
  UNKNOWN,
  DCALL,
  ICALL,
  DJUMP,
  IJUMP,
  RET,
}
BranchKind;
const char *BranchKindName(BranchKind kind) {
  switch(kind){
    case DCALL: return "DCALL  ";
    case ICALL: return "ICALL  ";
    case DJUMP: return "DJUMP  ";
    case IJUMP: return "IJUMP  ";
    case RET:   return "RET    ";
    default:    return "UNKNOWN";
  }
}

typedef
struct TraceEntry {
  ADDRINT    addr;
  BranchKind arrivedBy;
  BOOL       arrivedDirect;
  string*    arrivedFromImageNamed;
  string*    arrivedToImageNamed;
}
TraceEntry;

/* ===================================================================== */
/* TraceLog Class */
/* ===================================================================== */
class TraceLog
{
  public:
    TraceLog(THREADID tid);
    ~TraceLog();

    VOID DumpBufferToFile(TraceEntry* traces, UINT64 numElements, THREADID tid );

  private:
    ofstream _ofile;
};

TraceLog::TraceLog(THREADID tid)
{
    string filename = KnobOutputFile.Value() + "." + decstr(tid);

    _ofile.open(filename.c_str());

    if ( ! _ofile )
    {
        cerr << "Error: could not open output file." << endl;
        PIN_ExitProcess(tid);
    }
    _ofile << hex;
}

TraceLog::~TraceLog()
{
    _ofile.close();
}

VOID TraceLog::DumpBufferToFile(
                TraceEntry * entries,
                UINT64 numElements,
                THREADID tid)
{
    const string sep = " ";
    for(UINT64 i=0; i<numElements; i++, entries++)
    {
      const string *dstImageName = entries->arrivedToImageNamed;
      if (!entries->arrivedDirect) { // arrivedToImage is unknown if indirect
        dstImageName = lookupImageName(entries->addr);
      }

      // drop leading path to image name
      size_t pos = dstImageName->rfind('/');
      size_t offset = pos == string::npos ? 0 : pos+1;

      _ofile << "0x" << entries->addr
             << sep  << BranchKindName(entries->arrivedBy)
             << sep  << (dstImageName->c_str() + offset)
             << "\n";
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
    TraceLog *tlog = new TraceLog(tid);
    PIN_SetThreadData(tlog_key, tlog, tid);
}

// This routine is executed every time a thread is destroyed.
VOID ThreadFini(THREADID tid, const CONTEXT *ctxt, INT32 code, VOID *v)
{
    TraceLog *tlog = static_cast<TraceLog*>(PIN_GetThreadData(tlog_key, tid));
    delete tlog;
    PIN_SetThreadData(tlog_key, NULL, tid);
}


// This routine is executed every time a trace is created
VOID Trace(TRACE trace, VOID *v)
{
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl))
    {
        INS ins = BBL_InsTail(bbl);

        if(INS_IsBranchOrCall(ins)) {
          BOOL direct = false;
          const string *srcImgName = lookupImageName(INS_Address(ins));
          const string *dstImgName = &unknownImage;

          BranchKind kind = UNKNOWN;
          if(INS_IsDirectBranchOrCall(ins)) {
            if(INS_IsBranch(ins)) { kind = DJUMP; }
            else                  { kind = DCALL; }
            direct = true;
            ADDRINT dstAddr = INS_DirectBranchOrCallTargetAddress(ins);
            dstImgName = lookupImageName(dstAddr);
          }
          else if(INS_IsIndirectBranchOrCall(ins)) {
            if     (INS_IsBranch(ins))  { kind = IJUMP; }
            else if(INS_IsRet(ins))     { kind = RET;   }
            else                        { kind = ICALL; }
          }

          INS_InsertFillBuffer(ins, IPOINT_BEFORE, bufTypeId,
            IARG_BRANCH_TARGET_ADDR, offsetof(TraceEntry, addr),
            IARG_UINT32, kind, offsetof(TraceEntry, arrivedBy),
            IARG_BOOL, direct, offsetof(TraceEntry, arrivedDirect),
            IARG_PTR, srcImgName, offsetof(TraceEntry, arrivedFromImageNamed),
            IARG_PTR, dstImgName, offsetof(TraceEntry, arrivedToImageNamed),
            IARG_END
          );
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
    TraceEntry *traces = static_cast<TraceEntry*>(buf);
    TraceLog   *tlog   = static_cast<TraceLog*>(PIN_GetThreadData(tlog_key,tid));
    tlog->DumpBufferToFile(traces, numElements, tid);

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
                  sizeof(TraceEntry), NUM_BUF_PAGES, BufferFull, 0
                );
    if(bufTypeId == BUFFER_ID_INVALID)
    {
        cerr << "Error: could not allocate initial buffer" << endl;
        return 1;
    }

    // Initialize thread-specific data not handled by buffering api.
    tlog_key = PIN_CreateThreadDataKey(0);
    
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
