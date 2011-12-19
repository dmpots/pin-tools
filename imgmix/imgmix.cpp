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
/*! @file This file contains an PIN tool for collecting all the set of images
 *  (executables + dynamic libraries) used during exection of a program.
 */



#include "pin.H"
#include <iostream>
#include <fstream>
#include <sstream>
#include <set>
#include <stddef.h>

/* ===================================================================== */
/* Commandline Switches */
/* ===================================================================== */

KNOB<string> KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool",
    "o", "imgmix.out", "specify trace file name");

/* ===================================================================== */
/* Print Help Message                                                    */
/* ===================================================================== */

INT32 Usage()
{
    cerr <<
      "This tool produces a list of images used by the program.\n";

    cerr << KNOB_BASE::StringKnobSummary();

    cerr << endl;

    return -1;
}

/* ===================================================================== */
/* Global Variables */
/* ===================================================================== */

// image name data
typedef set<string> NameSet;
NameSet ImageNames;
string UnknownImage = "<UNKNOWN>";

// function prototypes
const string *lookupImageName(ADDRINT addr);
bool  allowImageInstrumentation(const string* imageName);

/* ===================================================================== */
/* Utility Functions */
/* ===================================================================== */
const string *lookupImageName(ADDRINT addr) {
    const string *ImgName = &UnknownImage;

    IMG img = IMG_FindByAddress(addr);
    if(IMG_Valid(img)) {
      string name = IMG_Name(img);
      set<string>::iterator it = ImageNames.find(name);
      if(it == ImageNames.end()) {
        ImgName = new string(name);
        ImageNames.insert(*ImgName);
      }
      else {
        ImgName = &(*it);
      }
    }

    return ImgName;
}

bool allowImageInstrumentation(const string* imageName) {
  return true;
}

// This routine is executed when the application completes
static std::ofstream* out = 0;
VOID Fini(INT32 code, VOID *v)
{
  for(NameSet::iterator I = ImageNames.begin(), E = ImageNames.end(); I!=E; ++I) {
    *out << *I << "\n";
  }
  out->close();
}

// This routine is executed when a new instruction is emitted to the code cache
VOID Instruction(INS ins, void *v) {
  const string* ImgName = lookupImageName(INS_Address(ins));
  if(allowImageInstrumentation(ImgName)) {
  }
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

    string filename =  KnobOutputFile.Value();
    out = new std::ofstream(filename.c_str());
    
    INS_AddInstrumentFunction(Instruction, 0);
    PIN_AddFiniFunction(Fini, 0);

    // Never returns
    PIN_StartProgram();
    
    return 0;
}

/* ===================================================================== */
/* eof */
/* ===================================================================== */
