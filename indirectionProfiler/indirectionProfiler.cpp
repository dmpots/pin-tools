#include "pin.H"
#include <stdio.h>
#include <assert.h>

#include <sys/types.h>
#include <unistd.h>

#include <iostream>
#include <sstream>
#include <iomanip>
#include <fstream>

#include <map>
#include <algorithm>

extern "C" {
#include "xed-interface.h"
}

#include "indirectionProfiler.hpp"

using namespace std;

static const int debug = 0;

static ofstream AddrMapOut;
static ofstream CallLogOut;
static ofstream sqlOut;

static vector<DirectBranch*> directBranches;
static vector<IndirectBranch*> indirectBranches;


xed_category_enum_t InsToCategory(VOID *ip) {
  xed_state_t dstate;

  // The state of the machine -- required for decoding
  xed_state_zero(&dstate);
  xed_state_init(&dstate,
                 XED_MACHINE_MODE_LONG_64, 
                 XED_ADDRESS_WIDTH_64b, 
                 XED_ADDRESS_WIDTH_64b);


  // create the decoded instruction, and fill in the machine mode (dstate)
  xed_decoded_inst_t xedd;
  xed_decoded_inst_zero_set_mode(&xedd, &dstate);
        
  // call decode
  //const uint8_t* ipp = static_cast<const uint8_t*,ip>(
  xed_error_enum_t xed_error = xed_decode(&xedd, 
                                          static_cast<const uint8_t*>(ip),
                                          15);

  // check for errors
  bool okay = (xed_error == XED_ERROR_NONE);
  if (okay) {
    xed_category_enum_t category = xed_decoded_inst_get_category(&xedd);
    return category;
  }
  return XED_CATEGORY_INVALID;
}

VOID ProfileIndirectBranch(VOID *v, VOID *target) {
  IndirectBranch *b = (IndirectBranch*)v;
  b->incrTarget(target);
}

VOID ProfileDirectBranch(VOID *v) {
  DirectBranch *b = (DirectBranch*)v;
  b->incrCount();
}

VOID InstrumentInstruction(INS ins, VOID *v)
{
  ADDRINT src = INS_Address(ins);
  xed_category_enum_t category = InsToCategory((void*)src);
  // Insert a call to printip before every instruction, and pass it the IP
  if (INS_IsIndirectBranchOrCall(ins)) {
    IndirectBranch *b = new IndirectBranch(category, (void*)src);
    indirectBranches.push_back(b);
    INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR)ProfileIndirectBranch, IARG_PTR, (VOID*)b, IARG_BRANCH_TARGET_ADDR, IARG_END);
  } else if (INS_IsDirectBranchOrCall(ins)) {
    ADDRINT dest = INS_DirectBranchOrCallTargetAddress(ins);
    if (INS_IsCall(ins)) {
      DirectBranch *b = new DirectBranch(category, (void*)src, (void*)dest);
      directBranches.push_back(b);
      INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR)ProfileDirectBranch, IARG_PTR, (VOID*)b, IARG_END);
      //cerr << "DirectCallSite:  " << (void*) src << " -> " << (void*)dest << endl;
    } else if (INS_IsBranch(ins)) {
      RTN rtn = INS_Rtn(ins);
      if (!RTN_Valid(rtn) ||
          dest < RTN_Address(rtn) ||
          dest >= (RTN_Address(rtn) + RTN_Size(rtn))) {
        DirectBranch *b = new DirectBranch(category, (void*)src, (void*)dest);
        directBranches.push_back(b);
        INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR)ProfileDirectBranch, IARG_PTR, (VOID*)b, IARG_END);
        //cerr << "DirectBranchSite:  " << (void*) src << " -> " << (void*)dest << endl;
      }
    } else {
      assert(false);
    }
  } else if (INS_IsBranchOrCall(ins)) {
    assert(false);
  }
}

VOID InstrumentImage(IMG img, VOID *) {
  AddrMapOut << "Image"
             << "\t" << IMG_Name(img)
             << "\t" << (void*)IMG_LowAddress(img)
             << "\t" << (void*)IMG_HighAddress(img) << endl;
  sqlOut << "INSERT INTO ImageMap (label, low, high) VALUES("
         << "'" << IMG_Name(img) << "'"
         << ", " << "'" << SQLAddressFormat((void*)IMG_LowAddress(img)) << "'"
         << ", " << "'" << SQLAddressFormat((void*)IMG_HighAddress(img)) << "'"
         << ");" << endl;
}

VOID InstrumentRoutine(RTN rtn, VOID *v) {
  //INT32 column, line;
  //string fileName;
  //PIN_LockClient();
  //PIN_GetSourceLocation(RTN_Address(rtn), &column, &line, &fileName);
  //PIN_UnlockClient();

  AddrMapOut << "Routine"
             << "\t" << RTN_Name(rtn)
             << "\t" << (void*)RTN_Address(rtn)
             << "\t" << (void*)(RTN_Address(rtn) + RTN_Size(rtn))
    //<< "\t" << fileName << ":" << line
             << endl;
  sqlOut << "INSERT INTO RoutineMap (label, low, high) VALUES("
         << "'" << RTN_Name(rtn) << "'"
         << ", " << "'" << SQLAddressFormat((void*)RTN_Address(rtn)) << "'"
         << ", " << "'" << SQLAddressFormat((void*)(RTN_Address(rtn) + RTN_Size(rtn))) << "'"
         << ");" << endl;
}

VOID Fini(INT32 code, VOID *v)
{
  sort(directBranches.begin(), directBranches.end(), Branch::compare);
  sort(indirectBranches.begin(), indirectBranches.end(), Branch::compare);


  //   for (vector<DirectBranch*>::iterator it = directBranches.begin();
  //        it < directBranches.end();
  //        ++it) {
  //     (*it)->dump(CallLogOut);
  //   }
  //   for (vector<IndirectBranch*>::iterator it = indirectBranches.begin();
  //        it < indirectBranches.end();
  //        ++it) {
  //     (*it)->dump(CallLogOut);
  //   }

  {
    unsigned int i;
    for (i = 0 ; i < directBranches.size() ; i++) {
      DirectBranch *cur = directBranches[i];
      DirectBranch *next = (i + 1 < directBranches.size() ? directBranches[i+1] : NULL);
      if (next && next->getSource() == cur->getSource()) {
        next->merge(*cur);
      } else {
        if (!cur->empty()) {
          cur->dump(CallLogOut);
          cur->dumpSQL(sqlOut);
        }
      }
    }
  }

  {
    unsigned int i;
    for (i = 0 ; i < indirectBranches.size() ; i++) {
      IndirectBranch *cur = indirectBranches[i];
      IndirectBranch *next = (i + 1 < indirectBranches.size() ? indirectBranches[i+1] : NULL);
      if (next && next->getSource() == cur->getSource()) {
        next->merge(*cur);
      } else {
        if (!cur->empty()) {
          cur->dump(CallLogOut);
          cur->dumpSQL(sqlOut);
        }
      }
    }
  }
  

  sqlOut << "-- These commands will update the Image and Routine fields in the" << endl;
  sqlOut << "-- CallSites and CallTargets tables; the fields will be set if there" << endl;
  sqlOut << "-- is a corresponding row in the ImageMap and AddressMap tables" << endl;
  sqlOut << "UPDATE CallSites SET image = (SELECT rowid FROM ImageMap WHERE ImageMap.low <= CallSites.addr AND CallSites.addr < ImageMap.high);" << endl;
  sqlOut << "UPDATE CallSites SET routine = (SELECT rowid FROM RoutineMap WHERE RoutineMap.low <= CallSites.addr AND CallSites.addr < RoutineMap.high);" << endl;
  sqlOut << "UPDATE CallTargets SET image = (SELECT rowid FROM ImageMap WHERE ImageMap.low <= CallTargets.target AND CallTargets.target < ImageMap.high);" << endl;
  sqlOut << "UPDATE CallTargets SET routine = (SELECT rowid FROM RoutineMap WHERE RoutineMap.low <= CallTargets.target AND CallTargets.target < RoutineMap.high);  " << endl;
  
  AddrMapOut.close();
  sqlOut.close();
  CallLogOut.close();
}

int main(int argc, char * argv[])
{

  // Initialize pin
  PIN_Init(argc, argv);
  PIN_InitSymbols();

  // initialize the XED tables -- one time.
  xed_tables_init();

  {
    pid_t pid = getpid();
    stringstream ss;
    ss << pid << ".AddressMap.log";
    AddrMapOut.open(ss.str().c_str());

    ss.str("");
    ss << pid << ".CallProfile.log";
    CallLogOut.open(ss.str().c_str());

    ss.str("");
    ss << pid << ".CallProfile.sql";
    sqlOut.open(ss.str().c_str());
  }



  sqlOut << "CREATE TABLE ImageMap "
         << "(rowid INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT, low TEXT UNIQUE, high TEXT UNIQUE);"
         << endl
         << "CREATE TABLE RoutineMap "
         << "(rowid INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT, low TEXT UNIQUE, high TEXT UNIQUE);"
         << endl
         << "CREATE TABLE CallSites "
         << "(rowid INTEGER PRIMARY KEY AUTOINCREMENT, type TEXT, category TEXT, addr TEXT UNIQUE, image INTEGER, routine INTEGER);"
         << endl
         << "CREATE TABLE CallTargets"
         << "(rowid INTEGER PRIMARY KEY AUTOINCREMENT, callsite INTEGER, target TEXT, image INTEGER, routine INTEGER, count INTEGER);"
         << endl;


  // Register callback for when an image is loaded
  IMG_AddInstrumentFunction(InstrumentImage, 0);

  // Register callback for instrumenting a routine
  RTN_AddInstrumentFunction(InstrumentRoutine, 0);

  // Register InstrumentInstruction to be called to instrument instructions
  INS_AddInstrumentFunction(InstrumentInstruction, 0);

  // Register Fini to be called when the application exits
  PIN_AddFiniFunction(Fini, 0);
    
  // Start the program, never returns
  PIN_StartProgram();
    
  return 0;
}
