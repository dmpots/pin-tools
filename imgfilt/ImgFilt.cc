//==--------------------------------------------------------------------------==
// This file contains functions to filter instrumentation based on the name of
// the image where the instrumentation will be inserted. It is useful for
// restricting the collection of data to only the main executable and the
// dynamic libraries related to the application.
//
//==--------------------------------------------------------------------------==
#include "ImgFilt.h"
#include <iostream>
#include <set>
#include <string>
#include <cstring>

#define dbgs()   cout
#define DEBUG(s) (s)

////////////////////////////////////////////////////////////////////////////////
// Command line paramaters
//
KNOB<BOOL> KnobAll(KNOB_MODE_WRITEONCE, "pintool",
                     "all", "0", "instrument all instructions");

KNOB<BOOL> KnobPrintFilt(KNOB_MODE_WRITEONCE, "pintool",
                         "print-filt", "0", "print filter results");

////////////////////////////////////////////////////////////////////////////////
// Private helper functions
//
namespace {
  // Types
  class ImgFilt {
  public:
    ImgFilt(const char** Allowed, int NumNames);
    bool ShouldInstrumentImageNamed(const string& imgname);
    void AllowedImage(const string& imgname);

  private:
    typedef std::set<std::string> NameSet;

    void RecordFilterStatus(const string& lib, bool Allowed);

    NameSet AllowedNamesSet;
    NameSet DeniedSet;
    NameSet AllowedSet;
  };

  // Helper functions
  bool ShouldInstrumentAddress(ADDRINT addr);
  const string basename(const string& libname);
  const string normalize(const string& libname);

  // Data
  const char* AllowedNamesList[] = {
    "libHSCabal",
    "libHSarray",
    "libHSbase",
    "libHSbin",
    "libHSbinary",
    "libHSbytestring",
    "libHScontainers",
    "libHSdeepseq",
    "libHSdirectory",
    "libHSdph",
    "libHSextensible",
    "libHSfilepath",
    "libHSghc",
    "libHShaskeline",
    "libHShoopl",
    "libHShpc",
    "libHSinteger",
    "libHSmtl",
    "libHSold",
    "libHSpretty",
    "libHSprimitive",
    "libHSprocess",
    "libHSrandom",
    "libHStemplate",
    "libHSterminfo",
    "libHStime",
    "libHSunix",
    "libHSutf8",
    "libHSvector",
    "libquantum_peak"
  };
  ImgFilt Filter(AllowedNamesList,
                 sizeof(AllowedNamesList) / sizeof(const char*));
}

////////////////////////////////////////////////////////////////////////////////
// Hobbes interface implementation
//
bool hobbes::ShouldInstrumentInstruction(INS ins) {
  if(!INS_Valid(ins)) return false;
  ADDRINT addr = INS_Address(ins);
  return ShouldInstrumentAddress(addr);
}

bool hobbes::ShouldInstrumentTrace(TRACE trace) {
  ADDRINT addr = TRACE_Address(trace);
  return ShouldInstrumentAddress(addr);
}

bool hobbes::ShouldInstrumentImage(IMG img) {
  if(KnobAll)
    return true;

  const string& name = IMG_Name(img);

  if(IMG_IsMainExecutable(img)) {
    Filter.AllowedImage(name);
    return true;
  }

  return Filter.ShouldInstrumentImageNamed(name);
}


////////////////////////////////////////////////////////////////////////////////
//  ImgFilt Implementation
//
ImgFilt::ImgFilt(const char **AllowedNames, int NumNames) {

  for(int i = 0; i < NumNames; ++i) {
    AllowedNamesSet.insert(AllowedNames[i]);
  }
}

void ImgFilt::AllowedImage(const string& imgname) {
  RecordFilterStatus(basename(imgname), true);
}

bool ImgFilt::ShouldInstrumentImageNamed(const string& imgname) {
  const string lib(basename(imgname));
  const string norm(normalize(lib));

  bool Allow = AllowedNamesSet.count(norm);
  RecordFilterStatus(lib, Allow);

  return Allow;
}

void ImgFilt::RecordFilterStatus(const string& lib, bool Allowed) {
  if(KnobPrintFilt) {
    if(Allowed) {
      std::pair<NameSet::iterator, bool> ret = AllowedSet.insert(lib);
      if(ret.second)
        cout << "Allowed " << lib << "\n";
    }
    else {
      std::pair<NameSet::iterator, bool> ret = DeniedSet.insert(lib);
      if(ret.second)
        cout << "Denied  " << lib << "\n";
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Helper Functions
//
namespace {
  bool ShouldInstrumentAddress(ADDRINT addr) {
    // Lookup the image corresponding to the address
    IMG img = IMG_FindByAddress(addr);
    if(!IMG_Valid(img)) return false;

    // Defer all questions to the image
    return hobbes::ShouldInstrumentImage(img);
  }

  const string basename(const string& libname) {
    size_t found = libname.find_last_of('/');
    if(found) {
      return libname.substr(found+1);
    }

    return libname;
  }

  const string normalize(const string& libname) {
    size_t found = libname.find_first_of(".-");
    if(found) {
      return libname.substr(0, found);
    }

    return libname;
  }
}
