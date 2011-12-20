#ifndef __PIN_TOOLS_IMGFILT_IMGFILT_H
#define __PIN_TOOLS_IMGFILT_IMGFILT_H

#include "pin.H"
namespace hobbes {
 bool ShouldInstrumentInstruction(INS);
 bool ShouldInstrumentTrace(TRACE);
 bool ShouldInstrumentImage(IMG);
}
#endif
