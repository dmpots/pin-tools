TOP=..
PIN_TOP=../../..
PIN_INC_DIRS=-I/home/dave/pin/Include -I/home/dave/pin/InstLib -I/home/dave/pin/extras/xed2-intel64/include -I/home/dave/pin/extras/components/include -I/home/dave/pin/source/include -I/home/dave/pin/source/include/gen
CXXFLAGS=-O3 -fomit-frame-pointer -fPIC -I$(PIN_INC_DIRS) -DTARGET_IA32E -DHOST_IA32E -fPIC -DTARGET_LINUX

IMGFILT_CC=ImgFilt.cc
IMGFILT_O=${IMGFILT_CC:%.cc=%.o}
IMGFILT_H=${IMGFILT_CC:%.cc=%.h}
IMGFILT_LIB_A=lib${IMGFILT_CC:%.cc=%.a}

all: $(IMGFILT_LIB_A)

$(IMGFILT_LIB_A): $(IMGFILT_O)
	ar cr $@ $^

$(IMGFILT_O): $(IMGFILT_CC) $(IMGFILT_H)
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(IMGFILT_LIB_A) $(IMGFILT_O)
