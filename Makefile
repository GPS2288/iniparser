#
# iniparser Makefile
#

# Compiler settings
CC = "gcc"
FC = "gfortran"
CFLAGS  = -O2 -fPIC -Wall -ansi -pedantic
FFLAGS  = -O2 -fPIC -Wall -J src

# Ar settings to build the library
AR = "ar"
ARFLAGS = rcv

SHLD = ${CC} ${CFLAGS}
LDSHFLAGS = -shared -Wl,-Bsymbolic  -Wl,-rpath -Wl,/usr/lib -Wl,-rpath,/usr/lib
LDFLAGS = -Wl,-rpath -Wl,/usr/lib -Wl,-rpath,/usr/lib

# Set RANLIB to ranlib on systems that require it (Sun OS < 4, Mac OSX)
# RANLIB  = ranlib
RANLIB = true

RM      = rm -f

# Implicit rules

SUFFIXES = .o .c .h .a .so .sl

CSRCS = src/iniparser.c \
	   src/dictionary.c \
	   src/iniparser_wrapper.c
COBJS = $(CSRCS:.c=.o)

FSRCS = src/iniparser_wrapper.f03
FOBJS = $(FSRCS:.f03=.f.o)
FMODS = $(FSRCS:.f03=.mod)

COMPILE.c=$(CC) $(CFLAGS) -c
$(COBJS): $(CSRCS)
	@(echo "compiling $< ...")
	@($(COMPILE.c) -o $@ $<)

COMPILE.f=$(FC) $(FFLAGS) -c
$(FOBJS): $(FSRCS)
	@(echo "compiling $< ...")
	@($(COMPILE.f) -o $@ $<)

default:	libiniparser.a

all: default

libiniparser.a:	$(COBJS) $(FOBJS)
	@($(AR) $(ARFLAGS) libiniparser.a $(OBJS))
#	@($(RANLIB) libiniparser.a)

clean:
	$(RM) $(COBJS) $(FOBJS) $(FMODS)

veryclean:
	$(RM) $(OBJS) libiniparser.a libiniparser.so*
	rm -rf ./html ; mkdir html
	cd test ; $(MAKE) veryclean

docs:
	@(cd doc ; $(MAKE))
	
check:
	@(cd test ; $(MAKE))
