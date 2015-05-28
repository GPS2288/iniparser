#
# iniparser Makefile
#
# Compiler settings
CC      ?= gcc
FC      ?= gfortran
CFLAGS	?= -O2
CFLAGS  += -fPIC -Wall -ansi -std=c99 -pedantic
FFLAGS  ?= -O2 -fPIC -Wall -J src -std=f2003

# Ar settings to build the library
AR	    ?= ar
ARFLAGS = rcv

SHLD = ${CC} ${CFLAGS}
LDSHFLAGS = -shared -Wl,-Bsymbolic
LDFLAGS += -Wl,-rpath -Wl,/usr/lib -Wl,-rpath,/usr/lib

# Set RANLIB to ranlib on systems that require it (Sun OS < 4, Mac OSX)
# RANLIB  = ranlib
RANLIB = true

RM      ?= rm -f

# Implicit rules

SUFFIXES = .o .c .h .a .so .sl

CSRCS = src/dictionary.c \
		src/iniparser.c \
		src/iniparser_wrapper_helper.c
COBJS = $(CSRCS:.c=.o)

FSRCS = src/iniparser_wrapper.f03
FOBJS = $(FSRCS:.f03=.o)
FMODS = $(FSRCS:.f03=.mod)

COMPILE.c	?= $(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c
	
ifndef V
QUIET_AR	= @echo "AR	$@";
QUIET_CC	= @echo "CC	$@";
QUIET_FC    = @echo "FC $@";
QUIET_LINK	= @echo "LINK	$@";
QUIET_RANLIB	= @echo "RANLIB	$@";
endif

.c.o:
	$(QUIET_CC)$(COMPILE.c) $(OUTPUT_OPTION) $<

COMPILE.f	?= $(FC) $(FFLAGS) $(TARGET_ARCH) -c
%.o: %.f03
	$(QUIET_FC)$(COMPILE.f) $(OUTPUT_OPTION) $<

default:	libiniparser.a

all: default

libiniparser.a:	$(COBJS) $(FOBJS)
	$(QUIET_AR)$(AR) $(ARFLAGS) libiniparser.a $(COBJS) $(FOBJS)
	$(QUIET_RANLIB)$(RANLIB) $@

libiniparser.so:	$(OBJS)
	$(QUIET_LINK)$(SHLD) $(LDSHFLAGS) $(LDFLAGS) -o $@.0 $(OBJS) \
		-Wl,-soname=`basename $@`.0

clean:
	$(RM) $(COBJS) $(FOBJS) $(FMODS)

veryclean:
	$(RM) $(OBJS) libiniparser.a libiniparser.so*
	rm -rf ./html ; mkdir html
	cd test ; $(MAKE) veryclean

docs:
	@("$(MAKE)" -C doc/)
	
check: default
	@(cd test ; $(MAKE))

.PHONY: default clean veryclean docs check
