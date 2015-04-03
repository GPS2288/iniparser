#
# iniparser Makefile
#
# Compiler settings
<<<<<<< HEAD
CC = "gcc"
FC = "gfortran"
CFLAGS  = -O2 -fPIC -Wall -ansi -pedantic
FFLAGS  = -O2 -fPIC -Wall -J src -std=f2003

# Ar settings to build the library
AR = "ar"
=======
CC      ?= gcc
CFLAGS	?= -O2
CFLAGS  += -fPIC -Wall -ansi -std=c99 -pedantic

# Ar settings to build the library
AR	    ?= ar
>>>>>>> master
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

<<<<<<< HEAD
CSRCS = src/dictionary.c \
		src/iniparser.c \
		src/iniparser_wrapper_helper.c
COBJS = $(CSRCS:.c=.o)

FSRCS = src/iniparser_wrapper.f03
FOBJS = $(FSRCS:.f03=.o)
FMODS = $(FSRCS:.f03=.mod)

COMPILE.c=$(CC) $(CFLAGS) -c
%.o: %.c
	@(echo "compiling $< ...")
	@($(COMPILE.c) -o $@ $<)
=======
COMPILE.c	?= $(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c

ifndef V
QUIET_AR	= @echo "AR	$@";
QUIET_CC	= @echo "CC	$@";
QUIET_LINK	= @echo "LINK	$@";
QUIET_RANLIB	= @echo "RANLIB	$@";
endif

.c.o:
	$(QUIET_CC)$(COMPILE.c) $(OUTPUT_OPTION) $<
>>>>>>> master

COMPILE.f=$(FC) $(FFLAGS) -c
%.o: %.f03
	@(echo "compiling $< ...")
	@($(COMPILE.f) -o $@ $<)

default:	libiniparser.a

<<<<<<< HEAD
all: default

libiniparser.a:	$(COBJS) $(FOBJS)
	@($(AR) $(ARFLAGS) libiniparser.a $(COBJS) $(FOBJS))
#	@($(RANLIB) libiniparser.a)
=======
libiniparser.a:	$(OBJS)
	$(QUIET_AR)$(AR) $(ARFLAGS) $@ $^
	$(QUIET_RANLIB)$(RANLIB) $@

libiniparser.so:	$(OBJS)
	$(QUIET_LINK)$(SHLD) $(LDSHFLAGS) $(LDFLAGS) -o $@.0 $(OBJS) \
		-Wl,-soname=`basename $@`.0
>>>>>>> master

clean:
	$(RM) $(COBJS) $(FOBJS) $(FMODS)

veryclean:
	$(RM) $(OBJS) libiniparser.a libiniparser.so*
	rm -rf ./html ; mkdir html
	cd test ; $(MAKE) veryclean

docs:
	@("$(MAKE)" -C doc/)
	
<<<<<<< HEAD
check:
	@("$(MAKE)" -C test/)
=======
check: default
	@(cd test ; $(MAKE))

.PHONY: default clean veryclean docs check
>>>>>>> master
