#
# iniparser tests Makefile
#

CC      = gcc
CFLAGS  = -g -I../src
LFLAGS  = -L.. -liniparser
FC      = gfortran
FFLAGS  = -g -I../src -Wall
AR	    = ar
ARFLAGS = rcv
RM      = rm -f


default: all

all: iniexample parse parse_array parse_f

iniexample: iniexample.c
	$(CC) $(CFLAGS) $(LFLAGS) -o iniexample iniexample.c ../libiniparser.a

parse: parse.c
	$(CC) $(CFLAGS) $(LFLAGS) -o parse parse.c ../libiniparser.a
	
parse_array: parse_array.c
	$(CC) $(CFLAGS) $(LFLAGS) -o parse_array parse_array.c ../libiniparser.a
	
parse_f: parse_fortran.f03
	$(FC) $(FFLAGS) $(LFLAGS) -o parse_f parse_fortran.f03 ../libiniparser.a

clean veryclean:
	$(RM) iniexample example.ini parse

