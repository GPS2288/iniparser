#
# iniparser tests Makefile
#

CC      = gcc
CFLAGS  = -g -I../src
LFLAGS  = -L.. -liniparser
FC      = gfortran
FFLAGS  = -g -I../src -Wall -std=f2003
AR	    = ar
ARFLAGS = rcv
RM      = rm -f


default: all

all: iniexample parse parse_array parse_fortran

iniexample: iniexample.c
	$(CC) $(CFLAGS) -o iniexample iniexample.c $(LFLAGS)

parse: parse.c
	$(CC) $(CFLAGS) -o parse parse.c $(LFLAGS)
	
parse_array: parse_array.c
	$(CC) $(CFLAGS) -o parse_array parse_array.c $(LFLAGS)
	
parse_fortran: parse_fortran.f03
	$(FC) $(FFLAGS) -o parse_fortran parse_fortran.f03 $(LFLAGS)

clean veryclean:
	$(RM) iniexample example.ini parse

