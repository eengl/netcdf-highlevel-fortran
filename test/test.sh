#!/bin/sh

# Build
gfortran -g -fbacktrace -o test.x test.f90 -I../ -I/usr/local/include ../libnetcdf_hl.a -L/usr/local/lib -lnetcdf -lnetcdff

# Run.  Input is a NetCDF file.
./test.x $1
