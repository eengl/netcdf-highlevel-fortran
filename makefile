PREFIX := /usr/local

FC := gfortran

NETCDF_INC := /usr/local/include
NETCDF_LIB := -L/usr/local/lib -lnetcdf -lnetcdff

ifeq ($(FC),gfortran)
	FFLAGS ?= -O3 -g -fbacktrace -fPIC
endif
ifeq ($(FC),ifort)
	FFLAGS ?= -O3 -g -traceback -fPIC
endif

.PHONY:	all clean cleanall install install-docs test

all:	netcdf_hl.mod libnetcdf_hl.a libnetcdf_hl.so

netcdf_hl.mod:	netcdf_hl.f90
	$(FC) $(FFLAGS) -c -I$(NETCDF_INC) $^

libnetcdf_hl.a:	netcdf_hl.o
	$(AR) -ruv $@ $^

libnetcdf_hl.so:	netcdf_hl.o
	$(FC) -I. $(FFLAGS) -shared -o $@ $^ $(NETCDF_LIB)

test:
	@$(FC) -I./ $(FFLAGS) -o test/test_shared.x test/test.f90 -L./ -lfnetcdf_hl
	@$(FC) -I./ $(FFLAGS) -o test/test_static.x test/test.f90 ./libnetcdf_hl.a
	@test/test.sh shared
	@test/test.sh static


install:
	-install -d -v -m 755 $(PREFIX)/include
	-install -d -v -m 755 $(PREFIX)/lib
	-install -d -v -m 755 $(PREFIX)/share/man/man3
	-install -v -m 755 netcdf_hl.mod $(PREFIX)/include/netcdf_hl.mod
	-install -v -m 755 libnetcdf_hl.a $(PREFIX)/lib/libnetcdf_hl.a
	-install -v -m 755 libnetcdf_hl.so $(PREFIX)/lib/libnetcdf_hl.so

install-docs:
	-install -v -m 755 docs/man/man3/netcdf_hl.3 $(PREFIX)/share/man/man3/netcdf_hl.3

clean:
	-rm -f netcdf_hl.mod netcdf_hl.o libnetcdf_hl.a libnetcdf_hl.so

cleanall:	clean
	-rm -f test/test.x
