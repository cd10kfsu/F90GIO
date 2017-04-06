include ../src/makefile.config

F90GIO_CFLAGS=-I$(F90GIO_INCLUDE_DIR)
F90GIO_NC_LFLAGS=-L$(F90GIO_LIB_DIR) -lf90gionc
F90GIO_H4_LFLAGS=-L$(F90GIO_LIB_DIR) -lf90gioh4
F90GIO_H5_LFLAGS=-L$(F90GIO_LIB_DIR) -lf90gioh5
F90GIO_ALL_LFLAGS=-L$(F90GIO_LIB_DIR) -lf90gio $(NC_LFLAGS) $(H4_LFLAGS) $(H5_LFLAGS)

all: test-netcdf test-hdf4 test-hdf5 test-f90gio

test-netcdf: test_nc.exe

test-hdf4: test_h4.exe

test-hdf5: test_h5.exe

test-f90gio: test_nc_g.exe test_h4_g.exe test_h5_g.exe

.PHONY: clean

clean:
	rm *.exe *.o *.hdf

test_nc.exe: test_nc.o
	$(FC) $< -o $@  $(F90GIO_NC_LFLAGS)  $(NC_LFLAGS)

test_h4.exe: test_h4.o
	$(FC) $< -o $@  $(F90GIO_H4_LFLAGS) $(H4_LFLAGS)

test_h5.exe: test_h5.o
	$(FC) $< -o $@  $(F90GIO_H5_LFLAGS) $(H5_LFLAGS)

test_nc.o: test_nc.f90
	$(FC) -c $(FC_CFLAGS) $(F90GIO_CFLAGS) $<

test_h4.o: test_h4.f90
	$(FC) -c $(FC_CFLAGS) $(F90GIO_CFLAGS) $<

test_h5.o: test_h5.f90
	$(FC) -c $(FC_CFLAGS) $(F90GIO_CFLAGS) $<


test_nc_g.exe: test_nc_g.o
	$(FC) $< -o $@ $(F90GIO_ALL_LFLAGS)

test_h4_g.exe: test_h4_g.o
	$(FC) $< -o $@ $(F90GIO_ALL_LFLAGS)

test_h5_g.exe: test_h5_g.o
	$(FC) $< -o $@ $(F90GIO_ALL_LFLAGS)

test_nc_g.o: test_nc_g.f90
	$(FC) -c $(FC_CFLAGS) $(F90GIO_CFLAGS) $<

test_h4_g.o: test_h4_g.f90
	$(FC) -c $(FC_CFLAGS) $(F90GIO_CFLAGS) $<

test_h5_g.o: test_h5_g.f90
	$(FC) -c $(FC_CFLAGS) $(F90GIO_CFLAGS) $<


