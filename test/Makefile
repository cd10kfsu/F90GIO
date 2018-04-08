include ../src/makefile.config

F90GIO_CFLAGS=-I$(F90GIO_INCLUDE_DIR)
F90GIO_NC_LFLAGS=-L$(F90GIO_LIB_DIR) -lf90gionc
F90GIO_H4_LFLAGS=-L$(F90GIO_LIB_DIR) -lf90gioh4
F90GIO_H5_LFLAGS=-L$(F90GIO_LIB_DIR) -lf90gioh5
F90GIO_ALL_LFLAGS=-L$(F90GIO_LIB_DIR) -lf90gio $(NC_LFLAGS) $(H4_LFLAGS) $(H5_LFLAGS)

all: test-netcdf test-hdf4 test-hdf5 test-f90gio
#all: test-netcdf test-hdf5 test-f90gio

test-netcdf: test_nc.exe
	./$< > nc.results
	diff nc.results_correct nc.results && echo "$@ ....... PASSED" || echo "$@ ....... FAILED"
	rm -f nc.results


test-hdf4: test_h4.exe
	./$< > h4.results
	diff h4.results_correct h4.results && echo "$@ ....... PASSED" || echo "$@ ....... FAILED"
	rm -f h4.results
	rm -f *.hdf

test-hdf5: test_h5.exe
	./$< > h5.results
	diff h5.results_correct h5.results && echo "$@ ....... PASSED" || echo "$@ ....... FAILED"
	rm -f h5.results

test-f90gio: test_nc_g.exe test_h4_g.exe test_h5_g.exe
#test-f90gio: test_nc_g.exe test_h5_g.exe
	./test_nc_g.exe > nc.results
	diff nc.results_correct nc.results && echo "$@:netcdf ....... PASSED" || echo "$@:netcdf ....... FAILED" 
	rm -f nc.results
	./test_h4_g.exe > h4.results
	diff h4.results_correct h4.results && echo "$@:hdf4 ....... PASSED" || echo "$@:hdf4 ....... FAILED" 
	rm -f h4.results
	./test_h5_g.exe > h5.results 
	diff h5.results_correct h5.results && echo "$@:hdf5 ....... PASSED" || echo "$@:hdf5 ....... FAILED"
	rm -f h5.results

#test-f90gio: test_nc_g.exe test_h5_g.exe
#	./test_nc_g.exe > nc.results
#	diff nc.results_correct nc.results && echo "$@:netcdf ....... PASSED" || echo "$@:netcdf ....... FAILED"
#	rm -f nc.results
#	./test_h5_g.exe > h5.results
#	diff h5.results_correct h5.results && echo "$@:hdf5 ....... PASSED" || echo "$@:hdf5 ....... FAILED"
#	rm -f h5.results

.PHONY: clean

clean:
	rm -f *.exe *.o *.hdf

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


