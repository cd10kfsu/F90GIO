include ./makefile.config

#all: netcdf hdf5 f90gio hdf4
all: netcdf hdf5 f90gio 

netcdf: libf90gionc.a

hdf4: libf90gioh4.a

hdf5: libf90gioh5.a

f90gio: libf90gio.a

.PHONY: install install-netcdf install-hdf4 install-hdf5 clean

#install: install-netcdf install-hdf4 install-hdf5 install-f90gio
install: install-netcdf install-hdf5 install-f90gio

install-netcdf:
	mv libf90gionc.a $(F90GIO_LIB_DIR)
	mv mod_f90gionc.mod $(F90GIO_INCLUDE_DIR)

install-hdf4:
	mv libf90gioh4.a $(F90GIO_LIB_DIR)
	mv mod_f90gioh4.mod $(F90GIO_INCLUDE_DIR)

install-hdf5:
	mv libf90gioh5.a $(F90GIO_LIB_DIR)
	mv mod_f90gioh5.mod $(F90GIO_INCLUDE_DIR)

install-f90gio:
	mv libf90gio.a $(F90GIO_LIB_DIR)
	mv *.mod $(F90GIO_INCLUDE_DIR)

clean:
	rm *.o
	rm $(F90GIO_LIB_DIR)/*.a 
	rm $(F90GIO_INCLUDE_DIR)/*.mod

libf90gionc.a: mod_f90gionc.o
	$(ARCHIVER) $(ARCHIEVER_FLAGS) $@ $<

libf90gioh4.a: mod_f90gioh4.o
	$(ARCHIVER) $(ARCHIEVER_FLAGS) $@ $<

libf90gioh5.a: mod_f90gioh5.o
	$(ARCHIVER) $(ARCHIEVER_FLAGS) $@ $<

#libf90gio.a: mod_f90gionc.o mod_f90gioh4.o mod_f90gioh5.o mod_f90gio.o
libf90gio.a: mod_f90gionc.o mod_f90gioh5.o mod_f90gio.o
	$(ARCHIVER) $(ARCHIEVER_FLAGS) $@ $^

mod_f90gio.o: mod_f90gio.f90 
	$(FC) -c $(FC_CFLAGS) $(NC_CFLAGS) $(H4_CFLAGS) $(H5_CFLAGS) $<

mod_f90gionc.o: mod_f90gionc.f90
	$(FC) -c $(FC_CFLAGS) $(NC_CFLAGS) $<

mod_f90gioh4.o: mod_f90gioh4.f90
	$(FC) -c $(FC_CFLAGS) $(H4_CFLAGS) $<

mod_f90gioh5.o: mod_f90gioh5.f90
	$(FC) -c $(FC_CFLAGS) $(H5_CFLAGS) $<

	
