#  F90GIO Release v1.1
[![build_gfortran](https://github.com/cd10kfsu/F90GIO/actions/workflows/build_gfortran.yml/badge.svg)](https://github.com/cd10kfsu/F90GIO/actions/workflows/build_gfortran.yml)

Author: Da,Cheng     Email: cda@umd.edu

Fortran 90 General I/O Interface (F90GIO) provides I/O interface for 
NetCDF, HDF4, HDF5 reading/writing with Fortran 90 languages. 
It is developed in the hope of allieviating researchers' burden in 
atmospheric and oceanic science to deal with either re-analysis, 
forecast, or remote sensed data. Instead of wasting time learning 
these techniques, researchers may prefer to focus on the scientific 
problems. With this thought in mind, I decide to deliever this package 
to the public. Most of the routines have been tested both on Mac Pro 
and Linux clusters.

This README file contains information about: I. what compiler flags 
should be included when compiling NetCDF, HDF4, HDF5 libraries; II. 
how to compile F90IO library, III. how to use F90IO library.

If you find this package helpful, and you are using it in your papers, 
please acknowledge it in your acknowledgement section in your papers if 
possible. Thanks!


I. Install NetCDF/HDF4/HDF5 library

1. For installing HDF4, if you'd like to use my netcdf & hdf4 module simultenously, you need
   to disable the netcdf-I/O capability:

       --disable-netcdf

   when runnning configure. Other important flags for F90GIO includes:
       
       --enable-fortran F77=YOUR_COMPILER (e.g., F77=ifort)
   
   The general flags when you configure HDF4 library can be found in the 
   file named INSTALL in the directory ~/release_notes of the official 
   source code pakage or see this file online
(https://www.hdfgroup.org/ftp/HDF/HDF_Current/src/unpacked/release_notes/INSTALL).

2. For HDF5 compilation, you need to add flag: 

       --enable-fortran --enable-fotran2003

   other general flags when configuring the HDF5 can be found in the directory of
   ~/release_docs of the official source code package or see this file online
   (http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/release_docs/INSTALL)

II. Install F90GIO library

1. For the first step, you need to modify the compiling environment in the file:

        makefile.config

2. If you want to install the whole F90GIO library, you can go to the src/, and
   type:

        make all
        make install

   Then go to the directory test/, run
     
        make all

   you will find several .exe excutables. Run these excuatables and compare their
   results with *.results_correct

3. F90GIO also supports to build the library alone. For example, if you only want
   to build netcdf library, just to to the src/, and type:
      
        make netcdf
        make install-netcdf

   Then go to the directory test/, run

         make test-netcdf

4. Someone may also want to compile those files directly with their own files 
   instead of building libs. This is also possible. Go to the test-nolib/ and
   run:

         make test-netcdf

   as an example. You can check *.bsh files to see how to use them.


