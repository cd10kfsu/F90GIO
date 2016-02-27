#  F90GIO Release v1.0
Author: Da,Cheng     Email: cda@umd.edu

Fortran 90 General I/O Interface (F90GIO) provides I/O interface for 
Binary, NetCDF, HDF4, HDF5 reading/writing by using Fortran 90
languages. It is developed in the hope of allieviating researchers'
pressure in atmospheric and oceanic science to deal with either re-
analysis, forecast, or remote sensed data. Instead of learning these
techniques, people may want to focus on the science problems. instead
of technical tricks. With this thought in mind, I decide to deliever 
this package to the public. Most of the routines have been tested 
both on Mac Pro and Linux clusters.

This README file contains information about: I. what compiler flags 
should be included when compiling NetCDF, HDF4, HDF5 libraries; II. 
how to compile F90IO library, III. how to use F90IO library.

I. Install NetCDF/HDF4/HDF5 library

1. If you'd like to use my netcdf & hdf4 module simultenously, you need
   to disable the netcdf-I/O capability (--disable-netcdf) when 
   runnning configure. Other important flags for F90GIO includes:
   --enable-fortran F77=YOUR_COMPILER (e.g., F77=ifort). The general
   flags when you configure HDF4 library can be found in the file named
   INSTALL in the directory ~/release_notes of the official source code 
   pakage or see this file online
(https://www.hdfgroup.org/ftp/HDF/HDF_Current/src/unpacked/release_notes/INSTALL).

2. For HDF5 module, you need to add flag: --enable-fortran --enable-fotran2003
   other general flags when configuring the HDF5 can be found in the directory of
   ~/release_docs of the official source code package or see this file online
   (http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/release_docs/INSTALL)

