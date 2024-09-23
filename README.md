#  F90GIO Release v2.0
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7729468.svg)](https://doi.org/10.5281/zenodo.7729468)
[![GitHub Clones](https://img.shields.io/badge/dynamic/json?color=blue&label=Unique%20clone%20since%2009/13/2023&query=uniques&url=https://gist.githubusercontent.com/cd10kfsu/10a9cdaa0d1d14b12e1f58bcef5e56c3/raw/clone.json&logo=github)](https://github.com/MShawon/github-clone-count-badge)
[![build_gfortran](https://github.com/cd10kfsu/F90GIO/actions/workflows/build_gfortran.yml/badge.svg)](https://github.com/cd10kfsu/F90GIO/actions/workflows/build_gfortran.yml)

**Online community**: [![Discord](https://img.shields.io/discord/1129502879477141528?logo=Discord)](https://discord.gg/FdFFUy9Zcc)

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


## Major changes
1. Add fast I/O libs for NetCDF and H5 files: `ncio` and `h5io` libs
   remove lots of checks in F90GIO, and reduce the times of open/close
   files.


## Install NetCDF/HDF4/HDF5 library

1. For installing HDF4, if you'd like to use my netcdf & hdf4 module 
   simultenously, you need to disable the netcdf-I/O capability:
   ```
       --disable-netcdf
   ```
   when runnning configure. Other important flags for F90GIO includes:
   ```    
       --enable-fortran F77=YOUR_COMPILER (e.g., F77=ifort)
   ```
   The general flags when you configure HDF4 library can be found in the 
   file named INSTALL in the directory `~/release_notes` of the official 
   source code pakage or see this file online
(https://www.hdfgroup.org/ftp/HDF/HDF_Current/src/unpacked/release_notes/INSTALL).

2. For HDF5 compilation, you need to add flag: 
   ```
       --enable-fortran --enable-fotran2003
   ```
   other general flags when configuring the HDF5 can be found in the 
   directory of `~/release_docs` of the official source code package or 
   see this file online
   (http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/release_docs/INSTALL)

## Install F90GIO library

"make all" in the current directory, and a directory "build" will be created here. The libs and includes will be under "build/libs" and "build/include".

config/env.sh sets the required environmental vars used by CMAKE.

1. To turn off specific lib, you can modify Makefile at the current directory as:
   - turn off hdf4
     ```
     mkdir build
     . config/env.sh; cd build; cmake -DBUILD_H4=off ..; make; make test
     ```

   - turn off netcdf
     ```
     mkdir build
     . config/env.sh; cd build; cmake -DBUILD_NC=off ..; make; make test
     ```

   - turn off hdf5 by
     ```
     mkdir build
     . config/env.sh; cd build; cmake -DBUILD_H5=off ..; make; make test
     ```

2. for hdf5 libs, if you are using hdf5 libs with major version <= 1.8
   add the `H5_VERSION_1_8=ON` when running cmake
    ```
    mkdir build
    . config/env.sh; cd build; cmake -DBUILD_H5=on -DH5_VERSION_1_8=ON ..; make; make test
    ```

3. install Fast I/O libs (currently support NC and HDF5)
   ```
   mkdir build
   . config/env.sh; cd build
   cmake .. -DBUILD_NC=ON -DBUILD_H5=on -DH5_VERSION_1_8=ON -DBUILD_FAST_IO=ON
   make; make test
   ```
4. Most common combination for Mac ARM (enable hdf5 (version>1.8) & nc, disable hdf4)
   ```
   . ../config/env.gnu.mac-arm.sh  &&  cmake .. -DBUILD_NC=ON -DBUILD_H4=OFF -DBUILD_H5=ON -DH5_VERSION_1_8=OFF -DBUILD_FAST_IO=ON && make && make test
   ```
