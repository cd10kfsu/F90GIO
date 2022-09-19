#!/bin/bash -xe

FC=mpif90
F90_OPT='-O2 -ffree-line-length-none -frecord-marker=4 -finit-local-zero -fbacktrace -fcheck=bounds' #CDA: for gfortran version < 10
F90_OPT="$F90_OPT -fallow-argument-mismatch" #CDA: for gfortran version >= 10
NETCDF_LIB=`nf-config --flibs`
NETCDF_INC=`nf-config --fflags`
echo "[$0] NETCDF_LIB=$NETCDF_LIB"
echo "[$0] NETCDF_INC=$NETCDF_INC"

$FC $F90_OPT $NETCDF_INC m_ncio.f90 test_ncio.f90 -o a.x $NETCDF_LIB

./a.x 20120101022144-NODC-L3C_GHRSST-SSTskin-AVHRR_Pathfinder-PFV5.2_NOAA19_G_2012001_night-v02.0-fv01.0.nc
