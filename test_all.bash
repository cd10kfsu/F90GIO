#!/bin/bash


echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
echo "1. test NetCDF interface"
echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
/bin/bash use_netcdf.bsh 
./test1_netcdf.exe

echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
echo "2. test HDF4 interface"
echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
/bin/bash use_hdf4.bsh 
./test2_hdf4.exe

echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
echo "3. test HDF5 interface"
echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
/bin/bash use_hdf5.bsh 
./test3_hdf5.exe

rm *.exe *.o *.mod
