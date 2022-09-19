#!/bin/bash -e

#mpifort -I/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/include/hdf5 a.f90 -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5hl_fortran.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5_hl.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5_fortran.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5.a -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -lm -lsz -lz -ldl -lm -Wl,-rpath -Wl,/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib

FC=mpif90
F90_OPT='-O2 -ffree-line-length-none -frecord-marker=4 -finit-local-zero -fbacktrace -fcheck=bounds' #CDA: for gfortran version < 10
F90_OPT="$F90_OPT -fallow-argument-mismatch" #CDA: for gfortran version >= 10
H5_INC="-I/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/include/hdf5"
H5_LIB="-L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5hl_fortran.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5_hl.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5_fortran.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5.a -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -lm -lsz -lz -ldl -lm -Wl,-rpath -Wl,/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib"

echo "[$0] H5_LIB=$H5_LIB"
echo "[$0] H5_INC=$H5_INC"

cmd="$FC $F90_OPT $H5_INC -cpp m_h5io.f90 mod_f90gioh5.f90 test_h5io.f90 -o a.x $H5_LIB"
echo 
echo 
echo $cmd

$cmd

rm -f *.mod *.o

./a.x SMAP_L2B_SSS_36950_20220101T005200_R18240_V5.0.h5

