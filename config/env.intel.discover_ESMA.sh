##/bin/bash -xe

# config for Netcdf
export NC_INCLUDE="$BASEDIR/Linux/include/netcdf"
export NC_LIBS=`$BASEDIR/Linux/bin/nf-config --flibs`

# config for HDF4
export H4_INCLUDE="$BASEDIR/Linux/include/hdf"
export H4_LIBS=`$BASEDIR/Linux/bin/h4fc-hdf4 -show |cut -d " " -f2-`

# config for HDF5
export H5_INCLUDE="$BASEDIR/Linux/include/hdf5"
export H5_LIBS=`$BASEDIR/Linux/bin/h5pfc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`


echo "====================================="
echo "      F90GIO configurations:"
echo
echo "NC_INCLUDE=$NC_INCLUDE"
echo "NC_LIBS=$NC_LIBS"

echo "H4_INCLUDE=$H4_INCLUDE"
echo "H4_LIBS=$H4_LIBS"

echo "H5_INCLUDE=$H5_INCLUDE"
echo "H5_LIBS=$H5_LIBS"
