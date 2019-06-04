#Compile with CMAKE:

"make all" in the current directory, and a directory "build" will be created here. The libs and includes will be under "build/libs" and "build/include".

config/env.sh sets the required environmental vars used by CMAKE.

To turn off specific lib, you can modify Makefile at the current directory as:

# turn off hdf
all:
    mkdir build
    . config/env.sh; cd build; cmake -DBUILD_H4=off ..; make; make test

# turn off netcdf
all:
    mkdir build
    . config/env.sh; cd build; cmake -DBUILD_NC=off ..; make; make test

# turn off hdf5
all:
    mkdir build
    . config/env.sh; cd build; cmake -DBUILD_H5=off ..; make; make test
