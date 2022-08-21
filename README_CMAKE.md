## Compile with CMAKE:

"make all" in the current directory, and a directory "build" will be created here. The libs and includes will be under "build/libs" and "build/include".

config/env.sh sets the required environmental vars used by CMAKE.

To turn off specific lib, you can modify Makefile at the current directory as:

### turn off hdf
```
    mkdir build
    . config/env.sh; cd build; cmake -DBUILD_H4=off ..; make; make test
```

### turn off netcdf
```
    mkdir build
    . config/env.sh; cd build; cmake -DBUILD_NC=off ..; make; make test
```

### turn off hdf5
```
    mkdir build
    . config/env.sh; cd build; cmake -DBUILD_H5=off ..; make; make test
```

#### for hdf5 libs, if you are using hdf5 libs with major version <= 1.8
add the `H5_VERSION_1_8=ON` when running cmake

```
    mkdir build
    . config/env.sh; cd build; cmake -DBUILD_H5=on -DH5_VERSION_1_8=ON ..; make; make test
```
