module mod_f90gioh5
!$$$  module documentation block
!         .           .            .
!  programmer: da,cheng        org: fsu         date: 2014-09-18
!
!  purpose:
!    For hdf5 data operation. Note that in order to read
!    integer*1, integer*2, intger*8, the HDF5 library must be compiled
!    with the flag: --enable-fortran2003
!
!  revision history:
!    2014-09-18     da    - creator
!
!
!  subroutines included:
!
!  functions included:
!    H5_ReadVar3d_Real8          -|-----> H5_ReadVar3d
!    H5_ReadVar3d_Real4           /

!    H5_ReadVar2d_Real8          -|-----> H5_ReadVar2d
!    H5_ReadVar2d_Real4          -|
!    H5_ReadVar2d_Integer8       -|
!    H5_ReadVar2d_Integer4       -|
!    H5_ReadVar2d_Integer2        /
!
!    H5_ReadVar1d_Real8          -|-----> H5_ReadVar1d
!    H5_ReadVar1d_Real4          -|
!    H5_ReadVar1d_Integer8       -|
!    H5_ReadVar1d_Integer4        /


!  attributes: 
!    language: fortran 90
!    machine: Darwin mango.met.fsu.edu 11.4.2 
!             Darwin Kernel Version 11.4.2: Thu Aug 23 16:25:48 PDT 2012; 
!             root:xnu-1699.32.7~1/RELEASE_X86_64 x86_64
!
!
!$$$ end documentation block

! imported module
  use hdf5  ! hdf5 lib
  use iso_c_binding, only: C_PTR, C_LOC
  implicit none

! set default as public
  private
  public :: H5_ReadVar1d
  public :: H5_ReadVar2d
  public :: H5_ReadVar3d

  public :: H5_ReadVar1d_Integer4
  public :: H5_ReadVar1d_Integer8
  public :: H5_ReadVar1d_Real4
  public :: H5_ReadVar1d_Real8

  public :: H5_ReadVar2d_Integer2
  public :: H5_ReadVar2d_Integer4
  public :: H5_ReadVar2d_Integer8
  public :: H5_ReadVar2d_Real4
  public :: H5_ReadVar2d_Real8

  public :: H5_ReadVar3d_Real4
  public :: H5_ReadVar3d_Real8


  public :: h5_CreateEmptyFile
  public :: h5_WriteVar3d_Real4
  public :: h5_WriteVar3d_Real8


!-----------------------
! procedure overloading
!-----------------------

  interface H5_ReadVar3d
    module procedure H5_ReadVar3d_Real8
    module procedure H5_ReadVar3d_Real4
  endinterface


  interface H5_ReadVar2d
    module procedure H5_ReadVar2d_Real8
    module procedure H5_ReadVar2d_Real4
    module procedure H5_ReadVar2d_Integer8
    module procedure H5_ReadVar2d_Integer4
  endinterface

  interface H5_ReadVar1d
    module procedure H5_ReadVar1d_Real8
    module procedure H5_ReadVar1d_Real4
    module procedure H5_ReadVar1d_Integer8
    module procedure H5_ReadVar1d_Integer4
  endinterface



!-----------
! constants
!-----------
! kind definition
  integer(4),parameter :: i1 = 1
  integer(4),parameter :: i2 = 2
  integer(4),parameter :: i4 = 4
  integer(4),parameter :: i8 = 8
  integer(4),parameter :: i_kind = i4

  integer(4),parameter :: r4 = 4
  integer(4),parameter :: r8 = 8
  integer(4),parameter :: r_kind = r8


! rank of a matrix
  integer(i4),parameter :: RANK_ONE        = 1
  integer(i4),parameter :: RANK_TWO        = 2
  integer(i4),parameter :: RANK_THREE      = 3
  integer(i4),parameter :: RANK_FOUR       = 4
  integer(i4),parameter :: RANK_FIVE       = 5
  integer(i4),parameter :: MAX_RANKS       = RANK_FIVE
  integer(i4),parameter :: MAX_CHAR_LENGTH = 256

! status flag  
  integer(i4),parameter :: SUCCEED = 0 
  integer(i4),parameter :: FAIL    = -1


contains
function h5_createEmptyFile(fileName) result(errStatus)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_createEmptyFile
!    programmer: Cheng Da            org: umd          date: 2017-02-22
!
!  purpose:
!    create an empty file
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$   
  implicit none

  character(*),intent(in) :: fileName
  integer                 :: errStatus

  integer(HID_T) :: file_id, dspace_id, dset_id
  logical        :: lexist

  inquire(file=trim(filename),exist=lexist)
  if (lexist) then
     write(*,*) "Error: file (", trim(fileName), ") already exists."
     errStatus = FAIL
     return 
  endif
  call h5open_f(errStatus)
  call h5fcreate_f(trim(fileName),H5F_ACC_EXCL_F,file_id,errStatus)
  if (errStatus/=SUCCEED) then
     write(*,*) "Error: fail to create the empty file: ", trim(fileName)
     return
  endif
  call h5fclose_f(file_id,errStatus)
  call h5close_f(errStatus)
  
endfunction

function h5_writeVar3d_Real8(fileName, varName, varValue) result (errStatus)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_writeVar3d_Real8
!    programmer: Cheng Da            org: umd          date: 2017-02-22
!
!  purpose:
!    write 64-bit float 3d array. 
!    if a variable, identified by the <varName>, is not found in the dataset,
!    this variable with the value <varValue> is added to the file.
!    if this variable is found in the file, its values will be overwritten.
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 3d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r8),target,intent(in) :: varValue(:,:,:)
  integer                    :: errStatus

  integer(HSIZE_T) :: ndims(RANK_THREE), ndims_max(RANK_THREE)
  integer(HID_T)   :: file_id, dspace_id, dset_id
  type(c_ptr)      :: f_ptr

  call h5open_f(errStatus)
  call h5eset_auto_f(printflag=0,hdferr=errStatus) ! printon (1), printoff(0)
  call h5fopen_f(trim(fileName),H5F_ACC_RDWR_F,file_id,errStatus)
  if (errStatus/=SUCCEED) then
     write(*,*) "Error: fail to open file:", trim(fileName)
     return
  endif
  call h5dopen_f(file_id,trim(varName), dset_id, errStatus)
  if (errStatus/=SUCCEED) then
     ! create new vars in the file
     ndims = shape(varValue)
     call h5screate_simple_f(RANK_THREE, ndims, dspace_id, errStatus)
     if (errStatus/=SUCCEED) then
         write(*,*) "Error: fail to create space id for var:", trim(varName)
         return
      endif
      call h5dcreate_f(file_id,trim(varName),h5kind_to_type(r8,H5_REAL_KIND),dspace_id,dset_id,errStatus)
      if (errStatus/=SUCCEED) then
         write(*,*) "Error: fail to create dataset id for var:", trim(varName)
         return
      endif
      write(*,*) "creating Var (", trim(varName),")"
  else
      ! if already exist the var, check if dimension matches
      call h5dget_space_f(dset_id, dspace_id, errStatus)
      call h5sget_simple_extent_dims_f(dspace_id, ndims, ndims_max, errStatus)
      if (any(shape(varValue)/=ndims)) then
         write(*,*) "Error: dataset dimension mismatch for var:", trim(varName)
         write(*,*) "       shape of vars to put=", shape(varValue)
         write(*,*) "       shape of vars from file=", ndims
         return
      endif
      write(*,*) "Var (", trim(varName),") already exists. Overwrite its contents."
  endif
  f_ptr = C_LOC(varValue(1,1,1))
  call h5dwrite_f(dset_id,h5kind_to_type(r8,H5_REAL_KIND),f_ptr,errStatus)
  if (errStatus/=SUCCEED) then
     write(*,*) "Error: fail to put var value into dataspace (", trim(varName), ")"
     return
  endif
  call h5fclose_f(file_id,errStatus)
  call h5close_f(errStatus)

endfunction


function h5_writeVar3d_Real4(fileName, varName, varValue) result (errStatus)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_writeVar3d_Real4
!    programmer: Cheng Da            org: umd          date: 2017-02-22
!
!  purpose:
!    write 32-bit float 3d array. 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 3d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none

  character(*),intent(in)    :: fileName
  character(*),intent(in)    :: varName
  real(r4),target,intent(in) :: varValue(:,:,:)
  integer                    :: errStatus

  integer(HSIZE_T) :: ndims(RANK_THREE), ndims_max(RANK_THREE)
  integer(HID_T)   :: file_id, dspace_id, dset_id
  type(c_ptr)      :: f_ptr

  call h5open_f(errStatus)
  call h5eset_auto_f(printflag=0,hdferr=errStatus) ! printon (1), printoff(0)
  call h5fopen_f(trim(fileName),H5F_ACC_RDWR_F,file_id,errStatus)
  if (errStatus/=SUCCEED) then
     write(*,*) "Error: fail to open file:", trim(fileName)
     return
  endif
  call h5dopen_f(file_id,trim(varName), dset_id, errStatus)
  if (errStatus/=SUCCEED) then
     ! create new vars in the file
     ndims = shape(varValue)
     call h5screate_simple_f(RANK_THREE, ndims, dspace_id, errStatus)
     if (errStatus/=SUCCEED) then
         write(*,*) "Error: fail to create space id for var:", trim(varName)
         return
      endif
      call h5dcreate_f(file_id,trim(varName),h5kind_to_type(r4,H5_REAL_KIND),dspace_id,dset_id,errStatus)
      if (errStatus/=SUCCEED) then
         write(*,*) "Error: fail to create dataset id for var:", trim(varName)
         return
      endif
      write(*,*) "creating Var (", trim(varName),")"
  else
      ! if already exist the var, check if dimension matches
      call h5dget_space_f(dset_id, dspace_id, errStatus)
      call h5sget_simple_extent_dims_f(dspace_id, ndims, ndims_max, errStatus)
      if (any(shape(varValue)/=ndims)) then
         write(*,*) "Error: dataset dimension mismatch for var:", trim(varName)
         write(*,*) "       shape of vars to put=", shape(varValue)
         write(*,*) "       shape of vars from file=", ndims
         return
      endif
      write(*,*) "Var (", trim(varName),") already exists. Overwrite its contents."
  endif
  f_ptr = C_LOC(varValue(1,1,1))
  call h5dwrite_f(dset_id,h5kind_to_type(r4,H5_REAL_KIND),f_ptr,errStatus)
  if (errStatus/=SUCCEED) then
     write(*,*) "Error: fail to put var value into dataspace (", trim(varName), ")"
     return
  endif
  call h5fclose_f(file_id,errStatus)
  call h5close_f(errStatus)

endfunction


function H5_ReadVar3d_Real4( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar3d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 32-bit float 3d array. 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 3d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in)      :: fileName
  character(*),intent(in)      :: varName
  real(r4),allocatable, target :: varValue(:,:,:)
  integer(i_kind)              :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_THREE), varMaxDims(MAX_RANKS)
  type(C_PTR)      :: f_ptr
! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_THREE ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 3."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1),varDims(2),varDims(3)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  f_ptr = C_LOC( varValue(1,1,1) )
  call h5dread_f( var_id, H5T_NATIVE_REAL_4, f_ptr, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction


function H5_ReadVar3d_Real8( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar3d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 64-bit float 3d array. 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 3d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in)      :: fileName
  character(*),intent(in)      :: varName
  real(r8),allocatable, target :: varValue(:,:,:)
  integer(i_kind)              :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_THREE), varMaxDims(MAX_RANKS)
  type(C_PTR)      :: f_ptr
! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_THREE ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 3."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1),varDims(2),varDims(3)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  f_ptr = C_LOC( varValue(1,1,1) )
  call h5dread_f( var_id, H5T_NATIVE_REAL_8, f_ptr, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction



function H5_ReadVar2d_Real8( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar2d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 64-bit float 2d array. Note the type of H5T_NATIVE_REAL_8 is used.
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r8),allocatable    :: varValue(:,:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_TWO), varMaxDims(MAX_RANKS)


! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 2."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  call h5dread_f( var_id, H5T_NATIVE_REAL_8, varValue, varDims, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction


function H5_ReadVar2d_Real4( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar2d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 32-bit float 2d array. Note the type of H5T_NATIVE_REAL_4 is used
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  real(r4),allocatable    :: varValue(:,:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_TWO), varMaxDims(MAX_RANKS)


! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 2."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  call h5dread_f( var_id, H5T_NATIVE_REAL_4, varValue, varDims, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction



function H5_ReadVar2d_Integer4( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar2d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 32-bit signed integer 2d array. 
!    Note that the type of H5T_NATIVE_INTEGER_4 is used
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i4),allocatable :: varValue(:,:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_TWO), varMaxDims(MAX_RANKS)


! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 2."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  call h5dread_f( var_id, H5T_NATIVE_INTEGER_4, varValue, varDims, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction


function H5_ReadVar2d_Integer8( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar2d_Integer8
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 64-bit signed integer 2d array. 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i8),allocatable, target :: varValue(:,:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_TWO), varMaxDims(MAX_RANKS)
  type(C_PTR)      :: f_ptr
! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 2."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  f_ptr = C_LOC( varValue(1,1) )
  call h5dread_f( var_id, H5T_NATIVE_INTEGER_8, f_ptr, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif


! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction



function H5_ReadVar1d_Integer8( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar1d_Integer8
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 64-bit signed integer 1d array. 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i8),allocatable, target :: varValue(:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_ONE), varMaxDims(MAX_RANKS)
  type(C_PTR)      :: f_ptr
! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 1."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  f_ptr = C_LOC( varValue(1) )
  call h5dread_f( var_id, H5T_NATIVE_INTEGER_8, f_ptr, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction


function H5_ReadVar1d_Integer4( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar1d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 32-bit signed integer 1d array. 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i4),allocatable, target :: varValue(:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_ONE), varMaxDims(MAX_RANKS)
  type(C_PTR)      :: f_ptr
! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 1."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  f_ptr = C_LOC( varValue(1) )
  call h5dread_f( var_id, H5T_NATIVE_INTEGER_4, f_ptr, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif


! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction


function H5_ReadVar1d_Real4( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar1d_Real4
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 32-bit float 1d array. 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in)      :: fileName
  character(*),intent(in)      :: varName
  real(r4),allocatable, target :: varValue(:)
  integer(i_kind)              :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_ONE), varMaxDims(MAX_RANKS)
  type(C_PTR)      :: f_ptr
! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 1."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  f_ptr = C_LOC( varValue(1) )
  call h5dread_f( var_id, H5T_NATIVE_REAL_4, f_ptr, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif


! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction


function H5_ReadVar1d_Real8( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar1d_Real8
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 64-bit float 1d array. 
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 1d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in)      :: fileName
  character(*),intent(in)      :: varName
  real(r8),allocatable, target :: varValue(:)
  integer(i_kind)              :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_ONE), varMaxDims(MAX_RANKS)
  type(C_PTR)      :: f_ptr
! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_ONE ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 1."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  f_ptr = C_LOC( varValue(1) )
  call h5dread_f( var_id, H5T_NATIVE_REAL_8, f_ptr, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif


! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction


elemental function int2unit_1_2( integer1 ) result (integer2)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: int2unit_1_2
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Convert the value of 8-bit unsigned integer to the value of 8-bit
!    signed integer. The result is stored in a 16-bit integer.
!
!  revision history:
!
!  input arguments:
!    integer1       - value if stored as the 8-bit signed integer.
!                     
!
!  output arguments:
!    integer2       - value if stored as the 8-bit unsigned integer.
!$$$  
  implicit none

  integer(i1),intent(in) :: integer1
  integer(i2)            :: integer2
! local arguments
  integer(i1)        :: i, weight
  integer(i8)        :: base

  integer2 = 0
  base = 2**(BIT_SIZE(integer1) - 1)
  do i = 1, BIT_SIZE(integer1)
     weight = IBITS( integer1, BIT_SIZE(integer1)-i, 1 )
     !write(*,'(I1\)') weight  !for check
     integer2 = integer2 + weight*base 
     base = base/2
  enddo

endfunction


elemental function int2unit_1_i_kind( integer1 ) result (integer_default)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: int2unit_1_i_kind
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Convert the value of 8-bit unsigned integer to the value of 8-bit
!    signed integer. The result is stored in a (8*(i_kind))-bit integer.
!
!  revision history:
!
!  input arguments:
!    integer1       - value if stored as the 8-bit signed integer.
!                     
!
!  output arguments:
!    integer2       - value if stored as the 8-bit unsigned integer.
!$$$  
  implicit none

  integer(i1),intent(in) :: integer1
  integer(i_kind)        :: integer_default
! local arguments
  integer(i1)        :: i, weight
  integer(i8)        :: base

  integer_default = 0
  base = 2**(BIT_SIZE(integer1) - 1)
  do i = 1, BIT_SIZE(integer1)
     weight = IBITS( integer1, BIT_SIZE(integer1)-i, 1 )
     !write(*,'(I1\)') weight  !for check
     integer_default = integer_default + weight*base 
     base = base/2
  enddo

endfunction


elemental function int2unit_2_i_kind( integer2 ) result (integer_default)
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: int2unit_1_2
!    programmer: Cheng Da            org: fsu          date: 2014-09-03
!
!  purpose:
!    Convert the value of 16-bit unsigned integer to the value of 16-bit
!    signed integer. The result is stored in a (8*i_kind)-bit integer.
!
!  revision history:
!
!  input arguments:
!    integer1       - value if stored as the 16-bit signed integer.
!                     
!
!  output arguments:
!    integer2       - value if stored as the 16-bit unsigned integer.
!$$$  
  implicit none

  integer(i2),intent(in) :: integer2
  integer(i_kind)        :: integer_default
! local arguments
  integer(i1)        :: i, weight  ! max{i}=64, weight=1 or 0
  integer(i8)        :: base

  integer_default = 0
  base = 2**(BIT_SIZE(integer2) - 1)
  do i = 1, BIT_SIZE(integer2)
     weight = IBITS( integer2, BIT_SIZE(integer2)-i, 1 )
     !write(*,'(I1\)') weight  !for check
     integer_default = integer_default + weight*base 
     base = base/2
  enddo

endfunction



function H5_ReadVar2d_Integer2( fileName, varName, varValue) result ( errStatus )
!$$$ subprogram documentation block
!       .           .                  .
!  subprogram: H5_ReadVar2d_Integer4
!    programmer: Cheng Da            org: fsu          date: 2014-09-18
!
!  purpose:
!    read 32-bit signed integer 2d array. 
!    Note that the type of H5T_NATIVE_INTEGER_4 is used
!
!  revision history:
!
!  input arguments:
!    fileName      - name of the HDF5 file
!    varName       - name of the inquired variable
!    varValue      - 2d value array of the inquired variable
!
!  output arguments:
!    errStatus     - return SUCCEED (0) if read successfully, otherwise FAIL(-1)
!$$$    
  implicit none
! input arguments
  character(*),intent(in) :: fileName
  character(*),intent(in) :: varName
  integer(i4),allocatable :: varValue(:,:)
  integer(i_kind)         :: errStatus
! local arguments
  integer(HID_T)   :: file_id, var_id, var_space_id 
  integer(i_kind)  :: varRank          
  integer(HSIZE_T) :: varDims(RANK_TWO), varMaxDims(MAX_RANKS)


! initialize fortran interface
  call h5open_f( errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to initialize FORTRAN interface"
     return
  endif

! open file
  call h5fopen_f( TRIM(fileName), H5F_ACC_RDONLY_F, file_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the file: ", TRIM(fileName)
     return
  endif

! open dataset
  call h5dopen_f( file_id, TRIM(varName), var_id, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to open the dataset: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get dataspace id
  call h5dget_space_f( var_id, var_space_id, errStatus )

! get the rank of the data
  call h5sget_simple_extent_ndims_f( var_space_id, varRank, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the rank of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif
  if ( varRank .ne. RANK_TWO ) then
     print*, "Error: the rank of the var (", TRIM(varName), ") is not 2."
     errStatus = FAIL
     return 
  endif

! get the dimension of the data
  call h5sget_simple_extent_dims_f( var_space_id, varDims, varMaxDims, &
                                    errStatus )
  allocate( varValue(varDims(1),varDims(2)), stat=errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to allocate space for the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! get the value 
  call h5dread_f( var_id, H5T_NATIVE_INTEGER_4, varValue, varDims, errStatus )
  if ( errStatus .ne. SUCCEED ) then
     print*, "Error: fail to get the value of the var: ", TRIM(varName)
     print*, "       of the file: ", TRIM(fileName)
     return
  endif

! close the dataset
  call h5dclose_f( var_id, errStatus )

! close the dataspace
  call h5sclose_f( var_space_id, errStatus )

! close the file
  call h5fclose_f( file_id, errStatus )

! close fortran interface
  call h5close_f( errStatus )

endfunction


endmodule
