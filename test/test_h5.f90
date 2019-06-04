program test_h5
!$$$  program documentation block
!         .           .            .
!    programmer: da,cheng        org: umd      date: 2015-Jun-01
!
!  purpose:
!    Test whether Module_HDF4 can work correctly.
!
!  revision history:
!    2015-Jun-01     da    - creator
!
!  file dependencies:
!
!  attributes: 
!    language: fortran 90
!    machine : 
!
!
!$$$ end documentation block
  Use mod_f90gioh5
  Implicit None

  Character(256)         :: var_name
  Integer(4),allocatable :: var_val(:,:), var2_val(:,:)
  
  Integer(4)             :: nx, ny
  Integer(4)             :: ix
  Integer(4)             :: ios

  logical :: lpass(2) = .true.


  var_name  = "latitude"
  nx = 19
  ny = 1
  Allocate( var_val(nx,ny) )
  Do ix = 1, nx
    var_val(ix,ny) = -90 + 10*(ix-1)
  Enddo


! get data from hdf4 file
  ios = H5_ReadVar2d( "hdf5_testdata.h5", TRIM(var_name), var2_val )
! 
  if ( any(var_val/=var2_val) ) lpass(1) = .false.
  if ( any(shape(var_val)/=shape(var2_val)) ) lpass(2) = .false.

  if (any(.not.lpass)) then
  
  Write(*,*) "Original data--------------------------"
  Write(*,*) "varname = ", TRIM(var_name)
  Write(*,*) "size of var = ", nx, ny
  Write(*,*) "var values:"
  Write(*,"(20(I4))") ( var_val(ix,ny), ix = 1, nx )

  Write(*,*) "Read from HDF5 file--------------------"
  Write(*,*) "varname = ", TRIM(var_name)
  Write(*,*) "size of var = ", SHAPE(var2_val) 
  Write(*,*) "var values:"
  Write(*,"(20(I4))") ( var2_val(ix,ny), ix = 1, nx )

  else

  Write(*,*) "Test passed"

  endif

  Deallocate( var_val, var2_val )


endprogram

