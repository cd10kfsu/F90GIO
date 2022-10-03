program test_h4
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
  Use mod_f90gio
  Implicit None

  Character(256)         :: file_name
  Character(256)         :: var_name
  Integer(4),allocatable :: var_val(:), var2_val(:)
  Character(256)         :: attr_name
  Character(256)         :: attr_val,  attr2_val
  
  Integer(4)             :: nx
  Integer(4)             :: ix
  Integer(4)             :: ios
  logical                :: lpass(2) = .true.

  !call get_command_argument(1,file_name)
  file_name="testdata_h4.hdf"
  Write(*,*) "file_name=",trim(file_name)

  var_name  = "longitude"
  nx = 10
  Allocate( var_val(nx) )
  Do ix = 1, nx
    var_val(ix) = 36*ix
  Enddo

  attr_name = "unit"
  attr_val  = "degreeNorth"

! write data into hdf4 file 
  ios = H4_CreateEmptyFile( TRIM(file_name) )
  ios = H4_WriteSDS1d( TRIM(file_name), TRIM(var_name), var_val )  
  ios = H4_WriteSDSAttr1d( TRIM(file_name),TRIM(var_name), &
                       TRIM(attr_name), TRIM(attr_val) )

! get data info from hdf4 file
  Call H4_InquireSDSInfo( TRIM(file_name), TRIM(var_name) )

! get data from hdf4 file
  ios = H4_ReadSDS1d( TRIM(file_name), TRIM(var_name), var2_val )
  ios = H4_ReadSDSAttr1d( TRIM(file_name), TRIM(var_name), &
                          TRIM(attr_name), attr2_val )

  if (any(var_val/=var2_val)) lpass(1) = .false.
  if (any(shape(var_val)/=shape(var2_val))) lpass(2) = .false.

  open(unit=10, iostat=ios, file=trim(file_name), status='old')
  if (ios==0) close(10,status="delete")
  
  if (any(.not.lpass)) then

  Write(*,*) "Original data--------------------------"
  Write(*,*) "varname = ", TRIM(var_name)
  Write(*,*) "size of var = ", nx
  Write(*,*) "var values:"
  Write(*,"(20(I4))") ( var_val(ix), ix = 1, nx )
  Write(*,*) "Attribute: ", TRIM(attr_name), " = ", TRIM(attr_val)

  Write(*,*) "Read from HDF4 file--------------------"
  Write(*,*) "varname = ", TRIM(var_name)
  Write(*,*) "size of var = ", SHAPE(var2_val) 
  Write(*,*) "var values:"
  Write(*,"(20(I4))") ( var2_val(ix), ix = 1, nx )
  Write(*,*) "Attribute: ", TRIM(attr_name), " = ", TRIM(attr2_val)
  
  else

  Write(*,*) "Test passed"

  endif


  Deallocate( var_val, var2_val )


endprogram

