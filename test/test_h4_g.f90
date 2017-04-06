program test_h5_g
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

  Character(256)         :: var_name
  Integer(4),allocatable :: var_val(:), var2_val(:)
  Character(256)         :: attr_name
  Character(256)         :: attr_val,  attr2_val
  
  Integer(4)             :: nx
  Integer(4)             :: ix
  Integer(4)             :: ios


  var_name  = "longitude"
  nx = 10
  Allocate( var_val(nx) )
  Do ix = 1, nx
    var_val(ix) = 36*ix
  Enddo

  attr_name = "unit"
  attr_val  = "degreeNorth"

! write data into hdf4 file 
  ios = H4_CreateEmptyFile( "hdf4_testdata.hdf" )
  ios = H4_WriteSDS1d( "hdf4_testdata.hdf", TRIM(var_name), var_val )  
  ios = H4_WriteSDSAttr1d( "hdf4_testdata.hdf",TRIM(var_name), &
                       TRIM(attr_name), TRIM(attr_val) )

! get data info from hdf4 file
  Call H4_InquireSDSInfo( "hdf4_testdata.hdf", TRIM(var_name) )

! get data from hdf4 file
  ios = H4_ReadSDS1d( "hdf4_testdata.hdf", TRIM(var_name), var2_val )
  ios = H4_ReadSDSAttr1d( "hdf4_testdata.hdf", TRIM(var_name), &
                          TRIM(attr_name), attr2_val )

! 
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


  Deallocate( var_val, var2_val )


endprogram

