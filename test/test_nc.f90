program test_nc
!$$$  program documentation block
!         .           .            .
!    programmer: da,cheng        org: umd      date: 2015-Jun-01
!
!  purpose:
!    Test whether Module_NetCDF can work correctly.
!    The output should be:
!      0  1  2  3  4  5  6  7  8  9 10 11
!     12 13 14 15 16 17 18 19 20 21 22 23
!     24 25 26 27 28 29 30 31 32 33 34 35
!     36 37 38 39 40 41 42 43 44 45 46 47
!     48 49 50 51 52 53 54 55 56 57 58 59
!     60 61 62 63 64 65 66 67 68 69 70 71
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
  Use mod_f90gionc
  Implicit None

  Character(256)         :: var_name 
  Integer(4)             :: nx, ny
  Integer(4),allocatable :: i4_2d(:,:), var_2d(:,:)
  Integer(4)             :: ix, iy
  Logical                :: lpass(4) = .true.

! These information can be dumped by using command ncdump 
  nx = 6
  ny = 12
  var_name = "data"

! Dimension order shown by the ncdump is in the reverse
! order as in the program
  Allocate( i4_2d(ny,nx), &
            var_2d(ny,nx) )

! Internal vars, data read from nc file should be the same
! as this one 
  Write(*,*) "Original data-----------------------"
  Do ix = 1, nx
     Do iy = 1, ny
        var_2d(iy,ix) = iy - 1 + ny*(ix-1) 
     Enddo
     Write(*,"(100(I3))") ( var_2d(iy,ix),iy=1,ny)
  Enddo
  
! Use the high interface, it selects low-level routines
! based on the kinds of i4_2d
  Call NC_ReadVar2d("netcdf_testdata.nc", TRIM(var_name), i4_2d )
  Write(*,*) "Got by NC_ReadVar2d------------------"
  Do ix = 1, nx
     Write(*,"(100(I3))") ( i4_2d(iy,ix),iy=1,ny)
  Enddo

  if (any(var_2d/=i4_2d)) lpass(1) = .false.
  if (any(shape(var_2d)/=shape(i4_2d))) lpass(2) = .false.


! Or you can select the low-level routines to read data
  Call NC_ReadVar2d_Integer4("netcdf_testdata.nc", TRIM(var_name), i4_2d )
  Write(*,*) "Got by NC_ReadVar2d_Integer4---------"
  Do ix = 1, nx
     Write(*,"(100(I3))") ( i4_2d(iy,ix),iy=1,ny)
  Enddo

  if (any(var_2d/=i4_2d)) lpass(3) = .false.
  if (any(shape(var_2d)/=shape(i4_2d))) lpass(4) = .false.

  if (.not.any(lpass)) then
     write(*,*) "Test failed"
  else
     write(*,*) "Test passed"
  endif

  Deallocate( i4_2d )
  

endprogram

