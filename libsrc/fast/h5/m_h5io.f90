MODULE m_h5io
  USE hdf5
  USE iso_c_binding, ONLY: c_ptr, c_loc, c_associated
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: HID_T ! type for all different ids used for H5
  PUBLIC :: h5_get_fid, h5_close_fid

! read att
  PUBLIC :: h5_rdatt_i1, h5_rdatt_i2, h5_rdatt_i4, &
            h5_rdatt_r4, h5_rdatt_r8

! read vars
  PUBLIC :: h5_rdvar1d, h5_rdvar2d, h5_rdvar3d, h5_rdvar4d

! low-level Read 1d
  PUBLIC :: h5_rdvar1d_i1, h5_rdvar1d_i2, h5_rdvar1d_i4, &
            h5_rdvar1d_r4, h5_rdvar1d_r8

! low-level Read 2d
  PUBLIC :: h5_rdvar2d_i1, h5_rdvar2d_i2, h5_rdvar2d_i4, &
            h5_rdvar2d_r4, h5_rdvar2d_r8

! low-level Read 3d
  PUBLIC :: h5_rdvar3d_i1, h5_rdvar3d_i2, h5_rdvar3d_i4, &
            h5_rdvar3d_r4, h5_rdvar3d_r8

! low-level Read 4d
  PUBLIC :: h5_rdvar4d_i1, h5_rdvar4d_i2, h5_rdvar4d_i4, &
            h5_rdvar4d_r4, h5_rdvar4d_r8

!-------------------------------------------------------------------------------
! Internal vars & subs
  INTERFACE h5_rdatt1d
    MODULE PROCEDURE h5_rdatt_i1, h5_rdatt_i2, h5_rdatt_i4, &
                     h5_rdatt_r4, h5_rdatt_r8
  END INTERFACE

  INTERFACE h5_rdvar1d
    MODULE PROCEDURE h5_rdvar1d_i1, h5_rdvar1d_i2, h5_rdvar1d_i4, &
                     h5_rdvar1d_r4, h5_rdvar1d_r8
  END INTERFACE

  INTERFACE h5_rdvar2d
    MODULE PROCEDURE h5_rdvar2d_i1, h5_rdvar2d_i2, h5_rdvar2d_i4, &
                     h5_rdvar2d_r4, h5_rdvar2d_r8
  END INTERFACE

  INTERFACE h5_rdvar3d
    MODULE PROCEDURE h5_rdvar3d_i1, h5_rdvar3d_i2, h5_rdvar3d_i4, &
                     h5_rdvar3d_r4, h5_rdvar3d_r8
  END INTERFACE

  INTERFACE h5_rdvar4d
    MODULE PROCEDURE h5_rdvar4d_i1, h5_rdvar4d_i2, h5_rdvar4d_i4, &
                     h5_rdvar4d_r4, h5_rdvar4d_r8
  END INTERFACE

  INTEGER,PARAMETER :: i1 = 1
  INTEGER,PARAMETER :: i2 = 2
  INTEGER,PARAMETER :: i4 = 4
  INTEGER,PARAMETER :: i8 = 8
  INTEGER,PARAMETER :: r4 = 4
  INTEGER,PARAMETER :: r8 = 8

  INTEGER,SAVE,PRIVATE :: lout_log = 6 ! output log

! status flag
  integer(i4),parameter :: SUCCEED = 0
  integer(i4),parameter :: FAIL    = -1

  TYPE,PRIVATE::t_h5_errcode
    ! i2 supports -128 -> 127
    INTEGER(i1) :: fid    = 1
    INTEGER(i1) :: varid  = 2
    INTEGER(i1) :: varval = 3
    INTEGER(i1) :: attid  = 10
    INTEGER(i1) :: attval = 4
    INTEGER(i1) :: dimid  = 5
    INTEGER(i1) :: dimval = 6
    INTEGER(i1) :: ftnapi = 7
    INTEGER(i1) :: varrank = 8
    INTEGER(i1) :: get_h5type = 9
    INTEGER(i1) :: undef  = 127 ! max positive for signed 8-byte int
  END TYPE
  TYPE(t_h5_errcode),SAVE,PRIVATE:: errcode

  PRIVATE :: mystop

CONTAINS


!--------------------------------------------------------------------------------
! Utils
!--------------------------------------------------------------------------------
SUBROUTINE mystop(errcode)
  IMPLICIT NONE
  INTEGER(i1),INTENT(IN) :: errcode
! change this part to MPI_Abort if using MPI
  STOP (INT(errcode,i4))
END SUBROUTINE

!--------------------------------------------------------------------------------
! open/close h5 file
!--------------------------------------------------------------------------------
SUBROUTINE h5_get_fid(filename, fid)
  IMPLICIT NONE

  CHARACTER(*),INTENT(IN)  :: filename
  INTEGER(HID_T), INTENT(OUT) :: fid
  INTEGER(i4) :: istat

! initialize fortran interface
  call h5open_f(istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_get_fid: fail to open fortran interface"
     call mystop(errcode%ftnapi)
  end if

! get file id
  call h5fopen_f(trim(filename), H5F_ACC_RDONLY_F, fid, istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_get_fid: fail to get file id of file:",trim(filename)
     call mystop(errcode%fid)
  end if

END SUBROUTINE h5_get_fid


SUBROUTINE h5_close_fid(fid)
  IMPLICIT NONE

  INTEGER(HID_T),INTENT(IN) :: fid
  INTEGER(i4) :: istat

! close file
  call h5fclose_f(fid, istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_close_fid: fail to close file with fid=", fid
     call mystop(errcode%fid)
  end if

! close fortran interface
  call h5close_f(istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_close_fid: fail to close fortran interface"
     call mystop(errcode%ftnapi)
  end if

END SUBROUTINE h5_close_fid

!--------------------------------------------------------------------------------
! read 1D
!--------------------------------------------------------------------------------
SUBROUTINE h5_rdvar1d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i1), TARGET, INTENT(OUT) :: varval(:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar1d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i2), TARGET, INTENT(OUT) :: varval(:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar1d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i4), TARGET, INTENT(OUT) :: varval(:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar1d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  REAL(r4), TARGET, INTENT(OUT) :: varval(:)
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar1d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  REAL(r8), TARGET, INTENT(OUT) :: varval(:)
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE


!--------------------------------------------------------------------------------
! read 2D
!--------------------------------------------------------------------------------
SUBROUTINE h5_rdvar2d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i1), TARGET, INTENT(OUT) :: varval(:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar2d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i2), TARGET, INTENT(OUT) :: varval(:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar2d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i4), TARGET, INTENT(OUT) :: varval(:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar2d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  REAL(r4), TARGET, INTENT(OUT) :: varval(:,:)
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar2d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  REAL(r8), TARGET, INTENT(OUT) :: varval(:,:)
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

!--------------------------------------------------------------------------------
! read 3D
!--------------------------------------------------------------------------------
SUBROUTINE h5_rdvar3d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i1), TARGET, INTENT(OUT) :: varval(:,:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar3d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i2), TARGET, INTENT(OUT) :: varval(:,:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar3d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i4), TARGET, INTENT(OUT) :: varval(:,:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar3d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  REAL(r4), TARGET, INTENT(OUT) :: varval(:,:,:)
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar3d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  REAL(r8), TARGET, INTENT(OUT) :: varval(:,:,:)
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

!--------------------------------------------------------------------------------
! read 4D
!--------------------------------------------------------------------------------
SUBROUTINE h5_rdvar4d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i1), TARGET, INTENT(OUT) :: varval(:,:,:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar4d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i2), TARGET, INTENT(OUT) :: varval(:,:,:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar4d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  INTEGER(i4), TARGET, INTENT(OUT) :: varval(:,:,:,:)
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar4d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  REAL(r4), TARGET, INTENT(OUT) :: varval(:,:,:,:)
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

SUBROUTINE h5_rdvar4d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(HID_T),   INTENT(IN)  :: fid
  CHARACTER(*),     INTENT(IN)  :: varname
  REAL(r8), TARGET, INTENT(OUT) :: varval(:,:,:,:)
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdvar.f90.inc"
END SUBROUTINE

!--------------------------------------------------------------------------------
! read attribute
!--------------------------------------------------------------------------------
SUBROUTINE h5_rdatt_i1(fid, varname, attname, attval)
  IMPLICIT NONE
  INTEGER(HID_T),    INTENT(IN) :: fid
  CHARACTER(*),      INTENT(IN) :: varname, attname
  INTEGER(i1),TARGET,INTENT(OUT) :: attval
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdatt.f90.inc"
END SUBROUTINE 

SUBROUTINE h5_rdatt_i2(fid, varname, attname, attval)
  IMPLICIT NONE
  INTEGER(HID_T),    INTENT(IN) :: fid
  CHARACTER(*),      INTENT(IN) :: varname, attname
  INTEGER(i2),TARGET,INTENT(OUT) :: attval
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdatt.f90.inc"
END SUBROUTINE 

SUBROUTINE h5_rdatt_i4(fid, varname, attname, attval)
  IMPLICIT NONE
  INTEGER(HID_T),    INTENT(IN) :: fid
  CHARACTER(*),      INTENT(IN) :: varname, attname
  INTEGER(i4),TARGET,INTENT(OUT) :: attval
  INTEGER(i4) :: h5io_kind = H5_INTEGER_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdatt.f90.inc"
END SUBROUTINE 

SUBROUTINE h5_rdatt_r4(fid, varname, attname, attval)
  IMPLICIT NONE
  INTEGER(HID_T),    INTENT(IN) :: fid
  CHARACTER(*),      INTENT(IN) :: varname, attname
  REAL(r4),   TARGET,INTENT(OUT) :: attval
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdatt.f90.inc"
END SUBROUTINE 

SUBROUTINE h5_rdatt_r8(fid, varname, attname, attval)
  IMPLICIT NONE
  INTEGER(HID_T),    INTENT(IN) :: fid
  CHARACTER(*),      INTENT(IN) :: varname, attname
  REAL(r8),   TARGET,INTENT(OUT) :: attval
  INTEGER(i4) :: h5io_kind = H5_REAL_KIND     ! h5kind_typ_type requires INTEGER(4) as input
  include "h5_rdatt.f90.inc"
END SUBROUTINE 

!--------------------------------------------------------------------------------
! utils 
!--------------------------------------------------------------------------------
! Returns either H5_REAL_KIND or H5_INTEGER_KIND
!
! reference:
! https://stackoverflow.com/questions/2560182/determining-variable-type-in-fortran
!
FUNCTION get_h5type(ptr) RESULT(h5type)
  IMPLICIT NONE
  CLASS(*),POINTER,INTENT(IN) :: ptr
  INTEGER(i4) :: h5type

  select type(ptr)
    type is (integer(i1))
      h5type = H5_INTEGER_KIND
    type is (integer(i2))
      h5type = H5_INTEGER_KIND
    type is (integer(i4))
      h5type = H5_INTEGER_KIND
    type is (integer(i8))
      h5type = H5_INTEGER_KIND
    type is (real(r4))
      h5type = H5_REAL_KIND
    type is (real(r8))
      h5type = H5_REAL_KIND
    class default
      write(lout_log,*) "[err] get_h5type: type not implemented in func::get_h5type"
      call mystop(errcode%get_h5type)
  end select
END FUNCTION


END MODULE m_h5io


