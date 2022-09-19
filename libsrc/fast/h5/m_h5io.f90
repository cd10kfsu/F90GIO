MODULE m_h5io
  USE hdf5
  USE iso_c_binding, ONLY: c_ptr, c_loc
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: h5_get_fid, h5_close_fid
  PUBLIC :: HID_T ! type for all different ids used for H5


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
    INTEGER(i1) :: attval = 4
    INTEGER(i1) :: dimid  = 5
    INTEGER(i1) :: dimval = 6
    INTEGER(i1) :: ftnapi = 7
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

END MODULE m_h5io


