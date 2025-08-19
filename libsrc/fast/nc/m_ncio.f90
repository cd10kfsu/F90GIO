MODULE m_ncio
!=====================================================================
! Purpose:
!   Module for simple NetCDF I/O. 
!   Modified from cheng's F90GIO (https://github.com/cd10kfsu/F90GIO):
!   1. Use file id to read all things, to reduce the frequency of opening/closing files
!   2. Remove size & shape checks
  USE NetCDF, ONLY: NF90_NOWRITE, NF90_WRITE, NF90_NOERR, NF90_CLOBBER, &
                    NF90_DOUBLE, NF90_FLOAT, NF90_INT, NF90_SHORT, NF90_BYTE, &
                    NF90_UNLIMITED, &
                    NF90_Open, NF90_Close, &
                    NF90_inquire, NF90_Inq_Ncid, NF90_Inq_Varid, NF90_Inquire_Variable, &
                    NF90_Inq_DimID, NF90_Inquire_Dimension, &
                    NF90_Get_Var, NF90_Put_Var, &
                    NF90_Create, NF90_Def_dim, NF90_Def_Var, NF90_Enddef, NF90_reDef, &
                    NF90_GLOBAL, NF90_Get_Att
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: NF90_DOUBLE, NF90_FLOAT, NF90_INT, NF90_SHORT, NF90_BYTE

! open/close file
  PUBLIC :: nc_get_fid, nc_close_fid

! get group id
  PUBLIC :: nc_get_gid

! Create new file/dim/var
  PUBLIC :: nc_create_file, nc_create_dim, nc_create_var

! End def mode
  PUBLIC :: nc_end_create

! Read Attributes
  PUBLIC :: nc_rdatt

! low-level read Attributes
  PUBLIC :: nc_rdatt_str, nc_rdatt_i1, nc_rdatt_i2, nc_rdatt_i4, &
            nc_rdatt_r4, nc_rdatt_r8

! Get dimension
  PUBLIC :: nc_rddim

! Check if var/dim in a file
  PUBLIC :: nc_fndvar, nc_fnddim

! read ND vars
  PUBLIC :: nc_rdvar
  PUBLIC :: nc_rdvar1d, nc_rdvar2d, nc_rdvar3d, nc_rdvar4d

! write ND vars
  PUBLIC :: nc_wrtvar
  PUBLIC :: nc_wrtvar1d, nc_wrtvar2d, nc_wrtvar3d, nc_wrtvar4d

! low-level Read 1D
  PUBLIC :: nc_rdvar1d_i1, nc_rdvar1d_i2, nc_rdvar1d_i4, nc_rdvar1d_r4, &
            nc_rdvar1d_r8, nc_rdvar1d_str
! low-level Read 2D
  PUBLIC :: nc_rdvar2d_i1, nc_rdvar2d_i2, nc_rdvar2d_i4, nc_rdvar2d_r4, &
            nc_rdvar2d_r8
! low-level Read 3D
  PUBLIC :: nc_rdvar3d_i1, nc_rdvar3d_i2, nc_rdvar3d_i4, nc_rdvar3d_r4, &
            nc_rdvar3d_r8
! low-level Read 4D
  PUBLIC :: nc_rdvar4d_i1, nc_rdvar4d_i2, nc_rdvar4d_i4, nc_rdvar4d_r4, &
            nc_rdvar4d_r8

! low-level Write 1D
  PUBLIC :: nc_wrtvar1d_i1, nc_wrtvar1d_i2, nc_wrtvar1d_i4, nc_wrtvar1d_r4, &
            nc_wrtvar1d_r8
! low-level Write 2D
  PUBLIC :: nc_wrtvar2d_i1, nc_wrtvar2d_i2, nc_wrtvar2d_i4, nc_wrtvar2d_r4, &
            nc_wrtvar2d_r8
! low-level Write 3D
  PUBLIC :: nc_wrtvar3d_i1, nc_wrtvar3d_i2, nc_wrtvar3d_i4, nc_wrtvar3d_r4, &
            nc_wrtvar3d_r8
! low-level Write 4D
  PUBLIC :: nc_wrtvar4d_i1, nc_wrtvar4d_i2, nc_wrtvar4d_i4, nc_wrtvar4d_r4, &
            nc_wrtvar4d_r8


!-------------------------------------------------------------------------------
! Internal vars & subs
  INTERFACE nc_rdatt
    MODULE PROCEDURE nc_rdatt_str
    MODULE PROCEDURE nc_rdatt_i1, nc_rdatt_i2, nc_rdatt_i4
    MODULE PROCEDURE nc_rdatt_r4, nc_rdatt_r8
  END INTERFACE

  INTERFACE nc_rdvar
    MODULE PROCEDURE nc_rdvar1d_i1, nc_rdvar1d_i2, nc_rdvar1d_i4
    MODULE PROCEDURE nc_rdvar1d_r4, nc_rdvar1d_r8
    MODULE PROCEDURE nc_rdvar2d_i1, nc_rdvar2d_i2, nc_rdvar2d_i4
    MODULE PROCEDURE nc_rdvar2d_r4, nc_rdvar2d_r8
    MODULE PROCEDURE nc_rdvar3d_i1, nc_rdvar3d_i2, nc_rdvar3d_i4
    MODULE PROCEDURE nc_rdvar3d_r4, nc_rdvar3d_r8
    MODULE PROCEDURE nc_rdvar4d_i1, nc_rdvar4d_i2, nc_rdvar4d_i4
    MODULE PROCEDURE nc_rdvar4d_r4, nc_rdvar4d_r8
  END INTERFACE

  INTERFACE nc_rdvar1d
    MODULE PROCEDURE nc_rdvar1d_i1, nc_rdvar1d_i2, nc_rdvar1d_i4
    MODULE PROCEDURE nc_rdvar1d_r4, nc_rdvar1d_r8
  END INTERFACE

  INTERFACE nc_rdvar2d
    MODULE PROCEDURE nc_rdvar2d_i1, nc_rdvar2d_i2, nc_rdvar2d_i4
    MODULE PROCEDURE nc_rdvar2d_r4, nc_rdvar2d_r8
  END INTERFACE

  INTERFACE nc_rdvar3d
    MODULE PROCEDURE nc_rdvar3d_i1, nc_rdvar3d_i2, nc_rdvar3d_i4
    MODULE PROCEDURE nc_rdvar3d_r4, nc_rdvar3d_r8
  END INTERFACE

  INTERFACE nc_rdvar4d
    MODULE PROCEDURE nc_rdvar4d_i1, nc_rdvar4d_i2, nc_rdvar4d_i4
    MODULE PROCEDURE nc_rdvar4d_r4, nc_rdvar4d_r8
  END INTERFACE

  INTERFACE nc_wrtvar
    MODULE PROCEDURE nc_wrtvar1d_i1, nc_wrtvar1d_i2, nc_wrtvar1d_i4
    MODULE PROCEDURE nc_wrtvar1d_r4, nc_wrtvar1d_r8
    MODULE PROCEDURE nc_wrtvar2d_i1, nc_wrtvar2d_i2, nc_wrtvar2d_i4
    MODULE PROCEDURE nc_wrtvar2d_r4, nc_wrtvar2d_r8
    MODULE PROCEDURE nc_wrtvar3d_i1, nc_wrtvar3d_i2, nc_wrtvar3d_i4
    MODULE PROCEDURE nc_wrtvar3d_r4, nc_wrtvar3d_r8
    MODULE PROCEDURE nc_wrtvar4d_i1, nc_wrtvar4d_i2, nc_wrtvar4d_i4
    MODULE PROCEDURE nc_wrtvar4d_r4, nc_wrtvar4d_r8
  END INTERFACE

  INTERFACE nc_wrtvar1d
    MODULE PROCEDURE nc_wrtvar1d_i1, nc_wrtvar1d_i2, nc_wrtvar1d_i4
    MODULE PROCEDURE nc_wrtvar1d_r4, nc_wrtvar1d_r8
  END INTERFACE

  INTERFACE nc_wrtvar2d
    MODULE PROCEDURE nc_wrtvar2d_i1, nc_wrtvar2d_i2, nc_wrtvar2d_i4
    MODULE PROCEDURE nc_wrtvar2d_r4, nc_wrtvar2d_r8
  END INTERFACE

  INTERFACE nc_wrtvar3d
    MODULE PROCEDURE nc_wrtvar3d_i1, nc_wrtvar3d_i2, nc_wrtvar3d_i4
    MODULE PROCEDURE nc_wrtvar3d_r4, nc_wrtvar3d_r8
  END INTERFACE

  INTERFACE nc_wrtvar4d
    MODULE PROCEDURE nc_wrtvar4d_i1, nc_wrtvar4d_i2, nc_wrtvar4d_i4
    MODULE PROCEDURE nc_wrtvar4d_r4, nc_wrtvar4d_r8
  END INTERFACE


  INTEGER,PARAMETER :: i1 = 1
  INTEGER,PARAMETER :: i2 = 2
  INTEGER,PARAMETER :: i4 = 4
  INTEGER,PARAMETER :: i8 = 8
  INTEGER,PARAMETER :: r4 = 4
  INTEGER,PARAMETER :: r8 = 8

  INTEGER,SAVE,PRIVATE :: lout_log = 6 ! output log

  TYPE::t_errcode
    ! i2 supports -128 -> 127
    INTEGER(i1) :: fid    = 1
    INTEGER(i1) :: varid  = 2
    INTEGER(i1) :: varval = 3
    INTEGER(i1) :: attval = 4
    INTEGER(i1) :: dimid  = 5
    INTEGER(i1) :: dimval = 6
    INTEGER(i1) :: genfile = 7
    INTEGER(i1) :: gendim  = 8
    INTEGER(i1) :: genvar  = 9
    INTEGER(i1) :: endgen  = 10
    INTEGER(i1) :: gid     = 11
    INTEGER(i1) :: undef  = 127 ! max positive for signed 8-byte int
  END TYPE 
  TYPE(t_errcode),SAVE,PRIVATE:: errcode

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
! end def mode
!--------------------------------------------------------------------------------
SUBROUTINE nc_end_create(fid)
  IMPLICIT NONE

  INTEGER(i4),INTENT(IN) :: fid

  INTEGER(i4) :: istat

  istat = nf90_enddef(fid)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_end_create: cannot close def mode for fid:", fid
     call mystop(errcode%endgen)
  end if

END SUBROUTINE 


!--------------------------------------------------------------------------------
! create nc file
!--------------------------------------------------------------------------------
SUBROUTINE nc_create_file(filename, fid)
  IMPLICIT NONE

  CHARACTER(*),INTENT(IN)  :: filename
  INTEGER(i4), INTENT(OUT) :: fid

  INTEGER(i4) :: istat

  istat = nf90_create(trim(filename), NF90_CLOBBER, fid)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_create_file: cannot get fid for file", trim(filename)
     call mystop(errcode%genfile) 
  end if

END SUBROUTINE nc_create_file

!--------------------------------------------------------------------------------
! open/close nc file
!--------------------------------------------------------------------------------
SUBROUTINE nc_get_fid(filename,fid,writemode)
  IMPLICIT NONE

  CHARACTER(*),INTENT(IN)  :: filename
  INTEGER(i4), INTENT(OUT) :: fid
  LOGICAL,     INTENT(IN), OPTIONAL :: writemode
  INTEGER(i4) :: istat

  logical :: writemode_

  writemode_ = .false. 
  if (present(writemode)) writemode_ = writemode

  if (writemode_) then
    istat = NF90_Open( TRIM(filename), NF90_WRITE, fid)
  else
    istat = NF90_Open( TRIM(filename), NF90_NOWRITE, fid)
  end if
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_get_fid::Fail to get fid of file:",trim(filename)
     call mystop(errcode%fid)
  end if

END SUBROUTINE nc_get_fid


SUBROUTINE nc_close_fid(fid)
  IMPLICIT NONE

  INTEGER(i4),INTENT(IN) :: fid
  INTEGER(i4) :: istat

  istat = NF90_Close(fid)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_close_fid::Fail to close file with fid=",fid
     call mystop(errcode%fid)
  end if

END SUBROUTINE nc_close_fid

!--------------------------------------------------------------------------------
! group operations
!--------------------------------------------------------------------------------
SUBROUTINE nc_get_gid(fid, gname, gid)
  IMPLICIT NONE

  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: gname
  INTEGER(i4),INTENT(OUT) :: gid
  INTEGER(i4) :: istat

  istat = nf90_inq_ncid(fid, trim(gname), gid)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_close_fid::Fail to get group id =",fid, "for var:", trim(gname)
     call mystop(errcode%gid)
  end if

END SUBROUTINE nc_get_gid

!--------------------------------------------------------------------------------
! create vars
!--------------------------------------------------------------------------------
SUBROUTINE nc_create_var(fid, varname, vartype, varsize, varid)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(IN) :: vartype
  INTEGER(i4), INTENT(IN) :: varsize(:)
  INTEGER(i4), INTENT(OUT) :: varid

  INTEGER(i4) :: istat

  istat = nf90_def_var(fid, trim(varname), vartype, varsize, varid)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_create_vars: cannot create var:", trim(varname)
     call mystop(errcode%genvar)
  end if

END SUBROUTINE
 
!--------------------------------------------------------------------------------
! read 1D
!--------------------------------------------------------------------------------
SUBROUTINE nc_rdvar1d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i1), INTENT(OUT) :: varval(:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar1d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i2), INTENT(OUT) :: varval(:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar1d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(OUT) :: varval(:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar1d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r4), INTENT(OUT) :: varval(:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar1d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r8), INTENT(OUT) :: varval(:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar1d_str(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  CHARACTER(*), INTENT(INOUT) :: varval(:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 



!--------------------------------------------------------------------------------
! write 1D
!--------------------------------------------------------------------------------
SUBROUTINE nc_wrtvar1d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i1), INTENT(IN) :: varval(:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar1d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i2), INTENT(IN) :: varval(:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar1d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(IN) :: varval(:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar1d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r4), INTENT(IN) :: varval(:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar1d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r8), INTENT(IN) :: varval(:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 


!--------------------------------------------------------------------------------
! read 2D
!--------------------------------------------------------------------------------
SUBROUTINE nc_rdvar2d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i1), INTENT(OUT) :: varval(:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar2d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i2), INTENT(OUT) :: varval(:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar2d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(OUT) :: varval(:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar2d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r4), INTENT(OUT) :: varval(:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar2d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r8), INTENT(OUT) :: varval(:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 


!--------------------------------------------------------------------------------
! write 2D
!--------------------------------------------------------------------------------
SUBROUTINE nc_wrtvar2d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i1), INTENT(IN) :: varval(:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar2d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i2), INTENT(IN) :: varval(:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar2d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(IN) :: varval(:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar2d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r4), INTENT(IN) :: varval(:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar2d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r8), INTENT(IN) :: varval(:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 


!--------------------------------------------------------------------------------
! read 3D
!--------------------------------------------------------------------------------
SUBROUTINE nc_rdvar3d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i1), INTENT(OUT) :: varval(:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar3d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i2), INTENT(OUT) :: varval(:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar3d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(OUT) :: varval(:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar3d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r4), INTENT(OUT) :: varval(:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar3d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r8), INTENT(OUT) :: varval(:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

!--------------------------------------------------------------------------------
! write 3D
!--------------------------------------------------------------------------------
SUBROUTINE nc_wrtvar3d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i1), INTENT(IN) :: varval(:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar3d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i2), INTENT(IN) :: varval(:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar3d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(IN) :: varval(:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar3d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r4), INTENT(IN) :: varval(:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar3d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r8), INTENT(IN) :: varval(:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 


!--------------------------------------------------------------------------------
! read 4D
!--------------------------------------------------------------------------------
SUBROUTINE nc_rdvar4d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i1), INTENT(OUT) :: varval(:,:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar4d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i2), INTENT(OUT) :: varval(:,:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar4d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(OUT) :: varval(:,:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar4d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r4), INTENT(OUT) :: varval(:,:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdvar4d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r8), INTENT(OUT) :: varval(:,:,:,:)
  include "nc_rdvar.f90.inc"
END SUBROUTINE 


!--------------------------------------------------------------------------------
! write 4D
!--------------------------------------------------------------------------------
SUBROUTINE nc_wrtvar4d_i1(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i1), INTENT(IN) :: varval(:,:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar4d_i2(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i2), INTENT(IN) :: varval(:,:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar4d_i4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(IN) :: varval(:,:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar4d_r4(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r4), INTENT(IN) :: varval(:,:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_wrtvar4d_r8(fid, varname, varval)
  IMPLICIT NONE
  INTEGER(i4),INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  REAL(r8), INTENT(IN) :: varval(:,:,:,:)
  include "nc_wrtvar.f90.inc"
END SUBROUTINE 



!--------------------------------------------------------------------------------
! read attributes
!--------------------------------------------------------------------------------
SUBROUTINE nc_rdatt_str(fid, varname, attname, attval)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname, attname
  CHARACTER(*),INTENT(INOUT) :: attval
  include "nc_rdatt.f90.inc"
END SUBROUTINE 


SUBROUTINE nc_rdatt_i1(fid, varname, attname, attval)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname, attname
  INTEGER(i1),    INTENT(OUT) :: attval
  include "nc_rdatt.f90.inc"
END SUBROUTINE 

SUBROUTINE nc_rdatt_i2(fid, varname, attname, attval)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname, attname
  INTEGER(i2),    INTENT(OUT) :: attval
  include "nc_rdatt.f90.inc"
END SUBROUTINE

SUBROUTINE nc_rdatt_i4(fid, varname, attname, attval)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname, attname
  INTEGER(i4), INTENT(OUT) :: attval
  include "nc_rdatt.f90.inc"
END SUBROUTINE

SUBROUTINE nc_rdatt_r4(fid, varname, attname, attval)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname, attname
  REAL(r4),    INTENT(OUT) :: attval
  include "nc_rdatt.f90.inc"
END SUBROUTINE

SUBROUTINE nc_rdatt_r8(fid, varname, attname, attval)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname, attname
  REAL(r8),    INTENT(OUT) :: attval
  include "nc_rdatt.f90.inc"
END SUBROUTINE

!--------------------------------------------------------------------------------
! define dimension
!--------------------------------------------------------------------------------
SUBROUTINE nc_create_dim(fid, dimname, dimsize, lunlimited, dimid)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN)  :: fid
  CHARACTER(*),INTENT(IN)  :: dimname
  INTEGER(i4), INTENT(IN)  :: dimsize
  LOGICAL,     INTENT(IN)  :: lunlimited
  INTEGER(i4), INTENT(OUT) :: dimid

  INTEGER :: istat

  if (lunlimited) then
    istat = nf90_def_dim(fid, trim(dimname), NF90_UNLIMITED, dimid)
  else
    istat = nf90_def_dim(fid, trim(dimname), dimsize, dimid)
  end if
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_create_dim: cannot create dim:"//trim(dimname)
     call mystop(errcode%gendim)
  end if


END SUBROUTINE 


!--------------------------------------------------------------------------------
! read dimension
!--------------------------------------------------------------------------------
SUBROUTINE nc_rddim(fid, dimname, dimval)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: dimname
  INTEGER(i4), INTENT(OUT) :: dimval

  INTEGER(i4) :: istat, dimid

  istat = NF90_INQ_DIMID(fid,trim(dimname),dimid)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_rddim::Fail to get dimid of dim:",trim(dimname)
     call mystop(errcode%dimid)
  end if
  istat = NF90_INQUIRE_DIMENSION(fid,dimid,len=dimval)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_rddim::Fail to get dim val of dim:",trim(dimname)
     call mystop(errcode%dimval)
  end if

END SUBROUTINE

!--------------------------------------------------------------------------------
! find if dim exist in a nc file by file id
!--------------------------------------------------------------------------------
FUNCTION nc_fnddim(fid, dimname, dimid) result (found)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: dimname
  INTEGER(i4), INTENT(OUT),OPTIONAL :: dimid
  LOGICAL :: found

  INTEGER(i4) :: istat, dimid_

  istat = NF90_INQ_DIMID(fid,trim(dimname),dimid_)
  if (istat /= NF90_NOERR) then
     found = .FALSE.
  else 
     found = .TRUE.
  end if
  if (PRESENT(dimid)) dimid = dimid_

END FUNCTION

!--------------------------------------------------------------------------------
! find if var exist in a nc file by file id
!--------------------------------------------------------------------------------
FUNCTION nc_fndvar(fid, varname, varid) result (found)
  IMPLICIT NONE

  INTEGER(i4), INTENT(IN) :: fid
  CHARACTER(*),INTENT(IN) :: varname
  INTEGER(i4), INTENT(OUT),OPTIONAL :: varid
  LOGICAL :: found

  INTEGER(i4) :: istat, varid_

  istat = NF90_INQ_VARID(fid,trim(varname),varid_)
  if (istat /= NF90_NOERR) then
     found = .FALSE.
  else 
     found = .TRUE.
  end if
  if (PRESENT(varid)) varid = varid_

END FUNCTION


ENDMODULE m_ncio
