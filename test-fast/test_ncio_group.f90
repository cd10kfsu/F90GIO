PROGRAM test_ncio_group
  USE iso_fortran_env, ONLY: r8 =>real64, r4 =>real32
  USE m_ncio,   ONLY: nc_get_fid, nc_get_gid, nc_close_fid, nc_rddim, nc_rdatt, &
                      nc_rdvar, nc_rdatt_cstr
  use netcdf
  IMPLICIT NONE

  CHARACTER(256) :: obsinfile

  INTEGER :: fid, gid
  INTEGER :: nx   ! Location
  REAL(r4),ALLOCATABLE :: alon1d(:), alat1d(:)  ! nx
  REAL(r4),ALLOCATABLE :: hofx0_waterTemperature(:) ! nx
  REAL(r4) :: rFillValue
  CHARACTER(256) :: sUnits

!-------------------------------------------------------------------------------
! Open the nc file 

  obsinfile = "profile_has_groups.nc4"
  WRITE(6,*) "[msg] ioda_read::obsinfile=", trim(obsinfile)
  CALL nc_get_fid(trim(obsinfile), fid)


!-------------------------------------------------------------------------------
! read global dimension
  CALL nc_rddim(fid, "Location", nx)
  WRITE(6,*) "[msg] ioda_read::Location=", nx

! read group "MetaData"
  CALL nc_get_gid(fid, "MetaData", gid)
  ALLOCATE(alon1d(nx),alat1d(nx))
  CALL nc_rdvar(gid, "longitude", alon1d)
  WRITE(6,*) "[msg] ioda_read::MetaData - longitude: min, max=", &
             minval(alon1d), maxval(alon1d)
  CALL nc_rdvar(gid, "latitude",  alat1d)
  WRITE(6,*) "[msg] ioda_read::MetaData - latitude: min, max=", &
             minval(alat1d), maxval(alat1d)
  CALL nc_rdatt(gid, "latitude", "_FillValue", rFillValue)
  WRITE(6,*) "[msg] ioda_read::MetaData - latitude: _FillValue =", rFillValue

#ifdef HAS_NC_C
  CALL nc_rdatt_cstr(fid, "", "_ioda_layout", sUnits)
  WRITE(6,*) "[msg] ioda_read:: global_att: _ioda_layout =", trim(sUnits)
  CALL verify_results(trim(sUnits), "ObsGroup", "incorrect global att", 1)
 
  CALL nc_rdatt_cstr(gid, "dateTime", "units", sUnits)
  WRITE(6,*) "[msg] ioda_read::MetaData - dateTime: units =", trim(sUnits)
  CALL verify_results(trim(sUnits), "seconds since 1970-01-01T00:00:00Z", "incorrect dateTime unit", 2)

  CALL nc_rdatt_cstr(gid, "depth", "units", sUnits)
  WRITE(6,*) "[msg] ioda_read::MetaData - depth: units =", trim(sUnits)
  CALL verify_results(trim(sUnits), "m", "incorrect depth unit", 3)

  CALL nc_rdatt_cstr(gid, "latitude", "units", sUnits)
  WRITE(6,*) "[msg] ioda_read::MetaData - latitude: units =", trim(sUnits)
  CALL verify_results(trim(sUnits), "degrees_north", "incorrect latitude unit", 4)

  CALL nc_rdatt_cstr(gid, "longitude", "units", sUnits)
  WRITE(6,*) "[msg] ioda_read::MetaData - longitude: units =", trim(sUnits)
  CALL verify_results(trim(sUnits), "degrees_east", "incorrect longitude unit", 5)
#endif

! read another group "hofx0"
  CALL nc_get_gid(fid, "hofx0", gid)
  ALLOCATE(hofx0_waterTemperature(nx))
  CALL nc_rdvar(gid, "waterTemperature",hofx0_waterTemperature)
  WRITE(6,*) "[msg] ioda_read::hofx0 - waterTemperature: min, max=", &
             minval(hofx0_waterTemperature), maxval(hofx0_waterTemperature)

!-------------------------------------------------------------------------------
! Close the nc file 

  CALL nc_close_fid(fid)

  WRITE(6,*) "Test passed"

#ifdef HAS_NC_C

CONTAINS
SUBROUTINE verify_results(input, ref, errmsg, errcode)
  IMPLICIT NONE
  
  CHARACTER(*),INTENT(IN) :: input, ref, errmsg
  INTEGER,INTENT(IN) :: errcode
  if (trim(input) .ne. trim(ref)) then
     WRITE(6,*) "ioda_read::"//trim(errmsg)
     STOP (errcode)
  end if
END SUBROUTINE

#endif


END PROGRAM


