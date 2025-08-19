PROGRAM test_ncio_group
  USE iso_fortran_env, ONLY: r8 =>real64, r4 =>real32
  USE m_ncio,   ONLY: nc_get_fid, nc_get_gid, nc_close_fid, nc_rddim, nc_rdatt, &
                      nc_rdvar
  use netcdf
  IMPLICIT NONE

  CHARACTER(256) :: obsinfile

  INTEGER :: fid, gid
  INTEGER :: nx   ! Location
  REAL(r4),ALLOCATABLE :: alon1d(:), alat1d(:)  ! nx
  REAL(r4),ALLOCATABLE :: hofx0_waterTemperature(:) ! nx
  REAL(r4) :: rFillValue

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

  ! [FIXME]: currently no way to read string attributes using netcdf-fortran
  !  	float longitude(Location) ;
  !		longitude:_FillValue = -3.368795e+38f ;
  !		string longitude:units = "degrees_east" ;

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

END PROGRAM


