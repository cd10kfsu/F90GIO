PROGRAM test_ncio
  USE iso_fortran_env, ONLY: r8 =>real64
  USE m_ncio,   ONLY: nc_get_fid, nc_close_fid, nc_rddim, nc_rdatt, &
                      nc_rdvar1d, nc_rdvar2d
  IMPLICIT NONE

  CHARACTER(256) :: obsinfile

  INTEGER :: fid
  INTEGER :: nlon, nlat, nt
  REAL(r8),ALLOCATABLE :: alon1d(:), alat1d(:) 
  REAL(r8),ALLOCATABLE :: sea_surface_salinity(:,:), stde(:,:) ! (nlon,nlat)
  REAL(r8),ALLOCATABLE :: land_fraction(:,:), ice_fraction(:,:) ! (nlon,nlat)
  REAL(r8),ALLOCATABLE :: sss_time(:) ! (nt)
  LOGICAL, ALLOCATABLE :: valid(:,:) ! (nlon,nlat)
  REAL(r8) :: rFillValue


!-------------------------------------------------------------------------------
! Open the nc file 

  obsinfile = "SMAP_L3_SSS_20220105_8DAYS_V5.0.nc"
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::obsinfile=", trim(obsinfile)
  CALL nc_get_fid(trim(obsinfile), fid)
  
!-------------------------------------------------------------------------------
! Read dimension & geo vars

  CALL nc_rddim(fid, "longitude", nlon)
  CALL nc_rddim(fid, "latitude", nlat)
  CALL nc_rddim(fid, "time",     nt)
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::nlon,nlat,nt=", nlon, nlat, nt

  ALLOCATE(alon1d(nlon), alat1d(nlat))
  CALL nc_rdvar1d(fid, "longitude", alon1d)
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::lon: min, max=", &
             minval(alon1d), maxval(alon1d)
  CALL nc_rdvar1d(fid, "latitude",  alat1d)
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::lat: min, max=", &
             minval(alat1d), maxval(alat1d)

!-------------------------------------------------------------------------------
! Read obs time info

  ALLOCATE(sss_time(nt))
  CALL nc_rdvar1d(fid, "time", sss_time)
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::sss_time=", sss_time

!-------------------------------------------------------------------------------
! Read sea surface salinity and its uncertainty

  ALLOCATE(valid(nlon,nlat))

  ALLOCATE(sea_surface_salinity(nlon,nlat))
  CALL nc_rdvar2d(fid, "smap_sss", sea_surface_salinity)
  CALL nc_rdatt(fid,   "smap_sss", "_FillValue", rFillValue)
  WRITE(6,*) "rFillValue=", rFillValue
  valid = .true.
  where (NINT(sea_surface_salinity)==NINT(rFillValue)) 
    valid = .false.
  end where
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::smap_sss: min, max=", &
             minval(sea_surface_salinity, mask=valid), &
             maxval(sea_surface_salinity, mask=valid)

  ALLOCATE(stde(nlon,nlat))
  CALL nc_rdvar2d(fid, "smap_sss_uncertainty", stde)
  CALL nc_rdatt(fid,   "smap_sss_uncertainty", "_FillValue", rFillValue)
  WRITE(6,*) "rFillValue=", rFillValue
  valid = .true.
  where (NINT(stde)==NINT(rFillValue)) 
    valid = .false.
  end where
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::smap_sss_uncertainty: min, max=", &
             minval(stde, mask=valid), &
             maxval(stde, mask=valid)

!-------------------------------------------------------------------------------
! Read land & ice fraction for additional QC

  ALLOCATE(land_fraction(nlon,nlat))
  CALL nc_rdvar2d(fid, "land_fraction", land_fraction)
  CALL nc_rdatt(fid,   "land_fraction", "_FillValue", rFillValue)
  WRITE(6,*) "rFillValue=", rFillValue
  valid = .true.
  where (NINT(land_fraction)==NINT(rFillValue)) 
    valid = .false.
  end where
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::land_fraction: min, max=", &
             minval(land_fraction, mask=valid), &
             maxval(land_fraction, mask=valid)

  ALLOCATE(ice_fraction(nlon,nlat))
  CALL nc_rdvar2d(fid, "ice_fraction", ice_fraction)
  CALL nc_rdatt(fid,   "ice_fraction", "_FillValue", rFillValue)
  WRITE(6,*) "rFillValue=", rFillValue
  valid = .true.
  where (NINT(ice_fraction)==NINT(rFillValue)) 
    valid = .false.
  end where
  WRITE(6,*) "[msg] read_jpl_smap_l3_sss_nc::ice_fraction: min, max=", &
             minval(ice_fraction, mask=valid), &
             maxval(ice_fraction, mask=valid)

!-------------------------------------------------------------------------------
! Close the nc file 

  CALL nc_close_fid(fid)

  WRITE(6,*) "Test passed"

END PROGRAM


