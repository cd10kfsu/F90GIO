PROGRAM test_h5io
  use iso_fortran_env, ONLY: r8 => real64, r4 => real32, i4 => int32
  USE m_h5io,    ONLY: h5_get_fid, h5_close_fid, h5_rdvarshp, h5_rdvar2d, &
                       h5_rdvar1d, h5_rdatt, &
                       HID_T, HSIZE_T
  IMPLICIT NONE
  CHARACTER(256) :: obsinfile

  INTEGER(HID_T) :: fid
  INTEGER(HSIZE_T),allocatable :: varsizes(:)  ! (2)
  REAL(r8),ALLOCATABLE :: alon2d(:,:), alat2d(:,:)             ! (nlines,npixels)
  REAL(r8),ALLOCATABLE :: sea_surface_salinity(:,:), stde(:,:) ! (nlines,npixels)
  REAL(r8),ALLOCATABLE :: sss_time(:)                          ! (nlines) 
  INTEGER(i4), ALLOCATABLE :: quality_flag(:,:)                ! (nlines,npixels), need to ensure bits>16. Use 32bits here.
  LOGICAL,     ALLOCATABLE :: valid(:,:) ! .true. if no missing info at this grid by checking different FillValues in ncfile
  REAL(r4)     :: r4FillValue ! need to ensure bits=32
  INTEGER(i4)  :: i4FillValue ! need to ensure bits>16. Use 32bits here
  INTEGER :: nlines, npixels

! Open the hdf5 file 
  obsinfile="SMAP_L2B_SSS_36950_20220101T005200_R18240_V5.0.h5"
  WRITE(6,*) "[msg] read_jpl_smap_l2_sss_h5::read obs from obsinfile=",trim(obsinfile)
  CALL h5_get_fid(trim(obsinfile), fid)

! get dim info
  CALL h5_rdvarshp(fid, "/lon", varsizes)
  nlines = varsizes(1); npixels = varsizes(2)
  WRITE(6,*) "nlines, npixels=", nlines, npixels

! Read row time
   ALLOCATE(sss_time(nlines))
   CALL h5_rdvar1d(fid, "/row_time", sss_time)
   WRITE(6,*) "[msg] read_jpl_smap_l2_sss_h5::sss_time(1), sss_time(end)=", sss_time(1),sss_time(nlines)

! Read lat & lon info
  ALLOCATE(alon2d(nlines,npixels), alat2d(nlines,npixels))
  ALLOCATE(valid(nlines,npixels))

  CALL h5_rdvar2d(fid, "/lon", alon2d)
  CALL h5_rdatt(fid,   "/lon", "_FillValue", r4FillValue)
  WRITE(6,*) "_FillValue=", NINT(r4FillValue)
  valid = .true.
  where (NINT(alon2d)==NINT(r4FillValue))
    valid = .false.
  end where
  WRITE(6,*) "[msg] read_jpl_smap_l2_sss_h5::lon: min, max=", &
             minval(alon2d,mask=valid), maxval(alon2d,mask=valid)

  CALL h5_rdvar2d(fid, "/lat", alat2d)
  CALL h5_rdatt(fid,   "/lat", "_FillValue", r4FillValue)
  WRITE(6,*) "_FillValue=", NINT(r4FillValue)
  valid = .true.
  where (NINT(alat2d)==NINT(r4FillValue))
    valid = .false.
  end where
  WRITE(6,*) "[msg] read_jpl_smap_l2_sss_h5::lat: min, max=", &
             minval(alat2d,mask=valid), maxval(alat2d,mask=valid)

! Read sea surface salinity & its uncertainty
   ALLOCATE(sea_surface_salinity(nlines,npixels), stde(nlines,npixels))
   CALL h5_rdvar2d(fid, "/smap_sss", sea_surface_salinity)
   CALL h5_rdatt(fid,   "/smap_sss", "_FillValue", r4FillValue)
   valid = .true.
   where (NINT(sea_surface_salinity)==NINT(r4FillValue))
      valid = .false.
   end where
   WRITE(6,*) "[msg] read_jpl_smap_l2_sss_h5::smap_sss: min, max=", &
              minval(sea_surface_salinity, mask=valid), &
              maxval(sea_surface_salinity, mask=valid)

   CALL h5_rdvar2d(fid, "/smap_sss_uncertainty", stde)
   CALL h5_rdatt(fid,   "/smap_sss_uncertainty", "_FillValue", r4FillValue)
   valid = .true.
   where (NINT(stde)==NINT(r4FillValue))
      valid = .false.
   end where
   WRITE(6,*) "[msg] read_jpl_smap_l2_sss_h5::smap_sss_uncertainty: min, max=", &
              minval(stde, mask=valid), maxval(stde, mask=valid)
 
! Read quality control flag
   ALLOCATE(quality_flag(nlines,npixels))
   CALL h5_rdvar2d(fid, "/quality_flag", quality_flag)
   CALL h5_rdatt(fid,   "/quality_flag", "_FillValue", i4FillValue)
   WRITE(6,*) "i4FillValue", i4FillValue
   valid = .true.
   where (quality_flag == i4FillValue)
     valid = .false.
   end where
   WRITE(6,*) "[msg] read_jpl_smap_l2_sss_h5::quality_flag: min, max=", &
              minval(quality_flag, mask=valid), maxval(quality_flag, mask=valid)

!-------------------------------------------------------------------------------
! Close the hdf5 file 
  CALL h5_close_fid(fid)

  WRITE(6,*) "Test passed"

END PROGRAM
