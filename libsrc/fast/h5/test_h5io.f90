program main
  use m_h5io
  use mod_f90gioh5, only : H5_ReadVar2d_Real4, H5_ReadVar2d_Real8
  implicit none

  character(256) :: fnin
  integer(HID_T) :: fid
  integer(2) :: i2buf
  integer(4) :: i4buf
  integer(8) :: i8buf
  real(4),allocatable :: r4buf2d(:,:), r4buf2d2(:,:)
  real(8),allocatable :: r8buf2d(:,:), r8buf2d2(:,:)

  integer(4),allocatable :: i4buf2d(:,:)
  integer(2),allocatable :: i2buf2d(:,:)


  integer :: nx, ny, ix, iy
  integer :: istat


!-----------------------------------------------------------------------
! config
  fnin = "SMAP_L2B_SSS_36950_20220101T005200_R18240_V5.0.h5"
  ny = 76
  nx = 1624
  allocate(r4buf2d(nx,ny))
  allocate(r8buf2d(nx,ny))
  allocate(i4buf2d(nx,ny))
  allocate(i2buf2d(nx,ny))

  iy = 20
  ix = 1145


!-----------------------------------------------------------------------
! F90GIO H5
  istat = H5_ReadVar2d_Real4(trim(fnin), "lon", r4buf2d2)
  print*, "F90GIO: sum=", sum(r4buf2d2)
  print*, "SHAPE(r4buf2d2)=", shape(r4buf2d2)
  print*, "F90GIO: ix, iy, val =", ix, iy, r4buf2d2(ix,iy)

! F90GIO H5
  istat = H5_ReadVar2d_Real8(trim(fnin), "lon", r8buf2d2)
  print*, "F90GIO: sum=", sum(r8buf2d2)
  print*, "SHAPE(r8buf2d2)=", shape(r8buf2d2)
  print*, "F90GIO: ix, iy, val =", ix, iy, r8buf2d2(ix,iy)


!-----------------------------------------------------------------------
! h5io
  call h5_get_fid(trim(fnin), fid)
  print*, "h5io: fid=", fid

  call h5_rdvar2d_r4(fid, "lon", r4buf2d)
  print*, "h5io: sum=", sum(r4buf2d)
  print*, "SHAPE(r4buf2d)=", shape(r4buf2d)
  print*, "h5io: ix, iy, val =", ix, iy, r4buf2d(ix,iy)

  call h5_rdvar2d_r8(fid, "lon", r8buf2d)
  print*, "h5io: sum=", sum(r4buf2d)
  print*, "SHAPE(r8buf2d)=", shape(r8buf2d)
  print*, "h5io: ix, iy, val =", ix, iy, r8buf2d(ix,iy)

  call h5_rdvar2d_i4(fid, "n_v_fore", i4buf2d)
  print*, "i4buf2d(484:488,31)=", i4buf2d(484:488,31)
  print*, "i4buf2d(484:488,32)=", i4buf2d(484:488,32)
  print*, "i4buf2d(484:488,33)=", i4buf2d(484:488,33)

  call h5_rdatt_i4(fid, "quality_flag", "_FillValue", i4buf)
  print*, "attval=", i4buf

  call h5_rdatt_i4(fid, "quality_flag", "QUAL_FLAG_HIGH_SPEED_USEABLE", i4buf)
  print*, "attval=", i4buf

  call h5_close_fid(fid)

endprogram

