module m_test
 
contains

subroutine check_len(str)
  implicit none

  character(*),intent(in) :: str

  print*, "len=", len_trim(str)
endsubroutine
end module

program main
  use m_test
  use m_ncio
  implicit none

  integer :: nargs
  character(256) :: fname
  integer(4) :: fid = 0

  integer(2),allocatable :: i2buf3d(:,:,:), i2buf2d(:,:)
  integer(4) :: nx, ny, nt = 1
  real(8) :: offset, scales
  character(256) :: str

  nx = 8640; ny = 4320; nt = 1 
  allocate(i2buf3d(nx,ny,1))
  allocate(i2buf2d(nx,ny))

  nargs = command_argument_count()
  if (nargs/=1) stop "nargs/=1"

  call get_command_argument(1,fname)
  print*, trim(fname)

  call nc_get_fid(trim(fname),fid)
  print*, "fid=",fid

  call nc_rdvar3d_i2(fid,"sea_surface_temperature",i2buf3d)
  print*, "i2buf3d: min, max=", minval(i2buf3d), maxval(i2buf3d)


  call nc_rdvar2d_i2(fid,"sea_surface_temperature",i2buf2d)
  print*, "i2buf3d: min, max=", minval(i2buf2d), maxval(i2buf2d)
  print*, "diff=", sum(i2buf3d(:,:,1)-i2buf2d(:,:))

  call nc_rdatt_r8(fid,"sea_surface_temperature", "add_offset", offset)
  print*, "offset=", offset

  call nc_rdatt_str(fid,"", "uuid", str)
  print*, "str=", str
  call nc_rdatt_str(fid,"", "gds_version_id", str)
  print*, "str=", str

  nx = 1; print*, "nx=", nx
  call nc_rddim(fid,"lon",nx)
  print*, "lon=", nx
  call nc_rddim(fid,"lat",nx)
  print*, "lat=", nx
  call nc_rddim(fid,"time",nx)
  print*, "time=", nx

  call nc_close_fid(fid)



endprogram
