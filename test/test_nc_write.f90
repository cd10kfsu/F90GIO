program main
  use common_nc
  implicit none

  character(80) :: fnout="test2.nc"
  integer :: ncid

  integer,parameter :: ndims = 3
  integer,parameter :: nx = 6, ny = 2, nz=4
  integer :: x_id, y_id
  character(80) :: x_name = "lon"
  character(80) :: y_name = "lat"
  character(80) :: z_name = "lev"

  character(80) :: dim_name(ndims)
  integer :: dim_length(ndims)
  logical :: lunlimited(ndims)

  integer  :: t_id, q_id, p_id
  character(80) :: t_name="theta"
  character(80) :: q_name="qv"
  character(80) :: p_name="pres"
  character(80) :: lon_name="lon"
 
  integer,parameter :: nvars = 4
  character(80) :: var_name(nvars)
  integer :: var_type(nvars)
  integer :: var_maxdim(nvars)
  character(80) :: var_dimname(4,nvars)

  integer :: t2d(ny,nx), q2d(ny,nx), p3d(nz,ny,nx), lon1d(nx), pp3d(nz,ny,nx)

  integer :: i, j
  integer :: ierr

  do i = 1, nx
     lon1d(i) = i
     do j = 1, ny
        t2d(j,i) = 10*j+i
        q2d(j,i) = 10*i+j
        p3d(1:nz,j,i) = 100*i+10*j
     enddo
  enddo


  call nc_createfile(trim(fnout))
  pause "create file"

  dim_name = (/x_name, y_name, z_name/)
  dim_length = (/nx, ny, nz/)
  lunlimited = (/.true., .false., .false./)
  call nc_createdims(trim(fnout),ndims,dim_name,lunlimited,dim_length)
  pause "create dim"

  call nc_addDim(trim(fnout),"nlevSoil",lunlimited=.false.,dimLength=4)


  var_name = (/t_name, q_name, lon_name, p_name/)
  var_type = (/NF90_INT, NF90_INT, NF90_INT, NF90_INT/)
  var_maxdim = (/2, 2, 1, 3/)
  var_dimname(1:2,1) = (/y_name, x_name/)
  var_dimname(1:2,2) = (/y_name, x_name/)
  var_dimname(1:1,3) = (/x_name/)
  var_dimname(1:3,4) = (/z_name, y_name, x_name/)

  call nc_createvars(trim(fnout),nvars,var_name, var_type, var_maxdim, var_dimname)
  pause "create vars"


  call nc_updatevar2d_integer4(trim(fnout),trim(var_name(1)),t2d)
  call nc_updatevar2d_integer4(trim(fnout),trim(var_name(2)),q2d)
  call nc_updatevar1d_integer4(trim(fnout),trim(var_name(3)),lon1d)
  call nc_updatevar3d_integer4(trim(fnout),trim(var_name(4)),p3d)

endprogram

