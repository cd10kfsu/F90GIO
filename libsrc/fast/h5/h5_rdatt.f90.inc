  INTEGER(HID_T) :: varid, attid, attspaceid
  INTEGER(HID_T) :: h5_internal_kind
  INTEGER(i4)    :: istat
!  INTEGER(HSIZE_T) :: attdim(1) ! F90 interface by h5aread_f
  TYPE(c_ptr)    :: attptr_c    ! F03 interface by h5aread_f

! get attribute id
  call h5aopen_by_name_f(fid, trim(varname), trim(attname), attid, istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_rdatt: cannot get att id for fid, varname, attname=", &
                       fid, trim(varname), trim(attname)
     call mystop(errcode%attid)
  end if

  h5_internal_kind = h5kind_to_type(kind(attval), h5io_kind)

!! read attribute value (F90 interface)
!  call h5aread_f(attid, h5_internal_kind, attval, attdim, istat) 
! read attribute value (F03 interface)
  attptr_c = C_LOC(attval) 
  call h5aread_f(attid, h5_internal_kind, attptr_c, istat) 
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_rdatt: cannot get att value for fid, varname, attname=", &
                       fid, trim(varname), trim(attname)
     call mystop(errcode%attid)
  end if 

! close attribute id
  call h5aclose_f(attid, istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_rdatt: cannot close att id for fid, varname, attname=", &
                       fid, trim(varname), trim(attname)
     call mystop(errcode%attid)
  end if 


