  INTEGER(HID_T) :: varid, vspaceid
  INTEGER(i4)    :: istat
  TYPE(c_ptr)    :: varptr_c

! open dataset
  call h5dopen_f(fid, trim(varname), varid, istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_rdvar*d: cannot get var id for fid, varname=", &
                       fid, trim(varname)
     call mystop(errcode%varid)
  end if

! read in var value
  varptr_c = C_LOC(varval)
  call h5dread_f(varid, h5kind_to_type(kind(varval), h5io_kind), varptr_c, istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_rdvar*d: cannot get var value for fid, varname, varid=", &
                       fid, trim(varname), varid
     call mystop(errcode%varval)
  end if

! close dataset
  call h5dclose_f(varid, istat)
  if (istat /= SUCCEED) then
     write(lout_log,*) "[err] h5_rdvar*d: cannot close var id for fid, varname, varid=", &
                       fid, trim(varname), varid
     call mystop(errcode%varid)
  end if


