  INTEGER(i4) :: istat, varid

  istat = NF90_INQ_VARID(fid, trim(varname), varid)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] rc_rdvar*d::Fail to get varid of var: ",trim(varname)
     call mystop(errcode%varid)
  end if

  istat = NF90_GET_VAR(fid, varid, varval)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] rc_rdvar*d::Fail to get value of var: ",trim(varname)
     call mystop(errcode%varval)
  end if
