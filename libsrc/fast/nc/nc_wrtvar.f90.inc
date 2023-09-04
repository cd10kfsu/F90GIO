  INTEGER(i4) :: istat, varid

  istat = NF90_INQ_VARID(fid, trim(varname), varid)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] rc_wrtvar*d::Fail to get varid of var: ",trim(varname)
     call mystop(errcode%varid)
  end if

  istat = NF90_PUT_VAR(fid, varid, varval)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] rc_wrtvar*d::Fail to update value of var: ",trim(varname)
     call mystop(errcode%varval)
  end if
