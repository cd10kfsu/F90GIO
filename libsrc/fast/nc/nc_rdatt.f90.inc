  INTEGER(i4) :: varid, istat

  if (len_trim(varname).EQ.0) then
     varid = NF90_GLOBAL
  else
     istat = NF90_INQ_VARID(fid, trim(varname), varid)
     if (istat /= NF90_NOERR) then
        write(lout_log,*) "[err] nc_rdatt::Fail to get varid of var:",trim(varname)
        call mystop(errcode%varid)
     end if
  end if
  istat = NF90_GET_ATT(fid, varid, trim(attname), attval)
  if (istat /= NF90_NOERR) then
     write(lout_log,*) "[err] nc_rdatt::Fail to get att: varname, attname=", trim(varname), trim(attname)
     call mystop(errcode%attval)
  end if
