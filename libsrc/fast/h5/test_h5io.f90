program main
  use m_h5io
  implicit none

  character(256) :: fnin
  integer(HID_T) :: fid

  fnin = "SMAP_L2B_SSS_36950_20220101T005200_R18240_V5.0.h5"
  call h5_get_fid(trim(fnin), fid)
  print*, "fid=", fid
  call h5_close_fid(fid)

endprogram

