program main
  implicit none

  integer,target :: n = 10
  real,target :: x = 10.0
  class(*),pointer :: xptr

  xptr => x
  selecttype(x)
    type is (real(4),real(8))
      print*, "real"
    type is (integer(4))
      print*, "integer"
  end select

endprogram
