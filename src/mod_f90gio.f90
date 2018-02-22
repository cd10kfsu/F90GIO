module mod_f90gio
!$$$  module documentation block
!         .           .            .
!  module name: mod_f90gio.f90
!    programmer: da,cheng        org: umd         date: 2017-04-05
!
!  purpose:
!    include netcdf, hdf4, hdf5 modules together
!
!
!  revision history:
!    2017-04-05     da    - creator
!
!  attributes: 
!    language: fortran 90
!    machine: Darwin Kernel Version 15.6.0: root:xnu-3248.60.11.2.1~1/RELEASE_X86_64 x86_64
!
!
!$$$ end documentation block

  use mod_f90gionc
  !use mod_f90gioh4
  use mod_f90gioh5
  implicit none

endmodule
