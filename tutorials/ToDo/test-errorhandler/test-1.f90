program main
  use globaldata
  use errorhandler_class
  implicit none

  integer( dip ) :: iostat
  character( len = 100 ) :: iomsg

  call ErrMsg % error( txt = "some-error-msg", unit=stderr, file=__FILE__, &
    & line=__LINE__ )

  call ErrMsg % error( txt = "some-error-msg", unit=stderr, file=__FILE__, &
    & line=__LINE__, iostat=iostat, iomsg=iomsg )

end program main

! use -fpp for ifort and -cpp for gfortran