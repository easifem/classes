module sinfunc
  implicit none

  interface sin
    module procedure sinfunction
  end interface

  interface operator( ** )
      module procedure powerfunction
  end interface

  type :: mytype
    real :: x
  end type mytype

  contains
    function sinfunction( obj ) result( ans )
      type( mytype ), intent( in ) :: obj
      real :: ans
      ans = SIN( Obj % x )
    end function sinfunction

    function powerfunction( obj, expo ) result( ans )
      type( mytype ), intent( in ) :: obj
      real, intent( in ) :: expo
      real :: ans
      ans = obj % x ** expo
    end function powerfunction

end module sinfunc

program main
  use sinfunc
  implicit none

  type( mytype ) :: obj
  real :: y

  obj = mytype( 2.0 )

  y = sin( obj )
  write( *, * ) y

  y = obj**2.0
  write( *, * ) y

end program main