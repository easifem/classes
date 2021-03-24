program main
use easifem
use gmshPoint_Class
implicit none

class( gmshPoint_ ), pointer :: obj
type( string ) :: l

obj => gmshPoint_Pointer( 1.d0, 1.d0, 1.d0, 1.d0, 1 )
l = obj % encodedStr( )
write( *, "(DT)" ) l

end program main