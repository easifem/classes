program main
use globaldata
use basetype
use basemethod
implicit none

! block
! 	type( rank2tensor_ ) :: obj
! 	real( dfp ) :: T( 3, 3 ), v3( 3 )

! 	call RANDOM_NUMBER( T )

! 	call initiate( obj, T )
! 	call display( obj, "obj = " )

! 	call random_number( v3 )
! 	call initiate( obj, v3, StressTypeVoigt )
! 	call display( obj, "obj = " )
! end block


! block
! 	type( rank2tensor_ ) :: obj
! 	real( dfp ) :: T( 3, 3 )

! 	call RANDOM_NUMBER( T )

! 	obj = T
! 	call display( obj, "obj = " )

! 	T = obj
! 	call displayarray( T, "T" )

! end block

end program main
