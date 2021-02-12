MODULE gmshPoint_Class
  !! This module defines a class for gmsh points

USE GlobalData
USE BaseType
USE gmshEntity_Class

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                 gmshPoint_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! [[gmshPoint_]] type represent point entities in gmsh

TYPE, EXTENDS( gmshEntity_ ) :: gmshPoint_
  REAL( DFP ) :: x = 0.0
  REAL( DFP ) :: y = 0.0
  REAL( DFP ) :: z = 0.0
  REAL( DFP ) :: lc = 0.0
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: encodedStr => point_encode
END TYPE gmshPoint_

PUBLIC :: gmshPoint_

TYPE :: gmshPointPointer_
  CLASS( gmshPoint_ ), POINTER :: Ptr => Null( )
END TYPE gmshPointPointer_

PUBLIC :: gmshPointPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE gmshPoint_Pointer
  MODULE PROCEDURE constructor_1
END INTERFACE gmshPoint_Pointer

PUBLIC :: gmshPoint_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                           gmsPoint_Pointer
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function [[gmshpoint_pointer]] returns pointer to [[gmshPoint_]]

FUNCTION constructor_1 ( x, y, z, lc, uid ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: x, y ,z, lc
  INTEGER( I4B ), INTENT( IN ) :: uid
  CLASS( gmshPoint_  ), POINTER :: Ans

  ALLOCATE( Ans )
  Ans % x = x
  Ans % y = y
  Ans % z = z
  Ans % lc = lc
  Ans % uid = uid
  Ans % dim = 0
END FUNCTION constructor_1

!----------------------------------------------------------------------------
!                                                                 encodeStr
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function encode the information stored in [[gmshPoint_]] in a string
! data type and return it for printing

FUNCTION point_encode( Obj ) RESULT( Ans )
  CLASS( gmshPoint_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans

  IF( obj % lc .GT. 0.0_DFP ) THEN
    ans = &
      & "Point( " // &
      & trim( str( obj % uid, no_sign=.true. ) ) // &
      & " ) = { " // &
      & trim( str( obj % x ) ) // &
      & ", " // &
      & trim( str( obj % y ) ) // &
      & ", " // &
      & trim( str( obj % z ) ) // &
      & ", " // &
      & trim( str( obj % lc ) ) // &
      & " };"
  ELSE
  ans = &
      & "Point( " // &
      & trim( str( obj % uid, no_sign=.true. ) ) // &
      & " ) = { " // &
      & trim( str( obj % x ) ) // &
      & ", " // &
      & trim( str( obj % y ) ) // &
      & ", " // &
      & trim( str( obj % z ) ) // &
      & " };"
  END IF
END FUNCTION point_encode


END MODULE gmshPoint_Class