PROGRAM MAIN
USE BaseType
USE BaseMethod
USE Element_Class

IMPLICIT NONE

!test-1
BLOCK
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Obj

RefElem => ReferenceLine_Pointer( )
CALL Obj % Initiate( [1,2,3], 1, RefElem )
CALL Display( Obj, "Obj" )
END BLOCK

! !test-1
! BLOCK
! CLASS( ReferenceElement_ ), POINTER :: RefElem
! CLASS( GenericElement_ ), POINTER :: Obj

! RefElem => ReferenceLine_Pointer( )
! Obj => Element_Pointer( [1,2,3], 1, RefElem )

! SELECT TYPE( Obj )
!   TYPE IS ( GenericElement_ )
!   CALL Display( Obj, "Obj" )
!   TYPE IS ( Element_ )
!   CALL Display( Obj, "Obj" )
! END SELECT

! END BLOCK

END PROGRAM MAIN
