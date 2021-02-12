PROGRAM MAIN
USE BaseType
USE BaseMethod
USE GenericElement_Class

IMPLICIT NONE

!test-1
BLOCK
CLASS( ReferenceElement_ ), POINTER :: RefElem
! TYPE( ReferenceLine_ ), TARGET :: RefElem
TYPE( GenericElement_ ) :: Obj

RefElem => ReferenceLine_Pointer( )
CALL Obj % Initiate( [1,2,3], 1, RefElem )
CALL Display( Obj, "Obj" )
END BLOCK

! ! test-2
! BLOCK
!   TYPE( GenericElement_ ) :: Obj
!   TYPE( ElementData_ ) :: ElemData

!   ElemData % XiDimension = 2
!   ElemData % NSD = 2
!   ElemData % ElemTopology = Triangle
!   ElemData % Mat_Type = 1

!   Obj = GenericElement( [1,2,3], ElemData )
!   CALL Display( Obj, "Empty Obj")

! END BLOCK

! ! test-3
! BLOCK
!   TYPE( GenericElement_ ) :: Obj
!   Obj = GenericElement( )
!   CALL Display( Obj, "Empty Obj")
! END BLOCK

! ! test-4
! BLOCK
!   CLASS( GenericElement_ ), POINTER :: Obj
!   Obj => GenericElement_Pointer( )
!   ! ALLOCATE( Obj )
!   CALL Display( Obj, "Empty Obj")
! END BLOCK

! ! test-5
! BLOCK
!   CLASS( GenericElement_ ), POINTER :: Obj
!   TYPE( ElementData_ ) :: ElemData

!   ElemData % XiDimension = 2
!   ElemData % NSD = 2
!   ElemData % ElemTopology = Triangle
!   ElemData % Mat_Type = 1

!   Obj => GenericElement_Pointer( [1,2,3], ElemData )
!   CALL Display( Obj, "Empty Obj")

! END BLOCK

! ! test-6
! BLOCK
!   TYPE( GenericElement_ ) :: Obj, AnotherObj
!   TYPE( ElementData_ ) :: ElemData

!   ElemData % XiDimension = 2
!   ElemData % NSD = 2
!   ElemData % ElemTopology = Triangle
!   ElemData % Mat_Type = 1

!   Obj = GenericElement( [1,2,3], ElemData )
!   CALL Display( Obj, "Obj")

!   CALL Convert( From = Obj, To = AnotherObj )
!   CALL Display( AnotherObj, "Another Obj")

! END BLOCK

! ! test-6
! BLOCK
!   TYPE( GenericElement_ ) :: Obj, AnotherObj
!   TYPE( ElementData_ ) :: ElemData

!   ElemData % XiDimension = 2
!   ElemData % NSD = 2
!   ElemData % ElemTopology = Triangle
!   ElemData % Mat_Type = 1

!   Obj = GenericElement( [1,2,3], ElemData )
!   CALL Display( Obj, "Obj")

!   CALL DeallocateData( Obj )
!   CALL Display( AnotherObj, "Another Obj")

! END BLOCK

! test-6
!BLOCK
!  TYPE( GenericElement_ ) :: Obj, AnotherObj
!  TYPE( ElementData_ ) :: ElemData
!  LOGICAL( LGT ) :: isBndyElem

!  ElemData % XiDimension = 2
!  ElemData % NSD = 2
!  ElemData % ElemTopology = Triangle
!  ElemData % Mat_Type = 1
!  Obj = GenericElement( [1,2,3], ElemData )
!  CALL Display( Obj, "Obj")
!  isBndyElem = Obj % isBoundaryElement( )
!  CALL Display( isBndyElem, "isBoundaryElement")
!END BLOCK

END PROGRAM MAIN
