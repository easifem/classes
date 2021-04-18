PROGRAM MAIN
USE BaseType
USE BaseMethod
USE GenericElement_Class

IMPLICIT NONE

!test-1
BLOCK
CLASS( ReferenceElement_ ), POINTER :: refelem
! TYPE( ReferenceLine_ ), TARGET :: refelem
TYPE( GenericElement_ ) :: obj

refelem => ReferenceLine_Pointer( )
CALL obj % Initiate( [1,2,3], 1, refelem )
CALL Display( obj, "obj" )
END BLOCK

! ! test-2
! BLOCK
!   TYPE( GenericElement_ ) :: obj
!   TYPE( ElementData_ ) :: ElemData

!   ElemData % XiDimension = 2
!   ElemData % NSD = 2
!   ElemData % ElemTopology = Triangle
!   ElemData % Mat_Type = 1

!   obj = GenericElement( [1,2,3], ElemData )
!   CALL Display( obj, "Empty obj")

! END BLOCK

! ! test-3
! BLOCK
!   TYPE( GenericElement_ ) :: obj
!   obj = GenericElement( )
!   CALL Display( obj, "Empty obj")
! END BLOCK

! ! test-4
! BLOCK
!   CLASS( GenericElement_ ), POINTER :: obj
!   obj => GenericElement_Pointer( )
!   ! ALLOCATE( obj )
!   CALL Display( obj, "Empty obj")
! END BLOCK

! ! test-5
! BLOCK
!   CLASS( GenericElement_ ), POINTER :: obj
!   TYPE( ElementData_ ) :: ElemData

!   ElemData % XiDimension = 2
!   ElemData % NSD = 2
!   ElemData % ElemTopology = Triangle
!   ElemData % Mat_Type = 1

!   obj => GenericElement_Pointer( [1,2,3], ElemData )
!   CALL Display( obj, "Empty obj")

! END BLOCK

! ! test-6
! BLOCK
!   TYPE( GenericElement_ ) :: obj, anotherobj
!   TYPE( ElementData_ ) :: ElemData

!   ElemData % XiDimension = 2
!   ElemData % NSD = 2
!   ElemData % ElemTopology = Triangle
!   ElemData % Mat_Type = 1

!   obj = GenericElement( [1,2,3], ElemData )
!   CALL Display( obj, "obj")

!   CALL Convert( From = obj, To = anotherobj )
!   CALL Display( anotherobj, "Another obj")

! END BLOCK

! ! test-6
! BLOCK
!   TYPE( GenericElement_ ) :: obj, anotherobj
!   TYPE( ElementData_ ) :: ElemData

!   ElemData % XiDimension = 2
!   ElemData % NSD = 2
!   ElemData % ElemTopology = Triangle
!   ElemData % Mat_Type = 1

!   obj = GenericElement( [1,2,3], ElemData )
!   CALL Display( obj, "obj")

!   CALL DeallocateData( obj )
!   CALL Display( anotherobj, "Another obj")

! END BLOCK

! test-6
!BLOCK
!  TYPE( GenericElement_ ) :: obj, anotherobj
!  TYPE( ElementData_ ) :: ElemData
!  LOGICAL( LGT ) :: isBndyElem

!  ElemData % XiDimension = 2
!  ElemData % NSD = 2
!  ElemData % ElemTopology = Triangle
!  ElemData % Mat_Type = 1
!  obj = GenericElement( [1,2,3], ElemData )
!  CALL Display( obj, "obj")
!  isBndyElem = obj % isBoundaryElement( )
!  CALL Display( isBndyElem, "isBoundaryElement")
!END BLOCK

END PROGRAM MAIN
