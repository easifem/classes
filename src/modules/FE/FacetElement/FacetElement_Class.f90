MODULE FacetElement_Class
USE GlobalData
USE BaseType
USE Element_Class

PRIVATE

!----------------------------------------------------------------------------
!                                                              FacetElement_
!----------------------------------------------------------------------------

TYPE, EXTENDS( Element_ ) :: FacetElement_
  INTEGER( I4B ) :: LocalID
  CLASS( Element_ ), POINTER :: Cell => NULL( )
  CLASS( Element_ ), POINTER :: OuterCell => NULL( )

  CONTAINS
  ! Constructor
  PROCEDURE, PUBLIC, PASS( Obj ) :: CellNptrs => getCellNptrs
  PROCEDURE, PUBLIC, PASS( Obj ) :: PointerToCell => getPointerToCell
  PROCEDURE, PUBLIC, PASS( Obj ) :: FacetLocalID => getFacetLocalID
  PROCEDURE, PUBLIC, PASS( Obj ) :: FacetLocalNptrs => getFacetLocalNptrs
  PROCEDURE, PUBLIC, PASS( Obj ) :: setPointerToCell
  PROCEDURE, PUBLIC, PASS( Obj ) :: FreePointerToCell
  PROCEDURE, PUBLIC, PASS( Obj ) :: setFacetLocalID
  PROCEDURE, PUBLIC, PASS( Obj ) :: Display => m_display_Obj

  ! ! getElemShapeData.part
  PROCEDURE, PUBLIC, PASS( Obj ) :: get_elemsd_H1_Lagrange
  ! PROCEDURE, PUBLIC, PASS( Obj ) :: getElemShapeData_From_XiJ2

END TYPE FacetElement_

PUBLIC :: FacetElement_

!----------------------------------------------------------------------------
!                                                            TypeFacetElement
!----------------------------------------------------------------------------

TYPE( FacetElement_ ), PARAMETER, PUBLIC :: &
  & TypeFacetElement = FacetElement_(Mat_Type = 0_I4B, &
  & RefElem = NULL(), LocalID = 0_I4B)

!----------------------------------------------------------------------------
!                                                        FacetElementPointer_
!----------------------------------------------------------------------------
TYPE :: FacetElementPointer_
  CLASS( FacetElementPointer_ ), POINTER :: Ptr => NULL( )
END TYPE FacetElementPointer_

PUBLIC :: FacetElementPointer_

!----------------------------------------------------------------------------
!                                                   FacetElement@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor1( Nptrs, Mat_Type, RefElem ) RESULT( Obj )
  TYPE( FacetElement_ ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Mat_Type
  CLASS( ReferenceElement_ ), TARGET, INTENT( INOUT ) :: RefElem
END FUNCTION Constructor1

MODULE FUNCTION Constructor2( ) RESULT( Obj )
  TYPE( FacetElement_ ) :: Obj
END FUNCTION Constructor2

MODULE FUNCTION Constructor3( AnotherObj ) RESULT( Obj )
  TYPE( FacetElement_ ) :: Obj
  CLASS( Element_ ), TARGET, INTENT( INOUT ) :: AnotherObj
END FUNCTION Constructor3
END INTERFACE

INTERFACE FacetElement
  MODULE PROCEDURE Constructor1, Constructor2, Constructor3
END INTERFACE

PUBLIC :: FacetElement

!------------------------------------------------------------------------------
!                                                                 FacetElement
!------------------------------------------------------------------------------


INTERFACE FacetElement_Pointer
  MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
END INTERFACE

PUBLIC :: FacetElement_Pointer

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE m_display_Obj( Obj, msg, UnitNo, FullDisp )
  CLASS( FacetElement_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: FullDisp
END SUBROUTINE m_display_Obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE s_display_Obj( Obj, msg, UnitNo, FullDisp )
  TYPE( FacetElement_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: FullDisp
END SUBROUTINE s_display_Obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE s_display_Obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Deallocate_Data( Obj )
  TYPE( FacetElement_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Deallocate_Data
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!------------------------------------------------------------------------------
!                                                    getCellNptrs@Constructor
!------------------------------------------------------------------------------

! Get Obj % Cell % Nptrs
INTERFACE
MODULE PURE FUNCTION getCellNptrs( Obj ) RESULT( Ans )
  CLASS( FacetElement_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION getCellNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetPointerToCell@Constructor
!----------------------------------------------------------------------------

! Set Obj % Cell
INTERFACE
MODULE PURE SUBROUTINE SetPointerToCell( Obj, CellObj )
  CLASS( FacetElement_ ), INTENT( INOUT ) :: Obj
  CLASS( Element_ ), INTENT( INOUT ), TARGET :: CellObj
END SUBROUTINE SetPointerToCell
END INTERFACE

!----------------------------------------------------------------------------
!                                               getPointerToCell@Constructor
!----------------------------------------------------------------------------

!Get Obj % Cell
INTERFACE
MODULE FUNCTION getPointerToCell( Obj ) RESULT( CellObj )
  CLASS( FacetElement_ ), INTENT( IN ), TARGET :: Obj
  CLASS( Element_ ), POINTER :: CellObj
END FUNCTION getPointerToCell
END INTERFACE

!------------------------------------------------------------------------------
!                                                FreePointerToCell@Constructor
!------------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE FreePointerToCell( Obj )
  CLASS( FacetElement_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE FreePointerToCell
END INTERFACE

!------------------------------------------------------------------------------
!                                                  getFacetLocalID@Constructor
!------------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getFacetLocalID( Obj ) RESULT( Ans )
  CLASS( FacetElement_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION getFacetLocalID
END INTERFACE

!------------------------------------------------------------------------------
!                                                  setFacetLocalID@Constructor
!------------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE setFacetLocalID( Obj, Id )
  CLASS( FacetElement_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Id
END SUBROUTINE setFacetLocalID
END INTERFACE

!------------------------------------------------------------------------------
!                                              getFacetLocalNptrs@Constructor
!------------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getFacetLocalNptrs( Obj ) RESULT( Nptrs )
  CLASS( FacetElement_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
END FUNCTION getFacetLocalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getElemShapeData@ShapeData
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE get_elemsd_H1_Lagrange( Obj, ElemSD, Quad, xiJ, &
  & ContinuityType, InterpolType )
  CLASS( FacetElement_ ), INTENT( INOUT) :: Obj
  CLASS( ElemShapeData_ ), INTENT( INOUT) :: ElemSD
  TYPE( QuadraturePoint_ ), INTENT( IN ) :: Quad
  REAL( DFP ), INTENT( IN ) :: xIJ( :, : )
  TYPE( H1_ ), INTENT( IN ) :: ContinuityType
  TYPE( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE get_elemsd_H1_Lagrange
END INTERFACE

!------------------------------------------------------------------------------
!                                                         Contains@Constructor
!------------------------------------------------------------------------------

CONTAINS

!------------------------------------------------------------------------------
!                                                              Element_Pointer
!------------------------------------------------------------------------------

FUNCTION Constructor_1( Nptrs, Mat_Type, RefElem ) RESULT( Obj )
  CLASS( FacetElement_ ), POINTER :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Mat_Type
  CLASS( ReferenceElement_ ), TARGET, INTENT( INOUT ) :: RefElem

  ALLOCATE( Obj )
  CALL Obj % Initiate( Nptrs = Nptrs, Mat_Type = Mat_Type, RefElem = RefElem )
END FUNCTION Constructor_1

!------------------------------------------------------------------------------
!                                                              Element_Pointer
!------------------------------------------------------------------------------

FUNCTION Constructor_2( ) RESULT( Obj )
  CLASS( FacetElement_ ), POINTER :: Obj
  ALLOCATE( Obj )
  Obj % Mat_Type = -1
  Obj % Nptrs = [-1]
  Obj % RefElem => NULL( )
END FUNCTION Constructor_2

!------------------------------------------------------------------------------
!                                                             Element_Pointer
!------------------------------------------------------------------------------

FUNCTION Constructor_3( AnotherObj ) RESULT( Obj )
  CLASS( FacetElement_ ), POINTER :: Obj
  CLASS( FacetElement_ ), TARGET, INTENT( INOUT ) :: AnotherObj
  ALLOCATE( Obj )
  CALL Obj % Initiate( AnotherObj )
END FUNCTION Constructor_3

END MODULE FacetElement_Class
