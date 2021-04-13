! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	This module defines finite element class
MODULE Element_Class
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                  Element_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	FiniteElement Datat type
!
!{!pages/Element.md}

PUBLIC :: Element_
TYPE :: Element_
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
    !! Node pointers for an element
  INTEGER( I4B ) :: MAT_Type = 0_I4B
    !! Material type of an element
  CLASS( ReferenceElement_ ), POINTER :: RefElem => NULL( )
    !! Pointer to the ReferenceElement

  CONTAINS
  ! Constructor
  GENERIC, PUBLIC :: Initiate => m_Initiate_obj, m_initiate_from_obj
    !! Generic method for initiating the instance of [[Element_]]
  GENERIC, PUBLIC :: OPERATOR( .Nptrs. ) => getNptrs
    !! Returns Node numbers
  GENERIC, PUBLIC :: getElemShapeData =>  get_elemsd_H1_Lagrange
    !! Returns element shape data

  PROCEDURE, PUBLIC, PASS( Obj ) :: m_Initiate_obj, m_initiate_from_obj
  PROCEDURE, PUBLIC, PASS( Obj ) :: Display => m_display_Obj
  PROCEDURE, PUBLIC, PASS( Obj ) :: isBoundaryElement => m_isBoundaryElement
  PROCEDURE, PUBLIC, PASS( Obj ) :: getNptrs => m_getNptrs
  PROCEDURE, PUBLIC, PASS( Obj ) :: setNptrs => m_setNptrs
  PROCEDURE, PUBLIC, PASS( Obj ) :: setMaterialType => setMaterialType_1
  PROCEDURE, PUBLIC, PASS ( Obj ) :: get_elemsd_H1_Lagrange
    ! Virtual procedures
  PROCEDURE, PUBLIC, PASS( Obj ) :: CellNptrs => getCellNptrs
  PROCEDURE, PUBLIC, PASS( Obj ) :: PointerToCell => getPointerToCell
  PROCEDURE, PUBLIC, PASS( Obj ) :: CellPointer => getPointerToCell
  PROCEDURE, PUBLIC, PASS( Obj ) :: FacetLocalID => getFacetLocalID
  PROCEDURE, PUBLIC, PASS( Obj ) :: FacetLocalNptrs => getFacetLocalNptrs
  PROCEDURE, PUBLIC, PASS( Obj ) :: setPointerToCell
  PROCEDURE, PUBLIC, PASS( Obj ) :: FreePointerToCell
  PROCEDURE, PUBLIC, PASS( Obj ) :: setFacetLocalID
END TYPE Element_

!----------------------------------------------------------------------------
!                                                              TypeElement
!----------------------------------------------------------------------------

TYPE( Element_ ), PARAMETER, PUBLIC :: &
  & TypeElement = Element_( Nptrs = NULL( ), RefElem = NULL( ) )

!----------------------------------------------------------------------------
!                                                           ElementPointer_
!----------------------------------------------------------------------------

TYPE :: ElementPointer_
  CLASS( Element_ ), POINTER :: Ptr => NULL( )
END TYPE ElementPointer_

PUBLIC :: ElementPointer_

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Initiate an instance of [[Element_]]

INTERFACE
MODULE SUBROUTINE m_Initiate_obj( Obj, Nptrs, Mat_Type, RefElem )
  CLASS( Element_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Mat_Type
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: RefElem
END SUBROUTINE m_Initiate_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Initiate an instance of [[Element_]]

INTERFACE
MODULE SUBROUTINE m_initiate_from_obj( Obj, AnotherObj  )
  CLASS( Element_ ), INTENT( INOUT ) :: Obj
  CLASS( Element_ ), TARGET, INTENT( IN ) :: AnotherObj
END SUBROUTINE m_initiate_from_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Initiate an instance of [[Element_]]

INTERFACE
! This is a subroutine
MODULE SUBROUTINE s_Initiate_obj( Obj, Nptrs, Mat_Type, RefElem )
  CLASS( Element_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Mat_Type
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: RefElem
END SUBROUTINE s_Initiate_obj
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE s_initiate_obj
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                       Element@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Construct an instance of [[Element_]]


INTERFACE
MODULE FUNCTION Constructor1( Nptrs, Mat_Type, RefElem ) RESULT( Obj )
  TYPE( Element_ ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Mat_Type
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: RefElem
END FUNCTION Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Element@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Construct an instance of [[Element_]]

INTERFACE
MODULE FUNCTION Constructor2( ) RESULT( Obj )
  TYPE( Element_ ) :: Obj
END FUNCTION Constructor2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Element@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Construct an instance of [[Element_]]

INTERFACE
MODULE FUNCTION Constructor3( AnotherObj ) RESULT( Obj )
  TYPE( Element_ ) :: Obj
  CLASS( Element_ ), TARGET, INTENT( IN ) :: AnotherObj
END FUNCTION Constructor3
END INTERFACE

INTERFACE Element
  MODULE PROCEDURE Constructor1, Constructor2, Constructor3
END INTERFACE Element

PUBLIC :: Element

!----------------------------------------------------------------------------
!                                                            Element_Pointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns a pointer to an instance of [[Element_]]

INTERFACE
MODULE FUNCTION Constructor_1( Nptrs, Mat_Type, RefElem ) RESULT( Obj )
  CLASS( Element_ ), POINTER :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Mat_Type
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: RefElem
END FUNCTION Constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Element_Pointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns a pointer to an instance of [[Element_]]

INTERFACE
MODULE FUNCTION Constructor_2( ) RESULT( Obj )
  CLASS( Element_ ), POINTER :: Obj
  ! ALLOCATE( Obj )
  ! Obj % Mat_Type = -1
  ! Obj % Nptrs = [-1]
  ! Obj % RefElem => NULL( )
END FUNCTION Constructor_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Element_Pointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns a pointer to an instance of [[Element_]]

INTERFACE
MODULE FUNCTION Constructor_3( AnotherObj ) RESULT( Obj )
  CLASS( Element_ ), POINTER :: Obj
  CLASS( Element_ ), TARGET, INTENT( IN ) :: AnotherObj
END FUNCTION Constructor_3
END INTERFACE

!----------------------------------------------------------------------------
!                                               Element_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE Element_Pointer
  MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
END INTERFACE Element_Pointer

PUBLIC :: Element_Pointer


!----------------------------------------------------------------------------
!                                                 setMaterialType@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	set the material property of an element

INTERFACE
MODULE PURE SUBROUTINE setMaterialType_1( Obj, MatType  )
  CLASS( Element_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: MatType
END SUBROUTINE setMaterialType_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Deallocate the data stored inside [[Element_]]

INTERFACE
  MODULE PURE SUBROUTINE Deallocate_Data( Obj )
    TYPE( Element_ ), INTENT( INOUT ) :: Obj
  END SUBROUTINE Deallocate_Data
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                              isBoundaryElement@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns true if the element is a boundary element

INTERFACE
  MODULE PURE FUNCTION m_isBoundaryElement( Obj, NSD ) RESULT( Ans )
    CLASS( Element_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: NSD
    LOGICAL( LGT ) :: Ans
  END FUNCTION m_isBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                        getNptrs@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the node numbers and connectivity

INTERFACE
MODULE PURE FUNCTION m_getNptrs( Obj ) RESULT( Nptrs )
  CLASS( Element_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), ALLOCATABLE ::  Nptrs( : )
END FUNCTION m_getNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setNptrs@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Set the node number and connnectivity

INTERFACE
MODULE PURE SUBROUTINE m_setNptrs( Obj, Nptrs )
  CLASS( Element_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
END SUBROUTINE m_setNptrs
END INTERFACE


!----------------------------------------------------------------------------
!                                                               Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	display the content of [[Element_]]

INTERFACE
MODULE SUBROUTINE m_display_Obj( Obj, msg, UnitNo, FullDisp )
  CLASS( Element_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: FullDisp
END SUBROUTINE m_display_Obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	display the content of [[Element_]]

INTERFACE
MODULE SUBROUTINE s_display_Obj( Obj, msg, UnitNo, FullDisp )
  TYPE( Element_ ), INTENT( IN ) :: Obj
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
!                                                 getElemShapeData@ShapeData
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the element shape data

INTERFACE
MODULE PURE SUBROUTINE get_elemsd_H1_Lagrange( Obj, ElemSD, Quad, xiJ, &
  & ContinuityType, InterpolType )
  CLASS( Element_ ), INTENT( INOUT) :: Obj
  CLASS( ElemShapeData_ ), INTENT( INOUT) :: ElemSD
  TYPE( QuadraturePoint_ ), INTENT( IN ) :: Quad
  REAL( DFP ), INTENT( IN ) :: xIJ( :, : )
  TYPE( H1_ ), INTENT( IN ) :: ContinuityType
  TYPE( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE get_elemsd_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getCellNptrs@Virtual
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the cell pointer

INTERFACE
MODULE PURE FUNCTION getCellNptrs( Obj ) RESULT( Ans )
  CLASS( Element_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION getCellNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetPointerToCell@Virtual
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	set the pointer to the cell object

INTERFACE
MODULE PURE SUBROUTINE SetPointerToCell( Obj, CellObj )
  CLASS( Element_ ), INTENT( INOUT ) :: Obj
  CLASS( Element_ ), INTENT( INOUT ), TARGET :: CellObj
END SUBROUTINE SetPointerToCell
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getPointerToCell@Virtual
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns pointer to cell

INTERFACE
MODULE FUNCTION getPointerToCell( Obj ) RESULT( CellObj )
  CLASS( Element_ ), INTENT( IN ), TARGET :: Obj
  CLASS( Element_ ), POINTER :: CellObj
END FUNCTION getPointerToCell
END INTERFACE

!----------------------------------------------------------------------------
!                                                  FreePointerToCell@Virtual
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Free the pointer to cell

INTERFACE
  MODULE PURE SUBROUTINE FreePointerToCell( Obj )
  CLASS( Element_ ), INTENT( INOUT ) :: Obj
  END SUBROUTINE FreePointerToCell
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getFacetLocalID@Virtual
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns facet local id

INTERFACE
MODULE PURE FUNCTION getFacetLocalID( Obj ) RESULT( Ans )
  CLASS( Element_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION getFacetLocalID
END INTERFACE

!----------------------------------------------------------------------------
!                                                   setFacetLocalID@Virtual
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: set the facet localID

INTERFACE
  MODULE PURE SUBROUTINE setFacetLocalID( Obj, Id )
  CLASS( Element_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Id
  END SUBROUTINE setFacetLocalID
END INTERFACE

!----------------------------------------------------------------------------
!                                                getFacetLocalNptrs@Virtual
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	get the facet local nptrs

INTERFACE
MODULE PURE FUNCTION getFacetLocalNptrs( Obj ) RESULT( Nptrs )
  CLASS( Element_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
END FUNCTION getFacetLocalNptrs
END INTERFACE

END MODULE Element_Class