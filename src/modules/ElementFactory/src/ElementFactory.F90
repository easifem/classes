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

MODULE ElementFactory
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE Element_Class
USE FacetElement_Class
IMPLICIT NONE
PRIVATE
PUBLIC :: Element_, ElementPointer_, Element, Element_Pointer, TypeElement
PUBLIC :: FacetElement_, FacetElementPointer_, FacetElement, FacetElement_Pointer, TypeFacetElement
CHARACTER( LEN=* ), PARAMETER :: modName="FACETELEMENT_CLASS"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Factory
  MODULE PROCEDURE elem_factory_elem, elem_factory_from_fpl
END INTERFACE Factory

PUBLIC :: Factory

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Display
  MODULE PROCEDURE elem_factor_display
END INTERFACE Display

PUBLIC :: Display

CONTAINS

!----------------------------------------------------------------------------
!                                                              getFEPointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Dynamically returns the finite element

FUNCTION elem_factory_from_fpl( param, refelem ) RESULT( ans )
  ! Define internal variable
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: refelem
  CLASS( Element_ ), POINTER :: ans
  ! Define internal variables
  INTEGER( I4B ) :: ierr
  CHARACTER( LEN=* ), PARAMETER :: myName="elem_factory_from_fpl()"
  CHARACTER( LEN=100 ) :: elemTypeName
  TYPE( String ) :: elemType

  IF( .NOT. param%ispresent(key="type") ) THEN
    CALL eElement%raiseError(modName//"::"//myName//" - "// &
        "'type' keyword should be present")
  ELSE
    ierr = param%get(key="type", value=elemTypeName)
    elemType = String(elemTypeName)
    elemType = elemType%lower()
  END IF
  SELECT CASE( TRIM(elemType%chars()) )
  CASE( "element_", "element" )
    ALLOCATE( Element_::ans )
    CALL ans%initiate(param=param, refelem=refelem)
  CASE( "facetelement_", "facetelement" )
    ALLOCATE( FacetElement_::ans )
    CALL ans%initiate(param=param, refelem=refelem)
  END SELECT
  CALL elemType%free()
END FUNCTION elem_factory_from_fpl

!----------------------------------------------------------------------------
!                                                                 Factory
!----------------------------------------------------------------------------

FUNCTION elem_factory_elem( obj ) RESULT( ans )
  CLASS( Element_ ), INTENT( IN ) :: obj
  CLASS( Element_ ), POINTER :: ans
  ! Define internal type
  CHARACTER( LEN=* ), PARAMETER :: myName="elem_factory_elem()"
  SELECT TYPE( obj )
  TYPE IS( Element_ )
    ALLOCATE( Element_ :: ans )
    CALL ans%Initiate( obj )
  TYPE IS( FacetElement_ )
    ALLOCATE( FacetElement_ :: ans )
    CALL ans%Initiate( obj )
  CLASS DEFAULT
    CALL eElement%raiseError(modName//"::"//myName//" - "// &
        "unknown type found")
  END SELECT
END FUNCTION elem_factory_elem

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

SUBROUTINE elem_factor_display( obj, msg, unitno, FullDisp )
  CLASS( Element_ ), INTENT( IN ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: FullDisp
  ! Define internal variables
  CHARACTER( LEN=* ), PARAMETER :: myName="elem_factor_display()"
  SELECT TYPE( obj )
  TYPE IS( Element_ )
    CALL obj%display(msg=msg, unitno=unitno, FullDisp=FullDisp)
  TYPE IS( FacetElement_ )
    CALL obj%display(msg=msg, unitno=unitno, FullDisp=FullDisp)
  CLASS DEFAULT
    CALL eElement%raiseError(modName//"::"//myName//" - "// &
        "unknown type found")
  END SELECT
END SUBROUTINE elem_factor_display

END MODULE ElementFactory

!----------------------------------------------------------------------------
!                                                 getElemShapeData@ShapeData
!----------------------------------------------------------------------------

! !> authors: Vikas Sharma, Ph. D.
! ! date: 	24 March 2021
! ! summary: 	Returns the element shape data

! INTERFACE
! MODULE PURE SUBROUTINE elem_get_elemsd_H1_Lagrange( obj, ElemSD, Quad, xiJ, &
!   & ContinuityType, InterpolType )
!   CLASS( Element_ ), INTENT( INOUT ) :: obj
!   CLASS( ElemShapeData_ ), INTENT( INOUT ) :: ElemSD
!   TYPE( QuadraturePoint_ ), INTENT( IN ) :: Quad
!   REAL( DFP ), INTENT( IN ) :: xIJ( :, : )
!   TYPE( H1_ ), INTENT( IN ) :: ContinuityType
!   TYPE( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
! END SUBROUTINE elem_get_elemsd_H1_Lagrange
! END INTERFACE


! MODULE PROCEDURE get_elemsd_H1_Lagrange
!   CALL initiate( obj = ElemSD, Quad = Quad, &
!     & refelem = obj%refelem, &
!     & ContinuityType= typeH1, &
!     & InterpolType = TypeLagrangeInterpolation )
!   CALL setValue( obj = ElemSD, Val = XiJ, N =ElemSD%N, dNdXi=ElemSD%dNdXi )
! END PROCEDURE get_elemsd_H1_Lagrange


!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: Returns [[ElemShapeData_]]

! INTERFACE
! MODULE PURE SUBROUTINE get_elemsd_H1_Lagrange( obj, ElemSD, Quad, xiJ, ContinuityType, InterpolType )
!   CLASS( FacetElement_ ), INTENT( INOUT ) :: obj
!   CLASS( ElemShapeData_ ), INTENT( INOUT ) :: ElemSD
!   TYPE( QuadraturePoint_ ), INTENT( IN ) :: Quad
!   REAL( DFP ), INTENT( IN ) :: xIJ( :, : )
!   TYPE( H1_ ), INTENT( IN ) :: ContinuityType
!   TYPE( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
! END SUBROUTINE get_elemsd_H1_Lagrange
! END INTERFACE