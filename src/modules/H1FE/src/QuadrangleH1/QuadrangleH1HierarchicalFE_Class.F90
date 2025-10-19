! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE QuadrangleH1HierarchicalFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE QuadrangleH1FE_Class, ONLY: QuadrangleH1FE_
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: QuadraturePoint_, ElemShapeData_
USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE

PRIVATE

PUBLIC :: QuadrangleH1HierarchicalFE_
PUBLIC :: QuadrangleH1HierarchicalFEPointer_
PUBLIC :: QuadrangleH1HierarchicalFEPointer

PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "QuadrangleH1HierarchicalFE_Class"

!----------------------------------------------------------------------------
!                                                     QuadrangleH1HierarchicalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-09
! summary:  Scalar H1 Hierarchical Finite Element

TYPE, EXTENDS(QuadrangleH1FE_) :: QuadrangleH1HierarchicalFE_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get local element shape data on quadrangle
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalFacetElemShapeData => &
    obj_GetLocalFacetElemShapeData
  !! Get local element shape data on a facet of quadrangle
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Get Global element shape data on quadrangle cell
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFacetElemShapeData => &
    obj_GetGlobalFacetElemShapeData
  !! Get Global element shape data on quadrangle and face
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order of shape functions
END TYPE QuadrangleH1HierarchicalFE_

!----------------------------------------------------------------------------
!                                              QuadrangleH1HierarchicalFEPointer_
!----------------------------------------------------------------------------

TYPE :: QuadrangleH1HierarchicalFEPointer_
  CLASS(QuadrangleH1HierarchicalFE_), POINTER :: ptr => NULL()
END TYPE QuadrangleH1HierarchicalFEPointer_

!----------------------------------------------------------------------------
!                                       QuadrangleH1HierarchicalFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE QuadrangleH1HierarchicalFEPointer
  MODULE FUNCTION obj_QuadrangleH1HierarchicalFEPointer1() RESULT(ans)
    TYPE(QuadrangleH1HierarchicalFE_), POINTER :: ans
  END FUNCTION obj_QuadrangleH1HierarchicalFEPointer1
END INTERFACE QuadrangleH1HierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                          HierarchicalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE QuadrangleH1HierarchicalFEPointer
  MODULE FUNCTION obj_QuadrangleH1HierarchicalFEPointer2( &
    cellOrder, faceOrder, nsd, cellOrient, faceOrient, &
    quadratureIsHomogeneous, quadratureType, quadratureOrder, &
    quadratureAlpha, quadratureBeta, quadratureLambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: cellOrder(:)
    !! cellOrder, the size should be 3
    INTEGER(I4B), INTENT(IN) :: faceOrder(:, :)
    !! faceOrder, the row size should be 3, and column size should be
    !! at least 4
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(3)
    !! Orientation of cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient, necessary for Hierarchical interpolation
    !! number of rows in faceorient is 3
    !! number of columns in faceorient is 4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! is quadratur homogeneous in all dimensions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! Order of quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha(:), quadratureBeta(:)
    !! Alpha and Beta for Jacobi quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda(:)
    !! Lambda for Ultraspherical quadrature
    TYPE(QuadrangleH1HierarchicalFE_), POINTER :: ans
    !! QuadrangleH1HierarchicalFE_ pointer
  END FUNCTION obj_QuadrangleH1HierarchicalFEPointer2
END INTERFACE QuadrangleH1HierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of QuadrangleH1HierarchicalFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(QuadrangleH1HierarchicalFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of QuadrangleH1HierarchicalFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(QuadrangleH1HierarchicalFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(QuadrangleH1HierarchicalFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetLocalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data in cell and facet

INTERFACE
  MODULE SUBROUTINE obj_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(QuadrangleH1HierarchicalFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd, facetElemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
  END SUBROUTINE obj_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoelemsd)
    CLASS(QuadrangleH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetGlobalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-16
! summary:  Get Global element shape data shape data in cell and facet

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
    CLASS(QuadrangleH1HierarchicalFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd, facetElemsd
    !! element shape data in cell and facet
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd, geoFacetElemsd
    !! element shape data for geometry in cell and facet
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of cell element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
  END SUBROUTINE obj_GetGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  This routine set order in the already initiated AbstractFE_
!
!# Introduction
!
! This routine sets order in the already initiated AbstractFE_
! Make sure the object is initiated by calling correct constructor methods

INTERFACE
  MODULE SUBROUTINE obj_SetOrder( &
    obj, order, anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, &
    faceOrient, edgeOrient, tCell, tFace, tEdge, errCheck)
    CLASS(QuadrangleH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! aniso tropic order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order
    !! number of rows in faceOrder is 3
    !! number of columns in faceOrder is tfaceorder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order
    !! size of edgeorder is tedgeorder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! cell orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient
    !! number of rows in faceoriient is 3
    !! number of columns in faceorient is tfaceorient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! size of cellOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! number of columns in faceOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! size of edgeorder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE QuadrangleH1HierarchicalFE_Class
