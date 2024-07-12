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

MODULE HierarchicalFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE ScalarFE_Class, ONLY: ScalarFE_

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemShapedata_

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: HierarchicalFE_
PUBLIC :: HierarchicalFEPointer_
PUBLIC :: FiniteElementDeallocate
PUBLIC :: HierarchicalFEPointer

CHARACTER(*), PARAMETER :: modName = "HierarchicalFE_Class"
CHARACTER(*), PARAMETER :: myprefix = "HierarchicalFE"

!----------------------------------------------------------------------------
!                                                                HierarchicalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-11
! summary: Finite element class

TYPE, EXTENDS(ScalarFE_) :: HierarchicalFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
END TYPE HierarchicalFE_

!----------------------------------------------------------------------------
!                                                         HierarchicalFEPointer_
!----------------------------------------------------------------------------

TYPE :: HierarchicalFEPointer_
  CLASS(HierarchicalFE_), POINTER :: ptr => NULL()
END TYPE HierarchicalFEPointer_

!----------------------------------------------------------------------------
!                                                       HierarchicalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE HierarchicalFEPointer
  MODULE FUNCTION obj_HierarchicalFEPointer(elemType, nsd, baseContinuity, &
                                cellOrder, faceOrder, edgeOrder, cellOrient, &
                                           faceOrient, edgeOrient) RESULT(ans)

    INTEGER(I4B), INTENT(IN) :: elemType
    !! Type of finite element
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! order on each cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! order on each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! order on each edge
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! orientation of each cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! orientation of each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orientation
    TYPE(HierarchicalFE_), POINTER :: ans
  END FUNCTION obj_HierarchicalFEPointer
END INTERFACE HierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of HierarchicalFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(HierarchicalFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of HierarchicalFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(HierarchicalFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                          GetPrefix@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Get prefix of the class

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(HierarchicalFE_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(HierarchicalFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data on facets

INTERFACE
  MODULE SUBROUTINE obj_GetLocalFacetElemShapeData(obj, cellElemsd, &
                                                   facetElemsd, quad)
    CLASS(HierarchicalFE_), INTENT(INOUT) :: obj
      !! finite element
    TYPE(ElemShapedata_), INTENT(INOUT) :: cellElemsd
      !! element shape data on cell
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd(:)
      !! element shapedata on facet element
      !! The size of facetElemsd should be equal to total number of
      !! facets in element.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad(:)
      !! Quadrature points on each facet element
  END SUBROUTINE obj_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoElemsd)
    CLASS(HierarchicalFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoElemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HierarchicalFE_Class
