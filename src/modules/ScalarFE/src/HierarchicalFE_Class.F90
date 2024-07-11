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

MODULE LagrangeFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE ScalarFE_Class, ONLY: ScalarFE_

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemShapedata_

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: LagrangeFE_
PUBLIC :: LagrangeFEPointer_
PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "LagrangeFE_Class"
CHARACTER(*), PARAMETER :: myprefix = "LagrangeFE"

!----------------------------------------------------------------------------
!                                                                LagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-13
! summary: Finite element class
!
!{!pages/docs-api/FiniteElement/FiniteElement_.md!}

TYPE, EXTENDS(ScalarFE_) :: LagrangeFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
END TYPE LagrangeFE_

!----------------------------------------------------------------------------
!                                                         LagrangeFEPointer_
!----------------------------------------------------------------------------

TYPE :: LagrangeFEPointer_
  CLASS(LagrangeFE_), POINTER :: ptr => NULL()
END TYPE LagrangeFEPointer_

!----------------------------------------------------------------------------
!                                                       LagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE LagrangeFEPointer
  MODULE FUNCTION obj_LagrangeFEPointer(elemType, nsd, baseContinuity, &
           baseInterpolation, ipType, basisType, alpha, beta, lambda, order, &
                                        anisoOrder) RESULT(ans)

    INTEGER(I4B), INTENT(IN) :: elemType
    !! Type of finite element
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !!
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis function family used for interpolation.
    !! LagrangeInterpolation, LagrangePolynomial
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial. It can take following
    !! values:
    !! Legendre, Chebyshev, Ultraspherical, Equidistance, Jacobi
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Basis type:
    !! Legendre, Lobatto, Ultraspherical, Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic Order of finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    TYPE(LagrangeFE_), POINTER :: ans
  END FUNCTION obj_LagrangeFEPointer
END INTERFACE LagrangeFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of LagrangeFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(LagrangeFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of LagrangeFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(LagrangeFEPointer_), ALLOCATABLE :: obj(:)
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
    CLASS(LagrangeFE_), INTENT(IN) :: obj
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
    CLASS(LagrangeFE_), INTENT(INOUT) :: obj
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
  module SUBROUTINE obj_GetLocalFacetElemShapeData(obj, cellElemsd, facetElemsd, &
                                                   quad)
    CLASS(LagrangeFE_), INTENT(INOUT) :: obj
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
    CLASS(LagrangeFE_), INTENT(INOUT) :: obj
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

END MODULE LagrangeFE_Class
