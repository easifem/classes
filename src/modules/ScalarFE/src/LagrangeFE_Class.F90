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
PUBLIC :: LagrangeFEPointer

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
!{!pages/docs-api/LagrangeFE/LagrangeFE_.md!}

TYPE, EXTENDS(ScalarFE_) :: LagrangeFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
END TYPE LagrangeFE_

!----------------------------------------------------------------------------
!                                                         LagrangeFEPointer_
!----------------------------------------------------------------------------

TYPE :: LagrangeFEPointer_
  CLASS(LagrangeFE_), POINTER :: ptr => NULL()
END TYPE LagrangeFEPointer_

!----------------------------------------------------------------------------
!                                                   LagrangeFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE LagrangeFEPointer
  MODULE FUNCTION obj_LagrangeFEPointer1() RESULT(ans)
    TYPE(LagrangeFE_), POINTER :: ans
  END FUNCTION obj_LagrangeFEPointer1
END INTERFACE LagrangeFEPointer

!----------------------------------------------------------------------------
!                                                       LagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE LagrangeFEPointer
  MODULE FUNCTION obj_LagrangeFEPointer2(elemType, nsd, baseContinuity, &
                              ipType, basisType, alpha, beta, lambda, order, &
                                         anisoOrder) RESULT(ans)

    INTEGER(I4B), INTENT(IN) :: elemType
    !! Type of finite element
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !!
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
  END FUNCTION obj_LagrangeFEPointer2
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
!
!----------------------------------------------------------------------------

END MODULE LagrangeFE_Class
