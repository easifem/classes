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

MODULE LagrangeOneDimFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE ScalarOneDimFE_Class, ONLY: ScalarOneDimFE_

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemShapedata_

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: LagrangeOneDimFE_
PUBLIC :: LagrangeOneDimFEPointer_
PUBLIC :: LagrangeOneDimFEPointer
PUBLIC :: LagrangeOneDimFE

PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "LagrangeOneDimFE_Class"
CHARACTER(*), PARAMETER :: myprefix = "LagrangeOneDimFE"

!----------------------------------------------------------------------------
!                                                          LagrangeOneDimFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: LagrangeOneDimFE is lagrange fe for one dimensional cases
!
!{!pages/docs-api/LagrangeOneDimFE/LagrangeOneDimFE_.md!}

TYPE, EXTENDS(ScalarOneDimFE_) :: LagrangeOneDimFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
END TYPE LagrangeOneDimFE_

!----------------------------------------------------------------------------
!                                                   LagrangeOneDimFEPointer_
!----------------------------------------------------------------------------

TYPE :: LagrangeOneDimFEPointer_
  CLASS(LagrangeOneDimFE_), POINTER :: ptr => NULL()
END TYPE LagrangeOneDimFEPointer_

!----------------------------------------------------------------------------
!                                           LagrangeOneDimFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE LagrangeOneDimFEPointer
  MODULE FUNCTION obj_LagrangeOneDimFEPointer1() RESULT(ans)
    TYPE(LagrangeOneDimFE_), POINTER :: ans
  END FUNCTION obj_LagrangeOneDimFEPointer1
END INTERFACE LagrangeOneDimFEPointer

!----------------------------------------------------------------------------
!                                                       LagrangeOneDimFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE LagrangeOneDimFEPointer
  MODULE FUNCTION obj_LagrangeOneDimFEPointer2(baseContinuity, &
                    ipType, basisType, order, alpha, beta, lambda) RESULT(ans)

    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! base continuity of the finite element
    !! read in BasisOpt_Class
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial.
    !! Read more infor in BasisOpt_Class
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Basis type:
    !! Legendre, Lobatto, Ultraspherical, Jacobi, Monomial
    INTEGER(I4B), INTENT(IN) :: order
    !! Isotropic Order of finite element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    TYPE(LagrangeOneDimFE_), POINTER :: ans
  END FUNCTION obj_LagrangeOneDimFEPointer2
END INTERFACE LagrangeOneDimFEPointer

!----------------------------------------------------------------------------
!                                                   LagrangeOneDimFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE LagrangeOneDimFE
  MODULE FUNCTION obj_LagrangeOneDimFE(baseContinuity, &
                    ipType, basisType, order, alpha, beta, lambda) RESULT(ans)

    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! base continuity of the finite element
    !! read in BasisOpt_Class
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial.
    !! Read more infor in BasisOpt_Class
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Basis type:
    !! Legendre, Lobatto, Ultraspherical, Jacobi, Monomial
    INTEGER(I4B), INTENT(IN) :: order
    !! Isotropic Order of finite element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    TYPE(LagrangeOneDimFE_) :: ans
  END FUNCTION obj_LagrangeOneDimFE
END INTERFACE LagrangeOneDimFE

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of LagrangeOneDimFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(LagrangeOneDimFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of LagrangeOneDimFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(LagrangeOneDimFEPointer_), ALLOCATABLE :: obj(:)
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
    CLASS(LagrangeOneDimFE_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LagrangeOneDimFE_Class
