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

MODULE OneDimLagrangeFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: QuadraturePoint_, ElemShapedata_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: OneDimLagrangeFE_
PUBLIC :: OneDimLagrangeFEPointer_
PUBLIC :: OneDimLagrangeFEPointer
PUBLIC :: OneDimLagrangeFE

PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "OneDimLagrangeFE_Class"

!----------------------------------------------------------------------------
!                                                          OneDimLagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: OneDimLagrangeFE is lagrange fe for one dimensional cases

TYPE, EXTENDS(AbstractOneDimFE_) :: OneDimLagrangeFE_
END TYPE OneDimLagrangeFE_

!----------------------------------------------------------------------------
!                                                   OneDimLagrangeFEPointer_
!----------------------------------------------------------------------------

TYPE :: OneDimLagrangeFEPointer_
  CLASS(OneDimLagrangeFE_), POINTER :: ptr => NULL()
END TYPE OneDimLagrangeFEPointer_

!----------------------------------------------------------------------------
!                                           OneDimLagrangeFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE OneDimLagrangeFEPointer
  MODULE FUNCTION obj_OneDimLagrangeFEPointer1() RESULT(ans)
    TYPE(OneDimLagrangeFE_), POINTER :: ans
  END FUNCTION obj_OneDimLagrangeFEPointer1
END INTERFACE OneDimLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                       OneDimLagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE OneDimLagrangeFEPointer
  MODULE FUNCTION obj_OneDimLagrangeFEPointer2( &
    baseContinuity, ipType, basisType, order, alpha, beta, lambda) &
    RESULT(ans)
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
    TYPE(OneDimLagrangeFE_), POINTER :: ans
  END FUNCTION obj_OneDimLagrangeFEPointer2
END INTERFACE OneDimLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                   OneDimLagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE OneDimLagrangeFE
  MODULE FUNCTION obj_OneDimLagrangeFE( &
    baseContinuity, ipType, basisType, order, alpha, beta, lambda) &
    RESULT(ans)
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
    TYPE(OneDimLagrangeFE_) :: ans
  END FUNCTION obj_OneDimLagrangeFE
END INTERFACE OneDimLagrangeFE

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of OneDimLagrangeFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(OneDimLagrangeFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of OneDimLagrangeFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(OneDimLagrangeFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimLagrangeFE_Class
