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

MODULE TriangleH1LagrangeFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractFE_Class, ONLY: AbstractFE_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: TriangleH1LagrangeFE_
PUBLIC :: TriangleH1LagrangeFEPointer_
PUBLIC :: TriangleH1LagrangeFEPointer

PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "TriangleH1LagrangeFE_Class"

!----------------------------------------------------------------------------
!                                                       TriangleH1LagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-09
! summary:  Scalar H1 Lagrange Finite Element

TYPE, EXTENDS(AbstractFE_) :: TriangleH1LagrangeFE_
END TYPE TriangleH1LagrangeFE_

!----------------------------------------------------------------------------
!                                                TriangleH1LagrangeFEPointer_
!----------------------------------------------------------------------------

TYPE :: TriangleH1LagrangeFEPointer_
  CLASS(TriangleH1LagrangeFE_), POINTER :: ptr => NULL()
END TYPE TriangleH1LagrangeFEPointer_

!----------------------------------------------------------------------------
!                                         TriangleH1LagrangeFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE TriangleH1LagrangeFEPointer
  MODULE FUNCTION obj_TriangleH1LagrangeFEPointer1() RESULT(ans)
    TYPE(TriangleH1LagrangeFE_), POINTER :: ans
  END FUNCTION obj_TriangleH1LagrangeFEPointer1
END INTERFACE TriangleH1LagrangeFEPointer

!----------------------------------------------------------------------------
!                                         TriangleH1LagrangeFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE TriangleH1LagrangeFEPointer
  MODULE FUNCTION obj_TriangleH1LagrangeFEPointer2( &
    nsd, ipType, basisType, alpha, beta, lambda, order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type, It can take following values:
    !! Legendre, Chebyshev, Ultraspherical, Equidistance, Jacobi
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis type: Legendre, Lobatto, Ultraspherical, Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    INTEGER(I4B), INTENT(IN) :: order
    !! Isotropic Order of finite element
    TYPE(TriangleH1LagrangeFE_), POINTER :: ans
    !! TriangleH1LagrangeFE_ pointer
  END FUNCTION obj_TriangleH1LagrangeFEPointer2
END INTERFACE TriangleH1LagrangeFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of TriangleH1LagrangeFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(TriangleH1LagrangeFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of TriangleH1LagrangeFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(TriangleH1LagrangeFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TriangleH1LagrangeFE_Class
