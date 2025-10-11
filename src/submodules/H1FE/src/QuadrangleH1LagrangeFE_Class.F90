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

MODULE QuadrangleH1LagrangeFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractFE_Class, ONLY: AbstractFE_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: QuadrangleH1LagrangeFE_
PUBLIC :: QuadrangleH1LagrangeFEPointer_
PUBLIC :: QuadrangleH1LagrangeFEPointer

PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "QuadrangleH1LagrangeFE_Class"

!----------------------------------------------------------------------------
!                                                     QuadrangleH1LagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-09
! summary:  Scalar H1 Lagrange Finite Element

TYPE, EXTENDS(AbstractFE_) :: QuadrangleH1LagrangeFE_
END TYPE QuadrangleH1LagrangeFE_

!----------------------------------------------------------------------------
!                                             QuadrangleH1LagrangeFEPointer_
!----------------------------------------------------------------------------

TYPE :: QuadrangleH1LagrangeFEPointer_
  CLASS(QuadrangleH1LagrangeFE_), POINTER :: ptr => NULL()
END TYPE QuadrangleH1LagrangeFEPointer_

!----------------------------------------------------------------------------
!                                       QuadrangleH1LagrangeFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE QuadrangleH1LagrangeFEPointer
  MODULE FUNCTION obj_QuadrangleH1LagrangeFEPointer1() RESULT(ans)
    TYPE(QuadrangleH1LagrangeFE_), POINTER :: ans
  END FUNCTION obj_QuadrangleH1LagrangeFEPointer1
END INTERFACE QuadrangleH1LagrangeFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of QuadrangleH1LagrangeFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(QuadrangleH1LagrangeFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of QuadrangleH1LagrangeFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(QuadrangleH1LagrangeFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE QuadrangleH1LagrangeFE_Class
