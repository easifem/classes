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

MODULE OrthogonalFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE ScalarFE_Class, ONLY: ScalarFE_
USE BaseType, ONLY: QuadraturePoint_, &
                    ElemShapedata_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: OrthogonalFE_
PUBLIC :: OrthogonalFEPointer_
PUBLIC :: OrthogonalFEPointer

PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "OrthogonalFE_Class"

!----------------------------------------------------------------------------
!                                                                OrthogonalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-13
! summary: Finite element class
!
!{!pages/docs-api/OrthogonalFE/OrthogonalFE_.md!}

TYPE, EXTENDS(ScalarFE_) :: OrthogonalFE_
END TYPE OrthogonalFE_

!----------------------------------------------------------------------------
!                                                         OrthogonalFEPointer_
!----------------------------------------------------------------------------

TYPE :: OrthogonalFEPointer_
  CLASS(OrthogonalFE_), POINTER :: ptr => NULL()
END TYPE OrthogonalFEPointer_

!----------------------------------------------------------------------------
!                                                   OrthogonalFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE OrthogonalFEPointer
  MODULE FUNCTION obj_OrthogonalFEPointer1() RESULT(ans)
    TYPE(OrthogonalFE_), POINTER :: ans
  END FUNCTION obj_OrthogonalFEPointer1
END INTERFACE OrthogonalFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of OrthogonalFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(OrthogonalFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of OrthogonalFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(OrthogonalFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OrthogonalFE_Class
