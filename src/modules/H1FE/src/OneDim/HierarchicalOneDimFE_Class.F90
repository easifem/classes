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

MODULE HierarchicalOneDimFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: QuadraturePoint_, &
                    ElemShapedata_

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: HierarchicalOneDimFE_
PUBLIC :: HierarchicalOneDimFEPointer_
PUBLIC :: FiniteElementDeallocate
PUBLIC :: HierarchicalOneDimFEPointer

CHARACTER(*), PARAMETER :: modName = "HierarchicalOneDimFE_Class"

!----------------------------------------------------------------------------
!                                                      HierarchicalOneDimFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-11
! summary: Finite element class

TYPE, EXTENDS(AbstractOneDimFE_) :: HierarchicalOneDimFE_
END TYPE HierarchicalOneDimFE_

!----------------------------------------------------------------------------
!                                               HierarchicalOneDimFEPointer_
!----------------------------------------------------------------------------

TYPE :: HierarchicalOneDimFEPointer_
  CLASS(HierarchicalOneDimFE_), POINTER :: ptr => NULL()
END TYPE HierarchicalOneDimFEPointer_

!----------------------------------------------------------------------------
!                                               HierarchicalOneDimFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE HierarchicalOneDimFEPointer
  MODULE FUNCTION obj_HierarchicalOneDimFEPointer1() RESULT(ans)
    TYPE(HierarchicalOneDimFE_), POINTER :: ans
  END FUNCTION obj_HierarchicalOneDimFEPointer1
END INTERFACE HierarchicalOneDimFEPointer

!----------------------------------------------------------------------------
!                                               HierarchicalOneDimFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE HierarchicalOneDimFEPointer
  MODULE FUNCTION obj_HierarchicalOneDimFEPointer2(baseContinuity, order) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! base continuity of the element
    TYPE(HierarchicalOneDimFE_), POINTER :: ans
  END FUNCTION obj_HierarchicalOneDimFEPointer2
END INTERFACE HierarchicalOneDimFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of HierarchicalOneDimFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(HierarchicalOneDimFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of HierarchicalOneDimFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(HierarchicalOneDimFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HierarchicalOneDimFE_Class
