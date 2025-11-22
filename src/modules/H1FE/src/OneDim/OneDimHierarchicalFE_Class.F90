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

MODULE OneDimHierarchicalFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: QuadraturePoint_, &
                    ElemShapedata_

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: OneDimHierarchicalFE_
PUBLIC :: OneDimHierarchicalFEPointer_
PUBLIC :: FiniteElementDeallocate
PUBLIC :: OneDimHierarchicalFEPointer

CHARACTER(*), PARAMETER :: modName = "OneDimHierarchicalFE_Class"

!----------------------------------------------------------------------------
!                                                      OneDimHierarchicalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-11
! summary: Finite element class

TYPE, EXTENDS(AbstractOneDimFE_) :: OneDimHierarchicalFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get local element shape data
END TYPE OneDimHierarchicalFE_

!----------------------------------------------------------------------------
!                                               OneDimHierarchicalFEPointer_
!----------------------------------------------------------------------------

TYPE :: OneDimHierarchicalFEPointer_
  CLASS(OneDimHierarchicalFE_), POINTER :: ptr => NULL()
END TYPE OneDimHierarchicalFEPointer_

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(OneDimHierarchicalFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                               OneDimHierarchicalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE OneDimHierarchicalFEPointer
  MODULE FUNCTION obj_OneDimHierarchicalFEPointer1() RESULT(ans)
    TYPE(OneDimHierarchicalFE_), POINTER :: ans
  END FUNCTION obj_OneDimHierarchicalFEPointer1
END INTERFACE OneDimHierarchicalFEPointer

!----------------------------------------------------------------------------
!                                               OneDimHierarchicalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE OneDimHierarchicalFEPointer
  MODULE FUNCTION obj_OneDimHierarchicalFEPointer2(baseContinuity, order) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! base continuity of the element
    TYPE(OneDimHierarchicalFE_), POINTER :: ans
  END FUNCTION obj_OneDimHierarchicalFEPointer2
END INTERFACE OneDimHierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of OneDimHierarchicalFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(OneDimHierarchicalFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of OneDimHierarchicalFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(OneDimHierarchicalFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimHierarchicalFE_Class
