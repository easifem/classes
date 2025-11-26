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

MODULE OneDimOrthogonalFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: QuadraturePoint_, &
                    ElemShapedata_

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: OneDimOrthogonalFE_
PUBLIC :: OneDimOrthogonalFEPointer_
PUBLIC :: FiniteElementDeallocate
PUBLIC :: OneDimOrthogonalFEPointer

CHARACTER(*), PARAMETER :: modName = "OneDimOrthogonalFE_Class"

!----------------------------------------------------------------------------
!                                                      OneDimOrthogonalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-11
! summary: Finite element class

TYPE, EXTENDS(AbstractOneDimFE_) :: OneDimOrthogonalFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get local element shape data
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalFacetElemShapeData => &
    obj_GetLocalFacetElemShapeData
  !! Get local element shape data
END TYPE OneDimOrthogonalFE_

!----------------------------------------------------------------------------
!                                               OneDimOrthogonalFEPointer_
!----------------------------------------------------------------------------

TYPE :: OneDimOrthogonalFEPointer_
  CLASS(OneDimOrthogonalFE_), POINTER :: ptr => NULL()
END TYPE OneDimOrthogonalFEPointer_

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(OneDimOrthogonalFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetLocalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalFacetElemShapeData(obj, elemsd)
    CLASS(OneDimOrthogonalFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
  END SUBROUTINE obj_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                               OneDimOrthogonalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE OneDimOrthogonalFEPointer
  MODULE FUNCTION obj_OneDimOrthogonalFEPointer1() RESULT(ans)
    TYPE(OneDimOrthogonalFE_), POINTER :: ans
  END FUNCTION obj_OneDimOrthogonalFEPointer1
END INTERFACE OneDimOrthogonalFEPointer

!----------------------------------------------------------------------------
!                                               OneDimOrthogonalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE OneDimOrthogonalFEPointer
  MODULE FUNCTION obj_OneDimOrthogonalFEPointer2(baseContinuity, order) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! base continuity of the element
    TYPE(OneDimOrthogonalFE_), POINTER :: ans
  END FUNCTION obj_OneDimOrthogonalFEPointer2
END INTERFACE OneDimOrthogonalFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of OneDimOrthogonalFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(OneDimOrthogonalFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of OneDimOrthogonalFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(OneDimOrthogonalFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimOrthogonalFE_Class
