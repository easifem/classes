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

MODULE HierarchicalFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE ScalarFE_Class, ONLY: ScalarFE_

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemShapedata_

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: HierarchicalFE_
PUBLIC :: HierarchicalFEPointer_
PUBLIC :: FiniteElementDeallocate
PUBLIC :: HierarchicalFEPointer

CHARACTER(*), PARAMETER :: modName = "HierarchicalFE_Class"
CHARACTER(*), PARAMETER :: myprefix = "HierarchicalFE"

!----------------------------------------------------------------------------
!                                                             HierarchicalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-11
! summary: Finite element class

TYPE, EXTENDS(ScalarFE_) :: HierarchicalFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
END TYPE HierarchicalFE_

!----------------------------------------------------------------------------
!                                                      HierarchicalFEPointer_
!----------------------------------------------------------------------------

TYPE :: HierarchicalFEPointer_
  CLASS(HierarchicalFE_), POINTER :: ptr => NULL()
END TYPE HierarchicalFEPointer_

!----------------------------------------------------------------------------
!                                                     HierarchicalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE HierarchicalFEPointer
  MODULE FUNCTION obj_HierarchicalFEPointer(elemType, nsd, baseContinuity, &
                                cellOrder, faceOrder, edgeOrder, cellOrient, &
                                           faceOrient, edgeOrient) RESULT(ans)

    INTEGER(I4B), INTENT(IN) :: elemType
    !! Type of finite element
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! order on each cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! order on each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! order on each edge
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! orientation of each cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! orientation of each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orientation
    TYPE(HierarchicalFE_), POINTER :: ans
  END FUNCTION obj_HierarchicalFEPointer
END INTERFACE HierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of HierarchicalFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(HierarchicalFE_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of HierarchicalFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(HierarchicalFEPointer_), ALLOCATABLE :: obj(:)
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
    CLASS(HierarchicalFE_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HierarchicalFE_Class
