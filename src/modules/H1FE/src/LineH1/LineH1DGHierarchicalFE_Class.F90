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

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Broken H1 spaces on line elements for Hierarchical poly.
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1DGFE
! submodule.
!
MODULE LineH1DGHierarchicalFE_Class
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE LineH1HierarchicalFE_Class, ONLY: LineH1HierarchicalFE_
IMPLICIT NONE
PRIVATE

PUBLIC :: LineH1DGHierarchicalFE_
PUBLIC :: LineH1DGHierarchicalFEPointer_
PUBLIC :: LineH1DGHierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                   LineH1DGHierarchicalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Broken H1 spaces on line elements for hierarchical basis
!
! Extends LineH1HierarchicalFE_ to handle broken H1 spaces on line element.
! Polynomial family is Hierarchical basis.
!
! Everything is inherited from parent. Nothing new is defined.
!
TYPE, EXTENDS(LineH1HierarchicalFE_) :: LineH1DGHierarchicalFE_
END TYPE LineH1DGHierarchicalFE_

!----------------------------------------------------------------------------
!                                             LineH1DGHierarchicalFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-31
! summary: Nullable LineH1DGHierarchicalFE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: LineH1DGHierarchicalFEPointer_
  CLASS(LineH1DGHierarchicalFE_), POINTER :: ptr => NULL()
END TYPE LineH1DGHierarchicalFEPointer_

!----------------------------------------------------------------------------
!                                       LineH1DGHierarchicalFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-31
! summary: Allocate an uninitialized finite element target
!
! Allocates a new target of the concrete finite element type without calling
! Initiate. The caller owns the allocation: call its DEALLOCATE binding to
! reset state, then use intrinsic DEALLOCATE on the target when finished.
!
INTERFACE LineH1DGHierarchicalFEPointer
  MODULE FUNCTION obj_LineH1DGHierarchicalFEPointer1() RESULT(ans)
    TYPE(LineH1DGHierarchicalFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_LineH1DGHierarchicalFEPointer1
END INTERFACE LineH1DGHierarchicalFEPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LineH1DGHierarchicalFE_Class
