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
! summary: PrismH1DGLagrangeFE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1DGFE
! submodule.
!
MODULE PrismH1DGLagrangeFE_Class
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE PrismH1LagrangeFE_Class, ONLY: PrismH1LagrangeFE_
IMPLICIT NONE
PRIVATE

PUBLIC :: PrismH1DGLagrangeFE_
PUBLIC :: PrismH1DGLagrangeFEPointer_
PUBLIC :: PrismH1DGLagrangeFEPointer

!----------------------------------------------------------------------------
!                                              PrismH1DGLagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: PrismH1DGLagrangeFE finite element interfaces
!
! Extends PrismH1DGFE_ without introducing additional data components.
! Inherits the initialization state and basis options and overrides the
! operations listed below. Some declared operations are WIP placeholders and
! report under-development errors in DEBUG mode; see each interface and
! bugs_report/H1DGFE.md. Pointer factories transfer ownership to their caller.
! Reset a target with its DEALLOCATE binding, then free owned allocations with
! intrinsic DEALLOCATE. The declared FiniteElementDeallocate vector helpers
! currently lack implementations.
!
! Examples
!
!```fortran
! PROGRAM main
! USE PrismH1DGLagrangeFE_Class, ONLY: &
!   FE_ => PrismH1DGLagrangeFE_
! USE Test_Method, ONLY: OK
! USE PrismH1DGLagrangeFE_Class, ONLY: &
!   NewFE => PrismH1DGLagrangeFEPointer
! IMPLICIT NONE
! TYPE(FE_), POINTER :: ptr
!
! ptr => NewFE()
! CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
! CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
! CALL ptr%DEALLOCATE()
! DEALLOCATE(ptr)
! END PROGRAM main
!```
!
TYPE, EXTENDS(PrismH1LagrangeFE_) :: PrismH1DGLagrangeFE_

CONTAINS
  ! Deferred methods from TriangleH1DGFE_
  !@Methods
  PROCEDURE, PUBLIC, PASS(obj) :: FixOrientOfNptrs => obj_FixOrientOfNptrs
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrientation => obj_SetOrientation
  ! Deferred methods from AbstractFE_
  !@Methods

END TYPE PrismH1DGLagrangeFE_

!----------------------------------------------------------------------------
!                                      PrismH1DGLagrangeFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable PrismH1DGLagrangeFE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: PrismH1DGLagrangeFEPointer_
  CLASS(PrismH1DGLagrangeFE_), POINTER :: ptr => NULL()
END TYPE PrismH1DGLagrangeFEPointer_

!----------------------------------------------------------------------------
!                               PrismH1DGLagrangeFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Allocate an uninitialized finite element target
!
! Allocates a new target of the concrete finite element type without calling
! Initiate. The caller owns the allocation: call its DEALLOCATE binding to
! reset state, then use intrinsic DEALLOCATE on the target when finished.
!
! Returns:
! ans : TYPE(PrismH1DGLagrangeFE_) - Allocated finite element target
! returned to the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE PrismH1DGLagrangeFE_Class, ONLY: &
!   FE_ => PrismH1DGLagrangeFE_
! USE Test_Method, ONLY: OK
! USE PrismH1DGLagrangeFE_Class, ONLY: &
!   NewFE => PrismH1DGLagrangeFEPointer
! IMPLICIT NONE
! TYPE(FE_), POINTER :: ptr
!
! ptr => NewFE()
! CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
! CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
! CALL ptr%DEALLOCATE()
! DEALLOCATE(ptr)
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE PrismH1DGLagrangeFEPointer
  MODULE FUNCTION obj_PrismH1DGLagrangeFEPointer1() RESULT(ans)
    TYPE(PrismH1DGLagrangeFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_PrismH1DGLagrangeFEPointer1
END INTERFACE PrismH1DGLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                  FixOrientOfNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Do not reorder connectivity
!
! We do not need reodering in case of DG. So we are overloading this method.
! This method does nothing.

INTERFACE
  MODULE SUBROUTINE obj_FixOrientOfNptrs(obj, nptrs, tsize)
    CLASS(PrismH1DGLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! Node connectivity; the first tsize entries form the active buffer.
    INTEGER(I4B), INTENT(IN) :: tsize
    !! Number of active connectivity entries.
  END SUBROUTINE obj_FixOrientOfNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                             SetOrientation
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-31
! summary: We do not need orientation in DG.
!
! This method sets unit orientation, we do not need
! orientation in case of DG
! vikaWe do not need orientation in DG.
!
! This method sets unit orientation, we do not need
! orientation in case of DG. That is why this routine
! overloads this method from [[TetrahedronH1FE_]].
INTERFACE
  MODULE SUBROUTINE obj_SetOrientation( &
    obj, cellOrient, faceOrient, edgeOrient, tCell, tFace, tEdge, errCheck)
    CLASS(PrismH1DGLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! Cell orientation data; conventions depend on element topology.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! Facet orientations, normally three rows per facet.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! Edge orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! Active size of cell order or orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! Active number of face columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional DEBUG validation control forwarded where supported.
  END SUBROUTINE obj_SetOrientation
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE PrismH1DGLagrangeFE_Class
