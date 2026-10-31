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
! summary: HexahedronH1DGLagrangeFE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1DGFE
! submodule.
!
MODULE HexahedronH1DGLagrangeFE_Class
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE HexahedronH1LagrangeFE_Class, ONLY: HexahedronH1LagrangeFE_
IMPLICIT NONE
PRIVATE

PUBLIC :: HexahedronH1DGLagrangeFE_
PUBLIC :: HexahedronH1DGLagrangeFEPointer_
PUBLIC :: HexahedronH1DGLagrangeFEPointer

!----------------------------------------------------------------------------
!                                              HexahedronH1DGLagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: HexahedronH1DGLagrangeFE finite element interfaces
!
! Extends HexahedronH1DGFE_ without introducing additional data components.
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
! USE HexahedronH1DGLagrangeFE_Class, ONLY: &
!   FE_ => HexahedronH1DGLagrangeFE_
! USE Test_Method, ONLY: OK
! USE HexahedronH1DGLagrangeFE_Class, ONLY: &
!   NewFE => HexahedronH1DGLagrangeFEPointer
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
TYPE, EXTENDS(HexahedronH1LagrangeFE_) :: HexahedronH1DGLagrangeFE_
END TYPE HexahedronH1DGLagrangeFE_

!----------------------------------------------------------------------------
!                                      HexahedronH1DGLagrangeFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable HexahedronH1DGLagrangeFE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: HexahedronH1DGLagrangeFEPointer_
  CLASS(HexahedronH1DGLagrangeFE_), POINTER :: ptr => NULL()
END TYPE HexahedronH1DGLagrangeFEPointer_

!----------------------------------------------------------------------------
!                               HexahedronH1DGLagrangeFEPointer@Methods
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
! ans : TYPE(HexahedronH1DGLagrangeFE_) - Allocated finite element target
! returned to the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE HexahedronH1DGLagrangeFE_Class, ONLY: &
!   FE_ => HexahedronH1DGLagrangeFE_
! USE Test_Method, ONLY: OK
! USE HexahedronH1DGLagrangeFE_Class, ONLY: &
!   NewFE => HexahedronH1DGLagrangeFEPointer
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

INTERFACE HexahedronH1DGLagrangeFEPointer
  MODULE FUNCTION obj_HexahedronH1DGLagrangeFEPointer1() RESULT(ans)
    TYPE(HexahedronH1DGLagrangeFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_HexahedronH1DGLagrangeFEPointer1
END INTERFACE HexahedronH1DGLagrangeFEPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HexahedronH1DGLagrangeFE_Class
