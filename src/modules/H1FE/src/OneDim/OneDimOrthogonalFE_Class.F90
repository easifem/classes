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

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: OneDimOrthogonalFE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1FE
! submodule.
!
MODULE OneDimOrthogonalFE_Class
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapedata_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: OneDimOrthogonalFE_
PUBLIC :: OneDimOrthogonalFEPointer_
PUBLIC :: FiniteElementDeallocate
PUBLIC :: OneDimOrthogonalFEPointer

!----------------------------------------------------------------------------
!                                                        OneDimOrthogonalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: OneDimOrthogonalFE finite element interfaces
!
! Extends AbstractOneDimFE_ without introducing additional data components.
! Inherits the initialization state and basis options and overrides the
! operations listed below. Pointer factories transfer ownership to their
! caller. Reset a target with its DEALLOCATE binding, then free owned
! allocations with intrinsic DEALLOCATE.
!
! Examples
!
!```fortran
! PROGRAM main
! USE OneDimOrthogonalFE_Class, ONLY: FE_ => OneDimOrthogonalFE_
! USE Test_Method, ONLY: OK
! USE OneDimOrthogonalFE_Class, ONLY: NewFE => OneDimOrthogonalFEPointer
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
TYPE, EXTENDS(AbstractOneDimFE_) :: OneDimOrthogonalFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Evaluate reference cell shape data.
END TYPE OneDimOrthogonalFE_

!----------------------------------------------------------------------------
!                                               OneDimOrthogonalFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable OneDimOrthogonalFE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: OneDimOrthogonalFEPointer_
  CLASS(OneDimOrthogonalFE_), POINTER :: ptr => NULL()
END TYPE OneDimOrthogonalFEPointer_

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Evaluate reference cell shape data
!
! Delegates to obj%opt%Orthogonal_GetLocalElemShapeData. Uses caller-prepared
! quadrature to evaluate basis values, reference derivatives and weights. The
! basis options may allocate or resize internal work arrays. Physical mapping
! is performed separately.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OneDimOrthogonalFE_Class, ONLY: FE_ => OneDimOrthogonalFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE BaseType, ONLY: ElemShapeData_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
!
! CALL obj%Initiate(baseContinuity="H1", &
!   baseInterpolation="Orthogonal", order=2_I4B, &
!   quadratureIsOrder=.TRUE., quadratureOrder=6_I4B)
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL OK(shapes%nns == 3_I4B, "Quadratic basis size")
! CALL OK(shapes%nips > 0_I4B, "Nonempty quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(OneDimOrthogonalFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Cell quadrature, or reference cell points restricted to a facet.
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                               OneDimOrthogonalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Allocate an uninitialized finite element target
!
! Allocates a new target of the concrete finite element type without calling
! Initiate. The caller owns the allocation: call its DEALLOCATE binding to
! reset state, then use intrinsic DEALLOCATE on the target when finished.
!
! Returns:
! ans : TYPE(OneDimOrthogonalFE_) - Allocated finite element target returned
! to the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE OneDimOrthogonalFE_Class, ONLY: FE_ => OneDimOrthogonalFE_
! USE Test_Method, ONLY: OK
! USE OneDimOrthogonalFE_Class, ONLY: NewFE => OneDimOrthogonalFEPointer
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
! See also: [[AbstractOneDimFE_]]

INTERFACE OneDimOrthogonalFEPointer
  MODULE FUNCTION obj_OneDimOrthogonalFEPointer1() RESULT(ans)
    TYPE(OneDimOrthogonalFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_OneDimOrthogonalFEPointer1
END INTERFACE OneDimOrthogonalFEPointer

!----------------------------------------------------------------------------
!                                               OneDimOrthogonalFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Allocate and initialize a finite element target
!
! Allocates a new scalar orthogonal finite element and calls its inherited
! Initiate binding with the supplied configuration. The caller owns the
! allocation and must reset and deallocate it when finished.
!
! Returns:
! ans : TYPE(OneDimOrthogonalFE_) - Allocated finite element target returned
! to the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OneDimOrthogonalFE_Class, ONLY: FE_ => OneDimOrthogonalFE_
! USE Test_Method, ONLY: OK
! USE OneDimOrthogonalFE_Class, ONLY: NewFE => OneDimOrthogonalFEPointer
! IMPLICIT NONE
! TYPE(FE_), POINTER :: ptr
!
! ptr => NewFE(baseContinuity="H1", order=2_I4B)
! CALL OK(ASSOCIATED(ptr), "Configured constructor allocates a target")
! CALL OK(ptr%isInit, "Target is initialized")
! CALL OK(ptr%GetOrder() == 2_I4B, "Constructor order")
! CALL ptr%DEALLOCATE()
! DEALLOCATE(ptr)
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE OneDimOrthogonalFEPointer
  MODULE FUNCTION obj_OneDimOrthogonalFEPointer2(baseContinuity, order) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial degree; scalar or directional input as declared.
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity family name passed to the one-dimensional basis options.
    TYPE(OneDimOrthogonalFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_OneDimOrthogonalFEPointer2
END INTERFACE OneDimOrthogonalFEPointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Reset and deallocate a finite element value vector
!
! If allocated, calls DEALLOCATE on each element, then deallocates the value
! array. An unallocated array is accepted.
!
! Examples
!
!```fortran
! PROGRAM main
! USE OneDimOrthogonalFE_Class, ONLY: FE_ => OneDimOrthogonalFE_
! USE Test_Method, ONLY: OK
! USE OneDimOrthogonalFE_Class, ONLY: Wrapper_ => OneDimOrthogonalFEPointer_
! USE OneDimOrthogonalFE_Class, ONLY: ReleaseFE => FiniteElementDeallocate
! IMPLICIT NONE
! TYPE(FE_), ALLOCATABLE :: values(:)
! TYPE(Wrapper_), ALLOCATABLE :: refs(:)
! TYPE(FE_), TARGET :: target
!
! ALLOCATE(values(2))
! CALL ReleaseFE(values)
! CALL OK(.NOT. ALLOCATED(values), "Value vector released")
! ALLOCATE(refs(2))
! refs(1)%ptr => target
! CALL ReleaseFE(refs)
! CALL OK(.NOT. ALLOCATED(refs), "Pointer wrappers released")
! CALL OK(.NOT. target%isInit, "Target reset and still accessible")
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(OneDimOrthogonalFE_), ALLOCATABLE :: obj(:)
    !! Allocatable finite element value vector to reset and deallocate.
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-09
! summary: Reset and detach a finite element pointer vector
!
! If allocated, resets each associated target, nullifies its wrapper and
! deallocates the wrapper array. Target allocations are not freed. Keep an
! owning reference when an allocated target must subsequently be deallocated.
!
! Examples
!
!```fortran
! PROGRAM main
! USE OneDimOrthogonalFE_Class, ONLY: FE_ => OneDimOrthogonalFE_
! USE Test_Method, ONLY: OK
! USE OneDimOrthogonalFE_Class, ONLY: Wrapper_ => OneDimOrthogonalFEPointer_
! USE OneDimOrthogonalFE_Class, ONLY: ReleaseFE => FiniteElementDeallocate
! IMPLICIT NONE
! TYPE(FE_), ALLOCATABLE :: values(:)
! TYPE(Wrapper_), ALLOCATABLE :: refs(:)
! TYPE(FE_), TARGET :: target
!
! ALLOCATE(values(2))
! CALL ReleaseFE(values)
! CALL OK(.NOT. ALLOCATED(values), "Value vector released")
! ALLOCATE(refs(2))
! refs(1)%ptr => target
! CALL ReleaseFE(refs)
! CALL OK(.NOT. ALLOCATED(refs), "Pointer wrappers released")
! CALL OK(.NOT. target%isInit, "Target reset and still accessible")
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(OneDimOrthogonalFEPointer_), ALLOCATABLE :: obj(:)
    !! Allocatable pointer-wrapper vector; target allocations are not freed.
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimOrthogonalFE_Class
