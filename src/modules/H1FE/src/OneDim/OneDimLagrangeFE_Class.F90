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
! summary: OneDimLagrangeFE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1FE
! submodule.
!
MODULE OneDimLagrangeFE_Class
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapedata_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE
PRIVATE

PUBLIC :: OneDimLagrangeFE_
PUBLIC :: OneDimLagrangeFEPointer_
PUBLIC :: OneDimLagrangeFEPointer
PUBLIC :: OneDimLagrangeFE
PUBLIC :: FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                          OneDimLagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: OneDimLagrangeFE finite element interfaces
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
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE Test_Method, ONLY: OK
! USE OneDimLagrangeFE_Class, ONLY: NewFE => OneDimLagrangeFEPointer
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
TYPE, EXTENDS(AbstractOneDimFE_) :: OneDimLagrangeFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Evaluate reference cell shape data.

  PROCEDURE, PASS(obj) :: GetTimeDOFValueFromSTFunction => &
    obj_GetTimeDOFValueFromSTFunction
  !! Sample one-dimensional nodal DOF values.
  PROCEDURE, PASS(obj) :: GetTimeDOFValueFromConstant => &
    obj_GetTimeDOFValueFromConstant
  !! Sample one-dimensional nodal DOF values.
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromTimeFunction => &
    obj_GetDOFValueFromTimeFunction
  !! Sample one-dimensional nodal DOF values.
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromSpaceFunction => &
    obj_GetDOFValueFromSpaceFunction
  !! Sample one-dimensional nodal DOF values.
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromConstant => &
    obj_GetDOFValueFromConstant
  !! Sample one-dimensional nodal DOF values.
END TYPE OneDimLagrangeFE_

!----------------------------------------------------------------------------
!                                                   OneDimLagrangeFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable OneDimLagrangeFE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: OneDimLagrangeFEPointer_
  CLASS(OneDimLagrangeFE_), POINTER :: ptr => NULL()
END TYPE OneDimLagrangeFEPointer_

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Evaluate reference cell shape data
!
! Delegates to obj%opt%Lagrange_GetLocalElemShapeData. Uses caller-prepared
! quadrature to evaluate basis values, reference derivatives and weights. The
! basis options may allocate or resize internal work arrays. Physical mapping
! is performed separately.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE BaseType, ONLY: ElemShapeData_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
!
! CALL obj%Initiate(baseContinuity="H1", &
!   baseInterpolation="Lagrange", order=2_I4B, &
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
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! Cell quadrature, or reference cell points restricted to a facet.
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                            OneDimLagrangeFEPointer@Methods
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
! ans : TYPE(OneDimLagrangeFE_) - Allocated finite element target returned to
! the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE Test_Method, ONLY: OK
! USE OneDimLagrangeFE_Class, ONLY: NewFE => OneDimLagrangeFEPointer
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

INTERFACE OneDimLagrangeFEPointer
  MODULE FUNCTION obj_OneDimLagrangeFEPointer1() RESULT(ans)
    TYPE(OneDimLagrangeFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_OneDimLagrangeFEPointer1
END INTERFACE OneDimLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                   OneDimLagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Allocate and initialize a finite element target
!
! Allocates a new scalar lagrange finite element and calls its inherited
! Initiate binding with the supplied configuration. The caller owns the
! allocation and must reset and deallocate it when finished.
!
! Returns:
! ans : TYPE(OneDimLagrangeFE_) - Allocated finite element target returned to
! the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE OneDimLagrangeFE_Class, ONLY: NewFE => OneDimLagrangeFEPointer
! USE BaseType, ONLY: TypePolynomialOpt
! IMPLICIT NONE
! TYPE(FE_), POINTER :: ptr
!
! ptr => NewFE(baseContinuity="H1", order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   basisType=TypePolynomialOpt%Monomial)
! CALL OK(ASSOCIATED(ptr), "Configured constructor allocates a target")
! CALL OK(ptr%isInit, "Target is initialized")
! CALL OK(ptr%GetOrder() == 2_I4B, "Constructor order")
! CALL ptr%DEALLOCATE()
! DEALLOCATE(ptr)
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE OneDimLagrangeFEPointer
  MODULE FUNCTION obj_OneDimLagrangeFEPointer2( &
    baseContinuity, ipType, basisType, order, alpha, beta, lambda) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity family name passed to the one-dimensional basis options.
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type code; use TypeInterpolationOpt constants.
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Polynomial basis type code(s); use TypePolynomialOpt constants.
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial degree; scalar or directional input as declared.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi alpha parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi beta parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters by reference direction.
    TYPE(OneDimLagrangeFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_OneDimLagrangeFEPointer2
END INTERFACE OneDimLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                   OneDimLagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Construct a one-dimensional Lagrange value
!
! Returns a scalar OneDimLagrangeFE_ initialized with the supplied continuity,
! polynomial degree, interpolation-point type and polynomial basis. Optional
! alpha, beta and lambda are passed to Initiate.
!
! Returns:
! ans : TYPE(OneDimLagrangeFE_) - Initialized finite element value.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE OneDimLagrangeFE_Class, ONLY: NewValue => OneDimLagrangeFE
! USE BaseType, ONLY: TypePolynomialOpt
! IMPLICIT NONE
! TYPE(FE_) :: obj
!
!
! obj = NewValue(baseContinuity="H1", order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   basisType=TypePolynomialOpt%Monomial)
! CALL OK(obj%isInit, "Value constructor initializes the object")
! CALL OK(obj%GetOrder() == 2_I4B, "Value constructor order")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE OneDimLagrangeFE
  MODULE FUNCTION obj_OneDimLagrangeFE( &
    baseContinuity, ipType, basisType, order, alpha, beta, lambda) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity family name passed to the one-dimensional basis options.
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type code; use TypeInterpolationOpt constants.
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Polynomial basis type code(s); use TypePolynomialOpt constants.
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial degree; scalar or directional input as declared.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi alpha parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi beta parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters by reference direction.
    TYPE(OneDimLagrangeFE_) :: ans
    !! Initialized finite element value.
  END FUNCTION obj_OneDimLagrangeFE
END INTERFACE OneDimLagrangeFE

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
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE Test_Method, ONLY: OK
! USE OneDimLagrangeFE_Class, ONLY: Wrapper_ => OneDimLagrangeFEPointer_
! USE OneDimLagrangeFE_Class, ONLY: ReleaseFE => FiniteElementDeallocate
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
    TYPE(OneDimLagrangeFE_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
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
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE Test_Method, ONLY: OK
! USE OneDimLagrangeFE_Class, ONLY: Wrapper_ => OneDimLagrangeFEPointer_
! USE OneDimLagrangeFE_Class, ONLY: ReleaseFE => FiniteElementDeallocate
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
    TYPE(OneDimLagrangeFEPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Allocatable pointer-wrapper vector; target allocations are not freed.
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                    GetTimeDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-02
! summary: Sample one-dimensional nodal DOF values
!
! Evaluates nodal coefficients directly; no mass-matrix projection is
! performed. For function sampling, prepare interpolation storage first with
! GetLocalElemShapeData. Writes the unit value one to each selected
! coefficient. onlyFaceBubble selects the max(elemsd%nns-2,0) interior
! entries; otherwise returns elemsd%nns entries. times, massMat, ipiv,
! funcValue and icompo are not used. The optional onlyFaceBubble is currently
! read directly, so examples supply it explicitly.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
! TYPE(UserFunction_) :: func
! REAL(DFP) :: ans(3), massMat(3, 3), samples(10)
! INTEGER(I4B) :: ipiv(3), tsize
!
! CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Lagrange", &
!   ipType=TypeInterpolationOpt%Equidistance, order=2_I4B, &
!   quadratureIsOrder=.TRUE., quadratureOrder=6_I4B)
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=1_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! CALL obj%GetDOFValueFromConstant(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 3_I4B .AND. ALL(ans == 1.0_DFP), "Constant spatial DOFs")
! CALL obj%GetTimeDOFValue(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(tsize == 1_I4B .AND. ans(1) == 1.0_DFP, "Interior time DOF")
! CALL obj%GetDOFValueFromSpaceFunction(elemsd=shapes, &
!   x=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Spatial samples")
! CALL obj%GetDOFValueFromTimeFunction(elemsd=shapes, &
!   times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Temporal samples")
! CALL func%DEALLOCATE()
! CALL func%Initiate(NAME="space_time", &
!   returnType=TypeFEVariableOpt%scalar, argType=TypeFEVariableOpt%space, &
!   numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=3.0_DFP)
! CALL obj%GetTimeDOFValue(elemsd=shapes, x=[0.0_DFP, 0.0_DFP, 0.0_DFP], &
!   nsd=3_I4B, times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 3.0_DFP) < 1.0E-10_DFP), "Space-time samples")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetTimeDOFValueFromConstant( &
    obj, elemsd, times, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble, icompo)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    REAL(DFP), INTENT(IN) :: times(:)
    !! Time interval endpoints; SIZE(times) = 2.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! DOF coefficient buffer; active entries or ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Caller-owned mass-matrix workspace for projection-based overloads.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! Pivot workspace for projection-based overloads.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Caller-owned sample workspace; size must fit the active quadrature.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional vector component index where supported; scalar paths ignore
    !! it.
  END SUBROUTINE obj_GetTimeDOFValueFromConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetDOFValue@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: Sample one-dimensional nodal DOF values
!
! Evaluates nodal coefficients directly; no mass-matrix projection is
! performed. For function sampling, prepare interpolation storage first with
! GetLocalElemShapeData. Samples a scalar space-time function at time
! interpolation points mapped to times(1:2), holding the spatial point x
! fixed. Function argument count determines where time is placed in the
! four-entry argument buffer. onlyFaceBubble skips the two endpoint DOFs. nsd,
! massMat, ipiv, funcValue and icompo are currently unused.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
! TYPE(UserFunction_) :: func
! REAL(DFP) :: ans(3), massMat(3, 3), samples(10)
! INTEGER(I4B) :: ipiv(3), tsize
!
! CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Lagrange", &
!   ipType=TypeInterpolationOpt%Equidistance, order=2_I4B, &
!   quadratureIsOrder=.TRUE., quadratureOrder=6_I4B)
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=1_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! CALL obj%GetDOFValueFromConstant(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 3_I4B .AND. ALL(ans == 1.0_DFP), "Constant spatial DOFs")
! CALL obj%GetTimeDOFValue(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(tsize == 1_I4B .AND. ans(1) == 1.0_DFP, "Interior time DOF")
! CALL obj%GetDOFValueFromSpaceFunction(elemsd=shapes, &
!   x=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Spatial samples")
! CALL obj%GetDOFValueFromTimeFunction(elemsd=shapes, &
!   times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Temporal samples")
! CALL func%DEALLOCATE()
! CALL func%Initiate(NAME="space_time", &
!   returnType=TypeFEVariableOpt%scalar, argType=TypeFEVariableOpt%space, &
!   numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=3.0_DFP)
! CALL obj%GetTimeDOFValue(elemsd=shapes, x=[0.0_DFP, 0.0_DFP, 0.0_DFP], &
!   nsd=3_I4B, times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 3.0_DFP) < 1.0E-10_DFP), "Space-time samples")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetTimeDOFValueFromSTFunction( &
    obj, elemsd, x, nsd, times, func, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble, icompo)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    REAL(DFP), INTENT(IN) :: x(:)
    !! Spatial point for time sampling, or two spatial interval endpoints.
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Physical spatial dimension of the finite element.
    REAL(DFP), INTENT(IN) :: times(:)
    !! Time interval endpoints; SIZE(times) = 2.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function as supported by this overload.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! DOF coefficient buffer; active entries or ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Caller-owned mass-matrix workspace for projection-based overloads.
    INTEGER(I4B), INTENT(OUT) :: ipiv(:)
    !! Pivot workspace for projection-based overloads.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Caller-owned sample workspace; size must fit the active quadrature.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional vector component index where supported; scalar paths ignore
    !! it.
  END SUBROUTINE obj_GetTimeDOFValueFromSTFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-02
! summary: Sample one-dimensional nodal DOF values
!
! Evaluates nodal coefficients directly; no mass-matrix projection is
! performed. For function sampling, prepare interpolation storage first with
! GetLocalElemShapeData. Writes the unit value one to each selected
! coefficient. onlyFaceBubble selects the max(elemsd%nns-2,0) interior
! entries; otherwise returns elemsd%nns entries. times, massMat, ipiv,
! funcValue and icompo are not used. The optional onlyFaceBubble is currently
! read directly, so examples supply it explicitly.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
! TYPE(UserFunction_) :: func
! REAL(DFP) :: ans(3), massMat(3, 3), samples(10)
! INTEGER(I4B) :: ipiv(3), tsize
!
! CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Lagrange", &
!   ipType=TypeInterpolationOpt%Equidistance, order=2_I4B, &
!   quadratureIsOrder=.TRUE., quadratureOrder=6_I4B)
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=1_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! CALL obj%GetDOFValueFromConstant(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 3_I4B .AND. ALL(ans == 1.0_DFP), "Constant spatial DOFs")
! CALL obj%GetTimeDOFValue(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(tsize == 1_I4B .AND. ans(1) == 1.0_DFP, "Interior time DOF")
! CALL obj%GetDOFValueFromSpaceFunction(elemsd=shapes, &
!   x=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Spatial samples")
! CALL obj%GetDOFValueFromTimeFunction(elemsd=shapes, &
!   times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Temporal samples")
! CALL func%DEALLOCATE()
! CALL func%Initiate(NAME="space_time", &
!   returnType=TypeFEVariableOpt%scalar, argType=TypeFEVariableOpt%space, &
!   numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=3.0_DFP)
! CALL obj%GetTimeDOFValue(elemsd=shapes, x=[0.0_DFP, 0.0_DFP, 0.0_DFP], &
!   nsd=3_I4B, times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 3.0_DFP) < 1.0E-10_DFP), "Space-time samples")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromConstant( &
    obj, elemsd, times, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble, icompo)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    REAL(DFP), INTENT(IN) :: times(:)
    !! Time interval endpoints; SIZE(times) = 2.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! DOF coefficient buffer; active entries or ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Caller-owned mass-matrix workspace for projection-based overloads.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! Pivot workspace for projection-based overloads.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Caller-owned sample workspace; size must fit the active quadrature.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional vector component index where supported; scalar paths ignore
    !! it.
  END SUBROUTINE obj_GetDOFValueFromConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetDOFValue@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: Sample one-dimensional nodal DOF values
!
! Evaluates nodal coefficients directly; no mass-matrix projection is
! performed. For function sampling, prepare interpolation storage first with
! GetLocalElemShapeData. Samples a scalar function at interpolation points
! mapped to x(1:2). onlyFaceBubble skips the first two endpoint DOFs. massMat,
! ipiv and funcValue are accepted but not used; ipiv is declared INTENT(OUT)
! and is not assigned.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
! TYPE(UserFunction_) :: func
! REAL(DFP) :: ans(3), massMat(3, 3), samples(10)
! INTEGER(I4B) :: ipiv(3), tsize
!
! CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Lagrange", &
!   ipType=TypeInterpolationOpt%Equidistance, order=2_I4B, &
!   quadratureIsOrder=.TRUE., quadratureOrder=6_I4B)
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=1_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! CALL obj%GetDOFValueFromConstant(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 3_I4B .AND. ALL(ans == 1.0_DFP), "Constant spatial DOFs")
! CALL obj%GetTimeDOFValue(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(tsize == 1_I4B .AND. ans(1) == 1.0_DFP, "Interior time DOF")
! CALL obj%GetDOFValueFromSpaceFunction(elemsd=shapes, &
!   x=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Spatial samples")
! CALL obj%GetDOFValueFromTimeFunction(elemsd=shapes, &
!   times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Temporal samples")
! CALL func%DEALLOCATE()
! CALL func%Initiate(NAME="space_time", &
!   returnType=TypeFEVariableOpt%scalar, argType=TypeFEVariableOpt%space, &
!   numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=3.0_DFP)
! CALL obj%GetTimeDOFValue(elemsd=shapes, x=[0.0_DFP, 0.0_DFP, 0.0_DFP], &
!   nsd=3_I4B, times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 3.0_DFP) < 1.0E-10_DFP), "Space-time samples")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromSpaceFunction( &
    obj, elemsd, x, func, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    REAL(DFP), INTENT(IN) :: x(:)
    !! Spatial point for time sampling, or two spatial interval endpoints.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function as supported by this overload.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! DOF coefficient buffer; active entries or ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Caller-owned mass-matrix workspace for projection-based overloads.
    INTEGER(I4B), INTENT(OUT) :: ipiv(:)
    !! Pivot workspace for projection-based overloads.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Caller-owned sample workspace; size must fit the active quadrature.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
  END SUBROUTINE obj_GetDOFValueFromSpaceFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetDOFValueFromTimeFunction@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: Sample one-dimensional nodal DOF values
!
! Evaluates nodal coefficients directly; no mass-matrix projection is
! performed. For function sampling, prepare interpolation storage first with
! GetLocalElemShapeData. Samples a scalar function at interpolation points
! mapped to times(1:2). onlyFaceBubble skips the first two endpoint DOFs.
! massMat, ipiv and funcValue are accepted but not used; ipiv is declared
! INTENT(OUT) and is not assigned.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
! TYPE(UserFunction_) :: func
! REAL(DFP) :: ans(3), massMat(3, 3), samples(10)
! INTEGER(I4B) :: ipiv(3), tsize
!
! CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Lagrange", &
!   ipType=TypeInterpolationOpt%Equidistance, order=2_I4B, &
!   quadratureIsOrder=.TRUE., quadratureOrder=6_I4B)
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=1_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! CALL obj%GetDOFValueFromConstant(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 3_I4B .AND. ALL(ans == 1.0_DFP), "Constant spatial DOFs")
! CALL obj%GetTimeDOFValue(elemsd=shapes, times=[0.0_DFP, 1.0_DFP], &
!   ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(tsize == 1_I4B .AND. ans(1) == 1.0_DFP, "Interior time DOF")
! CALL obj%GetDOFValueFromSpaceFunction(elemsd=shapes, &
!   x=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Spatial samples")
! CALL obj%GetDOFValueFromTimeFunction(elemsd=shapes, &
!   times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Temporal samples")
! CALL func%DEALLOCATE()
! CALL func%Initiate(NAME="space_time", &
!   returnType=TypeFEVariableOpt%scalar, argType=TypeFEVariableOpt%space, &
!   numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=3.0_DFP)
! CALL obj%GetTimeDOFValue(elemsd=shapes, x=[0.0_DFP, 0.0_DFP, 0.0_DFP], &
!   nsd=3_I4B, times=[0.0_DFP, 1.0_DFP], func=func, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 3.0_DFP) < 1.0E-10_DFP), "Space-time samples")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractOneDimFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromTimeFunction( &
    obj, elemsd, times, func, ans, tsize, massMat, ipiv, &
    funcValue, onlyFaceBubble)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    REAL(DFP), INTENT(IN) :: times(:)
    !! Time interval endpoints; SIZE(times) = 2.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function as supported by this overload.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! DOF coefficient buffer; active entries or ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Caller-owned mass-matrix workspace for projection-based overloads.
    INTEGER(I4B), INTENT(OUT) :: ipiv(:)
    !! Pivot workspace for projection-based overloads.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Caller-owned sample workspace; size must fit the active quadrature.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
  END SUBROUTINE obj_GetDOFValueFromTimeFunction
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimLagrangeFE_Class
