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
! summary: TetrahedronH1LagrangeFE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1FE
! submodule.
!
MODULE TetrahedronH1LagrangeFE_Class
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE TetrahedronH1FE_Class, ONLY: TetrahedronH1FE_

IMPLICIT NONE

PRIVATE

PUBLIC :: TetrahedronH1LagrangeFE_
PUBLIC :: TetrahedronH1LagrangeFEPointer_
PUBLIC :: TetrahedronH1LagrangeFEPointer

PUBLIC :: FiniteElementDeallocate

CHARACTER(*), PARAMETER :: modName = "TetrahedronH1LagrangeFE_Class"

!----------------------------------------------------------------------------
!                                                    TetrahedronH1LagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: TetrahedronH1LagrangeFE finite element interfaces
!
! Extends TetrahedronH1FE_ without introducing additional data components.
! Inherits the initialization state and basis options and overrides the
! operations listed below. Some declared operations are WIP placeholders and
! report under-development errors in DEBUG mode; see each interface and
! bugs_report/H1FE.md. Pointer factories transfer ownership to their caller.
! Reset a target with its DEALLOCATE binding, then free owned allocations with
! intrinsic DEALLOCATE. The declared FiniteElementDeallocate vector helpers
! currently lack implementations.
!
! Examples
!
!```fortran
! PROGRAM main
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE TetrahedronH1LagrangeFE_Class, ONLY: &
!   NewFE => TetrahedronH1LagrangeFEPointer
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
TYPE, EXTENDS(TetrahedronH1FE_) :: TetrahedronH1LagrangeFE_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get Local Elem Shape Data (WIP).
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Get Global Elem Shape Data (WIP).
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set Order (WIP).
END TYPE TetrahedronH1LagrangeFE_

!----------------------------------------------------------------------------
!                                             TetrahedronH1LagrangeFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable TetrahedronH1LagrangeFE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: TetrahedronH1LagrangeFEPointer_
  CLASS(TetrahedronH1LagrangeFE_), POINTER :: ptr => NULL()
END TYPE TetrahedronH1LagrangeFEPointer_

!----------------------------------------------------------------------------
!                                      TetrahedronH1LagrangeFEPointer@Methods
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
! ans : TYPE(TetrahedronH1LagrangeFE_) - Allocated finite element target
! returned to the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE TetrahedronH1LagrangeFE_Class, ONLY: &
!   NewFE => TetrahedronH1LagrangeFEPointer
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

INTERFACE TetrahedronH1LagrangeFEPointer
  MODULE FUNCTION obj_TetrahedronH1LagrangeFEPointer1() RESULT(ans)
    TYPE(TetrahedronH1LagrangeFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_TetrahedronH1LagrangeFEPointer1
END INTERFACE TetrahedronH1LagrangeFEPointer

!----------------------------------------------------------------------------
!                                                          LagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Configured pointer constructor (WIP)
!
! This overload is declared but its implementation is a WIP placeholder rather
! than a configured constructor. The implementation raises an
! under-development error in DEBUG mode. The example intentionally exposes the
! unsupported call.
!
! Returns:
! ans : TYPE(TetrahedronH1LagrangeFE_) - Allocated finite element target
! returned to the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! USE TetrahedronH1LagrangeFE_Class, ONLY: &
!   NewFE => TetrahedronH1LagrangeFEPointer
! IMPLICIT NONE
! TYPE(FE_), POINTER :: ptr
!
! ptr => NewFE(order=2_I4B, nsd=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, quadratureOrder=[6_I4B])
! CALL OK(ASSOCIATED(ptr), "Configured constructor allocates a target")
! CALL OK(ptr%isInit, "Target is initialized")
! CALL OK(ptr%opt%GetTotalDOF() == 9_I4B, &
!   "Constructor establishes quadratic DOFs")
! CALL ptr%DEALLOCATE()
! DEALLOCATE(ptr)
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE TetrahedronH1LagrangeFEPointer
  MODULE FUNCTION obj_TetrahedronH1LagrangeFEPointer2( &
    order, nsd, ipType, basisType, alpha, beta, lambda, cellOrient, &
    quadratureType, quadratureOrder, quadratureAlpha, quadratureBeta, &
    quadratureLambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial degree; scalar or directional input as declared.
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Physical spatial dimension of the finite element.
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type code; use TypeInterpolationOpt constants.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Polynomial basis type code(s); use TypePolynomialOpt constants.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi alpha parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi beta parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(3)
    !! Cell orientation data; conventions depend on element topology.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! Quadrature accuracy orders by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha(:)
    !! Quadrature Jacobi alpha parameters by direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta(:)
    !! Quadrature Jacobi beta parameters by direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda(:)
    !! Quadrature ultraspherical parameters by direction.
    TYPE(TetrahedronH1LagrangeFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_TetrahedronH1LagrangeFEPointer2
END INTERFACE TetrahedronH1LagrangeFEPointer

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Declared vector cleanup overload without an implementation
!
! This public FiniteElementDeallocate overload is declared but has no matching
! implementation in the active submodule for this class. Calls fail to link
! against the installed library. The example demonstrates both declared
! cleanup overloads; it is retained as a regression test for the missing
! implementation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE TetrahedronH1LagrangeFE_Class, ONLY: &
!   Wrapper_ => TetrahedronH1LagrangeFEPointer_
! USE TetrahedronH1LagrangeFE_Class, ONLY: &
!   ReleaseFE => FiniteElementDeallocate
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
! See also: [[AbstractFE_]]

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(TetrahedronH1LagrangeFE_), ALLOCATABLE :: obj(:)
    !! Allocatable finite element value vector to reset and deallocate.
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-09
! summary: Declared vector cleanup overload without an implementation
!
! This public FiniteElementDeallocate overload is declared but has no matching
! implementation in the active submodule for this class. Calls fail to link
! against the installed library. The example demonstrates both declared
! cleanup overloads; it is retained as a regression test for the missing
! implementation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE TetrahedronH1LagrangeFE_Class, ONLY: &
!   Wrapper_ => TetrahedronH1LagrangeFEPointer_
! USE TetrahedronH1LagrangeFE_Class, ONLY: &
!   ReleaseFE => FiniteElementDeallocate
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
! See also: [[AbstractFE_]]

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(TetrahedronH1LagrangeFEPointer_), ALLOCATABLE :: obj(:)
    !! Allocatable pointer-wrapper vector; target allocations are not freed.
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                               GetLocalElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Get Local Elem Shape Data (WIP)
!
! This binding is currently a WIP placeholder. It does not perform the
! operation described by the interface name. The implementation raises an
! under-development error in DEBUG mode. The example intentionally exposes the
! unsupported call.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: QuadraturePoint_
! USE BaseType, ONLY: ElemShapeData_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes
! TYPE(QuadraturePoint_) :: quad
!
! ! This operation is currently a WIP placeholder.
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL OK(shapes%nips > 0_I4B, "Local shapes populated")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(TetrahedronH1LagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or reference cell points restricted to a facet.
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Set Order (WIP)
!
! This binding is currently a WIP placeholder. It does not perform the
! operation described by the interface name. The implementation raises an
! under-development error in DEBUG mode. The example intentionally exposes the
! unsupported call.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
!
!
! ! This operation is currently a WIP placeholder.
! CALL obj%SetOrder(order=1_I4B)
! CALL OK(obj%opt%GetTotalDOF() > 0_I4B, "SetOrder establishes DOFs")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_SetOrder( &
    obj, order, anisoOrder, cellOrder, faceOrder, edgeOrder, tCell, tFace, &
    tEdge, errCheck)
    CLASS(TetrahedronH1LagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Polynomial degree; scalar or directional input as declared.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Anisotropic approximation orders by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Cell approximation degrees; counts and broadcasting follow BasisOpt_.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Facet approximation degrees; three rows and one column per facet.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Approximation orders on the edges.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! Active size of cell order or orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! Active number of face columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional DEBUG validation control forwarded where supported.
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Get Global Elem Shape Data (WIP)
!
! This binding is currently a WIP placeholder. It does not perform the
! operation described by the interface name. The implementation raises an
! under-development error in DEBUG mode. The example intentionally exposes the
! unsupported call.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(ElemShapeData_) :: shapes, geometry
! REAL(DFP) :: xij(3, 8)
!
! ! This operation is currently a WIP placeholder.
! xij = 0.0_DFP
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, geoelemsd=geometry, xij=xij)
! CALL OK(shapes%nips > 0_I4B, "Mapped shapes populated")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoelemsd)
    CLASS(TetrahedronH1LagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry-node coordinates; coordinate directions are rows.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required prepared geometry shapes at matching reference points.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TetrahedronH1LagrangeFE_Class
