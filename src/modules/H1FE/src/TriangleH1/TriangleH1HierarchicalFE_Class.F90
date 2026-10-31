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
! summary: TriangleH1HierarchicalFE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1FE
! submodule.
!
MODULE TriangleH1HierarchicalFE_Class
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE TriangleH1FE_Class, ONLY: TriangleH1FE_
IMPLICIT NONE
PRIVATE

PUBLIC :: TriangleH1HierarchicalFE_
PUBLIC :: TriangleH1HierarchicalFEPointer_
PUBLIC :: TriangleH1HierarchicalFEPointer

!----------------------------------------------------------------------------
!                                                  TriangleH1HierarchicalFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: TriangleH1HierarchicalFE finite element interfaces
!
! Extends TriangleH1FE_ without introducing additional data components.
! Inherits the initialization state and basis options and overrides the
! operations listed below. Pointer factories transfer ownership to their
! caller. Reset a target with its DEALLOCATE binding, then free owned
! allocations with intrinsic DEALLOCATE. The declared FiniteElementDeallocate
! vector helpers currently lack implementations.
!
! Examples
!
!```fortran
! PROGRAM main
! USE TriangleH1HierarchicalFE_Class, ONLY: FE_ => TriangleH1HierarchicalFE_
! USE Test_Method, ONLY: OK
! USE TriangleH1HierarchicalFE_Class, ONLY: &
!   NewFE => TriangleH1HierarchicalFEPointer
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
TYPE, EXTENDS(TriangleH1FE_) :: TriangleH1HierarchicalFE_
CONTAINS
  ! Deferred methods from TriangleH1FE_
  !@Methods

  ! Deferred methods from AbstractFE_
  !@Methods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Evaluate reference cell shape data.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetLocalFacetElemShapeData => obj_GetLocalFacetElemShapeData
  !! Evaluate reference cell traces and facet shapes.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Map cell shape data to physical coordinates.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetGlobalFacetElemShapeData => obj_GetGlobalFacetElemShapeData
  !! Map facet shapes and compute normals.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Update triangle hierarchical degrees and DOF count.
END TYPE TriangleH1HierarchicalFE_

!----------------------------------------------------------------------------
!                                            TriangleH1HierarchicalFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable TriangleH1HierarchicalFE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: TriangleH1HierarchicalFEPointer_
  CLASS(TriangleH1HierarchicalFE_), POINTER :: ptr => NULL()
END TYPE TriangleH1HierarchicalFEPointer_

!----------------------------------------------------------------------------
!                                     TriangleH1HierarchicalFEPointer@Methods
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
! ans : TYPE(TriangleH1HierarchicalFE_) - Allocated finite element target
! returned to the caller.
!
! Examples
!
!```fortran
! PROGRAM main
! USE TriangleH1HierarchicalFE_Class, ONLY: FE_ => TriangleH1HierarchicalFE_
! USE Test_Method, ONLY: OK
! USE TriangleH1HierarchicalFE_Class, ONLY: &
!   NewFE => TriangleH1HierarchicalFEPointer
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

INTERFACE TriangleH1HierarchicalFEPointer
  MODULE FUNCTION obj_TriangleH1HierarchicalFEPointer1() RESULT(ans)
    TYPE(TriangleH1HierarchicalFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_TriangleH1HierarchicalFEPointer1
END INTERFACE TriangleH1HierarchicalFEPointer

!----------------------------------------------------------------------------
!                                           GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Evaluate reference cell shape data
!
! Delegates to obj%opt%TriangleH1HieFE_GetLocalElemShapeData. Uses
! caller-prepared quadrature to evaluate basis values, reference derivatives
! and weights. The basis options may allocate or resize internal work arrays.
! Physical mapping is performed separately.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1HierarchicalFE_Class, ONLY: FE_ => TriangleH1HierarchicalFE_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(FE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Hierarchy", &
!                   cellOrder=[2_I4B], tcell=2_I4B, &
!                   cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!                   faceOrder=RESHAPE([2_I4B], [1, 1]), &
!                   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                        baseContinuity="H1", baseInterpolation="Hierarchy", &
!                        cellOrder=[1_I4B], tcell=2_I4B, &
!                        cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!                        faceOrder=RESHAPE([1_I4B], [1, 1]), &
!                        faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!                        ipType=TypeInterpolationOpt%Equidistance, &
!                        quadratureIsHomogeneous=.TRUE., &
!                        quadratureIsOrder=.TRUE., &
!                        quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns .EQ. obj%opt%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips .GT. 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!                     DIM=1) - 1.0_DFP) .LT. 1.0E-10_DFP), &
!                     "Vertex partition of unity")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) .GE. -1.0E-10_DFP), &
!         "Physical coordinates inside the unit cell")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns .EQ. 3_I4B, "Quadratic facet basis size")
! CALL obj%GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips .GT. 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(TriangleH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or reference cell points restricted to a facet.
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetLocalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Evaluate reference cell traces and facet shapes
!
! Delegates to obj%opt%TriangleH1HieFE_GetLocalFacetElemShapeData. Uses
! caller-prepared matching cell-trace and facet quadrature rules; evaluates
! basis values, derivatives and weights on the selected facet.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1HierarchicalFE_Class, ONLY: FE_ => TriangleH1HierarchicalFE_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(FE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Hierarchy", &
!                   cellOrder=[2_I4B], tcell=2_I4B, &
!                   cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!                   faceOrder=RESHAPE([2_I4B], [1, 1]), &
!                   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                        baseContinuity="H1", baseInterpolation="Hierarchy", &
!                        cellOrder=[1_I4B], tcell=2_I4B, &
!                        cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!                        faceOrder=RESHAPE([1_I4B], [1, 1]), &
!                        faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!                        ipType=TypeInterpolationOpt%Equidistance, &
!                        quadratureIsHomogeneous=.TRUE., &
!                        quadratureIsOrder=.TRUE., &
!                        quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns .EQ. obj%opt%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips .GT. 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!                     DIM=1) - 1.0_DFP) .LT. 1.0E-10_DFP), &
!                     "Vertex partition of unity")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) .GE. -1.0E-10_DFP), &
!         "Physical coordinates inside the unit cell")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns .EQ. 3_I4B, "Quadratic facet basis size")
! CALL obj%GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips .GT. 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(TriangleH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Facet shape data at the corresponding facet quadrature points.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or reference cell points restricted to a facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Corresponding quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE obj_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Map cell shape data to physical coordinates
!
! Delegates to obj%opt%TriangleH1LagFE_GetGlobalElemShapeData. Requires
! prepared local solution and geometry shapes at matching reference
! coordinates and a consistent reference domain. Uses xij with the geometry
! basis to construct physical coordinates, Jacobian data and derivatives.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1HierarchicalFE_Class, ONLY: FE_ => TriangleH1HierarchicalFE_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(FE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Hierarchy", &
!                   cellOrder=[2_I4B], tcell=2_I4B, &
!                   cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!                   faceOrder=RESHAPE([2_I4B], [1, 1]), &
!                   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                        baseContinuity="H1", baseInterpolation="Hierarchy", &
!                        cellOrder=[1_I4B], tcell=2_I4B, &
!                        cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!                        faceOrder=RESHAPE([1_I4B], [1, 1]), &
!                        faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!                        ipType=TypeInterpolationOpt%Equidistance, &
!                        quadratureIsHomogeneous=.TRUE., &
!                        quadratureIsOrder=.TRUE., &
!                        quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns .EQ. obj%opt%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips .GT. 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!                     DIM=1) - 1.0_DFP) .LT. 1.0E-10_DFP), &
!                     "Vertex partition of unity")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) .GE. -1.0E-10_DFP), &
!         "Physical coordinates inside the unit cell")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns .EQ. 3_I4B, "Quadratic facet basis size")
! CALL obj%GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips .GT. 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoelemsd)
    CLASS(TriangleH1HierarchicalFE_), INTENT(INOUT) :: obj
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
!                                      GetGlobalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-16
! summary: Map facet shapes and compute normals
!
! Delegates to obj%opt%TriangleH1LagFE_GetGlobalFacetElemShapeData. Requires
! prepared local solution and geometry data at matching reference points and
! physical geometry-node coordinates. Linear two-endpoint facet geometry is
! used by the delegated mapping.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1HierarchicalFE_Class, ONLY: FE_ => TriangleH1HierarchicalFE_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(FE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Hierarchy", &
!                   cellOrder=[2_I4B], tcell=2_I4B, &
!                   cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!                   faceOrder=RESHAPE([2_I4B], [1, 1]), &
!                   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                        baseContinuity="H1", baseInterpolation="Hierarchy", &
!                        cellOrder=[1_I4B], tcell=2_I4B, &
!                        cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!                        faceOrder=RESHAPE([1_I4B], [1, 1]), &
!                        faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!                        ipType=TypeInterpolationOpt%Equidistance, &
!                        quadratureIsHomogeneous=.TRUE., &
!                        quadratureIsOrder=.TRUE., &
!                        quadratureOrder=[6_I4B])
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad)
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData( &
!   elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nns .EQ. obj%opt%GetTotalDOF(), "Local basis size")
! CALL OK(shapes%nips .GT. 0_I4B, "Local quadrature size")
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
!                     DIM=1) - 1.0_DFP) .LT. 1.0E-10_DFP), &
!                     "Vertex partition of unity")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, xij=xij, geoelemsd=geoShapes)
! CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) .GE. -1.0E-10_DFP), &
!         "Physical coordinates inside the unit cell")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL obj%GetLocalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
!   localFaceNumber=1_I4B)
! CALL geometry%GetLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
!   facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(facets%nns .EQ. 3_I4B, "Quadratic facet basis size")
! CALL obj%GetGlobalFacetElemShapeData( &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets%nips .GT. 0_I4B, "Mapped facet data")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
    CLASS(TriangleH1HierarchicalFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Facet shape data at the corresponding facet quadrature points.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd
    !! Required prepared geometry shapes at matching reference points.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoFacetElemsd
    !! Required prepared facet geometry shapes; linear endpoint geometry.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry-node coordinates; coordinate directions are rows.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE obj_GetGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Update triangle hierarchical degrees and DOF count
!
! When order is present, broadcasts it to cell and face arrays, stores three
! cell entries and currently four face-order columns, then recomputes the
! triangular hierarchical DOF count and returns. Otherwise updates supplied
! cellOrder and faceOrder using their active sizes and computes the count from
! the first cell degree and three facet degrees. The four-column isotropic
! branch is recorded in bugs_report/H1FE.md.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TriangleH1HierarchicalFE_Class, ONLY: FE_ => TriangleH1HierarchicalFE_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! INTEGER(I4B) :: faceOrders(3, 6), nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Hierarchical", &
!   cellOrder=[2_I4B], tcell=2_I4B, &
!   cellOrient=[1_I4B, 2_I4B, 3_I4B], &
!   faceOrder=RESHAPE([2_I4B], [1, 1]), &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! CALL obj%SetOrder(order=3_I4B)
! CALL OK(obj%opt%GetTotalDOF() == 10_I4B, "Cubic basis DOF count")
! CALL obj%opt%GetFaceOrder(ans=faceOrders, nrow=nrow, ncol=ncol)
! CALL OK(ncol == 3_I4B, "Triangle has three face-order columns")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_SetOrder( &
    obj, order, anisoOrder, cellOrder, faceOrder, edgeOrder, tCell, tFace, &
    tEdge, errCheck)
    CLASS(TriangleH1HierarchicalFE_), INTENT(INOUT) :: obj
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
!
!----------------------------------------------------------------------------

END MODULE TriangleH1HierarchicalFE_Class
