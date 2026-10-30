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
! summary: TriangleH1FE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1FE
! submodule.
!
MODULE TriangleH1FE_Class
USE AbstractFE_Class, ONLY: AbstractFE_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE

PRIVATE

PUBLIC :: TriangleH1FE_
PUBLIC :: TriangleH1FEPointer_

!----------------------------------------------------------------------------
!                                                           TriangleH1FE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: TriangleH1FE finite element interfaces
!
! Extends AbstractFE_ without introducing additional data components. Inherits
! the initialization state and basis options and overrides the operations
! listed below. This is a topology-level class; use a concrete basis-family
! extension for shape evaluation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=2_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) .EQ. 8_I4B, "Quadrature accuracy")
! CALL obj%SetQuadratureType(quadratureType1=types(1))
! CALL obj%GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count .EQ. obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count .GT. 0_I4B, "Nonempty quadrature")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) .GT. 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
TYPE, EXTENDS(AbstractFE_) :: TriangleH1FE_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Construct cell quadrature.
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetQuadraturePoints => &
    obj_GetFacetQuadraturePoints
  !! Construct quadrature on one local facet.
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set quadrature accuracy.
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set quadrature rule types.
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalInterpolationPoints => &
    obj_GetTotalInterpolationPoints
  !! Count interpolation points.
  PROCEDURE, PUBLIC, PASS(obj) :: GetInterpolationPoints => &
    obj_GetInterpolationPoints
  !! Generate physical interpolation coordinates.
  PROCEDURE, PUBLIC, PASS(obj) :: FixOrientOfNptrs => obj_FixOrientOfNptrs
  !! Reorder connectivity according to facet orientations.
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrientation => obj_SetOrientation
  !! Set topology-specific orientation data.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetFacetDOFValueFromConstant => obj_GetFacetDOFValueFromConstant
  !! Project the unit constant on a spatial facet.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetFacetDOFValueFromSTFunc => obj_GetFacetDOFValueFromSTFunc
  !! Project a user function on a spatial facet.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetSTFacetDOFValueFromSTFunc => obj_GetSTFacetDOFValueFromSTFunc
  !! Project coefficients on a space-time facet.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetSTFacetDOFValueFromConstant => obj_GetSTFacetDOFValueFromConstant
  !! Project coefficients on a space-time facet.
END TYPE TriangleH1FE_

!----------------------------------------------------------------------------
!                                                    TriangleH1FEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable TriangleH1FE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: TriangleH1FEPointer_
  CLASS(TriangleH1FE_), POINTER :: ptr => NULL()
END TYPE TriangleH1FEPointer_

!----------------------------------------------------------------------------
!                                             TriangleH1FEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Construct cell quadrature
!
! Delegates to obj%opt%Triangle_GetQuadraturePoints. Populates quad from the
! configured basis quadrature options.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=2_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) .EQ. 8_I4B, "Quadrature accuracy")
! CALL obj%SetQuadratureType(quadratureType1=types(1))
! CALL obj%GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count .EQ. obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count .GT. 0_I4B, "Nonempty quadrature")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) .GT. 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or reference cell points restricted to a facet.
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                  GetFacetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Construct quadrature on one local facet
!
! Delegates to obj%opt%Triangle_GetFacetQuadraturePoints. Produces matching
! cell-trace and facet-coordinate quadrature rules for localFaceNumber.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=2_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) .EQ. 8_I4B, "Quadrature accuracy")
! CALL obj%SetQuadratureType(quadratureType1=types(1))
! CALL obj%GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count .EQ. obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count .GT. 0_I4B, "Nonempty quadrature")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) .GT. 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                 localFaceNumber)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or reference cell points restricted to a facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Corresponding quadrature points in facet reference coordinates.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE obj_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set quadrature accuracy
!
! Uses order(1) when supplied, otherwise the first supplied scalar among
! order1, order2 and order3. Requires at least one form in DEBUG mode.
! Delegates to opt%Triangle_SetQuadratureOrder. Does not change approximation
! degrees.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=2_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) .EQ. 8_I4B, "Quadrature accuracy")
! CALL obj%SetQuadratureType(quadratureType1=types(1))
! CALL obj%GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count .EQ. obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count .GT. 0_I4B, "Nonempty quadrature")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) .GT. 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureOrder(obj, order, order1, order2, order3)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    !! Quadrature accuracy array or directional scalar value.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1
    !! Polynomial degree; scalar or directional input as declared in direction
    !! 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2
    !! Polynomial degree; scalar or directional input as declared in direction
    !! 2.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order3
    !! Polynomial degree; scalar or directional input as declared in direction
    !! 3.
  END SUBROUTINE obj_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuadratureType@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set quadrature rule types
!
! Uses the first entry of quadratureType when supplied, otherwise the first
! supplied scalar quadratureType1, quadratureType2 or quadratureType3.
! Delegates to the topology-specific BasisOpt_ setter.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=2_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[6_I4B])
!
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) .EQ. 8_I4B, "Quadrature accuracy")
! CALL obj%SetQuadratureType(quadratureType1=types(1))
! CALL obj%GetQuadraturePoints(quad)
! count = GetTotalQuadraturePoints(quad)
! CALL OK(count .EQ. obj%GetTotalQuadraturePoints(), "Quadrature count")
! CALL OK(count .GT. 0_I4B, "Nonempty quadrature")
! CALL obj%GetFacetQuadraturePoints( &
!   quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) .GT. 0_I4B, "Facet quadrature")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureType( &
    obj, quadratureType, quadratureType1, quadratureType2, quadratureType3)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    !! Quadrature rule type codes by reference direction in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    !! Quadrature rule type codes by reference direction in direction 2.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
    !! Quadrature rule type codes by reference direction in direction 3.
  END SUBROUTINE obj_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                           GetTotalInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Count interpolation points
!
! Returns one point for the Center point type; otherwise returns the H1
! triangle count for order(1).
!
! Returns:
! ans : INTEGER(I4B) - Number of interpolation points for the requested degree
! and point type.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! REAL(DFP) :: xij(3, 8), points(3, 27)
! INTEGER(I4B) :: count, nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! CALL obj%GetRefElemCoord(ans=xij, nrow=nrow, ncol=ncol)
! count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL obj%GetInterpolationPoints(xij=xij(1:2, 1:3), ans=points, &
!   nrow=nrow, ncol=ncol, order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(ncol == count, "Interpolation output count")
! CALL OK(count == 6_I4B, "Quadratic point count")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE FUNCTION obj_GetTotalInterpolationPoints( &
    obj, order, ipType) RESULT(ans)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! Polynomial degree; scalar or directional input as declared.
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! Interpolation point type code; use TypeInterpolationOpt constants.
    INTEGER(I4B) :: ans
    !! Number of interpolation points for the requested degree and point type.
  END FUNCTION obj_GetTotalInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                 GetInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Generate physical interpolation coordinates
!
! Generates physical interpolation points using the supplied vertex
! coordinates, degrees and point types. Writes the active coordinate block and
! returns nrow and ncol. Uses the first degree and point-type entries.
! Polynomial parameter defaults are alpha=0, beta=0 and lambda=0.5. Layout is
! VEFC.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! REAL(DFP) :: xij(3, 8), points(3, 27)
! INTEGER(I4B) :: count, nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! CALL obj%GetRefElemCoord(ans=xij, nrow=nrow, ncol=ncol)
! count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL obj%GetInterpolationPoints(xij=xij(1:2, 1:3), ans=points, &
!   nrow=nrow, ncol=ncol, order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(ncol == count, "Interpolation output count")
! CALL OK(count == 6_I4B, "Quadratic point count")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetInterpolationPoints( &
    obj, xij, ans, nrow, ncol, order, ipType, alpha, beta, lambda)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry-node coordinates; coordinate directions are rows.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Coordinate output; use ans(1:nrow,1:ncol); supply enough buffer space.
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! Number of active coordinate rows written to ans.
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! Number of active coordinate columns written to ans.
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! Polynomial degree; scalar or directional input as declared.
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! Interpolation point type code; use TypeInterpolationOpt constants.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi alpha parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi beta parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters by reference direction.
  END SUBROUTINE obj_GetInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                    FixOrientOfNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Reorder connectivity according to facet orientations
!
! Delegates to opt%Triangle_FixOrientOfNptrs. For isotropic degrees greater
! than two, reverses edge-interior blocks when the corresponding facet
! orientation is negative. Vertex and cell-interior entries remain unchanged.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! INTEGER(I4B) :: nptrs(10), orient(3, 3), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,10)]
! orient = 1_I4B
! orient(1, 1) = -1_I4B
! CALL obj%SetOrientation(faceOrient=orient, tFace=3_I4B)
! CALL obj%FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
! CALL OK(ALL(nptrs(1:3) == [(ii, ii=1,3)]), "Vertices retained")
! CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), &
!   "Reversed first edge")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_FixOrientOfNptrs(obj, nptrs, tsize)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! Node connectivity; the first tsize entries form the active buffer.
    INTEGER(I4B), INTENT(IN) :: tsize
    !! Number of active connectivity entries.
  END SUBROUTINE obj_FixOrientOfNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project the unit constant on a spatial facet
!
! Forms quadrature samples of one. With onlyFaceBubble true, subtracts the
! endpoint interpolant, excludes the two endpoint basis functions and solves
! for interior facet coefficients. Uses GetL2ProjectionDOFValueFromQuadrature
! with the supplied mass and pivot workspaces. icompo is unused.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, facets, geoShapes, geoFacets
! TYPE(QuadraturePoint_) :: quad, facetQuad
! REAL(DFP) :: xij(2, 3), ans(2), massMat(2, 2), nodal(2)
! REAL(DFP), ALLOCATABLE :: samples(:)
! INTEGER(I4B) :: ipiv(2), tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=1_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!                                       elemsd=shapes, facetElemsd=facets, &
!                                       geoElemsd=geoShapes, &
!                                       geoFacetElemsd=geoFacets, quad=quad, &
!                                       facetQuad=facetQuad, &
!                                       xij=xij, localFaceNumber=1_I4B)
! ALLOCATE (samples(facets%nips))
! nodal = 2.0_DFP
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!                           geoElemsd=geoShapes, &
!                           geoFacetElemsd=geoFacets, xij=xij, &
!                           localFaceNumber=1_I4B, func=nodal, ans=ans, &
!                           tsize=tsize, &
!                           massMat=massMat, ipiv=ipiv, funcValue=samples, &
!                           onlyFaceBubble=.FALSE., tVertices=2_I4B)
! CALL OK(tsize .EQ. 2_I4B, "Facet projection size")
! CALL OK(ALL(ABS(ans - 2.0_DFP) .LT. 1.0E-10_DFP), &
!   "Nodal constant projected")
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!                           xij=xij, localFaceNumber=1_I4B, ans=ans, &
!                           tsize=tsize, &
!                           massMat=massMat, ipiv=ipiv, funcValue=samples, &
!                           onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 1.0_DFP) .LT. 1.0E-10_DFP), "Unit constant projected")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetDOFValueFromConstant( &
    obj, elemsd, facetElemsd, xij, localFaceNumber, ans, tsize, &
    massMat, ipiv, funcValue, onlyFaceBubble, icompo)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Facet shape data at the corresponding facet quadrature points.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry-node coordinates; coordinate directions are rows.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! DOF coefficient buffer; active entries or ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Caller-owned mass-matrix workspace for projection-based overloads.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! Pivot workspace for projection-based overloads.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Caller-owned sample workspace; size must fit the active quadrature.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional vector component index where supported; scalar paths ignore
    !! it.
  END SUBROUTINE obj_GetFacetDOFValueFromConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                   GetFacetDOFValueFromUserFunction@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Reserved user-function interface
!
! This private interface has no matching implementation in the active
! submodule and is not a type-bound procedure. Use GetFacetDOFValueFromSTFunc
! through the public GetFacetDOFValue generic instead.
!
! See also: [[obj_GetFacetDOFValueFromSTFunc]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetDOFValueFromUserFunction( &
    obj, elemsd, facetElemsd, xij, localFaceNumber, func, ans, tsize, &
    massMat, ipiv, funcValue, onlyFaceBubble)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Facet shape data at the corresponding facet quadrature points.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry-node coordinates; coordinate directions are rows.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function as supported by this overload.
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
  END SUBROUTINE obj_GetFacetDOFValueFromUserFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project coefficients on a space-time facet
!
! Uses spatial facet and time-element shape data to construct endpoint,
! spatial-interior, temporal-interior and product-interior coefficients.
! Samples the scalar user function at physical space-time points.
! onlyFaceBubble restricts the returned spatial row range, while all temporal
! columns are included. Returns active ranges nrowStart:nrowEnd and
! ncolStart:ncolEnd. The function overload calls scalar Get; icompo is not
! used.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE LineH1LagrangeFE_Class, ONLY: LineH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(UserFunction_) :: func
! REAL(DFP) :: xij(2, 3)
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(LineH1LagrangeFE_) :: timeFE, timeGeometry
! TYPE(ElemShapeData_) :: shapes, facets, geoShapes, geoFacets
! TYPE(ElemShapeData_) :: timeShapes, geoTimeShapes
! TYPE(QuadraturePoint_) :: quad, facetQuad, timeQuad
! REAL(DFP) :: ans(4, 4), massMat(16, 16), samples(100, 100), temp(16)
! REAL(DFP) :: timeCoords(1, 2)
! INTEGER(I4B) :: ipiv(16), r1, r2, c1, c2
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=1_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!                    argType=TypeFEVariableOpt%space, numArgs=4_I4B, &
!                    numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!                                       elemsd=shapes, facetElemsd=facets, &
!                                       geoElemsd=geoShapes, &
!                                       geoFacetElemsd=geoFacets, quad=quad, &
!                                       facetQuad=facetQuad, &
!                                       xij=xij, localFaceNumber=1_I4B)
! CALL timeFE%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!                      baseContinuity="H1", baseInterpolation="Lagrange", &
!                      order=1_I4B, &
!                      ipType=TypeInterpolationOpt%Equidistance, &
!                      quadratureIsHomogeneous=.TRUE., &
!                      quadratureIsOrder=.TRUE., &
!                      quadratureOrder=[8_I4B])
! CALL timeGeometry%Copy(timeFE)
! CALL timeFE%SetOrder(order=3_I4B)
! timeCoords(1, :) = [0.0_DFP, 1.0_DFP]
! CALL timeFE%GetQuadraturePoints(quad=timeQuad)
! CALL timeFE%GetLocalElemShapeData(elemsd=timeShapes, quad=timeQuad)
! CALL timeGeometry%GetLocalElemShapeData( &
!   elemsd=geoTimeShapes, quad=timeQuad)
! CALL timeFE%GetGlobalElemShapeData( &
!   elemsd=timeShapes, geoelemsd=geoTimeShapes, xij=timeCoords)
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!                           timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, &
!                           1.0_DFP], &
!                           localFaceNumber=1_I4B, ans=ans, nrowStart=r1, &
!                           nrowEnd=r2, &
!                           ncolStart=c1, ncolEnd=c2, massMat=massMat, &
!                           ipiv=ipiv, &
!                           funcValue=samples, temp=temp, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(r1 .EQ. 3_I4B .AND. r2 .EQ. 4_I4B, "Spatial bubble rows")
! CALL OK(c1 .EQ. 1_I4B .AND. c2 .EQ. 4_I4B, "All temporal columns")
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!                           timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, &
!                           1.0_DFP], &
!                           localFaceNumber=1_I4B, func=func, ans=ans, &
!                           nrowStart=r1, nrowEnd=r2, ncolStart=c1, &
!                           ncolEnd=c2, &
!                           massMat=massMat, ipiv=ipiv, funcValue=samples, &
!                           temp=temp, &
!                           onlyFaceBubble=.TRUE.)
! CALL OK(r1 .EQ. 3_I4B .AND. c2 .EQ. 4_I4B, "User-function output range")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! CALL timeFE%DEALLOCATE()
! CALL timeGeometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetSTFacetDOFValueFromSTFunc( &
    obj, elemsd, facetElemsd, timeElemsd, xij, times, localFaceNumber, &
    func, ans, nrowStart, nrowEnd, ncolStart, ncolEnd, massMat, ipiv, &
    funcValue, temp, onlyFaceBubble, icompo)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Facet shape data at the corresponding facet quadrature points.
    TYPE(ElemShapeData_), INTENT(INOUT) :: timeElemsd
    !! Prepared time-element shape data.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry-node coordinates; coordinate directions are rows.
    REAL(DFP), INTENT(IN) :: times(:)
    !! Time interval endpoints; SIZE(times) = 2.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function as supported by this overload.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! DOF coefficient buffer; active entries or ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: nrowStart
    !! First active spatial coefficient row in ans.
    INTEGER(I4B), INTENT(OUT) :: nrowEnd
    !! Last active spatial coefficient row in ans.
    INTEGER(I4B), INTENT(OUT) :: ncolStart
    !! First active time coefficient column in ans.
    INTEGER(I4B), INTENT(OUT) :: ncolEnd
    !! Last active time coefficient column in ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Caller-owned mass-matrix workspace for projection-based overloads.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! Pivot workspace for projection-based overloads.
    REAL(DFP), INTENT(INOUT) :: funcValue(:, :)
    !! Caller-owned sample workspace; size must fit the active quadrature.
    REAL(DFP), INTENT(INOUT) :: temp(:)
    !! Coefficient workspace; SIZE(temp) must fit the projected DOF block.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional vector component index where supported; scalar paths ignore
    !! it.
  END SUBROUTINE obj_GetSTFacetDOFValueFromSTFunc
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project coefficients on a space-time facet
!
! Uses spatial facet and time-element shape data to construct endpoint,
! spatial-interior, temporal-interior and product-interior coefficients. Uses
! the unit constant one. onlyFaceBubble restricts the returned spatial row
! range, while all temporal columns are included. Returns active ranges
! nrowStart:nrowEnd and ncolStart:ncolEnd. The function overload calls scalar
! Get; icompo is not used.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE LineH1LagrangeFE_Class, ONLY: LineH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(UserFunction_) :: func
! REAL(DFP) :: xij(2, 3)
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(LineH1LagrangeFE_) :: timeFE, timeGeometry
! TYPE(ElemShapeData_) :: shapes, facets, geoShapes, geoFacets
! TYPE(ElemShapeData_) :: timeShapes, geoTimeShapes
! TYPE(QuadraturePoint_) :: quad, facetQuad, timeQuad
! REAL(DFP) :: ans(4, 4), massMat(16, 16), samples(100, 100), temp(16)
! REAL(DFP) :: timeCoords(1, 2)
! INTEGER(I4B) :: ipiv(16), r1, r2, c1, c2
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=1_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!                    argType=TypeFEVariableOpt%space, numArgs=4_I4B, &
!                    numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!                                       elemsd=shapes, facetElemsd=facets, &
!                                       geoElemsd=geoShapes, &
!                                       geoFacetElemsd=geoFacets, quad=quad, &
!                                       facetQuad=facetQuad, &
!                                       xij=xij, localFaceNumber=1_I4B)
! CALL timeFE%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!                      baseContinuity="H1", baseInterpolation="Lagrange", &
!                      order=1_I4B, &
!                      ipType=TypeInterpolationOpt%Equidistance, &
!                      quadratureIsHomogeneous=.TRUE., &
!                      quadratureIsOrder=.TRUE., &
!                      quadratureOrder=[8_I4B])
! CALL timeGeometry%Copy(timeFE)
! CALL timeFE%SetOrder(order=3_I4B)
! timeCoords(1, :) = [0.0_DFP, 1.0_DFP]
! CALL timeFE%GetQuadraturePoints(quad=timeQuad)
! CALL timeFE%GetLocalElemShapeData(elemsd=timeShapes, quad=timeQuad)
! CALL timeGeometry%GetLocalElemShapeData( &
!   elemsd=geoTimeShapes, quad=timeQuad)
! CALL timeFE%GetGlobalElemShapeData( &
!   elemsd=timeShapes, geoelemsd=geoTimeShapes, xij=timeCoords)
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!                           timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, &
!                           1.0_DFP], &
!                           localFaceNumber=1_I4B, ans=ans, nrowStart=r1, &
!                           nrowEnd=r2, &
!                           ncolStart=c1, ncolEnd=c2, massMat=massMat, &
!                           ipiv=ipiv, &
!                           funcValue=samples, temp=temp, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(r1 .EQ. 3_I4B .AND. r2 .EQ. 4_I4B, "Spatial bubble rows")
! CALL OK(c1 .EQ. 1_I4B .AND. c2 .EQ. 4_I4B, "All temporal columns")
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!                           timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, &
!                           1.0_DFP], &
!                           localFaceNumber=1_I4B, func=func, ans=ans, &
!                           nrowStart=r1, nrowEnd=r2, ncolStart=c1, &
!                           ncolEnd=c2, &
!                           massMat=massMat, ipiv=ipiv, funcValue=samples, &
!                           temp=temp, &
!                           onlyFaceBubble=.TRUE.)
! CALL OK(r1 .EQ. 3_I4B .AND. c2 .EQ. 4_I4B, "User-function output range")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! CALL timeFE%DEALLOCATE()
! CALL timeGeometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetSTFacetDOFValueFromConstant( &
    obj, elemsd, facetElemsd, timeElemsd, xij, times, localFaceNumber, &
    ans, nrowStart, nrowEnd, ncolStart, ncolEnd, massMat, ipiv, funcValue, &
    temp, onlyFaceBubble, icompo)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Facet shape data at the corresponding facet quadrature points.
    TYPE(ElemShapeData_), INTENT(INOUT) :: timeElemsd
    !! Prepared time-element shape data.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry-node coordinates; coordinate directions are rows.
    REAL(DFP), INTENT(IN) :: times(:)
    !! Time interval endpoints; SIZE(times) = 2.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! DOF coefficient buffer; active entries or ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: nrowStart
    !! First active spatial coefficient row in ans.
    INTEGER(I4B), INTENT(OUT) :: nrowEnd
    !! Last active spatial coefficient row in ans.
    INTEGER(I4B), INTENT(OUT) :: ncolStart
    !! First active time coefficient column in ans.
    INTEGER(I4B), INTENT(OUT) :: ncolEnd
    !! Last active time coefficient column in ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Caller-owned mass-matrix workspace for projection-based overloads.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! Pivot workspace for projection-based overloads.
    REAL(DFP), INTENT(INOUT) :: funcValue(:, :)
    !! Caller-owned sample workspace; size must fit the active quadrature.
    REAL(DFP), INTENT(INOUT) :: temp(:)
    !! Coefficient workspace; SIZE(temp) must fit the projected DOF block.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional vector component index where supported; scalar paths ignore
    !! it.
  END SUBROUTINE obj_GetSTFacetDOFValueFromConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetOrientation@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Set topology-specific orientation data
!
! Updates cell and facet orientations through BasisOpt_. Edge data are
! forwarded with validation disabled. Active sizes are shared with the
! corresponding order arrays.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! INTEGER(I4B) :: nptrs(10), orient(3, 3), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,10)]
! orient = 1_I4B
! orient(1, 1) = -1_I4B
! CALL obj%SetOrientation(faceOrient=orient, tFace=3_I4B)
! CALL obj%FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
! CALL OK(ALL(nptrs(1:3) == [(ii, ii=1,3)]), "Vertices retained")
! CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), &
!   "Reversed first edge")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_SetOrientation( &
    obj, cellOrient, faceOrient, edgeOrient, tCell, tFace, tEdge, errCheck)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
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
!                                                   GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project a user function on a spatial facet
!
! Samples a scalar function or selected vector component at the supplied time,
! subtracts endpoint interpolation when onlyFaceBubble is true, then performs
! an L2 projection onto the selected facet basis block. The vector component
! defaults to one; the temporary vector return buffer has ten entries. Work
! arrays must fit the facet basis and quadrature sizes.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(UserFunction_) :: func
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes(3), geoShapes(3), facets(3), geoFacets(3)
! TYPE(ElemShapeData_) :: cellShapes, geoCellShapes
! TYPE(QuadraturePoint_) :: quad(3), facetQuad(3), cellQuad
! REAL(DFP) :: ans(10), massMat(10, 10), temp(10)
! REAL(DFP) :: facetAns(10)
! REAL(DFP), ALLOCATABLE :: samples(:)
! INTEGER(I4B) :: ipiv(10), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!                   baseContinuity="H1", baseInterpolation="Lagrange", &
!                   order=1_I4B, &
!                   ipType=TypeInterpolationOpt%Equidistance, &
!                   quadratureIsHomogeneous=.TRUE., &
!   quadratureIsOrder=.TRUE., &
!                   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!                    argType=TypeFEVariableOpt%space, numArgs=4_I4B, &
!                    numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! ! Use cubic facet basis functions.
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetQuadraturePoints(quad=cellQuad)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!                                        quad=quad, facetQuad=facetQuad, &
!                                        tsize=count)
! CALL geometry%GetAllLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!                                         geoElemsd=geoShapes, &
!                                         geoFacetElemsd=geoFacets, xij=xij, &
!                                         tsize=count)
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, elemsd=cellShapes, &
!                                  geoelemsd=geoCellShapes, xij=xij, &
!                                  quad=cellQuad)
! ALLOCATE (samples(MAX(cellShapes%nips, MAXVAL(facets%nips))))
! CALL obj%GetFacetDOFValue(elemsd=shapes(1), facetElemsd=facets(1), &
!                           xij=xij, times=0.0_DFP, localFaceNumber=1_I4B, &
!                           func=func, &
!                           ans=facetAns, tsize=tsize, massMat=massMat, &
!                           ipiv=ipiv, &
!                           funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(tsize .EQ. 4_I4B, "Cubic facet projection size")
! CALL OK(ALL(ABS(facetAns(1:tsize) - 2.0_DFP) .LT. 1.0E-9_DFP), &
!         "User function projected on facet")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetDOFValueFromSTFunc( &
    obj, elemsd, facetElemsd, xij, times, localFaceNumber, func, ans, tsize, &
    massMat, ipiv, funcValue, onlyFaceBubble, icompo)
    CLASS(TriangleH1FE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Cell shape data, or cell traces at a facet quadrature rule.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Facet shape data at the corresponding facet quadrature points.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical geometry-node coordinates; coordinate directions are rows.
    REAL(DFP), INTENT(IN) :: times
    !! Physical evaluation time.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function as supported by this overload.
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
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, return only interior spatial DOFs; see method description.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional vector component index where supported; scalar paths ignore
    !! it.
  END SUBROUTINE obj_GetFacetDOFValueFromSTFunc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TriangleH1FE_Class
