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
! summary: LineH1FE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1FE
! submodule.
!
MODULE LineH1FE_Class
USE AbstractFE_Class, ONLY: AbstractFE_
USE BaseType, ONLY: QuadraturePoint_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: LineH1FE_
PUBLIC :: LineH1FEPointer_

CHARACTER(*), PARAMETER :: modName = "LineH1FE_Class"

!----------------------------------------------------------------------------
!                                                           LineH1FE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: LineH1FE finite element interfaces
!
! Extends AbstractFE_ without introducing additional data components. Inherits
! the initialization state and basis options and overrides the operations
! listed below. This type is abstract; use a concrete Lagrange or hierarchical
! extension.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
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
TYPE, ABSTRACT, EXTENDS(AbstractFE_) :: LineH1FE_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Construct cell quadrature.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetFacetQuadraturePoints => obj_GetFacetQuadraturePoints
  !! Construct quadrature on one local facet.
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set quadrature accuracy.
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set quadrature rule types.
  PROCEDURE, PUBLIC, PASS(obj) :: GetInterpolationPoints => &
    obj_GetInterpolationPoints
  !! Generate physical interpolation coordinates.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetTotalInterpolationPoints => obj_GetTotalInterpolationPoints
  !! Count interpolation points.
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrientation => obj_SetOrientation
  !! Set topology-specific orientation data.
END TYPE LineH1FE_

!----------------------------------------------------------------------------
!                                                    LineH1FEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable LineH1FE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: LineH1FEPointer_
  CLASS(LineH1FE_), POINTER :: ptr => NULL()
END TYPE LineH1FEPointer_

!----------------------------------------------------------------------------
!                                                 GetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Construct cell quadrature
!
! Delegates to obj%opt%Line_GetQuadraturePoints. Populates quad from the
! configured basis quadrature options.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
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
    CLASS(LineH1FE_), INTENT(INOUT) :: obj
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
! Delegates to obj%opt%Line_GetFacetQuadraturePoints. Produces matching
! cell-trace and facet-coordinate quadrature rules for localFaceNumber. The
! delegated line-facet rule is currently an under-development placeholder in
! QuadratureOpt_.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
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
    CLASS(LineH1FE_), INTENT(INOUT) :: obj
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
! Accepts an accuracy array or a scalar in the order order1, order2, order3;
! the first supplied form wins. Uses only the first array entry. Delegates to
! opt%Line_SetQuadratureOrder. Does not change approximation degrees.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE BaseType, ONLY: QuadraturePoint_
! USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
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
    CLASS(LineH1FE_), INTENT(INOUT) :: obj
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
! USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
! USE GlobalData, ONLY: I4B
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
! INTEGER(I4B) :: orders(3), types(3), count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
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
    CLASS(LineH1FE_), INTENT(INOUT) :: obj
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
!                                        GetTotalInterpolationPoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Count interpolation points
!
! Returns the H1 line count for order(1); ipType is not used.
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
! USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! REAL(DFP) :: xij(3, 8), points(3, 27)
! INTEGER(I4B) :: count, nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! CALL obj%GetRefElemCoord(ans=xij, nrow=nrow, ncol=ncol)
! count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL obj%GetInterpolationPoints(xij=xij(1:1, 1:2), ans=points, &
!   nrow=nrow, ncol=ncol, order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(ncol == count, "Interpolation output count")
! CALL OK(count == 3_I4B, "Quadratic point count")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE FUNCTION obj_GetTotalInterpolationPoints( &
    obj, order, ipType) RESULT(ans)
    CLASS(LineH1FE_), INTENT(INOUT) :: obj
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
!                                              GetInterpolationPoints@Methods
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
! USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! REAL(DFP) :: xij(3, 8), points(3, 27)
! INTEGER(I4B) :: count, nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! CALL obj%GetRefElemCoord(ans=xij, nrow=nrow, ncol=ncol)
! count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL obj%GetInterpolationPoints(xij=xij(1:1, 1:2), ans=points, &
!   nrow=nrow, ncol=ncol, order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(ncol == count, "Interpolation output count")
! CALL OK(count == 3_I4B, "Quadratic point count")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetInterpolationPoints( &
    obj, xij, ans, nrow, ncol, order, ipType, alpha, beta, lambda)
    CLASS(LineH1FE_), INTENT(INOUT) :: obj
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
!                                                   GetOrientation@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Set topology-specific orientation data
!
! Updates only cell orientation through opt%SetCellOrientation; face and edge
! arguments are currently unused.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! INTEGER(I4B) :: cell(3), tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", &
!   order=2_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[6_I4B])
! CALL obj%SetOrientation(cellOrient=[1_I4B], tcell=1_I4B)
! CALL obj%opt%GetCellOrder(ans=cell, tsize=tsize)
! CALL OK(tsize == 1_I4B, "Cell orientation active size")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_SetOrientation( &
    obj, cellOrient, faceOrient, edgeOrient, tCell, tFace, tEdge, errCheck)
    CLASS(LineH1FE_), INTENT(INOUT) :: obj
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

END MODULE LineH1FE_Class
