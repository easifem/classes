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
! summary: TetrahedronH1FE public interfaces
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1FE
! submodule.
!
MODULE TetrahedronH1FE_Class
USE AbstractFE_Class, ONLY: AbstractFE_
USE BaseType, ONLY: QuadraturePoint_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: TetrahedronH1FE_
PUBLIC :: TetrahedronH1FEPointer_

CHARACTER(*), PARAMETER :: modName = "TetrahedronH1FE_Class"

!----------------------------------------------------------------------------
!                                                           TetrahedronH1FE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: TetrahedronH1FE finite element interfaces
!
! Extends AbstractFE_ without introducing additional data components. Inherits
! the initialization state and basis options and overrides the operations
! listed below. This type is abstract; use a concrete Lagrange or hierarchical
! extension. Some declared operations are WIP placeholders and report
! under-development errors in DEBUG mode; see each interface and
! bugs_report/H1FE.md.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad
!
! ! Topology operation is currently a WIP placeholder.
! CALL obj%GetQuadraturePoints(quad)
! CALL OK(obj%GetTotalQuadraturePoints() > 0_I4B, "Quadrature populated")
! END PROGRAM main
!```
!
TYPE, ABSTRACT, EXTENDS(AbstractFE_) :: TetrahedronH1FE_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Get Quadrature Points (WIP).
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetFacetQuadraturePoints => obj_GetFacetQuadraturePoints
  !! Get Facet Quadrature Points (WIP).
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set Quadrature Order (WIP).
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set Quadrature Type (WIP).
  PROCEDURE, PUBLIC, PASS(obj) :: GetInterpolationPoints => &
    obj_GetInterpolationPoints
  !! Get Interpolation Points (WIP).
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetTotalInterpolationPoints => obj_GetTotalInterpolationPoints
  !! Get Total Interpolation Points (WIP).
END TYPE TetrahedronH1FE_

!----------------------------------------------------------------------------
!                                                    TetrahedronH1FEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Nullable TetrahedronH1FE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: TetrahedronH1FEPointer_
  CLASS(TetrahedronH1FE_), POINTER :: ptr => NULL()
END TYPE TetrahedronH1FEPointer_

!----------------------------------------------------------------------------
!                                                 GetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Get Quadrature Points (WIP)
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
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad
!
! ! Topology operation is currently a WIP placeholder.
! CALL obj%GetQuadraturePoints(quad)
! CALL OK(obj%GetTotalQuadraturePoints() > 0_I4B, "Quadrature populated")
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(TetrahedronH1FE_), INTENT(INOUT) :: obj
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
! summary: Get Facet Quadrature Points (WIP)
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
! USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(FE_) :: obj
! TYPE(QuadraturePoint_) :: quad, facetQuad
!
! ! Topology operation is currently a WIP placeholder.
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
! CALL OK(GetTotalQuadraturePoints(facetQuad) > 0_I4B, &
!   "Facet quadrature populated")
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                 localFaceNumber)
    CLASS(TetrahedronH1FE_), INTENT(INOUT) :: obj
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
! summary: Set Quadrature Order (WIP)
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
! INTEGER(I4B) :: orders(3)
!
! ! Topology operation is currently a WIP placeholder.
! CALL obj%SetQuadratureOrder(order1=4_I4B)
! CALL obj%GetParam(quadratureOrder=orders)
! CALL OK(orders(1) == 4_I4B, "Quadrature order set")
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureOrder(obj, order, order1, order2, order3)
    CLASS(TetrahedronH1FE_), INTENT(INOUT) :: obj
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
! summary: Set Quadrature Type (WIP)
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
! INTEGER(I4B) :: types(3)
!
! ! Topology operation is currently a WIP placeholder.
! CALL obj%SetQuadratureType(quadratureType1=1_I4B)
! CALL obj%GetParam(quadratureType=types)
! CALL OK(types(1) == 1_I4B, "Quadrature type set")
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureType( &
    obj, quadratureType, quadratureType1, quadratureType2, quadratureType3)
    CLASS(TetrahedronH1FE_), INTENT(INOUT) :: obj
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
! summary: Get Total Interpolation Points (WIP)
!
! This binding is currently a WIP placeholder. It does not perform the
! operation described by the interface name. The implementation raises an
! under-development error in DEBUG mode. The example intentionally exposes the
! unsupported call.
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
! USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! INTEGER(I4B) :: count
!
! ! Topology operation is currently a WIP placeholder.
! count = obj%GetTotalInterpolationPoints( &
!   order=[1_I4B], ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(count > 0_I4B, "Interpolation point count")
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE FUNCTION obj_GetTotalInterpolationPoints( &
    obj, order, ipType) RESULT(ans)
    CLASS(TetrahedronH1FE_), INTENT(INOUT) :: obj
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
! summary: Get Interpolation Points (WIP)
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
! USE BaseType, ONLY: TypeInterpolationOpt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(FE_) :: obj
! REAL(DFP) :: xij(3, 4), points(3, 27)
! INTEGER(I4B) :: nrow, ncol
!
! ! Topology operation is currently a WIP placeholder.
! xij = 0.0_DFP
! CALL obj%GetInterpolationPoints(xij=xij, ans=points, &
!   nrow=nrow, ncol=ncol, order=[1_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(ncol > 0_I4B, "Interpolation coordinates populated")
! END PROGRAM main
!```
!
! See also: [[AbstractFE_]]

INTERFACE
  MODULE SUBROUTINE obj_GetInterpolationPoints( &
    obj, xij, ans, nrow, ncol, order, ipType, alpha, beta, lambda)
    CLASS(TetrahedronH1FE_), INTENT(INOUT) :: obj
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
!
!----------------------------------------------------------------------------

END MODULE TetrahedronH1FE_Class
