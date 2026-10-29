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
! summary: Abstract finite element configuration and projection interfaces
!
! Defines AbstractFE_, its pointer wrapper and exported lifecycle helpers.
! Concrete element classes implement topology-specific operations; shared
! wrappers coordinate quadrature, shape evaluation and DOF projection.

MODULE AbstractFE_Class
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BasisOpt_Class, ONLY: BasisOpt_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE
PRIVATE

PUBLIC :: AbstractFE_
PUBLIC :: AbstractFEPointer_
PUBLIC :: AbstractFEDeallocate
PUBLIC :: AbstractFEDisplay
PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                                 AbstractFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Shared finite element configuration and polymorphic operations
!
! AbstractFE_ cannot be instantiated directly. A concrete child supplies
! topology-specific quadrature, interpolation, shape evaluation, orientation
! and facet projection. The base provides lifecycle operations, option access,
! all-facet loops and DOF projection workflows. Its public components are
! isInit (initially false) and opt (BasisOpt_, containing basis, topology,
! orders, orientations, quadrature and working data). Initialization sets
! isInit; deallocation clears it. The flag alone does not guarantee that a
! child implements every operation. Copy and assignment transfer only this
! shared state. Error checks and placeholder diagnostics run in DEBUG mode.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: other
! INTEGER(I4B) :: order, nsd
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%isInit, "Initiate sets the initialization flag")
! CALL obj%GetParam(order=order, nsd=nsd)
! CALL OK(order == 1_I4B .AND. nsd == 2_I4B, "Read initial options")
! CALL other%Copy(obj)
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%isInit, "Deallocate resets initialization")
! CALL OK(other%isInit, "Copy has independent initialization state")
! obj = other
! CALL OK(obj%isInit, "Defined assignment copies shared state")
! CALL obj%SetParam(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "SetParam updates stored options")
! CALL other%GetParam(order=order)
! CALL OK(order == 1_I4B, "Copy retains its own options")
! CALL obj%DEALLOCATE()
! CALL other%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFEPointer_]], [[obj_Initiate]]

TYPE, ABSTRACT :: AbstractFE_
  ! PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  !! Initialization flag; set on initiation, cleared by DEALLOCATE.
  TYPE(BasisOpt_) :: opt
  !! Shared basis, topology, order, orientation and quadrature configuration.

CONTAINS
  ! CONSTRUCTOR:
  !@ConstructorMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initialize finite element basis options.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy initialization state and basis options.
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Initiate by copy

  !IO:
  !@IOMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display initialization state and basis options.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: MdEncode => obj_MdEncode
  !! Encode basis options as Markdown.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ReactEncode => &
    obj_ReactEncode
  !! Encode basis options as React markup.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Reset initialization state and release basis options.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ImportFromToml1 => &
    obj_ImportFromToml1
  !! Initialize basis options from a TOML table.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  !! Initialize basis options from a named TOML table.
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Initialize shared options from a TOML table or named file table.

  ! SET:
  ! @SetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Update selected stored basis parameters.
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set approximation orders through a child implementation.
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrientation => obj_SetOrientation
  !! Set cell, face and edge orientations in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set quadrature accuracy in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set quadrature rules in a child class.

  !GET:
  ! @GetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetRefElemCoord => &
    obj_GetRefElemCoord
  !! Copy reference element vertex coordinates.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTopologyType => &
    obj_GetTopologyType
  !! Return the reference topology code.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Read selected stored basis parameters.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Return the basis interpolation code.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseContinuity => &
    obj_GetBaseContinuity
  !! Return the basis continuity code.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetIpType => &
    obj_GetIpType
  !! Return the interpolation point type code.
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Evaluate reference cell shape data in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Map cell shape data in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData2 => &
    obj_GetGlobalElemShapeData2
  !! Prepare and map cell shape data.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetLocalFacetElemShapeData => obj_GetLocalFacetElemShapeData
  !! Evaluate reference facet shape data in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetAllLocalFacetElemShapeData => obj_GetAllLocalFacetElemShapeData
  !! Evaluate local shape data for every facet.
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFacetElemShapeData => &
    obj_GetGlobalFacetElemShapeData
  !! Map facet shape data in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFacetElemShapeData2 => &
    obj_GetGlobalFacetElemShapeData2
  !! Prepare and map shape data on one facet.
  PROCEDURE, PUBLIC, PASS(obj) :: GetAllGlobalFacetElemShapeData => &
    obj_GetAllGlobalFacetElemShapeData
  !! Map prepared shape data for every facet.

  ! GET:
  ! @QuadratureMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Construct cell quadrature in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetFacetQuadraturePoints => obj_GetFacetQuadraturePoints
  !! Construct facet quadrature in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetAllFacetQuadraturePoints => obj_GetAllFacetQuadraturePoints
  !! Obtain quadrature rules for every facet.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetTotalQuadraturePoints => obj_GetTotalQuadraturePoints
  !! Return the configured cell quadrature point count.

  ! GET:
  ! @InterpolationMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetInterpolationPoints => &
    obj_GetInterpolationPoints
  !! Construct physical interpolation points in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetTotalInterpolationPoints => obj_GetTotalInterpolationPoints
  !! Count interpolation points in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: FixOrientOfNptrs => obj_FixOrientOfNptrs
  !! Orient connectivity in a child class.

  ! GET:
  ! @FacetDOFMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetFacetDOFValueFromVertex => obj_GetFacetDOFValueFromVertex
  !! Project geometry nodal values onto a facet basis.
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetFacetDOFValueFromConstant => obj_GetFacetDOFValueFromConstant
  !! Project the unit constant on a facet in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetDOFValueFromSTFunc => &
    obj_GetFacetDOFValueFromSTFunc
  !! Project a user function on a facet in a child class.
  PROCEDURE, PUBLIC, PASS(obj) :: GetSTFacetDOFValueFromSTFunc => &
    obj_GetSTFacetDOFValueFromSTFunc
  !! Project a user function on a space-time facet.
  PROCEDURE, PUBLIC, PASS(obj) :: GetSTFacetDOFValueFromConstant => &
    obj_GetSTFacetDOFValueFromConstant
  !! Project the unit constant on a space-time facet.
  GENERIC, PUBLIC :: GetFacetDOFValue => &
    GetFacetDOFValueFromSTFunc, &
    GetFacetDOFValueFromVertex, &
    GetFacetDOFValueFromConstant, &
    GetSTFacetDOFValueFromSTFunc, &
    GetSTFacetDOFValueFromConstant
  !! Project nodal values, a user function or the unit constant on a facet.

  ! GET:
  ! @CellDOFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetVertexDOFValueFromSTUserFunc => &
    obj_GetVertexDOFValueFromSTFunc
  !! Evaluate a user function at cell vertices.
  GENERIC, PUBLIC :: GetVertexDOFValue => GetVertexDOFValueFromSTUserFunc
  !! Get the vertex dof values

  PROCEDURE, PUBLIC, PASS(obj) :: GetInCellDOFValueFromSTUserFunc => &
    obj_GetInCellDOFValueFromSTFunc
  !! Project the residual onto interior cell degrees of freedom.
  GENERIC, PUBLIC :: GetInCellDOFValue => GetInCellDOFValueFromSTUserFunc
  !! Project the interior residual after the boundary coefficients.

  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromSTFunc => &
    obj_GetDOFValueFromSTFunc
  !! Compute vertex, facet and interior cell coefficients.
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromSTFunc2 => &
    obj_GetDOFValueFromSTFunc2
  !! Prepare quadrature and shape data before DOF projection.
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromQuadrature => &
    obj_GetDOFValueFromQuadrature
  !! Project sampled values onto a cell basis.
  GENERIC, PUBLIC :: GetDOFValue => &
    GetDOFValueFromSTFunc, &
    GetDOFValueFromSTFunc2, &
    GetDOFValueFromQuadrature
  !! Get the degree of freedom values
END TYPE AbstractFE_

!----------------------------------------------------------------------------
!                                                         AbstractFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Polymorphic finite element pointer wrapper
!
! Stores a nullable pointer to a concrete extension of AbstractFE_. The
! wrapper does not establish ownership. Vector cleanup resets and detaches
! targets without freeing them.
!
! See also: [[AbstractFEDeallocate]]

TYPE :: AbstractFEPointer_
  CLASS(AbstractFE_), POINTER :: ptr => NULL()
  !! Finite element target; initially disassociated.
END TYPE AbstractFEPointer_

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-27
! summary: Initialize finite element basis options
!
! Sets isInit to true and forwards all basis, order, orientation and
! quadrature arguments to opt%Initiate. This initializes the shared
! configuration; element-specific operations are provided by the concrete
! child class.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: other
! INTEGER(I4B) :: order, nsd
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%isInit, "Initiate sets the initialization flag")
! CALL obj%GetParam(order=order, nsd=nsd)
! CALL OK(order == 1_I4B .AND. nsd == 2_I4B, "Read initial options")
! CALL other%Copy(obj)
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%isInit, "Deallocate resets initialization")
! CALL OK(other%isInit, "Copy has independent initialization state")
! obj = other
! CALL OK(obj%isInit, "Defined assignment copies shared state")
! CALL obj%SetParam(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "SetParam updates stored options")
! CALL other%GetParam(order=order)
! CALL OK(order == 1_I4B, "Copy retains its own options")
! CALL obj%DEALLOCATE()
! CALL other%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, elemType, nsd, baseContinuity, baseInterpolation, feType, ipType, &
    basisType, alpha, beta, lambda, dofType, transformType, order, &
    anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, faceOrient, &
    edgeOrient, tcell, tface, tedge, errCheck, quadratureIsHomogeneous, &
    quadratureType, quadratureOrder, quadratureIsOrder, quadratureNips, &
    quadratureIsNips, quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of physical spatial dimensions.
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Basis continuity or conformity, such as H1, HDiv, HCurl or DG.
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis family name, such as Lagrange or Hierarchy.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type code, or directional codes for point
    !! generation.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Polynomial basis type codes by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi alpha parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi beta parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element value type code: scalar, vector or matrix.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! DOF type codes for vertex, edge, face and cell entities; four entries.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! Reference-to-physical transformation type code.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Anisotropic approximation orders by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Cell approximation orders.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Face approximation orders; directions are rows and faces are columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Approximation orders on the edges.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! Cell orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! Face orientation data, one face per column.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! Edge orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! Active size of cell order or orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! Active number of face columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional control of validation performed by BasisOpt_ in DEBUG mode.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! Whether the same quadrature settings apply in all directions.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureOrder(:)
    !! Quadrature accuracy orders by reference direction.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Whether quadrature is specified by accuracy order.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! Quadrature point counts by reference direction.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Whether quadrature is specified by point count.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Quadrature Jacobi alpha parameters by direction.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    !! Quadrature Jacobi beta parameters by direction.
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    !! Quadrature ultraspherical parameters by direction.
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Copy@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary: Copy initialization state and basis options
!
! Copies isInit and calls opt%Copy on the source options. The assignment
! binding uses this same procedure. No additional components introduced by a
! child class are copied here.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: other
! INTEGER(I4B) :: order, nsd
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%isInit, "Initiate sets the initialization flag")
! CALL obj%GetParam(order=order, nsd=nsd)
! CALL OK(order == 1_I4B .AND. nsd == 2_I4B, "Read initial options")
! CALL other%Copy(obj)
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%isInit, "Deallocate resets initialization")
! CALL OK(other%isInit, "Copy has independent initialization state")
! obj = other
! CALL OK(obj%isInit, "Defined assignment copies shared state")
! CALL obj%SetParam(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "SetParam updates stored options")
! CALL other%GetParam(order=order)
! CALL OK(order == 1_I4B, "Copy retains its own options")
! CALL obj%DEALLOCATE()
! CALL other%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    CLASS(AbstractFE_), INTENT(IN) :: obj2
    !! Source finite element whose shared state is copied.
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-29
! summary: Reset initialization state and release basis options
!
! Sets isInit to false and calls opt%DEALLOCATE. The object itself remains
! allocated. This procedure is also exported through AbstractFEDeallocate.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: other
! INTEGER(I4B) :: order, nsd
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%isInit, "Initiate sets the initialization flag")
! CALL obj%GetParam(order=order, nsd=nsd)
! CALL OK(order == 1_I4B .AND. nsd == 2_I4B, "Read initial options")
! CALL other%Copy(obj)
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%isInit, "Deallocate resets initialization")
! CALL OK(other%isInit, "Copy has independent initialization state")
! obj = other
! CALL OK(obj%isInit, "Defined assignment copies shared state")
! CALL obj%SetParam(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "SetParam updates stored options")
! CALL other%GetParam(order=order)
! CALL OK(order == 1_I4B, "Copy retains its own options")
! CALL obj%DEALLOCATE()
! CALL other%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]]

INTERFACE AbstractFEDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractFEDeallocate

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-09
! summary: Reset and detach a vector of finite element pointers
!
! For each associated pointer, calls its DEALLOCATE binding and nullifies the
! pointer. Finally deallocates the wrapper array. Pointer targets themselves
! are not deallocated; retain a separate owning reference when their
! allocation must be released. An unallocated wrapper array is accepted.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE AbstractFE_Class, ONLY: AbstractFEPointer_
! USE AbstractFE_Class, ONLY: AbstractFEDeallocate
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(AbstractFEPointer_), ALLOCATABLE :: refs(:)
! TYPE(TriangleH1LagrangeFE_), TARGET :: target
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL target%Copy(obj)
! ALLOCATE(refs(2))
! refs(1)%ptr => target
! CALL AbstractFEDeallocate(refs)
! CALL OK(.NOT. ALLOCATED(refs), "Wrapper vector released")
! CALL OK(.NOT. target%isInit, "Target state reset and still accessible")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE AbstractFEDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(AbstractFEPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Allocatable vector of pointer wrappers; targets are reset and detached.
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE AbstractFEDeallocate

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Reset and detach a vector of finite element pointer wrappers
!
! Alias of the pointer-vector overload of AbstractFEDeallocate. It resets
! each associated target, nullifies the wrappers and releases the wrapper
! array. Target allocations remain the responsibility of their owner.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE AbstractFE_Class, ONLY: AbstractFEPointer_
! USE AbstractFE_Class, ONLY: AbstractFEDeallocate
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(AbstractFEPointer_), ALLOCATABLE :: refs(:)
! TYPE(TriangleH1LagrangeFE_), TARGET :: target
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL target%Copy(obj)
! ALLOCATE(refs(2))
! refs(1)%ptr => target
! CALL AbstractFEDeallocate(refs)
! CALL OK(.NOT. ALLOCATED(refs), "Wrapper vector released")
! CALL OK(.NOT. target%isInit, "Target state reset and still accessible")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractFEDeallocate]]

INTERFACE DEALLOCATE
  MODULE PROCEDURE Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                             Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-14
! summary: Display initialization state and basis options
!
! Writes msg and isInit to the selected output unit. If initialized, also
! calls opt%Display. The notFull argument is currently unused. This procedure
! is also exported through AbstractFEDisplay.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE String_Class, ONLY: String
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(String) :: markdown, react
! INTEGER, PARAMETER :: unitno = 20
! INTEGER :: ios
! CHARACTER(256) :: line, iomsg
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! OPEN(UNIT=unitno, STATUS="scratch", ACTION="readwrite")
! CALL obj%Display(msg="Finite element", unitno=unitno)
! REWIND(unitno)
! READ(unitno, '(A)', IOSTAT=ios, IOMSG=iomsg) line
! CALL OK(ios == 0, "Display writes output")
! CLOSE(unitno)
! markdown = obj%MdEncode()
! react = obj%ReactEncode()
! CALL OK(LEN(markdown%chars()) > 0, "Markdown encoding")
! CALL OK(LEN(react%chars()) > 0, "React encoding")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE AbstractFEDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno, notFull)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    CHARACTER(*), INTENT(IN) :: msg
    !! Message printed before the object state.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! Optional output unit passed to Display.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull
    !! Accepted for compatibility; unused in the base implementation.
  END SUBROUTINE obj_Display
END INTERFACE AbstractFEDisplay

!----------------------------------------------------------------------------
!                                                            MdEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-20
! summary: Encode basis options as Markdown
!
! Returns opt%MdEncode(). The initialization flag is not included by this
! wrapper.
!
!
! Returns:
!   ans : TYPE(String) - Basis options encoded as Markdown.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE String_Class, ONLY: String
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(String) :: markdown, react
! INTEGER, PARAMETER :: unitno = 20
! INTEGER :: ios
! CHARACTER(256) :: line, iomsg
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! OPEN(UNIT=unitno, STATUS="scratch", ACTION="readwrite")
! CALL obj%Display(msg="Finite element", unitno=unitno)
! REWIND(unitno)
! READ(unitno, '(A)', IOSTAT=ios, IOMSG=iomsg) line
! CALL OK(ios == 0, "Display writes output")
! CLOSE(unitno)
! markdown = obj%MdEncode()
! react = obj%ReactEncode()
! CALL OK(LEN(markdown%chars()) > 0, "Markdown encoding")
! CALL OK(LEN(react%chars()) > 0, "React encoding")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE FUNCTION obj_MdEncode(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(String) :: ans
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
  END FUNCTION obj_MdEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                        ReactEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-20
! summary: Encode basis options as React markup
!
! Returns opt%ReactEncode(). The initialization flag is not included by this
! wrapper.
!
!
! Returns:
!   ans : TYPE(String) - Basis options encoded as React markup.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE String_Class, ONLY: String
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(String) :: markdown, react
! INTEGER, PARAMETER :: unitno = 20
! INTEGER :: ios
! CHARACTER(256) :: line, iomsg
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! OPEN(UNIT=unitno, STATUS="scratch", ACTION="readwrite")
! CALL obj%Display(msg="Finite element", unitno=unitno)
! REWIND(unitno)
! READ(unitno, '(A)', IOSTAT=ios, IOMSG=iomsg) line
! CALL OK(ios == 0, "Display writes output")
! CLOSE(unitno)
! markdown = obj%MdEncode()
! react = obj%ReactEncode()
! CALL OK(LEN(markdown%chars()) > 0, "Markdown encoding")
! CALL OK(LEN(react%chars()) > 0, "React encoding")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE FUNCTION obj_ReactEncode(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(String) :: ans
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
  END FUNCTION obj_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-27
! summary: Update selected stored basis parameters
!
! Forwards the supplied optional arguments to opt%SetParam. It does not set
! isInit and does not dispatch to the child SetOrder or quadrature setters.
! Use the child setters when element-specific cached data must be updated.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: other
! INTEGER(I4B) :: order, nsd
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%isInit, "Initiate sets the initialization flag")
! CALL obj%GetParam(order=order, nsd=nsd)
! CALL OK(order == 1_I4B .AND. nsd == 2_I4B, "Read initial options")
! CALL other%Copy(obj)
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%isInit, "Deallocate resets initialization")
! CALL OK(other%isInit, "Copy has independent initialization state")
! obj = other
! CALL OK(obj%isInit, "Defined assignment copies shared state")
! CALL obj%SetParam(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "SetParam updates stored options")
! CALL other%GetParam(order=order)
! CALL OK(order == 1_I4B, "Copy retains its own options")
! CALL obj%DEALLOCATE()
! CALL other%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, nsd, order, anisoOrder, edgeOrder, faceOrder, cellOrder, fetype, &
    elemType, topoType, elemIndx, ipType, basisType, alpha, beta, lambda, &
    dofType, transformType, refElemDomain, baseContinuity, &
    baseInterpolation, isIsotropicOrder, isAnisotropicOrder, isEdgeOrder, &
    isFaceOrder, isCellOrder, tEdgeOrder, tFaceOrder, tCellOrder, &
    quadratureIsHomogeneous, quadratureType, quadratureType1, &
    quadratureType2, quadratureType3, quadratureOrder, quadratureOrder1, &
    quadratureOrder2, quadratureOrder3, quadratureIsOrder, quadratureNips, &
    quadratureNips1, quadratureNips2, quadratureNips3, quadratureIsNips, &
    quadratureAlpha, quadratureAlpha1, quadratureAlpha2, quadratureAlpha3, &
    quadratureBeta, quadratureBeta1, quadratureBeta2, quadratureBeta3, &
    quadratureLambda, quadratureLambda1, quadratureLambda2, quadratureLambda3)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of physical spatial dimensions.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Anisotropic approximation orders by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Approximation orders on the edges.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Face approximation orders; directions are rows and faces are columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Cell approximation orders.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element value type code: scalar, vector or matrix.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
    !! Reference element topology code.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemIndx
    !! Reference element index.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type code, or directional codes for point
    !! generation.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Polynomial basis type codes by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi alpha parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi beta parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! DOF type codes for vertex, edge, face and cell entities; four entries.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! Reference-to-physical transformation type code.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    !! Basis continuity or conformity, such as H1, HDiv, HCurl or DG.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
    !! Basis family name, such as Lagrange or Hierarchy.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refElemDomain
    !! Reference domain name.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isIsotropicOrder
    !! Whether isotropic approximation order is active.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isAnisotropicOrder
    !! Whether anisotropic approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isEdgeOrder
    !! Whether edge-specific approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFaceOrder
    !! Whether face-specific approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isCellOrder
    !! Whether cell-specific approximation orders are active.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdgeOrder
    !! Number of active edge orders.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFaceOrder
    !! Number of active face-order columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCellOrder
    !! Number of active cell orders.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! Whether the same quadrature settings apply in all directions.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    !! Quadrature rule type codes by reference direction in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    !! Quadrature rule type codes by reference direction in direction 2.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
    !! Quadrature rule type codes by reference direction in direction 3.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! Quadrature accuracy orders by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder1
    !! Quadrature accuracy orders by reference direction in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder2
    !! Quadrature accuracy orders by reference direction in direction 2.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder3
    !! Quadrature accuracy orders by reference direction in direction 3.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Whether quadrature is specified by accuracy order.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(:)
    !! Quadrature point counts by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips1
    !! Quadrature point counts by reference direction in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips2
    !! Quadrature point counts by reference direction in direction 2.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips3
    !! Quadrature point counts by reference direction in direction 3.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Whether quadrature is specified by point count.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha(:)
    !! Quadrature Jacobi alpha parameters by direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha1
    !! Quadrature Jacobi alpha parameters by direction in direction 1.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha2
    !! Quadrature Jacobi alpha parameters by direction in direction 2.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha3
    !! Quadrature Jacobi alpha parameters by direction in direction 3.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta(:)
    !! Quadrature Jacobi beta parameters by direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta1
    !! Quadrature Jacobi beta parameters by direction in direction 1.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta2
    !! Quadrature Jacobi beta parameters by direction in direction 2.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta3
    !! Quadrature Jacobi beta parameters by direction in direction 3.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda(:)
    !! Quadrature ultraspherical parameters by direction.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda1
    !! Quadrature ultraspherical parameters by direction in direction 1.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda2
    !! Quadrature ultraspherical parameters by direction in direction 2.
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda3
    !! Quadrature ultraspherical parameters by direction in direction 3.
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetOrder@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Set approximation orders through a child implementation
!
! Update approximation orders and any element-specific cached data. The base
! implementation is a placeholder and reports an error in DEBUG mode. Use a
! concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! REAL(DFP) :: xij(2, 3), points(3, 6)
! INTEGER(I4B) :: nrow, ncol, count, order
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%SetOrder(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "Child order setter")
! count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(count == 6_I4B, "Quadratic triangle point count")
! CALL obj%GetInterpolationPoints(xij=xij, ans=points, &
!   nrow=nrow, ncol=ncol, order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(ncol == count, "Interpolation output size")
! CALL OK(ALL(points(1:2, 1:ncol) >= -1.0E-12_DFP), "Points in triangle")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_SetOrder( &
    obj, order, anisoorder, cellOrder, faceOrder, edgeOrder, tcell, tface, &
    tedge, errCheck)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoorder(:)
    !! Anisotropic approximation orders by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Cell approximation orders.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Face approximation orders; directions are rows and faces are columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Approximation orders on the edges.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! Active size of cell order or orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! Active number of face columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional control of validation performed by BasisOpt_ in DEBUG mode.
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetOrder@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Set cell, face and edge orientations in a child class
!
! Apply the orientation arrays and their active sizes to the finite element.
! The base implementation is a placeholder and reports an error in DEBUG mode.
! Use a concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! INTEGER(I4B) :: nptrs(10), faceOrient(3, 3), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,10)]
! faceOrient = 1_I4B
! faceOrient(1, 1) = -1_I4B
! CALL obj%SetOrientation(faceOrient=faceOrient, tface=3_I4B)
! CALL obj%FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
! CALL OK(ALL(nptrs(1:3) == [1_I4B, 2_I4B, 3_I4B]), "Vertices retained")
! CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), "First edge DOFs reversed")
! CALL OK(ALL(nptrs(6:10) == [6_I4B, 7_I4B, 8_I4B, 9_I4B, 10_I4B]), &
!   "Other DOFs retained")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_SetOrientation( &
    obj, cellOrient, faceOrient, edgeOrient, tcell, tface, tedge, errCheck)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! Cell orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! Face orientation data, one face per column.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! Edge orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! Active size of cell order or orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! Active number of face columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional control of validation performed by BasisOpt_ in DEBUG mode.
  END SUBROUTINE obj_SetOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetRefElemCoord@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Copy reference element vertex coordinates
!
! Delegates to opt%GetRefElemCoord. Coordinates occupy ans(1:nrow,1:ncol);
! nrow is three, including unused coordinate directions, and ncol is the
! number of reference vertices.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! REAL(DFP) :: coords(3, 3)
! INTEGER(I4B) :: nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(LEN_TRIM(obj%GetBaseInterpolation()) > 0, "Interpolation code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference vertex layout")
! CALL OK(ALL(ABS(coords(3, :)) < 1.0E-12_DFP), "Planar reference vertices")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetRefElemCoord(obj, ans, nrow, ncol)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Coordinate output buffer; active block is ans(1:nrow,1:ncol).
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! Number of active coordinate rows written to ans.
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! Number of active coordinate columns written to ans.
  END SUBROUTINE obj_GetRefElemCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-27
! summary: Read selected stored basis parameters
!
! Forwards optional output arguments to opt%GetParam. Only requested values
! are copied. Caller-owned output arrays must accommodate the active order
! data.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: other
! INTEGER(I4B) :: order, nsd
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%isInit, "Initiate sets the initialization flag")
! CALL obj%GetParam(order=order, nsd=nsd)
! CALL OK(order == 1_I4B .AND. nsd == 2_I4B, "Read initial options")
! CALL other%Copy(obj)
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%isInit, "Deallocate resets initialization")
! CALL OK(other%isInit, "Copy has independent initialization state")
! obj = other
! CALL OK(obj%isInit, "Defined assignment copies shared state")
! CALL obj%SetParam(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "SetParam updates stored options")
! CALL other%GetParam(order=order)
! CALL OK(order == 1_I4B, "Copy retains its own options")
! CALL obj%DEALLOCATE()
! CALL other%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, nsd, order, anisoOrder, edgeOrder, faceOrder, cellOrder, fetype, &
    elemType, topoType, elemIndx, ipType, basisType, alpha, beta, lambda, &
    dofType, transformType, refElemDomain, baseContinuity, &
    baseInterpolation, isIsotropicOrder, isAnisotropicOrder, isEdgeOrder, &
    isFaceOrder, isCellOrder, tEdgeOrder, tFaceOrder, tCellOrder, &
    quadratureIsHomogeneous, quadratureType, quadratureOrder, &
    quadratureNips, quadratureIsOrder, quadratureIsNips, quadratureAlpha, &
    quadratureBeta, quadratureLambda)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
    !! Number of physical spatial dimensions.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: anisoOrder(3)
    !! Anisotropic approximation orders by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: edgeOrder(:)
    !! Approximation orders on the edges.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: faceOrder(:, :)
    !! Face approximation orders; directions are rows and faces are columns.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: cellOrder(:)
    !! Cell approximation orders.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: fetype
    !! Finite element value type code: scalar, vector or matrix.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: topoType
    !! Reference element topology code.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemIndx
    !! Reference element index.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ipType
    !! Interpolation point type code, or directional codes for point
    !! generation.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: basisType(3)
    !! Polynomial basis type codes by reference direction.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha(3)
    !! Jacobi alpha parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta(3)
    !! Jacobi beta parameters by reference direction.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda(3)
    !! Ultraspherical parameters by reference direction.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: dofType(4)
    !! DOF type codes for vertex, edge, face and cell entities; four entries.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: transformType
    !! Reference-to-physical transformation type code.
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseContinuity
    !! Basis continuity or conformity, such as H1, HDiv, HCurl or DG.
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseInterpolation
    !! Basis family name, such as Lagrange or Hierarchy.
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: refElemDomain
    !! Reference domain name.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isIsotropicOrder
    !! Whether isotropic approximation order is active.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isAnisotropicOrder
    !! Whether anisotropic approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isEdgeOrder
    !! Whether edge-specific approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isFaceOrder
    !! Whether face-specific approximation orders are active.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isCellOrder
    !! Whether cell-specific approximation orders are active.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tEdgeOrder
    !! Number of active edge orders.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tFaceOrder
    !! Number of active face-order columns.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tCellOrder
    !! Number of active cell orders.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsHomogeneous
    !! Whether the same quadrature settings apply in all directions.
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureType(3)
    !! Quadrature rule type codes by reference direction.
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureOrder(3)
    !! Quadrature accuracy orders by reference direction.
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureNips(3)
    !! Quadrature point counts by reference direction.
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureAlpha(3)
    !! Quadrature Jacobi alpha parameters by direction.
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureBeta(3)
    !! Quadrature Jacobi beta parameters by direction.
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureLambda(3)
    !! Quadrature ultraspherical parameters by direction.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsOrder
    !! Whether quadrature is specified by accuracy order.
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsNips
    !! Whether quadrature is specified by point count.
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetLocalElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Evaluate reference cell shape data in a child class
!
! Evaluate local basis values and reference derivatives at quad. The base
! implementation is a placeholder and reports an error in DEBUG mode. Use a
! concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes
! TYPE(QuadraturePoint_) :: quad
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad=quad)
! count = obj%GetTotalQuadraturePoints()
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData(elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nips == count, "Configured quadrature count")
! CALL OK(shapes%nns == 3_I4B, "Linear triangle basis size")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij)
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:count), DIM=1) - &
!   1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij, quad=quad)
! CALL OK(shapes%nips == count, "High-level shape preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetLocalFacetElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Evaluate reference facet shape data in a child class
!
! Evaluate cell traces and facet basis data at the supplied cell and facet
! quadrature points. The base implementation is a placeholder and reports an
! error in DEBUG mode. Use a concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes(3), facets(3), geoShapes(3), geoFacets(3)
! TYPE(QuadraturePoint_) :: quad(3), facetQuad(3)
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad(1), facetQuad=facetQuad(1), localFaceNumber=1_I4B)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(tsize == 3_I4B, "Triangle has three facets")
! CALL obj%GetLocalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   localFaceNumber=1_I4B)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL geometry%GetAllLocalFacetElemShapeData(elemsd=geoShapes, &
!   facetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(ALL(facets%nns == 2_I4B), "Linear facet basis sizes")
! CALL obj%GetGlobalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), xij=xij, localFaceNumber=1_I4B)
! ! Restore local data before mapping every face.
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, tsize=tsize)
! CALL OK(tsize == 3_I4B, "All facets mapped")
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes(1), facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets(1)%nips > 0_I4B, "High-level facet preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Facet quadrature, or one quadrature container per facet.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE obj_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetAllLocalFacetElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Evaluate local shape data for every facet
!
! Sets tsize to opt%GetTotalFace and calls GetLocalFacetElemShapeData for each
! local face. Each array must have at least tsize entries. Quadrature arrays
! must already contain valid rules.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes(3), facets(3), geoShapes(3), geoFacets(3)
! TYPE(QuadraturePoint_) :: quad(3), facetQuad(3)
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad(1), facetQuad=facetQuad(1), localFaceNumber=1_I4B)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(tsize == 3_I4B, "Triangle has three facets")
! CALL obj%GetLocalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   localFaceNumber=1_I4B)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL geometry%GetAllLocalFacetElemShapeData(elemsd=geoShapes, &
!   facetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(ALL(facets%nns == 2_I4B), "Linear facet basis sizes")
! CALL obj%GetGlobalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), xij=xij, localFaceNumber=1_I4B)
! ! Restore local data before mapping every face.
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, tsize=tsize)
! CALL OK(tsize == 3_I4B, "All facets mapped")
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes(1), facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets(1)%nips > 0_I4B, "High-level facet preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetAllLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, tsize)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd(:)
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd(:)
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad(:)
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad(:)
    !! Facet quadrature, or one quadrature container per facet.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of facet entries populated in the arrays.
  END SUBROUTINE obj_GetAllLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetGlobalElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Map cell shape data in a child class
!
! Transform prepared local shape data to physical coordinates using xij and
! the required geometry shape data geoelemsd. The base implementation is a
! placeholder and reports an error in DEBUG mode. Use a concrete child that
! overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes
! TYPE(QuadraturePoint_) :: quad
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad=quad)
! count = obj%GetTotalQuadraturePoints()
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData(elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nips == count, "Configured quadrature count")
! CALL OK(shapes%nns == 3_I4B, "Linear triangle basis size")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij)
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:count), DIM=1) - &
!   1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij, quad=quad)
! CALL OK(shapes%nips == count, "High-level shape preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoelemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required geometry cell shape data, or geometry traces by facet.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetGlobalElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Prepare and map cell shape data
!
! Unless doNotInitQuad is true (default false), obtains quadrature from obj.
! Evaluates local solution and geometry shape data at the same points, then
! dispatches to obj%GetGlobalElemShapeData. Both finite elements must support
! those operations.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes
! TYPE(QuadraturePoint_) :: quad
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad=quad)
! count = obj%GetTotalQuadraturePoints()
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData(elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nips == count, "Configured quadrature count")
! CALL OK(shapes%nns == 3_I4B, "Linear triangle basis size")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij)
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:count), DIM=1) - &
!   1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij, quad=quad)
! CALL OK(shapes%nips == count, "High-level shape preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData2( &
    obj, geofeptr, elemsd, geoelemsd, xij, quad, doNotInitQuad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    CLASS(AbstractFE_), INTENT(INOUT) :: geofeptr
    !! Concrete finite element describing the geometry basis.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! Required geometry cell shape data, or geometry traces by facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: doNotInitQuad
    !! If true, reuse quad; default false initializes quadrature.
  END SUBROUTINE obj_GetGlobalElemShapeData2
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetGlobalFacetElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Map facet shape data in a child class
!
! Transform prepared cell traces and facet shapes using physical coordinates
! and the required geometry datasets. The base implementation is a placeholder
! and reports an error in DEBUG mode. Use a concrete child that overrides this
! binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes(3), facets(3), geoShapes(3), geoFacets(3)
! TYPE(QuadraturePoint_) :: quad(3), facetQuad(3)
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad(1), facetQuad=facetQuad(1), localFaceNumber=1_I4B)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(tsize == 3_I4B, "Triangle has three facets")
! CALL obj%GetLocalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   localFaceNumber=1_I4B)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL geometry%GetAllLocalFacetElemShapeData(elemsd=geoShapes, &
!   facetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(ALL(facets%nns == 2_I4B), "Linear facet basis sizes")
! CALL obj%GetGlobalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), xij=xij, localFaceNumber=1_I4B)
! ! Restore local data before mapping every face.
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, tsize=tsize)
! CALL OK(tsize == 3_I4B, "All facets mapped")
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes(1), facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets(1)%nips > 0_I4B, "High-level facet preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd
    !! Required geometry cell shape data, or geometry traces by facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoFacetElemsd
    !! Required geometry shape data on the facet, or datasets by facet.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE obj_GetGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetGlobalFacetElemShapeData2@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Prepare and map shape data on one facet
!
! Obtains cell and facet quadrature from obj, evaluates local solution and
! geometry shape data, then dispatches to obj%GetGlobalFacetElemShapeData for
! the selected local face.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes(3), facets(3), geoShapes(3), geoFacets(3)
! TYPE(QuadraturePoint_) :: quad(3), facetQuad(3)
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad(1), facetQuad=facetQuad(1), localFaceNumber=1_I4B)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(tsize == 3_I4B, "Triangle has three facets")
! CALL obj%GetLocalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   localFaceNumber=1_I4B)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL geometry%GetAllLocalFacetElemShapeData(elemsd=geoShapes, &
!   facetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(ALL(facets%nns == 2_I4B), "Linear facet basis sizes")
! CALL obj%GetGlobalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), xij=xij, localFaceNumber=1_I4B)
! ! Restore local data before mapping every face.
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, tsize=tsize)
! CALL OK(tsize == 3_I4B, "All facets mapped")
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes(1), facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets(1)%nips > 0_I4B, "High-level facet preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalFacetElemShapeData2( &
    obj, geofeptr, elemsd, facetElemsd, geoElemsd, geoFacetElemsd, &
    localFaceNumber, quad, facetQuad, xij)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    CLASS(AbstractFE_), INTENT(INOUT) :: geofeptr
    !! Concrete finite element describing the geometry basis.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd
    !! Required geometry cell shape data, or geometry traces by facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoFacetElemsd
    !! Required geometry shape data on the facet, or datasets by facet.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Facet quadrature, or one quadrature container per facet.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
  END SUBROUTINE obj_GetGlobalFacetElemShapeData2
END INTERFACE

!----------------------------------------------------------------------------
!                                     GetAllGlobalFacetElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-15
! summary: Map prepared shape data for every facet
!
! Sets tsize to opt%GetTotalFace and calls GetGlobalFacetElemShapeData for
! each face. Solution and geometry arrays must contain prepared local shape
! data and have at least tsize entries.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes(3), facets(3), geoShapes(3), geoFacets(3)
! TYPE(QuadraturePoint_) :: quad(3), facetQuad(3)
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad(1), facetQuad=facetQuad(1), localFaceNumber=1_I4B)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(tsize == 3_I4B, "Triangle has three facets")
! CALL obj%GetLocalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   localFaceNumber=1_I4B)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL geometry%GetAllLocalFacetElemShapeData(elemsd=geoShapes, &
!   facetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(ALL(facets%nns == 2_I4B), "Linear facet basis sizes")
! CALL obj%GetGlobalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), xij=xij, localFaceNumber=1_I4B)
! ! Restore local data before mapping every face.
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, tsize=tsize)
! CALL OK(tsize == 3_I4B, "All facets mapped")
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes(1), facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets(1)%nips > 0_I4B, "High-level facet preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetAllGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, tsize, geoElemsd, geoFacetElemsd, xij)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd(:)
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd(:)
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd(:)
    !! Required geometry cell shape data, or geometry traces by facet.
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoFacetElemsd(:)
    !! Required geometry shape data on the facet, or datasets by facet.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of facet entries populated in the arrays.
  END SUBROUTINE obj_GetAllGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Construct cell quadrature in a child class
!
! Populate quad using the configured cell quadrature options. The base
! implementation is a placeholder and reports an error in DEBUG mode. Use a
! concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes
! TYPE(QuadraturePoint_) :: quad
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad=quad)
! count = obj%GetTotalQuadraturePoints()
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData(elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nips == count, "Configured quadrature count")
! CALL OK(shapes%nns == 3_I4B, "Linear triangle basis size")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij)
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:count), DIM=1) - &
!   1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij, quad=quad)
! CALL OK(shapes%nips == count, "High-level shape preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetFacetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Construct facet quadrature in a child class
!
! Populate quad with cell reference coordinates on the selected face and
! facetQuad with the corresponding facet coordinates. The base implementation
! is a placeholder and reports an error in DEBUG mode. Use a concrete child
! that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes(3), facets(3), geoShapes(3), geoFacets(3)
! TYPE(QuadraturePoint_) :: quad(3), facetQuad(3)
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad(1), facetQuad=facetQuad(1), localFaceNumber=1_I4B)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(tsize == 3_I4B, "Triangle has three facets")
! CALL obj%GetLocalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   localFaceNumber=1_I4B)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL geometry%GetAllLocalFacetElemShapeData(elemsd=geoShapes, &
!   facetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(ALL(facets%nns == 2_I4B), "Linear facet basis sizes")
! CALL obj%GetGlobalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), xij=xij, localFaceNumber=1_I4B)
! ! Restore local data before mapping every face.
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, tsize=tsize)
! CALL OK(tsize == 3_I4B, "All facets mapped")
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes(1), facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets(1)%nips > 0_I4B, "High-level facet preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints( &
    obj, quad, facetQuad, localFaceNumber)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Facet quadrature, or one quadrature container per facet.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
  END SUBROUTINE obj_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetAllFacetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Obtain quadrature rules for every facet
!
! Sets tsize to opt%GetTotalFace and calls GetFacetQuadraturePoints for each
! face. Both caller-owned arrays must have at least tsize entries.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes(3), facets(3), geoShapes(3), geoFacets(3)
! TYPE(QuadraturePoint_) :: quad(3), facetQuad(3)
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetFacetQuadraturePoints( &
!   quad=quad(1), facetQuad=facetQuad(1), localFaceNumber=1_I4B)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(tsize == 3_I4B, "Triangle has three facets")
! CALL obj%GetLocalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   localFaceNumber=1_I4B)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL geometry%GetAllLocalFacetElemShapeData(elemsd=geoShapes, &
!   facetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL OK(ALL(facets%nns == 2_I4B), "Linear facet basis sizes")
! CALL obj%GetGlobalFacetElemShapeData(elemsd=shapes(1), &
!   facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), xij=xij, localFaceNumber=1_I4B)
! ! Restore local data before mapping every face.
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, quad=quad, facetQuad=facetQuad, tsize=tsize)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, &
!   facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, xij=xij, tsize=tsize)
! CALL OK(tsize == 3_I4B, "All facets mapped")
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes(1), facetElemsd=facets(1), geoElemsd=geoShapes(1), &
!   geoFacetElemsd=geoFacets(1), quad=quad(1), facetQuad=facetQuad(1), &
!   xij=xij, localFaceNumber=1_I4B)
! CALL OK(facets(1)%nips > 0_I4B, "High-level facet preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetAllFacetQuadraturePoints( &
    obj, quad, facetQuad, tsize)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad(:)
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad(:)
    !! Facet quadrature, or one quadrature container per facet.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of facet entries populated in the arrays.
  END SUBROUTINE obj_GetAllFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetTotalQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Return the configured cell quadrature point count
!
! Delegates to opt%GetTotalQuadraturePoints. This queries the stored
! quadrature options rather than constructing a quadrature rule.
!
!
! Returns:
!   ans : INTEGER(I4B) - Number of configured cell quadrature points.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes
! TYPE(QuadraturePoint_) :: quad
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: count
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetQuadraturePoints(quad=quad)
! count = obj%GetTotalQuadraturePoints()
! CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
! CALL geometry%GetLocalElemShapeData(elemsd=geoShapes, quad=quad)
! CALL OK(shapes%nips == count, "Configured quadrature count")
! CALL OK(shapes%nns == 3_I4B, "Linear triangle basis size")
! CALL obj%GetGlobalElemShapeData( &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij)
! CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:count), DIM=1) - &
!   1.0_DFP) < 1.0E-10_DFP), "Partition of unity")
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, geoelemsd=geoShapes, xij=xij, quad=quad)
! CALL OK(shapes%nips == count, "High-level shape preparation")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE FUNCTION obj_GetTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B) :: ans
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
  END FUNCTION obj_GetTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetQuadratureOrder@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set quadrature accuracy in a child class
!
! Update quadrature accuracy using an order array or directional scalar
! arguments. Directional precedence is defined by the child implementation.
! The base implementation is a placeholder and reports an error in DEBUG mode.
! Use a concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! INTEGER(I4B) :: orders(3), types(3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL obj%SetQuadratureOrder(order1=6_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 6_I4B, "Quadrature accuracy updated")
! CALL obj%SetQuadratureType(quadratureType1=types(1))
! CALL obj%GetParam(quadratureType=orders)
! CALL OK(orders(1) == types(1), "Quadrature type retained")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureOrder(obj, order, order1, order2, order3)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction in direction 1.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction in direction 2.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order3
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction in direction 3.
  END SUBROUTINE obj_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuadratureType@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set quadrature rules in a child class
!
! Update quadrature type using an array or directional scalar arguments.
! Supported rules depend on the element topology. The base implementation is a
! placeholder and reports an error in DEBUG mode. Use a concrete child that
! overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! INTEGER(I4B) :: orders(3), types(3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL obj%SetQuadratureOrder(order1=6_I4B)
! CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
! CALL OK(orders(1) == 6_I4B, "Quadrature accuracy updated")
! CALL obj%SetQuadratureType(quadratureType1=types(1))
! CALL obj%GetParam(quadratureType=orders)
! CALL OK(orders(1) == types(1), "Quadrature type retained")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureType( &
    obj, quadratureType, quadratureType1, quadratureType2, quadratureType3)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
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
!                                                     GetTopologyType@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Return the reference topology code
!
! Returns opt%GetTopologyType().
!
!
! Returns:
!   ans : INTEGER(I4B) - Reference topology code.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! REAL(DFP) :: coords(3, 3)
! INTEGER(I4B) :: nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(LEN_TRIM(obj%GetBaseInterpolation()) > 0, "Interpolation code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference vertex layout")
! CALL OK(ALL(ABS(coords(3, :)) < 1.0E-12_DFP), "Planar reference vertices")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE FUNCTION obj_GetTopologyType(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B) :: ans
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
  END FUNCTION obj_GetTopologyType
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary: Initialize basis options from a TOML table
!
! Sets isInit to true and forwards table and optional element type and spatial
! dimension overrides to opt%ImportFromToml. This overload is selected by the
! ImportFromToml generic when table is supplied.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE tomlf, ONLY: toml_table
! USE tomlf, ONLY: set_value
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(toml_table) :: table
! INTEGER(I4B) :: nsd
!
! CALL set_value(table, "baseContinuity", "H1")
! CALL set_value(table, "baseInterpolation", "Lagrange")
! CALL obj%ImportFromToml(table=table, elemType=TypeElemNameOpt%Triangle, &
!   nsd=2_I4B)
! CALL obj%GetParam(nsd=nsd)
! CALL OK(obj%isInit .AND. nsd == 2_I4B, "TOML table initialization")
! CALL OK(obj%GetBaseContinuity() == "H1", "TOML continuity")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, elemType, nsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(toml_table), INTENT(INOUT) :: table
    !! TOML table containing basis options.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of physical spatial dimensions.
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-08
! summary: Initialize basis options from a named TOML table
!
! Reads a TOML document using GetValue, selects the table named tomlName and
! dispatches to the table overload. Provide a filename or an appropriate
! TxtFile_ handle. A missing table is reported in DEBUG mode. printToml is
! currently unused.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! INTEGER :: unitno
! INTEGER(I4B) :: nsd
!
! OPEN(NEWUNIT=unitno, FILE="abstractfe_example.toml", STATUS="replace", &
!   ACTION="write")
! WRITE(unitno, '(A)') '[finite_element]'
! WRITE(unitno, '(A)') 'baseContinuity = "H1"'
! WRITE(unitno, '(A)') 'baseInterpolation = "Lagrange"'
! CLOSE(unitno)
! CALL obj%ImportFromToml(tomlName="finite_element", &
!   filename="abstractfe_example.toml", elemType=TypeElemNameOpt%Triangle, &
!   nsd=2_I4B)
! CALL obj%GetParam(nsd=nsd)
! CALL OK(obj%isInit .AND. nsd == 2_I4B, "Named TOML table initialization")
! CALL obj%DEALLOCATE()
! OPEN(NEWUNIT=unitno, FILE="abstractfe_example.toml", STATUS="old", &
!   ACTION="read")
! CLOSE(unitno, STATUS="delete")
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml, elemType, nsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    CHARACTER(*), INTENT(IN) :: tomlName
    !! Name of the finite element table in the TOML document.
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! Optional text-file handle passed to the TOML reader.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! Optional TOML input filename.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! Accepted for compatibility; unused in this overload.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Element type code from TypeElemNameOpt.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of physical spatial dimensions.
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetBaseInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-16
! summary: Return the basis interpolation code
!
! Returns the four-character code from opt%GetBaseInterpolation, rather than
! the original long family name.
!
!
! Returns:
!   ans : CHARACTER(4) - Four-character interpolation family code.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! REAL(DFP) :: coords(3, 3)
! INTEGER(I4B) :: nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(LEN_TRIM(obj%GetBaseInterpolation()) > 0, "Interpolation code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference vertex layout")
! CALL OK(ALL(ABS(coords(3, :)) < 1.0E-12_DFP), "Planar reference vertices")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    CHARACTER(4) :: ans
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
  END FUNCTION obj_GetBaseInterpolation
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetBaseContinuity@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Return the basis continuity code
!
! Returns the two-character code from opt%GetBaseContinuity.
!
!
! Returns:
!   ans : CHARACTER(2) - Two-character continuity code.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! REAL(DFP) :: coords(3, 3)
! INTEGER(I4B) :: nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(LEN_TRIM(obj%GetBaseInterpolation()) > 0, "Interpolation code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference vertex layout")
! CALL OK(ALL(ABS(coords(3, :)) < 1.0E-12_DFP), "Planar reference vertices")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE FUNCTION obj_GetBaseContinuity(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    CHARACTER(2) :: ans
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
  END FUNCTION obj_GetBaseContinuity
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetIpType@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Return the interpolation point type code
!
! Returns opt%GetIpType().
!
!
! Returns:
!   ans : INTEGER(I4B) - Interpolation point type code.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! REAL(DFP) :: coords(3, 3)
! INTEGER(I4B) :: nrow, ncol
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL OK(obj%GetTopologyType() == TypeElemNameOpt%Triangle, "Topology")
! CALL OK(obj%GetBaseContinuity() == "H1", "Continuity code")
! CALL OK(LEN_TRIM(obj%GetBaseInterpolation()) > 0, "Interpolation code")
! CALL OK(obj%GetIpType() == TypeInterpolationOpt%Equidistance, "Point type")
! CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 3_I4B .AND. ncol == 3_I4B, "Reference vertex layout")
! CALL OK(ALL(ABS(coords(3, :)) < 1.0E-12_DFP), "Planar reference vertices")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE FUNCTION obj_GetIpType(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B) :: ans
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
  END FUNCTION obj_GetIpType
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalInterpolationPoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Count interpolation points in a child class
!
! Return the interpolation point count for the supplied order and point-type
! arrays. The base implementation is a placeholder and reports an error in
! DEBUG mode. Use a concrete child that overrides this binding.
!
!
! Returns:
!   ans : INTEGER(I4B) - Number of interpolation points for the requested
!   order and type.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! REAL(DFP) :: xij(2, 3), points(3, 6)
! INTEGER(I4B) :: nrow, ncol, count, order
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%SetOrder(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "Child order setter")
! count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(count == 6_I4B, "Quadratic triangle point count")
! CALL obj%GetInterpolationPoints(xij=xij, ans=points, &
!   nrow=nrow, ncol=ncol, order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(ncol == count, "Interpolation output size")
! CALL OK(ALL(points(1:2, 1:ncol) >= -1.0E-12_DFP), "Points in triangle")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE FUNCTION obj_GetTotalInterpolationPoints( &
    obj, order, ipType) RESULT(ans)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction.
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! Interpolation point type code, or directional codes for point
    !! generation.
    INTEGER(I4B) :: ans
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
  END FUNCTION obj_GetTotalInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetInterpolationPoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Construct physical interpolation points in a child class
!
! Generate interpolation points for the specified orders and point types using
! the physical vertex coordinates xij. Writes the active coordinate block of
! ans and its dimensions. The base implementation is a placeholder and reports
! an error in DEBUG mode. Use a concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! REAL(DFP) :: xij(2, 3), points(3, 6)
! INTEGER(I4B) :: nrow, ncol, count, order
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%SetOrder(order=2_I4B)
! CALL obj%GetParam(order=order)
! CALL OK(order == 2_I4B, "Child order setter")
! count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(count == 6_I4B, "Quadratic triangle point count")
! CALL obj%GetInterpolationPoints(xij=xij, ans=points, &
!   nrow=nrow, ncol=ncol, order=[2_I4B], &
!   ipType=[TypeInterpolationOpt%Equidistance])
! CALL OK(ncol == count, "Interpolation output size")
! CALL OK(ALL(points(1:2, 1:ncol) >= -1.0E-12_DFP), "Points in triangle")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetInterpolationPoints( &
    obj, xij, ans, nrow, ncol, order, ipType, alpha, beta, lambda)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Coordinate output buffer; active block is ans(1:nrow,1:ncol).
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! Number of active coordinate rows written to ans.
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! Number of active coordinate columns written to ans.
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! Isotropic approximation order; for quadrature setters, accuracy by
    !! direction.
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! Interpolation point type code, or directional codes for point
    !! generation.
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
! date: 2026-10-18
! summary: Orient connectivity in a child class
!
! Reorder the active nptrs entries according to the configured element
! orientation. The base implementation is a placeholder and reports an error
! in DEBUG mode. Use a concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! INTEGER(I4B) :: nptrs(10), faceOrient(3, 3), ii
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL obj%SetOrder(order=3_I4B)
! nptrs = [(ii, ii=1,10)]
! faceOrient = 1_I4B
! faceOrient(1, 1) = -1_I4B
! CALL obj%SetOrientation(faceOrient=faceOrient, tface=3_I4B)
! CALL obj%FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
! CALL OK(ALL(nptrs(1:3) == [1_I4B, 2_I4B, 3_I4B]), "Vertices retained")
! CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), "First edge DOFs reversed")
! CALL OK(ALL(nptrs(6:10) == [6_I4B, 7_I4B, 8_I4B, 9_I4B, 10_I4B]), &
!   "Other DOFs retained")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_FixOrientOfNptrs(obj, nptrs, tsize)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! Node connectivity array; first tsize entries are active.
    INTEGER(I4B), INTENT(IN) :: tsize
    !! Number of active connectivity entries in nptrs.
  END SUBROUTINE obj_FixOrientOfNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project the unit constant on a facet in a child class
!
! Compute facet coefficients for the function with value one; this overload
! has no constant-value argument. The base implementation is a placeholder and
! reports an error in DEBUG mode. Use a concrete child that overrides this
! binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
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
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, &
!   xij=xij, localFaceNumber=1_I4B)
! ALLOCATE(samples(facets%nips))
! nodal = 2.0_DFP
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   geoElemsd=geoShapes, geoFacetElemsd=geoFacets, xij=xij, &
!   localFaceNumber=1_I4B, func=nodal, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.FALSE., tVertices=2_I4B)
! CALL OK(tsize == 2_I4B, "Facet projection size")
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Nodal constant projected")
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   xij=xij, localFaceNumber=1_I4B, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 1.0_DFP) < 1.0E-10_DFP), "Unit constant projected")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetDOFValueFromConstant( &
    obj, elemsd, facetElemsd, xij, localFaceNumber, ans, tsize, &
    massMat, ipiv, funcValue, onlyFaceBubble, icompo)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Solution shape data on the facet, or one dataset per facet.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Sample-value workspace; must accommodate the active quadrature points.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, request only interior spatial facet DOFs.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional one-based vector component; default one where implemented.
  END SUBROUTINE obj_GetFacetDOFValueFromConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project geometry nodal values onto a facet basis
!
! Interpolates func with geoFacetElemsd%N at its quadrature points, then calls
! GetL2ProjectionDOFValueFromQuadrature for facetElemsd. The two shape
! datasets must use matching quadrature points. elemsd, geoElemsd, xij and
! localFaceNumber are currently unused by this base implementation.
!
!
! Formula:
! $$ M_{ij} = \int N_i N_j \, d\Omega, \qquad
! b_i = \int N_i f \, d\Omega, \qquad M c = b. $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
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
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, &
!   xij=xij, localFaceNumber=1_I4B)
! ALLOCATE(samples(facets%nips))
! nodal = 2.0_DFP
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   geoElemsd=geoShapes, geoFacetElemsd=geoFacets, xij=xij, &
!   localFaceNumber=1_I4B, func=nodal, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, &
!   onlyFaceBubble=.FALSE., tVertices=2_I4B)
! CALL OK(tsize == 2_I4B, "Facet projection size")
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Nodal constant projected")
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   xij=xij, localFaceNumber=1_I4B, ans=ans, tsize=tsize, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(ALL(ABS(ans - 1.0_DFP) < 1.0E-10_DFP), "Unit constant projected")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetDOFValueFromVertex( &
    obj, elemsd, facetElemsd, geoElemsd, geoFacetElemsd, xij, &
    localFaceNumber, func, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble, tVertices)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoElemsd
    !! Required geometry cell shape data, or geometry traces by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoFacetElemsd
    !! Required geometry shape data on the facet, or datasets by facet.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    REAL(DFP), INTENT(INOUT) :: func(:)
    !! Geometry facet nodal values; SIZE(func) >= geoFacetElemsd%nns.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Sample-value workspace; must accommodate the active quadrature points.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, request only interior spatial facet DOFs.
    INTEGER(I4B), INTENT(IN) :: tVertices
    !! Number of leading DOFs excluded when projecting the interior block.
  END SUBROUTINE obj_GetFacetDOFValueFromVertex
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project a user function on a facet in a child class
!
! Compute facet coefficients at the supplied scalar time, optionally selecting
! a vector component and restricting the result to facet bubble DOFs. The base
! implementation is a placeholder and reports an error in DEBUG mode. Use a
! concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
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
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! ! Cubic triangle: three vertices, six edge DOFs and one cell DOF.
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetQuadraturePoints(quad=cellQuad)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL geometry%GetAllLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!   geoElemsd=geoShapes, geoFacetElemsd=geoFacets, xij=xij, tsize=count)
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, elemsd=cellShapes, &
!   geoelemsd=geoCellShapes, xij=xij, quad=cellQuad)
! ALLOCATE(samples(MAX(cellShapes%nips, MAXVAL(facets%nips))))
! CALL obj%GetFacetDOFValue(elemsd=shapes(1), facetElemsd=facets(1), &
!   xij=xij, times=0.0_DFP, localFaceNumber=1_I4B, func=func, &
!   ans=facetAns, tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 4_I4B, "Cubic facet projection size")
! CALL OK(ALL(ABS(facetAns(1:tsize) - 2.0_DFP) < 1.0E-9_DFP), &
!   "User function projected on facet")
! CALL obj%GetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   cellElemsd=cellShapes, xij=xij, times=0.0_DFP, func=func, ans=ans, &
!   tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, temp=temp)
! CALL OK(tsize == 10_I4B, "Cubic triangle coefficient count")
! ! Exercise the interior residual step with the boundary values already set.
! CALL obj%GetInCellDOFValue(cellElemsd=cellShapes, func=func, &
!   times=0.0_DFP, ans=ans, temp=temp, tsize=tsize, massMat=massMat, &
!   ipiv=ipiv, funcValue=samples, offset=9_I4B)
! CALL OK(tsize == 1_I4B, "One interior coefficient")
! CALL obj%GetDOFValue(geofeptr=geometry, elemsd=shapes, &
!   geoelemsd=geoShapes, facetElemsd=facets, geoFacetElemsd=geoFacets, &
!   cellElemsd=cellShapes, geoCellElemsd=geoCellShapes, quad=quad, &
!   facetQuad=facetQuad, cellQuad=cellQuad, xij=xij, times=0.0_DFP, &
!   func=func, ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, temp=temp)
! CALL OK(tsize == 10_I4B, "High-level user-function projection")
! CALL OK(ALL(ABS(MATMUL(TRANSPOSE( &
!   cellShapes%N(1:10, 1:cellShapes%nips)), ans) - 2.0_DFP) &
!   < 1.0E-9_DFP), "Projected constant reproduced at quadrature points")
! CALL OK(ALL(ABS(ans(1:3) - 2.0_DFP) < 1.0E-10_DFP), "Vertex values")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetDOFValueFromSTFunc( &
    obj, elemsd, facetElemsd, xij, times, localFaceNumber, func, ans, tsize, &
    massMat, ipiv, funcValue, onlyFaceBubble, icompo)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Solution shape data on the facet, or one dataset per facet.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    REAL(DFP), INTENT(IN) :: times
    !! Physical time at which the user function is evaluated.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function accepting spatial coordinates and time.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Sample-value workspace; must accommodate the active quadrature points.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, request only interior spatial facet DOFs.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional one-based vector component; default one where implemented.
  END SUBROUTINE obj_GetFacetDOFValueFromSTFunc
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project a user function on a space-time facet
!
! Compute coefficients on the product of a spatial facet and a time element
! over times(1:2). Return the active row and column ranges of ans. The base
! implementation is a placeholder and reports an error in DEBUG mode. Use a
! concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE LineH1LagrangeFE_Class, ONLY: LineH1LagrangeFE_
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
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, &
!   xij=xij, localFaceNumber=1_I4B)
! CALL timeFE%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[8_I4B])
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
!   timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, 1.0_DFP], &
!   localFaceNumber=1_I4B, ans=ans, nrowStart=r1, nrowEnd=r2, &
!   ncolStart=c1, ncolEnd=c2, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, temp=temp, onlyFaceBubble=.TRUE.)
! CALL OK(r1 == 3_I4B .AND. r2 == 4_I4B, "Spatial bubble rows")
! CALL OK(c1 == 1_I4B .AND. c2 == 4_I4B, "All temporal columns")
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, 1.0_DFP], &
!   localFaceNumber=1_I4B, func=func, ans=ans, &
!   nrowStart=r1, nrowEnd=r2, ncolStart=c1, ncolEnd=c2, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, temp=temp, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(r1 == 3_I4B .AND. c2 == 4_I4B, "User-function output range")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! CALL timeFE%DEALLOCATE()
! CALL timeGeometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetSTFacetDOFValueFromSTFunc( &
    obj, elemsd, facetElemsd, timeElemsd, xij, times, localFaceNumber, &
    func, ans, nrowStart, nrowEnd, ncolStart, ncolEnd, massMat, ipiv, &
    funcValue, temp, onlyFaceBubble, icompo)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: timeElemsd
    !! Prepared time-element shape data.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    REAL(DFP), INTENT(IN) :: times(:)
    !! Time interval endpoints; SIZE(times) = 2.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function accepting spatial coordinates and time.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Coefficient buffer; active entries or index ranges are returned
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
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    REAL(DFP), INTENT(INOUT) :: funcValue(:, :)
    !! Sample-value workspace; must accommodate the active quadrature points.
    REAL(DFP), INTENT(INOUT) :: temp(:)
    !! Coefficient workspace; SIZE(temp) must fit the projected DOF block.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, request only interior spatial facet DOFs.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional one-based vector component; default one where implemented.
  END SUBROUTINE obj_GetSTFacetDOFValueFromSTFunc
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFacetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Project the unit constant on a space-time facet
!
! Compute coefficients for the function with value one on a spatial facet and
! time element. Return the active row and column ranges of ans. The base
! implementation is a placeholder and reports an error in DEBUG mode. Use a
! concrete child that overrides this binding.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! USE LineH1LagrangeFE_Class, ONLY: LineH1LagrangeFE_
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
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
!   elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
!   geoFacetElemsd=geoFacets, quad=quad, facetQuad=facetQuad, &
!   xij=xij, localFaceNumber=1_I4B)
! CALL timeFE%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[8_I4B])
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
!   timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, 1.0_DFP], &
!   localFaceNumber=1_I4B, ans=ans, nrowStart=r1, nrowEnd=r2, &
!   ncolStart=c1, ncolEnd=c2, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, temp=temp, onlyFaceBubble=.TRUE.)
! CALL OK(r1 == 3_I4B .AND. r2 == 4_I4B, "Spatial bubble rows")
! CALL OK(c1 == 1_I4B .AND. c2 == 4_I4B, "All temporal columns")
! CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, 1.0_DFP], &
!   localFaceNumber=1_I4B, func=func, ans=ans, &
!   nrowStart=r1, nrowEnd=r2, ncolStart=c1, ncolEnd=c2, &
!   massMat=massMat, ipiv=ipiv, funcValue=samples, temp=temp, &
!   onlyFaceBubble=.TRUE.)
! CALL OK(r1 == 3_I4B .AND. c2 == 4_I4B, "User-function output range")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! CALL timeFE%DEALLOCATE()
! CALL timeGeometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalElemShapeData2]],
! [[obj_GetAllLocalFacetElemShapeData]]

INTERFACE
  MODULE SUBROUTINE obj_GetSTFacetDOFValueFromConstant( &
    obj, elemsd, facetElemsd, timeElemsd, xij, times, localFaceNumber, &
    ans, nrowStart, nrowEnd, ncolStart, ncolEnd, massMat, ipiv, funcValue, &
    temp, onlyFaceBubble, icompo)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: timeElemsd
    !! Prepared time-element shape data.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    REAL(DFP), INTENT(IN) :: times(:)
    !! Time interval endpoints; SIZE(times) = 2.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! One-based local face number.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Coefficient buffer; active entries or index ranges are returned
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
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    REAL(DFP), INTENT(INOUT) :: funcValue(:, :)
    !! Sample-value workspace; must accommodate the active quadrature points.
    REAL(DFP), INTENT(INOUT) :: temp(:)
    !! Coefficient workspace; SIZE(temp) must fit the projected DOF block.
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! If true, request only interior spatial facet DOFs.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional one-based vector component; default one where implemented.
  END SUBROUTINE obj_GetSTFacetDOFValueFromConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                    GetDOFValueFromQuadrature@CellDOFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-01
! summary: Prepare quadrature and shape data before DOF projection
!
! Builds all facet and cell quadrature rules, evaluates local geometry and
! solution shapes, maps them using xij and delegates to GetDOFValueFromSTFunc.
! Caller supplies the shape-data containers, quadrature containers and numeric
! workspaces; their contents are populated or overwritten.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
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
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! ! Cubic triangle: three vertices, six edge DOFs and one cell DOF.
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetQuadraturePoints(quad=cellQuad)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL geometry%GetAllLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!   geoElemsd=geoShapes, geoFacetElemsd=geoFacets, xij=xij, tsize=count)
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, elemsd=cellShapes, &
!   geoelemsd=geoCellShapes, xij=xij, quad=cellQuad)
! ALLOCATE(samples(MAX(cellShapes%nips, MAXVAL(facets%nips))))
! CALL obj%GetFacetDOFValue(elemsd=shapes(1), facetElemsd=facets(1), &
!   xij=xij, times=0.0_DFP, localFaceNumber=1_I4B, func=func, &
!   ans=facetAns, tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 4_I4B, "Cubic facet projection size")
! CALL OK(ALL(ABS(facetAns(1:tsize) - 2.0_DFP) < 1.0E-9_DFP), &
!   "User function projected on facet")
! CALL obj%GetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   cellElemsd=cellShapes, xij=xij, times=0.0_DFP, func=func, ans=ans, &
!   tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, temp=temp)
! CALL OK(tsize == 10_I4B, "Cubic triangle coefficient count")
! ! Exercise the interior residual step with the boundary values already set.
! CALL obj%GetInCellDOFValue(cellElemsd=cellShapes, func=func, &
!   times=0.0_DFP, ans=ans, temp=temp, tsize=tsize, massMat=massMat, &
!   ipiv=ipiv, funcValue=samples, offset=9_I4B)
! CALL OK(tsize == 1_I4B, "One interior coefficient")
! CALL obj%GetDOFValue(geofeptr=geometry, elemsd=shapes, &
!   geoelemsd=geoShapes, facetElemsd=facets, geoFacetElemsd=geoFacets, &
!   cellElemsd=cellShapes, geoCellElemsd=geoCellShapes, quad=quad, &
!   facetQuad=facetQuad, cellQuad=cellQuad, xij=xij, times=0.0_DFP, &
!   func=func, ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, temp=temp)
! CALL OK(tsize == 10_I4B, "High-level user-function projection")
! CALL OK(ALL(ABS(MATMUL(TRANSPOSE( &
!   cellShapes%N(1:10, 1:cellShapes%nips)), ans) - 2.0_DFP) &
!   < 1.0E-9_DFP), "Projected constant reproduced at quadrature points")
! CALL OK(ALL(ABS(ans(1:3) - 2.0_DFP) < 1.0E-10_DFP), "Vertex values")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromSTFunc2( &
    obj, geofeptr, elemsd, geoelemsd, facetElemsd, geoFacetElemsd, &
    cellElemsd, geoCellElemsd, quad, facetQuad, cellQuad, xij, times, &
    func, ans, tsize, massMat, ipiv, funcValue, temp, &
    icompo)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    CLASS(AbstractFE_), INTENT(INOUT) :: geofeptr
    !! Concrete finite element describing the geometry basis.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd(:)
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd(:)
    !! Required geometry cell shape data, or geometry traces by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd(:)
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoFacetElemsd(:)
    !! Required geometry shape data on the facet, or datasets by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: cellElemsd
    !! Solution cell shape data used for interior DOF projection.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoCellElemsd
    !! Geometry cell shape data used for interior DOF projection.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad(:)
    !! Cell quadrature, or cell quadrature coordinates indexed by facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad(:)
    !! Facet quadrature, or one quadrature container per facet.
    TYPE(QuadraturePoint_), INTENT(INOUT) :: cellQuad
    !! Cell quadrature used for interior DOF projection.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    REAL(DFP), INTENT(IN) :: times
    !! Physical time at which the user function is evaluated.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function accepting spatial coordinates and time.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Sample-value workspace; must accommodate the active quadrature points.
    REAL(DFP), INTENT(INOUT) :: temp(:)
    !! Coefficient workspace; SIZE(temp) must fit the projected DOF block.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional one-based vector component; default one where implemented.
  END SUBROUTINE obj_GetDOFValueFromSTFunc2
END INTERFACE

!----------------------------------------------------------------------------
!                                  GetDOFValueFromUserFunction@CellDOFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary: Compute vertex, facet and interior cell coefficients
!
! Evaluates vertex values, loops over opt%GetTotalFace to project facet bubble
! residuals, then projects the interior cell residual. Coefficients are
! ordered as vertices, successive facet blocks, then the cell block. Shape
! data must already be mapped to physical coordinates. The child must
! implement facet projection for the chosen basis.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
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
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! ! Cubic triangle: three vertices, six edge DOFs and one cell DOF.
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetQuadraturePoints(quad=cellQuad)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL geometry%GetAllLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!   geoElemsd=geoShapes, geoFacetElemsd=geoFacets, xij=xij, tsize=count)
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, elemsd=cellShapes, &
!   geoelemsd=geoCellShapes, xij=xij, quad=cellQuad)
! ALLOCATE(samples(MAX(cellShapes%nips, MAXVAL(facets%nips))))
! CALL obj%GetFacetDOFValue(elemsd=shapes(1), facetElemsd=facets(1), &
!   xij=xij, times=0.0_DFP, localFaceNumber=1_I4B, func=func, &
!   ans=facetAns, tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 4_I4B, "Cubic facet projection size")
! CALL OK(ALL(ABS(facetAns(1:tsize) - 2.0_DFP) < 1.0E-9_DFP), &
!   "User function projected on facet")
! CALL obj%GetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   cellElemsd=cellShapes, xij=xij, times=0.0_DFP, func=func, ans=ans, &
!   tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, temp=temp)
! CALL OK(tsize == 10_I4B, "Cubic triangle coefficient count")
! ! Exercise the interior residual step with the boundary values already set.
! CALL obj%GetInCellDOFValue(cellElemsd=cellShapes, func=func, &
!   times=0.0_DFP, ans=ans, temp=temp, tsize=tsize, massMat=massMat, &
!   ipiv=ipiv, funcValue=samples, offset=9_I4B)
! CALL OK(tsize == 1_I4B, "One interior coefficient")
! CALL obj%GetDOFValue(geofeptr=geometry, elemsd=shapes, &
!   geoelemsd=geoShapes, facetElemsd=facets, geoFacetElemsd=geoFacets, &
!   cellElemsd=cellShapes, geoCellElemsd=geoCellShapes, quad=quad, &
!   facetQuad=facetQuad, cellQuad=cellQuad, xij=xij, times=0.0_DFP, &
!   func=func, ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, temp=temp)
! CALL OK(tsize == 10_I4B, "High-level user-function projection")
! CALL OK(ALL(ABS(MATMUL(TRANSPOSE( &
!   cellShapes%N(1:10, 1:cellShapes%nips)), ans) - 2.0_DFP) &
!   < 1.0E-9_DFP), "Projected constant reproduced at quadrature points")
! CALL OK(ALL(ABS(ans(1:3) - 2.0_DFP) < 1.0E-10_DFP), "Vertex values")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromSTFunc( &
    obj, elemsd, facetElemsd, cellElemsd, xij, times, func, ans, tsize, &
    massMat, ipiv, funcValue, temp, icompo)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd(:)
    !! Solution cell shape data, or cell traces indexed by facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: facetElemsd(:)
    !! Solution shape data on the facet, or one dataset per facet.
    TYPE(ElemShapeData_), INTENT(INOUT) :: cellElemsd
    !! Solution cell shape data used for interior DOF projection.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    REAL(DFP), INTENT(IN) :: times
    !! Physical time at which the user function is evaluated.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function accepting spatial coordinates and time.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Sample-value workspace; must accommodate the active quadrature points.
    REAL(DFP), INTENT(INOUT) :: temp(:)
    !! Coefficient workspace; SIZE(temp) must fit the projected DOF block.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional one-based vector component; default one where implemented.
  END SUBROUTINE obj_GetDOFValueFromSTFunc
END INTERFACE

!----------------------------------------------------------------------------
!                                  GetVertexDOFValueFromSTFunc@CellDOFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-01
! summary: Evaluate a user function at cell vertices
!
! Evaluates func at the first opt%GetTotalVertex columns of xij with arguments
! (x,y,z,t); unused spatial coordinates are zero. Scalar functions return one
! value. For vector functions icompo defaults to one; the internal return
! buffer holds ten values. DEBUG mode checks that the function accepts at
! least four arguments.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! USE BaseType, ONLY: TypeFEVariableOpt
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(UserFunction_) :: func
! REAL(DFP) :: xij(2, 3)
! INTEGER(I4B) :: tsize
! REAL(DFP) :: ans(3)
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetVertexDOFValue(ans=ans, tsize=tsize, &
!   func=func, xij=xij, times=0.0_DFP)
! CALL OK(tsize == 3_I4B, "Three vertex coefficients")
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Vertex values")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetVertexDOFValueFromSTFunc( &
    obj, ans, tsize, func, xij, times, icompo)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function accepting spatial coordinates and time.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Physical nodal coordinates, with coordinate directions in rows.
    REAL(DFP), INTENT(IN) :: times
    !! Physical time at which the user function is evaluated.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional one-based vector component; default one where implemented.
  END SUBROUTINE obj_GetVertexDOFValueFromSTFunc
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetInCellDOFValue@CellDOFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Project the residual onto interior cell degrees of freedom
!
! Samples func at cellElemsd%coord and subtracts the interpolant formed from
! ans(1:offset). Projects this residual using GetDOFValueFromQuadrature with
! onlyInside true and appends tsize coefficients to
! ans(offset+1:offset+tsize). For a vector function icompo defaults to one;
! the internal return buffer holds ten values.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
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
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
!   argType=TypeFEVariableOpt%space, numArgs=4_I4B, numReturns=1_I4B)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! ! Cubic triangle: three vertices, six edge DOFs and one cell DOF.
! CALL geometry%Copy(obj)
! CALL obj%SetOrder(order=3_I4B)
! CALL obj%SetQuadratureOrder(order1=8_I4B)
! CALL obj%GetQuadraturePoints(quad=cellQuad)
! CALL obj%GetAllFacetQuadraturePoints( &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL geometry%GetAllLocalFacetElemShapeData( &
!   elemsd=geoShapes, facetElemsd=geoFacets, &
!   quad=quad, facetQuad=facetQuad, tsize=count)
! CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
!   geoElemsd=geoShapes, geoFacetElemsd=geoFacets, xij=xij, tsize=count)
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, elemsd=cellShapes, &
!   geoelemsd=geoCellShapes, xij=xij, quad=cellQuad)
! ALLOCATE(samples(MAX(cellShapes%nips, MAXVAL(facets%nips))))
! CALL obj%GetFacetDOFValue(elemsd=shapes(1), facetElemsd=facets(1), &
!   xij=xij, times=0.0_DFP, localFaceNumber=1_I4B, func=func, &
!   ans=facetAns, tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, onlyFaceBubble=.FALSE.)
! CALL OK(tsize == 4_I4B, "Cubic facet projection size")
! CALL OK(ALL(ABS(facetAns(1:tsize) - 2.0_DFP) < 1.0E-9_DFP), &
!   "User function projected on facet")
! CALL obj%GetDOFValue(elemsd=shapes, facetElemsd=facets, &
!   cellElemsd=cellShapes, xij=xij, times=0.0_DFP, func=func, ans=ans, &
!   tsize=tsize, massMat=massMat, ipiv=ipiv, funcValue=samples, temp=temp)
! CALL OK(tsize == 10_I4B, "Cubic triangle coefficient count")
! ! Exercise the interior residual step with the boundary values already set.
! CALL obj%GetInCellDOFValue(cellElemsd=cellShapes, func=func, &
!   times=0.0_DFP, ans=ans, temp=temp, tsize=tsize, massMat=massMat, &
!   ipiv=ipiv, funcValue=samples, offset=9_I4B)
! CALL OK(tsize == 1_I4B, "One interior coefficient")
! CALL obj%GetDOFValue(geofeptr=geometry, elemsd=shapes, &
!   geoelemsd=geoShapes, facetElemsd=facets, geoFacetElemsd=geoFacets, &
!   cellElemsd=cellShapes, geoCellElemsd=geoCellShapes, quad=quad, &
!   facetQuad=facetQuad, cellQuad=cellQuad, xij=xij, times=0.0_DFP, &
!   func=func, ans=ans, tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   funcValue=samples, temp=temp)
! CALL OK(tsize == 10_I4B, "High-level user-function projection")
! CALL OK(ALL(ABS(MATMUL(TRANSPOSE( &
!   cellShapes%N(1:10, 1:cellShapes%nips)), ans) - 2.0_DFP) &
!   < 1.0E-9_DFP), "Projected constant reproduced at quadrature points")
! CALL OK(ALL(ABS(ans(1:3) - 2.0_DFP) < 1.0E-10_DFP), "Vertex values")
! CALL func%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetInCellDOFValueFromSTFunc( &
    obj, cellElemsd, func, times, ans, temp, tsize, massMat, ipiv, &
    funcValue, offset, icompo)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapeData_), INTENT(INOUT) :: cellElemsd
    !! Solution cell shape data used for interior DOF projection.
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! Scalar or vector user function accepting spatial coordinates and time.
    REAL(DFP), INTENT(IN) :: times
    !! Physical time at which the user function is evaluated.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
    REAL(DFP), INTENT(INOUT) :: temp(:)
    !! Coefficient workspace; SIZE(temp) must fit the projected DOF block.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! Sample-value workspace; must accommodate the active quadrature points.
    INTEGER(I4B), INTENT(IN) :: offset
    !! Number of coefficients already stored at the beginning of ans.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! Optional one-based vector component; default one where implemented.
  END SUBROUTINE obj_GetInCellDOFValueFromSTFunc
END INTERFACE

!----------------------------------------------------------------------------
!                                    GetDOFValueFromQuadrature@CellDOFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-01
! summary: Project sampled values onto a cell basis
!
! Assembles the mass matrix and load vector from elemsd and func, factors the
! selected mass matrix block with LU, then solves for coefficients. With
! onlyInside true, solves the block starting at tVertices+1 and compacts the
! resulting coefficients into ans(1:tsize). Otherwise solves for all
! elemsd%nns coefficients. Input samples should already have any prescribed
! boundary contribution removed when projecting a residual.
!
!
! Formula:
! $$ M_{ij} = \int N_i N_j \, d\Omega, \qquad
! b_i = \int N_i f \, d\Omega, \qquad M c = b. $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: TypeElemNameOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
! USE Test_Method, ONLY: OK
! USE BaseType, ONLY: ElemShapeData_
! USE BaseType, ONLY: QuadraturePoint_
! IMPLICIT NONE
! TYPE(TriangleH1LagrangeFE_) :: obj
! TYPE(TriangleH1LagrangeFE_) :: geometry
! TYPE(ElemShapeData_) :: shapes, geoShapes
! TYPE(QuadraturePoint_) :: quad
! REAL(DFP) :: xij(2, 3), ans(3), massMat(3, 3)
! REAL(DFP), ALLOCATABLE :: samples(:)
! INTEGER(I4B) :: ipiv(3), tsize
!
! CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange", order=1_I4B, &
!   ipType=TypeInterpolationOpt%Equidistance, &
!   quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
!   quadratureOrder=[4_I4B])
!
! CALL geometry%Copy(obj)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [1.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 1.0_DFP]
! CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, elemsd=shapes, &
!   geoelemsd=geoShapes, xij=xij, quad=quad)
! ALLOCATE(samples(shapes%nips))
! samples = 2.0_DFP
! CALL obj%GetDOFValue(elemsd=shapes, func=samples, ans=ans, &
!   tsize=tsize, massMat=massMat, ipiv=ipiv, &
!   onlyInside=.FALSE., tVertices=3_I4B)
! CALL OK(tsize == 3_I4B, "All coefficients projected")
! CALL OK(ALL(ABS(ans - 2.0_DFP) < 1.0E-10_DFP), "Constant reproduced")
! CALL obj%DEALLOCATE()
! CALL geometry%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetParam]], [[AbstractFEDeallocate]]

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromQuadrature( &
    obj, elemsd, func, ans, tsize, massMat, ipiv, onlyInside, tVertices)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object; use a concrete extension of AbstractFE_.
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! Solution cell shape data, or cell traces indexed by facet.
    REAL(DFP), INTENT(INOUT) :: func(:)
    !! Quadrature samples; SIZE(func) >= elemsd%nips.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Coefficient buffer; active entries or index ranges are returned
    !! separately.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of coefficients written to ans.
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! Mass-matrix workspace; both dimensions must fit the projected basis.
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! LU pivot workspace; SIZE(ipiv) must fit the projected basis.
    LOGICAL(LGT), INTENT(IN) :: onlyInside
    !! If true, solve only DOFs after tVertices and compact the output.
    INTEGER(I4B), INTENT(IN) :: tVertices
    !! Number of leading DOFs excluded when projecting the interior block.
  END SUBROUTINE obj_GetDOFValueFromQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractFE_Class
