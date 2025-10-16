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

MODULE AbstractFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: ElemShapeData_, QuadraturePoint_
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE BasisOpt_Class, ONLY: BasisOpt_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE
PRIVATE

PUBLIC :: AbstractFE_
PUBLIC :: AbstractFEPointer_
PUBLIC :: AbstractFEDeallocate
PUBLIC :: AbstractFEDisplay
PUBLIC :: DEALLOCATE

CHARACTER(*), PARAMETER :: modName = "AbstractFE_Class"

!----------------------------------------------------------------------------
!                                                                 AbstractFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Abstract class for finite element is defined
!
!{!pages/docs-api/AbstractFE/AbstractFE_.md!}

TYPE, ABSTRACT :: AbstractFE_
  ! PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is set to true at the time of constructor
  TYPE(BasisOpt_) :: opt
  !! basis options

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  !@ConstructorMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate method from arguments
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Initiate by copy
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Initiate by copy

  !IO:
  !@IOMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of a finite element
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: MdEncode => obj_MdEncode
  !! Display the contents
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ReactEncode => &
    obj_ReactEncode
  !! Display the contents
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Deallocate the data stored in an instance
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ImportFromToml1 => &
    obj_ImportFromToml1
  !! Import the data from toml file
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  !! Import the data from toml table
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

  ! SET:
  ! @SetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Sets the parameters of finite element
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order and reallocate appropriate data in
  !! already initiated AbstractFE_
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set order of quadrature
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set quadrature type

  !GET:
  ! @GetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetRefElemCoord => &
    obj_GetRefElemCoord
  !! Get the coordinates of reference element
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTopologyType => &
    obj_GetTopologyType
  !! returns the topoType
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Sets the parameters of finite element
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Get the base interpolation
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseContinuity => &
    obj_GetBaseContinuity
  !! Get the base continuity
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get local element shape data for cell element
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Get global element shape data
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetLocalFacetElemShapeData => obj_GetLocalFacetElemShapeData
  !! Get local element shape data for cell element and
  !! local face number
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFacetElemShapeData => &
    obj_GetGlobalFacetElemShapeData
  !! Get global element shape data for cell and facet

  ! GET:
  ! @QuadratureMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Get quadrature points in cell element
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetFacetQuadraturePoints => obj_GetFacetQuadraturePoints
  !! Get quadrature points on the face of cell element
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetTotalQuadraturePoints => obj_GetTotalQuadraturePoints
  !! Get total number of quadrature points

  ! GET:
  ! @InterpolationMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetInterpolationPoints => &
    obj_GetInterpolationPoints
  !! Get Interpolation points in cell element
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetTotalInterpolationPoints => obj_GetTotalInterpolationPoints
  !! Get total number of Interpolation points
END TYPE AbstractFE_

!----------------------------------------------------------------------------
!                                                         AbstractFEPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractFEPointer_
  CLASS(AbstractFE_), POINTER :: ptr => NULL()
END TYPE AbstractFEPointer_

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiates an instance of the finite element

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, elemType, nsd, baseContinuity, baseInterpolation, feType, ipType, &
    basisType, alpha, beta, lambda, dofType, transformType, order, &
    anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, faceOrient, &
    edgeOrient, tcell, tface, tedge, errCheck, quadratureIsHomogeneous, &
    quadratureType, quadratureOrder, quadratureIsOrder, quadratureNips, &
    quadratureIsNips, quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Finite element object
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Type of finite element
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity or Conformity of basis function.
    !! This parameter is used to determine the nodal coordinates of
    !! reference element, when xij is not present.
    !! If xij is present then this parameter is ignored
    !! H1* (default), HDiv, HCurl, DG
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis function family used for interpolation.
    !! This parameter is used to determine the nodal coordinates of
    !! reference element, when xij is not present.
    !! If xij is present then this parameter is ignored
    !! LagrangeInterpolation, LagrangePolynomial
    !! SerendipityInterpolation, SerendipityPolynomial
    !! HierarchyInterpolation, HierarchyPolynomial
    !! OrthogonalInterpolation, OrthogonalPolynomial
    !! HermitInterpolation, HermitPolynomial
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial
    !! Default ipType is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Basis type: Legendre, Lobatto, Ultraspherical,
    !! Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar
    !! For HDiv and Hcurl it should be Vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! Degree of freedom type, default is nodal
    !! The size of dofType is 4
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! aniso tropic order, necessary for Lagrange interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, necessary for Hierarchical interpolation
    !! number of rows in faceOrder is 3
    !! number of columns in faceOrder is tfaceorder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order, necessary for Hierarchical interpolation
    !! size of edgeorder is tedgeorder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! cell orient, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient, necessary for Hierarchical interpolation
    !! number of rows in faceoriient is 3
    !! number of columns in faceorient is tfaceorient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orient, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! size of cellOrder, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! number of columns in faceOrder,
    !! necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! size of edgeorder, necessary for Hierarchical interpolation
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! is quadratur homogeneous in all dimensions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    !! Quadrature type in x, y, and z directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureOrder(:)
    !! quadrature accuracy in x, y, and z direction
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Is quadrature order considered
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! Number of interpolation points in x, y, and z directions
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Quadrature nips
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Quadrature alpha
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    !! Quadrature beta
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    !! Quadrature lambda
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary: Initiates an instance of the finite element by copying

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(AbstractFE_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary: Deallocate the data

INTERFACE AbstractFEDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractFEDeallocate

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE AbstractFEDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(AbstractFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE AbstractFEDeallocate

INTERFACE DEALLOCATE
  MODULE PROCEDURE Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                             Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-14
! summary: Display the content

INTERFACE AbstractFEDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno, notFull)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull
  END SUBROUTINE obj_Display
END INTERFACE AbstractFEDisplay

!----------------------------------------------------------------------------
!                                                            MdEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the contents in mardown format

INTERFACE
  MODULE FUNCTION obj_MdEncode(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_MdEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                        ReactEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the reference elements in react components

INTERFACE
  MODULE FUNCTION obj_ReactEncode(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Set the parameters

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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! order in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! order of approximation on the edges of element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! order of approximation on the faces of element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! order of approximation in the cell of element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! finite element type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Reference element type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
    !! Topology of reference element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemIndx
    !! Element index of the reference element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation point type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Basis type in x, y, and z directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameter
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! degree of freedom type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    !! String name of type of continuity used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
    !! String name of type of interpolation used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refElemDomain
    !! Domain of reference element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isIsotropicOrder
    !! True if isotropic order
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isAnisotropicOrder
    !! True if anisoOrder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isEdgeOrder
    !! is edge order set
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFaceOrder
    !! is face order set
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isCellOrder
    !! is cell order set
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdgeOrder
    !! total edge order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFaceOrder
    !! total face order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCellOrder
    !! total cell order
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsHomogeneous
    !! is quadratur homogeneous in all dimensions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    !! Quadrature type in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    !! Quadrature type in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    !! Quadrature type in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
    !! Quadrature type in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder(:)
    !! quadrature accuracy in x, y, and z direction
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder1
    !! quadrature accuracy in x, y, and z direction
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder2
    !! quadrature accuracy in x, y, and z direction
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder3
    !! quadrature accuracy in x, y, and z direction
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Is quadrature order considered
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(:)
    !! Number of interpolation points in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips1
    !! Number of interpolation points in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips2
    !! Number of interpolation points in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips3
    !! Number of interpolation points in x, y, and z directions
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Should we consider nips
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha(:)
    !! Jacobi parameters for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha1
    !! Jacobi parameters for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha2
    !! Jacobi parameters for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha3
    !! Jacobi parameters for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta1
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta2
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta3
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda1
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda2
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda3
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetOrder@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  This routine set order in the already initiated AbstractFE_
!
!# Introduction
!
! This routine sets order in the already initiated AbstractFE_
! Make sure the object is initiated by calling correct constructor methods

INTERFACE
  MODULE SUBROUTINE obj_SetOrder( &
    obj, order, anisoorder, cellOrder, faceOrder, edgeOrder, cellOrient, &
    faceOrient, edgeOrient, tcell, tface, tedge, errCheck)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoorder(:)
    !! aniso tropic order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order
    !! number of rows in faceOrder is 3
    !! number of columns in faceOrder is tfaceorder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order
    !! size of edgeorder is tedgeorder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! cell orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient
    !! number of rows in faceoriient is 3
    !! number of columns in faceorient is tfaceorient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! size of cellOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! number of columns in faceOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! size of edgeorder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetRefElemCoord@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Get the reference element coordinates

INTERFACE
  MODULE SUBROUTINE obj_GetRefElemCoord(obj, ans, nrow, ncol)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Reference element coordinates
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of data written in ans
  END SUBROUTINE obj_GetRefElemCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Get the parameters

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
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: anisoOrder(3)
    !! order in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: edgeOrder(:)
    !! order of approximation on the edges of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: faceOrder(:, :)
    !! order of approximation on the faces of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: cellOrder(:)
    !! order of approximation in the cell of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: fetype
    !! finite element type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemType
    !! Reference element type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: topoType
    !! Get the topology type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemIndx
    !! Get the index of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ipType
    !! interpolation point type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: basisType(3)
    !! Basis type in x, y, and z directions
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha(3)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta(3)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda(3)
    !! Ultraspherical parameter
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: dofType(4)
    !! degree of freedom type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: transformType
    !! transformation type
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseContinuity
    !! String name of type of continuity used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseInterpolation
    !! String name of type of interpolation used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: refElemDomain
    !! Domain of reference element
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isIsotropicOrder
    !! True if isotropic order
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isAnisotropicOrder
    !! True if anisoOrder
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isEdgeOrder
    !! is edge order set
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isFaceOrder
    !! is face order set
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isCellOrder
    !! is cell order set
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tEdgeOrder
    !! total edge order
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tFaceOrder
    !! total face order
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tCellOrder
    !! total cell order
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsHomogeneous
    !! see QuadratureOpt_
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureType(3)
    !! see QuadratureOpt_
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureOrder(3)
    !! See QuadratureOpt_
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureNips(3)
    !! See QuadratureOpt_
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureAlpha(3)
    !! See QuadratureOpt_
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureBeta(3)
    !! See QuadratureOpt_
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureLambda(3)
    !! See QuadratureOpt_
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsOrder
    !! See QuadratureOpt_
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsNips
    !! See QuadratureOpt_
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetLocalElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetLocalFacetElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data in cell and facet

INTERFACE
  MODULE SUBROUTINE obj_GetLocalFacetElemShapeData( &
    obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd, facetElemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
  END SUBROUTINE obj_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetGlobalElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoelemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetGlobalFacetElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data in cell and facet

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalFacetElemShapeData( &
    obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd, facetElemsd
    !! element shape data in cell and facet
    TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd, geoFacetElemsd
    !! element shape data for geometry in cell and facet
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of cell element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
  END SUBROUTINE obj_GetGlobalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get quadrature points

! obj_Initiate9(obj, elemType, domainName, order, quadratureType,&
! alpha, beta, lambda, xij)

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetFacetQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get quadrature points on a local face of element

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                 localFaceNumber)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    !! Quadrature points
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
  END SUBROUTINE obj_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetTotalQuadraturePoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Get total number of quadrature points

INTERFACE
  MODULE FUNCTION obj_GetTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetQuadratureOrder@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the order for quadrature

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureOrder(obj, order, order1, order2, order3)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order3
  END SUBROUTINE obj_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuadratureType@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary:  Set the quadrature type

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureType( &
    obj, quadratureType, quadratureType1, quadratureType2, quadratureType3)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
  END SUBROUTINE obj_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetTopologyType@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Returns the topoType

INTERFACE
  MODULE FUNCTION obj_GetTopologyType(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTopologyType
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary:  Import data from toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, elemType, nsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type of finite element, optional
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml, elemType, nsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetBaseInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-16
! summary:  Get the baseInterpolation

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    CHARACTER(4) :: ans
  END FUNCTION obj_GetBaseInterpolation
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetBaseContinuity@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetBaseContinuity(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    CHARACTER(2) :: ans
  END FUNCTION obj_GetBaseContinuity
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalInterpolationPoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Get total number of interpolation points

INTERFACE
  MODULE FUNCTION obj_GetTotalInterpolationPoints( &
    obj, order, ipType) RESULT(ans)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order of interpolation in x, y, and z directions
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! interpolation point type in x, y, and z directions
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetInterpolationPoints@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get Interpolation points

INTERFACE
  MODULE SUBROUTINE obj_GetInterpolationPoints( &
    obj, xij, ans, nrow, ncol, order, ipType, alpha, beta, lambda)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Abstract finite elemenet
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of reference element
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nodal coordinates of interpolation points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! data written in xij
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType(:)
    !! interpolation point type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:), beta(:), lambda(:)
    !! Jacobi and Ultraspherical parameters
  END SUBROUTINE obj_GetInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetDOFValue@Methods
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractFE_Class
