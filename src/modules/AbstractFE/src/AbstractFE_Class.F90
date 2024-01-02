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
USE GlobalData
USE BaseType, ONLY: BaseInterpolation_, &
  & BaseContinuity_,  &
  & ElemShapeData_,  &
  & QuadraturePoint_,  &
  & LagrangeInterpolation_,  &
  & OrthogonalInterpolation_,  &
  & HierarchyInterpolation_,  &
  & SerendipityInterpolation_,  &
  & HermitInterpolation_,  &
  & ReferenceElement_
USE String_Class, ONLY: String
USE AbstractRefElement_Class
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
PUBLIC :: AbstractFE_
PUBLIC :: AbstractFEPointer_
PUBLIC :: SetAbstractFEParam
PUBLIC :: SetFiniteElementParam
PUBLIC :: AbstractFEDeallocate
PUBLIC :: AbstractFEDisplay
PUBLIC :: AbstractFEInitiate
PUBLIC :: AbstractFECheckEssentialParam
PUBLIC :: DEALLOCATE

CHARACTER(*), PARAMETER :: modName = "AbstractFE_Class"
CHARACTER(*), PARAMETER :: AbstractFEEssentialParams = &
& "/nsd/order/anisoOrder/tEdgeOrder/edgeOrder/tFaceOrder"//  &
& "/faceOrder/cellOrder/feType/elemType/ipType/dofType"//  &
& "/transformType/refElemDomain/baseContinuity/baseInterpolation"//  &
& "/isIsotropicOrder/isAnisotropicOrder/isEdgeOrder/isFaceOrder"//  &
& "/isCellOrder/tCellOrder/basisType/alpha/beta/lambda"

INTEGER(I4B), PARAMETER :: FE_DOF_POINT_EVAL = 1_I4B
INTEGER(I4B), PARAMETER :: DEFAULT_DOF_TYPE(4) = [1, 1, 1, 1]
INTEGER(I4B), PARAMETER :: FE_TRANSFORM_IDENTITY = 1_I4B
INTEGER(I4B), PARAMETER :: DEFAULT_TRANSFORM_TYPE = 1_I4B

INTEGER(I4B), PARAMETER :: MAX_NO_FACE = 6
!! Maximum number of faces in an element
INTEGER(I4B), PARAMETER :: MAX_NO_EDGE = 12
!! Maximum number of edges in an element

!----------------------------------------------------------------------------
!                                                        AbstractRefElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Abstract class for finite element is defined
!
!{!pages/docs-api/AbstractFE/AbstractFE_.md!}

TYPE, ABSTRACT :: AbstractFE_
  PRIVATE
  LOGICAL(LGT) :: firstCall = .TRUE.
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is set to true at the time of constructor
  INTEGER(I4B) :: nsd = 0
  !! spatial dimension of fintie element
  INTEGER(I4B) :: order = 0
  !! Isotropic order of polynomial space
  LOGICAL(LGT) :: isIsotropicOrder = .FALSE.
  !! True if the order is same in all the direction
  INTEGER(I4B) :: anisoOrder(3)
  !! Order in x, y, and z direction
  LOGICAL(LGT) :: isAnisotropicOrder = .FALSE.
  !! True if the order is different in different directions
  INTEGER(I4B) :: edgeOrder(MAX_NO_EDGE) = 0
  !! Order on each edge of the element
  INTEGER(I4B) :: tEdgeOrder = 0
  !! The actual size of edgeOrder
  LOGICAL(LGT) :: isEdgeOrder = .FALSE.
  !! True if we set the edge order
  INTEGER(I4B) :: faceOrder(MAX_NO_FACE)
  !! Order of approximation on each face of the element
  INTEGER(I4B) :: tFaceOrder = 0
  !! The actual size of faceOrder
  LOGICAL(LGT) :: isFaceOrder = .FALSE.
  !! True if we set the face order
  INTEGER(I4B) :: cellOrder(3)
  !! Order of approximation inside the element
  INTEGER(I4B) :: tCellOrder = 0
  !! The actual size of cellOrder
  LOGICAL(LGT) :: isCellOrder = .FALSE.
  !! True if we set the cell order
  INTEGER(I4B) :: feType = 0
  !! Type of finite element
  !! Scalar, Vector, Matrix
  INTEGER(I4B) :: elemType = 0
  !! Topology type of reference elemtn
  !! Line, Triangle, Quadrangle, Tetrahedron, Hexahedron,
  !! Prism, Pyramid
  INTEGER(I4B) :: ipType = 0
  !! Type of lattice point (i.e., interpolation point type)
  INTEGER(I4B) :: dofType(4) = 0
  !! Currently it is not used
  !! dofType(1): Type of dof for shape function defined on vertex
  !! dofType(2): Type of dof for shape functions on edge
  !! dofType(3): Type of dof for shape functions on face
  !! dofType(4): Type of dof for shape functions in cell
  !! These shape functions can take following values:
  !! - FE_DOF_POINT_EVAL
  INTEGER(I4B) :: transformType = 0
  !! Currently it is not used
  !! Type of Tranformation usef for polynomial space
  !! - FE_TRANSFORM_IDENTITY
  TYPE(String) :: baseContinuity0
  !! String name of base continuity
  TYPE(String) :: baseInterpolation0
  !! String name of base interpolation
  !! LagrangePolynomial
  !! SerendipityPolynomial
  !! HermitPolynomial
  !! OrthogonalPolynomial
  !! HierarchyPolynomial
  INTEGER(I4B) :: basisType(3)
  !! Integer code for basis type in x, y, and z direction
  !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto
  !! Ultraspherical
  REAL(DFP) :: alpha(3)
  !!Jacobi parameters
  REAL(DFP) :: beta(3)
  !! Jacobi parameters
  REAL(DFP) :: lambda(3)
  !! Ultraspherical parameters
  TYPE(String) :: refElemDomain
  !! String name for reference element domain.
  !! It can take following values:
  !! - UNIT
  !! - BIUNIT
  CLASS(BaseContinuity_), ALLOCATABLE :: baseContinuity
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG
  CLASS(BaseInterpolation_), ALLOCATABLE :: baseInterpolation
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation
  !! HermitInterpolation
  !! SerendipityInterpolation
  !! HierarchyInterpolation
  !! OrthogonalInterpolation
  CLASS(AbstractRefElement_), POINTER :: refelem => NULL()
  !! reference element
  TYPE(ReferenceElement_) :: refelem0
  !! This is only for internal use
  !! At the time of initiate we extract refelem0 from refelem
  !! This way we do not have to make copy every time we
  !! make quadrature points and shape function data
  TYPE(ReferenceElement_) :: facetElem0(MAX_NO_FACE)
  !! Facet elements
  REAL(DFP), ALLOCATABLE :: coeff(:, :)
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  !@ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Constructor method for AbstractFE element
  !! This method can be overloaded by Subclass of this abstract class.
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Initiate by copy
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Initiate by copy
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & obj_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of a finite element
  PROCEDURE, PUBLIC, PASS(obj) :: MdEncode => obj_MdEncode
  !! Display the contents
  PROCEDURE, PUBLIC, PASS(obj) :: ReactEncode => obj_ReactEncode
  !! Display the contents
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data stored in an instance

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Sets the parameters of finite element

  !GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Get prefix
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Sets the parameters of finite element
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData =>  &
    &  obj_GetLocalElemShapeData
  PROCEDURE, PRIVATE, PASS(obj) :: GetLocalElemshapeData_H1 =>  &
    & obj_GetLocalElemShapeData_H1_Master
  !! Get local element shape data for H1
  PROCEDURE, PRIVATE, PASS(obj) :: GetLocalElemshapeData_HDiv =>  &
    & obj_GetLocalElemShapeData_HDiv_Master
  !! Get local element shape data for Hdiv
  PROCEDURE, PRIVATE, PASS(obj) :: GetLocalElemshapeData_HCurl =>  &
    & obj_GetLocalElemShapeData_HCurl_Master
  !! Get local element shape data for HCurl
  PROCEDURE, PRIVATE, PASS(obj) :: GetLocalElemshapeData_DG =>  &
    & obj_GetLocalElemShapeData_DG_Master

  ! GET:
  ! @Global element shapedata

  !! Get local element shape data for Discontinuous Galerkin
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData =>  &
    &  obj_GetGlobalElemShapeData
  PROCEDURE, PRIVATE, PASS(obj) :: GetGlobalElemshapeData_H1 =>  &
    & obj_GetGlobalElemShapeData_H1_Master
  !! Get global shape data for H1
  PROCEDURE, PRIVATE, PASS(obj) :: GetGlobalElemshapeData_HDiv =>  &
    & obj_GetGlobalElemShapeData_HDiv_Master
  !! Get global shape data for Hdiv
  PROCEDURE, PRIVATE, PASS(obj) :: GetGlobalElemshapeData_HCurl =>  &
    & obj_GetGlobalElemShapeData_HCurl_Master
  !! Get global shape data for HCurl
  PROCEDURE, PRIVATE, PASS(obj) :: GetGlobalElemshapeData_DG =>  &
    & obj_GetGlobalElemShapeData_DG_Master
  !! Get global shape data for Discontinuous Galerkin

  ! GET:
  ! @QuadratureMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints =>  &
    & obj_GetQuadraturePoints1
END TYPE AbstractFE_

!----------------------------------------------------------------------------
!                                                         AbstractFEPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractFEPointer_
  CLASS(AbstractFE_), POINTER :: ptr => NULL()
END TYPE AbstractFEPointer_

!----------------------------------------------------------------------------
!                                    CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-08-11
! summary: This routine Check the essential parameters in param.

INTERFACE AbstractFECheckEssentialParam
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE AbstractFECheckEssentialParam

!----------------------------------------------------------------------------
!                                     SetAbstractFEParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-11
! summary:  Sets the parameters for initiating abstract finite element

INTERFACE
  MODULE SUBROUTINE SetAbstractFEParam( &
    & param, prefix, nsd, elemType, baseContinuity, &
    & baseInterpolation, ipType, basisType, alpha, &
    & beta, lambda, order, anisoOrder, edgeOrder,  &
    & faceOrder, cellOrder)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! ParameterList
    CHARACTER(*), INTENT(IN) :: prefix
    !! Prefix
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Type of finite element
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Basis type: Legendre, Lobatto, Ultraspherical,
    !! Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic Order of finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Anisotropic order, order in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Order of approximation along edges
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:)
    !! Order of approximation along face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Order of approximation along cell
  END SUBROUTINE SetAbstractFEParam
END INTERFACE

INTERFACE SetFiniteElementParam
  MODULE PROCEDURE SetAbstractFEParam
END INTERFACE SetFiniteElementParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiates an instance of the finite element

INTERFACE AbstractFEInitiate
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE AbstractFEInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
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
!                                             Deallocate@ConstructorMethods
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
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(AbstractFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
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
!                                                          MdEncode@Methods
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
!                                                        SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Set the parameters

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    & obj, nsd, order, anisoOrder, edgeOrder, faceOrder, &
    & cellOrder, feType, elemType, ipType, basisType, alpha, &
    & beta, lambda, dofType, transformType, refElemDomain, &
    & baseContinuity, baseInterpolation, isIsotropicOrder,  &
    & isAnisotropicOrder, isEdgeOrder, isFaceOrder, isCellOrder, &
    & tEdgeOrder, tFaceOrder, tCellOrder)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(3)
    !! order in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! order of approximation on the edges of element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:)
    !! order of approximation on the faces of element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(3)
    !! order of approximation in the cell of element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
    !! finite element type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Reference element type
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(4)
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFaceOrder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isCellOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdgeOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFaceOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCellOrder
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-11
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Get the parameters

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    & obj, nsd, order, anisoOrder, edgeOrder, faceOrder, &
    & cellOrder, feType, elemType, ipType, basisType, alpha, &
    & beta, lambda, dofType, transformType, refElemDomain, &
    & baseContinuity, baseInterpolation, isIsotropicOrder,  &
    & isAnisotropicOrder, isEdgeOrder, isFaceOrder, isCellOrder, &
    & tEdgeOrder, tFaceOrder, tCellOrder)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: anisoOrder(3)
    !! order in x, y, and z directions
    INTEGER(I4B), OPTIONAL, ALLOCATABLE, INTENT(OUT) :: edgeOrder(:)
    !! order of approximation on the edges of element
    INTEGER(I4B), OPTIONAL, ALLOCATABLE, INTENT(OUT) :: faceOrder(:)
    !! order of approximation on the faces of element
    INTEGER(I4B), OPTIONAL, ALLOCATABLE, INTENT(OUT) :: cellOrder(:)
    !! order of approximation in the cell of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: feType
    !! finite element type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemType
    !! Reference element type
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
    TYPE(String), OPTIONAL, INTENT(OUT) :: baseContinuity
    !! String name of type of continuity used for basis functions
    TYPE(String), OPTIONAL, INTENT(OUT) :: baseInterpolation
    !! String name of type of interpolation used for basis functions
    TYPE(String), OPTIONAL, INTENT(OUT) :: refElemDomain
    !! Domain of reference element
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isIsotropicOrder
    !! True if isotropic order
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isAnisotropicOrder
    !! True if anisoOrder
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isEdgeOrder
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isFaceOrder
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isCellOrder
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tEdgeOrder
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tFaceOrder
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tCellOrder
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data on facets

INTERFACE
  MODULE SUBROUTINE obj_GetLocalFacetElemShapeData(obj, cellElemsd, &
    & facetElemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
      !! finite element
    CLASS(ElemShapedata_), INTENT(INOUT) :: cellElemsd
      !! element shape data on cell
    CLASS(ElemShapedata_), INTENT(INOUT) :: facetElemsd(:)
      !! element shapedata on facet element
      !! The size of facetElemsd should be equal to total number of
      !! facets in element.
    CLASS(QuadraturePoint_), INTENT(IN) :: quad(:)
      !! Quadrature points on each facet element
  END SUBROUTINE obj_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoElemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    CLASS(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoElemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetLocalElemShapeData_H1@H1Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemshapeData_H1_Master(obj, elemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemshapeData_H1_Master
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@H1Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData_H1_Master(obj, elemsd,   &
    & xij, geoElemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    CLASS(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoElemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData_H1_Master
END INTERFACE

!----------------------------------------------------------------------------
!                                    GetLocalElemShapeData_HDiv@HDivMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData_HDiv_Master(obj, elemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData_HDiv_Master
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetGlobalElemShapeData@HDivMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData_HDiv_Master(obj, elemsd, &
    & xij, geoElemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    CLASS(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoElemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData_HDiv_Master
END INTERFACE

!----------------------------------------------------------------------------
!                                   GetLocalElemShapeData_HCurl@HCurlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData_HCurl_Master(obj, elemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData_HCurl_Master
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetGlobalElemShapeData@HCurlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData_HCurl_Master(obj, elemsd, &
    & xij, geoElemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    CLASS(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoElemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData_HCurl_Master
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetLocalElemShapeData_DG@DGMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData_DG_Master(obj, elemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData_DG_Master
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@DGMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get Global element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData_DG_Master(obj, elemsd, &
    & xij, geoElemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! Abstract finite element
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    CLASS(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoElemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData_DG_Master
END INTERFACE

!----------------------------------------------------------------------------
!                                     GetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get quadrature points

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints1(obj, quad, quadratureType,  &
    & order, nips, alpha, beta, lambda)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType(:)
    !! Type of quadrature points
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussLegendreRadau, GaussLegendreRadauLeft
    !! GaussLegendreRadauRight
    !! GaussChebyshev
    !! GaussChebyshevLobatto
    !! GaussChebyshevRadau, GaussChebyshevRadauLeft
    !! GaussChebyshevRadauRight
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    !! Order of integrand
    !! either the order or the nips should be present
    !! Both nips and order should not be present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nips(:)
    !! Number of integration points required
    !! Either order or nips should be present
    !! Both nips and order should not be present
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameter
  END SUBROUTINE obj_GetQuadraturePoints1
END INTERFACE

END MODULE AbstractFE_Class
