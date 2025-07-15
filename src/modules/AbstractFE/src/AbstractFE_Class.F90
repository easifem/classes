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
USE BaseType, ONLY: BaseInterpolation_, &
                    BaseContinuity_, &
                    ElemShapeData_, &
                    QuadraturePoint_, &
                    LagrangeInterpolation_, &
                    OrthogonalInterpolation_, &
                    HierarchyInterpolation_, &
                    SerendipityInterpolation_, &
                    HermitInterpolation_, &
                    ReferenceElement_
USE String_Class, ONLY: String
USE AbstractRefElement_Class, ONLY: AbstractRefElement_, &
                                    AbstractRefElementPointer_
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e

USE ReferenceElement_Method, ONLY: PARAM_REFELEM_MAX_FACES, &
                                   PARAM_REFELEM_MAX_EDGES, &
                                   PARAM_REFELEM_MAX_POINTS

USE BasisOpt_Class, ONLY: BasisOpt_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

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
  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is set to true at the time of constructor
  REAL(DFP), ALLOCATABLE :: coeff(:, :)
  !! coefficient necessary for lagrange Interpolation
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  !! Interpolation points for lagrange polynomial
  !! coeff, and xij are needed internally for
  !! constructing the lagrange polynomial
  TYPE(BasisOpt_) :: opt
  !! basis options

CONTAINS

  PRIVATE

  ! CONSTRUCTOR:
  !@ConstructorMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate method from the parameter list
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate method from the parameters
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Initiate by copy
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Initiate by copy
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam

  !IO:
  !@IOMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of a finite element
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: MdEncode => obj_MdEncode
  !! Display the contents
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: ReactEncode => obj_ReactEncode
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
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order and reallocate appropriate data in
  !! already initiated AbstractFE_

  !GET:
  ! @GetMethods

  PROCEDURE(obj_GetPrefix), DEFERRED, PUBLIC, PASS(obj) :: GetPrefix
  !! Get prefix
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get local element shape data for Discontinuous Galerkin
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Get global element shape data
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTopologyType => &
    obj_GetTopologyType
  !! returns the topoType
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Sets the parameters of finite element

  ! GET:
  ! @QuadratureMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetQuadraturePoints1 => &
    obj_GetQuadraturePoints1
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetQuadraturePoints2 => &
    obj_GetQuadraturePoints2
  GENERIC, PUBLIC :: GetQuadraturePoints => GetQuadraturePoints1, &
    GetQuadraturePoints2
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

INTERFACE SetFiniteElementParam
  MODULE SUBROUTINE SetAbstractFEParam(param, prefix, nsd, elemType, &
          baseContinuity, baseInterpolation, ipType, basisType, alpha, beta, &
         lambda, order, anisoOrder, edgeOrder, faceOrder, cellOrder, fetype, &
                                       dofType, transformType)
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic Order of finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    !! Anisotropic order, order in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! Order of approximation along edges
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! Order of approximation along face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! Order of approximation along cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar
    !! For HDiv and Hcurl it should be Vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(4)
    !! Degree of freedom type, default is nodal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
  END SUBROUTINE SetAbstractFEParam
END INTERFACE SetFiniteElementParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiates an instance of the finite element

INTERFACE AbstractFEInitiate
  MODULE SUBROUTINE obj_Initiate1(obj, param)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate1
END INTERFACE AbstractFEInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiates an instance of the finite element

INTERFACE AbstractFEInitiate
  MODULE SUBROUTINE obj_Initiate2(obj, elemType, nsd, baseContinuity, &
          baseInterpolation, ipType, basisType, alpha, beta, lambda, fetype, &
                       dofType, transformType, order, anisoOrder, cellOrder, &
                   faceOrder, edgeOrder, cellOrient, faceOrient, edgeOrient, &
                     tcell, tface, tedge, errCheck, quadratureIsHomogeneous, &
                         quadratureType, quadratureOrder, quadratureIsOrder, &
                  quadratureIsOrderScalar, quadratureNips, quadratureIsNips, &
                 quadratureIsNipsScalar, quadratureAlpha, quadratureIsAlpha, &
                  quadratureIsAlphaScalar, quadratureBeta, quadratureIsBeta, &
               quadratureIsBetaScalar, quadratureLambda, quadratureIsLambda, &
                                  quadratureIsLambdaScalar)
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(4)
    !! Degree of freedom type, default is nodal
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
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(3)
    !! Quadrature type in x, y, and z directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureOrder(3)
    !! quadrature accuracy in x, y, and z direction
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Is quadrature order considered
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrderScalar
    !! Is order scalar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(3)
    !! Number of interpolation points in x, y, and z directions
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNipsScalar
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsAlpha
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsAlphaScalar
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsBeta
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsBetaScalar
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsLambda
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsLambdaScalar
  END SUBROUTINE obj_Initiate2
END INTERFACE AbstractFEInitiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE obj_LagrangeFE(obj, fetype, elemType, nsd, baseContinuity, &
           baseInterpolation, ipType, basisType, alpha, beta, lambda, order, &
                                   anisoOrder)

    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), INTENT(IN) :: fetype
    !! finite element type: scalar, vector, matrix
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Type of finite element
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity of basis function
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis function family used for interpolation.
    !! LagrangeInterpolation, LagrangePolynomial
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial. It can take following
    !! values:
    !! Legendre, Chebyshev, Ultraspherical, Equidistance, Jacobi
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Basis type:
    !! Legendre, Lobatto, Ultraspherical, Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic Order of finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
  END SUBROUTINE obj_LagrangeFE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_HierarchicalFE(obj, fetype, elemType, nsd, baseContinuity, &
                                    baseInterpolation, cellOrder, faceOrder, &
                                edgeOrder, cellOrient, faceOrient, edgeOrient)

    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), INTENT(IN) :: fetype
    !! finite element type
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Type of finite element
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity of basis function
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis function family used for interpolation.
    !! LagrangeInterpolation, LagrangePolynomial
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
  END SUBROUTINE obj_HierarchicalFE
END INTERFACE

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

INTERFACE AbstractFEDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(AbstractFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE AbstractFEDeallocate

INTERFACE DEALLOCATE
  MODULE PROCEDURE Deallocate_Ptr_Vector
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
  MODULE SUBROUTINE obj_SetParam(obj, nsd, order, anisoOrder, edgeOrder, &
                 faceOrder, cellOrder, fetype, elemType, topoType, elemIndx, &
             ipType, basisType, alpha, beta, lambda, dofType, transformType, &
         refElemDomain, baseContinuity, baseInterpolation, isIsotropicOrder, &
      isAnisotropicOrder, isEdgeOrder, isFaceOrder, isCellOrder, tEdgeOrder, &
                                 tFaceOrder, tCellOrder)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(3)
    !! order in x, y, and z directions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! order of approximation on the edges of element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! order of approximation on the faces of element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(3)
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
!                                                   GetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  This routine set order in the already initiated AbstractFE_
!
!# Introduction
!
! This routine sets order in the already initiated AbstractFE_
! Make sure the object is initiated by calling correct constructor methods
!
! This routine will call SetLagrangeOrder for LagrangeFE
! This routine will call SetHierarchicalFE for HierarchicalFE

INTERFACE
  MODULE SUBROUTINE obj_SetOrder(obj, order, anisoorder, cellOrder, &
                   faceOrder, edgeOrder, cellOrient, faceOrient, edgeOrient, &
                                 tcell, tface, tedge, errCheck)
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
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-11
! summary:  Get prefix

ABSTRACT INTERFACE
  FUNCTION obj_GetPrefix(obj) RESULT(ans)
    IMPORT :: AbstractFE_
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
  MODULE SUBROUTINE obj_GetParam(obj, nsd, order, anisoOrder, edgeOrder, &
                 faceOrder, cellOrder, fetype, elemType, topoType, elemIndx, &
                                 ipType, basisType, alpha, beta, lambda, &
                                 dofType, transformType, refElemDomain, &
                        baseContinuity, baseInterpolation, isIsotropicOrder, &
                  isAnisotropicOrder, isEdgeOrder, isFaceOrder, isCellOrder, &
                                 tEdgeOrder, tFaceOrder, tCellOrder)
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
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
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
                                                   facetElemsd, quad)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
      !! finite element
    TYPE(ElemShapedata_), INTENT(INOUT) :: cellElemsd
      !! element shape data on cell
    TYPE(ElemShapedata_), INTENT(INOUT) :: facetElemsd(:)
      !! element shapedata on facet element
      !! The size of facetElemsd should be equal to total number of
      !! facets in element.
    TYPE(QuadraturePoint_), INTENT(IN) :: quad(:)
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
    TYPE(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoelemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                     GetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get quadrature points

! obj_Initiate9(obj, elemType, domainName, order, quadratureType,&
! alpha, beta, lambda, xij)

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints1(obj, quad, quadratureType, &
                                             order, alpha, beta, lambda)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Type of quadrature points
    !! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau, GaussLegendreRadauLeft
    !! GaussLegendreRadauRight ! GaussChebyshev
    !! GaussChebyshevLobatto ! GaussChebyshevRadau, GaussChebyshevRadauLeft
    !! GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of integrand
    !! either the order or the nips should be present
    !! Both nips and order should not be present
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE obj_GetQuadraturePoints1
END INTERFACE

!----------------------------------------------------------------------------
!                               GetQuadraturePoints2@QuadraturePointsMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-11
! summary:  Getting quadrature points in anisotropic order

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints2(obj, quad, p, q, r, &
           quadratureType1, quadratureType2, quadratureType3, alpha1, beta1, &
                      lambda1, alpha2, beta2, lambda2, alpha3, beta3, lambda3)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    !! abstract finite element
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! quadrature point
    INTEGER(I4B), INTENT(IN) :: p
    !! order of integrand in x
    INTEGER(I4B), INTENT(IN) :: q
    !! order of integrand in y
    INTEGER(I4B), INTENT(IN) :: r
    !! order of integrand in z direction
    INTEGER(I4B), INTENT(IN) :: quadratureType1
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: quadratureType2
    !! Type of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType3
    !! Type of quadrature points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi parameter and Ultraspherical parameters
  END SUBROUTINE obj_GetQuadraturePoints2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTopologyType@GetMethods
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
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary:  Import data from toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, elemType)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type of finite element, optional
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml, elemType)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

END MODULE AbstractFE_Class
