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
!

MODULE AbstractFE_Class
USE GlobalData
USE BaseType, ONLY: BaseInterpolation_, &
& BaseContinuity_, ElemShapeData_
USE String_Class, ONLY: String
USE AbstractRefElement_Class
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
PUBLIC :: AbstractFE_
PUBLIC :: AbstractFEPointer_
PUBLIC :: SetAbstractFEParam
PUBLIC :: SetFiniteElementParam
PUBLIC :: AbstractFEDeallocate
PUBLIC :: AbstractFEDisplay

CHARACTER(*), PARAMETER :: modName = "AbstractFE_Class"
CHARACTER(*), PARAMETER :: myprefix = "AbstractFE"

INTEGER(I4B), PARAMETER :: FE_DOF_POINT_EVAL = 1_I4B
INTEGER(I4B), PARAMETER :: DEFAULT_DOF_TYPE(4) = [1, 1, 1, 1]
INTEGER(I4B), PARAMETER :: FE_TRANSFORM_IDENTITY = 1_I4B
INTEGER(I4B), PARAMETER :: DEFAULT_TRANSFORM_TYPE = 1_I4B

INTEGER(I4B), PARAMETER :: MAX_NO_FACE = 30
!! Maximum number of faces in an element
INTEGER(I4B), PARAMETER :: MAX_NO_EDGE = 20
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
  CLASS(AbstractRefElement_), POINTER :: refelem => NULL()
  !! reference element
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
  TYPE(String) :: baseInterpol0
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
  CLASS(BaseInterpolation_), ALLOCATABLE :: baseInterpol
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation
  !! HermitInterpolation
  !! SerendipityInterpolation
  !! HierarchyInterpolation
  !! OrthogonalInterpolation
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => fe_Initiate
  !! Constructor method for AbstractFE element
  !! This method can be overloaded by Subclass of this abstract class.
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
  & fe_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Display => fe_Display
  !! Display the content of a finite element
  PROCEDURE, PUBLIC, PASS(obj) :: MdEncode => fe_MdEncode
  !! Display the contents
  PROCEDURE, PUBLIC, PASS(obj) :: ReactEncode => fe_ReactEncode
  !! Display the contents
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => fe_Deallocate
  !! Deallocate the data stored in an instance
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => fe_SetParam
  !! Sets the parameters of finite element
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => fe_GetParam
  !! Sets the parameters of finite element
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData =>  &
    &  fe_GetLocalElemShapeData
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemshapeData_H1 =>  &
    & fe_GetLocalElemShapeData_H1
  !! Get local element shape data for H1
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemshapeData_HDiv =>  &
    & fe_GetLocalElemShapeData_HDiv
  !! Get local element shape data for Hdiv
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemshapeData_HCurl =>  &
    & fe_GetLocalElemShapeData_HCurl
  !! Get local element shape data for HCurl
END TYPE AbstractFE_

!----------------------------------------------------------------------------
!                                                         AbstractFEPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractFEPointer_
  CLASS(AbstractFE_), POINTER :: ptr => NULL()
END TYPE AbstractFEPointer_

!----------------------------------------------------------------------------
!                                               checkEssentialParam@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-08-11
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE fe_checkEssentialParam(obj, param)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE fe_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     SetAbstractFEParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-11
! summary:  Sets the parameters for initiating abstract finite element

INTERFACE SetFiniteElementParam
  MODULE SUBROUTINE SetAbstractFEParam( &
    & param, &
    & nsd, &
    & elemType, &
    & baseContinuity, &
    & baseInterpol, &
    & ipType, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda, &
    & order,  &
    & anisoOrder,  &
    & edgeOrder,  &
    & faceOrder,  &
    & cellOrder)
    TYPE(ParameterList_) :: param
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
    CHARACTER(*), INTENT(IN) :: baseInterpol
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
END INTERFACE SetFiniteElementParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiates an instance of the finite element

INTERFACE
  MODULE SUBROUTINE fe_Initiate(obj, param)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE fe_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:         Deallocate the data

INTERFACE AbstractFEDeallocate
  MODULE SUBROUTINE fe_Deallocate(obj)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
  END SUBROUTINE fe_Deallocate
END INTERFACE AbstractFEDeallocate

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-14
! summary: Display the content

INTERFACE AbstractFEDisplay
  MODULE SUBROUTINE fe_Display(obj, msg, unitno, notFull)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull
  END SUBROUTINE fe_Display
END INTERFACE AbstractFEDisplay

!----------------------------------------------------------------------------
!                                                          MdEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the contents in mardown format

INTERFACE
  MODULE FUNCTION fe_MdEncode(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION fe_MdEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                        ReactEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the reference elements in react components

INTERFACE
  MODULE FUNCTION fe_ReactEncode(obj) RESULT(ans)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION fe_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Set the parameters

INTERFACE
  MODULE SUBROUTINE fe_SetParam( &
    & obj, &
    & nsd, &
    & order, &
    & anisoOrder, &
    & edgeOrder, &
    & faceOrder, &
    & cellOrder, &
    & feType, &
    & elemType, &
    & ipType, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda, &
    & dofType, &
    & transformType, &
    & refElemDomain, &
    & baseContinuity, &
    & baseInterpol, &
    & isIsotropicOrder,  &
    & isAnisotropicOrder,  &
    & isEdgeOrder, &
    & isFaceOrder,  &
    & isCellOrder, &
    & tEdgeOrder,  &
    & tFaceOrder,  &
    & tCellOrder)
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
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpol
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
  END SUBROUTINE fe_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Set the parameters

INTERFACE
  MODULE SUBROUTINE fe_GetParam( &
    & obj, &
    & nsd, &
    & order, &
    & anisoOrder, &
    & edgeOrder, &
    & faceOrder, &
    & cellOrder, &
    & feType, &
    & elemType, &
    & ipType, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda, &
    & dofType, &
    & transformType, &
    & refElemDomain, &
    & baseContinuity, &
    & baseInterpol, &
    & isIsotropicOrder,  &
    & isAnisotropicOrder,  &
    & isEdgeOrder, &
    & isFaceOrder,  &
    & isCellOrder, &
    & tEdgeOrder,  &
    & tFaceOrder,  &
    & tCellOrder)
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
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: cellOrder(3)
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
    TYPE(String), OPTIONAL, INTENT(OUT) :: baseInterpol
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
  END SUBROUTINE fe_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetLocalElemShapeData@H1Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local shape data

INTERFACE
  MODULE SUBROUTINE fe_GetLocalElemShapeData(obj, elemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  END SUBROUTINE fe_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetLocalElemShapeData_H1@H1Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local shape data

INTERFACE
  MODULE SUBROUTINE fe_GetLocalElemShapeData_H1(obj, elemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  END SUBROUTINE fe_GetLocalElemShapeData_H1
END INTERFACE

!----------------------------------------------------------------------------
!                                    GetLocalElemShapeData_HDiv@HDivMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local shape data

INTERFACE
  MODULE SUBROUTINE fe_GetLocalElemShapeData_HDiv(obj, elemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  END SUBROUTINE fe_GetLocalElemShapeData_HDiv
END INTERFACE

!----------------------------------------------------------------------------
!                                   GetLocalElemShapeData_HCurl@HCurlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local shape data

INTERFACE
  MODULE SUBROUTINE fe_GetLocalElemShapeData_HCurl(obj, elemsd)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  END SUBROUTINE fe_GetLocalElemShapeData_HCurl
END INTERFACE

END MODULE AbstractFE_Class
