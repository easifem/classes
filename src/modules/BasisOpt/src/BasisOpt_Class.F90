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

MODULE BasisOpt_Class

USE GlobalData, ONLY: I4B, DFP, LGT

USE String_Class, ONLY: String

USE BaseType, ONLY: ipopt => TypeInterpolationOpt, &
                    polyopt => TypePolynomialOpt, &
                    QuadraturePoint_, &
                    ElemShapeData_

USE QuadratureOpt_Class, ONLY: QuadratureOpt_

USE ReferenceElement_Method, ONLY: eleminfo => ReferenceElementInfo

USE FPL, ONLY: ParameterList_

USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "BasisOpt_Class"

PUBLIC :: BasisOpt_, TypeBasisOpt

!----------------------------------------------------------------------------
!                                                             BasisOpt_Class
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  This class contains the options for basis functions in
! multidimensional problems

TYPE :: BasisOpt_
  PRIVATE
  LOGICAL(LGT) :: firstCall = .TRUE.
  !! flag to check if the shape functions are constructed from scratch or not

  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is set to true at the time of constructor

  LOGICAL(LGT) :: isIsotropicOrder = .FALSE.
  !! flag to check if the order is isotropic or not
  !! see order

  LOGICAL(LGT) :: isAnisoOrder = .FALSE.
  !! flag to check if the order is anisotropic or not
  !! see anisoOrder

  LOGICAL(LGT) :: isEdgeOrder = .FALSE.
  !! True if we set the edge order

  LOGICAL(LGT) :: isFaceOrder = .FALSE.
  !! True if we set the face order

  LOGICAL(LGT) :: isCellOrder = .FALSE.
  !! True if we set the cell order

  INTEGER(I4B) :: tdof = 0
  !! total number of degrees of freedom

  INTEGER(I4B) :: nsd = 0
  !! number of spatial dimensions

  INTEGER(I4B) :: xidim = 0
  !! codimension of the element

  INTEGER(I4B) :: order = 0
  !! isotropic order of the basis function

  INTEGER(I4B) :: anisoOrder(3) = 0
  !! anisotropic order of the basis function in x, y, z direction

  INTEGER(I4B) :: edgeOrder(eleminfo%maxEdges) = 0
  !! Order on each edge of the element
  INTEGER(I4B) :: edgeOrient(eleminfo%maxEdges) = 0
  !! Orientation on each edge of the element
  INTEGER(I4B) :: tEdgeOrder = 0
  !! The actual size of edgeOrder

  INTEGER(I4B) :: faceOrder(3, eleminfo%maxFaces) = 0
  !! Order of approximation on each face of the element
  INTEGER(I4B) :: faceOrient(3, eleminfo%maxFaces) = 0
  !! orientation on each face
  INTEGER(I4B) :: tFaceOrder = 0
  !! The actual size of faceOrder

  INTEGER(I4B) :: cellOrder(3) = 0
  !! Order of approximation inside the element
  INTEGER(I4B) :: cellOrient(3) = 0
  !! Orientation of each cell
  INTEGER(I4B) :: tCellOrder = 0
  !! The actual size of cellOrder

  INTEGER(I4B) :: fetype = 0
  !! Type of finite element, Scalar, Vector, Matrix
  !! Also see fetype_char

  INTEGER(I4B) :: topoType = 0
  !! Topology type of element, also see topoType_char
  !! point, line, triangle, quadrangle, tetrahedron, hexahedron,
  !! prism, pyramid

  INTEGER(I4B) :: elemType = 0
  !! Topology type of reference element
  !! line, triangle, quadrangle, tetrahedron, hexahedron,
  !! prism, pyramid

  INTEGER(I4B) :: ipType = 0
  !! Type of lattice point for interpol (i.e., interpolation point type)
  !! It is used when ipType is LagrageInterpolation
  !! See also ipType_char

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

  INTEGER(I4B) :: basisType(3) = 0
  !! Integer code for basis type in x, y, and z direction
  !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto
  !! Ultraspherical
  !! See also basisType_char

  REAL(DFP) :: alpha(3) = 0.0_DFP
  !! Jacobi parameters
  REAL(DFP) :: beta(3) = 0.0_DFP
  !! Jacobi parameters

  REAL(DFP) :: lambda(3) = 0.5_DFP
  !! Ultraspherical parameters

  CHARACTER(1) :: refelemDomain = "B"
  !! String name for reference element domain.
  !! It can take following values:
  !! - UNIT "U"
  !! - BIUNIT "B"

  CHARACTER(2) :: baseContinuity = "H1"
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG

  CHARACTER(4) :: baseInterpolation = "LAGR"
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation, Lagrange
  !! HierarchyInterpolation, Hierarchy
  !! OrthogonalInterpolation, Orthogonal
  !! HermitInterpolation, Hermit
  !! SerendipityInterpolation, Serendipity

  REAL(DFP) :: refelemCoord(3, 8) = 0.0_DFP
  !! coordinate of reference element

  CHARACTER(4) :: topoType_char(3) = "    "
  !! basis type in string format

  CHARACTER(128) :: basisType_char(3) = "MONOMIAL"
  !! basis type in string format

  CHARACTER(128) :: ipType_char(3) = "EQUIDISTANCE"
  !! interpolation type in string format

  CHARACTER(128) :: feType_char(3) = "SCALAR"
  !! finite element type in string format

  TYPE(QuadratureOpt_) :: quadOpt
  !! Quadrature options

  TYPE(QuadraturePoint_) :: quad
  !! quadrature points

CONTAINS

  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
    !! Initiate basis options from parameters
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
    !! Initiate basis options from arguments
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2

END TYPE BasisOpt_

!----------------------------------------------------------------------------
!                                                              TypeBasisOpt
!----------------------------------------------------------------------------

TYPE(BasisOpt_), PARAMETER :: TypeBasisOpt = BasisOpt_()

!----------------------------------------------------------------------------
!                                                           BasisOptPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Container for pointer for BasisOpt_ class

TYPE :: BasisOptPointer_
  CLASS(BasisOpt_), POINTER :: ptr
  !! Pointer to BasisOpt_ class
END TYPE BasisOptPointer_

!----------------------------------------------------------------------------
CONTAINS
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!                                    CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Check essential param in parameter list

MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
  CLASS(BasisOpt_), INTENT(IN) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
END SUBROUTINE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                     SetAbstractFEParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-11
! summary:  Sets the parameters for initiating abstract finite element

SUBROUTINE SetBasisOptParam(param, prefix, nsd, elemType, &
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
END SUBROUTINE SetBasisOptParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Initiate basis options from parameters

SUBROUTINE obj_Initiate1(obj, param, prefix)
  CLASS(BasisOpt_), INTENT(inout) :: obj
  TYPE(ParameterList_), INTENT(in) :: param
  CHARACTER(*), INTENT(in) :: prefix

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "OneDimBasisOpt_Initiate()"
#endif

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Initiate basis options from arguments

SUBROUTINE obj_Initiate2(obj, elemType, nsd, baseContinuity, &
           baseInterpolation, ipType, basisType, alpha, beta, lambda, order, &
        anisoOrder, edgeOrder, faceOrder, cellOrder, edgeOrient, faceOrient, &
                         cellOrient, fetype, dofType, transformType)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
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
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! Cell orientation
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orientation
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orientation
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar
    !! For HDiv and Hcurl it should be Vector
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(4)
    !! Degree of freedom type, default is nodal
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
END SUBROUTINE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Copy basis options from one object to another

SUBROUTINE obj_Copy(obj, obj2)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  CLASS(BasisOpt_), INTENT(IN) :: obj2
END SUBROUTINE obj_Copy

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Deallocate basis options

SUBROUTINE obj_Deallocate(obj)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

SUBROUTINE Deallocate_Ptr_Vector(obj)
  TYPE(BasisOptPointer_), ALLOCATABLE :: obj(:)
END SUBROUTINE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Display the contents of basis options

SUBROUTINE obj_Display(obj, msg, unitno, notFull)
  CLASS(BasisOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull
END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

FUNCTION obj_MdEncode(obj) RESULT(ans)
  CLASS(BasisOpt_), INTENT(IN) :: obj
  TYPE(String) :: ans
END FUNCTION obj_MdEncode

!----------------------------------------------------------------------------
!                                                                ReactEncode
!----------------------------------------------------------------------------

FUNCTION obj_ReactEncode(obj) RESULT(ans)
  CLASS(BasisOpt_), INTENT(IN) :: obj
  TYPE(String) :: ans
END FUNCTION obj_ReactEncode

!----------------------------------------------------------------------------
!                                                                    SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Set the parameters

SUBROUTINE obj_SetParam(obj, nsd, order, anisoOrder, edgeOrder, &
           faceOrder, cellOrder, fetype, elemType, ipType, basisType, alpha, &
        beta, lambda, dofType, transformType, refElemDomain, baseContinuity, &
       baseInterpolation, isIsotropicOrder, isAnisotropicOrder, isEdgeOrder, &
                 isFaceOrder, isCellOrder, tEdgeOrder, tFaceOrder, tCellOrder)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
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

!----------------------------------------------------------------------------
!                                                                   SetOrder
!----------------------------------------------------------------------------

SUBROUTINE obj_SetOrder(obj, order, anisoorder, cellOrder, &
                   faceOrder, edgeOrder, cellOrient, faceOrient, edgeOrient, &
                        tcell, tface, tedge, errCheck)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
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

!----------------------------------------------------------------------------
!                                                        Set
!----------------------------------------------------------------------------

SUBROUTINE obj_GetParam(obj, nsd, order, anisoOrder, edgeOrder, &
           faceOrder, cellOrder, fetype, elemType, ipType, basisType, alpha, &
                        beta, lambda, dofType, transformType, refElemDomain, &
                        baseContinuity, baseInterpolation, isIsotropicOrder, &
                  isAnisotropicOrder, isEdgeOrder, isFaceOrder, isCellOrder, &
                        tEdgeOrder, tFaceOrder, tCellOrder)
  CLASS(BasisOpt_), INTENT(IN) :: obj
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

!----------------------------------------------------------------------------
!                                                      GetLocalElemShapeData
!----------------------------------------------------------------------------

SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
  TYPE(QuadraturePoint_), INTENT(IN) :: quad
END SUBROUTINE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                                 GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Get local facet element shape data

SUBROUTINE obj_GetLocalFacetElemShapeData(obj, cellElemsd, &
                                          facetElemsd, quad)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
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

!----------------------------------------------------------------------------
!                                                     GetGlobalElemShapeData
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Get global element shape data

SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoelemsd)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
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

!----------------------------------------------------------------------------
!                                                        GetQuadraturePoints
!----------------------------------------------------------------------------

SUBROUTINE obj_GetQuadraturePoints1(obj, quad, quadratureType, &
                                    order, alpha, beta, lambda)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
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

!----------------------------------------------------------------------------
!                                                        GetQuadraturePoints
!----------------------------------------------------------------------------

SUBROUTINE obj_GetQuadraturePoints2(obj, quad, p, q, r, &
           quadratureType1, quadratureType2, quadratureType3, alpha1, beta1, &
                      lambda1, alpha2, beta2, lambda2, alpha3, beta3, lambda3)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
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

!----------------------------------------------------------------------------
!                                                             GetTopologyType
!----------------------------------------------------------------------------

FUNCTION obj_GetTopologyType(obj) RESULT(ans)
  CLASS(BasisOpt_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
END FUNCTION obj_GetTopologyType

END MODULE BasisOpt_Class
