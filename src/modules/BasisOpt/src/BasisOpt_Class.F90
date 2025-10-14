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
                    fevaropt => TypeFEVariableOpt, &
                    elemnameopt => TypeElemNameOpt, &
                    QuadraturePoint_, &
                    ElemShapeData_
USE QuadratureOpt_Class, ONLY: QuadratureOpt_
USE ReferenceElement_Method, ONLY: eleminfo => ReferenceElementInfo
USE ExceptionHandler_Class, ONLY: e
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "BasisOpt_Class"
INTEGER(I4B), PARAMETER :: FE_DOF_POINT_EVAL = 1_I4B
INTEGER(I4B), PARAMETER :: DEFAULT_DOF_TYPE(4) = [1, 1, 1, 1]
INTEGER(I4B), PARAMETER :: FE_TRANSFORM_IDENTITY = 1_I4B
INTEGER(I4B), PARAMETER :: DEFAULT_TRANSFORM_TYPE = 1_I4B

PUBLIC :: BasisOpt_
PUBLIC :: TypeBasisOpt

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

  LOGICAL(LGT) :: isAnisotropicOrder = .FALSE.
  !! flag to check if the order is anisotropic or not
  !! see anisoOrder

  LOGICAL(LGT) :: isIpType = .FALSE.
  !! True if we set the edge order

  LOGICAL(LGT) :: isEdgeOrder = .FALSE.
  !! True if we set the edge order

  LOGICAL(LGT) :: isFaceOrder = .FALSE.
  !! True if we set the face order

  LOGICAL(LGT) :: isCellOrder = .FALSE.
  !! True if we set the cell order

  LOGICAL(LGT) :: isCellOrient = .FALSE.
  !! True if we set the cell orientation

  LOGICAL(LGT) :: isFaceOrient = .FALSE.
  !! True if we set the face orientation

  LOGICAL(LGT) :: isEdgeOrient = .FALSE.
  !! True if we set the edge orientation

  INTEGER(I4B) :: tdof = 0
  !! total number of degrees of freedom

  INTEGER(I4B) :: nsd = 0
  !! number of spatial dimensions

  INTEGER(I4B) :: xidim = 0
  !! codimension of the element

  INTEGER(I4B) :: topoType = 0
  !! Topology type of element, also see topoType_char
  !! point, line, triangle, quadrangle, tetrahedron, hexahedron,
  !! prism, pyramid

  INTEGER(I4B) :: elemType = 0
  !! Topology type of reference element
  !! line, triangle, quadrangle, tetrahedron, hexahedron,
  !! prism, pyramid

  INTEGER(I4B) :: elemIndx = 0
  !! Element index of topoType

  INTEGER(I4B) :: feType = fevaropt%Scalar
  !! Type of finite element, Scalar, Vector, Matrix
  !! Also see fetype_char

  INTEGER(I4B) :: tEdgeOrder = 0
  !! The actual size of edgeOrder
  INTEGER(I4B) :: tFaceOrder = 0
  !! The actual size of faceOrder
  INTEGER(I4B) :: tCellOrder = 0
  !! The actual size of cellOrder

  INTEGER(I4B) :: transformType = 0
  !! Currently it is not used
  !! Type of Tranformation usef for polynomial space
  !! - FE_TRANSFORM_IDENTITY

  INTEGER(I4B) :: ipType = ipopt%Equidistance
  !! Type of lattice point for interpol (i.e., interpolation point type)
  !! It is used when ipType is LagrageInterpolation
  !! See also ipType_char

  INTEGER(I4B) :: order = 0
  !! isotropic order of the basis function

  INTEGER(I4B) :: anisoOrder(3) = 0
  !! anisotropic order of the basis function in x, y, z direction

  INTEGER(I4B) :: edgeOrder(eleminfo%maxEdges) = 0
  !! Order on each edge of the element
  INTEGER(I4B) :: edgeOrient(eleminfo%maxEdges) = 0
  !! Orientation on each edge of the element

  INTEGER(I4B) :: faceOrder(3, eleminfo%maxFaces) = 0
  !! Order of approximation on each face of the element
  INTEGER(I4B) :: faceOrient(3, eleminfo%maxFaces) = 0
  !! orientation on each face

  INTEGER(I4B) :: cellOrder(3) = 0
  !! Order of approximation inside the element
  INTEGER(I4B) :: cellOrient(3) = 0
  !! Orientation of each cell

  INTEGER(I4B) :: dofType(4) = DEFAULT_DOF_TYPE
  !! Currently it is not used
  !! dofType(1): Type of dof for shape function defined on vertex
  !! dofType(2): Type of dof for shape functions on edge
  !! dofType(3): Type of dof for shape functions on face
  !! dofType(4): Type of dof for shape functions in cell
  !! These shape functions can take following values:
  !! - FE_DOF_POINT_EVAL

  INTEGER(I4B) :: basisType(3) = polyopt%Monomial
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

  CHARACTER(128) :: basisType_char(3) = "MONOMIAL"
  !! basis type in string format

  CHARACTER(128) :: ipType_char = "EQUIDISTANCE"
  !! interpolation type in string format

  CHARACTER(6) :: feType_char = "SCALAR"
  !! finite element type in string format
  !! scalar, vector, matrix

  TYPE(QuadratureOpt_) :: quadOpt
  !! Quadrature options

  REAL(DFP), ALLOCATABLE :: coeff(:, :)
  !! coefficient necessary for lagrange Interpolation
  !! coefficient matrix needed for Lagrange interpolation
  !! Coeff helps us in reducing the computation time for Lagrange polynomials

  REAL(DFP), ALLOCATABLE :: xij(:, :)
  !! Interpolation points used for Lagrange interpolation
  !! It is internal variables used in GetLocalElemShapeData

  REAL(DFP), ALLOCATABLE :: temp(:, :, :)
  !! temporary array used in case of Lagrange Interpolation
  !! It is internal variables used in GetLocalElemShapeData

CONTAINS

  ! CONSTRUCTOR:
  !@ConstructorMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Deallocate the data stored in an instance
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate method from the parameters
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
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml1 => &
    obj_ImportFromToml1
  !! Import from toml
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Import from toml file

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Sets the parameters of finite element
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order and reallocate appropriate data in
  !! already initiated AbstractFE_
  PROCEDURE, PUBLIC, PASS(obj) :: SetCellOrder => obj_SetCellOrder
  !! Set the cell order
  PROCEDURE, PUBLIC, PASS(obj) :: SetFaceOrder => obj_SetFaceOrder
  !! Set the face order
  PROCEDURE, PUBLIC, PASS(obj) :: SetEdgeOrder => obj_SetEdgeOrder
  !! Set the edge order
  PROCEDURE, PUBLIC, PASS(obj) :: SetIsotropicOrder => obj_SetIsotropicOrder
  !! Set the isotropic order
  PROCEDURE, PUBLIC, PASS(obj) :: SetAnisotropicOrder => &
    obj_SetAnisotropicOrder
  !! Set the anisotropic order
  PROCEDURE, PASS(obj) :: ResetAnisotropicOrder => &
    obj_ResetAnisotropicOrder
  !! Reset anisotropic order to factory settings
  PROCEDURE, PASS(obj) :: ResetIsotropicOrder => &
    obj_ResetIsotropicOrder
  !! Reset isotropic order ot factory settings
  PROCEDURE, PASS(obj) :: SetLagrangeOrder => &
    obj_SetLagrangeOrder
  !! Set the order of Lagrange finite elements, this is a private method
  PROCEDURE, PASS(obj) :: SetHierarchicalOrder => &
    obj_SetHierarchicalOrder
  !! Set the order of Hierarchical finite elements, private method
  PROCEDURE, PASS(obj) :: SetTotalDOF => obj_SetTotalDOF
  !! Set the total number of degrees of freedom
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set order of quadrature points
  PROCEDURE, PUBLIC, PASS(obj) :: Line_SetQuadratureOrder
  !! Set quadrature order on line element
  PROCEDURE, PUBLIC, PASS(obj) :: Triangle_SetQuadratureOrder
  !! Set quadrature order on triangle element
  PROCEDURE, PUBLIC, PASS(obj) :: Quadrangle_SetQuadratureOrder
  !! Set quadrature order on Quadrangle element
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set quadrature type
  PROCEDURE, PUBLIC, PASS(obj) :: Line_SetQuadratureType
  !! Set quadrature type on line element
  PROCEDURE, PUBLIC, PASS(obj) :: Triangle_SetQuadratureType
  !! Set quadrature type on Triangle element
  PROCEDURE, PUBLIC, PASS(obj) :: Quadrangle_SetQuadratureType
  !! Set quadrature type on Quadrangle element
  PROCEDURE, PUBLIC, PASS(obj) :: SetCellOrientation => obj_SetCellOrientation
  !! Set the cell orientation
  PROCEDURE, PUBLIC, PASS(obj) :: SetFaceOrientation => &
    obj_SetFaceOrientation
  !! Set the facet orientation
  PROCEDURE, PUBLIC, PASS(obj) :: SetEdgeOrientation => &
    obj_SetEdgeOrientation
  !! Set the Edge orientation

  !GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetRefElemCoord => obj_GetRefElemCoord
  !! Get the reference element coordiantes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalDOF => obj_GetTotalDOF
  !! Get the total number of degrees of freedom

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get local element shape data for Discontinuous Galerkin

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetLocalFacetElemShapeData => obj_GetLocalFacetElemShapeData
  !! Get local element shape data for Discontinuous Galerkin

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
  !! Get global element shape data

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTopologyType => &
    obj_GetTopologyType
  !! returns the topoType
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Sets the parameters of finite element

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Get quadrature points

  PROCEDURE, PUBLIC, PASS(obj) :: Line_GetQuadraturePoints
  !! Get quadrature points for line element

  PROCEDURE, PUBLIC, PASS(obj) :: Triangle_GetQuadraturePoints
  !! Get quadrature points for Triangle element

  PROCEDURE, PUBLIC, PASS(obj) :: Quadrangle_GetQuadraturePoints
  !! Get quadrature points for Quadrangle element

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetFacetQuadraturePoints => obj_GetFacetQuadraturePoints
  !! Get quadrature points on the facet

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetTotalQuadraturePoints => obj_GetTotalQuadraturePoints

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Get the base interpolation

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseContinuity => &
    obj_GetBaseContinuity
  !! Get the base continuity

  !@ LineH1LagrangeFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1LagFE_GetLocalElemShapeData
  !! Get local element shape data for LineH1LagrangeFE
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1LagFE_SetOrder
  !! Set the order of Quadrature points
  PROCEDURE, PUBLIC, PASS(obj) :: LineH1LagFE_GetGlobalElemShapeData
  !! Get global element shape data for LineH1LagrangeFE

  !@ TriangleH1LagrangeFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1LagFE_GetLocalElemShapeData
  !! Get local element shape data for TriangleH1LagrangeFE
  PROCEDURE, PUBLIC, PASS(obj) :: TriangleH1LagFE_SetOrder
  !! Set the order of Quadrature points

  !@ QuadrangleH1LagrangeFEMethods
  PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1LagFE_GetLocalElemShapeData
  !! Get local element shape data for QuadrangleH1LagrangeFE
  PROCEDURE, PUBLIC, PASS(obj) :: QuadrangleH1LagFE_SetOrder
  !! Set the order of Quadrature points

END TYPE BasisOpt_

!----------------------------------------------------------------------------
!                                                              TypeBasisOpt
!----------------------------------------------------------------------------

TYPE(BasisOpt_), PARAMETER :: TypeBasisOpt = BasisOpt_( &
                              xij=NULL(), temp=NULL(), coeff=NULL())

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
!                                  CheckErrorHierarchical@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Sanity check for Hierarchical finite element

INTERFACE
  MODULE SUBROUTINE obj_CheckErrorHierarchical(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_CheckErrorHierarchical
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Initiate basis options from arguments

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, elemType, nsd, baseContinuity, baseInterpolation, ipType, &
    basisType, alpha, beta, lambda, feType, dofType, transformType, order, &
    anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, faceOrient, &
    edgeOrient, tcell, tface, tedge, errCheck, quadratureIsHomogeneous, &
    quadratureType, quadratureOrder, quadratureIsOrder, quadratureNips, &
    quadratureIsNips, quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Finite element object
    INTEGER(I4B), INTENT(IN) :: elemType
    !! type of finite element for geometry
    !! Line, Triangle, Quadrangle, Tetrahedron, Prism, Pyramid,
    !! Hexahedron
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Number of spatial dimension
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity or Conformity of basis function.
    !! H1* (default), HDiv, HCurl, DG
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis function family used for interpolation.
    !! LagrangeInterpolation, LagrangePolynomial
    !! SerendipityInterpolation, SerendipityPolynomial
    !! HierarchyInterpolation, HierarchyPolynomial
    !! OrthogonalInterpolation, OrthogonalPolynomial
    !! HermitInterpolation, HermitPolynomial
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpolation is Lagrange
    !! Default ipType is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
    !! Basis type is used when baseInterpolation is
    !! Lagrange polynomial, it can take the following values:
    !! Legendre, Lobatto, Ultraspherical, Jacobi, Monomial
    !! Default is Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    !! Jacobi parameter, it is needed for lagrange interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
    !! Finite element type; Default is Scalar
    !! For HDiv and Hcurl it should be Vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(:)
    !! Degree of freedom type, default is nodal
    !! currently it is not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
    !! currently it is not used
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
    !! Should we consider quadratureNips
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Copy basis options from one object to another

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    CLASS(BasisOpt_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Deallocate basis options

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

INTERFACE DEALLOCATE
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(BasisOptPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Display the contents of basis options

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno, notFull)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         MdEncode@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_MdEncode(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_MdEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ReactEncode@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_ReactEncode(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Set the parameters

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, nsd, xidim, order, anisoOrder, edgeOrder, faceOrder, cellOrder, &
    fetype, elemType, topoType, elemIndx, ipType, basisType, alpha, beta, &
    lambda, dofType, transformType, refElemDomain, refelemCoord, &
    baseContinuity, baseInterpolation, isIsotropicOrder, isAnisotropicOrder, &
  isEdgeOrder, isFaceOrder, isCellOrder, tEdgeOrder, tFaceOrder, tCellOrder, &
    quadratureIsHomogeneous, quadratureType, quadratureType1, &
    quadratureType2, quadratureType3, quadratureOrder, quadratureOrder1, &
    quadratureOrder2, quadratureOrder3, quadratureIsOrder, quadratureNips, &
    quadratureNips1, quadratureNips2, quadratureNips3, quadratureIsNips, &
    quadratureAlpha, quadratureAlpha1, quadratureAlpha2, quadratureAlpha3, &
    quadratureBeta, quadratureBeta1, quadratureBeta2, quadratureBeta3, &
    quadratureLambda, quadratureLambda1, quadratureLambda2, quadratureLambda3)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of spatial dimension
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidim
    !! Xidimension of the element
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
    !! Element index of topoType
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
    REAL(DFP), INTENT(IN), OPTIONAL :: refelemCoord(:, :)
    !! Reference element coordinates
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
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips(:)
    !! Number of interpolation points in x, y, and z directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips1
    !! Number of interpolation points in x, y, and z directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips2
    !! Number of interpolation points in x, y, and z directions
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureNips3
    !! Number of interpolation points in x, y, and z directions
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Should we consider nips
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha(:)
    !! Jacobi parameters for quadrature
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha1
    !! Jacobi parameters for quadrature
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha2
    !! Jacobi parameters for quadrature
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureAlpha3
    !! Jacobi parameters for quadrature
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta1
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta2
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureBeta3
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda1
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda2
    REAL(DFP), INTENT(IN), OPTIONAL :: quadratureLambda3
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                              SetCellOrientation@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set the cell orientation

INTERFACE
  MODULE SUBROUTINE obj_SetCellOrientation(obj, cellOrient, tCell, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! cell orient, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! size of cellOrder, necessary for Hierarchical interpolation
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetCellOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetFaceOrientation@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary:  Set the face orientation

INTERFACE
  MODULE SUBROUTINE obj_SetFaceOrientation(obj, faceOrient, tFace, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient, necessary for Hierarchical interpolation
    !! Size(faceOrient, 1) should be 3
    !! Size(faceOrient, 2) should be at least tFace
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! size of cellOrder, necessary for Hierarchical interpolation
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetFaceOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                              SetEdgeOrientation@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary:  Set the edge orientation

INTERFACE
  MODULE SUBROUTINE obj_SetEdgeOrientation(obj, edgeOrient, tEdge, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orient, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! size of edgeOrder, necessary for Hierarchical interpolation
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetEdgeOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary:  Set the order and orientation in finite element

INTERFACE
  MODULE SUBROUTINE obj_SetOrder( &
    obj, order, anisoOrder, cellOrder, faceOrder, edgeOrder, cellOrient, &
    faceOrient, edgeOrient, tcell, tface, tedge, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! abstract finite element
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
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetCellOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set the cell order

INTERFACE
  MODULE SUBROUTINE obj_SetCellOrder(obj, cellOrder, tCell, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell orient, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! size of cellOrder, necessary for Hierarchical interpolation
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetFaceOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set the face order

INTERFACE
  MODULE SUBROUTINE obj_SetFaceOrder(obj, faceOrder, tFace, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! cell orient, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! size of cellOrder, necessary for Hierarchical interpolation
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetFaceOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetEdgeOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Set the edge order

INTERFACE
  MODULE SUBROUTINE obj_SetEdgeOrder(obj, edgeOrder, tEdge, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! abstract finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! cell orient, necessary for Hierarchical interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! size of cellOrder, necessary for Hierarchical interpolation
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetEdgeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetIsotropicOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Set isotropic order
!
!# Introduction
!
! This routine is needed for Lagrage finite element

INTERFACE
  MODULE SUBROUTINE obj_SetIsotropicOrder(obj, order, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
  END SUBROUTINE obj_SetIsotropicOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetIsotropicOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Set isotropic order
!
!# Introduction
!
! This routine is needed for Lagrange finite element

INTERFACE
  MODULE SUBROUTINE obj_SetAnisotropicOrder(obj, anisoOrder, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
  END SUBROUTINE obj_SetAnisotropicOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                          Line_SetQuadratureOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the order of accuracy for line elements

INTERFACE
  MODULE SUBROUTINE Line_SetQuadratureOrder(obj, order)
    CLASS(BasisOpt_), INTENT(inout) :: obj
    INTEGER(I4B), INTENT(IN) :: order
  END SUBROUTINE Line_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                      Triangle_SetQuadratureOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the order of accuracy for Triangle elements

INTERFACE
  MODULE SUBROUTINE Triangle_SetQuadratureOrder(obj, order)
    CLASS(BasisOpt_), INTENT(inout) :: obj
    INTEGER(I4B), INTENT(IN) :: order
  END SUBROUTINE Triangle_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                    Quadrangle_SetQuadratureOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the order of accuracy for Quadrangle elements

INTERFACE
  MODULE SUBROUTINE Quadrangle_SetQuadratureOrder(obj, order, order1, order2)
    CLASS(BasisOpt_), INTENT(inout) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2
  END SUBROUTINE Quadrangle_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                           ResetAnisotropicOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary:  Reset anisotropic order to factory settings

INTERFACE
  MODULE SUBROUTINE obj_ResetAnisotropicOrder(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
  !! Basis options
  END SUBROUTINE obj_ResetAnisotropicOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                           ResetIsotropicOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary:  Reset isotropic order to factory settings

INTERFACE
  MODULE SUBROUTINE obj_ResetIsotropicOrder(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
  !! Basis options
  END SUBROUTINE obj_ResetIsotropicOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetLagrangeOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-11
! summary:  Set the order of Lagrange finite elements

INTERFACE
  MODULE SUBROUTINE obj_SetLagrangeOrder(obj, order, anisoOrder, errCheck)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! user can ignore this option
    !! for dev: this option checks the errors in debug mode
  END SUBROUTINE obj_SetLagrangeOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                    SetHierarchicalOrder@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-11
! summary:  Set the order in hierarchical basis functions

INTERFACE
  MODULE SUBROUTINE obj_SetHierarchicalOrder(obj, cellOrder, faceOrder, &
                                             edgeOrder, cellOrient, &
                                             faceOrient, edgeOrient, &
                                             errCheck, tcell, tface, tedge)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! cell orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! eddge orient
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Check the eror in debug mode
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! size of cellOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! number of columns in faceOrder
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! size of edgeorder
  END SUBROUTINE obj_SetHierarchicalOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetTotalDOF@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary:  Set the total degree of freedom from the order

INTERFACE
  MODULE SUBROUTINE obj_SetTotalDOF(obj)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
  !! Basis options
  END SUBROUTINE obj_SetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the order for quadrature

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureOrder(obj, order, order1, order2, order3)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order3
  END SUBROUTINE obj_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the quadrature type

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureType( &
    obj, quadratureType, quadratureType1, quadratureType2, quadratureType3)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
  END SUBROUTINE obj_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                           Line_SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the quadrature type on line element

INTERFACE
  MODULE SUBROUTINE Line_SetQuadratureType(obj, quadratureType)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: quadratureType
  END SUBROUTINE Line_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                       Triangle_SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the quadrature type on Triangle element

INTERFACE
  MODULE SUBROUTINE Triangle_SetQuadratureType(obj, quadratureType)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: quadratureType
  END SUBROUTINE Triangle_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                     Quadrangle_SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the quadrature type on Quadrangle element

INTERFACE
  MODULE SUBROUTINE Quadrangle_SetQuadratureType( &
    obj, quadratureType, quadratureType1, quadratureType2)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
  END SUBROUTINE Quadrangle_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetRefElemCoord@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary: Get the reference element coordinates

INTERFACE
  MODULE SUBROUTINE obj_GetRefElemCoord(obj, ans, nrow, ncol)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow
    INTEGER(I4B), INTENT(OUT) :: ncol
  END SUBROUTINE obj_GetRefElemCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetParam@GetMethods
!----------------------------------------------------------------------------

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
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: topoType
    !! Get topology of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elemIndx
    !! Get the index of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ipType
    !! interpolation point type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: basisType(:)
    !! Basis type in x, y, and z directions
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta(:)
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda(:)
    !! Ultraspherical parameter
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: dofType(:)
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
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureType(:)
    !! see QuadratureOpt_
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureOrder(:)
    !! See QuadratureOpt_
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureNips(:)
    !! See QuadratureOpt_
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureAlpha(:)
    !! See QuadratureOpt_
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureBeta(:)
    !! See QuadratureOpt_
    REAL(DFP), INTENT(OUT), OPTIONAL :: quadratureLambda(:)
    !! See QuadratureOpt_
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsOrder
    !! See QuadratureOpt_
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: quadratureIsNips
    !! See QuadratureOpt_
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetLocalFacetElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Get local facet element shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalFacetElemShapeData(obj, elemsd, facetElemsd, &
                                                   quad, facetQuad, &
                                                   localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! finite element
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd, facetElemsd
    !! element shape data on cell
    TYPE(QuadraturePoint_), INTENT(IN) :: quad, facetQuad
    !! Quadrature points on each facet element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
  END SUBROUTINE obj_GetLocalFacetElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalElemShapeData@GetMethhods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Get global element shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, xij, elemsd, geoelemsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Abstract finite element
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetQuadraturePoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary:  Get the quadrature points for the basis functions
!
!# Introduction
!   You get the quadrature poitns based on the current state of the object
! You can change the state of the object by calling SetParam method

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetFacetQuadraturePoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary: Get quadrature points on a local face of element

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                 localFaceNumber)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    !! Quadrature points
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
  END SUBROUTINE obj_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetTotalQuadraturePoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary:  Get total quadrature points for the basis functions
!
!# Introduction
!   You get the quadrature poitns based on the current state of the object
! You can change the state of the object by calling SetParam method

INTERFACE
  MODULE FUNCTION obj_GetTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetTopologyType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-14
! summary:  Returns the topology type

INTERFACE
  MODULE FUNCTION obj_GetTopologyType(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTopologyType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-14
! summary:  Get the total degree of freedom

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-12
! summary:  Import data from toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, elemType, nsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type of finite element, optional
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of spatial dimension, optional
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml, elemType, nsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Number of spatial dimension
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetBaseInterpolation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-16
! summary:  Get the baseInterpolation

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    CHARACTER(4) :: ans
  END FUNCTION obj_GetBaseInterpolation
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetBaseContinuity@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetBaseContinuity(obj) RESULT(ans)
    CLASS(BasisOpt_), INTENT(IN) :: obj
    CHARACTER(2) :: ans
  END FUNCTION obj_GetBaseContinuity
END INTERFACE

!----------------------------------------------------------------------------
!                                          Line_GetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get the quadrature points on line

INTERFACE
  MODULE SUBROUTINE Line_GetQuadraturePoints(obj, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE Line_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                       TriangleGetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get the quadrature points on Triangle

INTERFACE
  MODULE SUBROUTINE Triangle_GetQuadraturePoints(obj, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE Triangle_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                     QuadrangleGetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get the quadrature points on Quadrangle

INTERFACE
  MODULE SUBROUTINE Quadrangle_GetQuadraturePoints(obj, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE Quadrangle_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                     LineH1LagFE_GetLocalElemShapeData@LineH1LagrangeMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE LineH1LagFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
  END SUBROUTINE LineH1LagFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                    LineH1LagFE_GetGlobalElemShapeData@LineH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-13
! summary:  Get global element shape data  for Line H1 Lagrange FE

INTERFACE
  MODULE SUBROUTINE LineH1LagFE_GetGlobalElemShapeData(obj, xij, elemsd, &
                                                       geoelemsd)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    !! Abstract finite element
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    !! shape function data
    TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE LineH1LagFE_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                  LineH1LagFE_SetOrder@LineH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-11
! summary:  Set the order of Lagrange finite elements

INTERFACE
  MODULE SUBROUTINE LineH1LagFE_SetOrder(obj, order)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of Lagrange finite element
  END SUBROUTINE LineH1LagFE_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!            TriangleH1LagFE_GetLocalElemShapeData@TriangleH1LagrangeMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE TriangleH1LagFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
  END SUBROUTINE TriangleH1LagFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                          TriangleH1LagFE_SetOrder@TriangleH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-11
! summary:  Set the order of Lagrange finite elements

INTERFACE
  MODULE SUBROUTINE TriangleH1LagFE_SetOrder(obj, order)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of Lagrange finite element
  END SUBROUTINE TriangleH1LagFE_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!         QuadrangleH1LagFE_GetLocalElemShapeData@QuadrangleH1LagrangeMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE QuadrangleH1LagFE_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
  END SUBROUTINE QuadrangleH1LagFE_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                      QuadrangleH1LagFE_SetOrder@QuadrangleH1LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-11
! summary:  Set the order of Lagrange finite elements

INTERFACE
  MODULE SUBROUTINE QuadrangleH1LagFE_SetOrder(obj, order)
    CLASS(BasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of Lagrange finite element
  END SUBROUTINE QuadrangleH1LagFE_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BasisOpt_Class
