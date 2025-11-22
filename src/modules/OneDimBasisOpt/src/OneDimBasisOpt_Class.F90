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

MODULE OneDimBasisOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: ipopt => TypeInterpolationOpt, &
                    polyopt => TypePolynomialOpt, &
                    QuadraturePoint_, ElemShapeData_
USE ExceptionHandler_Class, ONLY: e
USE OneDimQuadratureOpt_Class, ONLY: OneDimQuadratureOpt_
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

PUBLIC :: OneDimBasisOpt_, TypeOneDimBasisOpt

CHARACTER(*), PARAMETER :: modName = "OneDimBasisOpt_Class"

!----------------------------------------------------------------------------
!                                                             BasisOpt_Class
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary: This class contains data necessary for forming basis functions
! in one dimension

TYPE :: OneDimBasisOpt_
  PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  !! flag to check if the object is initialized or not

  LOGICAL(LGT) :: firstCall = .TRUE.
  !! flag to check if the shape functions are constructed from scratch or not

  INTEGER(I4B) :: tdof = 0
  !! total number of degrees of freedom
  !! this is equal to order + 1  in one dimension case

  INTEGER(I4B) :: order = 0
  !! order of the basis functions

  INTEGER(I4B) :: fetype = 0
  !! type of finite element
  !!  Scalar, Vector, Matrix

  INTEGER(I4B) :: dofType = 0
  !! degree of freedom type

  INTEGER(I4B) :: transformType = 0
  !! transformation type

  INTEGER(I4B) :: ipType = ipopt%equidistance
  !! interpolation type, it is used incase baseInterpolation is Lagrange

  INTEGER(I4B) :: basisType = polyopt%monomial
  !! basis type, it is used in case baseInterpolation is Lagrange
  !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto
  !! Ultraspherical

  INTEGER(I4B) :: cellOrient = 1_I4B
  !! cell orientation

  REAL(DFP) :: alpha = 0.0_DFP
  !! Jacobi polynomial parameter, x, y, z
  REAL(DFP) :: beta = 0.0_DFP
  !! Jacobi polynomial parametera, x, y, z
  REAL(DFP) :: lambda = 0.5_DFP
  !! Ultraspherical polynomial parameter, x, y, z

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
  !! LagrangeInterpolation
  !! HierarchyInterpolation
  !! OrthogonalInterpolation
  !! HermitInterpolation
  !! SerendipityInterpolation

  REAL(DFP) :: refelemCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])
  !! coordinate of reference element

  CHARACTER(128) :: basisType_char = "MONOMIAL"
  !! basis type in string format

  CHARACTER(128) :: ipType_char = "EQUIDISTANCE"
  !! interpolation type in string format

  CHARACTER(128) :: feType_char = "SCALAR"
  !! finite element type in string format

  TYPE(OneDimQuadratureOpt_) :: quadOpt
  !! Quadrature options

  REAL(DFP), ALLOCATABLE :: coeff(:, :), xx(:, :)
  !! coefficient necessary for lagrange Interpolation
  !! coefficient matrix needed for Lagrange interpolation
  !! Coeff helps us in reducing the computation time for Lagrange polynomials
  !! xx is evaluation of basis points

  REAL(DFP), ALLOCATABLE :: xij(:, :)
  !! Interpolation points used for Lagrange interpolation
  !! It is internal variables used in GetLocalElemShapeData

  REAL(DFP), ALLOCATABLE :: temp(:, :, :)
  !! temporary array used in case of Lagrange Interpolation
  !! It is internal variables used in GetLocalElemShapeData

CONTAINS

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate by arguments

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Deallocate the object

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the contents of the object

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set the parameters of the object

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order of the basis functions

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetQuadratureOrder => &
    obj_SetQuadratureOrder
  !! Set the order for quadrature points

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set the quadrature type

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseContinuity => &
    obj_GetBaseContinuity
  !! Get baseContinuity

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Get baseInterpolation

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Get the quadrature points

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetTotalQuadraturePoints => obj_GetTotalQuadraturePoints
  !! Get total number of quadrature points

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetCaseName => &
    obj_GetCaseName
  !! Get the case name

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetOrder => obj_GetOrder
  !! Get the order

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalDOF => &
    Obj_GetTotalDOF
  !! Get total degrees of freedom

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    Lagrange_GetLocalElemShapeData
  !! Get local element shape data

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    Hierarchical_GetLocalElemShapeData
  !! Get local element shape data

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    Orthogonal_GetLocalElemShapeData
  !! Get local element shape data

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml1 => &
    obj_ImportFromToml1
  !! Import from toml table
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  !! Improt from toml table
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

END TYPE OneDimBasisOpt_

!----------------------------------------------------------------------------
!                                                              TypeBasisOpt
!----------------------------------------------------------------------------

TYPE(OneDimBasisOpt_), PARAMETER :: TypeOneDimBasisOpt = OneDimBasisOpt_()

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary:  Initiate OneDimBasisOpt by arguments

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, baseContinuity, baseInterpolation, feType, ipType, &
    basisType, alpha, beta, lambda, dofType, transformType, order, &
    quadratureType, quadratureOrder, quadratureIsOrder, quadratureNips, &
    quadratureIsNips, quadratureAlpha, quadratureBeta, quadratureLambda)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    !! Finite element object
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar, For HDiv and Hcurl it should be Vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial
    !! Default ipType is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis type: Legendre, Lobatto, Ultraspherical,
    !! Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType
    !! degree of freedom type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic Order of finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Accuracy of quadrature rule
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! If true, then quadratureOrder is used to set the number of
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips
    !! Number of integration points
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! If true, then quadratureNips is used to set the number of
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Ultraspherical parameter for quadrature
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(OneDimBasisOpt_), INTENT(inout) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Copy
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(OneDimBasisOpt_), INTENT(inout) :: obj
    CLASS(OneDimBasisOpt_), INTENT(in) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary:  Display the contents of the object

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary:  Set the parameters in the OneDimBasisOpt object

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, order, fetype, ipType, basisType, alpha, beta, lambda, &
    refElemDomain, baseContinuity, baseInterpolation, firstCall, &
    quadratureType, quadratureOrder, quadratureNips, quadratureAlpha, &
    quadratureBeta, quadratureLambda, dofType, transformType)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! finite element type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation point type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis type in x, y, and z directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    !! String name of type of continuity used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
    !! String name of type of interpolation used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refElemDomain
    !! Domain of reference element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Quadrature order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Quadrature alpha parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Quadrature beta parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Quadrature lambda parameter
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                              SetOrder
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: Set the order of the basis functions

INTERFACE
  MODULE SUBROUTINE obj_SetOrder(obj, order)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetQuadratureOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: Set the order of the basis functions

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureOrder(obj, order)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order
  END SUBROUTINE obj_SetQuadratureOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: Set the quadrature type

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureType( &
    obj, quadratureType, alpha, beta, lambda)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: quadratureType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE obj_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  GetParam
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, order, tdof, fetype, ipType, basisType, alpha, beta, lambda, &
    refElemDomain, baseContinuity, baseInterpolation, firstCall, &
    quadratureType, quadratureOrder, quadratureNips, quadratureAlpha, &
    quadratureBeta, quadratureLambda, dofType, transformType)
    CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    !! Abstract one dimenstional finite element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order
    !! order of element (isotropic order)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tdof
    !! total degrees of freedom
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: fetype
    !! finite element type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ipType
    !! interpolation point type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: basisType
    !! Basis type in x, y, and z directions
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda
    !! Ultraspherical parameter
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseContinuity
    !! String name of type of continuity used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: baseInterpolation
    !! String name of type of interpolation used for basis functions
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: refElemDomain
    !! Domain of reference element
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: firstCall
    !!
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureOrder
    !! Quadrature order
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureNips
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureAlpha
    !! Quadrature alpha parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureBeta
    !! Quadrature beta parameter
    REAL(DFP), OPTIONAL, INTENT(OUT) :: quadratureLambda
    !! Quadrature lambda parameter
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: transformType
    !! Transformation type
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: dofType
    !! Degree of freedom type
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetBaseContinuity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-17
! summary: Get baseContinuity value from the object

INTERFACE
  MODULE FUNCTION obj_GetBaseContinuity(obj) RESULT(ans)
    CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    !! Object from which to get baseContinuity
    CHARACTER(2) :: ans
    !! The baseContinuity value
  END FUNCTION obj_GetBaseContinuity
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetBaseInterpolation
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-17
! summary: Get baseInterpolation value from the object

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    !! Object from which to get baseInterpolation
    CHARACTER(4) :: ans
    !! The baseInterpolation value
  END FUNCTION obj_GetBaseInterpolation
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetQuadraturePoints
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-19
! summary:  Get the quadratuere points

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    !! OneDimBasisOpt
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalQuadraturePoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-21
! summary:  Get total number of quadrature points

INTERFACE
  MODULE FUNCTION obj_GetTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetCaseName
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetCaseName(obj) RESULT(ans)
    CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetCaseName
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetOrder(obj) RESULT(ans)
    CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-22
! summary: Get total degrees of freedom

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF(obj) RESULT(ans)
    CLASS(OneDimBasisOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                  Lagrange_GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data for lagrange

INTERFACE
  MODULE SUBROUTINE Lagrange_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE Lagrange_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                              Hierarchical_GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data for lagrange

INTERFACE
  MODULE SUBROUTINE Hierarchical_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE Hierarchical_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                Orthogonal_GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data for lagrange

INTERFACE
  MODULE SUBROUTINE Orthogonal_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE Orthogonal_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary: Import OneDimBasisOpt_ from toml table
!
!# Introduction
! The toml table should have following contents:
!
!```toml
! [BasisOpt]
!
!```

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Import TimeOpt from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml)
    CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

END MODULE OneDimBasisOpt_Class
