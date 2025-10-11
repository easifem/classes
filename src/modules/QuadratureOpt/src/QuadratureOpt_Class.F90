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

MODULE QuadratureOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT, stdout, CHAR_LF
USE String_Class, ONLY: String
USE BaseType, ONLY: ipopt => TypeInterpolationOpt, &
                    QuadraturePoint_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

PUBLIC :: QuadratureOpt_
PUBLIC :: TypeQuadratureOpt
PUBLIC :: SetQuadratureOptParam1, SetQuadratureOptParam2

CHARACTER(*), PARAMETER :: modName = "QuadratureOpt_Class"

!----------------------------------------------------------------------------
!                                                       QuadratureOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-18
! summary: Quadrature options

TYPE :: QuadratureOpt_
  PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  !! Is the object initialized?
  LOGICAL(LGT) :: isHomogeneous = .FALSE.
  !! Does all direction have same quadrature type
  !! In this case all entries of an array have same value

  LOGICAL(LGT) :: isOrder = .FALSE.
  !! is order specified?

  LOGICAL(LGT) :: isNips = .FALSE.
  !! is number of integration points specified?

  INTEGER(I4B) :: topoType = 0_I4B
  !! Element topology name
  !! Line, Triangle, Quadrangle, Tetrahedron, Hexahedron, Prism, Pyramid

  INTEGER(I4B) :: nsd = 0_I4B
  !! number of spatial dimensions

  INTEGER(I4B) :: xidim = 0_I4B
  !! xi dimension of the element

  INTEGER(I4B) :: quadratureType(3) = ipopt%GaussLegendre
  !! quadrature type

  REAL(DFP) :: alpha(3) = 0.0_DFP
  !! alpha parameter for Jacobi polynomials

  REAL(DFP) :: beta(3) = 0.0_DFP
  !! beta parameter for Jacobi polynomials

  REAL(DFP) :: lambda(3) = 0.5_DFP
  !! lambda parameter for Ultraspherical polynomials

  REAL(DFP) :: refelemCoord(3, 8) = 0.0_DFP
  !! coordinate of reference element

  INTEGER(I4B) :: order(3) = 0_I4B
  !! order of accuracy of the quadrature

  INTEGER(I4B) :: nips(3) = 0_I4B
  !! number of integration points

  CHARACTER(128) :: quadratureType_char(3) = "GAUSSLEGENDRE"
  !! quadrature type

  CHARACTER(1) :: refelemDomain = "B"
  !! String name for reference element domain.
  !! It can take following values:
  !! - UNIT "U"
  !! - BIUNIT "B"

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy the options from another object

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of the object

  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set the parameters

  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters

  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Intiate the object with parameterList

  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Intiate by using parameters directly

  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2
  !! Generic method for initiating the object

  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import from toml table
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

  PROCEDURE, PUBLIC :: DEALLOCATE => obj_Deallocate
  !! Deallocate the object

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
  !! Get quadrature points in the cell

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetFacetQuadraturePoints => obj_GetFacetQuadraturePoints
  !! Get quadrature points on the local face of the cell

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetTotalQuadraturePoints => obj_GetTotalQuadraturePoints

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetRefElemCoord => &
    obj_SetRefElemCoord
  !! Set the quadrature type
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetQuadratureType => &
    obj_SetQuadratureType
  !! Set the quadrature type
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order of accuracy
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetNips => obj_SetNips
  !! Set the number of integration points
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetAlpha => obj_SetAlpha
  !! Set the alpha parameter
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetBeta => obj_SetBeta
  !! Set the beta parameter
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetLambda => obj_SetLambda
  !! Set the lambda parameter
END TYPE QuadratureOpt_

!----------------------------------------------------------------------------
!                                                           TypeQuadratureOpt
!----------------------------------------------------------------------------

TYPE(QuadratureOpt_) :: TypeQuadratureOpt = QuadratureOpt_()

!----------------------------------------------------------------------------
!                                                                Copy@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Copy the content from obj2 to obj

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    CLASS(QuadratureOpt_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Display the content of the object

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(QuadratureOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: msg
  !! Message to display
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
  !! Unit number for writing
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetQuadratureOptParam@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE SetQuadratureOptParam1(param, prefix, quadratureType, &
                                           order, nips, alpha, beta, lambda, &
                                           nsd, topoType)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN), OPTIONAL :: prefix
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha
    REAL(DFP), INTENT(IN), OPTIONAL :: beta
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
  END SUBROUTINE SetQuadratureOptParam1
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetQuadratureOptParam@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE SetQuadratureOptParam2(param, prefix, quadratureType, &
                                           order, nips, alpha, beta, lambda, &
                                           nsd, topoType)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN), OPTIONAL :: prefix
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(3)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order(3)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(3)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
  END SUBROUTINE SetQuadratureOptParam2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-05
! summary: Sets the parameters for 1D quadrature options

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, isHomogeneous, nsd, xidim, topoType, &
                                 isOrder, isNips, quadratureType, &
                                 quadratureType1, quadratureType2, &
                                 quadratureType3, order, order1, order2, &
                                 order3, nips, nips1, nips2, &
                                 nips3, alpha, alpha1, alpha2, alpha3, &
                                 beta, beta1, beta2, beta3, &
                                 lambda, lambda1, lambda2, lambda3, &
                                 refelemDomain, refelemCoord)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isHomogeneous
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! number of spatial dimensions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidim
    !! xidimension of the element
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    !! Quadrature type in each direction, Maximum size can be 3
    !! If size is 1, then all directions have same quuadrature type
    !! When isHomogeneous is true, the we consider only the first entry
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType1
    !! It works only when isHomogeneous is false
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType2
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType3
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order(3), order1, order2, order3
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(3), nips1, nips2, nips3
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(3), alpha1, alpha2, alpha3
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(3), beta1, beta2, beta3
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(3), lambda1, lambda2, lambda3
    !! If isHomogenous is true, then we access only the first entry
    !! Otherwise we access 1:nsd entries
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
    !! Topology name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrder
    !! construct quadrature points based on order of accuracy
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNips
    !! construct quadrature points based on number of integration points
    CHARACTER(1), OPTIONAL, INTENT(IN) :: refelemDomain
    !! String name for reference element domain.
    REAL(DFP), OPTIONAL, INTENT(IN) :: refelemCoord(:, :)
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, prefix)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Initiate by parameters

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, isHomogeneous, quadratureType, &
                                  order, isOrder, nips, isNips, alpha, beta, &
                                  lambda, topoType, nsd, xidim, &
                                  refelemDomain, refelemCoord)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isHomogeneous
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(:)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrder
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNips
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
    CHARACTER(1), OPTIONAL, INTENT(IN) :: refelemDomain
    REAL(DFP), OPTIONAL, INTENT(IN) :: refelemCoord(:, :)
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-19
! summary: Get parameters of quadrature

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, isHomogeneous, quadratureType, &
                                 quadratureType1, quadratureType2, &
                                 quadratureType3, order, order1, order2, &
                                 order3, nips, nips1, nips2, nips3, &
                                 alpha, alpha1, alpha2, alpha3, beta, &
                                 beta1, beta2, beta3, lambda, lambda1, &
                                 lambda2, lambda3, nsd, topoType, &
                                 isOrder, isNips)
    CLASS(QuadratureOpt_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isHomogeneous
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: quadratureType(:)
    !! The size of quadratureType should be 3
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureType1
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureType2
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: quadratureType3
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: order(:)
    !! The size of order should be 3
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order1
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order2
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: order3
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: nips(:)
    !! The size of nips should be 3
    REAL(DFP), OPTIONAL, INTENT(OUT) :: nips1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: nips2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: nips3
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: alpha(:)
    !! The size of alpha should be 3
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alpha3
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: beta(:)
    !! The size of beta should be 3
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: beta3
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: lambda(:)
    !! The size of lambda should be 3
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: lambda3
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: topoType
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isOrder
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isNips
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary: Import data from toml table
!
!# Introduction
! The toml table should have following contents:
!
!```toml
!```

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, topoType, nsd, xidim, &
                                        refelemDomain, refelemCoord)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidim
    CHARACTER(1), OPTIONAL, INTENT(IN) :: refelemDomain
    REAL(DFP), OPTIONAL, INTENT(IN) :: refelemCoord(:, :)
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Import TimeOpt from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml, topoType, nsd, xidim, &
                                        refelemDomain, refelemCoord)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidim
    CHARACTER(1), OPTIONAL, INTENT(IN) :: refelemDomain
    REAL(DFP), OPTIONAL, INTENT(IN) :: refelemCoord(:, :)
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Deallocate the object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(QuadratureOpt_), INTENT(inout) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetQuadraturePoint@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get the quadrature points
!
!# Introduction
!   You get the quadrature points from the current state of the object.

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! Quadrature points
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetFacetQuadraturePoint@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get the quadrature points on local face of the cell

INTERFACE
  MODULE SUBROUTINE obj_GetFacetQuadraturePoints(obj, quad, facetQuad, &
                                                 localFaceNumber)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    !! Quadrature points
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number of the cell
  END SUBROUTINE obj_GetFacetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalQuadraturePoint@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-15
! summary: Get the total quadrature points

INTERFACE
  MODULE FUNCTION obj_GetTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetRefElemCoord@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the reference element coordinates

INTERFACE
  MODULE SUBROUTINE obj_SetRefElemCoord(obj, refelemCoord)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: refelemCoord(:, :)
  END SUBROUTINE obj_SetRefElemCoord
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
    CLASS(QuadratureOpt_), INTENT(inout) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType3
  END SUBROUTINE obj_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetOrder@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the order of accuracy

INTERFACE
  MODULE SUBROUTINE obj_SetOrder(obj, order, order1, order2, order3)
    CLASS(QuadratureOpt_), INTENT(inout) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order3
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                             SetNips@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the number of integration points

INTERFACE
  MODULE SUBROUTINE obj_SetNips(obj, nips, nips1, nips2, nips3)
    CLASS(QuadratureOpt_), INTENT(inout) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nips(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nips1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nips2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nips3
  END SUBROUTINE obj_SetNips
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetAlpha@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the alpha parameter

INTERFACE
  MODULE SUBROUTINE obj_SetAlpha(obj, alpha, alpha1, alpha2, alpha3)
    CLASS(QuadratureOpt_), INTENT(inout) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3
  END SUBROUTINE obj_SetAlpha
END INTERFACE

!----------------------------------------------------------------------------
!                                                             SetBeta@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the beta parameter

INTERFACE
  MODULE SUBROUTINE obj_SetBeta(obj, beta, beta1, beta2, beta3)
    CLASS(QuadratureOpt_), INTENT(inout) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta3
  END SUBROUTINE obj_SetBeta
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SetLambda@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-17
! summary: Set the lambda parameter

INTERFACE
  MODULE SUBROUTINE obj_SetLambda(obj, lambda, lambda1, lambda2, lambda3)
    CLASS(QuadratureOpt_), INTENT(inout) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda3
  END SUBROUTINE obj_SetLambda
END INTERFACE

END MODULE QuadratureOpt_Class
