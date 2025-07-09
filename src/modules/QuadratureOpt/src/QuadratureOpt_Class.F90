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
USE BaseType, ONLY: ipopt => TypeInterpolationOpt
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
  LOGICAL(LGT) :: isHomogeneous = .FALSE.
  !! Does all direction have same quadrature type
  !! In this case all entries of an array have same value

  LOGICAL(LGT) :: isOrder = .FALSE.
  !! is order specified?

  LOGICAL(LGT) :: isNips = .FALSE.
  !! is number of integration points specified?

  INTEGER(I4B) :: topoName = 0_I4B
  !! Element topology name
  !! Line, Triangle, Quadrangle, Tetrahedron, Hexahedron, Prism, Pyramid

  INTEGER(I4B) :: nsd = 3_I4B
  !! number of spatial dimensions

  INTEGER(I4B) :: quadratureType(3) = ipopt%GaussLegendre
  !! quadrature type

  REAL(DFP) :: alpha(3) = 0.0_DFP
  !! alpha parameter for Jacobi polynomials

  REAL(DFP) :: beta(3) = 0.0_DFP
  !! beta parameter for Jacobi polynomials

  REAL(DFP) :: lambda(3) = 0.5_DFP
  !! lambda parameter for Ultraspherical polynomials

  INTEGER(I4B) :: order(3) = 0_I4B
  !! order of accuracy of the quadrature

  INTEGER(I4B) :: nips(3) = 0_I4B
  !! number of integration points

  CHARACTER(128) :: quadratureType_char(3) = "GAUSSLEGENDRE"
  !! quadrature type

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

END TYPE QuadratureOpt_

!----------------------------------------------------------------------------
!                                                    TypeQuadratureOpt
!----------------------------------------------------------------------------

TYPE(QuadratureOpt_) :: TypeQuadratureOpt = &
                        QuadratureOpt_()

!----------------------------------------------------------------------------
!                                                                       Copy
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
!                                                                   Display
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
!                                                SetQuadratureOptParam
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE SetQuadratureOptParam1(param, prefix, quadratureType, &
                                           order, nips, alpha, beta, lambda, &
                                           nsd, topoName)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN), OPTIONAL :: prefix
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha
    REAL(DFP), INTENT(IN), OPTIONAL :: beta
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoName
  END SUBROUTINE SetQuadratureOptParam1
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetQuadratureOptParam
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE SetQuadratureOptParam2(param, prefix, quadratureType, &
                                           order, nips, alpha, beta, lambda, &
                                           nsd, topoName)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN), OPTIONAL :: prefix
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(3)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order(3)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(3)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoName
  END SUBROUTINE SetQuadratureOptParam2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-05
! summary: Sets the parameters for 1D quadrature options

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, isHomogeneous, quadratureType, order, &
                    nips, alpha, beta, lambda, nsd, topoName, isOrder, isNips)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isHomogeneous
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(3)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order(3)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(3)
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(3)
  !! If isHomogenous is true, then we access only the first entry
  !! Otherwise we access 1:nsd entries
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
  !! number of spatial dimensions
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoName
  !! Topology name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNips
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, prefix)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Initiate by parameters

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, isHomogeneous, quadratureType, &
                  order, isOrder, isOrderScalar, nips, isNips, isNipsScalar, &
                  alpha, isAlpha, isAlphaScalar, beta, isBeta, isBetaScalar, &
                              lambda, isLambda, isLambdaScalar, nsd, topoName)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isHomogeneous
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType(3)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order(3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrderScalar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNips
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNipsScalar
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha(3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isAlpha
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isAlphaScalar
    REAL(DFP), INTENT(IN), OPTIONAL :: beta(3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isBeta
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isBetaScalar
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda(3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLambda
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLambdaScalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoName
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   GetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-19
! summary: Get parameters of quadrature

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, isHomogeneous, quadratureType, order, &
                                 nips, alpha, beta, lambda, nsd, topoName)
    CLASS(QuadratureOpt_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isHomogeneous
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: quadratureType(3)
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: order(3)
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: nips(3)
    REAL(DFP), INTENT(OUT), OPTIONAL :: alpha(3)
    REAL(DFP), INTENT(OUT), OPTIONAL :: beta(3)
    REAL(DFP), INTENT(OUT), OPTIONAL :: lambda(3)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: topoName
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
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
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table

    ! Internal variables
    CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"

    CHARACTER(:), ALLOCATABLE :: key
    INTEGER(I4B) :: origin, stat, order(3), nips(3), tsize, &
                    quadratureType(3), ii
    REAL(DFP) :: alpha(3), beta(3), lambda(3)
    INTEGER(I4B), ALLOCATABLE :: aintvec(:)
    REAL(DFP), ALLOCATABLE :: arealvec(:)
    TYPE(String), ALLOCATABLE :: quadratureType_char(:)
    LOGICAL(LGT) :: isQuadratureTypeScalar, isok, isFound, isOrder, isNips, &
                    isHomogeneous, isOrderScalar, isNipsScalar, &
                    isAlpha, isAlphaScalar, isBeta, isBetaScalar, &
                    isLambda, isLambdaScalar

  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Import TimeOpt from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Deallocate the object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(QuadratureOpt_), INTENT(inout) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

END MODULE QuadratureOpt_Class
