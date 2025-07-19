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

MODULE OneDimQuadratureOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE String_Class, ONLY: String
USE BaseType, ONLY: ipopt => TypeInterpolationOpt
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

PUBLIC :: OneDimQuadratureOpt_
PUBLIC :: TypeOneDimQuadratureOpt
PUBLIC :: SetOneDimQuadratureOptParam

CHARACTER(*), PARAMETER :: modName = "OneDimQuadratureOpt_Class"

!----------------------------------------------------------------------------
!                                                       OneDimQuadratureOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-18
! summary: Quadrature options in 1D

TYPE :: OneDimQuadratureOpt_
  PRIVATE
  INTEGER(I4B) :: quadratureType = ipopt%GaussLegendre
  !! quadrature type

  REAL(DFP) :: alpha = 0.0_DFP
  !! alpha parameter for Jacobi polynomials

  REAL(DFP) :: beta = 0.0_DFP
  !! beta parameter for Jacobi polynomials

  REAL(DFP) :: lambda = 0.5_DFP
  !! lambda parameter for Ultraspherical polynomials

  INTEGER(I4B) :: order = 0_I4B
  !! order of accuracy of the quadrature

  INTEGER(I4B) :: nips(1) = 0_I4B
  !! number of integration points

  CHARACTER(128) :: quadratureType_char = "GAUSSLEGENDRE"
  !! quadrature type

  LOGICAL(LGT) :: isOrder = .FALSE.
  !! is order specified?

  LOGICAL(LGT) :: isNips = .FALSE.
  !! is number of integration points specified?

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

END TYPE OneDimQuadratureOpt_

!----------------------------------------------------------------------------
!                                                    TypeOneDimQuadratureOpt
!----------------------------------------------------------------------------

TYPE(OneDimQuadratureOpt_) :: TypeOneDimQuadratureOpt = &
                              OneDimQuadratureOpt_()

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Copy the content from obj2 to obj

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
    CLASS(OneDimQuadratureOpt_), INTENT(IN) :: obj2
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
    CLASS(OneDimQuadratureOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: msg
    !! Message to display
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! Unit number for writing
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetOneDimQuadratureOptParam
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE SetOneDimQuadratureOptParam(param, prefix, quadratureType, &
                                             order, nips, alpha, beta, lambda)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN), OPTIONAL :: prefix
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(1)
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha
    REAL(DFP), INTENT(IN), OPTIONAL :: beta
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda
  END SUBROUTINE SetOneDimQuadratureOptParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                                SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-05
! summary: Sets the parameters for 1D quadrature options

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, quadratureType, order, nips, &
                                 alpha, beta, lambda)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(1)
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha
    REAL(DFP), INTENT(IN), OPTIONAL :: beta
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, prefix)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Initiate by parameters

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, quadratureType, order, nips, alpha, &
                                  beta, lambda, isOrder, isNips)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips(1)
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha
    REAL(DFP), INTENT(IN), OPTIONAL :: beta
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNips
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-19
! summary: Get the parameters of the 1D quadrature

INTERFACE
  module SUBROUTINE obj_GetParam(obj, quadratureType, order, nips, alpha, beta, lambda)
    CLASS(OneDimQuadratureOpt_), INTENT(in) :: obj
    INTEGER(i4b), INTENT(out), OPTIONAL :: quadratureType
    INTEGER(i4b), INTENT(out), OPTIONAL :: order
    INTEGER(i4b), INTENT(out), OPTIONAL :: nips(1)
    REAL(DFP), INTENT(out), OPTIONAL :: alpha
    REAL(DFP), INTENT(out), OPTIONAL :: beta
    REAL(DFP), INTENT(out), OPTIONAL :: lambda
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
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
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
  module SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, printToml)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
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
    CLASS(OneDimQuadratureOpt_), INTENT(inout) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimQuadratureOpt_Class
