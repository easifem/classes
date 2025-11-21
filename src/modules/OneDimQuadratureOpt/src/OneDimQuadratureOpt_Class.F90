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
USE BaseType, ONLY: QuadraturePoint_
USE ExceptionHandler_Class, ONLY: e
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

PUBLIC :: OneDimQuadratureOpt_
PUBLIC :: TypeOneDimQuadratureOpt

CHARACTER(*), PARAMETER :: modName = "OneDimQuadratureOpt_Class"

!----------------------------------------------------------------------------
!                                                       OneDimQuadratureOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-18
! summary: Quadrature options in 1D

TYPE :: OneDimQuadratureOpt_
  PRIVATE
  LOGICAL(LGT) :: isOrder = .FALSE.
  !! is order specified?

  LOGICAL(LGT) :: isNips = .FALSE.
  !! is number of integration points specified?

  INTEGER(I4B) :: quadratureType = ipopt%GaussLegendre
  !! quadrature type

  INTEGER(I4B) :: order = 0_I4B
  !! order of accuracy of the quadrature

  INTEGER(I4B) :: nips(1) = 0_I4B
  !! number of integration points

  CHARACTER(128) :: quadratureType_char = "GAUSSLEGENDRE"
  !! quadrature type

  REAL(DFP) :: alpha = 0.0_DFP
  !! alpha parameter for Jacobi polynomials

  REAL(DFP) :: beta = 0.0_DFP
  !! beta parameter for Jacobi polynomials

  REAL(DFP) :: lambda = 0.5_DFP
  !! lambda parameter for Ultraspherical polynomials

  REAL(DFP) :: refelemCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])
  !! coordinate of reference element

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy the options from another object
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of the object
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set the parameters
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Intiate by using parameters directly
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import from toml table
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  PROCEDURE, PUBLIC :: DEALLOCATE => obj_Deallocate
  !! Deallocate the object
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalQuadraturePoints => &
    obj_GetTotalQuadraturePoints
  !! Get total number of quadrature points
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
  !! Set the order of quadrature
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadratureType => obj_SetQuadratureType
  !! Set the quadrature type
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuadraturePoints => &
    obj_GetQuadraturePoints
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
!                                                                SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-05
! summary: Sets the parameters for 1D quadrature options

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, quadratureType, order, nips, alpha, beta, lambda)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha
    REAL(DFP), INTENT(IN), OPTIONAL :: beta
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Initiate OneDimQuadratureOpt_

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, quadratureType, order, nips, alpha, beta, lambda, isOrder, isNips, &
    refelemCoord)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: quadratureType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: order
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nips
    REAL(DFP), INTENT(IN), OPTIONAL :: alpha
    REAL(DFP), INTENT(IN), OPTIONAL :: beta
    REAL(DFP), INTENT(IN), OPTIONAL :: lambda
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrder
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNips
    REAL(DFP), OPTIONAL, INTENT(IN) :: refelemCoord(:, :)
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-19
! summary: Get the parameters of the 1D quadrature

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, quadratureType, order, nips, alpha, beta, lambda)
    CLASS(OneDimQuadratureOpt_), INTENT(in) :: obj
    INTEGER(i4b), INTENT(out), OPTIONAL :: quadratureType
    INTEGER(i4b), INTENT(out), OPTIONAL :: order
    INTEGER(i4b), INTENT(out), OPTIONAL :: nips
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
!                                                   GetTotalQuadraturePoints
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-21
! summary: Get the total number of quadrature points

INTERFACE
  MODULE FUNCTION obj_GetTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(OneDimQuadratureOpt_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-21
! summary: Set the order of quadrature

INTERFACE
  MODULE SUBROUTINE obj_SetOrder(obj, order)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order
  END SUBROUTINE obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetQuadratureType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-21
! summary: Set the quadrature type

INTERFACE
  MODULE SUBROUTINE obj_SetQuadratureType( &
    obj, quadratureType, alpha, beta, lambda)
    CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: quadratureType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE obj_SetQuadratureType
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetQuadraturePoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-21
! summary:  Get the quadrature points

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad)
    CLASS(OneDimQuadratureOpt_), INTENT(IN) :: obj
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimQuadratureOpt_Class
