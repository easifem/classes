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

MODULE CSDAlgorithm2_Class
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP, I4B, LGT
USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
USE SDAlgorithm2_Class, ONLY: SDAlgorithm2Pointer_
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE

PUBLIC :: CSDAlgorithm2_

CHARACTER(*), PARAMETER :: modName = "CSDAlgorithms2_Class()"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Composite Semi Discrete Algorithm

TYPE :: CSDAlgorithm2_
  CHARACTER(4) :: name

  INTEGER(I4B) :: totalSubstep = math%zero_i
  INTEGER(I4B) :: currentSubstep = math%one_i

  LOGICAL(LGT) :: singleStep = math%no

  TYPE(SDAlgorithm2Pointer_) :: substeps(2)

  REAL(DFP) :: splitRatios(2) = [math%half, math%one]
  ! split ratio for a time interval
  ! for example, in bathe method,
  ! this value corresponds to gamma

  REAL(DFP) :: rhs_subsol(3, 2) = math%zero
  LOGICAL(LGT) :: rhs_subsol_zero(3, 2) = math%yes

  REAL(DFP) :: rhs_subf(2) = math%zero
  LOGICAL(LGT) :: rhs_subf_zero(2) = math%yes

  REAL(DFP) :: dis_subsol = math%zero
  LOGICAL(LGT) :: dis_subsol_zero = math%yes

  REAL(DFP) :: vel_subsol = math%zero
  LOGICAL(LGT) :: vel_subsol_zero = math%yes

  REAL(DFP) :: acc_subsol = math%zero
  LOGICAL(LGT) :: acc_subsol_zero = math%yes

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PUBLIC, PASS(obj) :: BatheMethod => obj_BatheMethod

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalSubstep => obj_GetTotalSubstep
  PROCEDURE, PUBLIC, PASS(obj) :: GetCurrentSubstep => obj_GetCurrentSubstep
  PROCEDURE, PUBLIC, PASS(obj) :: UpdateCurrentSubstep => &
    obj_UpdateCurrentSubstep
  PROCEDURE, PUBLIC, PASS(obj) :: GetSubstepSize => obj_GetSubstepSize
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetTimeIntegration => obj_SetTimeIntegration

  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

END TYPE CSDAlgorithm2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary:  Display

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary:  Deallocate

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary:  Make Zeros

INTERFACE
  MODULE SUBROUTINE obj_MakeZeros(obj)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_MakeZeros
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary:  Get total substep

INTERFACE
  MODULE FUNCTION obj_GetTotalSubstep(obj) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalSubstep
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Get current substep

INTERFACE
  MODULE FUNCTION obj_GetCurrentSubstep(obj) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetCurrentSubstep
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Get current substep size

INTERFACE
  MODULE FUNCTION obj_GetSubstepSize(obj, dt, substepIndex) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: dt
    INTEGER(I4B), INTENT(IN) :: substepIndex
    REAL(DFP) :: ans
  END FUNCTION obj_GetSubstepSize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Get current substep

INTERFACE
  MODULE SUBROUTINE obj_UpdateCurrentSubstep(obj)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_UpdateCurrentSubstep
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Set coefficients

! INTERFACE
!   MODULE SUBROUTINE obj_SetTimeIntegration(obj)
!     CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_SetTimeIntegration
! END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary:  Master method to setup Bathe method

INTERFACE
  MODULE SUBROUTINE obj_BatheMethod(obj, gamma, rhoInf, beta1, beta2)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    REAL(DFP), OPTIONAL, INTENT(IN) :: rhoInf
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
  END SUBROUTINE obj_BatheMethod
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Import data from toml

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Import data from toml

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSDAlgorithm2_Class
