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

MODULE SDAlgorithm1_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE
PUBLIC :: SDAlgorithm1_
CHARACTER(*), PARAMETER :: modName = "SDAlgorithm1_Class()"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-24
! summary:  Displacement based single step semi discrete algorithm

TYPE :: SDAlgorithm1_
  CHARACTER(4) :: name = "CRAN"
  !! default name is "CrankNicolson"
  REAL(DFP) :: tanmat(2) = 0.0_DFP
  !! tanmat = tanmat(1) * M + tanmat(2) * K *dt
  !! tanmat(1) : coefficient of mass matrix (M)
  !! tanmat(2) : coefficient of damping matrix (C*dt)
  LOGICAL(LGT) :: tanmat_zero(2) = .TRUE.

  REAL(DFP) :: rhs_u1(2) = 0.0_DFP
  !! coefficient for rhs for displacement at time tn, Un
  !! rhs += rhs_u1(1) * M + rhs_u1(2) * K * dt
  LOGICAL(LGT) :: rhs_u1_zero(2) = .TRUE.

  REAL(DFP) :: rhs_v1(2) = 0.0_DFP
  !! coefficient for rhs for (velocity*dt) at time tn, Vn
  !! rhs += rhs_v1(1) *dt * M + rhs_v1(2) * dt * K * dt
  LOGICAL(LGT) :: rhs_v1_zero(2) = .TRUE.

  REAL(DFP) :: rhs_f1 = 0.0_DFP
  !! coefficient for rhs for (force*dt) at time tn, Fn
  !! rhs = rhs + rhs_f1 * force1 * dt
  LOGICAL(LGT) :: rhs_f1_zero = .TRUE.

  REAL(DFP) :: rhs_f2 = 0.0_DFP
  !! coefficient for rhs for (force*dt) at time tn+1, Fn+1
  !! rhs = rhs + rhs_f2 *dt * force2
  LOGICAL(LGT) :: rhs_f2_zero = .TRUE.

  REAL(DFP) :: dis(3) = 0.0_DFP
  !! dis coefficient for displacement update
  !! displacement = dis(1)*Un+dis(2)*Vn*dt+dis(3)*sol
  !! dis(1) coefficient of displacement
  !! dis(2) coefficient of velocity
  !! dis(3) coefficient of solution
  LOGICAL(LGT) :: dis_zero(3) = .TRUE.

  REAL(DFP) :: vel(3) = 0.0_DFP
  !! vel coefficient for velocity update
  !! velocity = vel(1)*Un / dt + vel(2) * Vn  + vel(3)*sol/dt
  !! vel(1) coefficient of displacement at tn
  !! vel(2) coefficient of velocity at tn
  !! vel(3) coefficient of solution at tn+1
  LOGICAL(LGT) :: vel_zero(3) = .TRUE.

  REAL(DFP) :: initialGuess(2) = 0.0_DFP
  !! coefficient for initial guess of solution
  !! u = coeff(1)*Un + coeff(2) * Vn
  !! coeff(1) coefficient of displacement at tn
  !! coeff(2) coefficient of velocity at tn
  LOGICAL(LGT) :: initialGuess_zero(2) = .TRUE.

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PUBLIC, PASS(obj) :: AlphaMethod => obj_AlphaMethod
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
END TYPE SDAlgorithm1_

!----------------------------------------------------------------------------
!                                             NewmarkBeta@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Initiate Newmark-Beta method

INTERFACE
  MODULE SUBROUTINE obj_AlphaMethod(obj, alpha)
    CLASS(SDAlgorithm1_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Default is 0.5
  END SUBROUTINE obj_AlphaMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                                MakeZeros@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Reset the SDAlgorithm1_ object to zero values

INTERFACE
  MODULE SUBROUTINE obj_MakeZeros(obj)
    CLASS(SDAlgorithm1_), INTENT(INOUT) :: obj
    ! internal varibales
    REAL(DFP), PARAMETER :: myzero = 0.0_DFP
  END SUBROUTINE obj_MakeZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Display@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(SDAlgorithm1_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary:  Deallocate the object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(SDAlgorithm1_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Import data from toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(SDAlgorithm1_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(SDAlgorithm1_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    ! internal variables
    CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
    TYPE(toml_table), ALLOCATABLE :: table
    TYPE(toml_table), POINTER :: node
    INTEGER(I4B) :: origin, stat
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SDAlgorithm1_Class
