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

MODULE SDAlgorithm2_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table

IMPLICIT NONE

PRIVATE
PUBLIC :: SDAlgorithm2_
CHARACTER(*), PARAMETER :: modName = "SDAlgorithm2_Class()"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-24
! summary:  Displacement based single step semi discrete algorithm

TYPE :: SDAlgorithm2_
  CHARACTER(4) :: name = "NEWM"
  REAL(DFP) :: tanmat(3) = 0.0_DFP
  !! tanmat = tanmat(1) * M + tanmat(2) * C *dt + tanmat(3) * K * dt * dt
  !! tanmat(1) : coefficient of mass matrix (M)
  !! tanmat(2) : coefficient of damping matrix (C*dt)
  !! tanmat(3) : coefficient of stiffness matrix (K*dt*dt)
  LOGICAL(LGT) :: tanmat_zero(3) = .TRUE.

  REAL(DFP) :: rhs_u1(3) = 0.0_DFP
  !! coefficient for rhs for displacement at time tn
  !! rhs = rhs + rhs_u1(1) * M + rhs_u1(2) * C * dt + rhs_u1(3) * K * dt * dt
  LOGICAL(LGT) :: rhs_u1_zero(3) = .TRUE.

  REAL(DFP) :: rhs_v1(3) = 0.0_DFP
  !! coefficient for rhs for (velocity*dt) at time tn
  !! rhs = rhs + rhs_v1(1) *dt * M + rhs_v1(2) * dt * C * dt  &
  !! & + rhs_v1(3) * dt * K * dt * dt
  LOGICAL(LGT) :: rhs_v1_zero(3) = .TRUE.

  REAL(DFP) :: rhs_a1(3) = 0.0_DFP
  !! coefficient for rhs for (acceleration*dt*dt) at time tn
  !! rhs = rhs + rhs_v1(1) *dt * M + rhs_v1(2) * dt * C * dt  &
  !! & + rhs_v1(3) * dt * K * dt * dt
  LOGICAL(LGT) :: rhs_a1_zero(3) = .TRUE.

  REAL(DFP) :: rhs_f1 = 0.0_DFP
  !! coefficient for rhs for (force*dt*dt) at time tn
  !! rhs = rhs + rhs_f1 *dt *dt * force1
  LOGICAL(LGT) :: rhs_f1_zero = .TRUE.

  REAL(DFP) :: rhs_f2 = 0.0_DFP
  !! coefficient for rhs for (force*dt*dt) at time tn+1
  !! rhs = rhs + rhs_f2 *dt *dt * force2
  LOGICAL(LGT) :: rhs_f2_zero = .TRUE.

  REAL(DFP) :: dis(4) = 0.0_DFP
  !! dis coefficient for velocity update
  !! displacement = dis(1)*u+dis(2)*v*dt+dis(3)*a*dt*dt+dis(4)*sol
  !! dis(1) coefficient of displacement
  !! dis(2) coefficient of velocity
  !! dis(3) coefficient of acceleration
  !! dis(4) coefficient of solution
  LOGICAL(LGT) :: dis_zero(4) = .TRUE.

  REAL(DFP) :: vel(4) = 0.0_DFP
  !! vel coefficient for velocity update
  !! velocity = vel(1)*u / dt + vel(2) * v + vel(3) * a *dt + vel(4)*sol/dt
  !! vel(1) coefficient of displacement
  !! vel(2) coefficient of velocity
  !! vel(3) coefficient of acceleration
  !! vel(4) coefficient of solution
  LOGICAL(LGT) :: vel_zero(4) = .TRUE.

  REAL(DFP) :: acc(4) = 0.0_DFP
  !! acc coefficient for acc update
  !! acc = acc(1)*u / dt/dt + acc(2) * v /dt + acc(3) * a + acc(4)*sol/dt/dt
  !! acc(1) coefficient of displacement
  !! acc(2) coefficient of velocity
  !! acc(3) coefficient of acceleration
  !! acc(4) coefficient of solution
  LOGICAL(LGT) :: acc_zero(4) = .TRUE.

  REAL(DFP) :: alpha = 0.0_DFP
  !! only in case of HHT-alpha method it can be less than 0.0

  REAL(DFP) :: initialGuess(3) = 0.0_DFP
  !! coefficient for initial guess of solution
  !! u = coeff(1)*Un + coeff(2) * Vn * dt + coeff(3) * An * dt * dt
  !! coeff(1) coefficient of displacement at tn
  !! coeff(2) coefficient of velocity at tn
  LOGICAL(LGT) :: initialGuess_zero(3) = .TRUE.

CONTAINS
  PRIVATE

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PUBLIC, PASS(obj) :: NewmarkBetaMethod => obj_NewmarkBetaMethod
  PROCEDURE, PUBLIC, PASS(obj) :: HHTAlphaMethod => obj_HHTAlphaMethod
  PROCEDURE, PUBLIC, PASS(obj) :: CollocationMethod => obj_CollocationMethod
  ! PROCEDURE, PUBLIC, PASS(obj) :: Houbolt => obj_Houbolt
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
END TYPE SDAlgorithm2_

!----------------------------------------------------------------------------
!                                             NewmarkBeta@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Initiate Newmark-Beta method

INTERFACE
  MODULE SUBROUTINE obj_NewmarkBetaMethod(obj, beta, gamma)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Default is 0.25
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    !! Default is 0.5
  END SUBROUTINE obj_NewmarkBetaMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                          HHTAlphaMethod@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary:  Initiate Hilber-Hughes-Taylor Alpha method

INTERFACE
  MODULE SUBROUTINE obj_HHTAlphaMethod(obj, alpha, beta, gamma)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Default -0.30
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Default is (1-alpha)**2/4
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    !! Default is (1-2alpha)/2
  END SUBROUTINE obj_HHTAlphaMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                        CollocationMethod@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary:  Initiate Collocation method

INTERFACE
  MODULE SUBROUTINE obj_CollocationMethod(obj, beta, gamma, theta)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Default is 1/6
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    !! Default is 0.50
    REAL(DFP), OPTIONAL, INTENT(IN) :: theta
    !! Default is 1.4 (Wilson theta)
  END SUBROUTINE obj_CollocationMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                            HouboltMethod@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Initiate Houbolt method

INTERFACE
  MODULE SUBROUTINE obj_HouboltMethod(obj)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    CHARACTER(*), PARAMETER :: myName = "Houbolt"
  END SUBROUTINE obj_HouboltMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                                MakeZeros@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Reset the SDAlgorithm2_ object to zero values

INTERFACE
  MODULE SUBROUTINE obj_MakeZeros(obj)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
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
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
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
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
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
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
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
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
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

END MODULE SDAlgorithm2_Class
