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

MODULE TDGAlgorithm2_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table
USE BaseType, ONLY: ElemShapeData_

IMPLICIT NONE

PRIVATE
PUBLIC :: TDGAlgorithm2_
CHARACTER(*), PARAMETER :: modName = "TDGAlgorithm2_Class()"

INTEGER(I4B), PARAMETER :: MAX_ORDER_TIME = 20

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-12
! summary:  Velocity based time discontinuous Galerkin algorithm

TYPE :: TDGAlgorithm2_
  LOGICAL(LGT) :: isInit = .FALSE.
  !! Flag to check if the object is initiated

  CHARACTER(4) :: name = "TDG2"

  INTEGER(I4B) :: nrow = 0_I4B, ncol = 0_I4B
  !! Number of rows and columns in ct, mt, mtplus matrices

  REAL(DFP) :: initialGuess(MAX_ORDER_TIME + 4) = 0.0_DFP
  !! coefficient for initial guess of solution
  LOGICAL(LGT) :: initialGuess_zero(MAX_ORDER_TIME + 4) = .TRUE.

  REAL(DFP) :: dis(MAX_ORDER_TIME + 4) = 0.0_DFP
  !! dis coefficient for displacement update
  !! displacement = dis(1)*Un+dis(2)*Vn*dt +dis(3)*An*dt^2+ dis(4)*sol(1) * dt + ...
  !! dis(1) coefficient of displacement at time tn
  !! dis(2) coefficient of velocity at time tn
  !! dis(3) coefficient of acceleration at time tn
  !! dis(4:MAX_ORDER_TIME+4) coefficient of solution dof at time t1, t2, ...
  LOGICAL(LGT) :: dis_zero(MAX_ORDER_TIME + 4) = .TRUE.

  REAL(DFP) :: vel(MAX_ORDER_TIME + 4) = 0.0_DFP
  !! vel coefficient for velocity update
  !! velocity = vel(1)*Un / dt + vel(2) * Vn  + vel(3)*An *dt + vel(4) * sol(1) + ...
  !! vel(1) coefficient of displacement at time tn
  !! vel(2) coefficient of velocity at time tn
  !! vel(3) coefficient of acceleration at time tn
  !! vel(4:MAX_ORDER_TIME+4) coefficient of solution dof at time t1, t2, ...
  LOGICAL(LGT) :: vel_zero(MAX_ORDER_TIME + 4) = .TRUE.

  REAL(DFP) :: acc(MAX_ORDER_TIME + 4) = 0.0_DFP
  !! acc coefficient for acceleration update
  !! acceleration = (1)*Un / dt^2 + acc(2) * Vn^2  + acc(3) * An + acc(4) * sol(1) / dt + ...
  !! acc(1) coefficient of displacement at time tn
  !! acc(2) coefficient of velocity at time tn
  !! acc(3) coefficient of acceleration at time tn
  !! acc(4:MAX_ORDER_TIME+4) coefficient of solution dof at time t1, t2, ...
  LOGICAL(LGT) :: acc_zero(MAX_ORDER_TIME + 4) = .TRUE.

  REAL(DFP) :: mt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1) = 0.0_DFP
  !! coefficient for mass matrix M
  !! This is equivalent to temporal convective matrix +
  !! jump contribution (mtplus)

  REAL(DFP) :: ct(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1) = 0.0_DFP
  !! coefficient for damping matrix C*dt
  !! This is equivalent to temporal mass matrix

  REAL(DFP) :: kt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1) = 0.0_DFP
  !! coefficient for stiffness matrix K * dt^2
  !! This is equivalent to T^t*\tilde{T}
  !! \tilde{T} is displacement-velocity function derived from
  !! the kinematical condition \dot{u}=v

  REAL(DFP) :: bt(MAX_ORDER_TIME + 1, 2 * MAX_ORDER_TIME + 2) = 0.0_DFP
  !! sub matrix used to derive kt

  REAL(DFP) :: bt_right(MAX_ORDER_TIME + 1) = 0.0_DFP
  !! bt at theta +1

  REAL(DFP) :: wt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: wmt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1) = 0.0_DFP
  !! transpose of wt*mt
  !! needed to make bt
  !! this matrix is made when we call GetWt

  REAL(DFP) :: at(MAX_ORDER_TIME + 1) = 0.0_DFP
  !! At matrix

  REAL(DFP) :: at_right = 0.0_DFP
  !! At at theta +1

  REAL(DFP) :: tat(MAX_ORDER_TIME + 1) = 0.0_DFP
  !! integral of shape function of time times at

  REAL(DFP) :: rhs_m_u1(MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: rhs_m_v1(MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: rhs_m_a1(MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: rhs_k_u1(MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: rhs_k_v1(MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: rhs_k_a1(MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: rhs_c_u1(MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: rhs_c_v1(MAX_ORDER_TIME + 1) = 0.0_DFP
  REAL(DFP) :: rhs_c_a1(MAX_ORDER_TIME + 1) = 0.0_DFP

  LOGICAL(LGT) :: rhs_m_u1_zero(MAX_ORDER_TIME + 1) = .TRUE.
  LOGICAL(LGT) :: rhs_m_v1_zero(MAX_ORDER_TIME + 1) = .TRUE.
  LOGICAL(LGT) :: rhs_m_a1_zero(MAX_ORDER_TIME + 1) = .TRUE.
  LOGICAL(LGT) :: rhs_k_u1_zero(MAX_ORDER_TIME + 1) = .TRUE.
  LOGICAL(LGT) :: rhs_k_v1_zero(MAX_ORDER_TIME + 1) = .TRUE.
  LOGICAL(LGT) :: rhs_k_a1_zero(MAX_ORDER_TIME + 1) = .TRUE.
  LOGICAL(LGT) :: rhs_c_u1_zero(MAX_ORDER_TIME + 1) = .TRUE.
  LOGICAL(LGT) :: rhs_c_v1_zero(MAX_ORDER_TIME + 1) = .TRUE.
  LOGICAL(LGT) :: rhs_c_a1_zero(MAX_ORDER_TIME + 1) = .TRUE.

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
END TYPE TDGAlgorithm2_

!----------------------------------------------------------------------------
!                                              IsInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-27
! summary: Check if the object is initiated

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(TDGAlgorithm2_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Set@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Initiate Newmark-Beta method

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, elemsd, facetElemsd)
    CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
    TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                MakeZeros@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Reset the TDGAlgorithm2_ object to zero values

INTERFACE
  MODULE SUBROUTINE obj_MakeZeros(obj)
    CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
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
    CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
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
    CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
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
    CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
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
    CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
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

END MODULE TDGAlgorithm2_Class
