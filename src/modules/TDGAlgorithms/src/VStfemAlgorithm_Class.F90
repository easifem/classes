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

MODULE VStfemAlgorithm_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: math => TypeMathOpt
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
IMPLICIT NONE
PRIVATE

PUBLIC :: VStfemAlgorithm_

!----------------------------------------------------------------------------
!                                                           VStfemAlgorithm_
!----------------------------------------------------------------------------

!> author: Vikas Sharma
! date: 2025-12-12
! summary:  Displacement based time discontinuous Galerkin algorithm
!
!# VStfemAlgorithm_
!
! This is like TDGAlgorithm3_ but in this case stiffMatCoeff is not
! identity

TYPE :: VStfemAlgorithm_
  LOGICAL(LGT) :: isInit = math%no
  !! Flag to check if the object is initiated
  INTEGER(I4B) :: nrow = math%zero_i, ncol = math%zero_i
  !! Number of rows and columns in ct, mt, mtplus matrices
  LOGICAL(LGT), ALLOCATABLE, DIMENSION(:) :: rhs_m_u1_zero, &
                                             rhs_k_u1_zero, &
                                             rhs_c_u1_zero, &
                                             rhs_m_v1_zero, &
                                             rhs_k_v1_zero, &
                                             rhs_c_v1_zero, &
                                             initialGuess_zero, &
                                             jumpDis_zero, &
                                             jumpVel_zero, &
                                             dis_zero, &
                                             vel_zero, &
                                             acc_zero
  REAL(DFP), ALLOCATABLE, DIMENSION(:) :: rhs_m_u1, rhs_k_u1, rhs_c_u1, &
                                          rhs_m_v1, rhs_k_v1, rhs_c_v1, &
                                          initialGuess, jumpDis, jumpVel, &
                                          dis, vel, acc
  !! initialGuess: coefficient for initial guess of solution
  !! jumpDis: coefficient for computing jump of displacement
  !!          jumpDisp = jumpDis(1)*Un+jumpDis(2)*Vn*dt +jumpDis(3)*An*dt^2 &
  !!                   + jumpDis(4)*sol(1) + ...
  !!          jumpDis(1) coefficient of displacement at time tn
  !!          jumpDis(2) coefficient of velocity at time tn
  !!          jumpDis(3) coefficient of acceleration at time tn
  !!          jumpDis(4:MAX_ORDER_TIME+4) coefficient of solution dof at time
  !!          t1, t2, ...
  !! jumpVel: coefficient for computing jump of displacement
  !!          jumpVel = jumpVel(1)*Un+jumpVel(2)*Vn*dt +jumpVel(3)*An*dt^2 &
  !!                   + jumpVel(4)*sol(1) + ...
  !!          jumpVel(1) coefficient of displacement at time tn
  !!          jumpVel(2) coefficient of velocity at time tn
  !!          jumpVel(3) coefficient of acceleration at time tn
  !!          jumpVel(4:MAX_ORDER_TIME+4) coefficient of solution dof at time
  !!          t1, t2, ...
  !! dis: coefficient for displacement update
  !!     displacement = dis(1)*Un+dis(2)*Vn*dt +dis(3)*An*dt^2 &
  !!                  + dis(4)*sol(1) * dt + ...
  !!     dis(1) coefficient of displacement at time tn
  !!     dis(2) coefficient of velocity at time tn
  !!     dis(3) coefficient of acceleration at time tn
  !!     dis(4:MAX_ORDER_TIME+4) coefficient of solution dof at t1, t2, ...
  !! vel: coefficient for velocity update
  !!      velocity = vel(1)*Un / dt + vel(2) * Vn  + vel(3)*An *dt &
  !!               + vel(4) * sol(1) + ...
  !!      vel(1) coefficient of displacement at time tn
  !!      vel(2) coefficient of velocity at time tn
  !!      vel(3) coefficient of acceleration at time tn
  !!      vel(4:MAX_ORDER_TIME+4) coefficient of solution dof at t1, t2, ...
  !! acc: coefficient for acceleration update
  !!      acceleration = (1)*Un / dt^2 + acc(2) * Vn^2  + acc(3) * An &
  !!                   + acc(4) * sol(1) / dt + ...
  !!      acc(1) coefficient of displacement at time tn
  !!      acc(2) coefficient of velocity at time tn
  !!      acc(3) coefficient of acceleration at time tn
  !!      acc(4:MAX_ORDER_TIME+4) coefficient of solution dof at t1, t2, ...
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: massMatCoeff, dampMatCoeff, &
                                             stiffMatCoeff, forceCoeff
  !! mt: (T,dTdt)_In + TnxTn
  !! kt: (T,T)_In
  !! invKt: inverse of kt matrix
  !! massMatCoeff: coefficient for mass matrix M
  !! dampMatCoeff: coefficient for damping matrix C
  !! stiffMatCoeff: coefficient for stiffness matrix K
  !! forceCoeff: coefficient for external force vector
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: AllocateData => obj_AllocateData
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
END TYPE VStfemAlgorithm_

!----------------------------------------------------------------------------
!                                                        IsInitiated@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-27
! summary: Check if the object is initiated

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(VStfemAlgorithm_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                       AllocateData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: AllocateData Newmark-Beta method

INTERFACE
  MODULE SUBROUTINE obj_AllocateData(obj, nnt)
    CLASS(VStfemAlgorithm_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nnt
  END SUBROUTINE obj_AllocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Initiate Newmark-Beta method

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, elemsd, facetElemsd, alpha, scalingOpt)
    CLASS(VStfemAlgorithm_), INTENT(INOUT) :: obj
    TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    CHARACTER(1), OPTIONAL, INTENT(IN) :: scalingOpt
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          MakeZeros@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Reset the VStfemAlgorithm_ object to zero values

INTERFACE
  MODULE SUBROUTINE obj_MakeZeros(obj)
    CLASS(VStfemAlgorithm_), INTENT(INOUT) :: obj
    ! internal varibales
    REAL(DFP), PARAMETER :: myzero = 0.0_DFP
  END SUBROUTINE obj_MakeZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(VStfemAlgorithm_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary:  Deallocate the object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(VStfemAlgorithm_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VStfemAlgorithm_Class
