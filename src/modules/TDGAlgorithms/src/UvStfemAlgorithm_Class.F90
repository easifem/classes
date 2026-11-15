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

MODULE UvStfemAlgorithm_Class
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

PUBLIC :: UvStfemAlgorithm_

INTEGER(I4B), PARAMETER :: MAX_ORDER_TIME = 20
INTEGER(I4B), PARAMETER :: MAX_NNT = MAX_ORDER_TIME + 1

!----------------------------------------------------------------------------
!                                                          UvStfemAlgorithm_
!----------------------------------------------------------------------------

!> author: Vikas Sharma
! date: 2025-12-12
! summary:  UV, two field based time discontinuous Galerkin algorithm

TYPE :: UvStfemAlgorithm_
  LOGICAL(LGT) :: isInit = math%no
  !! Flag to check if the object is initiated
  INTEGER(I4B) :: nrow = math%zero_i, ncol = math%zero_i
  !! Number of rows and columns in ct, mt, mtplus matrices
  REAL(DFP) :: initialGuess(MAX_NNT + 3) = math%zero
  !! coefficient for initial guess of solution
  LOGICAL(LGT) :: initialGuess_zero(MAX_NNT + 3) = math%yes
  REAL(DFP) :: jumpDis(MAX_NNT + 3) = math%zero
  !! coefficient for computing jump of displacement
  !! jumpDisp = jumpDis(1)*Un+jumpDis(2)*Vn*dt +jumpDis(3)*An*dt^2 &
  !!          + jumpDis(4)*sol(1) + ...
  !! jumpDis(1) coefficient of displacement at time tn
  !! jumpDis(2) coefficient of velocity at time tn
  !! jumpDis(3) coefficient of acceleration at time tn
  !! jumpDis(4:MAX_ORDER_TIME+4) coefficient of solution dof at time
  !! t1, t2, ...
  LOGICAL(LGT) :: jumpDis_zero(MAX_ORDER_TIME + 4) = math%yes
  !! flag for zeros for jumpDis
  REAL(DFP) :: jumpVel(MAX_NNT + 3) = math%zero
  !! coefficient for computing jump of displacement
  !! jumpVelp = jumpVel(1)*Un+jumpVel(2)*Vn*dt +jumpVel(3)*An*dt^2 &
  !!          + jumpVel(4)*sol(1) + ...
  !! jumpVel(1) coefficient of displacement at time tn
  !! jumpVel(2) coefficient of velocity at time tn
  !! jumpVel(3) coefficient of acceleration at time tn
  !! jumpVel(4:MAX_ORDER_TIME+4) coefficient of solution dof at time
  !! t1, t2, ...
  LOGICAL(LGT) :: jumpVel_zero(MAX_NNT + 3) = math%yes
  !! flag for zeros for jumpVel
  REAL(DFP) :: dis(MAX_NNT + 3) = math%zero
  !! dis: coefficient for displacement update
  !! displacement = dis(1)*Un+dis(2)*Vn*dt +dis(3)*An*dt^2 &
  !!              + dis(4)*sol(1) * dt + ...
  !! dis(1) coefficient of displacement at time tn
  !! dis(2) coefficient of velocity at time tn
  !! dis(3) coefficient of acceleration at time tn
  !! dis(4:MAX_ORDER_TIME+4) coefficient of solution dof at time t1, t2, ...
  LOGICAL(LGT) :: dis_zero(MAX_NNT + 3) = math%yes
  !! flag for zeros for dis
  REAL(DFP) :: vel(MAX_NNT + 3) = math%zero
  !! vel coefficient for velocity update
  !! velocity = vel(1)*Un / dt + vel(2) * Vn  + vel(3)*An *dt &
  !!          + vel(4) * sol(1) + ...
  !! vel(1) coefficient of displacement at time tn
  !! vel(2) coefficient of velocity at time tn
  !! vel(3) coefficient of acceleration at time tn
  !! vel(4:MAX_ORDER_TIME+4) coefficient of solution dof at time t1, t2, ...
  LOGICAL(LGT) :: vel_zero(MAX_NNT + 3) = math%yes
  !! flag for zeros for vel
  REAL(DFP) :: acc(MAX_NNT + 3) = math%zero
  !! acc coefficient for acceleration update
  !! acceleration = (1)*Un / dt^2 + acc(2) * Vn^2  + acc(3) * An &
  !!              + acc(4) * sol(1) / dt + ...
  !! acc(1) coefficient of displacement at time tn
  !! acc(2) coefficient of velocity at time tn
  !! acc(3) coefficient of acceleration at time tn
  !! acc(4:MAX_ORDER_TIME+4) coefficient of solution dof at time t1, t2, ...
  LOGICAL(LGT) :: acc_zero(MAX_NNT + 3) = math%yes
  !! flag for zeros for acc
  REAL(DFP), DIMENSION(MAX_NNT, MAX_NNT) :: mt = math%zero, &
                                            kt = math%zero, &
                                            invKt = math%zero, &
                                            massMatCoeff = math%zero, &
                                            dampMatCoeff = math%zero, &
                                            stiffMatCoeff = math%zero
  !! mt: (T,dTdt)_In + TnxTn
  !! kt: (T,T)_In
  !! invKt: inverse of kt matrix
  !! massMatCoeff: coefficient for mass matrix M
  !! dampMatCoeff: coefficient for damping matrix C
  !! stiffMatCoeff: coefficient for stiffness matrix K
  REAL(DFP) :: rhs_m_u1(MAX_NNT) = math%zero
  REAL(DFP) :: rhs_m_v1(MAX_NNT) = math%zero
  REAL(DFP) :: rhs_m_a1(MAX_NNT) = math%zero
  REAL(DFP) :: rhs_k_u1(MAX_NNT) = math%zero
  REAL(DFP) :: rhs_k_v1(MAX_NNT) = math%zero
  REAL(DFP) :: rhs_k_a1(MAX_NNT) = math%zero
  REAL(DFP) :: rhs_c_u1(MAX_NNT) = math%zero
  REAL(DFP) :: rhs_c_v1(MAX_NNT) = math%zero
  REAL(DFP) :: rhs_c_a1(MAX_NNT) = math%zero

  LOGICAL(LGT) :: rhs_m_u1_zero(MAX_NNT) = math%yes
  LOGICAL(LGT) :: rhs_m_v1_zero(MAX_NNT) = math%yes
  LOGICAL(LGT) :: rhs_m_a1_zero(MAX_NNT) = math%yes
  LOGICAL(LGT) :: rhs_k_u1_zero(MAX_NNT) = math%yes
  LOGICAL(LGT) :: rhs_k_v1_zero(MAX_NNT) = math%yes
  LOGICAL(LGT) :: rhs_k_a1_zero(MAX_NNT) = math%yes
  LOGICAL(LGT) :: rhs_c_u1_zero(MAX_NNT) = math%yes
  LOGICAL(LGT) :: rhs_c_v1_zero(MAX_NNT) = math%yes
  LOGICAL(LGT) :: rhs_c_a1_zero(MAX_NNT) = math%yes

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
END TYPE UvStfemAlgorithm_

!----------------------------------------------------------------------------
!                                                        IsInitiated@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-27
! summary: Check if the object is initiated

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(UvStfemAlgorithm_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Initiate Newmark-Beta method

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, elemsd, facetElemsd, alpha)
    CLASS(UvStfemAlgorithm_), INTENT(INOUT) :: obj
    TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          MakeZeros@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Reset the UvStfemAlgorithm_ object to zero values

INTERFACE
  MODULE SUBROUTINE obj_MakeZeros(obj)
    CLASS(UvStfemAlgorithm_), INTENT(INOUT) :: obj
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
    CLASS(UvStfemAlgorithm_), INTENT(INOUT) :: obj
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
    CLASS(UvStfemAlgorithm_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UvStfemAlgorithm_Class
