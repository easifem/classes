! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

MODULE ElastoPlasticDynamics1DVSTFEM_Class
USE Abstract1DSTFEM_Class, ONLY: Abstract1DSTFEM_, &
                                 Abstract1DSTDeallocate, &
                                 Abstract1DSTImportFromToml, &
                                 Abstract1DSTSet, &
                                 Abstract1DSTSetInitialVelocity, &
                                 Abstract1DSTApplyDirichletBC, &
                                 Abstract1DSTUpdate, &
                                 Abstract1DSTWriteData, &
                                 Abstract1DSTAssembleRHS, &
                                 Abstract1DSTAssembleTanmat, &
                                 ElementDataImportFromToml
USE GlobalData, ONLY: DFP, I4B, LGT

USE BaseType, ONLY: RealVector_, &
                    RealMatrix_, &
                    QuadraturePoint_, &
                    ElemShapeData_

USE FPL, ONLY: ParameterList_

USE TxtFile_Class, ONLY: TxtFile_

USE ExceptionHandler_Class, ONLY: e

USE String_Class, ONLY: String

USE CSVFile_Class, ONLY: CSVFile_

USE GnuPlot_Class, ONLY: GnuPlot_

USE tomlf, ONLY: toml_table

PRIVATE

PUBLIC :: ElastoPlasticDynamics1DVSTFEM_

CHARACTER(*), PARAMETER :: modName = 'ElastoPlasticDynamics1DVSTFEM_Class'
CHARACTER(*), PARAMETER :: prefix = "ElastoPlasticDynamics1DSTFEM"
CHARACTER(*), PARAMETER :: default_result_dir = "./results"
CHARACTER(*), PARAMETER :: default_filename = "ElastoPlasticDynamics1DVSTFEM"
CHARACTER(*), PARAMETER :: default_baseInterpolationForSpace = "LAGR"
CHARACTER(*), PARAMETER :: default_baseInterpolationForTime = "LAGR"
CHARACTER(*), PARAMETER :: default_baseTypeForSpace = "Monomial"
CHARACTER(*), PARAMETER :: default_baseTypeForTime = "Monomial"
CHARACTER(*), PARAMETER :: default_ipTypeForSpace = "Equidistance"
CHARACTER(*), PARAMETER :: default_ipTypeForTime = "Equidistance"
CHARACTER(*), PARAMETER :: default_quadTypeForSpace = "GaussLegendre"
CHARACTER(*), PARAMETER :: default_quadTypeForTime = "GaussLegendre"
CHARACTER(*), PARAMETER :: default_plasticityType = "ILH"
INTEGER(I4B), PARAMETER :: MAX_ORDER_SPACE = 10
INTEGER(I4B), PARAMETER :: MAX_ORDER_TIME = 10
INTEGER(I4B), PARAMETER :: default_verbosity = 0

!----------------------------------------------------------------------------
!                                                   ElastoPlasticDynamics1DVSTFEM_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Abstract1DSTFEM_) :: ElastoPlasticDynamics1DVSTFEM_

  LOGICAL(LGT) :: converged = .FALSE.

  LOGICAL(LGT) :: updateTanmat = .FALSE.
  !! whether pure Newton-Raphson method or
  !! modified Newton-Raphson method is used

  REAL(DFP) :: toleranceForNR = 1.0D-8

  INTEGER(I4B) :: currentNRStep = 0

  INTEGER(I4B), ALLOCATABLE :: NRConvergedSteps(:)

  REAL(DFP) :: currentResidualNorm = 0.0_DFP

  REAL(DFP) :: currentExternalForceNorm = 0.0_DFP

  INTEGER(I4B) :: maxIterNumNR = 10_I4B

  INTEGER(I4B) :: totalQuadPointsForSpace
  !! total number of quadrature points in space elements
  !! this variable will be moved to the Abstract class soon
  ! WARN: In case of GL quadrature points,
  ! duplicate happens. Currently it is not considered

  INTEGER(I4B) :: maxNIPSpace, maxNIPTime

  !! NOTE: it can be expected that we can improve the computational time
  !! by storing Space-Time Mass matrix to calculate residual
  !! however this is implemented in the future

  TYPE(RealVector_) :: rhsf, u_theta, sol0
  ! the force vector which remains constant during the time step
  ! NOTE: residual vector is directly stored in rhs

  TYPE(RealVector_), ALLOCATABLE :: pstrain0(:), tstrain0(:), &
                                    pparam0(:), stress0(:)
  !! Plastic strain, total strain, plastic parameter and stress
  !! the size is the same as the number of elements
  !! the size of each RealVector_ is the same as
  !! the number of quadrature points in space element
  !! Note that plastic parameter can be used to
  !! represent the kinematic hardening parameter or
  !! nonlinear hardening parameter etc
  !! depending on the model of plasticity

  TYPE(RealMatrix_), ALLOCATABLE :: pstrain(:), tstrain(:), &
                                    pparam(:), stress(:)
  !! Stress and Strain values at space-time quadrature points
  !! The size is the same as the number of elements
  !! The size of each RealMatrix_ is the same as
  !! the number of quadrature points in space-time element
  !!

  CHARACTER(4) :: plasticityType = "ILH"
  !! Type of elasto-plasticity
  !! Currently linear elasticity is assumed
  !! following values are allowed
  !! ILH, KLH, INH, KNH
  !! isotropic linear hardening, kinematic linear hardening
  !! isotropic nonlinear hardening, kinematic nonlinear hardening

  REAL(DFP), ALLOCATABLE :: plasticModulus(:), yieldStress(:), &
                            extraPlasticParam1(:), extraPlasticParam2(:)
  !! plastic modulus and etc. for each element

  LOGICAL(LGT) :: saveQPData(3) = .FALSE.
  LOGICAL(LGT) :: plotQPData(3) = .FALSE.

  TYPE(CSVFile_) :: stressfile, tstrainfile, pstrainfile

  TYPE(QuadraturePoint_) :: intQuadForTime
  TYPE(ElemShapeData_) :: intElemSDForTime

  PROCEDURE(SetQPValue_), POINTER, NOPASS :: UserReturnMapping => NULL()
  PROCEDURE(SetQPValue_), POINTER, NOPASS :: UserGetTangentModulus => NULL()

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate data

  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import data from toml file

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! set the problem

  ! PROCEDURE, PUBLIC, PASS(obj) :: SetInitialVelocity => &
  ! obj_SetInitialVelocity

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleTanmat => obj_AssembleTanmat

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHSF => obj_AssembleRHSF

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHS => obj_AssembleRHS

  PROCEDURE, PUBLIC, PASS(obj) :: GetMt => obj_GetMt
  !! Get Mt matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetMtPlus => obj_GetMtPlus
  !! Get MtPlus matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetCt => obj_GetCt
  !! Get Ct matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetWt => obj_GetWt
  !! Get Wt matrix
  !! It needs Ct, Mt, Mtplus

  PROCEDURE, PUBLIC, PASS(obj) :: GetAt => obj_GetAt
  !! Get At matrix
  !! it needs wt

  PROCEDURE, PUBLIC, PASS(obj) :: GetBt => obj_GetBt
  !! Get Bt matrix
  !! It needs Wt

  PROCEDURE, PUBLIC, PASS(obj) :: GetKt_Tilda => obj_GetKt_Tilda

  PROCEDURE, PUBLIC, PASS(obj) :: UpdateQPVariables &
    => obj_UpdateQPVariables
  !! Update

  PROCEDURE, PUBLIC, PASS(obj) :: Update &
    => obj_Update

  PROCEDURE, PUBLIC, PASS(obj) :: Run => obj_Run
  !! Debug mode

  PROCEDURE, PUBLIC, PASS(obj) :: WriteData => obj_WriteData
  !! write data

END TYPE ElastoPlasticDynamics1DVSTFEM_

!----------------------------------------------------------------------------
!                              -                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-10
! summary:  Initiate the class

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                              -                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-10
! summary:  Deallocate the data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-10
! summary:  Import data from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-10
! summary:  Set the variables

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetCt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-03
! summary:  Get Ct matrix (see notes)

INTERFACE
  MODULE SUBROUTINE obj_GetCt(obj, ans, nrow, ncol)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetCt
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetMt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-03
! summary:  Get Mt matrix (see notes)

INTERFACE
  MODULE SUBROUTINE obj_GetMt(obj, ans, nrow, ncol)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMt
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetMtPlus@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-03
! summary:  get MtPlus matrix (see notes), this matrix is due to the jump

INTERFACE
  MODULE SUBROUTINE obj_GetMtPlus(obj, ans, nrow, ncol)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMtPlus
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetAt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-05
! summary:  It is coefficient of Un in disp-vel relation
!
!# Introduction
!
! Make sure ct, mtplus, timeShapeFunc0 is calculated before calling this

INTERFACE
  MODULE SUBROUTINE obj_GetAt(obj)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_GetAt
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetBt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-05
! summary:  It is coefficient of Vn in disp-vel relation

INTERFACE
  MODULE SUBROUTINE obj_GetBt(obj)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_GetBt
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetWt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-07
! summary:  Wt matrix (see notes)

INTERFACE
  MODULE SUBROUTINE obj_GetWt(obj, ans, nrow, ncol)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetWt
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetKt_Tilda(obj, ans, nrow, ncol)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetKt_Tilda
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-19
! summary:  Set T_tilde shape functions

! INTERFACE
!   MODULE SUBROUTINE obj_SetT_tilde(obj, timeElemNum)
!     CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: timeElemNum
!   END SUBROUTINE obj_SetT_tilde
! END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetInitialVelocity@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-17
! summary:  Set initial velocity

! INTERFACE
!   MODULE SUBROUTINE obj_SetInitialVelocity(obj)
!     CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_SetInitialVelocity
! END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-11
! summary:  Set the variables

INTERFACE
  MODULE SUBROUTINE obj_AssembleTanmat(obj, timeElemNum, tij)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
    REAL(DFP), INTENT(IN) :: tij(1, 2)
  END SUBROUTINE obj_AssembleTanmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                               AssembleRHSF
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-12
! summary:  assemble external force term which is
! constant in time step

INTERFACE
  MODULE SUBROUTINE obj_AssembleRHSF(obj, timeElemNum)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
  END SUBROUTINE obj_AssembleRHSF
END INTERFACE

!----------------------------------------------------------------------------
!                                                               AssembleRHSF
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-12
! summary:  assemble external force term which is
! constant in time step

INTERFACE
  MODULE SUBROUTINE obj_AssembleRHS(obj, timeElemNum, tij)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
    REAL(DFP), INTENT(IN) :: tij(1, 2)
  END SUBROUTINE obj_AssembleRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetTangentModulus
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-17
! summary:  Abstract interface for return mapping and
! get tangent modulus

ABSTRACT INTERFACE
  SUBROUTINE SetQPValue_(obj, spaceElemNum, stress, stress0, tstrain, &
                         tstrain0, pstrain, pstrain0, pparam, pparam0, ans)
    IMPORT ElastoPlasticDynamics1DVSTFEM_
    IMPORT I4B, DFP
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(inout) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    REAL(DFP), INTENT(INOUT) :: stress, tstrain, pstrain, pparam
    REAL(DFP), OPTIONAL, INTENT(IN) :: stress0, tstrain0, pstrain0, pparam0
    REAL(DFP), OPTIONAL, INTENT(OUT) :: ans
  END SUBROUTINE SetQPValue_
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Debug@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-10
! summary:  Run the simulation

INTERFACE
  MODULE SUBROUTINE obj_Run(obj)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Run
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Update@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-10
! summary:  Update the stress

INTERFACE
  MODULE SUBROUTINE obj_UpdateQPVariables(obj)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_UpdateQPVariables
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Update
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-12-11
! summary:  Update the fields for next time step

INTERFACE
  MODULE SUBROUTINE obj_Update(obj)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Update
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_WriteData(obj)
    CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_WriteData
END INTERFACE

END MODULE ElastoPlasticDynamics1DVSTFEM_Class
