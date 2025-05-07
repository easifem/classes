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

MODULE ElastoPlasticDynamics1DSDFEM_Class
USE Abstract1DSDFEM_Class, ONLY: Abstract1DSDFEM_, &
                                 Abstract1DSDDeallocate, &
                                 Abstract1DSDImportFromToml, &
                                 Abstract1DSDSet, &
                                 Abstract1DSDApplyDirichletBC, &
                                 Abstract1DSDUpdate, &
                                 Abstract1DSDWriteData, &
                                 Abstract1DSDAssembleRHS, &
                                 Abstract1DSDAssembleTanmat, &
                                 ElementDataImportFromToml

USE GlobalData, ONLY: DFP, I4B, LGT

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemshapeData_, &
                    poly => TypePolynomialOpt, &
                    qp => TypeQuadratureOpt, &
                    ip => TypeInterpolationOpt, &
                    CSRMatrix_, &
                    DOF_, &
                    RealVector_, &
                    iface_SpaceTimeFunction, &
                    iface_1DFunction

USE tomlf, ONLY: toml_table

USE FPL, ONLY: ParameterList_

USE TxtFile_Class, ONLY: TxtFile_

USE ExceptionHandler_Class, ONLY: e

USE String_Class, ONLY: String

USE UserFunction_Class, ONLY: UserFunction_

USE GnuPlot_Class, ONLY: GnuPlot_

USE CSVFile_Class, ONLY: CSVFile_

USE SDAlgorithms, ONLY: SDAlgoParam_

PRIVATE

PUBLIC :: ElastoPlasticDynamics1DSDFEM_

CHARACTER(*), PARAMETER :: modName = 'ElastoPlasticDynamics1DSDFEM_Class'
CHARACTER(*), PARAMETER :: prefix = "ElastoPlasticDyanmics1DSDFEM"
CHARACTER(*), PARAMETER :: default_result_dir = "./results"
CHARACTER(*), PARAMETER :: default_filename = "ElastoPlasticDyanmics1DSDFEM"
CHARACTER(*), PARAMETER :: default_baseInterpolationForSpace = "LAGR"
CHARACTER(*), PARAMETER :: default_timeIntegrationScheme = "NEWM"
CHARACTER(*), PARAMETER :: default_baseTypeForSpace = "Monomial"
CHARACTER(*), PARAMETER :: default_ipTypeForSpace = "Equidistance"
CHARACTER(*), PARAMETER :: default_quadTypeForSpace = "GaussLegendre"
INTEGER(I4B), PARAMETER :: MAX_ORDER_SPACE = 10
INTEGER(I4B), PARAMETER :: default_verbosity = 0

!----------------------------------------------------------------------------
!                                                   ElastoPlasticDynamics1DSDFEM_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-19
! summary:  Define ElastoPlasticDynamics1DSDFEM_

TYPE, EXTENDS(Abstract1DSDFEM_) :: ElastoPlasticDynamics1DSDFEM_

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

  INTEGER(I4B) :: maxNIPSpace

  TYPE(RealVector_) :: rhsf, sol0
  ! the force vector which remains constant during the time step
  ! NOTE: residual vector is directly stored in rhs

  TYPE(RealVector_), ALLOCATABLE :: pstrain0(:), tstrain0(:), &
                                    pparam0(:), stress0(:)
  !! Plastic strain, total strain, plastic parameter and stress
  !! converged at the previous time step
  !! the size is the same as the number of elements
  !! the size of each RealVector_ is the same as
  !! the number of quadrature points in space element

  TYPE(RealVector_), ALLOCATABLE :: pstrain(:), tstrain(:), &
                                    pparam(:), stress(:)
  !! Plastic strain, total strain, plastic parameter and stress
  !! current time step to be converged
  !! the sizes are the same as the above

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
  !! Deallocate

  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import data from toml file

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! set the problem

  ! PROCEDURE, PUBLIC, PASS(obj) :: SetInitialVelocity => &
  ! obj_SetInitialVelocity

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleTanmat => obj_AssembleTanmat

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHSF => obj_AssembleRHSF

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHS => obj_AssembleRHS

  PROCEDURE, PUBLIC, PASS(obj) :: UpdateQPVariables &
    => obj_UpdateQPVariables
  !! Update

  PROCEDURE, PUBLIC, PASS(obj) :: Update => obj_Update

  PROCEDURE, PUBLIC, PASS(obj) :: Run => obj_Run
  !! Debug mode

  PROCEDURE, PUBLIC, PASS(obj) :: WriteData => obj_WriteData
  !! write data

END TYPE ElastoPlasticDynamics1DSDFEM_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-02-11
! summary:  abstract interface to implement user subroutine

ABSTRACT INTERFACE
  SUBROUTINE SetQPValue_(obj, spaceElemNum, stress, stress0, tstrain, &
                         tstrain0, pstrain, pstrain0, pparam, pparam0, ans)
    IMPORT ElastoPlasticDynamics1DSDFEM_
    IMPORT I4B, DFP
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(inout) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    REAL(DFP), INTENT(INOUT) :: stress, tstrain, pstrain, pparam
    REAL(DFP), OPTIONAL, INTENT(IN) :: stress0, tstrain0, pstrain0, pparam0
    REAL(DFP), OPTIONAL, INTENT(OUT) :: ans
  END SUBROUTINE SetQPValue_
END INTERFACE

!----------------------------------------------------------------------------
!                              -                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-02
! summary:  Initiate by param

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-02
! summary:  Deallocate the data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-02-11
! summary:  Import data from toml config

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-02
! summary:  Set the problem

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                     AssembleTanmat@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_AssembleTanmat(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
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
  MODULE SUBROUTINE obj_AssembleRHSF(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleRHSF
END INTERFACE

!----------------------------------------------------------------------------
!                                                        AssembleRHS@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-12
! summary:  Assemble RHS

INTERFACE
  MODULE SUBROUTINE obj_AssembleRHS(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Debug@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Debug mode

INTERFACE
  MODULE SUBROUTINE obj_Run(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Run
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Solve@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-09-19
! summary:  Set initial displacement

INTERFACE
  MODULE SUBROUTINE obj_Solve(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Update@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-02-11
! summary:  Update the quadrature point values

INTERFACE
  MODULE SUBROUTINE obj_UpdateQPVariables(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_UpdateQPVariables
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Update@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-09-19
! summary:  Set initial displacement

INTERFACE
  MODULE SUBROUTINE obj_Update(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Update
END INTERFACE

!----------------------------------------------------------------------------
!                                                         WriteData@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_WriteData(obj)
    CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_WriteData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ElastoPlasticDynamics1DSDFEM_Class
