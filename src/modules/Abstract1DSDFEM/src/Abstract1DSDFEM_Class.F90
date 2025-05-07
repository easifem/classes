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

MODULE Abstract1DSDFEM_Class
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

USE tomlf, ONLY: toml_table, &
                 toml_serialize, &
                 toml_get => get_value, &
                 toml_stat, toml_array, &
                 toml_len => len

USE FPL, ONLY: ParameterList_

USE TxtFile_Class, ONLY: TxtFile_

USE ExceptionHandler_Class, ONLY: e

USE Display_Method, ONLY: Display, ToString

USE String_Class, ONLY: String

USE UserFunction_Class, ONLY: UserFunction_

USE GnuPlot_Class, ONLY: GnuPlot_

USE CSVFile_Class, ONLY: CSVFile_

USE SDAlgorithms, ONLY: SDAlgoParam_

USE ReallocateUtility, ONLY: Reallocate

PRIVATE

PUBLIC :: Abstract1DSDFEM_
PUBLIC :: Abstract1DSDDeallocate
PUBLIC :: Abstract1DSDImportFromToml
PUBLIC :: Abstract1DSDWriteData
PUBLIC :: Abstract1DSDSet
PUBLIC :: Abstract1DSDUpdate
! PUBLIC :: Abstract1DSDSetInitialVelocity
PUBLIC :: Abstract1DSDApplyDirichletBC
PUBLIC :: Abstract1DSDAssembleTanmat
PUBLIC :: Abstract1DSDAssembleRHS
PUBLIC :: ElementDataImportFromToml

CHARACTER(*), PARAMETER :: modName = 'Abstract1DSDFEM_Class'
CHARACTER(*), PARAMETER :: prefix = "Abstract1DSDFEM"
CHARACTER(*), PARAMETER :: default_result_dir = "./results"
CHARACTER(*), PARAMETER :: default_filename = "Abstract1DSDFEM"
CHARACTER(*), PARAMETER :: default_baseInterpolationForSpace = "LAGR"
CHARACTER(*), PARAMETER :: default_timeIntegrationScheme = "NEWM"
CHARACTER(*), PARAMETER :: default_baseTypeForSpace = "Monomial"
CHARACTER(*), PARAMETER :: default_ipTypeForSpace = "Equidistance"
CHARACTER(*), PARAMETER :: default_quadTypeForSpace = "GaussLegendre"
INTEGER(I4B), PARAMETER :: MAX_ORDER_SPACE = 10
INTEGER(I4B), PARAMETER :: default_verbosity = 0

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

INTERFACE ElementDataImportFromToml
  MODULE PROCEDURE ElementDataImportFromToml_real
  MODULE PROCEDURE ElementDataImportFromToml_int
END INTERFACE ElementDataImportFromToml

!----------------------------------------------------------------------------
!                                                   Abstract1DSDFEM_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-02-11
! summary:  Define Abstract1DSDFEM_

TYPE, ABSTRACT :: Abstract1DSDFEM_
  TYPE(SDAlgoParam_) :: algoParam
  LOGICAL(LGT) :: isConnectivity = .FALSE.
  !! is connectivity matrix is calculated
  CHARACTER(2) :: baseContinuityForSpace = "H1"
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG
  CHARACTER(4) :: baseInterpolationForSpace = "LAGR"
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation ! HierarchyInterpolation
  !! OrthogonalInterpolation ! HermitInterpolation
  !! SerendipityInterpolation
  CHARACTER(4) :: timeIntegrationScheme = "NEWM"

  INTEGER(I4B) :: verbosity = 0
  !! verbosity level
  !! 0 means minimum

  INTEGER(I4B) :: totalSpaceNodes = 0
  !! total nodes in space

  INTEGER(I4B) :: totalSpaceElements = 0
  !! total elements in space

  INTEGER(I4B) :: totalTimeSteps = 0
  !! total time steps

  INTEGER(I4B) :: totalVertexDOFSpace = 0
  !! Total number of degree of freedom in space

  INTEGER(I4B) :: totalEdgeDOFSpace = 0
  !! total number of degree of freedom in space

  INTEGER(I4B) :: baseTypeForSpace = poly%monomial
  !! basisType for space
  !! It is used for LagrangeInterpolation

  INTEGER(I4B) :: ipTypeForSpace = ip%Equidistance
  !! interpolation point type for space
  !! It is used for LagrangeInpolation

  INTEGER(I4B) :: quadTypeForSpace = qp%GaussLegendre
  !! quadrature type for space

  INTEGER(I4B) :: maxSpaceOrder = 0
  !! maximum space order

  INTEGER(I4B) :: currentTimeStep = 1
  !! current time step

  REAL(DFP) :: currentTime = 0.0_DFP
  !! current time

  REAL(DFP) :: spaceDomain(2) = 0.0_DFP
  !! Length of the space domain

  REAL(DFP) :: timeRange(2) = 0.0_DFP
  !! Total length of time domain

  INTEGER(I4B), ALLOCATABLE :: spaceOrder(:)
  !! space order of each element

  INTEGER(I4B), ALLOCATABLE :: totalDOFSpace(:)
  !! total number of degree of freedom in space in space element

  REAL(DFP), ALLOCATABLE :: spaceElemLength(:)
  !! length of each space element
  !! the size should be totalSpaceElements

  REAL(DFP), ALLOCATABLE :: timeStepSize(:)
  !! size of each time step
  !! the size should be totalTimeSteps

  REAL(DFP), ALLOCATABLE :: elasticModulus(:)
  !! elastic modulus
  !! the size should be tElements

  REAL(DFP), ALLOCATABLE :: density(:)
  !! density
  !! the size should be tElements

  REAL(DFP), ALLOCATABLE :: rayleighAlpha(:)
  !! Rayleigh damping coefficient

  REAL(DFP), ALLOCATABLE :: rayleighBeta(:)
  !! Rayleigh damping coefficient

  TYPE(String) :: result_dir
  !! Result directory name

  TYPE(String) :: filename
  !! Filename

  TYPE(QuadraturePoint_) :: quadForSpace
  !! Quadrature points in space

  TYPE(ElemshapeData_) :: linElemsdForSpace
  !! linearElement shape data for space

  TYPE(ElemshapeData_) :: elemsdForSpace
  !! Element shape data for space

  REAL(DFP) :: spaceShapeFuncBndy(MAX_ORDER_SPACE + 1, 2)
  !! Value of space shape function at xi = 1

  REAL(DFP) :: ks(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space stiffness matrix

  REAL(DFP) :: ms(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space mass matrix

  REAL(DFP) :: cs(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space damping matrix

  REAL(DFP) :: rhse(MAX_ORDER_SPACE + 1)

  INTEGER(I4B), ALLOCATABLE :: subIndices(:)
  !! subindices, used when getting submatrix
  !! while apply dirichlet boudnary condition

  TYPE(DOF_) :: dof

  TYPE(CSRMatrix_) :: tanmat, submat
  !! tangent matrix

  TYPE(RealVector_) :: rhs, sol, u0, v0, a0, &
                       force1, force2, tmp1
  !! Right hand side, solution, initial displacement, initial
  !! velocity
  !! These are all degree of freedom in general
  !! Size of all vectors is same as the size of space dof

  INTEGER(I4B), ALLOCATABLE :: conIA(:), conJA(:)
  !!

  INTEGER(I4B) :: dbc_idof(2)
  !! internal use only
  !! used in ApplyDirichletBCLeft

  REAL(DFP) :: dbc_coeff(2)
  !! internal use only
  !! used in ApplyDirichletBCLeft

  REAL(DFP) :: dbc_rhs(1)
  !! internal use only
  !! used in ApplyDirichletBCLeft

  REAL(DFP) :: dbc_value(2)
  !! internal use only
  !! used in ApplyDirichletBCLeft

  INTEGER(I4B), ALLOCATABLE :: ipar(:)
  !! for linear solver

  REAL(DFP), ALLOCATABLE :: fpar(:), work(:)
  !! for linear solver

  TYPE(GnuPlot_) :: plot
  !! for plotting

  TYPE(UserFunction_), POINTER :: bodyForce => NULL()
  !! body force

  TYPE(UserFunction_), POINTER :: tractionRight => NULL()
  !! traction force on right boundary

  TYPE(UserFunction_), POINTER :: tractionLeft => NULL()
  !! traction force on left boundary

  TYPE(UserFunction_), POINTER :: displacementRight => NULL()
  !! displacement boundary condition on right boundary

  TYPE(UserFunction_), POINTER :: displacementLeft => NULL()
  !! displacement boundarty condition on left boundary

  TYPE(UserFunction_), POINTER :: initialVel => NULL()
  !! initial condition for velocity

  TYPE(UserFunction_), POINTER :: initialDisp => NULL()
  !! initial condition for displacement

  TYPE(UserFunction_), POINTER :: initialAcc => NULL()
  !! initial condition for acceleration

  TYPE(CSVFile_) :: dispfile, velfile, accfile, datafile
  !! file to write displacement, velocity, acceleration

  LOGICAL(LGT) :: saveData(4)
  !! boolean to decide write the data of
  !! diaplacement, velocity, acceleration, and all

  LOGICAL(LGT) :: plotData(3)
  !! boolean to decide plot the data of
  !! diaplacement, velocity, acceleration

  INTEGER(I4B) :: outputFreq = 1
  !! output frequency

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate data

  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import data from toml file

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  !! Import data from toml file

  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display data

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! set the problem

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadForSpace => obj_SetQuadForSpace
  !! Set quadrature points for space

  PROCEDURE, PUBLIC, PASS(obj) :: SetElemsdForSpace => obj_SetElemsdForSpace
  !! Set element shape data for space

  PROCEDURE, PUBLIC, PASS(obj) :: GetMs => obj_GetMs
  !! Get Ms matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetCs => obj_GetCs
  !! Get Cs matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetKs => obj_GetKs
  !! Get Ks matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetBodyForce => obj_GetBodyForce
  !! Get body force function

  PROCEDURE, PUBLIC, PASS(obj) :: GetTractionLeft => obj_GetTractionLeft
  !! Get traction on left

  PROCEDURE, PUBLIC, PASS(obj) :: GetTractionRight => obj_GetTractionRight
  !! Get traction on right

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateConnectivity => &
    obj_InitiateConnectivity

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFields => obj_InitiateFields
  !! Initiate tangent matrix

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleTanmat => obj_AssembleTanmat
  !! Assemble Tangent matrix

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHS => obj_AssembleRHS
  !! Assemble RHS matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => obj_GetConnectivity
  !! Get connectivity matrix

  PROCEDURE, PUBLIC, PASS(obj) :: ApplyDirichletBC => &
    obj_ApplyDirichletBC
  !! Apply Left Dirichlet boundary condition

  PROCEDURE, PUBLIC, PASS(obj) :: SetInitialAcceleration => &
    obj_SetInitialAcceleration
  !! Set initial acceleration

  PROCEDURE, PUBLIC, PASS(obj) :: SetInitialVelocity => &
    obj_SetInitialVelocity
  !! Set initial velocity

  PROCEDURE, PUBLIC, PASS(obj) :: SetInitialDisplacement => &
    obj_SetInitialDisplacement
  !! Set initial displacement

  PROCEDURE, PUBLIC, PASS(obj) :: Solve => obj_Solve
  !! Solve

  PROCEDURE, PUBLIC, PASS(obj) :: Update => obj_Update
  !! Update

  PROCEDURE, PUBLIC, PASS(obj) :: WriteData => obj_WriteData
  !! Update

  PROCEDURE, PUBLIC, PASS(obj) :: Run => obj_Run
  !! Debug mode

END TYPE Abstract1DSDFEM_

!----------------------------------------------------------------------------
!                              -                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-02-11
! summary:  Initiate by param

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-02-11
! summary:  Deallocate the data

INTERFACE Abstract1DSDDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE Abstract1DSDDeallocate

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-02-11
! summary:  Display the data

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-02-11
! summary:  Import data from toml config

INTERFACE Abstract1DSDImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE Abstract1DSDImportFromToml

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-02-11
! summary:  Import data from toml config

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-02
! summary:  Set the problem

INTERFACE Abstract1DSDSet
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Set
END INTERFACE Abstract1DSDSet

!----------------------------------------------------------------------------
!                                                            SetQuadForSpace
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetQuadForSpace(obj, spaceElemNum)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
  END SUBROUTINE obj_SetQuadForSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetElemsdForSpace@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetElemsdForSpace(obj, spaceElemNum, xij)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    REAL(DFP), INTENT(IN) :: xij(1, 2)
  END SUBROUTINE obj_SetElemsdForSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetMs@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Get Ms matrix

INTERFACE
  MODULE SUBROUTINE obj_GetMs(obj, ans, nrow, ncol)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMs
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetKs@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Get Ks matrix

INTERFACE
  MODULE SUBROUTINE obj_GetKs(obj, ans, nrow, ncol)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetKs
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetBodyForce@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Get body force function

INTERFACE
  MODULE SUBROUTINE obj_GetBodyForce(obj, ans, tsize, spaceElemNum, &
                                     time, anscoeff, scale)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    REAL(DFP), INTENT(IN) :: time
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_GetBodyForce
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTractionRight@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Get traction on right

INTERFACE
  MODULE SUBROUTINE obj_GetTractionRight(obj, ans, tsize, time, &
                                         anscoeff, scale)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: time
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_GetTractionRight
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTractionLeft@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetTractionLeft(obj, ans, tsize, time, &
                                        anscoeff, scale)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: time
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_GetTractionLeft
END INTERFACE

!----------------------------------------------------------------------------
!                                                InitiateConnectivity@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_InitiateConnectivity(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                     InitiateFields@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_InitiateFields(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFields
END INTERFACE

!----------------------------------------------------------------------------
!                                                     AssembleTanmat@Methods
!----------------------------------------------------------------------------

INTERFACE Abstract1DSDAssembleTanmat
  MODULE SUBROUTINE obj_AssembleTanmat(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleTanmat
END INTERFACE Abstract1DSDAssembleTanmat

!----------------------------------------------------------------------------
!                                                        AssembleRHS@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-12
! summary:  Assemble RHS

INTERFACE Abstract1DSDAssembleRHS
  MODULE SUBROUTINE obj_AssembleRHS(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleRHS
END INTERFACE Abstract1DSDAssembleRHS

!----------------------------------------------------------------------------
!                                                              GetCs@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Get Cs matrix

INTERFACE
  MODULE SUBROUTINE obj_GetCs(obj, alpha, beta, ans, nrow, ncol)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: alpha, beta
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetCs
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Debug@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Debug mode

INTERFACE
  MODULE SUBROUTINE obj_Run(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Run
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetConnectivity@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-09
! summary:  Get connectivity matrix

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity(obj, spaceElemNum, ans, tsize)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ApplyDirichletBC@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-09-19
! summary:  Apply Dirichlet boundary condition

INTERFACE Abstract1DSDApplyDirichletBC
  MODULE SUBROUTINE obj_ApplyDirichletBC(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_ApplyDirichletBC
END INTERFACE Abstract1DSDApplyDirichletBC

!----------------------------------------------------------------------------
!                                            SetInitialAcceleration@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-19
! summary:  Set initial acceleration

INTERFACE
  MODULE SUBROUTINE obj_SetInitialAcceleration(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetInitialAcceleration
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetInitialVelocity@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-09-19
! summary:  Set initial velocity

INTERFACE
  MODULE SUBROUTINE obj_SetInitialVelocity(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetInitialVelocity
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetInitialDisplacement@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-09-19
! summary:  Set initial displacement

INTERFACE
  MODULE SUBROUTINE obj_SetInitialDisplacement(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetInitialDisplacement
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Solve@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-09-19
! summary:  Set initial displacement

INTERFACE
  MODULE SUBROUTINE obj_Solve(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Update@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-09-19
! summary:  Set initial displacement

INTERFACE Abstract1DSDUpdate
  MODULE SUBROUTINE obj_Update(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Update
END INTERFACE Abstract1DSDUpdate

!----------------------------------------------------------------------------
!                                                         WriteData@Methods
!----------------------------------------------------------------------------

INTERFACE Abstract1DSDWriteData
  MODULE SUBROUTINE obj_WriteData(obj)
    CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_WriteData
END INTERFACE Abstract1DSDWriteData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ElementDataImportFromToml_real(table, key, val, telem, isfound)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:)
  INTEGER(I4B), INTENT(IN) :: telem
  LOGICAL(LGT), INTENT(INOUT) :: isfound

  CHARACTER(*), PARAMETER :: myName = "ElementDataImportFromToml()"
  TYPE(toml_array), POINTER :: array
  INTEGER(I4B) :: stat, origin, ii, tsize, ncol, &
                  iostat
  LOGICAL(LGT) :: isOk
  REAL(DFP) :: temp
  TYPE(String) :: filename, ext
  TYPE(TxtFile_) :: atxtfile
  TYPE(CSVFile_) :: acsvfile
  CHARACTER(512) :: iomsg
  INTEGER(I4B), ALLOCATABLE :: intvec1(:), intvec2(:)
  REAL(DFP), ALLOCATABLE :: realvec(:)

#ifdef DEBUG_VER
  CALL Display(myName//key)
#endif

  CALL toml_get(table, key, array, origin=origin, stat=stat, &
                requested=.FALSE.)

  IF (ASSOCIATED(array)) THEN
    tsize = toml_len(array)
    CALL Reallocate(val, tsize)
    DO ii = 1, tsize
      CALL toml_get(array, ii, val(ii))
    END DO
    NULLIFY (array)
    isfound = .TRUE.
    RETURN
  END IF

  CALL toml_get(table, key, temp, origin=origin, stat=stat)

  IF (stat .EQ. toml_stat%success) THEN
    CALL Reallocate(val, 1)
    val(1) = temp
    isfound = .TRUE.
    RETURN
  END IF

  CALL toml_get(table, key, filename%raw, origin=origin, stat=stat)

  IF (stat .EQ. toml_stat%success) THEN
    ext = filename%extension()
    SELECT CASE (ext%chars())
    CASE (".txt")
      CALL atxtfile%Initiate(filename=filename%Chars(), &
                             action="READ", status="OLD")
      CALL atxtfile%OPEN()
      CALL atxtfile%READ(val=val, iostat=iostat, iomsg=iomsg)

      isok = iostat .NE. 0 .AND. (.NOT. atxtfile%isEOF())
      IF (isok) THEN
        STOP "error occur "
        filename = ""
        RETURN
      END IF

      CALL atxtfile%DEALLOCATE()
      filename = ""
      isfound = .TRUE.
      RETURN
    CASE (".csv")
      CALL acsvfile%Initiate(filename=filename%Chars(), &
                             action="READ", status="OLD", &
                             delimiter=",")
      CALL acsvfile%OPEN()
      CALL acsvfile%SetHeaderIndx(1)
      CALL acsvfile%READ()
      ncol = acsvfile%Getncols()
      SELECT CASE (ncol)
      CASE (1) ! only value

        CALL acsvfile%get(1, val=realvec) ! value
        isOk = ALLOCATED(realvec)
        CALL AssertError_(isok, myname, &
                          "value column does not have data")
        CALL reallocate(val, 1)
        val(1) = realvec(1)

      CASE (2) ! element number and value

        CALL acsvfile%get(1, val=intvec1) ! element
        CALL acsvfile%get(2, val=realvec) ! value

        isok = ALLOCATED(intvec1) .AND. ALLOCATED(realvec)
        CALL AssertError_(isok, myname, &
                          "element column or value column "// &
                          " does not have data")
        isok = SIZE(intvec1) .EQ. telem
        CALL AssertError_(isok, myname, "the size of elemen column "// &
                          " should be the same as the number of elements")
        CALL reallocate(val, telem)
        DO ii = 1, telem
          val(intvec1(ii)) = realvec(ii)
        END DO

      CASE (3) ! start, end, value

        CALL acsvfile%get(1, val=intvec1) ! start
        CALL acsvfile%get(2, val=intvec2) ! end
        CALL acsvfile%get(3, val=realvec) ! value
        isok = ALLOCATED(intvec1) .AND. ALLOCATED(realvec) &
               .AND. ALLOCATED(intvec2)
        CALL AssertError_(isok, myname, &
                          "start column, end column or value column"// &
                          " does not have data")
        isok = MINVAL(intvec1) .EQ. one
        CALL AssertError_(isok, myname, "start must have 1 as a component")
        isok = MAXVAL(intvec2) .EQ. telem
        CALL AssertError_(isok, myname, "end must have the integer"// &
                          " which is the same as the number of elements")

        CALL reallocate(val, telem)
        DO ii = 1, SIZE(intvec1)
          val(intvec1(ii):intvec2(ii)) = realvec(ii)
        END DO

      CASE default
        CALL AssertError_(.FALSE., myname, &
                          "wrong number of columns in csv file ")
      END SELECT

      CALL acsvfile%DEALLOCATE()
      filename = ""
      isfound = .TRUE.
      RETURN
    END SELECT
  END IF

  isfound = .FALSE.

END SUBROUTINE ElementDataImportFromToml_real

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ElementDataImportFromToml_int(table, key, val, telem, isfound)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: val(:)
  INTEGER(I4B), INTENT(IN) :: telem
  LOGICAL(LGT), INTENT(INOUT) :: isfound

  CHARACTER(*), PARAMETER :: myName = "ElementDataImportFromToml()"
  TYPE(toml_array), POINTER :: array
  INTEGER(I4B) :: stat, origin, ii, tsize, ncol, &
                  iostat, temp
  LOGICAL(LGT) :: isOk
  TYPE(String) :: filename, ext
  TYPE(TxtFile_) :: atxtfile
  TYPE(CSVFile_) :: acsvfile
  CHARACTER(512) :: iomsg
  INTEGER(I4B), ALLOCATABLE :: intvec1(:), intvec2(:), intvec3(:)

#ifdef DEBUG_VER
  CALL Display(myName//key)
#endif

  CALL toml_get(table, key, array, origin=origin, stat=stat, &
                requested=.FALSE.)

  IF (ASSOCIATED(array)) THEN
    tsize = toml_len(array)
    CALL Reallocate(val, tsize)
    DO ii = 1, tsize
      CALL toml_get(array, ii, val(ii))
    END DO
    NULLIFY (array)
    isfound = .TRUE.
    RETURN
  END IF

  CALL toml_get(table, key, temp, origin=origin, stat=stat)

  IF (stat .EQ. toml_stat%success) THEN
    CALL Reallocate(val, 1)
    val(1) = temp
    isfound = .TRUE.
    RETURN
  END IF

  CALL toml_get(table, key, filename%raw, origin=origin, stat=stat)

  IF (stat .EQ. toml_stat%success) THEN
    ext = filename%extension()
    SELECT CASE (ext%chars())
    CASE (".txt")
      CALL atxtfile%Initiate(filename=filename%Chars(), &
                             action="READ", status="OLD")
      CALL atxtfile%OPEN()
      CALL atxtfile%READ(val=val, iostat=iostat, iomsg=iomsg)

      isok = iostat .NE. 0 .AND. (.NOT. atxtfile%isEOF())
      IF (isok) THEN
        STOP "error occur "
        filename = ""
        RETURN
      END IF

      CALL atxtfile%DEALLOCATE()
      filename = ""
      isfound = .TRUE.
      RETURN
    CASE (".csv")
      CALL acsvfile%Initiate(filename=filename%Chars(), &
                             action="READ", status="OLD", &
                             delimiter=",")
      CALL acsvfile%OPEN()
      CALL acsvfile%SetHeaderIndx(1)
      CALL acsvfile%READ()
      ncol = acsvfile%Getncols()
      SELECT CASE (ncol)
      CASE (1) ! only value

        CALL acsvfile%get(1, val=intvec3) ! value
        isOk = ALLOCATED(intvec3)
        CALL AssertError_(isok, myname, &
                          "value column does not have data")
        CALL reallocate(val, 1)
        val(1) = intvec3(1)

      CASE (2) ! element number and value

        CALL acsvfile%get(1, val=intvec1) ! element
        CALL acsvfile%get(2, val=intvec3) ! value

        isok = ALLOCATED(intvec1) .AND. ALLOCATED(intvec3)
        CALL AssertError_(isok, myname, &
                          "element column or value column "// &
                          " does not have data")
        isok = SIZE(intvec1) .EQ. telem
        CALL AssertError_(isok, myname, "the size of elemen column "// &
                          " should be the same as the number of elements")
        CALL reallocate(val, telem)
        DO ii = 1, telem
          val(intvec1(ii)) = intvec3(ii)
        END DO

      CASE (3) ! start, end, value

        CALL acsvfile%get(1, val=intvec1) ! start
        CALL acsvfile%get(2, val=intvec2) ! end
        CALL acsvfile%get(3, val=intvec3) ! value
        isok = ALLOCATED(intvec1) .AND. ALLOCATED(intvec3) &
               .AND. ALLOCATED(intvec2)
        CALL AssertError_(isok, myname, &
                          "start column, end column or value column"// &
                          " does not have data")
        isok = MINVAL(intvec1) .EQ. one
        CALL AssertError_(isok, myname, "start must have 1 as a component")
        isok = MAXVAL(intvec2) .EQ. telem
        CALL AssertError_(isok, myname, "end must have the integer"// &
                          " which is the same as the number of elements")

        CALL reallocate(val, telem)
        DO ii = 1, SIZE(intvec1)
          val(intvec1(ii):intvec2(ii)) = intvec3(ii)
        END DO

      CASE default
        CALL AssertError_(.FALSE., myname, &
                          "wrong number of columns in csv file ")
      END SELECT

      CALL acsvfile%DEALLOCATE()
      filename = ""
      isfound = .TRUE.
      RETURN
    END SELECT
  END IF

  isfound = .FALSE.

END SUBROUTINE ElementDataImportFromToml_int

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE AssertError_(a, myName, msg)
  LOGICAL(LGT), INTENT(IN) :: a
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: msg

  IF (.NOT. a) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INTERNAL ERROR] :: '//msg)
    RETURN
  END IF

END SUBROUTINE AssertError_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Abstract1DSDFEM_Class
