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

MODULE ElastoDynamics2DFEM_Class
USE GlobalData, ONLY: DFP, I4B, LGT

USE ParameterList, ONLY: ParameterList_

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemshapeData_, &
                    poly => TypePolynomialOpt, &
                    qp => TypeQuadratureOpt, &
                    ip => TypeInterpolationOpt, &
                    CSRMatrix_, &
                    DOF_, &
                    RealVector_, &
                    iface_SpaceTimeFunction, &
                    iface_1DFunction, &
                    FEVariable_

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

USE FEDomain_Class, ONLY: FEDomain_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE MatrixField_Class, ONLY: MatrixField_, &
                             MatrixFieldPointer_
USE VectorField_Class, ONLY: VectorField_, &
                             VectorFieldPointer_
USE NeumannBC_Class, ONLY: NeumannBCPointer_
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE SolidMaterial_Class, ONLY: SolidMaterialPointer_
USE LinSolver_Class, ONLY: LinSolver_
USE FEMesh_Class, ONLY: FEMesh_, FEMeshPointer_

PRIVATE

PUBLIC :: ElastoDynamics2DFEM_

CHARACTER(*), PARAMETER :: modName = 'ElastoDynamics2DFEM_Class'
CHARACTER(*), PARAMETER :: prefix = "ElastoDynamics2DFEM"
CHARACTER(*), PARAMETER :: default_result_dir = "./results"
CHARACTER(*), PARAMETER :: default_filename = "ElastoDynamics2DFEM"
CHARACTER(*), PARAMETER :: default_baseInterpolationForSpace = "LAGR"
CHARACTER(*), PARAMETER :: default_timeIntegrationScheme = "NEWM"
CHARACTER(*), PARAMETER :: default_baseTypeForSpace = "Monomial"
CHARACTER(*), PARAMETER :: default_ipTypeForSpace = "Equidistance"
CHARACTER(*), PARAMETER :: default_quadTypeForSpace = "GaussLegendre"
CHARACTER(*), PARAMETER :: default_engineName = "NATIVE_SERIAL"
INTEGER(I4B), PARAMETER :: MAX_ORDER_SPACE = 10
INTEGER(I4B), PARAMETER :: default_verbosity = 0
INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B), PARAMETER :: spaceCompo = 2
INTEGER(I4B), PARAMETER :: timeCompo = 1

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

#ifdef DEBUG_VER
LOGICAL(LGT), PARAMETER :: debug = .TRUE.
#else
LOGICAL(LGT), PARAMETER :: debug = .FALSE.
#endif

INTERFACE ElementDataImportFromToml
  MODULE PROCEDURE ElementDataImportFromToml_real
  MODULE PROCEDURE ElementDataImportFromToml_int
END INTERFACE ElementDataImportFromToml

!----------------------------------------------------------------------------
!                                                   ElastoDynamics2DFEM_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-22
! summary:  Define ElastoDynamics2DFEM_

TYPE :: ElastoDynamics2DFEM_
  TYPE(ParameterList_) :: param
  TYPE(SDAlgoParam_) :: algoParam
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_) :: fedof, fedofNBC
  ! TYPE(FEDOFPointer_), ALLOCATABLE :: fedofNBC(:)

  CLASS(AbstractMesh_), POINTER :: cellmesh => NULL()
  TYPE(FEMeshPointer_), ALLOCATABLE :: boundaries(:)

  INTEGER(I4B) :: totalSpaceElements = 0

  TYPE(String) :: meshfilename
  TYPE(String) :: filename

  INTEGER(I4B), ALLOCATABLE :: cellcon(:)

  INTEGER(I4B) :: maxNNE = 0, maxCON = 0, &
                  maxNodeNum_pointSource = 0

  TYPE(MatrixFieldPointer_) :: matrixFields(3)
  TYPE(MatrixField_), POINTER :: tanmat, massMat, stiffMat

  TYPE(VectorFieldPointer_) :: vectorfields(9)
  TYPE(VectorField_), POINTER :: rhs, sol, rhs0, &
                                 force1, force2, tmp1, &
                                 u0, v0, a0

  INTEGER(I4B) :: tnbc = 0, tdbc = 0, tnbc_point = 0

  TYPE(NeumannBCPointer_), ALLOCATABLE :: nbc(:), nbc_point(:)
  TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
  INTEGER(I4B), ALLOCATABLE :: dbcIndx(:)

  TYPE(LinSolver_) :: linsolver

  CHARACTER(2) :: baseContinuityForSpace = "H1"

  CHARACTER(4) :: baseInterpolationForSpace = "LAGR"

  CHARACTER(4) :: timeIntegrationScheme = "NEWM"

  INTEGER(I4B) :: verbosity = 0

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

  REAL(DFP) :: timeRange(2) = 0.0_DFP
  !! Total length of time domain

  INTEGER(I4B) :: spaceOrder = 1
  !! space order of each element

  REAL(DFP), ALLOCATABLE :: timeStepSize(:)
  !! size of each time step
  !! the size should be totalTimeSteps

  TYPE(SolidMaterialPointer_), ALLOCATABLE :: solidMaterial(:)

  TYPE(FEVariable_) :: density, rayleighAlpha, rayleighBeta, &
                       cijkl

  TYPE(String) :: result_dir
  !! Result directory name

  TYPE(QuadraturePoint_) :: quadForSpace, quadForSpaceBnd
  !! Quadrature points in space

  TYPE(ElemshapeData_) :: elemsdForSpace, elemsdForSpaceBnd
  !! Element shape data for space

  REAL(DFP), ALLOCATABLE :: ks(:, :), ms(:, :), cs(:, :)
  !! space stiffness matrix

  TYPE(UserFunction_), POINTER :: bodyForce => NULL()
  !! body force

  TYPE(UserFunction_), POINTER :: initialVel => NULL()
  !! initial condition for velocity

  TYPE(UserFunction_), POINTER :: initialDisp => NULL()
  !! initial condition for displacement

  TYPE(UserFunction_), POINTER :: initialAcc => NULL()
  !! initial condition for acceleration

  LOGICAL(LGT) :: saveData(4)
  !! boolean to decide write the data of
  !! diaplacement, velocity, acceleration, and all

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

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateDomains => obj_InitiateDomains

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFEDOF => obj_InitiateFEDOF

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFields => obj_InitiateFields
  !! Initiate tangent matrix

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! set the problem

  PROCEDURE, PUBLIC, PASS(obj) :: SetInitialAcceleration => &
    obj_SetInitialAcceleration
  !! Set initial acceleration

  PROCEDURE, PUBLIC, PASS(obj) :: SetInitialVelocity => &
    obj_SetInitialVelocity
  !! Set initial velocity

  PROCEDURE, PUBLIC, PASS(obj) :: SetInitialDisplacement => &
    obj_SetInitialDisplacement
  !! Set initial displacement

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleTanmat => obj_AssembleTanmat
  !! Assemble Tangent matrix

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHS => obj_AssembleRHS
  !! Assemble RHS matrix

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleSurfaceSource => obj_AssembleSurfaceSource
  !! Assemble RHS matrix

  PROCEDURE, PUBLIC, PASS(obj) :: AssemblePointSource => &
    obj_AssemblePointSource
  !! Assemble RHS matrix
  !! TODO: Implement body force Assembling

  PROCEDURE, PUBLIC, PASS(obj) :: ApplyDirichletBC => &
    obj_ApplyDirichletBC
  !! Apply Left Dirichlet boundary condition

  PROCEDURE, PUBLIC, PASS(obj) :: Solve => obj_Solve
  !! Solve

  PROCEDURE, PUBLIC, PASS(obj) :: Update => obj_Update
   !! Update

  PROCEDURE, PUBLIC, PASS(obj) :: WriteData => obj_WriteData
   !! Update

  PROCEDURE, PUBLIC, PASS(obj) :: Run => obj_Run
  !! Debug mode

END TYPE ElastoDynamics2DFEM_

!----------------------------------------------------------------------------
!                              -                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-02-11
! summary:  Initiate by param

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-02-11
! summary:  Deallocate the data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-02-11
! summary:  Display the data

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-02-11
! summary:  Import data from toml config

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetMs@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Get Ms matrix

INTERFACE
  MODULE SUBROUTINE obj_GetMs(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: time
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_GetTractionLeft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_InitiateDomains(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateDomains
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-02-27
! summary:  initiate dof

INTERFACE
  MODULE SUBROUTINE obj_InitiateFEDOF(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFEDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                     InitiateFields@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_InitiateFields(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFields
END INTERFACE

!----------------------------------------------------------------------------
!                                                     AssembleTanmat@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_AssembleTanmat(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleTanmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                        AssembleRHS@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-12
! summary:  Assemble RHS

INTERFACE
  MODULE SUBROUTINE obj_AssembleRHS(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                        AssembleRHS@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2025-03-01
! summary:  Assemble RHS

INTERFACE
  MODULE SUBROUTINE obj_AssembleSurfaceSource(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleSurfaceSource
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-24
! summary:  Assemble RHS

INTERFACE
  MODULE SUBROUTINE obj_AssemblePointSource(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssemblePointSource
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetCs@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Get Cs matrix

INTERFACE
  MODULE SUBROUTINE obj_GetCs(obj, alpha, beta, ans, nrow, ncol)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_ApplyDirichletBC
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetInitialAcceleration@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-19
! summary:  Set initial acceleration

INTERFACE
  MODULE SUBROUTINE obj_SetInitialAcceleration(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
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
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Update@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-09-19
! summary:  Set initial displacement

INTERFACE
  MODULE SUBROUTINE obj_Update(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Update
END INTERFACE

!----------------------------------------------------------------------------
!                                                         WriteData@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_WriteData(obj)
    CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_WriteData
END INTERFACE

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

  IF (debug) CALL Display(myName//key)

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

  IF (debug) CALL Display(myName//key)

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

END MODULE ElastoDynamics2DFEM_Class
