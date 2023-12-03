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

!> authors: Vikas Sharma, Ph. D.
! date: 18 Aug 2021
! summary: This module defines the abstract class for physicsKernel

MODULE AbstractKernel_Class
USE BaseType
USE String_Class, ONLY: String
USE AbstractKernelParam
USE AbstractMatrixField_Class
USE Domain_Class
USE DirichletBC_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE GlobalData
USE HDF5File_Class
USE LinSolverFactory
USE MeshSelection_Class
USE NeumannBC_Class
USE NitscheBC_Class
USE TxtFile_Class
USE VTKFile_Class
USE FiniteElement_Class
USE tomlf, ONLY: toml_table

IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractKernel_Class"
CHARACTER(*), PARAMETER :: AbstractKernelEssentialParam =&
  & "/name/engine/coordinateSystem/domainFile/isCommonDomain/gravity/"// &
  & "timeDependency/maxIter/nsd/nnt/tdof/dt/startTime/endTime/"//  &
  & "currentTime/currentTimeStep/totalTimeStep/baseInterpolationForSpace/"//&
  & "baseContinuityForSpace/quadratureTypeForSpace/"//  &
  & "baseInterpolationForTime/baseContinuityForTime/quadratureTypeForTime"

PUBLIC :: AbstractKernel_
PUBLIC :: AbstractKernelPointer_
PUBLIC :: SetAbstractKernelParam
PUBLIC :: AbstractKernelCheckEssentialParam
PUBLIC :: AbstractKernelInitiate
PUBLIC :: AbstractKernelDeallocate
PUBLIC :: AbstractKernelDisplay
PUBLIC :: AbstractKernelExport
PUBLIC :: AbstractKernelImport
PUBLIC :: AbstractKernelImportParamFromToml

PUBLIC :: KernelGetCoordinateSystemName
PUBLIC :: KernelGetCoordinateSystemID
PUBLIC :: KernelGetNSDFromID
PUBLIC :: KernelGetNSDFromName

!----------------------------------------------------------------------------
!                                                           AbstractKernel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 April 2022
! summary: Abstract class for kernel

TYPE, ABSTRACT :: AbstractKernel_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! This variable is Set to true when we initiate the kernel
  LOGICAL(LGT) :: isCommonDomain = .TRUE.
  !! This variable is True when the domain is common
  !! It is useful in the case of multi-physics simulations.
  !! In multi-physics applications different fields can have different
  !! type and order of shape functions. To construct such shape functions
  !! we may have to use different finite element meshes (hence, domains).
  !! For example, in the fluid mechanics, we often use Taylor-Hood element
  !! Which employs different order of interpolation for pressure and velocity.
  !! NOTE: In most of the application isCommonDomain is TRUE.
  TYPE(String) :: name
  !! This is the name of the kernel. It can be anything you want.
  TYPE(String) :: engine
  !! Which type of linear solver library (engine) we use to
  !! solve system of linear equations. We can specify following
  !! values.
  !! `NATIVE_SERIAL`
  !! `NATIVE_OMP`
  !! `NATIVE_MPI`
  !! `LIS_SERIAL`
  !! `LIS_OMP`
  !! `LIS_MPI`
  !! `PETSC`
  INTEGER(I4B) :: coordinateSystem = DEFAULT_coordinateSystem
  !! Spatial coordinate system type. It can take following values
  !! `KERNEL_CARTESIAN` for Cartesian coordinates
  !! `KERNEL_CYLINDRICAL` for Cylinderical coordinates
  !! `KERNEL_SPHERICAL` for Sperical coordinates
  !! NOTE: These parameters are defined in the AbstractKernelParam module.
  INTEGER(I4B) :: maxIter = DEFAULT_maxIter
  !! Maximum  number of iteration iterations
  !! This is useful when when we use iterative solvers like
  !! Newton method, Modified Newton method, or Iterative-predictor solvers.
  !! NOTE: DEFAULT_maxIter is defined in AbstractKernelParam
  INTEGER(I4B) :: timeDependency = 0
  !! This variable indicates if the problem is time dependent or not.
  !! It can take following values:
  !! KERNEL_STEADY  or KERNEL_STATIC
  !! KERNEL_PSEUDOSTATIC
  !! KERNEL_TRANSIENT or KERNEL_DYNAMIC
  !! NOTE: These variables are defined in AbstractKernelParam
  INTEGER(I4B) :: nsd = 0
  !! Spatial dimension of the problem,
  INTEGER(I4B) :: nnt = 1
  !! Number of nodes in time element
  !! NOTE: This variables is used only in space-time finite element methods
  INTEGER(I4B) :: tdof = 0
  !! Total number of degree of freedom per node
  !! NOTE: This variable is Set internally by each kernel while
  !! Setting the kernel.
  REAL(DFP) :: normRHS = 0.0_DFP
  !! norm of the right-hand-side vector in the system of linear equations
  !! NOTE: This variable is used internally
  REAL(DFP) :: dt = 0.0_DFP
  !! Time step size used in the pseudostatic and dynamic problems
  !! Needed in transient case
  REAL(DFP) :: startTime = 0.0
  !! Starting time of simulation
  !! NOTE: This varible is needed in the transient or pseudostatic simulation
  REAL(DFP) :: endTime = 0.0
  !! Final time of the simulation
  !! NOTE: This varible is needed in the transient or pseudostatic simulation
  REAL(DFP) :: currentTime = 0.0
  !! The current time of the simulation
  !! NOTE: This varible is needed in the transient simulation only.
  INTEGER(I4B) :: currentTimeStep = 1
  !! Current time step number of the simulation.
  !! NOTE: This varible is needed in the transient simulation only.
  INTEGER(I4B) :: totalTimeStep = 0
  !! Total number of time step number in the simulation.
  !! NOTE: This varible is needed in the transient simulation only.
  REAL(DFP) :: lengthScale = 1.0_DFP
  !! This variable denotes the length scale of the problem.
  !! NOTE: This variable is for internal use only.
  INTEGER(I4B) :: postProcessOpt = 0
  !! Postprocessing options
  !! INFO: The actual action depends upon the specific kernels
  REAL(DFP) :: gravity(3) = 0.0_DFP
  !! Acceleration vector due to gravity
  TYPE(IterationData_) :: iterData
  !! Iteration data
  !! INFO: The actual action depends upon the specific kernels
  INTEGER(I4B), ALLOCATABLE :: elemToMatId(:, :)
  !! This variable denotes the Element number to material mapping.
  !! For example, `elemToMatID( iel, POROUS_MATERIAL_ID )` denotes the
  !! porous material type assigned to element number `iel`
  !! Similarly, `elemToMatID( iel, FLUID_MATERIAL_ID )` denotes
  !! the fluid material type assigned to element number `iel`.
  INTEGER(I4B), ALLOCATABLE :: dbcIndx(:)
  !! Indices where Dirichlet boundary conditions is prescribed
  !! INFO: This variable is for internal use only.
  !! It is formed from the Dirichlet boundary conditions.
  CLASS(AbstractLinSolver_), POINTER :: linsol => NULL()
  !! A pointer to a Linear iterative solver
  !! NOTE: The actual linear solver depends upon the
  !! engine and type of problem. linsol is initiated in
  !! KernelInitiateFromParam routine.
  CLASS(AbstractMatrixField_), POINTER :: tanmat => NULL()
  !! Global tangent matrix
  !! NOTE: The actual form of tangent matrix depends upon the engine
  !! and type of problem.
  CLASS(Domain_), POINTER :: dom => NULL()
  !! Domain of the problem
  TYPE(DomainPointer_), ALLOCATABLE :: domains(:)
  !! Domain of the problem
  TYPE(ReferenceLine_) :: refTimeElem
  !! reference element for time domain
  TYPE(ReferenceLine_) :: refLinTimeElem
  !! reference element for time domain
  TYPE(String) :: baseContinuityForSpace
  !! Continuity of basis function in space
  TYPE(String) :: baseInterpolationForSpace
  !! Interpolation of shape function in space
  TYPE(String) :: quadratureTypeForSpace
  !! Quadrature type in space
  INTEGER(I4B) :: quadTypeForSpace
  !! Quadrature type in space
  INTEGER(I4B) :: ipTypeForSpace = 0
  !! Interpolation grid used for Lagrange polynomials
  INTEGER(I4B) :: basisTypeForSpace = 0
  !! Basis type for space
  REAL(DFP) :: alphaForSpace = 0.0_DFP
  !! Parameter for Jacobi polynomials in space
  REAL(DFP) :: betaForSpace = 0.0_DFP
  !! Parameter for Jacobi polynomials in space
  REAL(DFP) :: lambdaForSpace = 0.0_DFP
  !! Parameter for Ultraspherical polynomials in  space
  TYPE(String) :: baseContinuityForTime
  !! Continuity of basis function in time in time domain
  TYPE(String) :: baseInterpolationForTime
  !! Interpolation of basis function in time
  TYPE(String) :: quadratureTypeForTime
  !! Quadrature type in time
  INTEGER(I4B) :: quadTypeForTime
  !! Quadrature type in time
  INTEGER(I4B) :: ipTypeForTime
  !! Interpolation grid used for Lagrange polynomials
  INTEGER(I4B) :: basisTypeForTime
  !! Basis type for space
  REAL(DFP) :: alphaForTime = 0.0_DFP
  !! Parameter for Jacobi polynomials in space
  REAL(DFP) :: betaForTime = 0.0_DFP
  !! Parameter for Jacobi polynomials in space
  REAL(DFP) :: lambdaForTime = 0.0_DFP
  !! Parameter for Ultraspherical polynomials in space
  TYPE(String) :: domainFile
  !! Domain file name
  TYPE(QuadraturePoint_), ALLOCATABLE :: quadratureForSpace(:)
  !! Quadrature points in space element
  !! The size of quadratureForSpace is same as the total number of
  !! mesh in the domain
  TYPE(QuadraturePoint_) :: quadratureForTime
  !! Quadrature points in time element
  !! INFO: This is used in space-time computation
  TYPE(FiniteElementPointer_), ALLOCATABLE :: cellFE(:)
  !! Cell finite element
  TYPE(FiniteElementPointer_), ALLOCATABLE :: linCellFE(:)
  !! Linear cell finite element
  TYPE(FiniteElementPointer_), ALLOCATABLE :: facetFE(:)
  !! Facet finite element
  TYPE(FiniteElementPointer_), ALLOCATABLE :: linFacetFE(:)
  !! Linear facet finite element
  TYPE(FiniteElementPointer_), ALLOCATABLE :: edgeFE(:)
  !! Edge finite element
  TYPE(FiniteElementPointer_), ALLOCATABLE :: linEdgeFE(:)
  !! Linear edge finite element
  TYPE(FiniteElement_) :: timeFE
  !! Time finite element
  TYPE(FiniteElement_) :: linTimeFE
  !! Linear time finite element
  TYPE(ElemshapeData_) :: linTimeElemSD
    !! Element shape data on linear time element #STFEM
  TYPE(ElemshapeData_) :: timeElemSD
    !! Element shape data on time element #STFEM
  TYPE(ElemshapeData_), ALLOCATABLE :: linSpaceElemSD(:)
    !! Element shape data on linear space (simplex) element
  TYPE(ElemshapeData_), ALLOCATABLE :: spaceElemSD(:)
    !! Element shape data on space element
  TYPE(STElemshapeData_), ALLOCATABLE :: stelemsd(:, :)
    !! Element shape data on space-time element

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & obj_CheckEssentialParam
  !! Check the essential parameter of the kernel
  !! NOTE: When you want to add or remove a param from
  !! essential param, then just modify the
  !! astr variable in obj_CheckEssentialParam method.
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate the kernel. This is a constructor method.
  !! WARN: This routine is an interface only. It means
  !! it should be implemented by the subclass.
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the memory occupied by the kernel

  ! CONSTRUCTOR:
  ! @InitiateFieldsMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFields => obj_InitiateFields
  !! Initiate the fields

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Returns the prefix of the kernel
  !! This method should be implemented by the specific kernel

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! Perform final check, before starting the actual computations
  PROCEDURE, PUBLIC, PASS(obj) :: SetCurrentTimeStep => &
    & obj_SetCurrentTimeStep
  !! Sets the current time step of the kernel
  PROCEDURE, PUBLIC, PASS(obj) :: SetIterationNumber => &
    & obj_SetIterationNumber
  !! Sets the current time step of the kernel
  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshData => obj_SetMeshData
  !! This method is called from Set method.
  !! It Sets the mesh data.
  PROCEDURE, PUBLIC, PASS(obj) :: SetFiniteElements => obj_SetFiniteElements
  !! Set finite elements
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadPointsInSpace =>  &
    & obj_SetQuadPointsInSpace
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadPointsInTime =>  &
    & obj_SetQuadPointsInTime
  PROCEDURE, PUBLIC, PASS(obj) :: SetLocalElemShapeDataInSpace =>  &
    & obj_SetLocalElemShapeDataInSpace
  !! Set local element shape data in space
  PROCEDURE, PUBLIC, PASS(obj) :: SetLocalElemShapeDataInTime =>  &
    & obj_SetLocalElemShapeDataInTime
  !! Set local element shape data in time
  PROCEDURE, PUBLIC, PASS(obj) :: SetGlobalElemShapeDataInSpace =>  &
    & obj_SetGlobalElemShapeDataInSpace
  !! Set global element shape data in space
  ! TODO: Implement SetGlobalElemShapeDataInSpace
  PROCEDURE, PUBLIC, PASS(obj) :: SetGlobalElemShapeDataInTime =>  &
    & obj_SetGlobalElemShapeDataInTime
  !! Set global element shape data in time
  ! TODO: Implement SetGlobalElemShapeDataInTime
  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetFiniteElements =>  &
    & obj_SetFacetFiniteElements
  !! Set Facet Finite Elements
  !! TODO: Implement SetFacetFiniteElements method

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from the input file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data to an external file
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Displays the content of kernel
  PROCEDURE, PASS(obj) :: WriteData_hdf5 => obj_WriteData_hdf5
  !! Write simulation results in hdf5 file format
  PROCEDURE, PASS(obj) :: WriteData_vtk => obj_WriteData_vtk
  !! Write simulation data in vtk file format
  ! PROCEDURE, PASS( obj ) :: WriteData_xdmf => obj_WriteData_xdmf
  GENERIC, PUBLIC :: WriteData => WriteData_hdf5, WriteData_vtk
  !! Export data to an external file
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

  PROCEDURE, PUBLIC, PASS(obj) :: ImportParamFromToml =>  &
    & obj_ImportParamFromToml
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToToml => obj_ExportToToml

  ! GET:
  ! @RunMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Run => obj_Run
  !! Run the kernel

  ! SET:
  ! @AssembleMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Assemble => obj_Assemble
  !! This procedure pointer assembles the problem

  ! SET:
  ! @AssembleTanmatMethods
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleTanmat => obj_AssembleTanmat
  !! This procedure pointer assembles the global tangent matrix

  ! SET:
  ! @AssembleRHSMethods
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHS => obj_AssembleRHS
  !! This procedure pointer assembles the right-hand-side vector

  ! GET:
  ! @SolveMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Solve => obj_Solve
  !! This procedure pointer solves the problem

  ! SET:
  ! @UpdateMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Update => obj_Update
  !! This procedure pointer update the problem
  PROCEDURE, PUBLIC, PASS(obj) :: UpdateIteration => obj_UpdateIteration
  !! This procedure pointer update the problem

  ! GET:
  ! @ConvergenceMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsConverged => obj_IsConverged
  !! This procedure pointer checks the convergence
END TYPE AbstractKernel_

!----------------------------------------------------------------------------
!                                                   AbstractKernelPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractKernelPointer_
  CLASS(AbstractKernel_), POINTER :: Ptr => NULL()
END TYPE AbstractKernelPointer_

!----------------------------------------------------------------------------
!                                          SetKernelParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: Set kernel parameters

INTERFACE
  MODULE SUBROUTINE SetAbstractKernelParam( &
    & prefix, &
    & param, &
    & name, &
    & engine, &
    & coordinateSystem, &
    & domainFile, &
    & isCommonDomain, &
    & gravity, &
    & timeDependency, &
    & maxIter, &
    & nsd, &
    & nnt, &
    & tdof, &
    & dt, &
    & startTime, &
    & endTime, &
    & currentTime, &
    & currentTimeStep, &
    & totalTimeStep, &
    & baseInterpolationForSpace, &
    & baseContinuityForSpace, &
    & quadratureTypeForSpace, &
    & ipTypeForSpace, &
    & basisTypeForSpace, &
    & alphaForSpace, &
    & betaForSpace, &
    & lambdaForSpace, &
    & baseInterpolationForTime, &
    & baseContinuityForTime, &
    & quadratureTypeForTime, &
    & ipTypeForTime, &
    & basisTypeForTime, &
    & alphaForTime, &
    & betaForTime, &
    & lambdaForTime, &
    & postProcessOpt)
    CHARACTER(*), INTENT(IN) :: prefix
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuityForSpace
    !! Type of continuity of basis function for Space
    !! NOTE: Default value is given in AbstractKernelParam
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuityForTime
    !! Type of continuity of basis function for Time
    !! NOTE: Default value is given in AbstractKernelParam
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolationForSpace
    !! Type of interpolation function used for Space
    !! NOTE: Default value is given in AbstractKernelParam
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolationForTime
    !! Type of interpolation function used for Time
    !! NOTE: Default value is given in AbstractKernelParam
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainFile
    !! Name of domain file
    CHARACTER(*), OPTIONAL, INTENT(IN) :: engine
    !! Engine of the kernel
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    !! name of the kernel
    CHARACTER(*), OPTIONAL, INTENT(IN) :: quadratureTypeForSpace
    !! Type of quadrature for space
    !! NOTE: Default value is given in AbstractKernelParam
    CHARACTER(*), OPTIONAL, INTENT(IN) :: quadratureTypeForTime
    !! Type of quadrature for time
    !! NOTE: Default value is given in AbstractKernelParam
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: coordinateSystem
    !! Coordinate system of the problem
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: currentTimeStep
    !! Current time step
    !! INFO: Default value is 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! maximum number of iterations, required for iterData
    !! NOTE: Default value is given in AbstractKernelParam
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! Number of nodes in time element.
    !! NOTE: This variable is needed only in the case of
    !! space-time finite element method.
    !! The default value is 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! Spatial dimension of the problem
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: postProcessOpt
    !! Postprocessing option for writeData
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tdof
    !! Total degree of freedom per nodes in the problem
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeDependency
    !! time dependency of the problem
    !! INFO: Default value is KERNEL_STEADY
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: totalTimeStep
    !!Total number of time steps
    !! INFO: Default value is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isCommonDomain
    !! Is domain comman or different domain for multi-physics
    REAL(DFP), OPTIONAL, INTENT(IN) :: currentTime
    !! Current time step of the simulation
    !! INFO: Default value if 0.0_DFP
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! Time step size
    !! INFO: Default value is 0.0
    REAL(DFP), OPTIONAL, INTENT(IN) :: endTime
    !! Last time of simulation
    !! INFO: Default value is 0.0_DFP
    REAL(DFP), OPTIONAL, INTENT(IN) :: gravity(3)
    !! Acceleration due to gravity
    !! INFO: Default value if [0,0,0]
    REAL(DFP), OPTIONAL, INTENT(IN) :: startTime
    !! Start time of the simulation
    !! INFO: Default value is 0.0
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Put parameters here
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipTypeForSpace
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipTypeForTime
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisTypeForSpace
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisTypeForTime
    REAL(DFP), OPTIONAL, INTENT(IN) :: alphaForSpace
    REAL(DFP), OPTIONAL, INTENT(IN) :: alphaForTime
    REAL(DFP), OPTIONAL, INTENT(IN) :: betaForSpace
    REAL(DFP), OPTIONAL, INTENT(IN) :: betaForTime
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambdaForSpace
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambdaForTime
  END SUBROUTINE SetAbstractKernelParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Aug 2021
! summary: Checks the essential parameters in the param of kernels

INTERFACE AbstractKernelCheckEssentialParam
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param, prefix)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE AbstractKernelCheckEssentialParam

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This initiates the Kernel
!
!# Introduction
! This routine initiates the kernel
!
!INFO: domains is necessary when isCommonDomain is false.
! This can happen in multi-physics applications
!WARN: This routine should be implemented by the subclass

INTERFACE AbstractKernelInitiate
  MODULE SUBROUTINE obj_Initiate(obj, param, dom, domains)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    !! Kernel object
    TYPE(ParameterList_), INTENT(IN) :: param
    !! Parameter list
    CLASS(Domain_), OPTIONAL, TARGET, INTENT(INOUT) :: dom
    !! Domain of computation
    TYPE(DomainPointer_), OPTIONAL, TARGET, INTENT(INOUT) :: domains(:)
    !! multiple domains is necessary when isCommonDomain is false
  END SUBROUTINE obj_Initiate
END INTERFACE AbstractKernelInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine deallocates the data stored inside the Kernel
!
!# Introduction
! - This subroutine deallocates the data stored inside the Kernel
! - This subroutine should be defined by specific Kernel

INTERFACE AbstractKernelDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractKernelDeallocate

!----------------------------------------------------------------------------
!                                       InitiateFields@InitiateFieldsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the matrix and vector fields
!
!# Introduction
!
! This routine should be implemented by subclass.

INTERFACE
  MODULE SUBROUTINE obj_InitiateFields(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFields
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Assemble@AssembleMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the system of linear equation
!
!# Introduction
! This subroutine assembles the system of linear equation.

INTERFACE
  MODULE SUBROUTINE obj_Assemble(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Assemble
END INTERFACE

!----------------------------------------------------------------------------
!                                       AssembleTanmat@AssembleTanmatMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the system of linear equation
!
!# Introduction
! This subroutine assembles the system of linear equation.

INTERFACE
  MODULE SUBROUTINE obj_AssembleTanmat(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleTanmat
END INTERFACE

!----------------------------------------------------------------------------
!                                             AssembleRHS@AssembleRHSMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the system of linear equation
!
!# Introduction
! This subroutine assembles the system of linear equation.

INTERFACE
  MODULE SUBROUTINE obj_AssembleRHS(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Solve@SolveMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine solves the system of linear equation

INTERFACE
  MODULE SUBROUTINE obj_Solve(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Update@UpdateMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine update the state of the kernel

INTERFACE
  MODULE SUBROUTINE obj_Update(obj, reSet)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: reSet
  END SUBROUTINE obj_Update
END INTERFACE

!----------------------------------------------------------------------------
!                                              UpdateIteration@UpdateMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine update the state of the kernel after an iteration

INTERFACE
  MODULE SUBROUTINE obj_UpdateIteration(obj, reSet)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: reSet
  END SUBROUTINE obj_UpdateIteration
END INTERFACE

!----------------------------------------------------------------------------
!                                              isConverge@ConvergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: Returns true if the iteration in the kernel is converged
!
!# Introduction
! This subroutine returns true if the iteration in the kernel is converged

INTERFACE
  MODULE FUNCTION obj_IsConverged(obj) RESULT(Ans)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    LOGICAL(LGT) :: Ans
  END FUNCTION obj_IsConverged
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This routine displays the content of kernel

INTERFACE AbstractKernelDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE AbstractKernelDisplay

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This routine exports the kernel

INTERFACE AbstractKernelExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    !! Kernel object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! Parameter list
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE AbstractKernelExport

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This initiates the Kernel from input file
!
!# Introduction
! This routine initiates the kernel from input file
! - `hdf5` contains the parameters for kernel in hdf5file format, the main
! content depends upon the type of kernel
! - `dom` is the domain of computation
!
! If `dom` is present then we point to this domain, otherwise, we
! allocate memory for the domain, and use the mesh file information given
! in the hdf5 file to generate that domain.

INTERFACE AbstractKernelImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    !! Kernel object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! Parameter list
    CHARACTER(*), INTENT(IN) :: group
    !! group or directory
    CLASS(Domain_), TARGET, INTENT(INOUT) :: dom
    !! Domain of computation
  END SUBROUTINE obj_Import
END INTERFACE AbstractKernelImport

!----------------------------------------------------------------------------
!                                                 ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE AbstractKernelImportParamFromToml
  MODULE SUBROUTINE obj_ImportParamFromToml(obj, param, table)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportParamFromToml
END INTERFACE AbstractKernelImportParamFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE AbstractKernelImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE AbstractKernelImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE AbstractKernelImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename,  &
    & printToml)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE AbstractKernelImportFromToml

!----------------------------------------------------------------------------
!                                                   ExportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Export kernel to the toml file

INTERFACE
  MODULE SUBROUTINE obj_ExportToToml(obj, afile)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(TxtFile_), INTENT(INOUT) :: afile
  END SUBROUTINE obj_ExportToToml
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary:  This routine writes the data in the hdf5 file
!
!# Introduction
! This routine write the data in the hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_WriteData_hdf5(obj, hdf5, group)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_WriteData_hdf5
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary:  This routine writes the data in the hdf5 file
!
!# Introduction
! This routine write the data in the hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk(obj, vtk, group)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_WriteData_vtk
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary:  This routine writes the data in the xdmf3 file
!
!# Introduction
! This routine write the data in the xdmf3 file

! INTERFACE
!   MODULE SUBROUTINE obj_WriteData_xdmf(obj, xdmf, group)
!     IMPORT :: AbstractKernel_, XDMFFile_, I4B
!     CLASS(AbstractKernel_), INTENT(INOUT) :: obj
!     TYPE(XDMFFile_), INTENT(INOUT) :: xdmf
!     CHARACTER(len=*), INTENT(IN) :: group
!   END SUBROUTINE obj_WriteData_xdmf
! END INTERFACE

!----------------------------------------------------------------------------
!                                                            Run@RunMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 May 2022
! summary: Run the simulation

INTERFACE
  MODULE SUBROUTINE obj_Run(obj, param)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Run
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine Sets the option of the kernel and build the kernel
!
!# Introduction
! This routine builds the kernel
! After calling this routine we can start the main computation.
! The call to this subroutine indicates that all the options have been
! Set to the kernel, and it is ready to be used.

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetCurrentTimeStep@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Nov 2022
! summary: Set the current time step number of kernel

INTERFACE
  MODULE SUBROUTINE obj_SetCurrentTimeStep(obj, its)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: its
  END SUBROUTINE obj_SetCurrentTimeStep
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetIterationNumber@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Nov 2022
! summary: Set the current iteration number of kernel

INTERFACE
  MODULE SUBROUTINE obj_SetIterationNumber(obj, iter)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iter
  END SUBROUTINE obj_SetIterationNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMeshData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Set mesh data

INTERFACE
  MODULE SUBROUTINE obj_SetMeshData(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetMeshData
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetFiniteElements@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Set mesh data

INTERFACE
  MODULE SUBROUTINE obj_SetFiniteElements(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetFiniteElements
END INTERFACE

!----------------------------------------------------------------------------
!                                           SetQuadPointsInSpace@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-31
! summary:  Set quadrature points in space

INTERFACE
  MODULE SUBROUTINE obj_SetQuadPointsInSpace(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetQuadPointsInSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                           SetQuadPointsInTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-31
! summary:  Set quadrature points in space

INTERFACE
  MODULE SUBROUTINE obj_SetQuadPointsInTime(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetQuadPointsInTime
END INTERFACE

!----------------------------------------------------------------------------
!                                   SetLocalElemShapeDataInSpace@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-31
! summary:  Set Local element shape data in space

INTERFACE
  MODULE SUBROUTINE obj_SetLocalElemShapeDataInSpace(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetLocalElemShapeDataInSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                     SetLocalElemShapeDataInTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-31
! summary:  Set Local element shape data in time

INTERFACE
  MODULE SUBROUTINE obj_SetLocalElemShapeDataInTime(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetLocalElemShapeDataInTime
END INTERFACE

!----------------------------------------------------------------------------
!                                     SetGlobalElemShapeDataInTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-31
! summary:  Set Global element shape data in time

INTERFACE
  MODULE SUBROUTINE obj_SetGlobalElemShapeDataInTime(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetGlobalElemShapeDataInTime
END INTERFACE

!----------------------------------------------------------------------------
!                                     SetGlobalElemShapeDataInTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-31
! summary:  Set Global element shape data in time

INTERFACE
  MODULE SUBROUTINE obj_SetGlobalElemShapeDataInSpace(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetGlobalElemShapeDataInSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                          SetFacetFiniteElements@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Set mesh data

INTERFACE
  MODULE SUBROUTINE obj_SetFacetFiniteElements(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetFacetFiniteElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  Get the prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractKernel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                             KernelGetNSDFromID@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION KernelGetNSDFromID(uid) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: uid
    INTEGER(I4B) :: ans
  END FUNCTION KernelGetNSDFromID
END INTERFACE

!----------------------------------------------------------------------------
!                                            KernelGetNSDFromName@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION KernelGetNSDFromName(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION KernelGetNSDFromName
END INTERFACE

!----------------------------------------------------------------------------
!                                   KernelGetCoordinateSystemName@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION KernelGetCoordinateSystemName(uid) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: uid
    TYPE(String) :: ans
  END FUNCTION KernelGetCoordinateSystemName
END INTERFACE

!----------------------------------------------------------------------------
!                                    KernelGetCoordinateSystemID@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION KernelGetCoordinateSystemID(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION KernelGetCoordinateSystemID
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractKernel_Class
