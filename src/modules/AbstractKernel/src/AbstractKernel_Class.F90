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
USE Mesh_Class
USE Domain_Class
USE DomainConnectivity_Class
USE DirichletBC_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE GlobalData
USE HDF5File_Class
USE LinSolverFactory
USE MeshSelection_Class
USE DirichletBC_Class
USE NeumannBC_Class
USE NitscheBC_Class
USE TxtFile_Class
USE VTKFile_Class
USE FiniteElement_Class
USE tomlf, ONLY: toml_table
USE SolidMaterial_Class
USE Field
USE KernelUtility
USE UserFunction_Class

IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractKernel_Class"
CHARACTER(*), PARAMETER :: AbstractKernelEssentialParam =&
  & "/name/engine/coordinateSystem/domainFile/isCommonDomain/gravity/"// &
  & "timeDependency/maxIter/nsd/nnt/tdof/dt/startTime/endTime/"//  &
  & "currentTime/currentTimeStep/totalTimeStep/baseInterpolationForSpace/"//&
  & "baseContinuityForSpace/quadratureTypeForSpace/"//  &
  & "ipTypeForSpace/basisTypeForSpace/"//  &
  & "alphaForSpace/betaForSpace/lambdaForSpace/betaForSpace/"//  &
  & "baseInterpolationForTime/baseContinuityForTime/quadratureTypeForTime/"//&
  & "ipTypeForTime/basisTypeForTime/"//  &
  & "alphaForTime/betaForTime/lambdaForTime/"//  &
  & "/tMaterialInterfaces/tSolidMaterials/tDirichletBC/tWeakDirichletBC/"//  &
  & "isSymNitsche/nitscheAlpha/tNeumannBC/rtoleranceForDisplacement/"//  &
  & "rtoleranceForResidual/atoleranceForDisplacement/tanmatProp/"//&
  & "atoleranceForResidual/rtoleranceForVelocity/atoleranceForVelocity/"//  &
  & "isConstantMatProp/isIsotropic/isIncompressible/algorithm/"//  &
  & "problemType/tOverlappedMaterials/outputPath/tPointSource"

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
PUBLIC :: AbstractKernelImportFromToml
PUBLIC :: AbstractKernelInitiateTangentMatrix
PUBLIC :: AbstractKernelInitiateFields
PUBLIC :: AbstractKernelPreCheckError
PUBLIC :: AbstractKernelPostCheckError
PUBLIC :: AbstractKernelApplyDirichletBC
PUBLIC :: AbstractKernelApplyIC

!----------------------------------------------------------------------------
!                                                           AbstractKernel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 April 2022
! summary: Abstract class for kernel

TYPE, ABSTRACT :: AbstractKernel_
  LOGICAL(LGT) :: isConstantMatProp = DEFAULT_isConstantMatProp
  !! Set it to True if the material properties are constant
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! This variable is Set to true when we initiate the kernel
  LOGICAL(LGT) :: isCommonDomain = DEFAULT_isCommonDomain
  !! This variable is True when the domain is common
  !! It is useful in the case of multi-physics simulations.
  !! In multi-physics applications different fields can have different
  !! type and order of shape functions. To construct such shape functions
  !! we may have to use different finite element meshes (hence, domains).
  !! For example, in the fluid mechanics, we often use Taylor-Hood element
  !! Which employs different order of interpolation for pressure and velocity.
  !! NOTE: In most of the application isCommonDomain is TRUE.
  LOGICAL(LGT) :: ismaterialInterfaces = .FALSE.
  !! True if materialInterfaces are allocated
  !! We can have multiple solids
  LOGICAL(LGT) :: isIsotropic = DEFAULT_isIsotropic
  !! Set it to True for isotropic elasticity.
  LOGICAL(LGT) :: isIncompressible = DEFAULT_isIncompressible
    !! TRUE if the material is incompressible
  LOGICAL(LGT) :: isNitsche = .FALSE.
  !! If true, then it means weak dirichlet boundary condition is used
  !! This variable is set in Initiate method
  !! This variable is set to true if the tWeakDirichletBCForDisplacement
  !! is greater than zero, otherwise it is set to false
  INTEGER(I4B) :: problemType = DEFAULT_PROBLEM_TYPE
  !! Kernel problem type
  !! KernelProblemType%scalar
  !! KernelProblemType%Vector
  !! KernelProblemType%MultiPhysics
  INTEGER(I4B) :: tOverlappedMaterials = DEFAULT_tOverlappedMaterials
  !! Total overlapped materials (like fluid, soil, solid)
  INTEGER(I4B) :: tSolidMaterials = 0
  !! Total number of solid materials
  INTEGER(I4B) :: SOLID_MATERIAL_ID = 0
  !! solid material id
  INTEGER(I4B) :: algorithm = 0
  !! algorithm
  TYPE(String) :: name
  !! This is the name of the kernel. It can be anything you want.
  TYPE(String) :: engine
  !! Which type of linear solver library (engine) we use to
  !! solve system of linear equations. We can specify following
  !! values.
  !! `NATIVE_SERIAL`, `NATIVE_OMP`, `NATIVE_MPI`, `LIS_SERIAL`
  !! `LIS_OMP`, `LIS_MPI`, `PETSC`
  TYPE(String) :: tanmatProp
  !! Symmetric or Unsymmetric tangent matrix
  TYPE(String) :: outputPath
  !! Path to put output files
  !! Default is results
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
  INTEGER(I4B) :: timeDependency = DEFAULT_TimeDependency
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
  REAL(DFP), ALLOCATABLE :: timeVec(:)
  !! time vector
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
  REAL(DFP) :: nitscheAlpha = DEFAULT_nitscheAlpha
  !! coefficient for nitsche formulation
  REAL(DFP) :: nitscheType = Nitsche_Sym
  !! -1.0 for symmetric formulation
  !! 1.0 for skew symmetric formulation
  REAL(DFP) :: incrementScale = 1.0_DFP
  !! x = x + incrementScale * displacement
  REAL(DFP) :: rtoleranceForDisplacement = DEFAULT_rtoleranceForDisplacement
  !! relative tolerance for convergence in displacement field
  REAL(DFP) :: atoleranceForDisplacement = DEFAULT_atoleranceForDisplacement
  !! absolute tolerance for displacement field
  REAL(DFP) :: displacementError0 = 0.0_DFP
  !! initial displacement error
  REAL(DFP) :: displacementError = 0.0_DFP
  !! displacement error
  REAL(DFP) :: rtoleranceForVelocity = DEFAULT_rtoleranceForVelocity
  !! relative tolerance for convergence in velocity field
  REAL(DFP) :: atoleranceForVelocity = DEFAULT_atoleranceForVelocity
  !! absolute tolerance for convergence in velocity field
  REAL(DFP) :: velocityError0 = 0.0_DFP
  !! initial velocity error
  REAL(DFP) :: velocityError = 0.0_DFP
  !! velocity error
  REAL(DFP) :: rtoleranceForResidual = DEFAULT_rtoleranceForResidual
  !! relative tolerance for convergence in velocity field
  REAL(DFP) :: atoleranceForResidual = DEFAULT_atoleranceForResidual
  !! absolute tolerance for convergence in velocity field
  REAL(DFP) :: residualError0 = 0.0_DFP
  !! initial velocity error
  REAL(DFP) :: residualError = 0.0_DFP
  !! velocity error
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
  INTEGER(I4B) :: ipTypeForSpace = DEFAULT_ipTypeForSpace
  !! Interpolation grid used for Lagrange polynomials
  INTEGER(I4B) :: basisTypeForSpace = DEFAULT_basisTypeForSpace
  !! Basis type for space
  REAL(DFP) :: alphaForSpace = DEFAULT_alphaForSpace
  !! Parameter for Jacobi polynomials in space
  REAL(DFP) :: betaForSpace = DEFAULT_betaForSpace
  !! Parameter for Jacobi polynomials in space
  REAL(DFP) :: lambdaForSpace = DEFAULT_lambdaForSpace
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
  REAL(DFP) :: alphaForTime = DEFAULT_alphaForTime
  !! Parameter for Jacobi polynomials in space
  REAL(DFP) :: betaForTime = DEFAULT_betaForTime
  !! Parameter for Jacobi polynomials in space
  REAL(DFP) :: lambdaForTime = DEFAULT_lambdaForTime
  !! Parameter for Ultraspherical polynomials in space
  TYPE(String) :: domainFile
  !! Domain file name
  TYPE(QuadraturePoint_), ALLOCATABLE :: quadratureForSpace(:)
  !! Quadrature points in space element (cell element)
  !! The size of quadratureForSpace is same as the total number of
  !! mesh in the domain
  TYPE(QuadraturePoint_), ALLOCATABLE :: quadratureForSpace_facet(:)
  !! Quadrature points in space element (facet element)
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
    !! cell data only
  TYPE(ElemshapeData_), ALLOCATABLE :: spaceElemSD(:)
    !! Element shape data on space element
    !! cell data only
  TYPE(ElemshapeData_), ALLOCATABLE :: linSpaceElemSD_facet(:)
    !! Element shape data on linear space (simplex) element
    !! facet element
  TYPE(ElemshapeData_), ALLOCATABLE :: spaceElemSD_facet(:)
    !! Element shape data on space element
    !! Facet element
  TYPE(STElemshapeData_), ALLOCATABLE :: stelemsd(:, :)
    !! Element shape data on space-time element
  TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
  !! Dirichlet boundary condition for displacement
  TYPE(NeumannBCPointer_), ALLOCATABLE :: nbc(:)
  !! Neumann boundary condition for displacement
  TYPE(NeumannBCPointer_), ALLOCATABLE :: nbcPointSource(:)
  !! Neumann boundary condition for displacement
  TYPE(NitscheBCPointer_), ALLOCATABLE :: wdbc(:)
  !! Weak dirichlet boundary condition for displacement
  INTEGER(I4B), ALLOCATABLE :: nitscheLocalID(:)
  !! nitscheLocalID is a mapping from global mesh-id (of dimension
  !! nsd-1), to local id.
  !! If nitscheLocalID(meshID) = 0, then it means
  !! meshID of dimension nsd-1 is not a nitsche boundary.
  !! This mapping is used to access entries in nitscheFacetToCell.
  TYPE(DomainConnectivityPointer_), ALLOCATABLE :: nitscheFacetToCell(:)
  !! Nitsche facet to cell data connectivity information
  !! We form FacetToCellData for each Nitsche boundary
  !! The size of nitscheFacetToCell is same as the
  !! total number of boundaries (mesh-ids) in wdbcForDisplacement
  !! This data is initiated in Set Method
  ! TYPE(ScalarMeshField_), ALLOCATABLE :: shearModulus(:)
  !! Young's modulus, needed in case of Isotropic elasticity
  ! TYPE(ScalarMeshField_), ALLOCATABLE :: youngsModulus(:)
  !! Poisson's ratio, needed in case of Isotropic elasticity
  ! TYPE(TensorMeshField_), ALLOCATABLE :: Cijkl(:)
  !! Elasticity tensor used for non isotropic materials
  ! TYPE(TensorMeshField_), ALLOCATABLE :: stress(:)
  !! Stress field
  ! TYPE(TensorMeshField_), ALLOCATABLE :: strain(:)
  !! Strain field
  INTEGER(I4B), ALLOCATABLE :: materialInterfaces(:)
  !! mesh id of material interfaces
  TYPE(DomainConnectivity_), ALLOCATABLE :: matIfaceConnectData(:)
  !! facet to cell data for each materialInterface mesh
  !! The size of matIfaceConnectData is same as the size of
  !! materialInterfaces
  TYPE(MeshSelection_), ALLOCATABLE :: solidMaterialToMesh(:)
  !! Map solid material to the mesh portion
  !! The size of solidMaterialToMesh is the same as `tSolidMaterials`
  !! In this way, solidMaterialToMesh(i) gives the mesh region of ith element
  TYPE(SolidMaterialPointer_), ALLOCATABLE :: solidMaterial(:)
  !! Pointer to the solid material
  TYPE(MatrixFieldPointer_), ALLOCATABLE :: matrixFields(:)
  !! List of vectorfields
  TYPE(VectorFieldPointer_), ALLOCATABLE :: vectorFields(:)
  !! List of vectorfields
  TYPE(ScalarFieldPointer_), ALLOCATABLE :: scalarFields(:)
  !! List of scalarFields
  CLASS(MatrixField_), POINTER :: stiffnessMat => NULL()
  !! Global Stiffness matrix
  CLASS(MatrixField_), POINTER :: massMat => NULL()
  !! Global mass matrix
  CLASS(MatrixField_), POINTER :: dampingMat => NULL()
  !! Global damping matrix
  CLASS(VectorField_), POINTER :: displacement => NULL()
  !! Vector field for nodal displacement
  CLASS(VectorField_), POINTER :: velocity => NULL()
  !! Vector field for nodal displacement
  CLASS(VectorField_), POINTER :: acceleration => NULL()
  !! Vector field for nodal acceleration
  CLASS(VectorField_), POINTER :: nodeCoord => NULL()
  !! Vector field for nodal coordinates
  TYPE(VectorMeshFieldPointer_), ALLOCATABLE :: solidMechData(:)
  !! Constitutive data for solid materials
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE :: massDensity(:)
  !! Mass density
  !! This will be a scalar mesh field
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE :: shearModulus(:)
  !! Lame parameter
  !! NOTE: It is need in the case of Isotropic elasticity
  !! This will be a scalar mesh field
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE :: youngsModulus(:)
  !! Lame parameter
  !! NOTE: It is need in the case of Isotropic elasticity
  !! This will be a scalar mesh field
  TYPE(AbstractTensorMeshFieldPointer_), ALLOCATABLE :: Cijkl(:)
  !! Elasticity tensor
  !! NOTE: It is used for non Isotropic elasticity
  !! This will be a tensor mesh field
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE :: dampCoeff_alpha(:)
  !! Rayleigh damping coefficient alpha
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE :: dampCoeff_beta(:)
  !! Rayleigh damping coefficient beta
  TYPE(AbstractVectorMeshFieldPointer_), ALLOCATABLE :: stress(:)
  !! Stress tensor
  !! This will be a tensor mesh field
  TYPE(AbstractVectorMeshFieldPointer_), ALLOCATABLE :: strain(:)
  !! Strain tensor
  !! This will be a tensor mesh field
  CLASS(UserFunction_), POINTER :: bodySourceFunc => NULL()
  !! body force function
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
  PROCEDURE, PUBLIC, PASS(obj) :: PreCheckError => obj_PreCheckError
  !! Check error before setting kernel
  PROCEDURE, PUBLIC, PASS(obj) :: PostCheckError => obj_PostCheckError
  !! Check error after setting kernel

  ! CONSTRUCTOR:
  ! @InitiateFieldsMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFields => obj_InitiateFields
  !! Initiate the fields
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateTangentMatrix =>  &
    & obj_InitiateTangentMatrix
  !! Initiate tangent matrix
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateScalarFields =>  &
    & obj_InitiateScalarFields
  !! Initiate scalar fields
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateVectorFields => &
    & obj_InitiateVectorFields
  !! Initiate vector fields
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateMatrixFields => &
    & obj_InitiateMatrixFields
  !! Initiate vector fields

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Returns the prefix of the kernel
  !! This method should be implemented by the specific kernel

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: PreSet => obj_PreSet
  !! Perform final check, before starting the actual computations
  PROCEDURE, PUBLIC, PASS(obj) :: PostSet => obj_PostSet
  !! Perform final check, before starting the actual computations
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
  PROCEDURE, PUBLIC, PASS(obj) :: SetMatIFaceConnectData =>  &
    & obj_SetMatIFaceConnectData

  ! SET:
  ! @BCMethods
  PROCEDURE, PUBLIC, PASS(obj) :: AddDirichletBC => obj_AddDirichletBC
  !! Add dirichlet boundary conditions
  PROCEDURE, PUBLIC, PASS(obj) :: AddNeumannBC => obj_AddNeumannBC
  !! Add Neumann boundary condition
  PROCEDURE, PUBLIC, PASS(obj) :: AddPointSource => obj_AddPointSource
  !! Add point source in the nbcPointSource
  PROCEDURE, PUBLIC, PASS(obj) :: AddNitscheBC => obj_AddNitscheBC
  !! Add weak dirichlet boundary conditions to wdbc
  PROCEDURE, PUBLIC, PASS(obj) :: GetDirichletBCPointer => &
    & obj_GetDirichletBCPointer
  !! Get pointer to the pressure dirichlet boundary condition
  PROCEDURE, PUBLIC, PASS(obj) :: GetNitscheBCPointer => &
    & obj_GetNitscheBCPointer
  !! Get pointer to the pressure dirichlet boundary condition
  PROCEDURE, PUBLIC, PASS(obj) :: GetNeumannBCPointer => &
    & obj_GetNeumannBCPointer
  !! Get pointer to the neumann boundary condition for velocity
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointSourcePointer =>  &
    & obj_GetPointSourcePointer
  !! Get point source in the nbcPointSource
  PROCEDURE, PUBLIC, PASS(obj) :: SetNitscheMeshData => &
    & obj_SetNitscheMeshData
  !! This routine set mesh data necessary for implementing the
  !! Nitsche boundary condition.

  ! SET:
  ! @ApplyDirichletBCMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ApplyDirichletBC =>  &
    & obj_ApplyDirichletBC
  !! Apply Dirichlet boundary condition

  ! SET:
  ! @ApplyICMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ApplyIC => obj_ApplyIC
  !! Apply Dirichlet boundary condition

  ! SET:
  ! @MaterialMethods
  PROCEDURE, PUBLIC, PASS(obj) :: AddSolidMaterial => obj_AddSolidMaterial
  !! Add a solid material to the kernel
  PROCEDURE, PUBLIC, PASS(obj) :: GetSolidMaterialPointer =>  &
    & obj_GetSolidMaterialPointer
  !! Get a solid material to the kernel
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateMaterialProperties =>  &
    & obj_InitiateMaterialProperties
  !! Initiate material properties
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterialProperties =>  &
    & obj_SetMaterialProperties
  !! Set material properties
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateMassDensity =>  &
    & obj_InitiateMassDensity
  !! Initiate mass density field if massDensity is defined in the materials
  PROCEDURE, PUBLIC, PASS(obj) :: SetMassDensity =>  &
    & obj_SetMassDensity
  !! Set mass density if mass density is defined in the materials
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElasticityProperties =>  &
    & obj_InitiateElasticityProperties
  !! Initiate lame parameters for isotropic elasticity
  PROCEDURE, PUBLIC, PASS(obj) :: SetElasticityProperties =>  &
    & obj_SetElasticityProperties
  !! Set Lame parameters for isotropic elasticity
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateDampingProperties =>  &
    & obj_InitiateDampingProperties
  !! Initiate rayleight damping properties
  PROCEDURE, PUBLIC, PASS(obj) :: SetDampingProperties =>  &
    & obj_SetDampingProperties
  !! Set Lame parameters for isotropic elasticity
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterialToDomain =>  &
    & obj_SetMaterialToDomain
  !! Set material to mesh
  PROCEDURE, PUBLIC, PASS(obj) :: SetElementToMatID =>  &
    & obj_SetElementToMatID
  !! Set element to material id

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
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleMassMat => obj_AssembleMassMat
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleStiffnessMat =>  &
    & obj_AssembleStiffnessMat
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleDampingMat =>  &
    & obj_AssembleDampingMat
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleNitscheMat =>  &
    & obj_AssembleNitscheMat

  ! SET:
  ! @AssembleRHSMethods
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHS => obj_AssembleRHS
  !! This procedure pointer assembles the right-hand-side vector
  PROCEDURE, PUBLIC, PASS(obj) :: SetBodySourceFunc => obj_SetBodySourceFunc
  !! Set body force function
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleBodySource => &
    & obj_AssembleBodySource
  !! This procedure assemble the body force term to RHS
  PROCEDURE, PUBLIC, PASS(obj) :: AssembleSurfaceSource => &
    & obj_AssembleSurfaceSource
  !! This procedure assemble the surface force term to RHS
  PROCEDURE, PUBLIC, PASS(obj) :: AssemblePointSource => &
    & obj_AssemblePointSource
  !! This procedure assemble the point source to rhs

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
    & param, prefix, problemType, name, engine, coordinateSystem, &
    & domainFile, isCommonDomain, gravity, timeDependency, &
    & maxIter, nsd, nnt, tdof, dt, startTime, endTime, &
    & currentTime, currentTimeStep, totalTimeStep, &
    & baseInterpolationForSpace, baseContinuityForSpace, &
    & quadratureTypeForSpace, ipTypeForSpace, &
    & basisTypeForSpace, alphaForSpace, &
    & betaForSpace, lambdaForSpace, &
    & baseInterpolationForTime, baseContinuityForTime, &
    & quadratureTypeForTime, ipTypeForTime, &
    & basisTypeForTime, alphaForTime, betaForTime, lambdaForTime, &
    & postProcessOpt, tDirichletBC, tNeumannBC, tWeakDirichletBC, &
    & isSymNitsche, nitscheAlpha, materialInterfaces, isConstantMatProp, &
    & tSolidMaterials, algorithm, isIsotropic, isIncompressible,  &
    & rtoleranceForDisplacement, atoleranceForDisplacement,  &
    & rtoleranceForVelocity, atoleranceForVelocity,  &
    & rtoleranceForResidual, atoleranceForResidual, tanmatProp,  &
    & tOverlappedMaterials, outputPath, tPointSource)
    CHARACTER(*), INTENT(IN) :: prefix
    INTEGER(I4B), INTENT(IN) :: problemType
    !! Kernel problem type. Problem can be scalar, vector, or multi-physics
    !! KernelProblemType%Scalar
    !! KernelProblemType%Vector
    !! KernelProblemType%MultiPhysics
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
    !! Interpolation point type in space
    !! Default is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipTypeForTime
    !! Interpolation point type in time
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisTypeForSpace
    !! Basis type used for constructing the shape functions
    !! Like monomials, Legendre, Chebyshev, Jacobi
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisTypeForTime
    !! Basis type used for constructing the shape function in time
    REAL(DFP), OPTIONAL, INTENT(IN) :: alphaForSpace
    !! Jacobi polynomial parameter  in space
    REAL(DFP), OPTIONAL, INTENT(IN) :: alphaForTime
    !! Jacobi polynomial parameter in time
    REAL(DFP), OPTIONAL, INTENT(IN) :: betaForSpace
    !! Jacobi polynomial parameter in space
    REAL(DFP), OPTIONAL, INTENT(IN) :: betaForTime
    !! Jacobi polynomial parameter in time
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambdaForSpace
    !! Ultraspherical polynomial parameter in space
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambdaForTime
    !! Ultraspherical polynomial parameter in time
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tDirichletBC
    !! Total number of Dirichlet domain for pressure, default=0
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tWeakDirichletBC
    !! Total number of Nitsche boundary conditions for displacement
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tNeumannBC
    !! Total number of Neumann domain for pressure, default=0
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSymNitsche
    !! True if symmetric Nitsche formulation
    REAL(DFP), OPTIONAL, INTENT(IN) :: nitscheAlpha
    !! Alpha parameter used in Nitsche formulation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: materialInterfaces(:)
    !! Mesh-IDs of materialInterfaces
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSolidMaterials
    !! total number of materials
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isConstantMatProp
    !! It is true if the material properties are constant
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: algorithm
    !! algorithm
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isIsotropic
    !! It is true if the material is isotropic
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isIncompressible
    !! It is true if the material is incompressible
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtoleranceForDisplacement
    !! relative tolerance for convergence in displacement field
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtoleranceForVelocity
    !! relative tolerance for convergence in velocity field
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtoleranceForResidual
    !! relative tolerance for velocity field
    REAL(DFP), OPTIONAL, INTENT(IN) :: atoleranceForDisplacement
    !! absolute tolerance for convergence in displacement field
    REAL(DFP), OPTIONAL, INTENT(IN) :: atoleranceForVelocity
    !! absolute tolerance for convergence in velocity field
    REAL(DFP), OPTIONAL, INTENT(IN) :: atoleranceForResidual
    !! absolute tolerance for velocity
    CHARACTER(*), OPTIONAL, INTENT(IN) :: tanmatProp
    !! Tangent matrix properties
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tOverlappedMaterials
    !! Total number of overlapped materials
    CHARACTER(*), OPTIONAL, INTENT(IN) :: outputPath
    !! path where output of kernel will be written
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPointSource
    !! total number of point sources, size of nbcPointSource
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
!                                         PreCheckError@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-06
! summary: Check error before setting the kernel

INTERFACE AbstractKernelPreCheckError
  MODULE SUBROUTINE obj_PreCheckError(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_PreCheckError
END INTERFACE AbstractKernelPreCheckError

!----------------------------------------------------------------------------
!                                          PostCheckError@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-06
! summary: Check error after setting the kernel

INTERFACE AbstractKernelPostCheckError
  MODULE SUBROUTINE obj_PostCheckError(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_PostCheckError
END INTERFACE AbstractKernelPostCheckError

!----------------------------------------------------------------------------
!                                             CheckError@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-06
! summary: Check error

INTERFACE AbstractKernelCheckError
  MODULE SUBROUTINE obj_CheckError(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_CheckError
END INTERFACE AbstractKernelCheckError

!----------------------------------------------------------------------------
!                                 InitiateTangentMatrix@InitiateFieldMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the tangent matrix

INTERFACE AbstractKernelInitiateTangentMatrix
  MODULE SUBROUTINE obj_InitiateTangentMatrix(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateTangentMatrix
END INTERFACE AbstractKernelInitiateTangentMatrix

!----------------------------------------------------------------------------
!                                InitiateScalarFields@InitiateFieldsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the matrix and vector fields

INTERFACE
  MODULE SUBROUTINE obj_InitiateScalarFields(obj, names)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: names(:)
  END SUBROUTINE obj_InitiateScalarFields
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateVectorFields@InitiateFieldsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the matrix and vector fields

INTERFACE
  MODULE SUBROUTINE obj_InitiateVectorFields(obj, names)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: names(:)
  END SUBROUTINE obj_InitiateVectorFields
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateMatrixFields@InitiateFieldsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the matrix and vector fields

INTERFACE
  MODULE SUBROUTINE obj_InitiateMatrixFields(obj, names, matrixProp,  &
    & spaceCompo, timeCompo)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: names(:)
    TYPE(String), INTENT(IN) :: matrixProp(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
  END SUBROUTINE obj_InitiateMatrixFields
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateFields@InitiateFieldsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the matrix and vector fields

INTERFACE AbstractKernelInitiateFields
  MODULE SUBROUTINE obj_InitiateFields(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFields
END INTERFACE AbstractKernelInitiateFields

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
!                                                         PreSet@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine Sets the option of the kernel and build the kernel

INTERFACE
  MODULE SUBROUTINE obj_PreSet(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_PreSet
END INTERFACE

!----------------------------------------------------------------------------
!                                                         PostSet@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine Sets the option of the kernel and build the kernel

INTERFACE
  MODULE SUBROUTINE obj_PostSet(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_PostSet
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
!                                               SetMaterialToMesh@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-07
! summary:  Set material to mesh

INTERFACE
  MODULE SUBROUTINE obj_SetMaterialToDomain(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetMaterialToDomain
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetMaterialToMesh@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-07
! summary:  Set material to mesh

INTERFACE
  MODULE SUBROUTINE obj_SetElementToMatID(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetElementToMatID
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetMatIFaceConnectData@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-07
! summary:  Set material to mesh

INTERFACE
  MODULE SUBROUTINE obj_SetMatIFaceConnectData(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetMatIFaceConnectData
END INTERFACE

!----------------------------------------------------------------------------
!                                          AddSolidMaterial@MaterialMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 April 2022
! summary: This routine Adds a solid material to kernel
!
!# Introduction
!
! - This routine Adds Solid material
! - It also prepares `obj%SolidMaterialToMesh(materialNo)` and
! `obj%solidMaterial(materialNo)`.
! - `param` contains the parameters for constructing a `SolidMaterial_`
! - `materialName` is the name of material, it should be `solidMaterial`.
! - `region` is an instance of MeshSelection.
!
!@warning
! `materialNo` should be lesser than or equal to the total
! number of Solid materials
!@endwarning

INTERFACE
  MODULE SUBROUTINE obj_AddSolidMaterial(obj, materialNo, materialName, &
    & param, region)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: materialNo
    !! material number should be lesser than or equal to the
    !! total number of solid materials.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: materialName
    !! name of the material
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
    !! parameters
    TYPE(MeshSelection_), OPTIONAL, INTENT(IN) :: region
    !! mesh region
  END SUBROUTINE obj_AddSolidMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                          AddSolidMaterial@MaterialMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 April 2022
! summary: This routine returns pointer to solid material to kernel

INTERFACE
  MODULE FUNCTION obj_GetSolidMaterialPointer(obj, materialNo) RESULT(ans)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: materialNo
    !! material number should be lesser than or equal to the
    !! total number of solid materials.
    CLASS(SolidMaterial_), POINTER :: ans
  END FUNCTION obj_GetSolidMaterialPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateConstantMatProp@MaterialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Initiate material properties

INTERFACE
  MODULE SUBROUTINE obj_InitiateMaterialProperties(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateMaterialProperties
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetConstantMatProp@MaterialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Set material properties

INTERFACE
  MODULE SUBROUTINE obj_SetMaterialProperties(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetMaterialProperties
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateMassDensity@MaterialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Initiate mass density field

INTERFACE
  MODULE SUBROUTINE obj_InitiateMassDensity(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateMassDensity
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetMassDensity@MaterialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Set mass density

INTERFACE
  MODULE SUBROUTINE obj_SetMassDensity(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetMassDensity
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateElasticityProperties@MaterialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Initiate elasticity properties

INTERFACE
  MODULE SUBROUTINE obj_InitiateElasticityProperties(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateElasticityProperties
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetMassDensity@MaterialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Set elasticity properties

INTERFACE
  MODULE SUBROUTINE obj_SetElasticityProperties(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetElasticityProperties
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateDampingProperties@MaterialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Initiate elasticity properties

INTERFACE
  MODULE SUBROUTINE obj_InitiateDampingProperties(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateDampingProperties
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetMassDensity@MaterialMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Set elasticity properties

INTERFACE
  MODULE SUBROUTINE obj_SetDampingProperties(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetDampingProperties
END INTERFACE

!----------------------------------------------------------------------------
!                                                   AddDirichletBC@BCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 April 2022
! summary: This routine sets dirichlet boundary condition for pressure field
!
!# Introduction
!
! - This routine sets the Dirichlet boundary condition for pressure field
! in [[AbstractElasticity_]] kernel.
! - It also makes the `obj%DBCForPressure(dbcNo)`
!
! - `dbcNo` should be lesser than total dirichlet boundary condition for
! pressure field

INTERFACE
  MODULE SUBROUTINE obj_AddDirichletBC(obj, param, boundary, dbcNo)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcNo
    !! Dirichlet boundary nunber
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[DirichletBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
  END SUBROUTINE obj_AddDirichletBC
END INTERFACE

!----------------------------------------------------------------------------
!                                                     AddNeumannBC@BCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine adds Neumann BC in nbc
!
!# Introduction
!
! - This routine adds the Neumann boundary condition to nbc

INTERFACE
  MODULE SUBROUTINE obj_AddNeumannBC(obj, param, boundary, nbcNo)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[NeumannBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nbcNo
    !! Neumann boundary number
    !! If nbcNo is not present then the default will be
    !! size(obj%nbc) + 1
    !! If nbcNo is not present then this method works as append
    !! if nbcNo is out of bound then size of nbc is expanded
  END SUBROUTINE obj_AddNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!                                                   AddPointSource@BCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine adds a point source NBC to nbcPointSource
!
!# Introduction
!
! - This routine sets the Neumann boundary condition for point source

INTERFACE
  MODULE SUBROUTINE obj_AddPointSource(obj, param, boundary, nbcNo)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[NeumannBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nbcNo
    !! Neumann boundary number
  END SUBROUTINE obj_AddPointSource
END INTERFACE

!----------------------------------------------------------------------------
!                                                    AddNitscheBC@BCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 April 2022
! summary: This routine sets dirichlet boundary condition for displacement

INTERFACE
  MODULE SUBROUTINE obj_AddNitscheBC(obj, param, boundary, dbcNo)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[DirichletBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcNo
    !! Dirichlet boundary nunber
  END SUBROUTINE obj_AddNitscheBC
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetDirichletBCPointer@BCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine returns the pointer to dirichlet boundary condition
!
!# Introduction
!
! - This routine returns the pointer to Dirichlet boundary condition of
! pressure field in [[AbstractElasticity_]] kernel, that is
! `obj%DBCForPressure(dbcNo)%ptr`.
! - After obtaining the Dirichlet boundary condition pointer, user can set the
! boundary condition
! - `dbcNo` should be lesser than total dirichlet boundary condition

INTERFACE
  MODULE FUNCTION obj_GetDirichletBCPointer(obj, dbcNo) RESULT(ans)
    CLASS(AbstractKernel_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcNo
    !! Dirichlet boundary nunber
    CLASS(DirichletBC_), POINTER :: ans
  END FUNCTION obj_GetDirichletBCPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetNitscheBCPointer@BCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine returns the pointer to dirichlet boundary condition

INTERFACE
  MODULE FUNCTION obj_GetNitscheBCPointer(obj, dbcNo) RESULT(ans)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcNo
    !! Nitsche boundary nunber
    CLASS(NitscheBC_), POINTER :: ans
  END FUNCTION obj_GetNitscheBCPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetNeumannBCPointer@BCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine returns the pointer to Neumann boundary condition
!
!# Introduction
!
! - This routine returns the pointer to Neumann boundary condition, that is
! `obj%nbc(nbcNo)%ptr`.
! - After obtaining the Neumann boundary condition pointer, user can set the
! boundary condition
! - `nbcNo` should be lesser than total Neumann boundary condition, that
! is size of `nbc`
! - if nbcNo is absent then its default value is size of nbc, in this case
! this method returns the last entry in nbc.

INTERFACE
  MODULE FUNCTION obj_GetNeumannBCPointer(obj, nbcNo) RESULT(ans)
    CLASS(AbstractKernel_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nbcNo
    !! Neumann boundary nunber
    CLASS(NeumannBC_), POINTER :: ans
  END FUNCTION obj_GetNeumannBCPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetPointSourcePointer@BCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine returns the pointer to Neumann boundary condition
!
!# Introduction
!
! - This routine returns the pointer to Neumann boundary condition, that is
! `obj%nbcPointSource(nbcNo)%ptr`.
! - After obtaining the Neumann boundary condition pointer, user can set the
! boundary condition
! - `nbcNo` should be lesser than total Neumann boundary condition, that
! is size of `nbcPointSource`
! - if nbcNo is absent then its default value is size of nbcPointSource,
! in this case this method returns the last entry in nbcPointSource.

INTERFACE
  MODULE FUNCTION obj_GetPointSourcePointer(obj, nbcNo) RESULT(ans)
    CLASS(AbstractKernel_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nbcNo
    !! Neumann boundary nunber
    CLASS(NeumannBC_), POINTER :: ans
  END FUNCTION obj_GetPointSourcePointer
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetNitscheMeshData@BCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-19
! summary: Set mesh data for Nitsche boundary condition

INTERFACE
  MODULE SUBROUTINE obj_SetNitscheMeshData(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetNitscheMeshData
END INTERFACE

!----------------------------------------------------------------------------
!                                  ApplyDirichletBC@ApplyDirichletBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-20
! summary: Apply Dirichlet boundary condition

INTERFACE AbstractKernelApplyDirichletBC
  MODULE SUBROUTINE obj_ApplyDirichletBC(obj, name, times, extField)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    !! Abstract kernel
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    !! name of variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! Time vector
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
    !! External field
  END SUBROUTINE obj_ApplyDirichletBC
END INTERFACE AbstractKernelApplyDirichletBC

!----------------------------------------------------------------------------
!                                                    ApplyIC@ApplyICMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-20
! summary: Apply initial conditions to the fields

INTERFACE AbstractKernelApplyIC
  MODULE SUBROUTINE obj_ApplyIC(obj, name, func, extField, times, ivar,  &
    & idof, spaceCompo, timeCompo)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    !! Abstract kernel
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    !! name of variable
    CLASS(UserFunction_), OPTIONAL, INTENT(INOUT) :: func
    !! User function
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
    !! External field
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! Time vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE obj_ApplyIC
END INTERFACE AbstractKernelApplyIC

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

INTERFACE
  MODULE SUBROUTINE obj_AssembleTanmat(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleTanmat
END INTERFACE

!----------------------------------------------------------------------------
!                                       AssembleMassMat@AssembleTanmatMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the mass matrix of the system

INTERFACE
  MODULE SUBROUTINE obj_AssembleMassMat(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleMassMat
END INTERFACE

!----------------------------------------------------------------------------
!                                 AssembleStiffnessMat@AssembleTanmatMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the stiffness matrix of the system

INTERFACE
  MODULE SUBROUTINE obj_AssembleStiffnessMat(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleStiffnessMat
END INTERFACE

!----------------------------------------------------------------------------
!                                 AssembleDampingMat@AssembleTanmatMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the damping matrix of the system

INTERFACE
  MODULE SUBROUTINE obj_AssembleDampingMat(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleDampingMat
END INTERFACE

!----------------------------------------------------------------------------
!                                  AssembleNitscheMat@AssembleTanmatMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the Nitsche matrix of the system

INTERFACE
  MODULE SUBROUTINE obj_AssembleNitscheMat(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleNitscheMat
END INTERFACE

!----------------------------------------------------------------------------
!                                             AssembleRHS@AssembleRHSMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the system of linear equation

INTERFACE
  MODULE SUBROUTINE obj_AssembleRHS(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                       SetBodyForceFunc@AssembleRHSMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the system of linear equation

INTERFACE
  MODULE SUBROUTINE obj_SetBodySourceFunc(obj, func)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), TARGET, INTENT(IN) :: func
  END SUBROUTINE obj_SetBodySourceFunc
END INTERFACE

!----------------------------------------------------------------------------
!                                       AssembleBodySource@AssembleRHSMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the body source to rhs

INTERFACE
  MODULE SUBROUTINE obj_AssembleBodySource(obj, func, extField)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), OPTIONAL, INTENT(INOUT) :: func
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_AssembleBodySource
END INTERFACE

!----------------------------------------------------------------------------
!                                  AssembleSurfaceSource@AssembleRHSMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the surface force terms in RHS

INTERFACE
  MODULE SUBROUTINE obj_AssembleSurfaceSource(obj, extField)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_AssembleSurfaceSource
END INTERFACE

!----------------------------------------------------------------------------
!                                     AssemblePointSource@AssembleRHSMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Aug 2021
! summary: This subroutine assembles the point sources

INTERFACE
  MODULE SUBROUTINE obj_AssemblePointSource(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssemblePointSource
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
!                                         ImportFromToml@ImportTomlMethods
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
!                                           ImportFromToml@ImportTomlMethods
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
!                                           ImportFromToml@ImportTomlMethods
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

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_WriteData_vtk
END INTERFACE

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
!
!----------------------------------------------------------------------------

END MODULE AbstractKernel_Class
