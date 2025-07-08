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

MODULE KernelComponents
USE GlobalData, ONLY: DFP, I4B, LGT

USE String_Class, ONLY: String

USE BaseType, ONLY: ipopt => TypeInterpolationOpt, &
                    polyopt => TypePolynomialOpt, &
                    ElemshapeData_, &
                    STElemshapeData_, &
                    IterationData_

USE AbstractKernelParam, ONLY: TypeKernelProblemOpt, &
                               TypeKernelCoordOpt, &
                               TypeKernelTimeOpt, &
                               TypeKernelNitscheOpt, &
                               KernelBasisOpt_, &
                               KernelErrorOpt_

USE AbstractField_Class, ONLY: TypeField

USE MatrixField_Class, ONLY: MatrixField_, &
                             MatrixFieldPointer_

USE VectorField_Class, ONLY: VectorField_, &
                             VectorFieldPointer_

USE STVectorField_Class, ONLY: STVectorField_, &
                               STVectorFieldPointer_

USE ScalarField_Class, ONLY: ScalarField_, &
                             ScalarFieldPointer_

USE STScalarField_Class, ONLY: STScalarField_, &
                               STScalarFieldPointer_

USE DirichletBC_Class, ONLY: DirichletBCPointer_

USE NeumannBC_Class, ONLY: NeumannBCPointer_

USE NitscheBC_Class, ONLY: NitscheBCPointer_

USE DomainConnectivity_Class, ONLY: DomainConnectivityPointer_, &
                                    DomainConnectivity_

USE SolidMaterial_Class, ONLY: SolidMaterialPointer_

USE AbstractMeshField_Class, ONLY: AbstractScalarMeshFieldPointer_, &
                                   AbstractVectorMeshFieldPointer_, &
                                   AbstractTensorMeshFieldPointer_, &
                                   AbstractScalarMeshField_, &
                                   AbstractTensorMeshField_, &
                                   AbstractVectorMeshField_

USE ScalarMeshField_Class, ONLY: ScalarMeshFieldPointer_

USE VectorMeshField_Class, ONLY: VectorMeshFieldPointer_

USE MeshSelection_Class, ONLY: MeshSelection_

USE BaseType, ONLY: TypeFEVariableOpt

IMPLICIT NONE

PRIVATE

PUBLIC :: KernelMaterials_
PUBLIC :: KernelMeshFields_
PUBLIC :: KernelFields_
PUBLIC :: KernelElemshapeData_
PUBLIC :: KernelBC_
PUBLIC :: KernelOpt_
PUBLIC :: TypeKernelOpt
PUBLIC :: KernelAlgoParam_

!----------------------------------------------------------------------------
!                                                          KernelAlgoParam_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-23
! summary:  Kernel algorithm parameters

TYPE :: KernelAlgoParam_
END TYPE KernelAlgoParam_

!----------------------------------------------------------------------------
!                                                           KernelMaterials_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-23
! summary:  material of kernel

TYPE :: KernelMaterials_
  LOGICAL(LGT) :: isConstantMatProp = .FALSE.
  !! Set it to True if the material properties are constant

  LOGICAL(LGT) :: isIsotropic = .FALSE.
  !! Set it to True for isotropic medium

  LOGICAL(LGT) :: isIncompressible = .FALSE.
  !! TRUE if the material is incompressible

  LOGICAL(LGT) :: isMaterialInterface = .FALSE.
  !! True if materialInterfaces are allocated.
  !! We can have multiple solids

  INTEGER(I4B) :: tOverlappedMaterials = 1
  !! Total overlapped materials (like fluid, soil, solid)

  INTEGER(I4B) :: tSolidMaterials = 0
  !! Total number of solid materials

  INTEGER(I4B) :: SOLID_MATERIAL_ID = 0
  !! solid material id

  INTEGER(I4B) :: tMaterialInterfaces = 0
  !! total number of material interfaces

  TYPE(SolidMaterialPointer_), ALLOCATABLE :: solidMaterial(:)
  !! Pointer to the solid material

  TYPE(MeshSelection_), ALLOCATABLE :: solidMaterialToMesh(:)
  !! Map solid material to the mesh portion
  !! The size of solidMaterialToMesh is the same as `tSolidMaterials`
  !! In this way, solidMaterialToMesh(i) gives the mesh region of ith element

  TYPE(DomainConnectivity_), ALLOCATABLE :: matIfaceConnectData(:)
  !! facet to cell data for each materialInterface mesh
  !! The size of matIfaceConnectData is same as the size of
  !! materialInterfaces

  INTEGER(I4B), ALLOCATABLE :: materialInterfaces(:)
  !! mesh id of material interfaces

END TYPE KernelMaterials_

!----------------------------------------------------------------------------
!                                                          KernelMeshFields_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-23
! summary:  KernelMeshFields

TYPE :: KernelMeshFields_
  TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE :: scalarFields(:)
  !! Scalar mesh fields

  TYPE(AbstractVectorMeshFieldPointer_), ALLOCATABLE :: vectorFields(:)
  !! Vector mesh fields

  TYPE(AbstractTensorMeshFieldPointer_), ALLOCATABLE :: tensorFields(:)
  !! tensor mesh fields

  CLASS(AbstractScalarMeshField_), POINTER :: massDensity => NULL()
  !! Mass density, scalar mesh field

  INTEGER(I4B) :: massDensityFieldType = TypeField%normal
  !! field type for mass density
  !! normal means the field will change from one element to another

  INTEGER(I4B) :: massDensityVarType = TypeField%constant
  !! varType denotes how the field is changing inside the element
  !! constant means the field is constant inside the element
  !! space means the field changes in space element
  !! spaceTime means the field changes in space-time element
  !! time means the field changes in time element

  INTEGER(I4B) :: massDensityDefineOn = TypeFEVariableOpt%quadrature
  !! mass density is defined on quadrature points

  INTEGER(I4B) :: massDensityMaxNNS = 0
  !! maximum number of nodes in space for mass density

  INTEGER(I4B) :: massDensityMaxNNT = 0
  !! maximum number of nodes in time for mass density

  CLASS(AbstractScalarMeshField_), POINTER :: shearModulus => NULL()
  !! Lame parameter
  !! NOTE: It is need in the case of Isotropic elasticity
  !! This will be a scalar mesh field

  INTEGER(I4B) :: shearModulusFieldType = TypeField%normal
  !! field type for mass density
  !! normal means the field will change from one element to another

  INTEGER(I4B) :: shearModulusVarType = TypeField%constant
  !! varType denotes how the field is changing inside the element
  !! constant means the field is constant inside the element
  !! space means the field changes in space element
  !! spaceTime means the field changes in space-time element
  !! time means the field changes in time element

  INTEGER(I4B) :: shearModulusDefineOn = TypeFEVariableOpt%quadrature
  !! mass density is defined on quadrature points

  INTEGER(I4B) :: shearModulusMaxNNS = 0
  !! maximum number of nodes in space for mass density

  INTEGER(I4B) :: shearModulusMaxNNT = 0
  !! maximum number of nodes in time for mass density

  CLASS(AbstractScalarMeshField_), POINTER :: youngsModulus => NULL()
  !! Lame parameter
  !! NOTE: It is need in the case of Isotropic elasticity
  !! This will be a scalar mesh field

  INTEGER(I4B) :: youngsModulusFieldType = TypeField%normal
  !! field type for mass density
  !! normal means the field will change from one element to another

  INTEGER(I4B) :: youngsModulusVarType = TypeField%constant
  !! varType denotes how the field is changing inside the element
  !! constant means the field is constant inside the element
  !! space means the field changes in space element
  !! spaceTime means the field changes in space-time element
  !! time means the field changes in time element

  INTEGER(I4B) :: youngsModulusDefineOn = TypeFEVariableOpt%quadrature
  !! mass density is defined on quadrature points

  INTEGER(I4B) :: youngsModulusMaxNNS = 0
  !! maximum number of nodes in space for mass density

  INTEGER(I4B) :: youngsModulusMaxNNT = 0
  !! maximum number of nodes in time for mass density

  CLASS(AbstractScalarMeshField_), POINTER :: dampCoeff_alpha => NULL()
  !! Rayleigh damping coefficient alpha

  CLASS(AbstractScalarMeshField_), POINTER :: dampCoeff_beta => NULL()
  !! Rayleigh damping coefficient beta

  INTEGER(I4B) :: dampCoeffFieldType = TypeField%normal
  !! field type for mass density
  !! normal means the field will change from one element to another

  INTEGER(I4B) :: dampCoeffVarType = TypeField%constant
  !! varType denotes how the field is changing inside the element
  !! constant means the field is constant inside the element
  !! space means the field changes in space element
  !! spaceTime means the field changes in space-time element
  !! time means the field changes in time element

  INTEGER(I4B) :: dampCoeffDefineOn = TypeFEVariableOpt%quadrature
  !! mass density is defined on quadrature points

  INTEGER(I4B) :: dampCoeffMaxNNS = 0
  !! maximum number of nodes in space for mass density

  INTEGER(I4B) :: dampCoeffMaxNNT = 0
  !! maximum number of nodes in time for mass density

  CLASS(AbstractScalarMeshField_), POINTER :: scalarCoeff => NULL()
  !! it can be phase velocity or coefficient of permiabillity for
  !! isotropic medium this will be a scalar mesh field

  INTEGER(I4B) :: scalarCoeffFieldType = TypeField%normal
  !! field type for mass density
  !! normal means the field will change from one element to another

  INTEGER(I4B) :: scalarCoeffVarType = TypeField%constant
  !! varType denotes how the field is changing inside the element
  !! constant means the field is constant inside the element
  !! space means the field changes in space element
  !! spaceTime means the field changes in space-time element
  !! time means the field changes in time element

  INTEGER(I4B) :: scalarCoeffDefineOn = TypeFEVariableOpt%quadrature
  !! mass density is defined on quadrature points

  INTEGER(I4B) :: scalarCoeffMaxNNS = 0
  !! maximum number of nodes in space for mass density

  INTEGER(I4B) :: scalarCoeffMaxNNT = 0
  !! maximum number of nodes in time for mass density

  CLASS(AbstractScalarMeshField_), POINTER :: phase_velocity => NULL()
  !! phase_velocity ! This will be a scalar mesh field

  CLASS(AbstractVectorMeshField_), POINTER :: stress => NULL()
  !! Stress tensor ! This will be a tensor mesh field

  CLASS(AbstractVectorMeshField_), POINTER :: strain => NULL()
  !! Strain tensor ! This will be a tensor mesh field

  CLASS(AbstractVectorMeshField_), POINTER :: solidMechData => NULL()
  !! Constitutive data for solid materials

  CLASS(AbstractTensorMeshField_), POINTER :: Cijkl => NULL()
  !! Elasticity tensor
  !! NOTE: It is used for non Isotropic elasticity
  !! This will be a tensor mesh field

  INTEGER(I4B) :: cijklFieldType = TypeField%normal
  !! field type for mass density
  !! normal means the field will change from one element to another

  INTEGER(I4B) :: cijklVarType = TypeField%constant
  !! varType denotes how the field is changing inside the element
  !! constant means the field is constant inside the element
  !! space means the field changes in space element
  !! spaceTime means the field changes in space-time element
  !! time means the field changes in time element

  INTEGER(I4B) :: cijklDefineOn = TypeFEVariableOpt%quadrature
  !! mass density is defined on quadrature points

  INTEGER(I4B) :: cijklMaxNNS = 0
  !! maximum number of nodes in space for mass density

  INTEGER(I4B) :: cijklMaxNNT = 0
  !! maximum number of nodes in time for mass density

  INTEGER(I4B) :: cijklDim1 = 0
  !! dimension of elasticity tensor

  INTEGER(I4B) :: cijklDim2 = 0
  !! dimension of elasticity tensor

END TYPE KernelMeshFields_

!----------------------------------------------------------------------------
!                                                             KernelFields_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-23
! summary:  Kernel fields

TYPE :: KernelFields_
  INTEGER(I4B) :: tMatrixFields = 0
  !! total matrix fields

  INTEGER(I4B) :: tVectorFields = 0
  !! total vector fields

  INTEGER(I4B) :: tScalarFields = 0
  !! total scalar fields

  INTEGER(I4B) :: tSTMatrixFields = 0
  !! total space-time matrix fields

  INTEGER(I4B) :: tSTVectorFields = 0
  !! total space-time vector fields

  INTEGER(I4B) :: tSTScalarFields = 0
  !! total space-time scalar fields

  TYPE(MatrixFieldPointer_), ALLOCATABLE :: matrixFields(:)
  !! list of matrix fields

  TYPE(VectorFieldPointer_), ALLOCATABLE :: vectorFields(:)
  !! list of vector fields

  TYPE(ScalarFieldPointer_), ALLOCATABLE :: scalarFields(:)
  !! list of scalar fields

  TYPE(STVectorFieldPointer_), ALLOCATABLE :: stVectorFields(:)
  !! list of space-time vector fields

  TYPE(STScalarFieldPointer_), ALLOCATABLE :: stScalarFields(:)
  !! list of space-time scalar fields

  CLASS(MatrixField_), POINTER :: stiffnessMat => NULL()
  !! Global Stiffness matrix

  CLASS(MatrixField_), POINTER :: diffusionMat => NULL()
  !! Global diffusion matrix

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

  CLASS(ScalarField_), POINTER :: pressure => NULL()
  !! scalar field for nodal pressure

  CLASS(ScalarField_), POINTER :: p_velocity => NULL()
  !! scalar field for nodal pressure

  CLASS(ScalarField_), POINTER :: p_acceleration => NULL()
  !! scalar field for nodal pressure
END TYPE KernelFields_

!----------------------------------------------------------------------------
!                                                             KernelElemSD_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-23
! summary:  Element shape data in kernel

TYPE KernelElemshapeData_
  TYPE(ElemshapeData_) :: geoTimeElemSD
  !! Element shape data on linear time element #STFEM

  TYPE(ElemshapeData_) :: timeElemSD
  !! Element shape data on time element #STFEM

  TYPE(ElemshapeData_) :: geoSpaceElemSD
  !! Element shape data on linear space (simplex) element
  !! cell data only

  TYPE(ElemshapeData_) :: spaceElemSD
  !! Element shape data on space element
  !! cell data only

  TYPE(ElemshapeData_) :: geoSpaceElemSD_facet
  !! Element shape data on linear space (simplex) element
  !! facet element

  TYPE(ElemshapeData_) :: spaceElemSD_facet
  !! Element shape data on space element facet element

  TYPE(STElemshapeData_), ALLOCATABLE :: stelemsd(:)
  !! Element shape data on space-time element
END TYPE KernelElemshapeData_

!----------------------------------------------------------------------------
!                                                                KernelBC_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-23
! summary:  Kernel boundary conditions

TYPE KernelBC_
  INTEGER(I4B) :: tdbc = 0
  !! total Dirichlet boundary conditions

  INTEGER(I4B) :: tnbc = 0
  !! total Neumann boundary conditions

  INTEGER(I4B) :: tnbcPointSource = 0
  !! total Neumann boundary conditions

  INTEGER(I4B) :: twdbc = 0
  !! total weak Dirichlet boundary conditions

  INTEGER(I4B) :: tnitscheLocalID = 0
  !! total nitsche local id

  LOGICAL(LGT) :: isNitsche = .FALSE.
  !! If true, then it means weak dirichlet boundary condition is used.
  !! This variable is set in Initiate method
  !! This variable is set to true if the tWeakDirichletBC
  !! is greater than zero, otherwise it is set to false

  LOGICAL(LGT) :: isSymNitsche = .TRUE.
  !! Sym or unsymmetric Nitsche formulation

  REAL(DFP) :: nitscheAlpha = 100.0
  !! coefficient for nitsche formulation

  REAL(DFP) :: nitscheType = TypeKernelNitscheOpt%sym
  !! -1.0 for symmetric formulation
  !! 1.0 for skew symmetric formulation

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

END TYPE KernelBC_

!----------------------------------------------------------------------------
!                                                               KernelOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-22
! summary:  Kernel option

TYPE KernelOpt_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! This variable is Set to true when we initiate the kernel

  LOGICAL(LGT) :: isCommonDomain = .TRUE.
  !! This variable is True when the domain is common.
  !! It is useful in the case of multi-physics simulations.
  !! In multi-physics applications different fields can have different
  !! type and order of shape functions. To construct such shape functions
  !! we may have to use different finite element meshes (hence, domains).
  !! For example, in the fluid mechanics, we often use Taylor-Hood element
  !! Which employs different order of interpolation for pressure and velocity.

  LOGICAL(LGT) :: showTime = .FALSE.
  !! If it is set to true then we create a file called
  !! KernelName_time_stat.csv
  !! This file contains the statics of time taken by the kernel
  !! It will be helpful in improving the kernel

  LOGICAL(LGT) :: unifyVTK = .FALSE.
  !! if it is true all data are exported into one vtu file
  !! in WriteData_vtk method

  LOGICAL(LGT) :: createPVD = .FALSE.
  !! if true paraview data file is created
  !! in WriteData_vtk method

  INTEGER(I4B) :: problemType = TypeKernelProblemOpt%scalar
  !! Kernel problem type
  !! TypeKernelProblemOpt%scalar
  !! TypeKernelProblemOpt%Vector
  !! TypeKernelProblemOpt%MultiPhysics

  INTEGER(I4B) :: algorithm = 1
  !! algorithm

  INTEGER(I4B) :: vtkOutputFreq = 0
  !! frequency of output with WriteData_vtk

  INTEGER(I4B) :: hdfOutputFreq = 0
  !! frequency of output with WriteData_vtk

  INTEGER(I4B) :: coordinateSystem = TypeKernelCoordOpt%default
  !! Spatial coordinate system type. It can take following values
  !! `KERNEL_CARTESIAN` for Cartesian coordinates
  !! `KERNEL_CYLINDRICAL` for Cylinderical coordinates
  !! `KERNEL_SPHERICAL` for Sperical coordinates
  !! NOTE: These parameters are defined in the AbstractKernelParam module.

  INTEGER(I4B) :: maxIter = 100
  !! Maximum  number of iteration iterations
  !! This is useful when when we use iterative solvers like
  !! Newton method, Modified Newton method, or Iterative-predictor solvers.
  !! NOTE: DEFAULT_maxIter is defined in AbstractKernelParam

  INTEGER(I4B) :: timeDependency = TypeKernelTimeOpt%default
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

  INTEGER(I4B) :: postProcessOpt = 1
  !! Postprocessing options
  !! INFO: The actual action depends upon the specific kernels

  INTEGER(I4B) :: currentTimeStep = 1
  !! Current time step number of the simulation.
  !! NOTE: This varible is needed in the transient simulation only.

  INTEGER(I4B) :: totalTimeStep = 0
  !! Total number of time step number in the simulation.
  !! NOTE: This varible is needed in the transient simulation only.

  ! Reals

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

  REAL(DFP) :: lengthScale = 1.0_DFP
  !! This variable denotes the length scale of the problem.
  !! NOTE: This variable is for internal use only.

  REAL(DFP) :: gravity(3) = 0.0_DFP
  !! Acceleration vector due to gravity

  REAL(DFP) :: incrementScale = 1.0_DFP
  !! x = x + incrementScale * displacement

  TYPE(KernelErrorOpt_) :: dispError
  !! Displacement error

  TYPE(KernelErrorOpt_) :: velError
  !! Velocity error

  TYPE(KernelErrorOpt_) :: solError
  !! solution error

  TYPE(KernelErrorOpt_) :: resError
  !! Residual error

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

  TYPE(String) :: tanmatName
  !! MATRIX
  !! BLOCKMATRIX

  TYPE(String) :: outputPath
  !! Path to put output files
  !! Default is results

  TYPE(String) :: domainFile
  !! domain file name

  TYPE(KernelBasisOpt_) :: basisForSpace
  !! Basis function option for space

  TYPE(KernelBasisOpt_) :: basisForTime
  !! Basis function option for time

  TYPE(KernelBasisOpt_) :: basisForPressure
  !! Basis function option for pressure

  TYPE(KernelBasisOpt_) :: basisForVelocity
  !! Basis function option for velocity

  TYPE(IterationData_) :: iterData
  !! Iteration data
  !! INFO: The actual action depends upon the specific kernels

END TYPE KernelOpt_

TYPE(KernelOpt_), PARAMETER :: TypeKernelOpt = KernelOpt_()

END MODULE KernelComponents
