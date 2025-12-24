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
USE GlobalData, ONLY: DFP, I4B, LGT

USE FEDomain_Class, ONLY: FEDomain_, FEDomainPointer_

USE FPL, ONLY: ParameterList_

USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_

USE AbstractFE_Class, ONLY: AbstractFE_

USE PVDFile_Class, ONLY: PVDFile_

USE AbstractLinSolver_Class, ONLY: AbstractLinSolver_

USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_

USE TxtFile_Class, ONLY: TxtFile_

USE UserFunction_Class, ONLY: UserFunction_

USE String_Class, ONLY: String

USE MeshSelection_Class, ONLY: MeshSelection_

USE SolidMaterial_Class, ONLY: SolidMaterial_

USE DirichletBC_Class, ONLY: DirichletBC_

USE NitscheBC_Class, ONLY: NitscheBC_

USE NeumannBC_Class, ONLY: NeumannBC_

USE AbstractNodeField_Class, ONLY: AbstractNodeField_

USE Tomlf, ONLY: toml_table

USE HDF5File_Class, ONLY: HDF5File_

USE Domain_Class, ONLY: Domain_

USE ExceptionHandler_Class, ONLY: e

USE KernelComponents

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractKernel_Class"

PUBLIC :: AbstractKernel_
PUBLIC :: AbstractKernelPointer_
PUBLIC :: SetAbstractKernelParam
PUBLIC :: AbstractKernelCheckEssentialParam
PUBLIC :: AbstractKernelInitiate
PUBLIC :: AbstractKernelDeallocate
PUBLIC :: AbstractKernelPreCheckError
PUBLIC :: AbstractKernelPostCheckError
PUBLIC :: AbstractKernelCheckError

PUBLIC :: AbstractKernelDisplay
PUBLIC :: AbstractKernelExport
PUBLIC :: AbstractKernelImport
PUBLIC :: AbstractKernelImportParamFromToml
PUBLIC :: AbstractKernelImportFromToml
PUBLIC :: AbstractKernelInitiateTangentMatrix
PUBLIC :: AbstractKernelInitiateFields
PUBLIC :: AbstractKernelApplyDirichletBC
PUBLIC :: AbstractKernelApplyIC

!----------------------------------------------------------------------------
!                                                           AbstractKernel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D., Shion Shimizu
! date: 27 April 2022
! summary: Abstract class for kernel

TYPE, ABSTRACT :: AbstractKernel_

  TYPE(FEDomain_), POINTER :: dom => NULL()
  !! Domain of the problem

  TYPE(FEDomainPointer_), ALLOCATABLE :: domains(:)
  !! Domains of the problem

  TYPE(KernelOpt_) :: opt
  !! options of the kernel
  !! It is defined in the KernelComponents module

  TYPE(KernelFields_) :: fields
  !! fields of kernel

  TYPE(KernelMeshFields_) :: meshFields
  !! mesh fields of kernel

  TYPE(KernelMaterials_) :: materials
  !! materials of kernel

  TYPE(KernelElemshapeData_) :: elemsd
  !! element shape data used in kernels

  TYPE(KernelBC_) :: bc
  !! boundary conditions of the kernel

  CLASS(FEDOF_), POINTER :: fedof => NULL()
  !! Degree of freedom of the problem

  TYPE(FEDOFPointer_), ALLOCATABLE :: fedofs(:)
  !! Degree of freedom of the problem

  CLASS(FEDOF_), POINTER :: geofedof => NULL()
  !! Degree of freedom of the geometry of the problem

  CLASS(AbstractFE_), POINTER :: timeFE => NULL()
  !! Time finite element

  CLASS(AbstractFE_), POINTER :: geoTimeFE => NULL()
  !! Linear time finite element

  TYPE(PVDFile_) :: pvdFile
  !! instance of pvd file class

  CLASS(AbstractLinSolver_), POINTER :: linsol => NULL()
  !! A pointer to a Linear iterative solver
  !! NOTE: The actual linear solver depends upon the
  !! engine and type of problem. linsol is initiated in
  !! KernelInitiateFromParam routine.

  CLASS(AbstractMatrixField_), POINTER :: tanmat => NULL()
  !! Global tangent matrix
  !! NOTE: The actual form of tangent matrix depends upon the engine
  !! and type of problem.

  TYPE(TxtFile_) :: showTimeFile
  !! File which keeps the time statics of the kernel (performance related)
  !! This file is created when showTime is set to true.
  !! The name of this file will be
  !! outputPath / name + _time_stat.csv

  CLASS(UserFunction_), POINTER :: bodySourceFunc => NULL()
  !! body force function

  REAL(DFP), ALLOCATABLE :: timeVec(:)
  !! time vector

  INTEGER(I4B), ALLOCATABLE :: dbcIndx(:)
  !! Indices where Dirichlet boundary conditions is prescribed
  !! INFO: This variable is for internal use only.
  !! It is formed from the Dirichlet boundary conditions.

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! Check the essential parameter of the kernel
  !! NOTE: When you want to add or remove a param from
  !! essential param, then just modify the
  !! astr variable in obj_CheckEssentialParam method.

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate the kernel. This is a constructor method.
  !! WARN: This routine is an interface only. It means it should be
  !! implemented by the subclass.

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the memory occupied by the kernel

  PROCEDURE, PUBLIC, PASS(obj) :: PreCheckError => obj_PreCheckError
  !! Check error before setting kernel

  PROCEDURE, PUBLIC, PASS(obj) :: PostCheckError => obj_PostCheckError
  !! Check error after setting kernel

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Returns the prefix of the kernel
  !! This method should be implemented by the specific kernel

  ! CONSTRUCTOR:
  ! @InitiateFieldsMethods

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFields => obj_InitiateFields
  !! Initiate the fields

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateTangentMatrix => &
    obj_InitiateTangentMatrix
  !! Initiate tangent matrix

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateScalarFields => &
    obj_InitiateScalarFields
  !! Initiate scalar fields

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateSTScalarFields => &
    obj_InitiateSTScalarFields
  !! Initiate scalar fields

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateVectorFields => &
    obj_InitiateVectorFields
  !! Initiate vector fields

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateSTVectorFields => &
    obj_InitiateSTVectorFields
  !! Initiate vector fields

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateMatrixFields => &
    obj_InitiateMatrixFields
  !! Initiate vector fields

  ! SET:
  ! @MaterialMethods

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateMaterialProperties => &
    obj_InitiateMaterialProperties
  !! Initiate material properties

  PROCEDURE, PUBLIC, PASS(obj) :: AddSolidMaterial => obj_AddSolidMaterial
  !! Add a solid material to the kernel

  PROCEDURE, PUBLIC, PASS(obj) :: GetSolidMaterialPointer => &
    obj_GetSolidMaterialPointer
  !! Get a solid material to the kernel

  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterialProperties => &
    obj_SetMaterialProperties
  !! Set material properties

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateMassDensity => &
    obj_InitiateMassDensity
  !! Initiate mass density field if massDensity is defined in the materials

  PROCEDURE, PUBLIC, PASS(obj) :: SetMassDensity => &
    obj_SetMassDensity
  !! Set mass density if mass density is defined in the materials

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElasticityProperties => &
    obj_InitiateElasticityProperties
  !! Initiate lame parameters for isotropic elasticity

  PROCEDURE, PUBLIC, PASS(obj) :: SetElasticityProperties => &
    obj_SetElasticityProperties
  !! Set Lame parameters for isotropic elasticity

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateDampingProperties => &
    obj_InitiateDampingProperties
  !! Initiate rayleight damping properties

  PROCEDURE, PUBLIC, PASS(obj) :: SetDampingProperties => &
    obj_SetDampingProperties
  !! Set Lame parameters for isotropic elasticity

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateScalarCoefficient => &
    obj_InitiateScalarCoefficient

  PROCEDURE, PUBLIC, PASS(obj) :: SetScalarCoefficient => &
    obj_SetScalarCoefficient

  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterialToDomain => &
    obj_SetMaterialToDomain
  !! Set material to mesh

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: SetShowTime => obj_SetShowTime
  !! Set show time

  PROCEDURE, PUBLIC, PASS(obj) :: PreSet => obj_PreSet
  !! Perform final check, before starting the actual computations

  PROCEDURE, PUBLIC, PASS(obj) :: PostSet => obj_PostSet
  !! Perform final check, before starting the actual computations

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! Perform final check, before starting the actual computations

  PROCEDURE, PUBLIC, PASS(obj) :: SetCurrentTimeStep => &
    obj_SetCurrentTimeStep
  !! Sets the current time step of the kernel

  PROCEDURE, PUBLIC, PASS(obj) :: SetIterationNumber => &
    obj_SetIterationNumber
  !! Sets the current time step of the kernel

  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshData => obj_SetMeshData
  !! This method is called from Set method.
  !! It Sets the mesh data.

  PROCEDURE, PUBLIC, PASS(obj) :: SetFiniteElements => obj_SetFiniteElements
  !! Set finite elements

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadPointsInSpace => &
    obj_SetQuadPointsInSpace

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadPointsInTime => &
    obj_SetQuadPointsInTime

  PROCEDURE, PUBLIC, PASS(obj) :: SetLocalElemShapeDataInSpace => &
    obj_SetLocalElemShapeDataInSpace
  !! Set local element shape data in space

  PROCEDURE, PUBLIC, PASS(obj) :: SetLocalElemShapeDataInTime => &
    obj_SetLocalElemShapeDataInTime
  !! Set local element shape data in time

  PROCEDURE, PUBLIC, PASS(obj) :: SetGlobalElemShapeDataInSpace => &
    obj_SetGlobalElemShapeDataInSpace
  !! Set global element shape data in space
  ! TODO: Implement SetGlobalElemShapeDataInSpace

  PROCEDURE, PUBLIC, PASS(obj) :: SetGlobalElemShapeDataInTime => &
    obj_SetGlobalElemShapeDataInTime
  !! Set global element shape data in time
  ! TODO: Implement SetGlobalElemShapeDataInTime

  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetFiniteElements => &
    obj_SetFacetFiniteElements
  !! Set Facet Finite Elements
  !! TODO: Implement SetFacetFiniteElements method

  PROCEDURE, PUBLIC, PASS(obj) :: SetMatIFaceConnectData => &
    obj_SetMatIFaceConnectData
  !! Set material to facet connection data

  PROCEDURE, PUBLIC, PASS(obj) :: SetAlgorithm => obj_SetAlgoParam
  !! Set algorithm param
  !! This routine should be implemented by subclasses

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
    obj_GetDirichletBCPointer
  !! Get pointer to the pressure dirichlet boundary condition

  PROCEDURE, PUBLIC, PASS(obj) :: GetNitscheBCPointer => &
    obj_GetNitscheBCPointer
  !! Get pointer to the pressure dirichlet boundary condition

  PROCEDURE, PUBLIC, PASS(obj) :: GetNeumannBCPointer => &
    obj_GetNeumannBCPointer
  !! Get pointer to the neumann boundary condition for velocity

  PROCEDURE, PUBLIC, PASS(obj) :: GetPointSourcePointer => &
    obj_GetPointSourcePointer
  !! Get point source in the nbcPointSource

  PROCEDURE, PUBLIC, PASS(obj) :: SetNitscheMeshData => &
    obj_SetNitscheMeshData
  !! This routine set mesh data necessary for implementing the
  !! Nitsche boundary condition.

  ! SET:
  ! @ApplyDirichletBCMethods

  ! PROCEDURE, PUBLIC, PASS(obj) :: ApplyDirichletBC => &
  !   obj_ApplyDirichletBC
  ! !! Apply Dirichlet boundary condition

  ! SET:
  ! @ApplyICMethods

  ! PROCEDURE, PASS(obj) :: ApplyIC1 => obj_ApplyIC
  ! PROCEDURE, PASS(obj) :: ApplyIC2 => obj_ApplyICFromToml
  ! GENERIC, PUBLIC :: ApplyIC => ApplyIC1, ApplyIC2
  ! !! Apply Dirichlet boundary condition

  ! IO:
  ! @IOMethods

  ! PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  ! !! Import data from the input file
  ! PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  ! !! Export data to an external file
  ! PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  ! !! Displays the content of kernel
  !
  ! PROCEDURE, PASS(obj) :: WriteData_hdf5 => obj_WriteData_hdf5
  ! !! Write simulation results in hdf5 file format
  ! PROCEDURE, PASS(obj) :: WriteData_vtk => obj_WriteData_vtk
  ! !! Write simulation data in vtk file format
  ! ! PROCEDURE, PASS( obj ) :: WriteData_xdmf => obj_WriteData_xdmf
  ! GENERIC, PUBLIC :: WriteData => WriteData_hdf5, WriteData_vtk
  ! !! Export data to an external file
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  ! PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  ! GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: ImportParamFromToml => &
  !   obj_ImportParamFromToml
  ! PROCEDURE, PUBLIC, PASS(obj) :: ExportToToml => obj_ExportToToml

  ! GET:
  ! @RunMethods

  ! PROCEDURE, PASS(obj) :: Run1 => obj_Run1
  ! PROCEDURE, PASS(obj) :: Run2 => obj_Run2
  ! GENERIC, PUBLIC :: Run => Run1, Run2
  ! !! Run the kernel

  ! SET:
  ! @AssembleMethods

  ! PROCEDURE, PUBLIC, PASS(obj) :: Assemble => obj_Assemble
  ! !! This procedure pointer assembles the problem

  ! SET:
  ! @AssembleTanmatMethods

  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleTanmat => obj_AssembleTanmat
  ! !! This procedure pointer assembles the global tangent matrix
  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleMassMat => obj_AssembleMassMat
  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleStiffnessMat => &
  !   obj_AssembleStiffnessMat
  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleDiffusionMat => &
  !   obj_AssembleDiffusionMat
  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleDampingMat => &
  !   obj_AssembleDampingMat
  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleNitscheMat => &
  !   obj_AssembleNitscheMat

  ! SET:
  ! @AssembleRHSMethods

  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHS => obj_AssembleRHS
  ! !! This procedure pointer assembles the right-hand-side vector
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetBodySourceFunc => obj_SetBodySourceFunc
  ! !! Set body force function
  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleBodySource => &
  !   obj_AssembleBodySource
  ! !! This procedure assemble the body force term to RHS
  ! PROCEDURE, PUBLIC, PASS(obj) :: AssembleSurfaceSource => &
  !   obj_AssembleSurfaceSource
  ! !! This procedure assemble the surface force term to RHS
  ! PROCEDURE, PUBLIC, PASS(obj) :: AssemblePointSource => &
  !   obj_AssemblePointSource
  ! !! This procedure assemble the point source to rhs

  ! GET:
  ! @SolveMethods

  ! PROCEDURE, PUBLIC, PASS(obj) :: Solve => obj_Solve
  ! !! This procedure pointer solves the problem

  ! SET:
  ! @UpdateMethods

  ! PROCEDURE, PUBLIC, PASS(obj) :: Update => obj_Update
  ! !! This procedure pointer update the problem
  ! PROCEDURE, PUBLIC, PASS(obj) :: UpdateIteration => obj_UpdateIteration
  ! !! This procedure pointer update the problem

  ! GET:
  ! @ConvergenceMethods

  ! PROCEDURE, PUBLIC, PASS(obj) :: IsConverged => obj_IsConverged
  ! !! This procedure pointer checks the convergence

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
!> author: Shion Shimizu
! update: 2024-02-10
! summary: Set kernel parameters

INTERFACE
  MODULE SUBROUTINE SetAbstractKernelParam( &
    param, prefix, problemType, name, engine, coordinateSystem, &
    domainFile, isCommonDomain, gravity, timeDependency, &
    maxIter, nsd, nnt, tdof, dt, startTime, endTime, &
    currentTime, currentTimeStep, totalTimeStep, &
    baseInterpolationForSpace, baseContinuityForSpace, &
    quadratureTypeForSpace, ipTypeForSpace, &
    basisTypeForSpace, alphaForSpace, &
    betaForSpace, lambdaForSpace, baseInterpolationForTime, &
    baseContinuityForTime, quadratureTypeForTime, ipTypeForTime, &
    basisTypeForTime, alphaForTime, betaForTime, lambdaForTime, &
    postProcessOpt, tDirichletBC, tNeumannBC, tWeakDirichletBC, &
    isSymNitsche, nitscheAlpha, &
    materialInterfaces, isConstantMatProp, tSolidMaterials, &
    algorithm, vtkOutputFreq, isIsotropic, isIncompressible, &
    rtoleranceForDisplacement, atoleranceForDisplacement, &
    rtoleranceForVelocity, atoleranceForVelocity, &
    rtoleranceForResidual, atoleranceForResidual, tanmatProp, &
    tanmatName, tOverlappedMaterials, outputPath, tPointSource, showTime, &
    unifyVTK, createPVD)

    CHARACTER(*), INTENT(IN) :: prefix
    !! prefix
    INTEGER(I4B), INTENT(IN) :: problemType
    !! Kernel problem type. Problem can be scalar, vector, or multi-physics
    !! TypeKernelProblemOpt%Scalar
    !! TypeKernelProblemOpt%Vector
    !! TypeKernelProblemOpt%MultiPhysics
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: vtkOutputFreq
    !! frequency of output with WriteData_vtk
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
    CHARACTER(*), OPTIONAL, INTENT(IN) :: tanmatName
    !! Tangent matrix name
    !! MATRIX, BLOCKMATRIX
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tOverlappedMaterials
    !! Total number of overlapped materials
    CHARACTER(*), OPTIONAL, INTENT(IN) :: outputPath
    !! path where output of kernel will be written
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPointSource
    !! total number of point sources, size of nbcPointSource
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: showTime
    !! Show time of each steps
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: unifyVTK
    !! unified write data to vtk file
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: createPVD
    !! create the paraview data file
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
    CLASS(FEDomain_), OPTIONAL, TARGET, INTENT(INOUT) :: dom
    !! Domain of computation
    TYPE(FEDomainPointer_), OPTIONAL, INTENT(INOUT) :: domains(:)
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
! summary: This routine initiates the tangent matrix of the kernel

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
! summary: This routine initiates the scalar fieldds of the kernel

INTERFACE
  MODULE SUBROUTINE obj_InitiateScalarFields(obj, names)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: names(:)
  END SUBROUTINE obj_InitiateScalarFields
END INTERFACE

!----------------------------------------------------------------------------
!                              InitiateSTScalarFields@InitiateFieldsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the matrix and vector fields

INTERFACE
  MODULE SUBROUTINE obj_InitiateSTScalarFields(obj, names)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: names(:)
  END SUBROUTINE obj_InitiateSTScalarFields
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
!                              InitiateSTVectorFields@InitiateFieldsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the matrix and vector fields

INTERFACE
  MODULE SUBROUTINE obj_InitiateSTVectorFields(obj, names)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: names(:)
  END SUBROUTINE obj_InitiateSTVectorFields
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateMatrixFields@InitiateFieldsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Oct 2022
! summary: This routine initiates the matrix and vector fields

INTERFACE
  MODULE SUBROUTINE obj_InitiateMatrixFields(obj, names, matrixProp, &
                                             spaceCompo, timeCompo)
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
!                                                     SetShowTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-25
! summary:  Set show time

INTERFACE
  MODULE SUBROUTINE obj_SetShowTime(obj, showTime)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: showTime
  END SUBROUTINE obj_SetShowTime
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
!                                            SetQuadPointsInTime@SetMethods
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
!                                                 SetAlgoParam@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetAlgoParam(obj, algoParam)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    CLASS(KernelAlgoParam_), INTENT(IN) :: algoParam
  END SUBROUTINE obj_SetAlgoParam
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
                                         param, region)
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
!                          obj_InitiateScalarCoefficient@MaterialMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-01-06
! summary: Initiate scalar coefficient for diffusion matrix

INTERFACE
  MODULE SUBROUTINE obj_InitiateScalarCoefficient(obj, varname)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: varname
  END SUBROUTINE obj_InitiateScalarCoefficient
END INTERFACE

!----------------------------------------------------------------------------
!                          obj_SetScalarCoefficient@MaterialMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-01-06
! summary: Set scalar coefficient for diffusion matrix

INTERFACE
  MODULE SUBROUTINE obj_SetScalarCoefficient(obj, varname)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: varname
  END SUBROUTINE obj_SetScalarCoefficient
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetMaterialToMesh@MaterialMethods
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
  MODULE SUBROUTINE obj_ApplyIC(obj, name, func, extField, times, ivar, &
                                idof, spaceCompo, timeCompo)
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
!                                                    ApplyIC@ApplyICMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-21
! summary:  Apply initial conditions to the fields from toml

INTERFACE AbstractKernelApplyIC
  MODULE SUBROUTINE obj_ApplyICFromToml(obj, table, tomlName)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), OPTIONAL, INTENT(IN) :: tomlName
  END SUBROUTINE obj_ApplyICFromToml
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
!                                 AssembleDiffusionMat@AssembleTanmatMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-01-04
! summary: This subroutine assembles the diffusion matrix of the system

INTERFACE
  MODULE SUBROUTINE obj_AssembleDiffusionMat(obj)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleDiffusionMat
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
!                                   AssembleSurfaceSource@AssembleRHSMethods
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
  MODULE SUBROUTINE obj_Run1(obj, param)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Run1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Run@RunMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 May 2022
! summary: Run the simulation

INTERFACE
  MODULE SUBROUTINE obj_Run2(obj, table)
    CLASS(AbstractKernel_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_Run2
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
