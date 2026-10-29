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

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-28
! summary: Vector field data type is defined

MODULE VectorField_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: math => TypeMathOpt
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE DirichletBC_Class, ONLY: DirichletBC_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDOF_Class, ONLY: FEDOF_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP, I4B, LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE String_Class, ONLY: String
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeOpt_Class, ONLY: TimeOpt_
USE tomlf, ONLY: toml_table
USE UserFunction_Class, ONLY: UserFunction_
USE VTKFile_Class, ONLY: VTKFile_

IMPLICIT NONE

PRIVATE

INTEGER(I4B), PARAMETER :: myconversion = TypeFieldOpt%conversionNodesToDOF

PUBLIC :: VectorField_
PUBLIC :: VectorFieldPointer_
PUBLIC :: VectorFieldInitiate
PUBLIC :: VectorFieldDeallocate
PUBLIC :: VectorFieldDisplay
PUBLIC :: VectorFieldExport
PUBLIC :: VectorFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                              VectorField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Vector field
!
! A vector field has one physical variable with spaceCompo space
! components and one time component. There is one value for each DOF
! of fedof and each component, stored in the vector of
! AbstractNodeField_ in DOF format: component 1 of all DOFs, then
! component 2, and so on. So component c of DOF i is entry
! (c - 1) * tNodes + i, where tNodes = fedof%GetTotalDOF().
!
! The values are set by Set (one node, one component, all nodes,
! selected nodes, rank 2 arrays in nodes or DOF format, FEVariable),
! SetByFunction, SetLagrangeDOF and the boundary condition methods,
! and read by Get.
!
! Components:
!   spaceCompo - number of space components
!   idofs - [1, ..., spaceCompo], the DOFs of the components

TYPE, EXTENDS(AbstractNodeField_) :: VectorField_
  INTEGER(I4B) :: spaceCompo = math%zero_i
  !! number of space components
  INTEGER(I4B), ALLOCATABLE :: idofs(:)
  !! DOFs of the components, [1, ..., spaceCompo]
CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by copying
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by arguments
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data
  FINAL :: obj_Final

  ! @SetMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set single entry
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set the vector of all nodes
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set one component of all nodes to a value
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set all values from a rank 2 array
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set one component of all nodes from a vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set one component from a node field (under development)
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set the same vector at the selected nodes
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set8 => obj_Set8
  !! Set the selected nodes from a rank 2 array
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set9 => obj_Set9
  !! Set one component of the selected nodes
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set10 => obj_Set10
  !! Set one component of one node
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set11 => obj_Set11
  !! Set values by using a FEVariable
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set12 => obj_Set12
  !! Set all values to one value
  PROCEDURE, PRIVATE, PASS(obj) :: Set13 => obj_Set13
  !! obj = value, copy another vector field
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetByFunction => &
    obj_SetByFunction
  !! Set values from the user function
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, &
    Set7, Set8, Set9, Set10, Set11, Set12, Set13
  !! Set the values of the vector field
  PROCEDURE, PUBLIC, PASS(obj) :: SetHigherOrderNodeCoord => &
    obj_SetHigherOrderNodeCoord
  !! Set the values to the coordinates of the DOFs
  PROCEDURE, PUBLIC, PASS(obj) :: SetLagrangeDOF => &
    obj_SetLagrangeDOF
  !! Set the Lagrange DOFs from another vector field

  ! @GetMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get the vector of a node or a component of all nodes
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! returns all entries in rank2 array of real
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! returns selected values in rank2 aray of real
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! returns a component of the selected nodes
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get5 => obj_Get5
  !! Get a single value
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get6 => obj_Get6
  !! Get nodal values of a vector in FEVariable
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get7 => obj_Get7
  !! Copy the values into another vector field
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, &
    Get5, Get6, Get7
  !! Get the entries of Vector field
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get multiple values in FEVariable
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshField => obj_GetMeshField
  !! Get the mesh field corresponding to abstract field
  PROCEDURE, PRIVATE, PASS(obj) :: GetVtkUnstructuredValue1 => &
    obj_GetVtkUnstructuredValue1
  !! Get data array for Vtk UnstructuredGrid with allocation.
  PROCEDURE, PRIVATE, PASS(obj) :: GetVtkUnstructuredValue2 => &
    obj_GetVtkUnstructuredValue2
  !! Get data array for Vtk UnstructuredGrid without allocation.
  GENERIC, PUBLIC :: GetVtkUnstructuredValue => GetVtkUnstructuredValue1, &
    GetVtkUnstructuredValue2
  !! Values at the points of the VTK unstructured grid

  ! @DirichletBCMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyDirichletBC1 => obj_ApplyDirichletBC1
  !! Apply a Dirichlet boundary condition
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyDirichletBC2 => obj_ApplyDirichletBC2
  !! Apply a list of Dirichlet boundary conditions
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyDirichletBC3 => obj_ApplyDirichletBC3
  !! Apply the Dirichlet boundary conditions of the field
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    ApplyDirichletBC2, ApplyDirichletBC3
  !! Apply Dirichlet boundary conditions

  ! @PointNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: ApplyPointNeumannBC => &
    obj_ApplyPointNeumannBC
  !! Apply point neumann boundary condition

  ! @SurfaceNBCMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ApplySurfaceNeumannBC => &
    obj_ApplySurfaceNeumannBC
  !! Apply Surface neumann boundary condition

  ! @BodySourceMethods
  PROCEDURE, PRIVATE, PASS(obj) :: ApplyBodySource1 => &
    obj_ApplyBodySource1
  !! Add the body source given by a user function
  PROCEDURE, PRIVATE, PASS(obj) :: ApplyBodySource2 => &
    obj_ApplyBodySource2
  !! Add the body source given by a vector field
  GENERIC, PUBLIC :: ApplyBodySource => ApplyBodySource1, ApplyBodySource2
  !! Generic method for setting body source

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of VectorField
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data to HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
  !! Export data to VTK file
  PROCEDURE, PUBLIC, PASS(obj) :: ExportHigherOrderNodeCoordToVTK &
    => obj_ExportHigherOrderNodeCoordToVTK
  !! Write the field as the node coordinates of a high order mesh

  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetFromToml => obj_SetFromToml
  !! Set the values from a toml table
END TYPE VectorField_

!----------------------------------------------------------------------------
!                                                       VectorFieldPointer_
!----------------------------------------------------------------------------

TYPE :: VectorFieldPointer_
  CLASS(VectorField_), POINTER :: ptr => NULL()
END TYPE VectorFieldPointer_

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Initiate a vector field from another field
!
! Calls Initiate of AbstractNodeField_ (copy of the field data, the DOF
! data and the values). If obj2 is a vector field, spaceCompo and idofs
! are copied too.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field to be initiated
!   obj2 : CLASS(AbstractField_) - field to be copied, a child of
!     AbstractNodeField_
!   copyFull, copyStructure, usePointer : LOGICAL(LGT) - (optional) not
!     used
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyFull
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyStructure
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: usePointer
    !! not used
  END SUBROUTINE obj_Initiate1
END INTERFACE

INTERFACE VectorFieldInitiate
  MODULE PROCEDURE obj_Initiate1
END INTERFACE VectorFieldInitiate

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-19
! summary: Initiate a vector field by passing arguments
!
! The data stored in obj is deallocated first. A vector field has one
! physical variable, whose name is the first character of name, with
! spaceCompo(1) space components and one time component. There is one
! value for each DOF of fedof and each component, so obj%Size() is
! fedof%GetTotalDOF() * spaceCompo(1). The values are stored in DOF
! format: component 1 of all DOFs, then component 2, and so on. idofs
! is set to [1, ..., spaceCompo(1)].
!
! spaceCompo is required (checked in debug mode). The arguments
! storageFMT, isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo,
! isTimeCompoScalar, tPhysicalVarNames, physicalVarNames,
! isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes,
! isTNodesScalar and tSize are not used.
!
! Initiate(obj2) makes a copy of another vector field
! ([[obj_Initiate1]]); the example shows both.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   name : CHARACTER(*) - name of the field
!   engine : CHARACTER(*) - engine, e.g., "NATIVE_SERIAL"
!   fieldType : INTEGER(I4B) - (optional) TypeFieldOpt%normal (default)
!     or TypeFieldOpt%constant
!   comm, local_n, global_n : INTEGER(I4B) - (optional) parallel data
!   spaceCompo : INTEGER(I4B) - number of space components, spaceCompo(1)
!   fedof, geofedof : CLASS(FEDOF_) - FEDOFs of the field and geometry
!   timefedof : CLASS(TimeFEDOF_) - (optional) FEDOF in time
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u, v, w
! CHARACTER(1) :: names(1)
! REAL(DFP) :: raw(12)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL v%Initiate(name="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
!                 fedof=fedof, geofedof=geofedof)
!
! CALL OK(v%IsInitiated(), "IsInitiated")
! CALL OK(v%spaceCompo .EQ. 2, "spaceCompo")
! CALL OK(v%Size() .EQ. 2 * fedof%GetTotalDOF(), "Size = 2 * total DOF")
! CALL OK(v%Size() .EQ. 12, "Size = 12")
! CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "GetTotalPhysicalVars")
! CALL v%GetPhysicalNames(names)
! CALL OK(names(1) .EQ. "v", "GetPhysicalNames")
! CALL OK(ALL(v%GetSpaceCompo(1) .EQ. [2]), "GetSpaceCompo")
! CALL OK(ALL(v%GetTimeCompo(1) .EQ. [1]), "GetTimeCompo")
! CALL OK(v%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
!         "GetStorageFMT")
!
! ! copy
! CALL v%Set(VALUE=3.0_DFP)
! CALL w%Initiate(obj2=v)
! CALL OK(w%spaceCompo .EQ. 2, "Initiate(obj2): spaceCompo")
! CALL OK(w%Size() .EQ. 12, "Initiate(obj2): Size")
! CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(raw - 3.0_DFP) .LT. tol), "Initiate(obj2): values")
!
! CALL w%DEALLOCATE()
! CALL OK(.NOT. w%IsInitiated(), "DEALLOCATE: not initiated")
! CALL OK(w%spaceCompo .EQ. 0, "DEALLOCATE: spaceCompo = 0")
!
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate1]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, name, engine, fieldType, storageFMT, &
                                  comm, local_n, global_n, spaceCompo, &
                                  isSpaceCompo, isSpaceCompoScalar, &
                                  timeCompo, isTimeCompo, isTimeCompoScalar, &
                                  tPhysicalVarNames, physicalVarNames, &
                                  isPhysicalVarNames, &
                                  isPhysicalVarNamesScalar, tNodes, &
                                  isTNodes, isTNodesScalar, tSize, fedof, &
                                  geofedof, timefedof)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), INTENT(IN), OPTIONAL :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), INTENT(IN), OPTIONAL :: storageFMT
    !! storage format of the scalar field
    !! Not required.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: spaceCompo(:)
    !! space components
    !! Not required
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSpaceCompo
    !! if true we will try to access spaceCompo
    !! Not required
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    !! Not required
    INTEGER(I4B), INTENT(IN), OPTIONAL :: timeCompo(:)
    !! Time components
    !! Not required
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTimeCompo
    !! if true we will try to access TimeCompo
    !! Not required
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    !! Not required
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    !! Not required
    CHARACTER(*), INTENT(IN), OPTIONAL :: physicalVarNames(:)
    !! Names of the physical variables
    !! Not required
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    !! Not required
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isPhysicalVarNamesScalar
    !! if true then physicalVarNames is scalar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tNodes(:)
    !! total number of nodes in each physical variable
    !! Not required
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTNodes
    !! if true we will try to access tNodes
    !! Not required
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTNodesScalar
    !! is tNodes scalar
    !! Not required
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tSize
    !! total size of node field
    !! not required
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof, geofedof
    !! FEDOF object
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate2
END INTERFACE

INTERFACE VectorFieldInitiate
  MODULE PROCEDURE obj_Initiate2
END INTERFACE VectorFieldInitiate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Deallocate the data of a vector field
!
! spaceCompo is set to zero, idofs is deallocated and DEALLOCATE of
! AbstractNodeField_ is called.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE VectorFieldDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE VectorFieldDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Deallocate a vector of vector field pointers
!
! Each associated pointer is deallocated and nullified, and obj is
! deallocated.
!
! Arguments:
!   obj : TYPE(VectorFieldPointer_) - vector of vector field pointers
!
! See also: [[obj_VectorFieldSafeAllocate1]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(VectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of vector field pointers
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE

INTERFACE VectorFieldDeallocate
  MODULE PROCEDURE obj_Deallocate_ptr_vector
END INTERFACE VectorFieldDeallocate

!----------------------------------------------------------------------------
!                                  VectorFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-25
! summary: Safely allocate a vector of vector field pointers
!
! If obj is not allocated, it is allocated with size newsize. If its
! size is less than newsize, VectorFieldDeallocate is called and obj is
! allocated again with size newsize. Otherwise obj is not changed.
!
! Arguments:
!   obj : TYPE(VectorFieldPointer_) - vector of vector field pointers
!   newsize : INTEGER(I4B) - required size
!
! See also: [[obj_Deallocate_ptr_vector]]

INTERFACE
  MODULE SUBROUTINE obj_VectorFieldSafeAllocate1(obj, newsize)
    TYPE(VectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! allocatable Vector field pointer
    INTEGER(I4B), INTENT(IN) :: newsize
    !! new size of obj
  END SUBROUTINE obj_VectorFieldSafeAllocate1
END INTERFACE

INTERFACE VectorFieldSafeAllocate
  MODULE PROCEDURE obj_VectorFieldSafeAllocate1
END INTERFACE VectorFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Finalizer of VectorField_
!
! Calls DEALLOCATE.

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(VectorField_), INTENT(INOUT) :: obj
    !! vector field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-26
! summary: Display the content of a vector field
!
! Calls Display of AbstractNodeField_ and displays spaceCompo.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   msg : CHARACTER(*) - message printed first
!   unitno : INTEGER(I4B) - (optional) unit number, default stdout
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL u%Set(VALUE=1.0_DFP)
! CALL u%Display("u: ")
! CALL OK(.TRUE., "Display")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    CHARACTER(*), INTENT(IN) :: msg
    !! message
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE VectorFieldDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE VectorFieldDisplay

!----------------------------------------------------------------------------
!                                                           Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Import a vector field from an HDF5 file
!
! Calls Import of AbstractNodeField_, and then reads spaceCompo
! (group/IntR0/spaceCompo) and idofs (group/IntR1/idofs). An error is
! raised (in debug mode) if these datasets are missing.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   hdf5 : TYPE(HDF5File_) - HDF5 file opened for reading
!   group : CHARACTER(*) - group of the field in the file
!   fedof, geofedof : CLASS(FEDOF_) - (optional) FEDOFs of the field
!   fedofs, geofedofs, timefedof, timefedofs - (optional) other FEDOFs
!
! See also: [[obj_Export]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
    CLASS(FEDOF_), TARGET, INTENT(IN), OPTIONAL :: fedof, geofedof
    !! FEDOFs of the field and of the geometry
    TYPE(FEDOFPointer_), INTENT(IN), OPTIONAL :: fedofs(:), geofedofs(:)
    !! FEDOFs of the physical variables
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! FEDOF in time
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedofs(:)
    !! FEDOFs in time of the physical variables
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Export a vector field to an HDF5 file
!
! Nothing is done if obj is not initiated. Otherwise Export of
! AbstractNodeField_ is called, and spaceCompo and idofs are written
! in group/IntR0/spaceCompo and group/IntR1/idofs.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   hdf5 : TYPE(HDF5File_) - HDF5 file opened for writing
!   group : CHARACTER(*) - group of the field in the file
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
! TYPE(VectorField_) :: u, v
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: raw(12), want(12)
! INTEGER(I4B) :: tsize, ii
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! want = [(REAL(ii, DFP), ii=1, 12)]
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL u%Export(hdf5=h5, group="/u")
! CALL h5%DEALLOCATE()
!
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL v%Import(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
! CALL h5%DEALLOCATE()
!
! CALL OK(v%IsInitiated(), "Import: IsInitiated")
! CALL OK(v%spaceCompo .EQ. 2, "Import: spaceCompo")
! CALL OK(ALL(v%idofs .EQ. [1, 2]), "Import: idofs")
! CALL OK(v%Size() .EQ. 12, "Import: Size")
! CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(raw - want) .LT. tol), "Import: values")
!
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]]

INTERFACE VectorFieldExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE VectorFieldExport

!----------------------------------------------------------------------------
!                                                      ExportToVTK@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Write the values of a vector field in a VTK file
!
! The values of all DOFs are written as one data array (named as the
! field) with three components x, y, z; the missing components are
! zero. It is called by WriteData_vtk of AbstractNodeField_.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   vtk : TYPE(VTKFile_) - VTK file
!
! See also: [[AbstractNodeField_:obj_WriteData_vtk1]]

INTERFACE
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtk)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! node field object
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! vtkfile object
  END SUBROUTINE obj_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                  ExportHigherOrderNodeCoordToVTK@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-12
! summary: Export the field as the node coordinates of a high order
!   mesh
!
! obj contains the coordinates of the DOFs of a (higher order)
! Lagrange FEDOF, e.g., set by [[obj_SetHigherOrderNodeCoord]]. They
! are written as the points of a VTK unstructured grid, and the cells
! are made from the connectivity of fedof with the VTK Lagrange cell
! types. If vtk is not initiated, it is initiated with filename
! (binary appended, unstructured grid). opentag, content and closetag
! select the parts of the piece which are written.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field of node coordinates
!   vtk : TYPE(VTKFile_) - VTK file
!   filename : CHARACTER(*) - (optional) file name, needed if vtk is not
!     initiated
!   opentag, content, closetag : LOGICAL(LGT) - (optional) default true
!
! See also: [[obj_SetHigherOrderNodeCoord]]

INTERFACE
  MODULE SUBROUTINE obj_ExportHigherOrderNodeCoordToVTK(obj, vtk, filename, &
                                                        opentag, content, &
                                                        closetag)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field of node coordinates
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! VTK file
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! file name, needed if vtk is not initiated
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: opentag
    !! Default is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: closetag
    !! Default is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: content
    !! Default is true
  END SUBROUTINE obj_ExportHigherOrderNodeCoordToVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the vector of one node
!
! The components of node globalNode are set from VALUE (component c
! from VALUE(c)); with addContribution, scale * VALUE is added.
! SIZE(VALUE) must be spaceCompo. The values are stored in DOF format,
! so component c of node i is entry (c - 1) * tNodes + i.
!
! The example also shows Set(VALUE) for all entries ([[obj_Set12]]),
! Set(VALUE, globalNode, islocal, spaceCompo) for one value
! ([[obj_Set10]]), Set(globalNode(:), islocal, VALUE(:)) for the same
! vector at several nodes ([[obj_Set7]]), and the matching Get.
!
! @note
!   Without addContribution, scale is not used.
! @endnote
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   globalNode : INTEGER(I4B) - node number
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   VALUE : REAL(DFP) - vector, SIZE(VALUE) = spaceCompo
!   scale : REAL(DFP) - (optional) scale, used with addContribution
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u
! REAL(DFP) :: raw(12), vec(6), val
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! ! all entries to one value
! CALL u%Set(VALUE=0.0_DFP)
! CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(raw) .LT. tol), "Set all to a value")
!
! ! vector of one node
! CALL u%Set(globalNode=3, islocal=math%yes, VALUE=[1.0_DFP, 2.0_DFP])
! CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
!                    tsize=tsize)
! CALL OK(ABS(raw(3) - 1.0_DFP) .LT. tol .AND. &
!         ABS(raw(9) - 2.0_DFP) .LT. tol, "Set node: DOF storage")
! CALL u%Get(VALUE=vec, tsize=tsize, globalNode=3)
! CALL OK(tsize .EQ. 2, "Get node: tsize")
! CALL OK(ALL(ABS(vec(1:2) - [1.0_DFP, 2.0_DFP]) .LT. tol), "Get node")
!
! CALL u%Set(globalNode=3, islocal=math%yes, VALUE=[1.0_DFP, 1.0_DFP], &
!            scale=2.0_DFP, addContribution=math%yes)
! CALL u%Get(VALUE=vec, tsize=tsize, globalNode=3)
! CALL OK(ALL(ABS(vec(1:2) - [3.0_DFP, 4.0_DFP]) .LT. tol), &
!         "Set node: add 2 * value")
!
! ! one component of one node
! CALL u%Set(VALUE=7.0_DFP, globalNode=4, islocal=math%yes, spaceCompo=2)
! CALL u%Get(VALUE=val, globalNode=4, islocal=math%yes, spaceCompo=2)
! CALL OK(ABS(val - 7.0_DFP) .LT. tol, "Set/Get one value")
!
! ! the same vector at several nodes
! CALL u%Set(globalNode=[1, 2], islocal=math%yes, &
!            VALUE=[5.0_DFP, 6.0_DFP])
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
! CALL OK(tsize .EQ. 6, "Get component: tsize")
! CALL OK(ALL(ABS(vec - [5.0_DFP, 5.0_DFP, 3.0_DFP, 0.0_DFP, 0.0_DFP, &
!                        0.0_DFP]) .LT. tol), "Get component 1")
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
! CALL OK(ALL(ABS(vec - [6.0_DFP, 6.0_DFP, 4.0_DFP, 7.0_DFP, 0.0_DFP, &
!                        0.0_DFP]) .LT. tol), "Get component 2")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Get1]], [[obj_Set7]], [[obj_Set10]], [[obj_Set12]]

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value, size of value should be obj%spaceCompo
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the vector of all nodes
!
! Component c of all nodes becomes scale * VALUE(c), or scale * VALUE(c)
! is added with addContribution. SIZE(VALUE) must be spaceCompo.
!
! @note
!   In debug mode the check of the field type is inverted: an error is
!   raised if the field is not constant.
! @endnote
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - vector, SIZE(VALUE) = spaceCompo
!   scale : REAL(DFP) - (optional) scale, default 1
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u
! REAL(DFP) :: vec(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL u%Set(VALUE=[1.0_DFP, 2.0_DFP])
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
! CALL OK(ALL(ABS(vec - 1.0_DFP) .LT. tol), "component 1")
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
! CALL OK(ALL(ABS(vec - 2.0_DFP) .LT. tol), "component 2")
!
! CALL u%Set(VALUE=[1.0_DFP, 1.0_DFP], scale=-1.0_DFP, &
!            addContribution=math%yes)
! CALL u%Get(VALUE=vec, tsize=tsize, globalNode=5)
! CALL OK(ALL(ABS(vec(1:2) - [0.0_DFP, 1.0_DFP]) .LT. tol), &
!         "add -1 * value")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Set12]]

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value, size of value should be obj%spaceCompo
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one component of all nodes to a value
!
! Component spaceCompo of all nodes becomes VALUE, or scale * VALUE is
! added with addContribution.
!
! The example also shows Set(VALUE(:), spaceCompo) ([[obj_Set5]]),
! Set(VALUE(:), globalNode(:), islocal, spaceCompo) ([[obj_Set9]]) and
! Get(VALUE(:), tsize, globalNode(:), islocal, spaceCompo)
! ([[obj_Get4]]).
!
! @note
!   Without addContribution, scale is not used.
! @endnote
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - value
!   spaceCompo : INTEGER(I4B) - space component, 1 to obj%spaceCompo
!   scale : REAL(DFP) - (optional) scale, used with addContribution
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u
! REAL(DFP) :: vec(6), src(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!
! CALL u%Set(VALUE=-1.0_DFP, spaceCompo=2)
! CALL u%Set(VALUE=src, spaceCompo=1)
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
! CALL OK(ALL(ABS(vec - src) .LT. tol), "Set component from vector")
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
! CALL OK(ALL(ABS(vec + 1.0_DFP) .LT. tol), "Set component to value")
!
! CALL u%Set(VALUE=[10.0_DFP, 20.0_DFP], globalNode=[2, 5], &
!            islocal=math%yes, spaceCompo=2)
! CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[5, 2, 1], &
!            islocal=math%yes, spaceCompo=2)
! CALL OK(tsize .EQ. 3, "Get selected component: tsize")
! CALL OK(ALL(ABS(vec(1:3) - [20.0_DFP, 10.0_DFP, -1.0_DFP]) .LT. tol), &
!         "Set selected component")
!
! CALL u%Set(VALUE=src, spaceCompo=1, scale=2.0_DFP, &
!            addContribution=math%yes)
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
! CALL OK(ALL(ABS(vec - 3.0_DFP * src) .LT. tol), &
!         "Set component: add 2 * vector")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Set5]], [[obj_Set9]], [[obj_Get4]]

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field object
    REAL(DFP), INTENT(IN) :: VALUE
    !! value (all values are set to value)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all the values from a rank 2 array
!
! If storageFMT is TypeFieldOpt%storageFormatNodes, VALUE(c, i) is
! component c of node i (SHAPE(VALUE) = [spaceCompo, tNodes]). If it is
! TypeFieldOpt%storageFormatDOF, VALUE(i, c) is component c of node i
! (SHAPE(VALUE) = [tNodes, spaceCompo]). The shape is checked in debug
! mode.
!
! The example also shows the selected nodes ([[obj_Set8]]) and Get with
! both formats ([[obj_Get2]], [[obj_Get3]]).
!
! @note
!   Without addContribution, scale is not used.
! @endnote
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - values in the given format
!   storageFMT : INTEGER(I4B) - storageFormatNodes or storageFormatDOF
!   scale : REAL(DFP) - (optional) scale, used with addContribution
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u
! REAL(DFP) :: xn(2, 6), xd(6, 2), yn(2, 2), yd(2, 2)
! INTEGER(I4B) :: nrow, ncol, ii
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! DO ii = 1, 6
!   xn(:, ii) = [REAL(ii, DFP), 10.0_DFP * ii]
! END DO
!
! ! all nodes, nodes format
! CALL u%Set(VALUE=xn, storageFMT=TypeFieldOpt%storageFormatNodes)
! CALL u%Get(VALUE=xd, nrow=nrow, ncol=ncol, &
!            storageFMT=TypeFieldOpt%storageFormatDOF)
! CALL OK(nrow .EQ. 6 .AND. ncol .EQ. 2, "Get DOF format: nrow, ncol")
! CALL OK(ALL(ABS(xd - TRANSPOSE(xn)) .LT. tol), &
!         "Set nodes format, Get DOF format")
!
! ! all nodes, DOF format
! CALL u%Set(VALUE=2.0_DFP * xd, &
!            storageFMT=TypeFieldOpt%storageFormatDOF)
! CALL u%Get(VALUE=xn, nrow=nrow, ncol=ncol, &
!            storageFMT=TypeFieldOpt%storageFormatNodes)
! CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 6, "Get nodes format: nrow, ncol")
! CALL OK(ALL(ABS(xn - 2.0_DFP * TRANSPOSE(xd)) .LT. tol), &
!         "Set DOF format, Get nodes format")
!
! ! selected nodes, nodes format
! yn = RESHAPE([-1.0_DFP, -2.0_DFP, -3.0_DFP, -4.0_DFP], [2, 2])
! CALL u%Set(globalNode=[1, 6], islocal=math%yes, VALUE=yn, &
!            storageFMT=TypeFieldOpt%storageFormatNodes)
! CALL u%Get(VALUE=yd, nrow=nrow, ncol=ncol, &
!            storageFMT=TypeFieldOpt%storageFormatDOF, &
!            globalNode=[1, 6], islocal=math%yes)
! CALL OK(ALL(ABS(yd - TRANSPOSE(yn)) .LT. tol), &
!         "Set selected, nodes format")
!
! ! selected nodes, DOF format
! CALL u%Set(globalNode=[1, 6], islocal=math%yes, VALUE=-yd, &
!            storageFMT=TypeFieldOpt%storageFormatDOF)
! CALL u%Get(VALUE=yn, nrow=nrow, ncol=ncol, &
!            storageFMT=TypeFieldOpt%storageFormatNodes, &
!            globalNode=[6, 1], islocal=math%yes)
! CALL OK(ALL(ABS(yn - RESHAPE([3.0_DFP, 4.0_DFP, 1.0_DFP, 2.0_DFP], &
!                              [2, 2])) .LT. tol), &
!         "Set selected, DOF format")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Set8]], [[obj_Get2]], [[obj_Get3]]

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, VALUE, storageFMT, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! size(value, 1) should be obj%spaceCompo
    !! size(value, 2) should be tNodes
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! if storageFMT is NODES_FMT then:
    !! size(value, 1) should be obj%spaceCompo
    !! size(value, 2) should be tNodes
    !!
    !! if storageFMT is NODES_FMT then:
    !! size(value, 2) should be obj%spaceCompo
    !! size(value, 1) should be tNodes
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one component of all nodes from a vector
!
! Component spaceCompo of node i is set from VALUE(i); with
! addContribution, scale * VALUE is added. SIZE(VALUE) must be the
! number of nodes.
!
! @note
!   Without addContribution, scale is not used.
! @endnote
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - values, SIZE(VALUE) = tNodes
!   spaceCompo : INTEGER(I4B) - space component
!   scale : REAL(DFP) - (optional) scale, used with addContribution
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! size of value should be tNodes
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one component from another node field
!
! @note
!   This routine is under development; it raises an error.
! @endnote
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : CLASS(AbstractNodeField_) - node field
!   spaceCompo : INTEGER(I4B) - space component
!   scale : REAL(DFP) - (optional) scale, default 1
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    !! abstract node field
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the same vector at the selected nodes
!
! Calls [[obj_Set1]] for each node of globalNode with VALUE.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - vector, SIZE(VALUE) = spaceCompo
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   scale : REAL(DFP) - (optional) scale, used with addContribution
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, VALUE, globalNode, islocal, scale, &
                             addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value, this value will be assigned to all global nodes
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the vectors of the selected nodes from a rank 2 array
!
! If storageFMT is TypeFieldOpt%storageFormatNodes, VALUE(c, j) is
! component c of node globalNode(j) (SHAPE(VALUE) = [spaceCompo,
! SIZE(globalNode)]). If it is TypeFieldOpt%storageFormatDOF,
! VALUE(j, c) is component c of node globalNode(j). The shape is
! checked in debug mode.
!
! @note
!   Without addContribution, scale is not used.
! @endnote
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   VALUE : REAL(DFP) - values in the given format
!   scale : REAL(DFP) - (optional) scale, used with addContribution
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!   storageFMT : INTEGER(I4B) - storageFormatNodes or storageFormatDOF
!
! See also: [[obj_Set4]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, globalNode, islocal, VALUE, scale, &
                             addContribution, storageFMT)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! value which will be used for obj=value
    !! if storageFMT is NODES_FMT then
    !!   size(value, 1) should be obj%spaceCompo
    !!   size(value, 2) should be size(globalNode)
    !! if storageFMT is DOF_FMT then
    !!   size(value, 2) should be obj%spaceCompo
    !!   size(value, 1) should be size(globalNode)
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format can be NODES_FMT or DOF_FMT
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one component of the selected nodes
!
! Component spaceCompo of node globalNode(j) is set from VALUE(j); with
! addContribution, scale * VALUE is added. SIZE(VALUE) must be
! SIZE(globalNode).
!
! @note
!   Without addContribution, scale is not used.
! @endnote
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - values, SIZE(VALUE) = SIZE(globalNode)
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   spaceCompo : INTEGER(I4B) - space component
!   scale : REAL(DFP) - (optional) scale, used with addContribution
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, VALUE, globalNode, islocal, spaceCompo, &
                             scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be used in obj = value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one component of one node
!
! Component spaceCompo of node globalNode becomes scale * VALUE, or
! scale * VALUE is added with addContribution.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - value
!   globalNode : INTEGER(I4B) - node number
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   spaceCompo : INTEGER(I4B) - space component
!   scale : REAL(DFP) - (optional) scale, default 1
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, VALUE, globalNode, islocal, spaceCompo, &
                              scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected nodes by using an FEVariable
!
! VALUE is a vector, nodal FEVariable. If it is constant, its vector is
! set at all nodes of globalNode ([[obj_Set7]]). If it is a space
! variable, its values (shape (spaceCompo, SIZE(globalNode))) are set
! with the nodes format ([[obj_Set8]]). Other variable types raise an
! error.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : TYPE(FEVariable_) - vector, nodal FEVariable
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   scale : REAL(DFP) - (optional) scale, default 1
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: TypeFEVariableVector
! USE BaseType, ONLY: TypeFEVariableSpace
! USE BaseType, ONLY: TypeFEVariableConstant
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FEVariable_Method, ONLY: NodalVariable
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u
! TYPE(FEVariable_) :: fevar
! REAL(DFP) :: vec(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL u%Set(VALUE=0.0_DFP)
!
! ! constant FEVariable: one vector for all selected nodes
! fevar = NodalVariable([1.0_DFP, 2.0_DFP], TypeFEVariableVector, &
!                       TypeFEVariableConstant)
! CALL u%Set(VALUE=fevar, globalNode=[3, 4], islocal=math%yes)
!
! ! space FEVariable: one vector for each selected node
! fevar = NodalVariable(RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP], &
!                               [2, 2]), TypeFEVariableVector, &
!                       TypeFEVariableSpace)
! CALL u%Set(VALUE=fevar, globalNode=[5, 6], islocal=math%yes)
!
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
! CALL OK(ALL(ABS(vec - [0.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP, 5.0_DFP, &
!                        7.0_DFP]) .LT. tol), "Set FEVariable: compo 1")
! CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
! CALL OK(ALL(ABS(vec - [0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, 6.0_DFP, &
!                        8.0_DFP]) .LT. tol), "Set FEVariable: compo 2")
!
! ! get the vectors of the selected nodes in an FEVariable
! CALL u%Get(VALUE=fevar, globalNode=[6, 3], islocal=math%yes)
! CALL OK(ALL(fevar%s(1:2) .EQ. [2, 2]), "Get FEVariable: shape")
! CALL OK(ALL(ABS(fevar%val(1:4) - [7.0_DFP, 8.0_DFP, 1.0_DFP, &
!                                   2.0_DFP]) .LT. tol), &
!         "Get FEVariable: values")
!
! CALL u%GetFEVariable(globalNode=[5], islocal=math%yes, VALUE=fevar)
! CALL OK(ALL(ABS(fevar%val(1:2) - [5.0_DFP, 6.0_DFP]) .LT. tol), &
!         "GetFEVariable")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Get6]], [[obj_GetFeVariable]]

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, VALUE, globalNode, islocal, scale, &
                              addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! FEVariable, space-nodal values of vector
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if tree then globalNode is local node number
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all the values to one value
!
! All values (all components of all nodes) become scale * VALUE, or
! scale * VALUE is added with addContribution. It calls SetAll of
! AbstractNodeField_.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - value
!   scale : REAL(DFP) - (optional) scale, default 1
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, VALUE, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Copy the values of another vector field
!
! obj = VALUE: all values of VALUE are copied into obj. VALUE must be a
! VectorField_ (an error is raised in debug mode otherwise) with the
! same size.
!
! Get(VALUE=w) copies the values of obj into w ([[obj_Get7]]); the
! example shows both.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : CLASS(VectorField_) - vector field to be copied
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u, v, w
! REAL(DFP) :: raw(12), want(12)
! INTEGER(I4B) :: tsize, ii
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL v%Initiate(name="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
!                 fedof=fedof, geofedof=geofedof)
! CALL w%Initiate(name="w", engine="NATIVE_SERIAL", spaceCompo=[2], &
!                 fedof=fedof, geofedof=geofedof)
!
! want = [(REAL(ii, DFP), ii=1, 12)]
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
! CALL v%Set(VALUE=0.0_DFP)
! CALL w%Set(VALUE=0.0_DFP)
!
! CALL v%Set(VALUE=u)
! CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(raw - want) .LT. tol), "Set(VALUE=u): v = u")
!
! CALL v%Get(VALUE=w)
! CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(raw - want) .LT. tol), "Get(VALUE=w): w = v")
!
! CALL w%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Get7]]

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, VALUE)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field, obj = VALUE
    CLASS(VectorField_), INTENT(IN) :: VALUE
    !! vector field to be copied
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetByFunction@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-19
! update:
!   - 2025-11-12: scalar and vector functions (Shion Shimizu)
! summary: Set the vector field by using a user function
!
! If func is a vector function, all components are set from it. If func
! is a scalar function, only component spaceCompo is set (spaceCompo is
! required). For each element the DOF values are computed from func by
! the finite element (for Lagrange polynomials, the values of func at
! the interpolation points) and set in obj.
!
! func has the arguments (x, y, z, time); the time is given by
! times(1) (default 0).
!
! Arguments:
!   obj : CLASS(VectorField_) - initiated vector field
!   func : CLASS(UserFunction_) - scalar or vector user function
!   times : REAL(DFP) - (optional) time, SIZE(times) = 1
!   ivar, idof, timeCompo : INTEGER(I4B) - (optional) not used
!   spaceCompo : INTEGER(I4B) - (optional) component for a scalar func
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
! USE BaseType, ONLY: InterfaceVectorSubroutine
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorField_) :: u
! TYPE(UserFunction_) :: vfunc, sfunc
! PROCEDURE(InterfaceVectorSubroutine), POINTER :: vptr
! PROCEDURE(InterfaceScalarSubroutine), POINTER :: sptr
! CLASS(AbstractMesh_), POINTER :: mesh
! REAL(DFP), ALLOCATABLE :: xij(:, :)
! REAL(DFP) :: found(6), want(2), g
! INTEGER(I4B) :: ii, nrow, ncol, tsize
! LOGICAL(LGT) :: isok1, isok2
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL vfunc%Initiate(NAME="f", returnType=TypeFEVariableOpt%vector, &
!                     argType=TypeFEVariableOpt%space, numArgs=4, &
!                     numReturns=2)
! vptr => vectorFunc
! CALL vfunc%SetVectorFunctionPointer(func=vptr)
!
! CALL sfunc%Initiate(NAME="g", returnType=TypeFEVariableOpt%scalar, &
!                     argType=TypeFEVariableOpt%space, numArgs=4, &
!                     numReturns=1)
! sptr => scalarFunc
! CALL sfunc%SetScalarFunctionPointer(func=sptr)
!
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! mesh => dom%GetMeshPointer()
! ALLOCATE (xij(3, mesh%GetTotalNodes()))
! CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)
!
! ! vector function
! CALL u%Set(VALUE=0.0_DFP)
! CALL u%SetByFunction(func=vfunc)
! isok1 = .TRUE.
! DO ii = 1, ncol
!   CALL u%Get(VALUE=found, tsize=tsize, globalNode=ii)
!   CALL vectorFunc(xij(:, ii), 3, want, tsize)
!   isok1 = isok1 .AND. ALL(ABS(found(1:2) - want) .LT. 1.0E-8_DFP)
! END DO
! CALL OK(isok1, "vector function: u = f at the nodes")
!
! ! scalar function for component 2
! CALL u%SetByFunction(func=sfunc, spaceCompo=2)
! isok1 = .TRUE.
! isok2 = .TRUE.
! DO ii = 1, ncol
!   CALL u%Get(VALUE=found, tsize=tsize, globalNode=ii)
!   CALL vectorFunc(xij(:, ii), 3, want, tsize)
!   CALL scalarFunc(xij(:, ii), 3, g)
!   isok1 = isok1 .AND. ABS(found(1) - want(1)) .LT. 1.0E-8_DFP
!   isok2 = isok2 .AND. ABS(found(2) - g) .LT. 1.0E-8_DFP
! END DO
! CALL OK(isok1, "scalar function: component 1 unchanged")
! CALL OK(isok2, "scalar function: component 2 = g")
!
! NULLIFY (mesh)
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
!
! CONTAINS
!
! SUBROUTINE vectorFunc(args, nargs, ans, tsize)
!   REAL(DFP), INTENT(IN) :: args(:)
!   INTEGER(I4B), INTENT(IN) :: nargs
!   REAL(DFP), INTENT(INOUT) :: ans(:)
!   INTEGER(I4B), INTENT(OUT) :: tsize
!   tsize = 2
!   ans(1) = 1.0_DFP + 2.0_DFP * args(1) + 3.0_DFP * args(2)
!   ans(2) = 4.0_DFP - args(1) + args(2)
! END SUBROUTINE vectorFunc
!
! SUBROUTINE scalarFunc(args, nargs, ans)
!   REAL(DFP), INTENT(IN) :: args(:)
!   INTEGER(I4B), INTENT(IN) :: nargs
!   REAL(DFP), INTENT(INOUT) :: ans
!   ans = args(1) + args(2)
! END SUBROUTINE scalarFunc
!
! END PROGRAM main
!```
!
! See also: [[obj_SetLagrangeDOF]]

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction(obj, func, times, ivar, idof, &
                                      spaceCompo, timeCompo)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    REAL(DFP), INTENT(IN), OPTIONAL :: times(:)
    !! If present then its size should be 1
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ivar
    !! ivar (not used)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: idof
    !! idof (not used)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: spaceCompo
    !! space component, used when func is a scalar function
    INTEGER(I4B), INTENT(IN), OPTIONAL :: timeCompo
    !! time component, not used
  END SUBROUTINE obj_SetByFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                          SetHigherOrderNodeCoord@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-12
! summary: Set the field to the coordinates of the DOFs
!
! For each active element, the interpolation points of fedof (order and
! ipType of the element) are computed in the physical element from the
! nodes of the mesh, and they are set as the values of the DOFs of the
! element (nodes format, orientation fixed). For a Lagrange fedof of
! higher order than the mesh, obj then contains the node coordinates of
! the higher order mesh; it is used with
! [[obj_ExportHigherOrderNodeCoordToVTK]].
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field with a Lagrange fedof,
!     spaceCompo is the space dimension
!
! See also: [[obj_ExportHigherOrderNodeCoordToVTK]]

INTERFACE
  MODULE SUBROUTINE obj_SetHigherOrderNodeCoord(obj)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field, the values are the DOF coordinates
  END SUBROUTINE obj_SetHigherOrderNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetLagrangeDOF@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-12
! summary: Set the Lagrange DOFs of obj from another vector field
!
! For each active element, obj2 is interpolated at the interpolation
! points of obj (order and ipType of the element in obj%fedof), and the
! values of all components are set as the Lagrange DOFs of obj. obj
! must use Lagrange polynomials; obj2 can use any basis. Both fields
! must have the same spaceCompo and meshes with the same number of
! elements.
!
! Arguments:
!   obj : CLASS(VectorField_) - Lagrange vector field, values are set
!   obj2 : CLASS(VectorField_) - vector field which is interpolated
!
! See also: [[obj_SetByFunction]]

INTERFACE
  MODULE SUBROUTINE obj_SetLagrangeDOF(obj, obj2)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! values of obj will be set.
    CLASS(VectorField_), INTENT(INOUT) :: obj2
    !! values of obj will be set from obj2
  END SUBROUTINE obj_SetLagrangeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the vector of one node or one component of all nodes
!
! Exactly one of globalNode and spaceCompo must be present (checked in
! debug mode). With globalNode, VALUE(1:spaceCompo) is the vector of
! the node (tsize = spaceCompo). With spaceCompo, VALUE(1:tNodes) is
! the component of all nodes (tsize = tNodes).
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - values
!   tsize : INTEGER(I4B) - number of values written in VALUE
!   globalNode : INTEGER(I4B) - (optional) local node number
!   spaceCompo : INTEGER(I4B) - (optional) space component
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, tsize, globalNode, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of data written in value
    !! if globalNode is present then tsize = obj%spaceCompo
    !! if spaceCompo is present then tsize = tNodes
    INTEGER(I4B), INTENT(IN), OPTIONAL :: globalNode
    !! This should be a local node
    INTEGER(I4B), INTENT(IN), OPTIONAL :: spaceCompo
    !! space component
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get all the values in a rank 2 array
!
! With TypeFieldOpt%storageFormatNodes, VALUE(c, i) is component c of
! node i (nrow = spaceCompo, ncol = tNodes). With storageFormatDOF,
! VALUE(i, c) is component c of node i (nrow = tNodes, ncol =
! spaceCompo). VALUE must be large enough (checked in debug mode).
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - values
!   nrow, ncol : INTEGER(I4B) - rows and columns written in VALUE
!   storageFMT : INTEGER(I4B) - storageFormatNodes or storageFormatDOF
!   force3D : LOGICAL(LGT) - (optional) not used by this routine
!
! See also: [[obj_Set4]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, nrow, ncol, storageFMT, force3D)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! values
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in value
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! NODES_FMT:: nrow is obj%spaceCompo or 3, ncol is tNodes
    !! DOF_FMT:: nrow is tNodes, ncol is obj%spaceCompo or 3
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: force3D
    !! not used
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the vectors of the selected nodes in a rank 2 array
!
! Same as [[obj_Get2]] for the nodes of globalNode: with the nodes
! format VALUE(c, j) is component c of node globalNode(j), with the DOF
! format VALUE(j, c).
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - values
!   nrow, ncol : INTEGER(I4B) - rows and columns written in VALUE
!   storageFMT : INTEGER(I4B) - storageFormatNodes or storageFormatDOF
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   force3D : LOGICAL(LGT) - (optional) not used by this routine
!
! See also: [[obj_Set4]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, nrow, ncol, storageFMT, globalNode, &
                             islocal, force3D)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! The number of columns in value is same as the
    !! the size of globalNode
    !! The number of rows in columns is equal to the
    !! spaceCompo
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! The number of columns in value is same as the
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! NODES_FMT:: nrow is obj%spaceCompo or 3, ncol is size(globalNode)
    !! DOF_FMT:: nrow is size(globalNode), ncol is obj%spaceCompo or 3
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: force3D
    !! if true then minimum 3 components are returned in value
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get one component of the selected nodes
!
! VALUE(j) is component spaceCompo of node globalNode(j); tsize =
! SIZE(globalNode).
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - values, SIZE(VALUE) >= SIZE(globalNode)
!   tsize : INTEGER(I4B) - number of values written in VALUE
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   spaceCompo : INTEGER(I4B) - space component
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, tsize, globalNode, islocal, &
                             spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! the size of value should be same as globalNode
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get one component of one node
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - value
!   globalNode : INTEGER(I4B) - node number
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   spaceCompo : INTEGER(I4B) - space component
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, islocal, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value to be returned
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the vectors of the selected nodes in an FEVariable
!
! Calls [[obj_GetFeVariable]].
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : TYPE(FEVariable_) - vector, nodal, space FEVariable
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - true if globalNode are local node numbers
!
! See also: [[obj_Set11]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, globalNode, islocal)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! space nodal values of vector
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! globla or local node numbe
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Copy the values into another vector field
!
! VALUE = obj: the values of obj are copied into VALUE by COPY. Both
! fields must be initiated (checked in debug mode) and have the same
! size.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : CLASS(VectorField_) - initiated vector field, VALUE = obj
!
! See also: [[obj_Set13]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field
    CLASS(VectorField_), INTENT(INOUT) :: VALUE
    !! vector field, VALUE = obj
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get the vectors of the selected nodes in an FEVariable
!
! VALUE becomes a vector, nodal, space FEVariable with shape
! (spaceCompo, SIZE(globalNode)); column j is the vector of node
! globalNode(j). ivar is not used.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - true if globalNode are local node numbers
!   VALUE : TYPE(FEVariable_) - values
!   ivar : INTEGER(I4B) - (optional) not used
!
! See also: [[obj_Set11]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! is true then globalNode is local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! vector, nodal, space FEVariable
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ivar
    !! not used
  END SUBROUTINE obj_GetFeVariable
END INTERFACE

INTERFACE VectorFieldGetFEVariable
  MODULE PROCEDURE obj_GetFeVariable
END INTERFACE VectorFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                     GetMeshField@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-04
! summary: Get the mesh field of the interpolated values
!
! For each element, obj is interpolated at the interpolation points of
! the given order and ipType of the element, and the vectors are
! inserted in meshField (vector, nodal, space). If meshField is not
! initiated, it is initiated from obj.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   meshField : CLASS(MeshField_) - mesh field, values are inserted
!   order : INTEGER(I4B) - order of interpolation points in x, y, z
!   ipType : INTEGER(I4B) - interpolation point type in x, y, z

INTERFACE
  MODULE SUBROUTINE obj_GetMeshField(obj, meshField, order, ipType)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    CLASS(MeshField_), INTENT(INOUT) :: meshField
    !! mesh field, values are inserted
    INTEGER(I4B), INTENT(IN) :: order(3), ipType(3)
    !! order and type of interpolation points
  END SUBROUTINE obj_GetMeshField
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetVtkUnstructuredValue@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Vectors at the points of the VTK unstructured grid
!
! Returns the vector of obj at each point of the grid given by
! [[AbstractNodeField_:obj_GetVtkUnstructuredGridData1]], in the same
! order. The points are not shared between cells (DG like storage), so
! SIZE(VALUE, 2) is fedof%GetSumElemTotalDOF(). VALUE always has three
! rows, so that VTK readers (ParaView) treat it as a vector; the rows
! after spaceCompo are zero. VALUE is reallocated; the work arrays of
! [[obj_GetVtkUnstructuredValue2]] are allocated here.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - vectors at the points, reallocated to
!     (3, fedof%GetSumElemTotalDOF())
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BaseType, ONLY: TypeFEVariableOpt
!   USE BaseType, ONLY: InterfaceVectorSubroutine
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: INT8
!   USE Test_Method, ONLY: OK
!   USE UserFunction_Class, ONLY: UserFunction_
!   USE VectorField_Class, ONLY: VectorField_
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: tomlFileName = &
!     "GetVtkUnstructuredValue_test_1.toml"
!   REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
!   TYPE(FEDomain_) :: dom
!   TYPE(FEDOF_), TARGET :: fedof, geofedof
!   TYPE(VectorField_) :: u
!   INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
!   INTEGER(INT8), ALLOCATABLE :: types(:)
!   REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:, :), fvals(:, :)
!   TYPE(UserFunction_) :: func
!   PROCEDURE(InterfaceVectorSubroutine), POINTER :: procptr
!   INTEGER(I4B) :: ii, tsize
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   ! user function
!   CALL func%Initiate(NAME="f", returnType=TypeFEVariableOpt%vector, &
!                      argType=TypeFEVariableOpt%space, numArgs=4, &
!                      numReturns=2)
!   procptr => vectorFunc
!   CALL func%SetVectorFunctionPointer(func=procptr)
!
!   ! domain and u = f
!   CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
!   CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                         filename=tomlFileName, tomlName="u")
!   CALL u%Set(VALUE=math%zero)
!   CALL u%SetByFunction(func=func)
!
!   ! grid and values
!   CALL u%GetVtkUnstructuredGridData(points=points, &
!                                     connectivity=connectivity, &
!                                     offsets=offsets, types=types)
!   CALL u%GetVtkUnstructuredValue(VALUE=VALUE)
!
!   CALL OK(SIZE(VALUE, 1) .EQ. 3, "three rows")
!   CALL OK(SIZE(VALUE, 2) .EQ. 32, "number of columns")
!   CALL OK(SIZE(VALUE, 2) .EQ. SIZE(points, 2), "one vector for each point")
!   CALL OK(ALL(VALUE(3, :) .EQ. math%zero), "third row is zero")
!
!   ALLOCATE(fvals(2, SIZE(VALUE, 2)))
!   DO ii = 1, SIZE(VALUE, 2)
!     CALL vectorFunc(points(:, ii), 3, fvals(:, ii), tsize)
!   END DO
!   CALL OK(MAXVAL(ABS(VALUE(1:2, :) - fvals)) .LT. tol, &
!           "value = f at points")
!
!   CALL u%DEALLOCATE()
!   CALL fedof%DEALLOCATE()
!   CALL geofedof%DEALLOCATE()
!   CALL dom%DEALLOCATE()
!
! CONTAINS
!
!   SUBROUTINE vectorFunc(args, nargs, ans, tsize)
!     REAL(DFP), INTENT(IN) :: args(:)
!     INTEGER(I4B), INTENT(IN) :: nargs
!     REAL(DFP), INTENT(INOUT) :: ans(:)
!     INTEGER(I4B), INTENT(OUT) :: tsize
!     tsize = 2
!     ans(1) = args(1) * (args(1) - 10.0_DFP) * args(2) * &
!              (args(2) - 10.0_DFP)
!     ans(2) = args(1) + 2.0_DFP * args(2)
!   END SUBROUTINE vectorFunc
!
! END PROGRAM main
!```
!
! See also: [[obj_GetVtkUnstructuredValue2]],
! [[VtkHDF5File_:obj_WritePointData2]]

INTERFACE
  MODULE SUBROUTINE obj_GetVtkUnstructuredValue1(obj, VALUE)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    !! vectors at the points of the grid, shape (3, number of points)
  END SUBROUTINE obj_GetVtkUnstructuredValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetVtkUnstructuredValue@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Vectors at the points of the VTK unstructured grid
!   (no allocation)
!
! Same as [[obj_GetVtkUnstructuredValue1]], but VALUE and the work
! arrays are given by the caller, so that nothing is allocated. For each
! active element, the nodal vectors of obj are interpolated at the VTK
! points of the cell (the interpolation points of the cell order).
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   VALUE : REAL(DFP) - vectors, SIZE(VALUE, 1) >= 3,
!     SIZE(VALUE, 2) >= fedof%GetSumElemTotalDOF()
!   nrow : INTEGER(I4B) - number of rows written in VALUE (3)
!   ncol : INTEGER(I4B) - number of columns written in VALUE
!   refElemCoord, xij, elemCoord, nodalValue, fecon - work arrays, see
!     the sizes of the dummy arguments
!
! See also: [[obj_GetVtkUnstructuredValue1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetVtkUnstructuredValue2(obj, VALUE, nrow, ncol, &
                                                 refElemCoord, xij, &
                                                 elemCoord, nodalValue, &
                                                 fecon)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! vectors at the points of the grid
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value, always 3
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns (points) written in value
    REAL(DFP), INTENT(INOUT) :: refElemCoord(:, :)
    !! temporary storage for nodal coordinate of reference element
    !! size should be at least (3, maxNNE)
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! temporary storage for nodal coordinate of interpolation points
    !! size should be at least (4, maxCon). This variable
    !! is used to construct the quadrature points
    REAL(DFP), INTENT(INOUT) :: elemCoord(:, :)
    !! temporary storage for nodal coordinate of physical element
    !! size should be at least (3, maxNNE)
    REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! temporary storage for element level nodal vectors of obj
    !! size should be at least (spaceCompo, maxCon)
    INTEGER(I4B), INTENT(INOUT) :: fecon(:)
    !! temporary storage for connectivity of an element
    !! size should be at least maxCon
  END SUBROUTINE obj_GetVtkUnstructuredValue2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a Dirichlet boundary condition
!
! The node numbers and the values of dbc (at time times) are obtained
! from dbc%Get, and component dbc%GetDOFNo() of these nodes is set to
! the values. The component must not be larger than spaceCompo
! (checked in debug mode).
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   dbc : CLASS(DirichletBC_) - Dirichlet boundary condition
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplyDirichletBC2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    !! Dirichlet boundary condition
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a list of Dirichlet boundary conditions
!
! Same as [[obj_ApplyDirichletBC1]] for each associated pointer of dbc.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   dbc : TYPE(DirichletBCPointer_) - Dirichlet boundary conditions
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplyDirichletBC1]], [[obj_ApplyDirichletBC3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary conditions
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply the Dirichlet boundary conditions of the field
!
! Calls [[obj_ApplyDirichletBC2]] with obj%dbc.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplyDirichletBC2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC3(obj, times)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time
  END SUBROUTINE obj_ApplyDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!                                         ApplyPointNeumannBC@PointNBCMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-06
! summary: Add the point Neumann boundary conditions
!
! For each point Neumann boundary condition of the field
! (obj%nbc_point), the node numbers and the values (at time times) are
! obtained, and scale * value is added to component nbc%GetDOFNo() of
! these nodes.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field
!   scale : REAL(DFP) - scale
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplySurfaceNeumannBC]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyPointNeumannBC(obj, scale, times)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! Vector field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for neumann boundary condition
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time
  END SUBROUTINE obj_ApplyPointNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!                                     ApplySurfaceNeumannBC@SurfaceNBCMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-06
! summary: Add the surface Neumann boundary conditions
!
! nbcField is set to zero, and the Neumann boundary conditions of the
! field are applied to it as Dirichlet values (extension of the
! boundary data to the domain). Then, for each boundary condition, the
! integral of the test functions times nbcField over the boundary
! facets is computed, and scale times this integral is added to obj.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field (right hand side)
!   nbcField : CLASS(VectorField_) - work field for the boundary data
!   scale : REAL(DFP) - scale
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplyPointNeumannBC]]

INTERFACE
  MODULE SUBROUTINE obj_ApplySurfaceNeumannBC(obj, nbcField, scale, times)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! Vector field
    CLASS(VectorField_), INTENT(INOUT) :: nbcField
    !! Vector field where we will keep the neumann boundary condition
    !! extension to the entire domain
    REAL(DFP), INTENT(IN) :: scale
    !! Scale for neumann boundary condition
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! times
  END SUBROUTINE obj_ApplySurfaceNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!                                           ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-06
! summary: Add the body source given by a user function
!
! For each element, the integral of the test functions times the body
! source over the element is computed by quadrature for each component,
! and scale times this integral is added to obj:
!
! $$ u_{cI} = u_{cI} + s \int_{\Omega} N_I f_c(x, y, z, t) \, d\Omega $$
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field (right hand side)
!   bodySource : CLASS(UserFunction_) - vector function of (x, y, z, t)
!   scale : REAL(DFP) - scale
!   times : REAL(DFP) - (optional) time passed to bodySource
!
! See also: [[obj_ApplyBodySource2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource1(obj, bodySource, scale, times)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field (right hand side)
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! Body source user function
    !! It should be a vector function with
    !! total arguments 4 (x, y, z, time)
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time, which will be passed to the body source function
  END SUBROUTINE obj_ApplyBodySource1
END INTERFACE

!----------------------------------------------------------------------------
!                                           ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-06
! summary: Add the body source given by a vector field
!
! Same as [[obj_ApplyBodySource1]], but the body source is the vector
! field bodySource, which is interpolated at the quadrature points.
!
! Arguments:
!   obj : CLASS(VectorField_) - vector field (right hand side)
!   bodySource : CLASS(VectorField_) - body source
!   scale : REAL(DFP) - scale
!
! See also: [[obj_ApplyBodySource1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource2(obj, bodySource, scale)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! Vector field
    !! The test function will be corresponding to the obj
    CLASS(VectorField_), INTENT(INOUT) :: bodySource
    !! Body source in terms of vector field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
  END SUBROUTINE obj_ApplyBodySource2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-12
! summary: Set the values of the field from a toml table
!
! If the table defines a user function, the field is set by it.
! Otherwise the type of the nodal value is read from the table, and the
! field is set to a constant vector or to the given space values.
!
! Arguments:
!   obj : CLASS(VectorField_) - initiated vector field
!   table : TYPE(toml_table) - toml table with the values

INTERFACE
  MODULE SUBROUTINE obj_SetFromToml(obj, table)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table
  END SUBROUTINE obj_SetFromToml
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VectorField_Class
