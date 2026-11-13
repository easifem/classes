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
! summary: Space-time vector field data type is defined

MODULE STVectorField_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: FEVariable_
USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE GlobalData, ONLY: DFP, DOF_FMT, I4B, LGT, NODES_FMT, NodesToDOF
USE HDF5File_Class, ONLY: HDF5File_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_
USE UserFunction_Class, ONLY: UserFunction_
USE VectorField_Class, ONLY: VectorField_
USE VectorField_Class, ONLY: VectorField_

IMPLICIT NONE

PRIVATE

INTEGER(I4B), PARAMETER :: myconversion = NodesToDOF

PUBLIC :: STVectorField_
PUBLIC :: STVectorFieldPointer_
PUBLIC :: STVectorFieldInitiate
PUBLIC :: STVectorFieldDeallocate
PUBLIC :: STVectorFieldDisplay
PUBLIC :: STVectorFieldImport
PUBLIC :: STVectorFieldExport
PUBLIC :: STVectorFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                             STVectorField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-06
! summary: Space-time vector field
!
! A space-time vector field has one physical variable with spaceCompo
! space components and timeCompo time components (timeCompo is the
! number of DOFs of timefedof). There is one value for each DOF of
! fedof, each space component and each time component, stored in the
! vector of AbstractNodeField_ in DOF format. The component (s, t) is
! the local DOF idof = (t - 1) * spaceCompo + s, and the value of DOF i
! of this component is entry (idof - 1) * tNodes + i, where
! tNodes = fedof%GetTotalDOF().
!
! The values are set by Set (all values, one node, one component,
! selected nodes, rank 2 and rank 3 arrays, FEVariable, copy),
! SetByFunction, SetFromVectorField and the boundary condition and
! body source methods, and read by Get and SetToVectorField.
!
! Components:
!   spaceCompo - number of space components
!   timeCompo - number of time components
!   space_idofs - [1, ..., spaceCompo]
!   time_idofs - [1, ..., timeCompo]
!   idofs - [1, ..., spaceCompo * timeCompo], the local DOFs of all
!     space-time components

TYPE, EXTENDS(AbstractNodeField_) :: STVectorField_
  INTEGER(I4B), PUBLIC :: spaceCompo = 0_I4B
  !! number of space components
  INTEGER(I4B), PUBLIC :: timeCompo = 0_I4B
  !! number of time components
  INTEGER(I4B), ALLOCATABLE :: space_idofs(:)
  !! space components, [1, ..., spaceCompo]
  INTEGER(I4B), ALLOCATABLE :: time_idofs(:)
  !! time components, [1, ..., timeCompo]
  INTEGER(I4B), ALLOCATABLE :: idofs(:)
  !! local DOFs of all space-time components,
  !! [1, ..., spaceCompo * timeCompo]
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by copy
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by arguments
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data stored
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the data from HDFFile
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the data to HDFfile

  ! SET:
  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set the space-time values of one node
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set every node to the same space-time values
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set one space-time component of all nodes to a value
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set all values from a rank 3 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set one space-time component of all nodes from a vector
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set the same space-time values at the selected nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set the selected nodes from a rank 3 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set8 => obj_Set8
  !! Set one space-time component of the selected nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set9 => obj_Set9
  !! Set one space-time component of one node
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set10 => obj_Set10
  !! Set the selected nodes from a FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set11 => obj_Set11
  !! Set all values to one value
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set12 => obj_Set12
  !! Set several space components of one time component
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set13 => obj_Set13
  !! Set several time components of one space component
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set14 => obj_Set14
  !! Copy another space-time vector field
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, &
    Set7, Set8, Set9, Set10, Set11, Set12, Set13, Set14
  !! Set the values of the field
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SetByFunction => &
    obj_SetByFunction
  !! Set the values from a user function
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SetFromVectorField => &
    obj_SetFromVectorField
  !! Set one time component from a vector field
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SetToVectorField => &
    obj_SetToVectorField
  !! Set a vector field from one time component

  ! GET:
  ! @GetMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get the values of a node, a component, or both in a vector
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get all values in a rank 3 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! Get the values of the selected nodes in a rank 3 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! Get one space-time component of the selected nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get5 => obj_Get5
  !! Get one space-time component of one node
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get6 => obj_Get6
  !! Get the space-time values of one node
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get7 => obj_Get7
  !! Get the values of the selected nodes in a FEVariable
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, &
    Get6, Get7
  !! Get the values of the field
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetFEVariable => &
    obj_GetFeVariable
  !! Get multiple values in FEVariable

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC1 => &
    obj_ApplyDirichletBC1
  !! Apply a Dirichlet boundary condition
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC2 => &
    obj_ApplyDirichletBC2
  !! Apply a list of Dirichlet boundary conditions
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC3 => &
    obj_ApplyDirichletBC3
  !! Apply the Dirichlet boundary conditions of the field
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    ApplyDirichletBC2, ApplyDirichletBC3
  !! Apply Dirichlet boundary conditions

  ! SET:
  ! @BodySourceMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyBodySource1 => obj_ApplyBodySource1
  !! Add the space-time body source given by a user function
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyBodySource2 => obj_ApplyBodySource2
  !! Add the body source given by a field (under development)
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyBodySource3 => obj_ApplyBodySource3
  !! Add the body source at one time given by a user function
  GENERIC, PUBLIC :: ApplyBodySource => ApplyBodySource1, &
    ApplyBodySource2, ApplyBodySource3
  !! Generic method for setting body source

  ! SET:
  ! @SurfaceNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: ApplySurfaceNeumannBC => &
    obj_ApplySurfaceNeumannBC
  !! Add the surface Neumann boundary conditions

  ! SET:
  ! @PointNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: ApplyPointNeumannBC => &
    obj_ApplyPointNeumannBC
  !! Add the point Neumann boundary conditions

END TYPE STVectorField_

!---------------------------------------------------------------------------
!                                                     STVectorFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STVectorFieldPointer_
  CLASS(STVectorField_), POINTER :: ptr => NULL()
END TYPE STVectorFieldPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Initiate a space-time vector field from another field
!
! Calls Initiate of AbstractNodeField_ (copy of the field data, the DOF
! data and the values). If obj2 is a space-time vector field, spaceCompo,
! timeCompo, idofs, space_idofs and time_idofs are copied too.
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Initiate1( &
    obj, obj2, copyFull, copyStructure, usePointer)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    !! not used
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    !! not used
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
    !! not used
  END SUBROUTINE obj_Initiate1
END INTERFACE

INTERFACE STVectorFieldInitiate
  MODULE PROCEDURE obj_Initiate1
END INTERFACE STVectorFieldInitiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-06
! summary: Initiate a space-time vector field by passing arguments
!
! The data stored in obj is deallocated first. A space-time vector field
! has one physical variable, whose name is the first character of name,
! with spaceCompo(1) space components and timeCompo time components.
! If timefedof is present and initiated, timeCompo is
! timefedof%GetTotalDOF(); otherwise timeCompo(1) is used (one of them is
! required, checked in debug mode). spaceCompo is required (checked in
! debug mode).
!
! There is one value for each DOF of fedof, each space component and
! each time component, so obj%Size() is
! fedof%GetTotalDOF() * spaceCompo * timeCompo. The values are stored in
! DOF format: the component (s, t) is the local DOF
! idof = (t - 1) * spaceCompo + s, and entry (idof - 1) * tNodes + i
! is the value of DOF i (tNodes = fedof%GetTotalDOF()).
!
! space_idofs is [1, ..., spaceCompo], time_idofs is
! [1, ..., timeCompo] and idofs is [1, ..., spaceCompo * timeCompo].
!
! The arguments storageFMT, isSpaceCompo, isSpaceCompoScalar,
! isTimeCompo, isTimeCompoScalar, tPhysicalVarNames, physicalVarNames,
! isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes,
! isTNodesScalar and tSize are not used.
!
! Initiate(obj2) makes a copy of another field ([[obj_Initiate1]]); the
! example shows both.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE STVectorField_Class, ONLY: STVectorField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STVectorField_) :: u, v, w
! CHARACTER(1) :: names(1)
! REAL(DFP) :: found(24)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! ! Test 1: ImportFromToml
! CALL OK(u%IsInitiated(), "ImportFromToml: IsInitiated")
! CALL OK(u%spaceCompo .EQ. 2, "ImportFromToml: spaceCompo")
! CALL OK(u%timeCompo .EQ. 2, "ImportFromToml: timeCompo")
! CALL OK(u%SIZE() .EQ. 24, "ImportFromToml: Size = 24")
!
! ! Test 2: Initiate by arguments
! CALL v%Initiate(name="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
!                 fedof=fedof, geofedof=geofedof, timefedof=timefedof)
! CALL OK(v%IsInitiated(), "Initiate: IsInitiated")
! CALL OK(v%spaceCompo .EQ. 2, "Initiate: spaceCompo")
! CALL OK(v%timeCompo .EQ. timefedof%GetTotalDOF(), &
!         "Initiate: timeCompo = total time DOF")
! CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF() * 2 * 2, &
!         "Initiate: Size = space DOF * spaceCompo * timeCompo")
! CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "GetTotalPhysicalVars")
! CALL v%GetPhysicalNames(names)
! CALL OK(names(1) .EQ. "v", "GetPhysicalNames")
! CALL OK(ALL(v%GetSpaceCompo(1) .EQ. [2]), "GetSpaceCompo")
! CALL OK(ALL(v%GetTimeCompo(1) .EQ. [2]), "GetTimeCompo")
! CALL OK(v%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
!         "GetStorageFMT")
!
! ! Test 3: copy
! CALL v%Set(VALUE=3.0_DFP)
! CALL w%Initiate(obj2=v)
! CALL OK(w%spaceCompo .EQ. 2, "Initiate(obj2): spaceCompo")
! CALL OK(w%timeCompo .EQ. 2, "Initiate(obj2): timeCompo")
! CALL w%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(tsize .EQ. 24, "Initiate(obj2): Size")
! CALL OK(ALL(ABS(found - 3.0_DFP) .LT. tol), "Initiate(obj2): values")
!
! ! Test 4: DEALLOCATE
! CALL w%DEALLOCATE()
! CALL OK(.NOT. w%IsInitiated(), "DEALLOCATE: not initiated")
! CALL OK(w%spaceCompo .EQ. 0, "DEALLOCATE: spaceCompo = 0")
! CALL OK(w%timeCompo .EQ. 0, "DEALLOCATE: timeCompo = 0")
!
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate1]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2( &
    obj, name, engine, fieldType, storageFMT, comm, local_n, global_n, &
    spaceCompo, isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo, &
    isTimeCompoScalar, tPhysicalVarNames, physicalVarNames, &
    isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes, &
    isTNodesScalar, tSize, fedof, geofedof, timefedof)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field, needed
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine, needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL, needed
    !! following options are available FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: storageFMT
    !! storage format of the Vector field
    !! Not required.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! space components
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo, NOT REQUIRED
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1), NOT REQUIRED
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! Time components, needed for space-time field
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    !! Not required, Not needed
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1), Not needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true, Not required
    CHARACTER(*), OPTIONAL, INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables, NOT REQUIRED
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    !! NOT REQUIRED
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNamesScalar
    !! if true then physicalVarNames is scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tNodes(:)
    !! total number of nodes in each physical variable
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTNodes
    !! if true we will try to access tNodes
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTNodesScalar
    !! is tNodes scalar
    !! Not required
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSize
    !! total size of node field
    !! not required
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof, geofedof
    !! FEDOF object
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(IN) :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate2
END INTERFACE

INTERFACE STVectorFieldInitiate
  MODULE PROCEDURE obj_Initiate2
END INTERFACE STVectorFieldInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Deallocate the data stored in the field
!
! spaceCompo and timeCompo are set to zero, idofs, space_idofs and
! time_idofs are deallocated, and the data of AbstractNodeField_ is
! deallocated.
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE STVectorFieldDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE STVectorFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-25
! summary: Deallocate a vector of STVectorFieldPointer_
!
! Each associated pointer is deallocated and nullified (the targets are
! not freed), then obj is deallocated.
!
! See also: [[obj_STVectorFieldSafeAllocate1]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(STVectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of space-time vector field pointers
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE

INTERFACE STVectorFieldDeallocate
  MODULE PROCEDURE obj_Deallocate_ptr_vector
END INTERFACE STVectorFieldDeallocate

!----------------------------------------------------------------------------
!                                STVectorFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-25
! summary: Safely allocate a vector of STVectorFieldPointer_
!
! If obj is not allocated, it is allocated with newsize. If its size is
! less than newsize, it is deallocated with STVectorFieldDeallocate and
! allocated with newsize. Otherwise nothing is done.
!
! See also: [[obj_Deallocate_ptr_vector]]

INTERFACE
  MODULE SUBROUTINE obj_STVectorFieldSafeAllocate1(obj, newsize)
    TYPE(STVectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! allocatable STVector field pointer
    INTEGER(I4B), INTENT(IN) :: newsize
    !! new size of obj
  END SUBROUTINE obj_STVectorFieldSafeAllocate1
END INTERFACE

INTERFACE STVectorFieldSafeAllocate
  MODULE PROCEDURE obj_STVectorFieldSafeAllocate1
END INTERFACE STVectorFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Finalizer of the field
!
! Calls [[obj_Deallocate]].

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-26
! summary: Display the content of the field
!
! Displays the data of AbstractNodeField_, spaceCompo and timeCompo.
!
! Examples
!
!```fortran
! TYPE(STVectorField_) :: u
! CALL u%Display("u = ")
!```
!
! Full example: src/modules/STVectorField/examples/Initiate_test_1.F90

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CHARACTER(*), INTENT(IN) :: msg
    !! message to display first
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE STVectorFieldDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE STVectorFieldDisplay

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Import the field from an HDF5 file
!
! Reads the data of AbstractNodeField_ from group, then spaceCompo and
! timeCompo. The field must have been written by [[obj_Export]].
!
! See also: [[obj_Export]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof, geofedof
    !! FEDOF of the space and of the geometry
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
    !! FEDOFs of the space and of the geometry (several physical variables)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    !! TimeFEDOF of the time
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
    !! TimeFEDOFs of the time (several physical variables)
  END SUBROUTINE obj_Import
END INTERFACE

INTERFACE STVectorFieldImport
  MODULE PROCEDURE obj_Import
END INTERFACE STVectorFieldImport

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Export the field to an HDF5 file
!
! Writes the data of AbstractNodeField_ in group, and
! group/IntR0/timeCompo and group/IntR0/spaceCompo. Nothing is written
! if the field is not initiated.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE STVectorField_Class, ONLY: STVectorField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
! CHARACTER(*), PARAMETER :: h5FileName = "Export_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STVectorField_) :: u, v
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: found(24), found2(24)
! INTEGER(I4B) :: tsize, ii, s, t
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
! DO t = 1, 2
!   DO s = 1, 2
!     CALL u%Set(VALUE=[(100.0_DFP * s + 10.0_DFP * t + ii, ii=1, 6)], &
!                spaceCompo=s, timeCompo=t)
!   END DO
! END DO
!
! ! Export
! CALL h5%Initiate(h5FileName, mode="NEW")
! CALL h5%OPEN()
! CALL u%Export(hdf5=h5, group="u")
! CALL h5%DEALLOCATE()
! CALL OK(.TRUE., "Export")
!
! ! Import
! CALL h5%Initiate(h5FileName, mode="READ")
! CALL h5%OPEN()
! CALL v%IMPORT(hdf5=h5, group="u", fedof=fedof, geofedof=geofedof, &
!               timefedof=timefedof)
! CALL h5%DEALLOCATE()
!
! CALL OK(v%IsInitiated(), "Import: IsInitiated")
! CALL OK(v%spaceCompo .EQ. 2, "Import: spaceCompo")
! CALL OK(v%timeCompo .EQ. 2, "Import: timeCompo")
! CALL OK(v%SIZE() .EQ. 24, "Import: Size")
! CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL v%GetMultiple(istart=1, iend=24, stride=1, VALUE=found2, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - found2) .LT. tol), "Import: values")
! CALL v%Get(VALUE=found2, tsize=tsize, spaceCompo=2, timeCompo=1)
! CALL OK(ALL(ABS(found2(1:6) - found(7:12)) .LT. tol), &
!         "Import: Get component")
!
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]]

INTERFACE STVectorFieldExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE STVectorFieldExport

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the space-time values of one node
!
! VALUE(s, t) is set to space component s and time component t of
! globalNode, so SIZE(VALUE, 1) = spaceCompo and SIZE(VALUE, 2) =
! timeCompo (checked in debug mode). globalNode is a local node number;
! islocal is not used.
!
! Without addContribution the entries become scale * VALUE; with
! addContribution = .TRUE. scale * VALUE is added. The same rule holds
! for all Set methods.
!
! The example also shows Set(VALUE) for all entries ([[obj_Set11]]),
! Set(VALUE(:, :)) for all nodes ([[obj_Set2]]) and one entry
! ([[obj_Set9]]).
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE STVectorField_Class, ONLY: STVectorField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STVectorField_) :: u
! REAL(DFP) :: found(24), mat(2, 2), val
! INTEGER(I4B) :: tsize, nrow, ncol
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! ! Test 1: all entries to one value
! BLOCK
!   CALL u%Set(VALUE=1.0_DFP)
!   CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Set all")
! END BLOCK
!
! ! Test 2: the same space-time matrix at every node
! BLOCK
!   mat = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
!   CALL u%Set(VALUE=mat)
!   CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found(1:6) - 1.0_DFP) .LT. tol) .AND. &
!           ALL(ABS(found(7:12) - 2.0_DFP) .LT. tol) .AND. &
!           ALL(ABS(found(13:18) - 3.0_DFP) .LT. tol) .AND. &
!           ALL(ABS(found(19:24) - 4.0_DFP) .LT. tol), &
!           "Set matrix at all nodes: DOF format")
! END BLOCK
!
! ! Test 3: space-time matrix of one node
! BLOCK
!   mat = RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP], [2, 2])
!   CALL u%Set(globalNode=2, islocal=math%yes, VALUE=mat)
!   CALL u%Get(VALUE=mat, nrow=nrow, ncol=ncol, globalNode=2, &
!              islocal=math%yes)
!   CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "Get node: nrow, ncol")
!   CALL OK(ALL(ABS(mat - RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, &
!                                  8.0_DFP], [2, 2])) .LT. tol), &
!           "Set node")
!   ! node 2 is entry 2, 8, 14, 20
!   CALL u%GetMultiple(indx=[2, 8, 14, 20], VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found(1:4) - [5.0_DFP, 6.0_DFP, 7.0_DFP, &
!                                 8.0_DFP]) .LT. tol), &
!           "Set node: DOF format")
!
!   mat = 1.0_DFP
!   CALL u%Set(globalNode=2, islocal=math%yes, VALUE=mat, &
!              scale=2.0_DFP, addContribution=math%yes)
!   CALL u%Get(VALUE=mat, nrow=nrow, ncol=ncol, globalNode=2, &
!              islocal=math%yes)
!   CALL OK(ALL(ABS(mat - RESHAPE([7.0_DFP, 8.0_DFP, 9.0_DFP, &
!                                  10.0_DFP], [2, 2])) .LT. tol), &
!           "Set node: add 2 * value")
! END BLOCK
!
! ! Test 4: one entry
! BLOCK
!   CALL u%Set(VALUE=9.0_DFP, globalNode=3, islocal=math%yes, &
!              spaceCompo=2, timeCompo=1)
!   CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, spaceCompo=2, &
!              timeCompo=1)
!   CALL OK(ABS(val - 9.0_DFP) .LT. tol, "Set single")
!   CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, spaceCompo=1, &
!              timeCompo=1)
!   CALL OK(ABS(val - 1.0_DFP) .LT. tol, "Set single: other component")
!   CALL u%Set(VALUE=1.0_DFP, globalNode=3, islocal=math%yes, &
!              spaceCompo=2, timeCompo=1, addContribution=math%yes)
!   CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, spaceCompo=2, &
!              timeCompo=1)
!   CALL OK(ABS(val - 10.0_DFP) .LT. tol, "Set single: add")
! END BLOCK
!
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Get6]], [[obj_Set6]], [[obj_Set7]]

INTERFACE
  MODULE SUBROUTINE obj_Set1( &
    obj, globalNode, islocal, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! space-time nodal values at global node
    !! number of rows in value is equal to obj%spaceCompo
    !! number of columns in value is equal to obj%timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set every node to the same space-time values
!
! VALUE(s, t) is set to space component s and time component t of all
! nodes. SIZE(VALUE, 1) = spaceCompo and SIZE(VALUE, 2) = timeCompo
! (checked in debug mode).
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! space-time values, SIZE(VALUE) = (spaceCompo, timeCompo)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, scale * value is added, otherwise set
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one space-time component of all nodes to a value
!
! Space component spaceCompo and time component timeCompo of all nodes
! are set to VALUE.
!
! The example also shows [[obj_Set5]], [[obj_Set12]] and [[obj_Set13]].
!
! Examples
!
!```fortran
! TYPE(STVectorField_) :: u
! REAL(DFP) :: a(6), m(2, 6), found(6)
! INTEGER(I4B) :: tsize
! ! u: spaceCompo = 2, timeCompo = 2, six nodes
! CALL u%Set(VALUE=4.0_DFP, spaceCompo=2, timeCompo=1)
! CALL u%Set(VALUE=a, spaceCompo=1, timeCompo=2)
! CALL u%Set(VALUE=m, spaceCompo=[1, 2], timeCompo=2, &
!            storageFMT=NODES_FMT)
! CALL u%Set(VALUE=m, spaceCompo=2, timeCompo=[1, 2], &
!            storageFMT=NODES_FMT)
! CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
!```
!
! Full example: src/modules/STVectorField/examples/Set_test_2.F90
!
! See also: [[obj_Get1]]

INTERFACE
  MODULE SUBROUTINE obj_Set3( &
    obj, VALUE, spaceCompo, timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE
    !! obj = value
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all values from a rank 3 array
!
! VALUE(s, t, i) is set to space component s and time component t of
! node i. SIZE(VALUE) must be (spaceCompo, timeCompo, tNodes) (checked
! in debug mode).
!
! The example also shows [[obj_Set6]], [[obj_Set7]], [[obj_Set8]],
! [[obj_Get2]], [[obj_Get3]] and [[obj_Get4]].
!
! Examples
!
!```fortran
! TYPE(STVectorField_) :: u
! REAL(DFP) :: v3(2, 2, 6), w3(2, 2, 3), mat(2, 2), found(6)
! INTEGER(I4B) :: dim1, dim2, dim3, tsize
! ! u: spaceCompo = 2, timeCompo = 2, six nodes
! CALL u%Set(VALUE=v3)
! CALL u%Get(VALUE=v3, storageFMT=NODES_FMT, dim1=dim1, dim2=dim2, &
!            dim3=dim3)
! CALL u%Set(VALUE=mat, globalNode=[2, 5], islocal=.TRUE.)
! CALL u%Set(globalNode=[6, 4, 1], islocal=.TRUE., VALUE=w3)
! CALL u%Get(VALUE=w3, storageFMT=NODES_FMT, globalNode=[6, 4, 1], &
!            islocal=.TRUE., dim1=dim1, dim2=dim2, dim3=dim3)
! CALL u%Set(VALUE=[7.0_DFP, 8.0_DFP], globalNode=[3, 5], &
!            islocal=.TRUE., spaceCompo=2, timeCompo=2)
! CALL u%Get(VALUE=found, tsize=tsize, globalNode=[3, 5], &
!            islocal=.TRUE., spaceCompo=2, timeCompo=2)
!```
!
! Full example: src/modules/STVectorField/examples/Set_test_3.F90
!
! See also: [[obj_Get2]]

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    !! number of first dim in value is equal to obj%spaceCompo
    !! number of second dim in value is equal to obj%timeCompo
    !! number of 3rd dimension is equal to total number of nodes
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, scale * value is added, otherwise set
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one space-time component of all nodes from a vector
!
! VALUE(i) is set to space component spaceCompo and time component
! timeCompo of node i. SIZE(VALUE) is the total number of nodes.
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set5( &
    obj, VALUE, spaceCompo, timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! nodal values of space-time component
    !! the size should be equal to the total number of nodes
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the same space-time values at the selected nodes
!
! VALUE(s, t) is set to space component s and time component t of each
! node in globalNode. SIZE(VALUE, 1) = spaceCompo and SIZE(VALUE, 2) =
! timeCompo (checked in debug mode).
!
! See also: [[obj_Set4]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set6( &
    obj, VALUE, globalNode, islocal, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! space-time nodal value which will be assigned to globalNode
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, scale * value is added, otherwise set
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected nodes from a rank 3 array
!
! VALUE(s, t, j) is set to space component s and time component t of
! node globalNode(j). SIZE(VALUE) must be (spaceCompo, timeCompo,
! SIZE(globalNode)) (checked in debug mode).
!
! See also: [[obj_Set4]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set7( &
    obj, globalNode, islocal, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    !! space-time nodal values
    !! number of rows in value is equal to obj%spaceCompo
    !! number of cols in value is equal to obj%timeCompo
    !! number of 3rd dimension is equal to size of globalNode
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one space-time component of the selected nodes
!
! VALUE(j) is set to space component spaceCompo and time component
! timeCompo of node globalNode(j). SIZE(VALUE) = SIZE(globalNode)
! (checked in debug mode).
!
! See also: [[obj_Set4]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set8( &
    obj, VALUE, globalNode, islocal, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! nodal values of space-time component
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component of obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, scale * value is added, otherwise set
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one space-time component of one node
!
! VALUE is set to space component spaceCompo and time component
! timeCompo of globalNode.
!
! See also: [[obj_Set1]] (contains an example), [[obj_Get5]]

INTERFACE
  MODULE SUBROUTINE obj_Set9( &
    obj, VALUE, globalNode, islocal, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE
    !! obj = value
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global opr local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component of obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, scale * value is added, otherwise set
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected nodes from a FEVariable
!
! VALUE must be a space-time FEVariable (other types raise an error in
! debug mode). Its rank 3 data is passed to [[obj_Set7]], so it is read
! as (spaceCompo, timeCompo, SIZE(globalNode)).
!
! Note: [[obj_Get7]] returns a FEVariable of shape (spaceCompo, nns,
! nnt), which does not fit this layout; see the example.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: NODES_FMT
! USE STVectorField_Class, ONLY: STVectorField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STVectorField_) :: u, v
! TYPE(FEVariable_) :: fevar, fevar2
! REAL(DFP) :: v3(2, 2, 6), a3(2, 2, 3), b3(2, 2, 3), found(24), &
!              found2(24)
! INTEGER(I4B) :: ii, jj, kk, dim1, dim2, dim3, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! DO kk = 1, 6
!   DO jj = 1, 2
!     DO ii = 1, 2
!       v3(ii, jj, kk) = 100.0_DFP * ii + 10.0_DFP * jj + kk
!     END DO
!   END DO
! END DO
! CALL u%Set(VALUE=v3)
!
! ! Test 1: FEVariable round trip
! BLOCK
!   CALL u%Get(VALUE=fevar, globalNode=[1, 5, 2], islocal=math%yes)
!   CALL u%GetFEVariable(VALUE=fevar2, globalNode=[1, 5, 2], &
!                        islocal=math%yes)
!   CALL u%Set(VALUE=fevar, globalNode=[3, 6, 4], islocal=math%yes)
!   CALL u%Get(VALUE=a3, storageFMT=NODES_FMT, globalNode=[1, 5, 2], &
!              islocal=math%yes, dim1=dim1, dim2=dim2, dim3=dim3)
!   CALL u%Get(VALUE=b3, storageFMT=NODES_FMT, globalNode=[3, 6, 4], &
!              islocal=math%yes, dim1=dim1, dim2=dim2, dim3=dim3)
!   CALL OK(ALL(ABS(a3 - b3) .LT. tol), &
!           "Set(FEVariable) from Get(FEVariable)")
!   CALL OK(ALL(ABS(a3(:, :, 2) - v3(:, :, 5)) .LT. tol), &
!           "Set(FEVariable): source nodes unchanged")
! END BLOCK
!
! ! Test 2: GetFEVariable gives the same FEVariable as Get
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%Set(VALUE=fevar2, globalNode=[3, 6, 4], islocal=math%yes)
!   CALL u%Get(VALUE=b3, storageFMT=NODES_FMT, globalNode=[3, 6, 4], &
!              islocal=math%yes, dim1=dim1, dim2=dim2, dim3=dim3)
!   CALL OK(ALL(ABS(a3 - b3) .LT. tol), "GetFEVariable")
! END BLOCK
!
! ! Test 3: copy another field
! BLOCK
!   CALL u%Set(VALUE=v3)
!   CALL v%Initiate(obj2=u)
!   CALL v%Set(VALUE=0.0_DFP)
!   CALL v%Set(VALUE=u)
!   CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL v%GetMultiple(istart=1, iend=24, stride=1, VALUE=found2, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - found2) .LT. tol), "Set(STVectorField_)")
! END BLOCK
!
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Get7]], [[obj_GetFeVariable]], [[obj_Set14]]

INTERFACE
  MODULE SUBROUTINE obj_Set10( &
    obj, VALUE, globalNode, islocal, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! FE variable
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all values to one value
!
! Calls SetAll of AbstractNodeField_.
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE
    !! Set all value to a constant value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, scale * value is added, otherwise set
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-10
! summary: Set several space components of one time component
!
! Sets space components spaceCompo(:) at time component timeCompo of all
! nodes. With storageFMT = NODES_FMT, VALUE(j, i) is component
! spaceCompo(j) of node i, so VALUE is (SIZE(spaceCompo), tNodes); with
! DOF_FMT VALUE is the transpose, (tNodes, SIZE(spaceCompo)). The sizes
! are checked in debug mode.
!
! See also: [[obj_Set3]] (contains an example), [[obj_Set13]]

INTERFACE
  MODULE SUBROUTINE obj_Set12( &
    obj, VALUE, spaceCompo, timeCompo, storageFMT, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! If storage is NODES_FMT
    !! The number of rows equal to the size of spaceCompo
    !! The number of columns equal to total number of nodes in domain
    !! each row represents a space components
    !! If storage is DOF_FMT then transpose of the above
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! Several space components
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! Several time components
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! Storage format of value
    !! NODES_FMT: row is space component and column is nodes
    !! DOF_FMT: row is nodes and column is space component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! Scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true,then we add instead of set
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-10
! summary: Set several time components of one space component
!
! Sets time components timeCompo(:) of space component spaceCompo of all
! nodes. With storageFMT = NODES_FMT, VALUE(j, i) is time component
! timeCompo(j) of node i, so VALUE is (SIZE(timeCompo), tNodes); with
! DOF_FMT VALUE is the transpose, (tNodes, SIZE(timeCompo)). The sizes
! are checked in debug mode.
!
! See also: [[obj_Set3]] (contains an example), [[obj_Set12]]

INTERFACE
  MODULE SUBROUTINE obj_Set13( &
    obj, VALUE, spaceCompo, timeCompo, storageFMT, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! The numbe rows in value should be equal to the size of timeCompo
    !! The number of columns in value should be equal to the
    !! total number of  nodes in domain.
    !! The ith row of value denotes the nodal value of
    !! timeCompo(i) and spaceCompo.
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! several time components
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of value
    !! NODES_FMT: row is time component and column is nodes
    !! DOF_FMT: row is nodes and column is time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !!  Scalae, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! Add contribution
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Copy another space-time vector field
!
! If VALUE is of type STVectorField_, obj%Copy(VALUE) is called; other
! types raise an error in debug mode.
!
! See also: [[obj_Set10]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set14(obj, VALUE)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CLASS(STVectorField_), INTENT(IN) :: VALUE
    !! space-time vector field to copy
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

! authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all nodal values of a given space-time component
!
!# Introduction
!
! This routine Sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a vector of scalars. These vectors of scalar are stored
! inside a scalar field called `value`. Note that the size of `value` should
! be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, : ) = value

! INTERFACE
!   MODULE SUBROUTINE obj_Set14( &
!     obj, VALUE, spaceCompo, timeCompo, scale, addContribution)
!     CLASS(STVectorField_), INTENT(INOUT) :: obj
!     !! obj@[spaceCompo, timeCompo] = value (value is scalar field)
!     !! obj@[spaceCompo, timeCompo] = value@timeCompo (value is st scalar)
!     !! obj@[spaceCompo, timeCompo] = value@[spaceCompo, timeCompo]
!     CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
!     !! Instance of abstract node field which contains
!     !! the nodal values of space-time component
!     INTEGER(I4B), INTENT(IN) :: spaceCompo
!     !! space component of obj
!     INTEGER(I4B), INTENT(IN) :: timeCompo
!     !! time component in obj
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     !! scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!     !! add or set
!   END SUBROUTINE obj_Set14
! END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

! authors: Vikas Sharma, Ph. D.
! date: 2024-06-08
! summary: Set values
!
!# Introduction
!
! obj@[ivar, idof] = value@[ivar_value, idof_value]
!
! value can be any field. The conversion rule are defined as follows:
!
! If value is scalr, vector stscalr or stvector field then
! We first get the pointer to the value by calling GetPointer method
! Then we call setMultiple method on obj to set the multiple values
! using the range.

! INTERFACE
!   MODULE SUBROUTINE obj_Set16( &
!     obj, ivar, idof, VALUE, ivar_value, idof_value, scale, addContribution)
!     CLASS(STVectorField_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: ivar
!     !! physical variable in obj
!     INTEGER(I4B), INTENT(IN) :: idof
!     !! local degree of freedom in obj
!     CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
!     !! obj = value
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     !! physical variable in value
!     INTEGER(I4B), INTENT(IN) :: idof_value
!     !! local degree of freedom in value
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!   END SUBROUTINE obj_Set16
! END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-24
! summary: Set one time component from a vector field
!
! For each space component s, component s of VALUE is set to space
! component s and time component timeCompo of obj. VALUE must have
! obj%spaceCompo components and the same DOFs as obj.
!
! VALUE can use any engine: with a native engine (NATIVE_*) its Fortran
! vector is used directly (GetPointer); otherwise, for example a
! VectorFieldLis_, all its values are first read into a temporary array
! by VALUE%GetMultiple (tested in
! src/modules/STVectorField/examples/SetFromVectorField_test_2.F90).
!
! Changes:
!   - 2026-11-14: VALUE can be a VectorFieldLis_ (before, GetPointer
!     returned NULL and SetMultiple dereferenced it).
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE STVectorField_Class, ONLY: STVectorField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! USE VectorField_Class, ONLY: VectorField_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STVectorField_) :: u
! TYPE(VectorField_) :: v, w
! REAL(DFP) :: a(6), b(6), found(12)
! INTEGER(I4B) :: tsize, ii
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
! CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
!                 fedof=fedof, geofedof=geofedof)
! CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", spaceCompo=[2], &
!                 fedof=fedof, geofedof=geofedof)
!
! a = [(REAL(ii, DFP), ii=1, 6)]
! b = 10.0_DFP * a
! CALL v%Set(VALUE=a, spaceCompo=1)
! CALL v%Set(VALUE=b, spaceCompo=2)
!
! ! Test 1: SetFromVectorField
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%SetFromVectorField(VALUE=v, timeCompo=2)
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - a) .LT. tol), "SetFromVectorField: x")
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - b) .LT. tol), "SetFromVectorField: y")
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6)) .LT. tol), &
!           "SetFromVectorField: time 1 unchanged")
!
!   CALL u%SetFromVectorField(VALUE=v, timeCompo=2, scale=2.0_DFP, &
!                             addContribution=math%yes)
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * b) .LT. tol), &
!           "SetFromVectorField: add 2 * value")
! END BLOCK
!
! ! Test 2: SetToVectorField
! BLOCK
!   CALL w%Set(VALUE=0.0_DFP)
!   CALL u%SetToVectorField(VALUE=w, timeCompo=2)
!   CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * a) .LT. tol), &
!           "SetToVectorField: x")
!   CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * b) .LT. tol), &
!           "SetToVectorField: y")
!
!   CALL u%SetToVectorField(VALUE=w, timeCompo=1, scale=1.0_DFP, &
!                           addContribution=math%yes)
!   CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - 3.0_DFP * a) .LT. tol), &
!           "SetToVectorField: add time 1 (zero)")
! END BLOCK
!
! CALL w%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetToVectorField]]

INTERFACE
  MODULE SUBROUTINE obj_SetFromVectorField( &
    obj, VALUE, timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CLASS(VectorField_), INTENT(IN) :: VALUE
    !! vector field, any engine (native or LIS)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component of vector field
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, scale * value is added, otherwise set
  END SUBROUTINE obj_SetFromVectorField
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-11
! summary: Set a vector field from one time component
!
! For each space component s, space component s and time component
! timeCompo of obj is set to component s of VALUE. VALUE must have
! obj%spaceCompo components and the same DOFs as obj.
!
! See also: [[obj_SetFromVectorField]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_SetToVectorField(obj, VALUE, timeCompo, &
                                         scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CLASS(VectorField_), INTENT(INOUT) :: VALUE
    !! vector field, set from time component timeCompo of obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component of obj, at most obj%timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, scale * value is added, otherwise set
  END SUBROUTINE obj_SetToVectorField
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-06
! summary: Set the values from a user function
!
! func is a vector function of (x, y, z, t) with spaceCompo values. For
! each node i of the mesh, the coordinates of the node are passed to
! func:
!
! - with times, SIZE(times) = timeCompo (checked in debug mode), and time
!   component a of node i is set to func(x_i, times(a)),
! - without times, t = 0 and all time components of node i are set to
!   func(x_i, 0).
!
! Only the mesh nodes are set, so this is correct for order 1 Lagrange in
! space (DOF number = node number); the other DOFs of higher orders are
! not set. ivar, idof, spaceCompo and timeCompo are not used.
!
! Examples
!
!```fortran
! TYPE(STVectorField_) :: u
! TYPE(UserFunction_) :: func
! ! func: vector function, numArgs = 4, numReturns = spaceCompo
! CALL u%SetByFunction(func=func, times=[0.0_DFP, 1.0_DFP])
! CALL u%SetByFunction(func=func)
!```
!
! Full example: src/modules/STVectorField/examples/SetByFunction_test_1.F90

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction( &
    obj, func, times, ivar, idof, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CLASS(UserFunction_), INTENT(INOUT) :: func
      !! User function
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! If present then its size should be 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! ivar (not used)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
    !! idof (not used)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space component, not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
    !! time component, not used
  END SUBROUTINE obj_SetByFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-06
! summary: Get the values of a node, a component, or both
!
! The optional arguments that are present select the values:
!
! - globalNode, spaceCompo, timeCompo: one value (tsize = 1),
! - globalNode: all values of the node, (s, t) = (1, 1), (2, 1), ...,
!   (spaceCompo, timeCompo) (tsize = spaceCompo * timeCompo),
! - globalNode, spaceCompo: the time components of (node, s)
!   (tsize = timeCompo),
! - globalNode, timeCompo: the space components of (node, t)
!   (tsize = spaceCompo),
! - spaceCompo, timeCompo: component (s, t) of all nodes (tsize = tNodes),
! - spaceCompo: all time components of s, time 1 first
!   (tsize = tNodes * timeCompo),
! - timeCompo: all space components of t, space 1 first
!   (tsize = tNodes * spaceCompo).
!
! At least one of them must be present. globalNode is a local node
! number.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE STVectorField_Class, ONLY: STVectorField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STVectorField_) :: u
! REAL(DFP) :: found(24), want(12)
! INTEGER(I4B) :: tsize, ii, s, t
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! DO t = 1, 2
!   DO s = 1, 2
!     CALL u%Set(VALUE=[(f(s, t, ii), ii=1, 6)], spaceCompo=s, timeCompo=t)
!   END DO
! END DO
!
! ! Test 1: node, space and time
! CALL u%Get(VALUE=found, tsize=tsize, globalNode=3, spaceCompo=2, &
!            timeCompo=1)
! CALL OK(tsize .EQ. 1 .AND. ABS(found(1) - f(2, 1, 3)) .LT. tol, &
!         "node, space, time")
!
! ! Test 2: node
! CALL u%Get(VALUE=found, tsize=tsize, globalNode=3)
! CALL OK(tsize .EQ. 4, "node: tsize")
! CALL OK(ALL(ABS(found(1:4) - [f(1, 1, 3), f(2, 1, 3), f(1, 2, 3), &
!                               f(2, 2, 3)]) .LT. tol), "node: values")
!
! ! Test 3: node and space
! CALL u%Get(VALUE=found, tsize=tsize, globalNode=3, spaceCompo=2)
! CALL OK(tsize .EQ. 2, "node, space: tsize")
! CALL OK(ALL(ABS(found(1:2) - [f(2, 1, 3), f(2, 2, 3)]) .LT. tol), &
!         "node, space: values")
!
! ! Test 4: node and time
! CALL u%Get(VALUE=found, tsize=tsize, globalNode=3, timeCompo=1)
! CALL OK(tsize .EQ. 2, "node, time: tsize")
! CALL OK(ALL(ABS(found(1:2) - [f(1, 1, 3), f(2, 1, 3)]) .LT. tol), &
!         "node, time: values")
!
! ! Test 5: space and time
! CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
! CALL OK(tsize .EQ. 6, "space, time: tsize")
! CALL OK(ALL(ABS(found(1:6) - [(f(1, 2, ii), ii=1, 6)]) .LT. tol), &
!         "space, time: values")
!
! ! Test 6: space
! CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
! CALL OK(tsize .EQ. 12, "space: tsize")
! want = [[(f(1, 1, ii), ii=1, 6)], [(f(1, 2, ii), ii=1, 6)]]
! CALL OK(ALL(ABS(found(1:12) - want) .LT. tol), "space: values")
!
! ! Test 7: time
! CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
! CALL OK(tsize .EQ. 12, "time: tsize")
! want = [[(f(1, 2, ii), ii=1, 6)], [(f(2, 2, ii), ii=1, 6)]]
! CALL OK(ALL(ABS(found(1:12) - want) .LT. tol), "time: values")
!
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
!
! CONTAINS
!
! PURE FUNCTION f(sc, tc, nd) RESULT(ans)
!   INTEGER(I4B), INTENT(IN) :: sc, tc, nd
!   REAL(DFP) :: ans
!   ans = 100.0_DFP * sc + 10.0_DFP * tc + nd
! END FUNCTION f
!
! END PROGRAM main
!```
!
! See also: [[obj_Set3]], [[obj_Get4]]

INTERFACE
  MODULE SUBROUTINE obj_Get1( &
    obj, VALUE, tsize, globalNode, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! value = obj
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in value
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    !! local node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
    !! time component
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get all values in a rank 3 array
!
! With storageFMT = NODES_FMT, VALUE(s, t, i) is space component s and
! time component t of node i, and (dim1, dim2, dim3) = (spaceCompo,
! timeCompo, tNodes). With DOF_FMT, VALUE(i, s, t) holds the same value
! and (dim1, dim2, dim3) = (tNodes, spaceCompo, timeCompo).
!
! See also: [[obj_Set4]] (contains an example), [[obj_Get3]]

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, storageFMT, dim1, dim2, dim3)
    CLASS(STVectorField_), INTENT(IN) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :, :)
    !! values, see storageFMT
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of value
    !! NODES_FMT: dim1, dim2, dim3 => obj%spaceCompo, obj%timeCompo, tNodes
    !! DOF_FMT: dim1, dim2, dim3 => tNodes, obj%spaceCompo, obj%timeCompo
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! sizes of the data written in value, see storageFMT
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the values of the selected nodes in a rank 3 array
!
! Same as [[obj_Get2]] for the nodes in globalNode: with NODES_FMT,
! VALUE(s, t, j) is the value of node globalNode(j) and (dim1, dim2,
! dim3) = (spaceCompo, timeCompo, SIZE(globalNode)); with DOF_FMT,
! VALUE(j, s, t) and (dim1, dim2, dim3) = (SIZE(globalNode),
! spaceCompo, timeCompo).
!
! See also: [[obj_Set4]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get3( &
    obj, VALUE, storageFMT, globalNode, islocal, dim1, dim2, dim3)
    CLASS(STVectorField_), INTENT(IN) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :, :)
    !! returned value
    !! dim1 is space compoennt
    !! dim2 is time component
    !! dim3 is size of globalNode
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of value
    !! NODES_FMT: dim1, dim2, dim3 => spaceCompo, timeCompo, size(globalNode)
    !! DOF_FMT: dim1, dim2, dim3 => size(globalNode), spaceCompo, timeCompo
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local or global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = obj%spaceCompo
    !! dim2 = obj%timeCompo
    !! dim3 = size of globalNode
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get one space-time component of the selected nodes
!
! VALUE(j) is space component spaceCompo and time component timeCompo of
! node globalNode(j); tsize = SIZE(globalNode).
!
! See also: [[obj_Set4]] (contains an example), [[obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Get4( &
    obj, VALUE, tsize, globalNode, islocal, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! nodal values of space-time component
    !! size of value is same as size of global node
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    ! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true the global node number are local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get one space-time component of one node
!
! VALUE is space component spaceCompo and time component timeCompo of
! globalNode.
!
! See also: [[obj_Set1]] (contains an example), [[obj_Set9]]

INTERFACE
  MODULE SUBROUTINE obj_Get5( &
    obj, VALUE, globalNode, islocal, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! nodal value of space-time component
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the space-time values of one node
!
! VALUE(s, t) is space component s and time component t of globalNode;
! nrow = spaceCompo and ncol = timeCompo.
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, nrow, ncol, globalNode, islocal)
    CLASS(STVectorField_), INTENT(IN) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! space-time nodal values at global node
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in value
    !! nrow will be obj%spaceCompo
    !! ncol will be obj%timeCompo
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the values of the selected nodes in a FEVariable
!
! VALUE is a nodal, vector, space-time FEVariable of shape (spaceCompo,
! SIZE(globalNode), timeCompo): entry (s, j, t) is space component s and
! time component t of node globalNode(j).
!
! See also: [[obj_Set10]] (contains an example), [[obj_GetFeVariable]]

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE, globalNode, islocal)
    CLASS(STVectorField_), INTENT(IN) :: obj
    !! space-time vector field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! nodal, vector, space-time FEVariable (spaceCompo, nodes, timeCompo)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

! authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Get value in an instance of AbstractNodeField_
!
!# Introduction
!
! If spaceCompo and timeCompo are present then:
! - value = obj@[spaceCompo, timeCompo] (if value is scalar field)
! - value@timeCompo = obj@[spaceCompo, timeCompo] (if value is st scalar field)
! - value@spaceCompo = obj@[spaceCompo, timeCompo] (if value is vector field)
! - value@[spaceCompo, timeCompo] = obj@[spaceCompo, timeCompo]
!    (if value is st vector field)
!
! If spacecompo is present and timecompo is not present then:
!   value should be an instance of STScalarField or STVectorField
!   If value is ST scalar field:
!     value@timeCompo = obj@[spaceCompo, timeCompo] for all timeCompo
!   If value is ST vector field:
!     value@[spaceCompo, timeCompo]= obj@[spaceCompo, timeCompo]
!       for all timeCompo
!
! If spacecompo is not present and timecompo is present then:
!   value should an instance of VectorField_ and STVectorField_
!   If value is VectorField_:
!     value@spaceCompo = obj@[spaceCompo, timeCompo] for all spaceCompo
!   If value is STVectorField_:
!     value@[spaceCompom timeCompo] = obj@[spaceCompo, timeCompo]
!       for all spaceCompo
!
! If spacecompo and timecompo are not present then:
!   value should be an instance of STVectorField_
!   In this case; value = obj

! INTERFACE
!   MODULE SUBROUTINE obj_Get8(obj, VALUE, spaceCompo, timeCompo)
!     CLASS(STVectorField_), INTENT(IN) :: obj
!     !! value=obj
!     CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
!     !! value=obj
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
!     !!  space component
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
!     !! time component
!   END SUBROUTINE obj_Get8
! END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

! authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: REturns the value

! INTERFACE
!   MODULE SUBROUTINE obj_Get9( &
!     obj, ivar, idof, VALUE, ivar_value, idof_value)
!     CLASS(STVectorField_), INTENT(IN) :: obj
!     CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
!     INTEGER(I4B), INTENT(IN) :: ivar
!     INTEGER(I4B), INTENT(IN) :: idof
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     INTEGER(I4B), INTENT(IN) :: idof_value
!   END SUBROUTINE obj_Get9
! END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get the values of the selected nodes in a FEVariable
!
! Same as [[obj_Get7]]; ivar is not used.
!
! See also: [[obj_Set10]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(STVectorField_), INTENT(IN) :: obj
    !! space-time vector field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! FEvariable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical variable
  END SUBROUTINE obj_GetFeVariable
END INTERFACE

INTERFACE STVectorFieldGetFEVariable
  MODULE PROCEDURE obj_GetFeVariable
END INTERFACE STVectorFieldGetFEVariable

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a Dirichlet boundary condition
!
! The node numbers and the nodal values at the time nodes are obtained
! from dbc%Get with times, fedof, geofedof and timefedof: the values are
! (nodes, timeCompo). Space component dbc%GetDOFNo() and time component
! a of these nodes is set to the values of column a. The component must
! not be larger than spaceCompo (checked in debug mode).
!
! The example also shows [[obj_ApplyDirichletBC2]] and
! [[obj_ApplyDirichletBC3]].
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE DirichletBC_Class, ONLY: DirichletBCDeallocate
! USE DirichletBC_Class, ONLY: DirichletBCImportFromToml
! USE DirichletBC_Class, ONLY: DirichletBCPointer_
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE STVectorField_Class, ONLY: STVectorField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ApplyDirichletBC_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STVectorField_) :: u
! TYPE(DirichletBCPointer_), ALLOCATABLE :: extDBC(:)
! REAL(DFP) :: found(24), want(6)
! INTEGER(I4B) :: tsize, itime
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
! CALL DirichletBCImportFromToml(obj=extDBC, dom=dom, tomlName="extDBC", &
!                                filename=tomlFileName)
!
! ! Test 1: internal list of Dirichlet BCs
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%ApplyDirichletBC(times=times)
!   DO itime = 1, 2
!     want = [1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP]
!     CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=itime)
!     CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), &
!             "internal dbc: x, time "//CHAR(48 + itime))
!     want = [0.0_DFP, 2.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!     CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=itime)
!     CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), &
!             "internal dbc: y, time "//CHAR(48 + itime))
!   END DO
! END BLOCK
!
! ! Test 2: vector of DirichletBCPointer_
! BLOCK
!   CALL u%Set(VALUE=-1.0_DFP)
!   CALL u%ApplyDirichletBC(dbc=extDBC, times=times)
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) + 1.0_DFP) .LT. tol), &
!           "dbc vector: x unchanged")
!   want = [7.0_DFP, 7.0_DFP, -1.0_DFP, -1.0_DFP, 7.0_DFP, -1.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "dbc vector: y")
! END BLOCK
!
! ! Test 3: single DirichletBC_
! BLOCK
!   CALL u%Set(VALUE=3.0_DFP)
!   CALL u%ApplyDirichletBC(dbc=extDBC(1)%ptr, times=times)
!   want = [7.0_DFP, 7.0_DFP, 3.0_DFP, 3.0_DFP, 7.0_DFP, 3.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "single dbc: y")
! END BLOCK
!
! CALL DirichletBCDeallocate(extDBC)
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ApplyDirichletBC2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    !! Dirichlet boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times of the time nodes, SIZE(times) = timeCompo
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a list of Dirichlet boundary conditions
!
! Same as [[obj_ApplyDirichletBC1]] for each associated pointer of dbc.
!
! See also: [[obj_ApplyDirichletBC1]] (contains an example),
! [[obj_ApplyDirichletBC3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply the Dirichlet boundary conditions of the field
!
! Calls [[obj_ApplyDirichletBC2]] with obj%dbc.
!
! See also: [[obj_ApplyDirichletBC1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC3(obj, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: times(:)
    !! times
  END SUBROUTINE obj_ApplyDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyBodySource@NBCMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-11
! summary: Add the space-time body source given by a user function
!
! The time element on times (the time nodes) gives the time shape
! functions T_a and quadrature points. For each element, the body source
! is evaluated at the space-time quadrature points (x, y, z, t), and
!
!   obj(c, a, I) = obj(c, a, I) + scale * integral N_I T_a f_c
!
! is added for space component c, time component a and node I. The
! source has elemsd%nsd rows, so spaceCompo must be equal to the
! dimension of the mesh.
!
! Examples
!
!```fortran
! TYPE(STVectorField_) :: u
! TYPE(UserFunction_) :: func
! ! func: vector function, numArgs = 4 (x, y, z, t),
! ! numReturns = spaceCompo
! CALL u%Set(VALUE=0.0_DFP)
! CALL u%ApplyBodySource(bodySource=func, scale=1.0_DFP, &
!                        times=[0.0_DFP, 1.0_DFP])
!```
!
! Full example: src/modules/STVectorField/examples/ApplyBodySource_test_1.F90
!
! See also: [[obj_ApplyBodySource3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource1(obj, bodySource, scale, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! Body source user function
    !! It should be a vector function with
    !! total arguments 4 (x, y, z, time)
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
    REAL(DFP), INTENT(IN) :: times(:)
    !! time, which will be passed to the body source function
  END SUBROUTINE obj_ApplyBodySource1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyBodySource@NBCMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-11
! summary: Add the body source given by a space-time vector field
!
! Under development: raises an error in debug mode and does nothing.
!
! See also: [[obj_ApplyBodySource1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource2(obj, bodySource, scale)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! ST vector field
    !! The test function will be corresponding to the obj
    CLASS(STVectorField_), INTENT(INOUT) :: bodySource
    !! Body source in terms of vector field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
  END SUBROUTINE obj_ApplyBodySource2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyBodySource@NBCMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-11
! summary: Add the body source at one time given by a user function
!
! The body source is evaluated at the space quadrature points with
! t = times, and the element vector integral N_I f_c (spaceCompo, nns)
! is passed to Set with the element nodes. That Set expects the values of
! one node for all time components (spaceCompo, timeCompo), so this
! routine stops in debug mode when nns differs from timeCompo; it does
! not work yet.
!
! See also: [[obj_ApplyBodySource1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource3(obj, bodySource, scale, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! Body source user function
    !! It should be a vector function with
    !! total arguments 4 (x, y, z, time)
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
    REAL(DFP), INTENT(IN) :: times
    !! time, which will be passed to the body source function
    !! This time can also represent a quadrature point time
  END SUBROUTINE obj_ApplyBodySource3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ApplyNeumannBC@NBCMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-11
! summary:  Add Contribution of neumann boundary condition
!
! For each Neumann boundary condition of the field, nbcField is set to zero
! and only this boundary condition is applied to it as Dirichlet values
! (extension of its boundary data to the domain). Then the integral of the
! test functions times nbcField over the facets of this boundary condition and
! the time interval is computed, and scale times this integral is added to
! obj. So each facet gets only the data of its own boundary condition (in
! particular, x and y conditions on the same boundary are not added twice),
! and the contributions of several boundary conditions add up. Nothing is done
! if the field has no Neumann boundary condition.
!
! Changes:
!   - 2026-11-02: integrate each Neumann boundary condition with its own
!     data only (before, the data of all conditions was used)
!   - 2026-11-03: pass the nodal values to the force vector as
!     (spaceCompo, nns, nnt); before, space and time nodes were swapped
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDOF_Class, ONLY: FEDOF_
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE STVectorField_Class, ONLY: STVectorField_
!   USE Test_Method, ONLY: OK
!   USE TimeFEDOF_Class, ONLY: TimeFEDOF_
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: tomlFileName = &
!                              "ApplySurfaceNeumannBC_test_1.toml"
!   REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!   INTEGER(I4B), PARAMETER :: nodes(6) = [1, 2, 3, 4, 5, 6]
!   TYPE(FEDomain_) :: dom
!   TYPE(FEDOF_), TARGET :: fedof, geofedof
!   TYPE(TimeFEDOF_), TARGET :: timefedof
!   TYPE(STVectorField_) :: u, nbcField
!   REAL(DFP) :: val(6), expected(6)
!   INTEGER(I4B) :: tsize, itime
!   CHARACTER(1) :: ch
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
!   CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                         fedof=fedof, geofedof=geofedof, &
!                         timefedof=timefedof, dom=dom)
!   CALL nbcField%Initiate(obj2=u, copyFull=math%no, &
!                          copyStructure=math%yes, usePointer=math%yes)
!
!   CALL OK(u%GetTotalNBC() .EQ. 1, "GetTotalNBC")
!   CALL OK(u%timeCompo .EQ. 2, "timeCompo")
!
!   CALL u%SetAll(VALUE=0.0_DFP)
!   CALL u%ApplySurfaceNeumannBC(nbcField=nbcField, scale=1.0_DFP, &
!                                times=[0.0_DFP, 1.0_DFP])
!
!   ! node:       1             2             3        4        5
!   expected = [25.0_DFP / 12.0_DFP, 125.0_DFP / 12.0_DFP, 0.0_DFP, &
!               0.0_DFP, 12.5_DFP, 0.0_DFP]
!
!   DO itime = 1, 2
!     ch = CHAR(48 + itime)
!     CALL u%Get(VALUE=val, tsize=tsize, globalNode=nodes, &
!                islocal=math%yes, spaceCompo=1, timeCompo=itime)
!     CALL OK(ALL(ABS(val(1:tsize) - expected) .LT. tol), &
!             "x component, time component "//ch)
!     CALL u%Get(VALUE=val, tsize=tsize, globalNode=nodes, &
!                islocal=math%yes, spaceCompo=2, timeCompo=itime)
!     CALL OK(ALL(ABS(val(1:tsize)) .LT. tol), &
!             "y component, time component "//ch)
!   END DO
!
!   CALL nbcField%DEALLOCATE()
!   CALL u%DEALLOCATE()
!   CALL timefedof%DEALLOCATE()
!   CALL fedof%DEALLOCATE()
!   CALL geofedof%DEALLOCATE()
!   CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ApplyPointNeumannBC]]

INTERFACE
  MODULE SUBROUTINE obj_ApplySurfaceNeumannBC(obj, nbcField, scale, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! Vector field
    CLASS(STVectorField_), INTENT(INOUT) :: nbcField
    !! Vector field where we will keep the neumann boundary condition
    !! extension to the entire domain
    REAL(DFP), INTENT(IN) :: scale
    !! Scale for neumann boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times
  END SUBROUTINE obj_ApplySurfaceNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyPointNeumannBC@NBCMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-11
! summary: Add the point Neumann boundary conditions
!
! The time element on times gives the time mass matrix
! ct(a, b) = integral T_a T_b dt. For each point Neumann boundary
! condition of the field (obj%nbc_point), the node numbers and the nodal
! values at the time nodes f_b are obtained, and
!
!   obj(c, a, I) = obj(c, a, I) + scale * sum_b ct(a, b) f_b(I)
!
! is added for space component c = nbc%GetDOFNo(). Nothing is done if the
! field has no point Neumann boundary condition.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE STVectorField_Class, ONLY: STVectorField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = &
!                            "ApplyPointNeumannBC_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STVectorField_) :: u
! REAL(DFP) :: found(24), want(6)
! INTEGER(I4B) :: tsize, itime
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! ! Test 1: number of point Neumann BCs
! CALL OK(u%GetTotalPointNBC() .EQ. 2, "GetTotalPointNBC")
!
! ! Test 2: scale = 1
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%ApplyPointNeumannBC(scale=1.0_DFP, times=times)
!   DO itime = 1, 2
!     want = [1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!     CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=itime)
!     CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), &
!             "scale = 1: x, time "//CHAR(48 + itime))
!     want = [0.0_DFP, 0.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!     CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=itime)
!     CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), &
!             "scale = 1: y, time "//CHAR(48 + itime))
!   END DO
! END BLOCK
!
! ! Test 3: added to the current values, scale = 0.5
! BLOCK
!   CALL u%Set(VALUE=1.0_DFP)
!   CALL u%ApplyPointNeumannBC(scale=0.5_DFP, times=times)
!   want = [1.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "add with scale = 0.5: x")
!   want = [1.0_DFP, 1.0_DFP, 2.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "add with scale = 0.5: y")
! END BLOCK
!
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ApplySurfaceNeumannBC]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyPointNeumannBC(obj, scale, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! Vector field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for neumann boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times of the time nodes, SIZE(times) = timeCompo
  END SUBROUTINE obj_ApplyPointNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STVectorField_Class
