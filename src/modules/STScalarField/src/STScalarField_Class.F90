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
! summary: Space-time scalar field data type is defined

MODULE STScalarField_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: FEVariable_
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE DirichletBC_Class, ONLY: DirichletBC_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDOF_Class, ONLY: FEDOF_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP, I4B, LGT, NODES_FMT, NodesToDOF
USE HDF5File_Class, ONLY: HDF5File_
USE ScalarField_Class, ONLY: ScalarField_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeOpt_Class, ONLY: TimeOpt_
USE tomlf, ONLY: toml_table
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE
PRIVATE

INTEGER(I4B), PARAMETER :: MYCONVERSION = TypeFieldOpt%conversionNodesToDOF

PUBLIC :: STScalarField_
PUBLIC :: STScalarFieldPointer_
PUBLIC :: STScalarFieldDeallocate
PUBLIC :: STScalarFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                            STScalarField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Space-time scalar field
!
! A space-time scalar field has one physical variable with one space
! component and timeCompo time components. For a space-time finite
! element, timeCompo is the number of DOFs of the time element
! (timefedof); for example two for order 1 Lagrange in time.
!
! The values are stored in the vector of AbstractNodeField_ in the DOF
! storage format: first the values of all space DOFs of time component
! 1, then those of time component 2, and so on. The value of space DOF
! i (the node number for order 1 Lagrange) and time component a is
! entry (a - 1) * tNodes + i, where tNodes is the number of space DOFs
! of fedof. idofs = [1, ..., timeCompo] are the DOF numbers of the time
! components in the DOF object.
!
! The values are set by Set, SetByFunction, SetFromScalarField, the
! boundary condition methods (ApplyDirichletBC, ...) and the methods
! of AbstractNodeField_, and they are read by Get, GetFEVariable and
! SetToScalarField.
!
! Bindings without PUBLIC are private (PRIVATE after CONTAINS); the
! Set and Get generics are the API.
!
! See also: [[ScalarField_]], [[AbstractNodeField_]],
! [[STScalarFieldLis_]]

TYPE, EXTENDS(AbstractNodeField_) :: STScalarField_
  INTEGER(I4B), PUBLIC :: timeCompo = 0_I4B
  !! number of time components (DOFs of the time element)
  INTEGER(I4B), ALLOCATABLE :: idofs(:)
  !! DOF numbers of the time components, idofs = [1, ..., timeCompo]
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by copying another node field
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate an instance of STScalarField_ by passing arguments
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data stored inside the STScalarField_ object
  FINAL :: obj_Final
  !! Finalizer

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of STScalarField_
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the content of STScalarField_ from an HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the content of STScalarField_ to an HDF5 file

  ! SET:
  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set all time components of one node
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set time component a of all nodes to VALUE(a)
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set one time component of all nodes to one value
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set all entries from a rank-2 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set one time component of all nodes from a vector
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set one time component from a node field (not yet available)
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set all time components of the selected nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set8 => obj_Set8
  !! Set the selected nodes from a rank-2 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set9 => obj_Set9
  !! Set one time component of the selected nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set10 => obj_Set10
  !! Set one time component of one node
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set11 => obj_Set11
  !! Set the selected nodes from an FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set12 => obj_Set12
  !! Set all entries to one value
  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
  !! Copy another space-time scalar field
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, &
    Set7, Set8, Set9, Set10, Set11, Set12, Set13
  !! Generic Set Method
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    SetByFunction => obj_SetByFunction
  !! Set by user function
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    SetFromScalarField => obj_SetFromScalarField
  !! Set one time component from a scalar field
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    SetToScalarField => obj_SetToScalarField
  !! Set a scalar field from one time component

  ! GET:
  ! @GetMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get all time components of one node, or all nodes of one time
  !! component
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get all values in a rank-2 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! Get the values of the selected nodes in a rank-2 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! Get one time component of the selected nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get5 => obj_Get5
  !! Get single entry; call GetSingle method
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get6 => obj_Get6
  !! Get the values of the selected nodes in an FEVariable
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, Get6
  !! Generic Get method
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get Finite Element variable

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyDirichletBC1 => obj_ApplyDirichletBC1
  !! Apply one Dirichlet boundary condition
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyDirichletBC2 => obj_ApplyDirichletBC2
  !! Apply a vector of Dirichlet boundary conditions
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyDirichletBC3 => obj_ApplyDirichletBC3
  !! Apply the Dirichlet boundary conditions of the field
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    ApplyDirichletBC2, ApplyDirichletBC3
  !! Apply Dirichlet boundary conditions

  ! SET:
  ! @BodySourceMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyBodySource1 => obj_ApplyBodySource1
  !! Add contribution of body source to the scalar field
  !! body source is given as user function
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyBodySource2 => obj_ApplyBodySource2
  !! Add contribution of body source to the scalar field
  !! body source is given external scalar field
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyBodySource3 => obj_ApplyBodySource3
  !! Add contribution of body source to the scalar field
  !! body source is given as user function
  GENERIC, PUBLIC :: ApplyBodySource => ApplyBodySource1, &
    ApplyBodySource2, ApplyBodySource3
  !! Generic method for setting body source

  ! SET:
  ! @SurfaceNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: ApplySurfaceNeumannBC => &
    obj_ApplySurfaceNeumannBC
  !! Apply Surface neumann boundary condition

  ! SET:
  ! @PointNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: ApplyPointNeumannBC => &
    obj_ApplyPointNeumannBC
  !! Apply point neumann boundary condition
END TYPE STScalarField_

!----------------------------------------------------------------------------
!                                                     STScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldPointer_
  CLASS(STScalarField_), POINTER :: ptr => NULL()
  !! pointer to a space-time scalar field
END TYPE STScalarFieldPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Initiate by copying another node field
!
! The node field data of obj2 (name, engine, sizes, DOF data, FEDOF and
! boundary condition pointers, values) are copied by
! AbstractNodeFieldInitiate. When obj2 is a STScalarField_, timeCompo
! and idofs are copied as well.
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Initiate1( &
    obj, obj2, copyFull, copyStructure, usePointer)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field to be initiated
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    !! passed to AbstractNodeFieldInitiate
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    !! passed to AbstractNodeFieldInitiate
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
    !! passed to AbstractNodeFieldInitiate
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-19
! summary: Initiates by passing arguments
!
! obj is deallocated first. timeCompo is the total number of DOFs of
! timefedof when timefedof is present and initiated, otherwise it is
! timeCompo(1) (an error is raised in debug mode if it is missing).
! idofs is set to [1, ..., timeCompo]. The node field has one physical
! variable (named by the first character of name), one space
! component, timeCompo time components and fedof%GetTotalDOF() space
! DOFs, so its size is fedof%GetTotalDOF() * timeCompo.
!
! ImportFromToml calls this method; the TimeFEDOF is made from the
! table named by timefedofName.
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
! USE STScalarField_Class, ONLY: STScalarField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarField_) :: u, v, w, c
! REAL(DFP) :: found(12), want(12)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! ! Test 1: field from the toml file
! CALL OK(u%IsInitiated(), "u: IsInitiated")
! CALL OK(u%timeCompo .EQ. 2, "u: timeCompo")
! CALL OK(u%SIZE() .EQ. 12, "u: Size")
! CALL OK(u%GetTotalPhysicalVars() .EQ. 1, "u: one physical variable")
!
! ! Test 2: timeCompo from timefedof
! BLOCK
!   CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
!                   geofedof=geofedof, timefedof=timefedof)
!   CALL OK(v%timeCompo .EQ. timefedof%GetTotalDOF(), &
!           "v: timeCompo = time DOF")
!   CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF() * v%timeCompo, &
!           "v: Size = space DOF * timeCompo")
! END BLOCK
!
! ! Test 3: timeCompo given without timefedof
! BLOCK
!   CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", fedof=fedof, &
!                   geofedof=geofedof, timeCompo=[3])
!   CALL OK(w%timeCompo .EQ. 3, "w: timeCompo")
!   CALL OK(w%SIZE() .EQ. 18, "w: Size")
! END BLOCK
!
! ! Test 4: copy of u, with its values
! BLOCK
!   want = [(REAL(ii, DFP), ii=1, 12)]
!   CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
!   CALL c%Initiate(obj2=u, copyFull=math%yes)
!   CALL OK(c%timeCompo .EQ. u%timeCompo, "copy: timeCompo")
!   CALL OK(c%SIZE() .EQ. u%SIZE(), "copy: Size")
!   CALL c%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "copy: values")
! END BLOCK
!
! CALL c%DEALLOCATE()
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
! See also: [[obj_Initiate1]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2( &
    obj, name, engine, fieldType, storageFMT, comm, local_n, global_n, &
    spaceCompo, isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo, &
    isTimeCompoScalar, tPhysicalVarNames, physicalVarNames, &
    isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes, &
    isTNodesScalar, tSize, fedof, geofedof, timefedof)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field to be initiated
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field, needed
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine, needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL, needed
    !! following options are available FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: storageFMT
    !! storage format of the scalar field
    !! Not used, the DOF format is used
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
    !! space components, not used (always 1)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo, NOT REQUIRED
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1), NOT REQUIRED
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! Time components, timeCompo(1) is used when timefedof is not
    !! present or not initiated
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
    !! FEDOF of the field and FEDOF of the geometry
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(IN) :: timefedof
    !! TimeFEDOF object, gives timeCompo
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Deallocate the data stored inside the STScalarField_ obj
!
! timeCompo is set to zero, idofs is deallocated and the node field
! data are deallocated by AbstractNodeFieldDeallocate.
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE STScalarFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
  END SUBROUTINE obj_Deallocate
END INTERFACE STScalarFieldDeallocate

!----------------------------------------------------------------------------
!                              STScalarFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-25
! summary: Safely allocate a vector of STScalarFieldPointer_
!
! obj is allocated with newsize entries if it is not allocated. If its
! size is less than newsize, it is deallocated (STScalarFieldDeallocate)
! and allocated again with newsize entries. Otherwise nothing is done.

INTERFACE STScalarFieldSafeAllocate
  MODULE SUBROUTINE obj_STScalarFieldSafeAllocate1(obj, newsize)
    TYPE(STScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! allocatable scalar field pointer
    INTEGER(I4B), INTENT(IN) :: newsize
    !! new size of obj
  END SUBROUTINE obj_STScalarFieldSafeAllocate1
END INTERFACE STScalarFieldSafeAllocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-25
! summary: Deallocate a vector of STScalarFieldPointer_
!
! Each associated field is deallocated, then obj is deallocated.

INTERFACE STScalarFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(STScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of space-time scalar field pointers
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE STScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Finalizer, calls Deallocate
!
! See also: [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-26
! summary: Display the content of [[STScalarField_]]
!
! Displays the node field (AbstractNodeFieldDisplay) and timeCompo.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    CHARACTER(*), INTENT(IN) :: msg
    !! message displayed first
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Import the field from an HDF5 file
!
! The node field data are read by AbstractNodeFieldImport and timeCompo
! is read from group/timeCompo. The routine then checks the datasets
! group/tSize, group/dof and group/realVec and raises an error when one
! of them is missing. These are old paths: Export writes them as
! group/IntR0/tSize, group/DofR0/dof and group/RealVectorR0/realVec, so
! the routine currently always raises the error; idofs is not rebuilt
! either (see bugs-report/STScalarField_bug_report.md, H-03).
!
! See also: [[obj_Export]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened for reading
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof, geofedof
    !! FEDOF of the field and FEDOF of the geometry
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
    !! FEDOFs for multiple physical variables
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    !! TimeFEDOF of the field
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
    !! TimeFEDOFs for multiple physical variables
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Export the field to an HDF5 file
!
! Writes the node field data in group (AbstractNodeFieldExport) and
! timeCompo in group/timeCompo. In the example the Import part stops
! with an error until Import is fixed (see [[obj_Import]]).
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
! USE STScalarField_Class, ONLY: STScalarField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
! CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarField_) :: u, y
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: src(12), found(12)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! src = [(REAL(ii, DFP), ii=1, 12)]
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL u%Export(hdf5=h5, group="/u")
! CALL h5%DEALLOCATE()
!
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL y%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof, &
!               timefedof=timefedof)
! CALL h5%DEALLOCATE()
!
! ! Test 1: data of the field
! CALL OK(y%IsInitiated(), "Import: IsInitiated")
! CALL OK(y%timeCompo .EQ. 2, "Import: timeCompo")
! CALL OK(y%SIZE() .EQ. 12, "Import: Size")
!
! ! Test 2: values
! BLOCK
!   CALL y%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - src) .LT. tol), "Import: values")
! END BLOCK
!
! ! Test 3: Get by node on the imported field
! BLOCK
!   found = 0.0_DFP
!   CALL y%Get(VALUE=found, tsize=tsize, globalNode=3)
!   CALL OK(tsize .EQ. 2, "Import: Get node, tsize")
!   CALL OK(ALL(ABS(found(1:2) - [3.0_DFP, 9.0_DFP]) .LT. tol), &
!           "Import: Get node, values")
! END BLOCK
!
! CALL y%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened for writing
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all time components of one node
!
! VALUE(a) is the value of time component a at globalNode,
! SIZE(VALUE) = obj%timeCompo. The entries become scale * VALUE, or
! scale * VALUE is added when addContribution is true (SetMultiple with
! the indices of the node). In debug mode an error is raised if obj is
! not initiated, is a constant field, or SIZE(VALUE) is not timeCompo.
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
! USE STScalarField_Class, ONLY: STScalarField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarField_) :: u
! REAL(DFP) :: found(12), val
! INTEGER(I4B) :: tsize
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
!   CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Set all")
! END BLOCK
!
! ! Test 2: all time components of one node
! BLOCK
!   CALL u%Set(globalNode=2, islocal=math%yes, VALUE=[5.0_DFP, 6.0_DFP])
!   CALL u%Get(VALUE=found, tsize=tsize, globalNode=2)
!   CALL OK(ALL(ABS(found(1:2) - [5.0_DFP, 6.0_DFP]) .LT. tol), &
!           "Set node")
!   ! DOF format: node 2 is entry 2 (time 1) and entry 8 (time 2)
!   CALL u%GetMultiple(indx=[2, 8], VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found(1:2) - [5.0_DFP, 6.0_DFP]) .LT. tol), &
!           "Set node: DOF format")
!
!   CALL u%Set(globalNode=2, islocal=math%yes, VALUE=[1.0_DFP, 1.0_DFP], &
!              scale=2.0_DFP, addContribution=math%yes)
!   CALL u%Get(VALUE=found, tsize=tsize, globalNode=2)
!   CALL OK(ALL(ABS(found(1:2) - [7.0_DFP, 8.0_DFP]) .LT. tol), &
!           "Set node: add 2 * value")
! END BLOCK
!
! ! Test 3: one entry
! BLOCK
!   CALL u%Set(VALUE=9.0_DFP, globalNode=3, islocal=math%yes, timeCompo=2)
!   CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, timeCompo=2)
!   CALL OK(ABS(val - 9.0_DFP) .LT. tol, "Set single")
!   CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, timeCompo=1)
!   CALL OK(ABS(val - 1.0_DFP) .LT. tol, "Set single: other time")
! END BLOCK
!
! ! Test 4: one value for each time component, all nodes
! BLOCK
!   CALL u%Set(VALUE=[2.0_DFP, 3.0_DFP])
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - 2.0_DFP) .LT. tol), &
!           "Set time values: time 1")
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - 3.0_DFP) .LT. tol), &
!           "Set time values: time 2")
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
! See also: [[obj_Set7]], [[obj_Set10]]

INTERFACE
  MODULE SUBROUTINE obj_Set1( &
    obj, globalNode, islocal, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set @globalNode, obj = value
    !! value(a) denotes the value at time node `a`
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set time component a of all nodes to VALUE(a)
!
! For each time component a, all its entries become scale * VALUE(a)
! (or scale * VALUE(a) is added when addContribution is true).
! SIZE(VALUE) = obj%timeCompo. Not callable for a constant field.
!
! See also: [[obj_Set1]] (contains an example), [[obj_Set12]]

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set, obj = value
    !! the size of value should be equal to obj%timeCompo
    !! value(i) is value at ith time node
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one time component of all nodes to one value
!
! All entries of time component timeCompo become scale * VALUE, or
! scale * VALUE is added when addContribution is true. The other time
! components are not changed.
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
! USE STScalarField_Class, ONLY: STScalarField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarField_) :: u
! REAL(DFP) :: found(12), want(6)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! want = [(REAL(ii, DFP), ii=1, 6)]
! CALL u%Set(VALUE=0.0_DFP)
!
! ! Test 1: one time component to one value
! BLOCK
!   CALL u%Set(VALUE=4.0_DFP, timeCompo=1)
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - 4.0_DFP) .LT. tol), "timeCompo = 1")
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6)) .LT. tol), "time 2 unchanged")
! END BLOCK
!
! ! Test 2: one time component from a vector
! BLOCK
!   CALL u%Set(VALUE=want, timeCompo=2)
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "timeCompo = 2: vector")
!
!   CALL u%Set(VALUE=want, timeCompo=2, scale=-1.0_DFP, &
!              addContribution=math%yes)
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6)) .LT. tol), "timeCompo = 2: add -1 * vector")
! END BLOCK
!
! ! Test 3: one time component at selected nodes
! BLOCK
!   CALL u%Set(VALUE=[10.0_DFP, 20.0_DFP], globalNode=[1, 3], &
!              islocal=math%yes, timeCompo=1)
!   CALL u%Get(VALUE=found, globalNode=[1, 3], tsize=tsize, &
!              islocal=math%yes, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:2) - [10.0_DFP, 20.0_DFP]) .LT. tol), &
!           "selected nodes, timeCompo")
!   CALL u%Get(VALUE=found, tsize=tsize, globalNode=2)
!   CALL OK(ALL(ABS(found(1:2) - [4.0_DFP, 0.0_DFP]) .LT. tol), &
!           "other nodes unchanged")
! END BLOCK
!
! ! Test 4: all time components at selected nodes
! BLOCK
!   CALL u%Set(VALUE=[-1.0_DFP, -2.0_DFP], globalNode=[5, 6], &
!              islocal=math%yes)
!   CALL u%Get(VALUE=found, tsize=tsize, globalNode=5)
!   CALL OK(ALL(ABS(found(1:2) - [-1.0_DFP, -2.0_DFP]) .LT. tol), &
!           "selected nodes: node 5")
!   CALL u%Get(VALUE=found, tsize=tsize, globalNode=6)
!   CALL OK(ALL(ABS(found(1:2) - [-1.0_DFP, -2.0_DFP]) .LT. tol), &
!           "selected nodes: node 6")
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
! See also: [[obj_Set5]], [[obj_Set9]], [[obj_Set7]]

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, VALUE, timeCompo, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! obj@timeCompo = value
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set, obj=value
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    !! timecompo should be less than or equal to obj%timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all entries from a rank-2 array
!
! With storageFMT = DOF format, VALUE(i, a) is the value of space DOF i
! at time component a (SIZE(VALUE) = [tNodes, timeCompo]); each column
! is set by Set(VALUE(:), timeCompo). With the NODES format, VALUE(a, i)
! is used (SIZE(VALUE) = [timeCompo, tNodes]); each column is set by
! Set(globalNode, islocal, VALUE(:)).
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
! USE STScalarField_Class, ONLY: STScalarField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarField_) :: u
! REAL(DFP) :: vdof(6, 2), vnodes(2, 6), found(6, 6)
! INTEGER(I4B) :: ii, nrow, ncol
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! ! value of node i at time component a is 10 * a + i
! DO ii = 1, 6
!   vdof(ii, 1:2) = [10.0_DFP, 20.0_DFP] + REAL(ii, DFP)
! END DO
! vnodes = TRANSPOSE(vdof)
!
! ! Test 1: all entries, DOF format
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%Set(VALUE=vdof, storageFMT=TypeFieldOpt%storageFormatDOF)
!   CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, &
!              storageFMT=TypeFieldOpt%storageFormatDOF)
!   CALL OK(ALL(ABS(found(1:6, 1:2) - vdof) .LT. tol), "all, DOF")
! END BLOCK
!
! ! Test 2: all entries, NODES format
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%Set(VALUE=vnodes, storageFMT=TypeFieldOpt%storageFormatNodes)
!   CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, &
!              storageFMT=TypeFieldOpt%storageFormatDOF)
!   CALL OK(ALL(ABS(found(1:6, 1:2) - vdof) .LT. tol), "all, NODES")
!
!   CALL u%Set(VALUE=vnodes, storageFMT=TypeFieldOpt%storageFormatNodes, &
!              scale=-1.0_DFP, addContribution=math%yes)
!   CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, &
!              storageFMT=TypeFieldOpt%storageFormatDOF)
!   CALL OK(ALL(ABS(found(1:6, 1:2)) .LT. tol), "all, NODES: add -1")
! END BLOCK
!
! ! Test 3: selected nodes, NODES format
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%Set(globalNode=[2, 4], islocal=math%yes, &
!              VALUE=vnodes(1:2, [2, 4]), &
!              storageFMT=TypeFieldOpt%storageFormatNodes)
!   CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, globalNode=[2, 4], &
!              islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatNodes)
!   CALL OK(ALL(ABS(found(1:2, 1:2) - vnodes(1:2, [2, 4])) .LT. tol), &
!           "selected, NODES")
!   CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, globalNode=[1, 3], &
!              islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatNodes)
!   CALL OK(ALL(ABS(found(1:2, 1:2)) .LT. tol), "other nodes unchanged")
! END BLOCK
!
! ! Test 4: selected nodes, DOF format
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%Set(globalNode=[5, 6], islocal=math%yes, &
!              VALUE=vdof([5, 6], 1:2), &
!              storageFMT=TypeFieldOpt%storageFormatDOF)
!   CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, globalNode=[5, 6], &
!              islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatDOF)
!   CALL OK(ALL(ABS(found(1:2, 1:2) - vdof([5, 6], 1:2)) .LT. tol), &
!           "selected, DOF")
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
! See also: [[obj_Set8]], [[obj_Get2]]

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, VALUE, storageFMT, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! obj = value
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! values to be set obj = value
    !! DOF format: SIZE(VALUE) = [tNodes, timeCompo]
    !! NODES format: SIZE(VALUE) = [timeCompo, tNodes]
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! Storage format of value,
    !! TypeFieldOpt%storageFormatDOF or TypeFieldOpt%storageFormatNodes
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one time component of all nodes from a vector
!
! The entries of time component timeCompo become scale * VALUE(i) for
! space DOF i, SIZE(VALUE) = number of space DOFs, or they are added
! when addContribution is true.
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, VALUE, timeCompo, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set obj = value
    !! size of values should be equal to the total number of nodes
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one time component from a node field (not yet available)
!
! Meant to set time component timeCompo of obj from the node field
! VALUE. The routine is not implemented: it raises an error. Use
! SetFromScalarField for a scalar field.
!
! See also: [[obj_SetFromScalarField]]

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, VALUE, timeCompo, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! obj@timeCompo = value
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    !! value in abstract node field
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all time components of the selected nodes
!
! For each node in globalNode, time component a becomes scale * VALUE(a)
! (Set1 is called for each node). SIZE(VALUE) = obj%timeCompo; all
! selected nodes get the same values.
!
! See also: [[obj_Set3]] (contains an example), [[obj_Set1]]

INTERFACE
  MODULE SUBROUTINE obj_Set7( &
    obj, VALUE, globalNode, islocal, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set, i.e., obj(:, globalNode)=value
    !! The size of the value should be equal to obj%timeCompo
    !! value denotes the time component value at globalNodes
    !! note that all space nodes have same value.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected nodes from a rank-2 array
!
! With the NODES format VALUE(a, j) is the value of time component a at
! globalNode(j) (Set1 for each node); with the DOF format VALUE(j, a)
! is used (Set9 for each time component).
!
! See also: [[obj_Set4]] (contains an example), [[obj_Get3]]

INTERFACE
  MODULE SUBROUTINE obj_Set8( &
    obj, globalNode, islocal, VALUE, storageFMT, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! space-time nodal values at globalNode
    !! The values should be stored in NODES_FMT, that is:
    !!   - number of rows in value denotes the time nodes
    !!   - number of columns in value denotes the space nodes
    !!   - size(value,1) should be obj%timeCompo
    !!   - size(value,2) should be size(globalNode)
    !! The values can be stored in DOF_FMT, that is:
    !!   - number of cols in value denotes the time nodes
    !!   - number of rows in value denotes the space nodes
    !!   - size(value,2) should be obj%timeCompo
    !!   - size(value,1) should be size(globalNode)
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! The storage format of value,
    !! It can be either NODES_FMT or DOF_FMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one time component of the selected nodes
!
! The entry of time component timeCompo at globalNode(j) becomes
! scale * VALUE(j), SIZE(VALUE) = SIZE(globalNode), or it is added when
! addContribution is true.
!
! See also: [[obj_Set3]] (contains an example), [[obj_Get4]]

INTERFACE
  MODULE SUBROUTINE obj_Set9( &
    obj, VALUE, globalNode, islocal, timeCompo, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set, i.e., obj(:, globalNode)=value
    !! these are values at space nodes
    !! the size of value should be equal to sie of globalnode
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set one time component of one node
!
! The entry of time component timeCompo at globalNode becomes
! scale * VALUE (SetSingle), or it is added when addContribution is
! true.
!
! See also: [[obj_Set1]] (contains an example), [[obj_Get5]]

INTERFACE
  MODULE SUBROUTINE obj_Set10( &
    obj, VALUE, globalNode, islocal, timeCompo, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: VALUE
    !! values to be set, i.e., obj(:, globalNode)=value
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected nodes from an FEVariable
!
! VALUE must be a nodal, scalar, space-time FEVariable whose values are
! stored as val(a, j), the value of time component a at globalNode(j)
! (the layout returned by Get6 and GetFEVariable). The values are set
! by Set8 with the NODES format. In debug mode an error is raised for
! another vartype.
!
! Examples
!
!```fortran
! vals(1:2, 1) = [1.0_DFP, 2.0_DFP]
! vals(1:2, 2) = [3.0_DFP, 4.0_DFP]
! fevar = NodalVariable(vals, TypeFEVariableScalar, &
!                       TypeFEVariableSpaceTime)
! CALL u%Set(VALUE=fevar, globalNode=[1, 2], islocal=math%yes)
!```
!
! Full example: src/modules/STScalarField/examples/Set_test_4.F90
!
! See also: [[obj_Get6]], [[obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Set11( &
    obj, VALUE, globalNode, islocal, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! value in FEVariable
    !! It should be nodal scalar space-time variable
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Set all entries to one value
!
! All entries become scale * VALUE (SetAll), or scale * VALUE is added
! when addContribution is true.
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be set
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Copy another space-time scalar field, obj = VALUE
!
! The values of VALUE are copied into obj (COPY). Both fields must be
! initiated (checked in debug mode) and have the same size.
!
! Examples
!
!```fortran
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)
! CALL v%Set(VALUE=u)
!```
!
! Full example: src/modules/STScalarField/examples/Set_test_4.F90

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, VALUE)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field, obj = VALUE
    CLASS(STScalarField_), INTENT(IN) :: VALUE
    !! space-time scalar field to copy
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Set the STScalarField

! INTERFACE
!   MODULE SUBROUTINE obj_Set13(obj, ivar, idof, VALUE, ivar_value, &
!                               idof_value, scale, addContribution)
!     CLASS(STScalarField_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: ivar
!     INTEGER(I4B), INTENT(IN) :: idof
!     CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     INTEGER(I4B), INTENT(IN) :: idof_value
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!   END SUBROUTINE obj_Set13
! END INTERFACE

!----------------------------------------------------------------------------
!                                              SetFromScalarField@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set one time component from a scalar field
!
! The entries of time component timeCompo become scale times the
! values of the scalar field VALUE, or they are added when
! addContribution is true. VALUE must have the same space FEDOF and
! use the native engine (its Fortran vector is used through
! GetPointer).
!
! Examples
!
!```fortran
! CALL s%Set(VALUE=want)
! CALL u%SetFromScalarField(timeCompo=2, VALUE=s)
!```
!
! Full example: src/modules/STScalarField/examples/Set_test_4.F90
!
! See also: [[obj_SetToScalarField]]

INTERFACE
  MODULE SUBROUTINE obj_SetFromScalarField( &
    obj, timeCompo, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component of obj to be set
    CLASS(ScalarField_), INTENT(IN) :: VALUE
    !! scalar field with the native engine
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetFromScalarField
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetToScalarField@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set a scalar field from one time component
!
! The values of the scalar field VALUE become scale times the entries
! of time component timeCompo of obj, or they are added when
! addContribution is true. obj must use the native engine (its Fortran
! vector is used through GetPointer).
!
! Examples
!
!```fortran
! CALL u%SetToScalarField(timeCompo=1, VALUE=s)
! CALL s%Get(VALUE=found, tsize=tsize)
!```
!
! Full example: src/modules/STScalarField/examples/Set_test_4.F90
!
! See also: [[obj_SetFromScalarField]]

INTERFACE
  MODULE SUBROUTINE obj_SetToScalarField( &
    obj, timeCompo, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field with the native engine
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component of obj to be copied
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
    !! scalar field to be set
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetToScalarField
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetByFunction@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-18
! summary: Set by a user function
!
! For each node of the mesh, func(x, y, z, t) is evaluated at the node
! coordinates. When times is present, time component a uses
! t = times(a), SIZE(times) = obj%timeCompo; otherwise t = 0 and the
! same value is used for all time components. The values are set at
! the node number (Set1), that is, the mesh node number is used as the
! space DOF number.
!
! Only order 1 Lagrange interpolation in space is supported. In debug
! mode a non-Lagrange interpolation raises an error. For a Lagrange
! space of order greater than 1 the DOFs that are not mesh nodes are
! not set; this is not checked.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE STScalarField_Class, ONLY: STScalarField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: f0(6) = [1.0_DFP, 11.0_DFP, 31.0_DFP, &
!                                  21.0_DFP, 6.0_DFP, 26.0_DFP]
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarField_) :: u
! TYPE(UserFunction_) :: func
! PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
! REAL(DFP) :: found(12)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL func%Initiate(NAME="func", returnType=TypeFEVariableOpt%scalar, &
!                    argType=TypeFEVariableOpt%space, numArgs=4, &
!                    numReturns=1)
! procptr => linearFunc
! CALL func%SetScalarFunctionPointer(func=procptr)
!
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! ! Test 1: one time for each time component
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%SetByFunction(func=func, times=[0.0_DFP, 1.0_DFP])
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - f0) .LT. tol), "times: t = 0")
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - f0 - 3.0_DFP) .LT. tol), "times: t = 1")
! END BLOCK
!
! ! Test 2: without times, f at t = 0 for all time components
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%SetByFunction(func=func)
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - f0) .LT. tol), "no times")
! END BLOCK
!
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
!
! CONTAINS
!
! SUBROUTINE linearFunc(args, nargs, ans)
!   REAL(DFP), INTENT(IN) :: args(:)
!   INTEGER(I4B), INTENT(IN) :: nargs
!   REAL(DFP), INTENT(INOUT) :: ans
!   ans = 1.0_DFP + args(1) + 2.0_DFP * args(2) + 3.0_DFP * args(4)
! END SUBROUTINE linearFunc
!
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction( &
    obj, func, times, ivar, idof, spaceCompo, timeCompo)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function, scalar with 4 arguments (x, y, z, t)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! times of the time components, SIZE(times) = obj%timeCompo
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
! date: 2021-06-25
! summary: Get all time components of one node, or one time component
!
! If globalNode is present, VALUE(1:timeCompo) are the values of all
! time components at globalNode (a local node number) and
! tsize = timeCompo. If timeCompo is present, VALUE(1:tNodes) are the
! values of all space DOFs of this time component and tsize = tNodes.
! Exactly one of globalNode and timeCompo must be present (checked in
! debug mode).
!
! Examples
!
!```fortran
! src = [(REAL(ii, DFP), ii=1, 12)]
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)
! CALL u%Get(VALUE=found, tsize=tsize, globalNode=4)
! CALL OK(ALL(ABS(found(1:2) - [4.0_DFP, 10.0_DFP]) .LT. tol), &
!         "node: values")
! CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
! CALL OK(ALL(ABS(found(1:6) - src(7:12)) .LT. tol), &
!         "timeCompo: values")
!```
!
! Full example: src/modules/STScalarField/examples/Get_test_1.F90
!
! See also: [[obj_Get4]], [[obj_Get5]]

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, tsize, globalNode, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! Value to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in value
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    !! global node number (it is local node number)
    !! In this case all the time nodal values of scalar at globalNode
    !! will be returned
    !! in this case tsize will be timeCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
    !! In this case all the space nodal values of obj at timeCompo
    !! will be returned.
    !! In this case tsize will be tnodes
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get all values in a rank-2 array
!
! With the NODES format VALUE(a, i) is the value of time component a at
! space DOF i (nrow = timeCompo, ncol = tNodes); with the DOF format
! VALUE(i, a) (nrow = tNodes, ncol = timeCompo).
!
! Examples
!
!```fortran
! CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, &
!            storageFMT=TypeFieldOpt%storageFormatNodes)
! CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 6, "all, NODES: shape")
!```
!
! Full example: src/modules/STScalarField/examples/Get_test_1.F90
!
! See also: [[obj_Set4]], [[obj_Get3]]

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, nrow, ncol, storageFMT)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! values, SIZE(VALUE, 1) >= nrow, SIZE(VALUE, 2) >= ncol
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of cols written in value
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! if storageFMT is NODES_FMT then
    !! nrow = obj%timeCompo, ncol = number of space DOFs
    !! if stroageFMT is DOF_FMT then
    !! nrow = number of space DOFs, ncol = obj%timeCompo
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the values of the selected nodes in a rank-2 array
!
! With the NODES format VALUE(a, j) is the value of time component a at
! globalNode(j) (nrow = timeCompo, ncol = SIZE(globalNode)); with the
! DOF format VALUE(j, a) (nrow = SIZE(globalNode), ncol = timeCompo).
!
! Examples
!
!```fortran
! CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, globalNode=[2, 5], &
!            islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatDOF)
!```
!
! Full example: src/modules/STScalarField/examples/Get_test_1.F90
!
! See also: [[obj_Set8]], [[obj_Get2]]

INTERFACE
  MODULE SUBROUTINE obj_Get3( &
    obj, VALUE, nrow, ncol, globalNode, islocal, storageFMT)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! values, SIZE(VALUE, 1) >= nrow, SIZE(VALUE, 2) >= ncol
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of cols written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! if storageFMT is NODES_FMT then
    !! nrow = obj%timeCompo, ncol = size(globalNode)
    !! if storageFMT is DOF_FMT then
    !! nrow = size(globalNode), ncol = obj%timeCompo
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get one time component of the selected nodes
!
! VALUE(j) is the value of time component timeCompo at globalNode(j),
! tsize = SIZE(globalNode).
!
! Examples
!
!```fortran
! CALL u%Get(VALUE=found, globalNode=[3, 6], tsize=tsize, &
!            islocal=math%yes, timeCompo=2)
!```
!
! Full example: src/modules/STScalarField/examples/Get_test_1.F90
!
! See also: [[obj_Set9]]

INTERFACE
  MODULE SUBROUTINE obj_Get4( &
    obj, VALUE, globalNode, tsize, islocal, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    !! size should be equal to the size of globalNode
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component, it should be less than obj%timeCompo
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get single entry
!
! VALUE is the value of time component timeCompo at globalNode
! (GetSingle).
!
! Examples
!
!```fortran
! CALL u%Get(VALUE=val, globalNode=6, islocal=math%yes, timeCompo=1)
!```
!
! Full example: src/modules/STScalarField/examples/Get_test_1.F90
!
! See also: [[obj_Set10]]

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, islocal, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value to be returned
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component, it should be less than obj%timeCompo
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the values of the selected nodes in an FEVariable
!
! VALUE becomes a nodal, scalar, space-time FEVariable with
! val(a, j) = value of time component a at globalNode(j)
! (s = [timeCompo, SIZE(globalNode)]), from Get3 with the NODES format.
!
! See also: [[obj_Set11]] (contains an example), [[obj_GetFeVariable]]

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, globalNode, islocal)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! returned value in FEVariable format
    !! Space-time nodal values of scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get one time component in a node field (commented out)

! INTERFACE
!   MODULE SUBROUTINE obj_Get7(obj, VALUE, timeCompo)
!     CLASS(STScalarField_), INTENT(IN) :: obj
!     !! space-time scalar field
!     CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
!     !! returned value in AbstractNodeField format
!     INTEGER(I4B), INTENT(IN) :: timeCompo
!     !! time component, it should be less than obj%timeCompo
!   END SUBROUTINE obj_Get7
! END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Get values

! INTERFACE
!   MODULE SUBROUTINE obj_Get8( &
!     obj, ivar, idof, VALUE, ivar_value, idof_value)
!     CLASS(STScalarField_), INTENT(IN) :: obj
!     CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
!     INTEGER(I4B), INTENT(IN) :: ivar
!     !! physical variable in obj
!     INTEGER(I4B), INTENT(IN) :: idof
!     !! local degree of freedom in obj
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     !! physical variable in VALUE
!     INTEGER(I4B), INTENT(IN) :: idof_value
!     !! local degree of freedom in VALUE
!   END SUBROUTINE obj_Get8
! END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTimeCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-10-29
! summary: Returns timeCompo
!
! This function is not bound to the type and its generic name
! STScalarFieldGetTimeCompo is not public; use the component timeCompo
! or GetTimeCompo of AbstractNodeField_ (see
! bugs-report/STScalarField_bug_report.md, L-01).
!
! Returns:
!   ans : INTEGER(I4B) - obj%timeCompo

INTERFACE
  MODULE FUNCTION obj_GetTimeCompo(obj) RESULT(ans)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    INTEGER(I4B) :: ans
    !! number of time components
  END FUNCTION obj_GetTimeCompo
END INTERFACE

INTERFACE STScalarFieldGetTimeCompo
  MODULE PROCEDURE obj_GetTimeCompo
END INTERFACE STScalarFieldGetTimeCompo

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get the values of the selected nodes in an FEVariable
!
! Same as Get(VALUE=fevar, globalNode, islocal) (Get6); ivar is not
! used.
!
! Examples
!
!```fortran
! CALL u%GetFEVariable(globalNode=[2, 1], islocal=math%yes, VALUE=fevar)
! CALL OK(ALL(fevar%s(1:2) .EQ. [2, 2]), "GetFEVariable: shape")
!```
!
! Full example: src/modules/STScalarField/examples/Set_test_4.F90
!
! See also: [[obj_Get6]]

INTERFACE
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! returned value in FEVariable format (nodal, space, time, scalar)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! This argument is not used
    !! Physical component
  END SUBROUTINE obj_GetFeVariable
END INTERFACE

INTERFACE STScalarFieldGetFEVariable
  MODULE PROCEDURE obj_GetFeVariable
END INTERFACE STScalarFieldGetFEVariable

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply one Dirichlet boundary condition
!
! The node numbers and the values of dbc at the time nodes (times) are
! obtained (dbc%Get with fedof, geofedof and timefedof), and for each
! time component the entries of these nodes are set to the values
! (Set9). Only the boundary entries are changed.
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
! USE STScalarField_Class, ONLY: STScalarField_
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
! TYPE(STScalarField_) :: u
! TYPE(DirichletBCPointer_), ALLOCATABLE :: extDBC(:)
! REAL(DFP) :: found(12), want(6)
! INTEGER(I4B) :: tsize
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
!   want = [1.0_DFP, 2.0_DFP, 2.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "internal dbc: time 1")
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "internal dbc: time 2")
! END BLOCK
!
! ! Test 2: vector of DirichletBCPointer_
! BLOCK
!   CALL u%Set(VALUE=-1.0_DFP)
!   CALL u%ApplyDirichletBC(dbc=extDBC, times=times)
!   want = [7.0_DFP, 7.0_DFP, -1.0_DFP, -1.0_DFP, 7.0_DFP, -1.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "dbc vector")
! END BLOCK
!
! ! Test 3: single DirichletBC_
! BLOCK
!   CALL u%Set(VALUE=3.0_DFP)
!   CALL u%ApplyDirichletBC(dbc=extDBC(1)%ptr, times=times)
!   want = [7.0_DFP, 7.0_DFP, 3.0_DFP, 3.0_DFP, 7.0_DFP, 3.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "single dbc")
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
! See also: [[obj_ApplyDirichletBC2]], [[obj_ApplyDirichletBC3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    !! Dirichlet boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times of the time nodes, SIZE(times) = obj%timeCompo
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a vector of Dirichlet boundary conditions
!
! Same as [[obj_ApplyDirichletBC1]] for each associated pointer of dbc,
! in order.
!
! See also: [[obj_ApplyDirichletBC1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary conditions, null pointers are skipped
    REAL(DFP), INTENT(IN) :: times(:)
    !! times of the time nodes, SIZE(times) = obj%timeCompo
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply the Dirichlet boundary conditions of the field
!
! Same as [[obj_ApplyDirichletBC2]] with the internal list obj%dbc (for
! example the dirichletBC tables of the toml file).
!
! See also: [[obj_ApplyDirichletBC1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC3(obj, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: times(:)
    !! times of the time nodes, SIZE(times) = obj%timeCompo
  END SUBROUTINE obj_ApplyDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!                                         ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-19
! summary: Add the body source given by a user function
!
! For each element, the integral over the element and the time slab
! times(1) to times(2) of the space test functions N_I, the time test
! functions T_a and the body source is computed by quadrature
! (STForceVector_), and scale times this integral is added to the
! entry of node I and time component a.
!
! Formula:
!   $$ u_{Ia} = u_{Ia} + s \int_{t} \int_{\Omega} N_I T_a f(x, y, z, t)
!      \, d\Omega \, dt $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE STScalarField_Class, ONLY: STScalarField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
! REAL(DFP), PARAMETER :: space(6) = [12.5_DFP, 12.5_DFP, 12.5_DFP, &
!                                     12.5_DFP, 25.0_DFP, 25.0_DFP]
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarField_) :: u
! TYPE(UserFunction_) :: funcOne, funcT
! PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
! REAL(DFP) :: found(12)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL funcOne%Initiate(NAME="one", returnType=TypeFEVariableOpt%scalar, &
!                       argType=TypeFEVariableOpt%space, numArgs=4, &
!                       numReturns=1)
! procptr => oneFunc
! CALL funcOne%SetScalarFunctionPointer(func=procptr)
! CALL funcT%Initiate(NAME="t", returnType=TypeFEVariableOpt%scalar, &
!                     argType=TypeFEVariableOpt%space, numArgs=4, &
!                     numReturns=1)
! procptr => tFunc
! CALL funcT%SetScalarFunctionPointer(func=procptr)
!
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
!
! ! Test 1: f = 1
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%ApplyBodySource(bodySource=funcOne, scale=1.0_DFP, times=times)
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * space) .LT. tol), &
!           "f = 1: time 1")
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * space) .LT. tol), &
!           "f = 1: time 2")
! END BLOCK
!
! ! Test 2: f = t, added with scale 6
! BLOCK
!   CALL u%Set(VALUE=1.0_DFP)
!   CALL u%ApplyBodySource(bodySource=funcT, scale=6.0_DFP, times=times)
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + space)) .LT. tol), &
!           "f = t: time 1")
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + 2.0_DFP * space)) .LT. tol), &
!           "f = t: time 2")
! END BLOCK
!
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
!
! CONTAINS
!
! SUBROUTINE oneFunc(args, nargs, ans)
!   REAL(DFP), INTENT(IN) :: args(:)
!   INTEGER(I4B), INTENT(IN) :: nargs
!   REAL(DFP), INTENT(INOUT) :: ans
!   ans = 1.0_DFP
! END SUBROUTINE oneFunc
!
! SUBROUTINE tFunc(args, nargs, ans)
!   REAL(DFP), INTENT(IN) :: args(:)
!   INTEGER(I4B), INTENT(IN) :: nargs
!   REAL(DFP), INTENT(INOUT) :: ans
!   ans = args(4)
! END SUBROUTINE tFunc
!
! END PROGRAM main
!```
!
! See also: [[obj_ApplyBodySource3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource1(obj, bodySource, scale, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field (right hand side)
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! Body source user function
    !! It should be a scalar function with
    !! total arguments 4 (x, y, z, time)
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
    REAL(DFP), INTENT(IN) :: times(:)
    !! times of the time slab, passed to the time element
  END SUBROUTINE obj_ApplyBodySource1
END INTERFACE

!----------------------------------------------------------------------------
!                                         ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-19
! summary: Add the body source given by a field (not yet available)
!
! Meant to add the body source given by the space-time field
! bodySource. The routine is not implemented: it raises an error.
!
! See also: [[obj_ApplyBodySource1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource2(obj, bodySource, scale)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    !! The test function will be corresponding to the obj
    CLASS(STScalarField_), INTENT(INOUT) :: bodySource
    !! Body source in terms of scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
  END SUBROUTINE obj_ApplyBodySource2
END INTERFACE

!----------------------------------------------------------------------------
!                                         ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-19
! summary: Add the body source at one time given by a user function
!
! For each element, the space integral of N_I f(x, y, z, times) is
! computed (ForceVector_) and added with obj%Set(globalNode, VALUE).
! Without a time component this call adds the space force vector to
! all time components of each node as if it were one value per time
! component, which is wrong; in debug mode it stops with a size error
! (see bugs-report/STScalarField_bug_report.md, H-04).
!
! See also: [[obj_ApplyBodySource1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource3(obj, bodySource, scale, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field (right hand side)
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! Body source user function
    !! It should be a scalar function with
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
!                                     ApplySurfaceNeumannBC@SurfaceNBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add Contribution of neumann boundary condition
!
! For each Neumann boundary condition of the field, nbcField is set to zero
! and only this boundary condition is applied to it as Dirichlet values
! (extension of its boundary data to the domain). Then the integral of the
! test functions times nbcField over the facets of this boundary condition and
! the time interval is computed, and scale times this integral is added to
! obj. So each facet gets only the data of its own boundary condition (also at
! nodes shared by two boundaries), and the contributions of several boundary
! conditions add up. Nothing is done if the field has no Neumann boundary
! condition.
!
! Changes:
!   - 2026-11-02: integrate each Neumann boundary condition with its own
!     data only (before, the data of all conditions was used)
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
! USE STScalarField_Class, ONLY: STScalarField_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = &
!                            "ApplySurfaceNeumannBC_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarField_) :: u, nbcField
! REAL(DFP) :: found(12), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
! CALL nbcField%Initiate(obj2=u, copyFull=math%no, &
!                        copyStructure=math%yes, usePointer=math%yes)
!
! CALL OK(u%GetTotalNBC() .EQ. 2, "GetTotalNBC")
!
! want = [6.25_DFP, 11.25_DFP, 5.0_DFP, 0.0_DFP, 12.5_DFP, 0.0_DFP]
!
! ! Test 1: values of both time components
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%ApplySurfaceNeumannBC(nbcField=nbcField, scale=1.0_DFP, &
!                                times=times)
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "time 1")
!   CALL OK(ABS(SUM(found(1:6)) - 35.0_DFP) .LT. tol, "time 1: total")
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "time 2")
! END BLOCK
!
! CALL nbcField%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ApplyPointNeumannBC]]

INTERFACE
  MODULE SUBROUTINE obj_ApplySurfaceNeumannBC(obj, nbcField, scale, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    CLASS(STScalarField_), INTENT(INOUT) :: nbcField
    !! Scalar field where we will keep the neumann boundary condition
    !! extension to the entire domain
    REAL(DFP), INTENT(IN) :: scale
    !! Scale for neumann boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times of the time slab
  END SUBROUTINE obj_ApplySurfaceNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!                                         ApplyPointNeumannBC@PointNBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add Contribution of point neumann boundary condition
!
! For each point Neumann boundary condition (obj%nbc_point), the node
! numbers and the values at the time nodes are obtained. The load of
! time component a is the integral of T_a(t) f(t) over the time slab:
! the nodal values f_b are multiplied by the time mass matrix
! ct(a, b), and scale times the result is added to the entries of the
! nodes. Nothing is done if the field has no point Neumann boundary
! condition.
!
! Formula:
!   $$ u_{Ia} = u_{Ia} + s \sum_b \int_t T_a T_b \, dt \, f_{Ib} $$
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
! USE STScalarField_Class, ONLY: STScalarField_
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
! TYPE(STScalarField_) :: u
! REAL(DFP) :: found(12), want(6)
! INTEGER(I4B) :: tsize
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
!   want = [1.0_DFP, 0.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "scale = 1: time 1")
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "scale = 1: time 2")
! END BLOCK
!
! ! Test 3: added to the current values, scale = 0.5
! BLOCK
!   CALL u%Set(VALUE=1.0_DFP)
!   CALL u%ApplyPointNeumannBC(scale=0.5_DFP, times=times)
!   want = [1.5_DFP, 1.0_DFP, 2.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
!   CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
!   CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "add with scale = 0.5")
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
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for neumann boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times of the time slab
  END SUBROUTINE obj_ApplyPointNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STScalarField_Class
