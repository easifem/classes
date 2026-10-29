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
! date: 28 June 2021
! summary: Scalar field data type is defined

MODULE ScalarField_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: FEVariable_
USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP, I4B, LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE NeumannBC_Class, ONLY: NeumannBCPointer_
USE String_Class, ONLY: String
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_
USE TimeOpt_Class, ONLY: TimeOpt_
USE Tomlf, ONLY: toml_table
USE UserFunction_Class, ONLY: UserFunction_
USE VTKFile_Class, ONLY: VTKFile_
IMPLICIT NONE
PRIVATE

PUBLIC :: ScalarField_
PUBLIC :: ScalarFieldPointer_
PUBLIC :: ScalarFieldInitiate
PUBLIC :: ScalarFieldImport
PUBLIC :: ScalarFieldExport
PUBLIC :: ScalarFieldDeallocate
PUBLIC :: ScalarFieldSafeAllocate
PUBLIC :: ScalarFieldApplyBodySource

!----------------------------------------------------------------------------
!                                                              ScalarField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Scalar field data type
!
! A scalar field has one physical variable with one space and one
! time component. Its values (one for each DOF of fedof) are stored
! in the vector of AbstractNodeField_ (DOF storage format), so the
! value of DOF i is entry i.
!
! The values are set by Set, SetByFunction, SetLagrangeDOF, the
! boundary condition methods (ApplyDirichletBC, ...) and the BLAS
! methods of AbstractNodeField_, and they are read by Get.

TYPE, EXTENDS(AbstractNodeField_) :: ScalarField_
CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate an instance of ScalarField_ by passing arguments
  FINAL :: obj_Final

  ! @SetMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set single entry, we call SetSingle method
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set all values to a scalar values
  !! We call SetAll method here
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set all values to a given vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set selected values to given scalar
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set selected values to given vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set selected values using FEVariable
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Copy another scalar field, used for the ASSIGNMENT operator
  PROCEDURE, PRIVATE, PASS(obj) :: Set8 => obj_Set8
  !! obj = obj + scale*obj2
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetByFunction => &
    obj_SetByFunction
  !! Set scalar field using a function
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, &
    Set5, Set6, Set7, Set8
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Set7
  !! obj = obj2, copy the values of another scalar field
  PROCEDURE, PUBLIC, PASS(obj) :: SetLagrangeDOF => obj_SetLagrangeDOF
  !! Set the Lagrange DOFs of obj from another scalar field

  ! @GetMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get single entry
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get all values in Real vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! Get selected values
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! Get selected values in FEVariable
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4
  !! Get the entries of scalar field
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: GetFEVariable => &
    obj_GetFeVariable
  !! Get Finite Element variable
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Number of values of the scalar field
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

  ! @DBCMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC1 => &
    obj_ApplyDirichletBC1
  !! Apply Dirichlet Boundary Condition
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC2 => &
    obj_ApplyDirichletBC2
  !! Apply Dirichlet Boundary Condition
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC3 => &
    obj_ApplyDirichletBC3
  !! Apply Dirichlet Boundary Condition
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    ApplyDirichletBC2, ApplyDirichletBC3
  !! Apply Dirichlet boundary conditions
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyZeroDirichletBC1 => obj_ApplyZeroDirichletBC1
  !! Apply Zero Dirichlet Boundary Condition
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyZeroDirichletBC2 => obj_ApplyZeroDirichletBC2
  !! Apply Zero Dirichlet Boundary Condition
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyZeroDirichletBC3 => obj_ApplyZeroDirichletBC3
  !! Apply Zero Dirichlet Boundary Condition
  GENERIC, PUBLIC :: ApplyZeroDirichletBC => ApplyZeroDirichletBC1, &
    ApplyZeroDirichletBC2, ApplyZeroDirichletBC3
  !! Set zero values at the Dirichlet nodes

  ! @PointNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: ApplyPointNeumannBC => &
    obj_ApplyPointNeumannBC
  !! Add the point Neumann boundary conditions

  ! @SurfaceNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    ApplySurfaceNeumannBC1 => obj_ApplySurfaceNeumannBC1
  !! Apply Surface neumann boundary condition using internal nbc list
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    ApplySurfaceNeumannBC2 => obj_ApplySurfaceNeumannBC2
  !! Apply Surface neumann boundary condition using externally given list
  GENERIC, PUBLIC :: ApplySurfaceNeumannBC => ApplySurfaceNeumannBC1, &
    ApplySurfaceNeumannBC2
  !! Generic method for applying surface neumann boundary conditions

  ! @BodySourceMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyBodySource1 => &
    obj_ApplyBodySource1
  !! Add contribution of body source to the scalar field
  !! body source is given as user function
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyBodySource2 => &
    obj_ApplyBodySource2
  !! Add contribution of body source to the scalar field
  !! body source is given external scalar field
  GENERIC, PUBLIC :: ApplyBodySource => ApplyBodySource1, ApplyBodySource2
  !! Generic method for setting body source

  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data to HDF5 file

  ! @VTKMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
  !! Write the vertex values in a VTK file

  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetFromToml => obj_SetFromToml
  !! Set the values from a toml table
END TYPE ScalarField_

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldPointer_
  CLASS(ScalarField_), POINTER :: ptr => NULL()
END TYPE ScalarFieldPointer_

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-19
! summary: Initiate a scalar field by passing arguments
!
! The data stored in obj is deallocated first. A scalar field has one
! physical variable, whose name is the first character of name, with
! one space component and one time component. The number of values is
! the total number of DOFs of fedof (fedof%GetTotalDOF()). The other
! data (engine, fieldType, comm, local_n, global_n, FEDOF pointers) are
! set by Initiate of AbstractNodeField_.
!
! The arguments storageFMT, spaceCompo, isSpaceCompo,
! isSpaceCompoScalar, timeCompo, isTimeCompo, isTimeCompoScalar,
! tPhysicalVarNames, physicalVarNames, isPhysicalVarNames,
! isPhysicalVarNamesScalar, tNodes, isTNodes, isTNodesScalar and tSize
! are not used; they are kept so that the interface is the same as in
! AbstractNodeField_.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   name : CHARACTER(*) - name of the field
!   engine : CHARACTER(*) - engine, e.g., "NATIVE_SERIAL"
!   fieldType : INTEGER(I4B) - (optional) TypeFieldOpt%normal (default)
!     or TypeFieldOpt%constant
!   comm, local_n, global_n : INTEGER(I4B) - (optional) parallel data
!   fedof : CLASS(FEDOF_) - FEDOF of the field (pointer is stored)
!   geofedof : CLASS(FEDOF_) - FEDOF of the geometry (pointer is stored)
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
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u, v
! CHARACTER(1) :: names(1)
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL v%Initiate(name="v", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! CALL OK(v%IsInitiated(), "IsInitiated")
! CALL OK(v%Size() .EQ. fedof%GetTotalDOF(), "Size = total DOF")
! CALL OK(v%Size() .EQ. 6, "Size = 6")
! CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "GetTotalPhysicalVars")
! CALL v%GetPhysicalNames(names)
! CALL OK(names(1) .EQ. "v", "GetPhysicalNames")
! CALL OK(ALL(v%GetSpaceCompo(1) .EQ. [1]), "GetSpaceCompo")
! CALL OK(ALL(v%GetTimeCompo(1) .EQ. [1]), "GetTimeCompo")
! CALL OK(v%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
!         "GetStorageFMT")
!
! CALL v%DEALLOCATE()
! CALL OK(.NOT. v%IsInitiated(), "DEALLOCATE")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractNodeField_:obj_Initiate2]], [[obj_Size]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, name, engine, fieldType, &
                                  storageFMT, &
                                  comm, local_n, global_n, spaceCompo, &
                                  isSpaceCompo, isSpaceCompoScalar, &
                                  timeCompo, isTimeCompo, &
                                  isTimeCompoScalar, &
                                  tPhysicalVarNames, physicalVarNames, &
                                  isPhysicalVarNames, &
                                  isPhysicalVarNamesScalar, tNodes, &
                                  isTNodes, isTNodesScalar, tSize, fedof, &
                                  geofedof, timefedof)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
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

INTERFACE ScalarFieldInitiate
  MODULE PROCEDURE obj_Initiate2
END INTERFACE ScalarFieldInitiate

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Finalizer of ScalarField_
!
! Calls DEALLOCATE.

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-03
! summary: Deallocate a vector of scalar field pointers
!
! @note
!   The body of this routine is empty: obj is not deallocated.
! @endnote
!
! Arguments:
!   obj : TYPE(ScalarFieldPointer_) - vector of scalar field pointers
!
! See also: [[obj_ScalarFieldSafeAllocate1]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(ScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of scalar field pointers
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE

INTERFACE ScalarFieldDeallocate
  MODULE PROCEDURE obj_Deallocate_ptr_vector
END INTERFACE ScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                  ScalarFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-25
! summary: Safely allocate a vector of scalar field pointers
!
! If obj is not allocated, it is allocated with size newsize. If its
! size is less than newsize, ScalarFieldDeallocate is called and obj is
! allocated again with size newsize. Otherwise obj is not changed.
!
! Arguments:
!   obj : TYPE(ScalarFieldPointer_) - vector of scalar field pointers
!   newsize : INTEGER(I4B) - required size
!
! See also: [[obj_Deallocate_ptr_vector]]

INTERFACE
  MODULE SUBROUTINE obj_ScalarFieldSafeAllocate1(obj, newsize)
    TYPE(ScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! allocatable scalar field pointer
    INTEGER(I4B), INTENT(IN) :: newsize
    !! new size of obj
  END SUBROUTINE obj_ScalarFieldSafeAllocate1
END INTERFACE

INTERFACE ScalarFieldSafeAllocate
  MODULE PROCEDURE obj_ScalarFieldSafeAllocate1
END INTERFACE ScalarFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set a single entry of the scalar field
!
! The entry of node globalNode becomes scale * VALUE. If addContribution
! is true, then scale * VALUE is added to the entry. The default of
! scale is 1. An error is raised (in debug mode) if obj is not
! initiated.
!
! Set(VALUE) without globalNode sets all the entries (see
! [[obj_Set2]]); the example shows both.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   globalNode : INTEGER(I4B) - node number (DOF number)
!   islocal : LOGICAL(LGT) - must be true: globalNode is a local node
!     number (global numbers are not supported, checked in debug mode)
!   VALUE : REAL(DFP) - value
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! REAL(DFP) :: val, found(6)
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! ! all entries
! CALL u%Set(VALUE=1.0_DFP)
! DO ii = 1, 6
!   CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
! END DO
! CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Set all")
!
! CALL u%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
! CALL u%Get(VALUE=val, globalNode=4, islocal=math%yes)
! CALL OK(ABS(val - 3.0_DFP) .LT. tol, "Set all: add 2 * 1")
!
! ! single entry
! CALL u%Set(globalNode=2, islocal=math%yes, VALUE=5.0_DFP)
! CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
! CALL OK(ABS(val - 5.0_DFP) .LT. tol, "Set single")
!
! CALL u%Set(globalNode=2, islocal=math%yes, VALUE=5.0_DFP, &
!            scale=2.0_DFP)
! CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
! CALL OK(ABS(val - 10.0_DFP) .LT. tol, "Set single: scale")
!
! CALL u%Set(globalNode=2, islocal=math%yes, VALUE=1.0_DFP, &
!            scale=3.0_DFP, addContribution=math%yes)
! CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
! CALL OK(ABS(val - 13.0_DFP) .LT. tol, "Set single: add 3 * 1")
!
! CALL u%Get(VALUE=val, globalNode=1, islocal=math%yes)
! CALL OK(ABS(val - 3.0_DFP) .LT. tol, "other entries unchanged")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Set2]], [[obj_Get1]], [[AbstractNodeField_:obj_SetSingle]]

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if present and true then globalNode is a local node
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add contribution
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all the entries of the scalar field to a value
!
! All entries become scale * VALUE, or scale * VALUE is added to them
! if addContribution is true. It calls SetAll of AbstractNodeField_.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : REAL(DFP) - value
!   scale : REAL(DFP) - (optional) scale, default 1
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(IN) :: VALUE
    !! All values of scalar field will be set to value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add contribution (add or set)
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all the entries from a vector
!
! The entries of obj are set from VALUE (entry i from VALUE(i)). If
! addContribution is true, then scale * VALUE is added. SIZE(VALUE)
! must not be less than the number of nodes; an error is raised (in all
! builds) if obj is not initiated or if it is a constant field.
!
! Set(globalNode(:), islocal, VALUE) and Set(globalNode(:), islocal,
! VALUE(:)) set selected entries (see [[obj_Set4]] and [[obj_Set5]]);
! the example shows all three.
!
! @note
!   Without addContribution, the values are set to scale * VALUE.
! @endnote
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : REAL(DFP) - values, SIZE(VALUE) >= obj%Size()
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! ! all entries from a vector
! want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL u%Set(VALUE=want)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(tsize .EQ. 6, "Get all: tsize")
! CALL OK(ALL(ABS(found - want) .LT. tol), "Set vector")
!
! CALL u%Set(VALUE=want, scale=2.0_DFP, addContribution=math%yes)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 3.0_DFP * want) .LT. tol), &
!         "Set vector: add 2 * value")
!
! ! selected entries to one value
! CALL u%Set(VALUE=want)
! CALL u%Set(globalNode=[1, 3, 5], islocal=math%yes, VALUE=0.0_DFP)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - [0.0_DFP, 2.0_DFP, 0.0_DFP, 4.0_DFP, &
!                          0.0_DFP, 6.0_DFP]) .LT. tol), &
!         "Set selected: one value")
!
! ! selected entries to given values
! CALL u%Set(globalNode=[2, 4], islocal=math%yes, &
!            VALUE=[20.0_DFP, 40.0_DFP])
! CALL u%Get(VALUE=found, globalNode=[2, 4], islocal=math%yes, &
!            tsize=tsize)
! CALL OK(tsize .EQ. 2, "Get selected: tsize")
! CALL OK(ALL(ABS(found(1:2) - [20.0_DFP, 40.0_DFP]) .LT. tol), &
!         "Set selected: values")
!
! CALL u%Set(globalNode=[2, 4], islocal=math%yes, &
!            VALUE=[1.0_DFP, 1.0_DFP], scale=-1.0_DFP, &
!            addContribution=math%yes)
! CALL u%Get(VALUE=found, globalNode=[2, 4], islocal=math%yes, &
!            tsize=tsize)
! CALL OK(ALL(ABS(found(1:2) - [19.0_DFP, 39.0_DFP]) .LT. tol), &
!         "Set selected: add -1 * values")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Set4]], [[obj_Set5]], [[obj_Get2]], [[obj_Get3]]

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values of all entries
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
! summary: Set the selected entries to a value
!
! The entries of the nodes globalNode become scale * VALUE, or
! scale * VALUE is added to them if addContribution is true. An error
! is raised (in debug mode) if obj is not initiated or if it is a
! constant field.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   VALUE : REAL(DFP) - value
!   scale : REAL(DFP) - (optional) scale, default 1
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be assigned on globalNode
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale (if we are adding)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected entries to the given values
!
! The entry of node globalNode(i) is set from VALUE(i). If
! addContribution is true, then scale * VALUE(i) is added. SIZE(VALUE)
! must not be less than SIZE(globalNode).
!
! @note
!   Without addContribution, the values are set to scale * VALUE.
! @endnote
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   VALUE : REAL(DFP) - values, SIZE(VALUE) >= SIZE(globalNode)
!   scale : REAL(DFP) - (optional) scale, used with addContribution
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! value to be assigned on globalNode
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale (if we are adding)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected entries by using an FEVariable
!
! VALUE is a scalar, nodal FEVariable. If it is constant, all the
! selected entries are set to its value ([[obj_Set4]]). If it is a
! space variable, the entry of node globalNode(i) is set from its i-th
! value ([[obj_Set5]]). Other variable types raise an error.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   VALUE : TYPE(FEVariable_) - scalar, nodal FEVariable (constant or
!     space)
!   scale : REAL(DFP) - (optional) scale
!   addContribution : LOGICAL(LGT) - (optional) add or set, default set
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: TypeFEVariableScalar
! USE BaseType, ONLY: TypeFEVariableSpace
! USE BaseType, ONLY: TypeFEVariableConstant
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FEVariable_Method, ONLY: NodalVariable
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(FEVariable_) :: fevar
! REAL(DFP) :: found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL u%Set(VALUE=0.0_DFP)
!
! ! constant FEVariable: one value for all selected nodes
! fevar = NodalVariable(7.0_DFP, TypeFEVariableScalar, &
!                       TypeFEVariableConstant)
! CALL u%Set(globalNode=[1, 2], islocal=math%yes, VALUE=fevar)
!
! ! space FEVariable: one value for each selected node
! fevar = NodalVariable([3.0_DFP, 5.0_DFP], TypeFEVariableScalar, &
!                       TypeFEVariableSpace)
! CALL u%Set(globalNode=[5, 6], islocal=math%yes, VALUE=fevar)
!
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - [7.0_DFP, 7.0_DFP, 0.0_DFP, 0.0_DFP, &
!                          3.0_DFP, 5.0_DFP]) .LT. tol), &
!         "Set with FEVariable")
!
! ! get the selected entries in an FEVariable
! fevar = NodalVariable(6, TypeFEVariableScalar, TypeFEVariableSpace)
! CALL u%Get(VALUE=fevar, globalNode=[6, 1], islocal=math%yes)
! CALL OK(fevar%len .EQ. 2, "Get FEVariable: len")
! CALL OK(ALL(ABS(fevar%val(1:2) - [5.0_DFP, 7.0_DFP]) .LT. tol), &
!         "Get FEVariable: values")
!
! CALL u%GetFEVariable(globalNode=[5], islocal=math%yes, VALUE=fevar)
! CALL OK(ABS(fevar%val(1) - 3.0_DFP) .LT. tol, "GetFEVariable")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Get4]], [[obj_GetFeVariable]]

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! Scalar, Nodal, FEVariable (Space or Constant)
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale (if we are adding)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Copy the values of another scalar field (assignment)
!
! obj = VALUE: the values of VALUE are copied into obj. It is used for
! the assignment operator. Both fields must have the same FEDOF.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : CLASS(ScalarField_) - scalar field to be copied
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u, v
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL v%Initiate(name="v", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL u%Set(VALUE=want)
! CALL v%Set(VALUE=0.0_DFP)
!
! v = u
! CALL v%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - want) .LT. tol), "v = u")
!
! CALL v%Set(VALUE=u, scale=2.0_DFP, addContribution=math%yes)
! CALL v%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 3.0_DFP * want) .LT. tol), "v = v + 2 * u")
!
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, VALUE)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field, obj = VALUE
    CLASS(ScalarField_), INTENT(IN) :: VALUE
    !! scalar field to be copied
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set or add another scalar field: obj = obj + scale * VALUE
!
! If addContribution is true, then scale * VALUE is added to obj.
! Otherwise obj is set to scale * VALUE.
! An error is raised (in debug mode) if obj or VALUE is not initiated.
! Both fields must have the same FEDOF.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : CLASS(ScalarField_) - other scalar field
!   scale : REAL(DFP) - scale, used with addContribution
!   addContribution : LOGICAL(LGT) - add or set
!
! See also: [[obj_Set7]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CLASS(ScalarField_), INTENT(IN) :: VALUE
    !! other scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetByFunction@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Set the scalar field by using a user function
!
! For each element of the mesh, the DOF values of the element are
! computed from func by GetDOFValue of the finite element (for Lagrange
! polynomials these are the values of func at the interpolation
! points), and they are set in obj.
!
! func is a scalar user function of the space variables, with the
! arguments (x, y, z, time). The time is given by times (default 0).
!
! @note
!   This routine is valid only for the Lagrange polynomials.
! @endnote
!
! Arguments:
!   obj : CLASS(ScalarField_) - initiated scalar field
!   func : CLASS(UserFunction_) - scalar user function
!   times : REAL(DFP) - (optional) time, default 0
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(UserFunction_) :: func
! PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
! CLASS(AbstractMesh_), POINTER :: mesh
! REAL(DFP), ALLOCATABLE :: xij(:, :)
! REAL(DFP) :: found, want
! INTEGER(I4B) :: ii, nrow, ncol
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL func%Initiate(NAME="func", returnType=TypeFEVariableOpt%scalar, &
!                    argType=TypeFEVariableOpt%space, numArgs=4, &
!                    numReturns=1)
! procptr => linearFunc
! CALL func%SetScalarFunctionPointer(func=procptr)
!
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL u%Set(VALUE=0.0_DFP)
! CALL u%SetByFunction(func=func)
!
! mesh => dom%GetMeshPointer()
! ALLOCATE (xij(3, mesh%GetTotalNodes()))
! CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)
!
! isok = .TRUE.
! DO ii = 1, ncol
!   CALL u%Get(VALUE=found, globalNode=ii, islocal=math%yes)
!   CALL linearFunc(xij(:, ii), 3, want)
!   isok = isok .AND. ABS(found - want) .LT. tol
! END DO
! CALL OK(ncol .EQ. 6, "number of nodes")
! CALL OK(isok, "u = f at the nodes")
!
! NULLIFY (mesh)
! CALL u%DEALLOCATE()
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
!   ans = 1.0_DFP + 2.0_DFP * args(1) + 3.0_DFP * args(2)
! END SUBROUTINE linearFunc
!
! END PROGRAM main
!```
!
! See also: [[obj_SetLagrangeDOF]]

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction(obj, func, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! If present then its size should be 1
  END SUBROUTINE obj_SetByFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetLagrangeDOF@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-12
! summary: Set the Lagrange DOFs of obj from another scalar field
!
! For each active element, the interpolation points of obj (order and
! ipType of the element in obj%fedof) are computed in the reference
! element of obj2, obj2 is interpolated at these points, and the values
! are set as the Lagrange DOFs of obj. The orientation of the DOFs of
! obj is fixed for H1 conformity.
!
! obj must use Lagrange polynomials; obj2 can use any basis. Both
! fields must be defined on meshes with the same number of elements.
!
! Arguments:
!   obj : CLASS(ScalarField_) - Lagrange scalar field, values are set
!   obj2 : CLASS(ScalarField_) - scalar field which is interpolated
!
! See also: [[obj_SetByFunction]]

INTERFACE
  MODULE SUBROUTINE obj_SetLagrangeDOF(obj, obj2)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! values of obj will be set.
    CLASS(ScalarField_), INTENT(INOUT) :: obj2
    !! values of obj will be set from obj2
  END SUBROUTINE obj_SetLagrangeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get a single entry of the scalar field
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : REAL(DFP) - value of node globalNode
!   globalNode : INTEGER(I4B) - node number (DOF number)
!   islocal : LOGICAL(LGT) - must be true: globalNode is a local node
!     number (global numbers are not supported, checked in debug mode)
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, islocal)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! Scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value to be returned
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if present and true then globalNode is a local node
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get all the entries of the scalar field
!
! The values of all entries are written in VALUE(1:tsize).
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : REAL(DFP) - values, SIZE(VALUE) >= obj%Size()
!   tsize : INTEGER(I4B) - number of values written in VALUE
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, tsize)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! real vector which contains the values stored in scalar field
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written in Value
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the selected entries of the scalar field
!
! The value of node globalNode(i) is written in VALUE(i), and tsize is
! SIZE(globalNode).
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : REAL(DFP) - values, SIZE(VALUE) >= SIZE(globalNode)
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   tsize : INTEGER(I4B) - number of values written in VALUE
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode, islocal, tsize)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written in Value
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the selected entries in an FEVariable
!
! The values of the nodes globalNode are written in VALUE, which becomes
! a scalar, nodal, space FEVariable of length SIZE(globalNode). VALUE
! must be initiated before the call (e.g., by NodalVariable(tsize,
! TypeFEVariableScalar, TypeFEVariableSpace)) with enough capacity.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : TYPE(FEVariable_) - initiated FEVariable, values are written
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!
! See also: [[obj_Set6]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, globalNode, islocal)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! scalar field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! FEVariable, which contains nodal value of scalar
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then globalNodes are local nodes
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get the selected entries in an FEVariable
!
! Same as [[obj_Get4]]; ivar is not used for a scalar field.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   globalNode : INTEGER(I4B) - node numbers
!   islocal : LOGICAL(LGT) - must be true: globalNode are local node
!     numbers (global numbers are not supported, checked in debug mode)
!   VALUE : TYPE(FEVariable_) - initiated FEVariable, values are written
!   ivar : INTEGER(I4B) - (optional) physical variable, not used
!
! See also: [[obj_Set6]] (contains an example)

INTERFACE ScalarFieldGetFEVariable
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is a local node
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! returned value in fevariable
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ivar
    !! physical variable
  END SUBROUTINE obj_GetFeVariable
END INTERFACE ScalarFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                             Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-25
! summary: Returns the number of values of the scalar field
!
! Returns local_n, the number of values stored on this process. For a
! serial field it is the total number of DOFs of fedof.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   dims : INTEGER(I4B) - (optional) not used
!
! Returns:
!   ans : INTEGER(I4B) - number of values
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used
    INTEGER(I4B) :: ans
    !! number of values
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMeshField@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-29
! summary: Get the mesh field of the interpolated values
!
! For each element, obj is interpolated at the interpolation points of
! the given order and ipType of the element, and the values are
! inserted in meshField (scalar, nodal, space). If meshField is not
! initiated, it is initiated with the name and engine of obj, and the
! maximum number of interpolation points over the elements.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   meshField : CLASS(MeshField_) - mesh field, values are inserted
!   order : INTEGER(I4B) - order of interpolation points in x, y, z
!   ipType : INTEGER(I4B) - interpolation point type in x, y, z

INTERFACE
  MODULE SUBROUTINE obj_GetMeshField(obj, meshField, order, ipType)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
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
! date: 2026-10-27
! summary: Values at the points of the VTK unstructured grid
!
! Returns the value of obj at each point of the grid given by
! [[AbstractNodeField_:obj_GetVtkUnstructuredGridData1]], in the same
! order. The points are not shared between cells (DG like storage), so
! SIZE(VALUE) is fedof%GetSumElemTotalDOF(). VALUE is reallocated; the
! work arrays of [[obj_GetVtkUnstructuredValue2]] are allocated here.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : REAL(DFP) - values at the points, reallocated
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = &
!   "GetVtkUnstructuredGridData_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
! INTEGER(INT8), ALLOCATABLE :: types(:)
! REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:), fvals(:)
! TYPE(UserFunction_) :: func
! PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! ! user function
! CALL func%Initiate(NAME="func1", returnType=TypeFEVariableOpt%scalar, &
!                    argType=TypeFEVariableOpt%space, numArgs=4, &
!                    numReturns=1)
! procptr => scalarFunc
! CALL func%SetScalarFunctionPointer(func=procptr)
!
! ! domain and u = f
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL u%Set(math%zero)
! CALL u%SetByFunction(func=func)
!
! ! grid and values
! CALL u%GetVtkUnstructuredGridData(points=points, &
!                                   connectivity=connectivity, &
!                                   offsets=offsets, types=types)
! CALL u%GetVtkUnstructuredValue(VALUE=VALUE)
!
! CALL OK(SIZE(VALUE) .EQ. 32, "size of value")
! CALL OK(SIZE(VALUE) .EQ. SIZE(points, 2), "one value for each point")
! CALL OK(SIZE(VALUE) .EQ. fedof%GetSumElemTotalDOF(), &
!         "size = GetSumElemTotalDOF")
!
! ALLOCATE (fvals(SIZE(VALUE)))
! DO ii = 1, SIZE(VALUE)
!   CALL scalarFunc(points(:, ii), 3, fvals(ii))
! END DO
! CALL OK(MAXVAL(ABS(VALUE - fvals)) .LT. tol, "value = f at points")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
!
! CONTAINS
!
! SUBROUTINE scalarFunc(args, nargs, ans)
!   REAL(DFP), INTENT(IN) :: args(:)
!   INTEGER(I4B), INTENT(IN) :: nargs
!   REAL(DFP), INTENT(INOUT) :: ans
!   ans = args(1) * (args(1) - 10.0_DFP) * args(2) * (args(2) - 10.0_DFP)
! END SUBROUTINE scalarFunc
!
! END PROGRAM main
!```
!
! See also: [[obj_GetVtkUnstructuredValue2]],
! [[VtkHDF5File_:obj_WritePointData1]]

INTERFACE
  MODULE SUBROUTINE obj_GetVtkUnstructuredValue1(obj, VALUE)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values at the points of the grid
  END SUBROUTINE obj_GetVtkUnstructuredValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetVtkUnstructuredValue@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Values at the points of the VTK unstructured grid
!   (no allocation)
!
! Same as [[obj_GetVtkUnstructuredValue1]], but VALUE and the work
! arrays are given by the caller, so that nothing is allocated. For each
! active element, the nodal values of obj are interpolated at the VTK
! points of the cell (the interpolation points of the cell order). The
! local shape functions are used; the orientation of the element is
! taken into account by FixOrientOfNptrs on the connectivity, so the
! node coordinates of the element are not needed. FixOrientOfNptrs is
! implemented for Lagrange finite elements of quadrangles and triangles
! only. Since the local shape functions at the VTK points depend only on
! the topology and the cell order, they are computed again only when one
! of them differs from the previous element.
!
! Changes:
!   - 2026-10-29: vtkindx removed; the number of points of a cell is the
!     number of interpolation points of its order
!   - 2026-10-29: local shape functions and FixOrientOfNptrs; elemCoord
!     removed; shape data reused for the same topology and order
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   VALUE : REAL(DFP) - values, SIZE(VALUE) >= fedof%GetSumElemTotalDOF()
!   tsize : INTEGER(I4B) - number of values written in VALUE
!   refElemCoord, xij, nodalValue, fecon - work arrays, see the sizes of
!     the dummy arguments
!
! See also: [[obj_GetVtkUnstructuredValue1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetVtkUnstructuredValue2(obj, VALUE, tsize, &
                                                 refElemCoord, xij, &
                                                 nodalValue, fecon)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values at the points of the grid
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in value
    REAL(DFP), INTENT(INOUT) :: refElemCoord(:, :)
    !! temporary storage for nodal coordinate of reference element
    !! size should be at least (3, maxNNE)
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! temporal storage for nodal coordinate of interpolation points
    !! size should be at least (4, maxCon). This variable
    !! is used to construct the quadrature points
    REAL(DFP), INTENT(INOUT) :: nodalValue(:)
    !! temporal storage for element level nodal value of scalarField
    !! size should be at least maximum connectivity in fedof
    INTEGER(I4B), INTENT(INOUT) :: fecon(:)
    !! temporal storage for connectivity of an element
    !! size should be at least max connectivity in fedof
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
! from dbc%Get, and the entries of these nodes are set to the values.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   dbc : CLASS(DirichletBC_) - Dirichlet boundary condition
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplyDirichletBC2]], [[obj_ApplyZeroDirichletBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
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
! Same as [[obj_ApplyDirichletBC1]] for each associated pointer of dbc,
! in order.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   dbc : TYPE(DirichletBCPointer_) - Dirichlet boundary conditions
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplyDirichletBC1]], [[obj_ApplyDirichletBC3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
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
!   obj : CLASS(ScalarField_) - scalar field
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplyDirichletBC2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC3(obj, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time
  END SUBROUTINE obj_ApplyDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyZeroDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Apply a zero Dirichlet boundary condition
!
! The entries of the nodes of dbc (dbc%GetNodeNumber) are set to zero;
! the values of dbc are not used.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   dbc : CLASS(DirichletBC_) - Dirichlet boundary condition
!
! See also: [[obj_ApplyZeroDirichletBC2]], [[obj_ApplyDirichletBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyZeroDirichletBC1(obj, dbc)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    !! Dirichlet boundary condition
  END SUBROUTINE obj_ApplyZeroDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyZeroDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Apply a list of zero Dirichlet boundary conditions
!
! Same as [[obj_ApplyZeroDirichletBC1]] for each associated pointer of
! dbc.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   dbc : TYPE(DirichletBCPointer_) - Dirichlet boundary conditions
!
! See also: [[obj_ApplyZeroDirichletBC1]], [[obj_ApplyZeroDirichletBC3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyZeroDirichletBC2(obj, dbc)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary conditions
  END SUBROUTINE obj_ApplyZeroDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyZeroDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Apply zero values at the Dirichlet nodes of the field
!
! Calls [[obj_ApplyZeroDirichletBC2]] with obj%dbc.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!
! See also: [[obj_ApplyZeroDirichletBC2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyZeroDirichletBC3(obj)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
  END SUBROUTINE obj_ApplyZeroDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!                                         ApplyPointNeumannBC@PointNBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add the point Neumann boundary conditions
!
! For each point Neumann boundary condition of the field
! (obj%nbc_point), the node numbers and the values (at time times) are
! obtained, and scale * value is added to the entries of these nodes.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   scale : REAL(DFP) - scale
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplySurfaceNeumannBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyPointNeumannBC(obj, scale, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for neumann boundary condition
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time
  END SUBROUTINE obj_ApplyPointNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!                                     ApplySurfaceNeumannBC@SurfaceNBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add the surface Neumann boundary conditions of the field
!
! nbcField is set to zero, and the Neumann boundary conditions of the
! field are applied to it as Dirichlet values, so that nbcField is the
! extension of the boundary data to the domain. Then, for each boundary
! condition, the integral of the test functions times nbcField over the
! boundary facets is computed, and scale times this integral is added
! to obj. Nothing is done if the field has no Neumann boundary
! condition.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field (right hand side)
!   nbcField : CLASS(ScalarField_) - work field for the boundary data
!   scale : REAL(DFP) - scale
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplySurfaceNeumannBC2]], [[obj_ApplyPointNeumannBC]]

INTERFACE
  MODULE SUBROUTINE obj_ApplySurfaceNeumannBC1( &
    obj, nbcField, scale, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    CLASS(ScalarField_), INTENT(INOUT) :: nbcField
    !! Scalar field where we will keep the neumann boundary condition
    !! extension to the entire domain
    REAL(DFP), INTENT(IN) :: scale
    !! Scale for neumann boundary condition
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! times
  END SUBROUTINE obj_ApplySurfaceNeumannBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                     ApplySurfaceNeumannBC@SurfaceNBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add a list of surface Neumann boundary conditions
!
! Same as [[obj_ApplySurfaceNeumannBC1]], but the Neumann boundary
! conditions are given by nbc. Entries of nbc that are not associated
! are skipped. Nothing is done if SIZE(nbc) is zero.
!
! Changes:
!   - 2026-10-29: use nbc(ibc)%ptr, not the boundary conditions of obj
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field (right hand side)
!   nbc : TYPE(NeumannBCPointer_) - Neumann boundary conditions
!   nbcField : CLASS(ScalarField_) - work field for the boundary data
!   scale : REAL(DFP) - scale
!   times : REAL(DFP) - (optional) time
!
! See also: [[obj_ApplySurfaceNeumannBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplySurfaceNeumannBC2(obj, nbc, nbcField, scale, &
                                               times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbc(:)
    !! list of neumann boundary conditions
    CLASS(ScalarField_), INTENT(INOUT) :: nbcField
    !! Scalar field where we will keep the neumann boundary condition
    !! extension to the entire domain
    REAL(DFP), INTENT(IN) :: scale
    !! Scale for neumann boundary condition
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! times
  END SUBROUTINE obj_ApplySurfaceNeumannBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                           ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-05
! summary: Add the body source given by a user function
!
! For each element, the integral of the test functions times the body
! source over the element is computed by quadrature, and scale times
! this integral is added to obj:
!
! $$ u_I = u_I + s \int_{\Omega} N_I f(x, y, z, t) \, d\Omega $$
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field (right hand side)
!   bodySource : CLASS(UserFunction_) - scalar function of (x, y, z, t)
!   scale : REAL(DFP) - scale
!   times : REAL(DFP) - (optional) time passed to bodySource
!
! See also: [[obj_ApplyBodySource2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource1(obj, bodySource, scale, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field (right hand side)
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! Body source user function
    !! It should be a scalar function with
    !! total arguments 4 (x, y, z, time)
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time, which will be passed to the body source function
  END SUBROUTINE obj_ApplyBodySource1
END INTERFACE

INTERFACE ScalarFieldApplyBodySource
  MODULE PROCEDURE obj_ApplyBodySource1
END INTERFACE ScalarFieldApplyBodySource

!----------------------------------------------------------------------------
!                                           ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-05
! summary: Add the body source given by a scalar field
!
! Same as [[obj_ApplyBodySource1]], but the body source is the scalar
! field bodySource, which is interpolated at the quadrature points:
!
! $$ u_I = u_I + s \int_{\Omega} N_I f_h \, d\Omega $$
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field (right hand side)
!   bodySource : CLASS(ScalarField_) - body source
!   scale : REAL(DFP) - scale
!
! See also: [[obj_ApplyBodySource1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource2(obj, bodySource, scale)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    !! The test function will be corresponding to the obj
    CLASS(ScalarField_), INTENT(INOUT) :: bodySource
    !! Body source in terms of scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
  END SUBROUTINE obj_ApplyBodySource2
END INTERFACE

INTERFACE ScalarFieldApplyBodySource
  MODULE PROCEDURE obj_ApplyBodySource2
END INTERFACE ScalarFieldApplyBodySource

!----------------------------------------------------------------------------
!                                                     SetFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-10
! summary: Set the values of the field from a toml table
!
! If the table defines a user function, the field is set by it.
! Otherwise the type of the nodal value is read from the table, and the
! field is set to a constant value or to the given space values.
!
! Arguments:
!   obj : CLASS(ScalarField_) - initiated scalar field
!   table : TYPE(toml_table) - toml table with the values

INTERFACE
  MODULE SUBROUTINE obj_SetFromToml(obj, table)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table
  END SUBROUTINE obj_SetFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-01
! summary: Import the scalar field from an HDF5 file
!
! Calls Import of AbstractNodeField_.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   hdf5 : TYPE(HDF5File_) - HDF5 file opened for reading
!   group : CHARACTER(*) - group of the field in the file
!   fedof, geofedof : CLASS(FEDOF_) - (optional) FEDOFs of the field
!   fedofs, geofedofs, timefedof, timefedofs - (optional) other FEDOFs
!
! See also: [[AbstractNodeField_:obj_Export]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the file
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

INTERFACE ScalarFieldImport
  MODULE PROCEDURE obj_Import
END INTERFACE ScalarFieldImport

!----------------------------------------------------------------------------
!                                                           Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-01
! summary: Export the scalar field to an HDF5 file
!
! Calls Export of AbstractNodeField_.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   hdf5 : TYPE(HDF5File_) - HDF5 file opened for writing
!   group : CHARACTER(*) - group of the field in the file
!
! See also: [[AbstractNodeField_:obj_Export]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the file
  END SUBROUTINE obj_Export
END INTERFACE

INTERFACE ScalarFieldExport
  MODULE PROCEDURE obj_Export
END INTERFACE ScalarFieldExport

!----------------------------------------------------------------------------
!                                                      ExportToVTK@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-01
! summary: Write the vertex values in a VTK file
!
! The values of the vertex DOFs (the first fedof%GetTotalVertexDOF()
! values) are written as a data array with one component. The name of
! the array is the name of the field joined with the physical variable
! name by "_". It is called by WriteData_vtk of AbstractNodeField_.
!
! Arguments:
!   obj : CLASS(ScalarField_) - scalar field
!   vtk : TYPE(VTKFile_) - VTK file
!
! See also: [[AbstractNodeField_:obj_WriteData_vtk1]]

INTERFACE
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtk)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! node field object
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! vtkfile object
  END SUBROUTINE obj_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarField_Class
