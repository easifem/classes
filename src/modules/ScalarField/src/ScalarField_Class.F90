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
! summary: Scalar field, a node field with one physical variable
!
! Defines [[ScalarField_]] and its pointer wrapper
! [[ScalarFieldPointer_]]. `ScalarFieldDeallocate` and
! `ScalarFieldSafeAllocate` manage vectors of these pointers.

MODULE ScalarField_Class
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: FEVariable_
USE DirichletBC_Class, ONLY: DirichletBC_
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE NeumannBC_Class, ONLY: NeumannBCPointer_
USE String_Class, ONLY: String
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
USE TimeOpt_Class, ONLY: TimeOpt_
USE Tomlf, ONLY: toml_table
USE UserFunction_Class, ONLY: UserFunction_
USE VTKFile_Class, ONLY: VTKFile_
IMPLICIT NONE
PRIVATE

PUBLIC :: ScalarField_
PUBLIC :: ScalarFieldPointer_
PUBLIC :: ScalarFieldDeallocate
PUBLIC :: ScalarFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                              ScalarField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Scalar field data type
!
! A scalar field stores one real value for each degree of freedom
! (DOF) of its `fedof`. It has one physical variable. This variable
! has one space component and one time component.
!
! The values live in the storage of [[AbstractNodeField_]]. The
! storage uses the DOF format, so the value of DOF `i` is entry `i`.
!
! The type adds no components. It adds methods that work with node
! numbers.
!
! - `Initiate`, `ImportFromToml1` and `IMPORT` create the field.
! - `Set`, `SetByFunction`, `SetLagrangeDOF` and `SetFromToml` change
!   the values.
! - `ApplyDirichletBC`, `ApplyZeroDirichletBC`, `ApplyPointNeumannBC`,
!   `ApplySurfaceNeumannBC` and `ApplyBodySource` apply boundary
!   conditions and sources.
! - `Get`, `GetFEVariable`, `GetMeshField` and
!   `GetVtkUnstructuredValue` read the values.
!
! The BLAS methods of [[AbstractNodeField_]] also work on the field.
!
! @note
! Methods with `globalNode` and `islocal` take local node numbers.
! Pass `islocal = .TRUE.`. Debug builds raise an error otherwise.
! @endnote

TYPE, EXTENDS(AbstractNodeField_) :: ScalarField_
CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by copying another node field
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate from a name, an engine and FEDOFs
  GENERIC, PUBLIC :: Initiate => Initiate2
  !! Initiate a scalar field
  FINAL :: obj_Final
  !! Deallocate the field when it goes out of scope

  ! @SetMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set or add one entry
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set or add one value to all entries
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set or add all entries from a vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set or add one value to selected entries
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set or add a vector of values to selected entries
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set or add selected entries from an FEVariable
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Copy another scalar field, used by the assignment
  PROCEDURE, PUBLIC, PASS(obj) :: Set8 => obj_Set8
  !! Set `obj` to `scale * VALUE`, or add `scale * VALUE` to it
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SetByFunction => &
    obj_SetByFunction
  !! Set the values from a user function
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, &
    Set5, Set6, Set7, Set8
  !! Set the entries of the scalar field
  PROCEDURE, PUBLIC, PASS(obj) :: SetLagrangeDOF => obj_SetLagrangeDOF
  !! Set the Lagrange DOFs by interpolating another scalar field

  ! @GetMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get one entry
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get all entries in a real vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! Get selected entries in a real vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! Get selected entries in an FEVariable
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4
  !! Get the entries of the scalar field
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get selected entries in an FEVariable, same as `Get4`
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Number of values stored on this process
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshField => obj_GetMeshField
  !! Interpolate the field into a mesh field
  PROCEDURE, PRIVATE, PASS(obj) :: GetVtkUnstructuredValue1 => &
    obj_GetVtkUnstructuredValue1
  !! Values at the VTK grid points, allocates the result
  PROCEDURE, PRIVATE, PASS(obj) :: GetVtkUnstructuredValue2 => &
    obj_GetVtkUnstructuredValue2
  !! Values at the VTK grid points, without allocation
  GENERIC, PUBLIC :: GetVtkUnstructuredValue => GetVtkUnstructuredValue1, &
    GetVtkUnstructuredValue2
  !! Values at the points of the VTK unstructured grid

  ! @DBCMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC1 => &
    obj_ApplyDirichletBC1
  !! Apply one Dirichlet boundary condition
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC2 => &
    obj_ApplyDirichletBC2
  !! Apply a list of Dirichlet boundary conditions
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC3 => &
    obj_ApplyDirichletBC3
  !! Apply the Dirichlet boundary conditions of the field
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    ApplyDirichletBC2, ApplyDirichletBC3
  !! Apply Dirichlet boundary conditions
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyZeroDirichletBC1 => obj_ApplyZeroDirichletBC1
  !! Set zero at the nodes of one Dirichlet boundary condition
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyZeroDirichletBC2 => obj_ApplyZeroDirichletBC2
  !! Set zero at the nodes of a list of Dirichlet conditions
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    ApplyZeroDirichletBC3 => obj_ApplyZeroDirichletBC3
  !! Set zero at the Dirichlet nodes of the field
  GENERIC, PUBLIC :: ApplyZeroDirichletBC => ApplyZeroDirichletBC1, &
    ApplyZeroDirichletBC2, ApplyZeroDirichletBC3
  !! Set zero values at the Dirichlet nodes

  ! @PointNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: ApplyPointNeumannBC => &
    obj_ApplyPointNeumannBC
  !! Add the point Neumann boundary conditions of the field

  ! @SurfaceNBCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    ApplySurfaceNeumannBC1 => obj_ApplySurfaceNeumannBC1
  !! Add the surface Neumann boundary conditions of the field
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    ApplySurfaceNeumannBC2 => obj_ApplySurfaceNeumannBC2
  !! Add a given list of surface Neumann boundary conditions
  GENERIC, PUBLIC :: ApplySurfaceNeumannBC => ApplySurfaceNeumannBC1, &
    ApplySurfaceNeumannBC2
  !! Add surface Neumann boundary conditions

  ! @BodySourceMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyBodySource1 => &
    obj_ApplyBodySource1
  !! Add a body source given by a user function
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyBodySource2 => &
    obj_ApplyBodySource2
  !! Add a body source given by a scalar field
  GENERIC, PUBLIC :: ApplyBodySource => ApplyBodySource1, ApplyBodySource2
  !! Add a body source

  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the field from an HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the field to an HDF5 file

  ! @VTKMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
  !! Write the vertex values in a VTK file

  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Initiate the field from a toml table
  PROCEDURE, PUBLIC, PASS(obj) :: SetFromToml => obj_SetFromToml
  !! Set the values from a toml table
END TYPE ScalarField_

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-17
! summary: Pointer to a scalar field
!
! Holds a pointer to a [[ScalarField_]]. Use it to build vectors of
! scalar fields.
!
! See also: [[obj_ScalarFieldSafeAllocate1]],
! [[obj_Deallocate_ptr_vector]]

TYPE :: ScalarFieldPointer_
  CLASS(ScalarField_), POINTER :: ptr => NULL()
  !! pointer to a scalar field
END TYPE ScalarFieldPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Initiate a scalar field by copying another node field
!
! Makes `obj` a copy of `obj2`.
!
! 1. If `obj` is initiated, it deallocates `obj` first.
! 2. It copies the name, the engine, the DOF data and the values.
! 3. It shares the FEDOFs and the boundary conditions by pointers.
!
! The values come from the Fortran vector `realVec` of `obj2`.
!
! @warning
! A LIS field such as `ScalarFieldLis_` does not keep `realVec` up to
! date. To copy its current values, initiate `obj` first. Then use
! `obj = obj2`.
! @endwarning
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
!   USE ScalarField_Class, ONLY: ScalarField_
!   USE Test_Method, ONLY: OK
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
!   REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!   TYPE(FEDomain_) :: dom
!   TYPE(FEDOF_), TARGET :: fedof, geofedof
!   TYPE(ScalarField_) :: x, y
!   REAL(DFP) :: src(6), found(6)
!   INTEGER(I4B) :: tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!   CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
!   CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                         filename=tomlFileName, tomlName="u")
!
!   src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!   CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
!
!   CALL y%Initiate(obj2=x)
!   CALL OK(y%IsInitiated(), "Initiate: IsInitiated")
!   CALL OK(y%Size() .EQ. x%Size(), "Initiate: Size")
!   CALL OK(y%GetTotalPhysicalVars() .EQ. 1, "Initiate: physical vars")
!   CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - src) .LT. tol), "Initiate: values")
!
!   CALL y%DEALLOCATE()
!   CALL OK(.NOT. y%IsInitiated(), "DEALLOCATE: not initiated")
!   CALL OK(y%Size() .EQ. 0, "DEALLOCATE: Size = 0")
!   CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - src) .LT. tol), "DEALLOCATE: x unchanged")
!
!   CALL x%DEALLOCATE()
!   CALL fedof%DEALLOCATE()
!   CALL geofedof%DEALLOCATE()
!   CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! [test 1](|classes|AbstractNodeField/examples/Initiate_test_1.F90)
!
! See also: [[obj_Initiate2]], [[obj_Set7]],
! [[AbstractNodeField_:obj_Initiate1]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field to initiate
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! field to copy, a child of `AbstractNodeField_`
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyFull
    !! not used at present
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyStructure
    !! not used at present
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: usePointer
    !! not used at present
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-19
! summary: Initiate a scalar field by passing arguments
!
! Creates a scalar field on `fedof`.
!
! 1. It deallocates the data stored in `obj`.
! 2. It sets one physical variable. The first character of `name`
!    names it. It has one space component and one time component.
! 3. It sets the number of values to the total number of DOFs of
!    `fedof`.
! 4. It stores pointers to `fedof` and `geofedof`.
!
! `ScalarFieldLis_` overrides this method with the same arguments.
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
! CHARACTER(*), PARAMETER :: tomlFileName = "Initiate_test_1.toml"
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u, v
! CHARACTER(1) :: names(1)
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! CALL OK(v%IsInitiated(), "IsInitiated")
! CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF(), "Size = total DOF")
! CALL OK(v%SIZE() .EQ. 6, "Size = 6")
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
! [test 1](|classes|ScalarField/examples/Initiate_test_1.F90)
! [test 2](|classes|ScalarField/examples/Initiate_test_2.F90)
!
! See also: [[AbstractNodeField_:obj_Initiate2]], [[obj_Size]],
! [[obj_ImportFromToml1]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, name, engine, fedof, geofedof, &
                                  fieldType, comm, local_n, global_n)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field; its first character names the variable
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine, for example `"NATIVE_SERIAL"`
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    !! FEDOF of the field; `obj` keeps a pointer to it
    CLASS(FEDOF_), TARGET, INTENT(IN) :: geofedof
    !! FEDOF of the geometry; `obj` keeps a pointer to it
    INTEGER(I4B), INTENT(IN), OPTIONAL :: fieldType
    !! field type, `TypeFieldOpt%normal` (default) or
    !! `TypeFieldOpt%constant`
    INTEGER(I4B), INTENT(IN), OPTIONAL :: comm
    !! communication group, needed only in parallel runs
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_n
    !! number of values on this process, needed only in parallel runs
    INTEGER(I4B), INTENT(IN), OPTIONAL :: global_n
    !! number of values on all processes, needed only in parallel runs
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Finalizer of ScalarField_
!
! Deallocates `obj` when it goes out of scope.
!
! See also: [[AbstractNodeField_:obj_Deallocate]]

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
! Deallocates the vector `obj` and the fields it points to.
!
! 1. For each associated pointer, it calls `DEALLOCATE` of the field.
!    Then it nullifies the pointer.
! 2. It deallocates `obj`.
!
! It does nothing if `obj` is not allocated.
!
! @warning
! It deallocates the data of the target fields. Other pointers to
! these fields see empty fields afterwards.
! @endwarning
!
! See also: [[obj_ScalarFieldSafeAllocate1]]

INTERFACE ScalarFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(ScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of scalar field pointers; unallocated on return
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE ScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                  ScalarFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-25
! summary: Safely allocate a vector of scalar field pointers
!
! Makes sure that `obj` has at least `newsize` entries.
!
! - If `obj` is not allocated, it allocates `obj` with size `newsize`.
! - If `SIZE(obj)` is less than `newsize`, it calls
!   `ScalarFieldDeallocate`. Then it allocates `obj` with size
!   `newsize`.
! - Otherwise it does not change `obj`.
!
! @warning
! When `obj` is too small, `ScalarFieldDeallocate` also deallocates
! the fields it points to.
! @endwarning
!
! See also: [[obj_Deallocate_ptr_vector]]

INTERFACE ScalarFieldSafeAllocate
  MODULE SUBROUTINE obj_ScalarFieldSafeAllocate1(obj, newsize)
    TYPE(ScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of scalar field pointers
    INTEGER(I4B), INTENT(IN) :: newsize
    !! smallest size of `obj` on return
  END SUBROUTINE obj_ScalarFieldSafeAllocate1
END INTERFACE ScalarFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set a single entry of the scalar field
!
! Sets the entry of node `globalNode` to `scale * VALUE`.
! The default of `scale` is 1.
!
! If `addContribution` is true, it adds `scale * VALUE` to the entry
! instead.
!
! `globalNode` is a local node number. Pass `islocal = .TRUE.`.
!
! In debug builds, it raises an error in these cases.
!
! - `obj` is not initiated.
! - `islocal` is `.FALSE.`.
!
! `Set(VALUE)` without `globalNode` sets all entries. See
! [[obj_Set2]]. The example shows both calls.
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "Set_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! REAL(DFP) :: val, found(6)
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! ! all entries
! CALL u%Set(VALUE=math%one)
! DO ii = 1, 6
!   CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
! END DO
! CALL OK(ALL(ABS(found - math%one) .LE. tol), "Set all: ")
!
! CALL u%Set(VALUE=math%one, scale=math%two, addContribution=math%yes)
! CALL u%Get(VALUE=val, globalNode=4, islocal=math%yes)
! CALL OK(ABS(val - math%three) .LE. tol, "Set all: add 2 * 1")
!
! ! Single entry
! ! Make sure islocal is true
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
! [test 1](|classes|ScalarField/examples/Set_test_1.F90)
!
! See also: [[obj_Set2]], [[obj_Get1]],
! [[AbstractNodeField_:obj_SetSingle]]

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be `.TRUE.`
    REAL(DFP), INTENT(IN) :: VALUE
    !! value of the entry
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! factor for `VALUE`, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! if true, add to the entry; default false
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all the entries of the scalar field to a value
!
! Sets every entry of the field to `scale * VALUE`.
! The default of `scale` is 1.
!
! If `addContribution` is true, it adds `scale * VALUE` to every
! entry instead.
!
! The example of [[obj_Set1]] shows this call.
!
! [test 1](|classes|ScalarField/examples/Set_test_1.F90)
!
! See also: [[obj_Set1]], [[AbstractNodeField_:obj_SetAll]]

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(IN) :: VALUE
    !! value of all entries
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! factor for `VALUE`, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! if true, add to the entries; default false
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set all the entries from a vector
!
! Sets entry `i` of the field to `scale * VALUE(i)`.
! The default of `scale` is 1.
!
! If `addContribution` is true, it adds `scale * VALUE(i)` to entry
! `i` instead.
!
! `SIZE(VALUE)` must be at least the number of DOFs.
!
! In debug builds, it raises an error in these cases.
!
! - `obj` is not initiated.
! - `obj` is a constant field.
! - `VALUE` is too short.
!
! `Set(globalNode, islocal, VALUE)` sets selected entries. See
! [[obj_Set4]] for a scalar `VALUE` and [[obj_Set5]] for a vector.
! The example shows all three calls.
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "Set_test_2.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
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
! ! Selected entries to one value
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
! [test 2](|classes|ScalarField/examples/Set_test_2.F90)
!
! See also: [[obj_Set4]], [[obj_Set5]], [[obj_Get2]], [[obj_Get3]]

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values of all entries, `SIZE(VALUE) >= obj%Size()`
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! factor for `VALUE`, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! if true, add to the entries; default false
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected entries to a value
!
! Sets the entry of each node in `globalNode` to `scale * VALUE`.
! The default of `scale` is 1.
!
! If `addContribution` is true, it adds `scale * VALUE` to these
! entries instead.
!
! `globalNode` holds local node numbers. Pass `islocal = .TRUE.`.
!
! In debug builds, it raises an error in these cases.
!
! - `obj` is not initiated.
! - `obj` is a constant field.
! - `islocal` is `.FALSE.`.
!
! The example of [[obj_Set3]] shows this call.
!
! [test 2](|classes|ScalarField/examples/Set_test_2.F90)
!
! See also: [[obj_Set3]], [[obj_Set5]]

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be `.TRUE.`
    REAL(DFP), INTENT(IN) :: VALUE
    !! value of the selected entries
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! factor for `VALUE`, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! if true, add to the entries; default false
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected entries to the given values
!
! Sets the entry of node `globalNode(i)` to `scale * VALUE(i)`.
! The default of `scale` is 1.
!
! If `addContribution` is true, it adds `scale * VALUE(i)` to the
! entry instead.
!
! `globalNode` holds local node numbers. Pass `islocal = .TRUE.`.
! `SIZE(VALUE)` must be at least `SIZE(globalNode)`.
!
! In debug builds, it raises an error in these cases.
!
! - `obj` is not initiated.
! - `obj` is a constant field.
! - `VALUE` is too short.
! - `islocal` is `.FALSE.`.
!
! The example of [[obj_Set3]] shows this call.
!
! [test 2](|classes|ScalarField/examples/Set_test_2.F90)
!
! See also: [[obj_Set3]], [[obj_Set4]]

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be `.TRUE.`
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `VALUE(i)` is the value of node `globalNode(i)`,
    !! `SIZE(VALUE) >= SIZE(globalNode)`
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! factor for `VALUE`, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! if true, add to the entries; default false
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set the selected entries by using an FEVariable
!
! Sets the entries of the nodes in `globalNode` from `VALUE`.
! `VALUE` is a scalar, nodal [[FEVariable_]].
!
! - If `VALUE` is constant, every selected entry gets its value. See
!   [[obj_Set4]].
! - If `VALUE` is a space variable, node `globalNode(i)` gets its
!   `i`-th value. See [[obj_Set5]]. `VALUE` must have at least
!   `SIZE(globalNode)` values.
!
! `scale` and `addContribution` work as in [[obj_Set4]].
! `globalNode` holds local node numbers. Pass `islocal = .TRUE.`.
!
! Debug builds raise an error for other variable types. Release
! builds leave `obj` unchanged then.
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
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FEVariable_Method, ONLY: NodalVariable
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "Set_test_3.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(FEVariable_) :: fevar
! REAL(DFP) :: want(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
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
! want = [7.0_DFP, 7.0_DFP, 0.0_DFP, 0.0_DFP, 3.0_DFP, 5.0_DFP]
! CALL OK(ALL(ABS(found - want) .LT. tol), "Set with FEVariable")
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
! [test 3](|classes|ScalarField/examples/Set_test_3.F90)
!
! See also: [[obj_Get4]], [[obj_GetFeVariable]]

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be `.TRUE.`
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! scalar, nodal FEVariable, constant or space
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! factor for `VALUE`, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! if true, add to the entries; default false
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Copy the values of another scalar field
!
! Copies the values of `VALUE` into `obj`. Call it as
! `CALL obj%Set(VALUE=VALUE)`. Do not write `obj = VALUE`: since
! 2026-11-18 it is not bound to this method and is the intrinsic
! assignment, a shallow copy of the FEDOF pointers and lis_ptr.
!
! `obj` must be initiated. Both fields must have the same DOF
! layout. Nothing checks this.
! `VALUE` can use any engine.
!
! It does the same as `Set(VALUE, scale=1, addContribution=.FALSE.)`.
! See [[obj_Set8]].
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "Set_test_4.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u, v
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
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
! [test 4](|classes|ScalarField/examples/Set_test_4.F90)
!
! See also: [[obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, VALUE)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! initiated scalar field, receives the values
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
    !! scalar field to copy
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set or add another scalar field
!
! Sets `obj` to `scale * VALUE`.
!
! If `addContribution` is true, it adds `scale * VALUE` to `obj`
! instead.
!
! Both fields must be initiated. Debug builds check this.
! Both fields must have the same DOF layout. Nothing checks this.
!
! `VALUE` can use any engine, native or LIS. This method also
! moves values between engines.
!
! Changes:
!   - 2026-11-13: `VALUE` can be a `ScalarFieldLis_`. Before,
!     `GetPointer` returned `NULL` and `obj%SetMultiple` dereferenced it.
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
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL u%Set(VALUE=want)
!
! ! Test 1: Size
! BLOCK
!   CALL OK(u%SIZE() .EQ. 6, "u%Size()")
!   CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF(), "v%Size() = total DOF")
! END BLOCK
!
! ! Test 2: v = scale * u, the old values of v are discarded
! BLOCK
!   CALL v%Set(VALUE=9.0_DFP)
!   CALL v%Set(VALUE=u, scale=-3.0_DFP, addContribution=math%no)
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(tsize .EQ. 6, "tsize")
!   CALL OK(ALL(ABS(found + 3.0_DFP * want) .LT. tol), "v = -3 * u")
! END BLOCK
!
! ! Test 3: v = v - u
! BLOCK
!   CALL v%Set(VALUE=u, scale=-1.0_DFP, addContribution=math%yes)
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found + 4.0_DFP * want) .LT. tol), "v = v - u")
! END BLOCK
!
! ! Test 4: u is not changed
! BLOCK
!   CALL u%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
! END BLOCK
!
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! [test 5](|classes|ScalarField/examples/Set_test_5.F90)
! [test 4](|classes|ScalarFieldLis/examples/Set_test_4.F90)
!
! See also: [[obj_Set7]]

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, VALUE, scale, addContribution)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
    !! other scalar field, any engine;
    !! must not be the same object as `obj`
    REAL(DFP), INTENT(IN) :: scale
    !! factor for `VALUE`
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add to `obj`; if false, overwrite `obj`
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetByFunction@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Set the scalar field by using a user function
!
! Sets the DOF values of the field from the user function `func`.
! For Lagrange elements, these are the values of `func` at the
! interpolation points.
!
! `func` is a scalar user function with four arguments `(x, y, z, t)`.
! `times` gives `t`. The default of `times` is 0.
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
! CHARACTER(*), PARAMETER :: tomlFileName = "./toml/SetByFunction_test_1.toml"
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
! [test 1](|classes|ScalarField/examples/SetByFunction_test_1.F90)
! [test 2](|classes|ScalarField/examples/SetByFunction_test_2.F90)
!
! See also: [[obj_SetLagrangeDOF]]

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction(obj, func, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! scalar user function of `(x, y, z, t)`
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time `t` passed to `func`, default 0
  END SUBROUTINE obj_SetByFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetLagrangeDOF@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-12
! summary: Set the Lagrange DOFs of obj from another scalar field
!
! Interpolates `obj2` and stores the result as the Lagrange DOFs of
! `obj`. It skips the elements that are inactive in the mesh of `obj2`.
! For each active element, it does these steps.
!
! 1. It finds the interpolation points of `obj` in the reference
!    element of `obj2`. They use the cell order and `ipType` of `obj`.
! 2. It interpolates `obj2` at these points.
! 3. It sets the results as the DOF values of `obj` in this element.
!
! The DOFs follow the orientation of each element of `obj`. This keeps
! the result continuous across elements.
!
! `obj` must use Lagrange polynomials. `obj2` can use any basis.
! The two fields can use different FEDOFs. Their meshes must have the
! same elements in the same order. Debug builds check the number of
! elements.
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
! USE GlobalData, ONLY: INT8
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "SetLagrangeDOF_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof, fedof2, geofedof2
! TYPE(ScalarField_) :: u, w
! INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
! INTEGER(INT8), ALLOCATABLE :: types(:)
! REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:), found(:)
! REAL(DFP) :: xij(2, 6), uval(6)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL w%ImportFromToml(fedof=fedof2, geofedof=geofedof2, dom=dom, &
!                       filename=tomlFileName, tomlName="w")
!
! ! Test 1: sizes of the two fields
! CALL OK(u%SIZE() .EQ. 6, "size of u")
! CALL OK(w%SIZE() .EQ. 15, "size of w")
!
! ! Test 2: a constant field gives constant Lagrange DOFs
! BLOCK
!   CALL u%Set(VALUE=4.0_DFP)
!   CALL w%Set(VALUE=0.0_DFP)
!   CALL w%SetLagrangeDOF(obj2=u)
!   ALLOCATE (found(w%SIZE()))
!   CALL w%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found(1:tsize) - 4.0_DFP) .LT. tol), "constant")
! END BLOCK
!
! ! Test 3: bilinear field, compared at the VTK points of w
! BLOCK
!   xij(1, :) = [0.0_DFP, 10.0_DFP, 10.0_DFP, 0.0_DFP, 5.0_DFP, 5.0_DFP]
!   xij(2, :) = [0.0_DFP, 0.0_DFP, 10.0_DFP, 10.0_DFP, 0.0_DFP, 10.0_DFP]
!   DO ii = 1, 6
!     uval(ii) = f(xij(:, ii))
!   END DO
!   CALL u%Set(VALUE=uval)
!   CALL w%Set(VALUE=0.0_DFP)
!   CALL w%SetLagrangeDOF(obj2=u)
!
!   CALL w%GetVtkUnstructuredGridData(points=points, &
!                                     connectivity=connectivity, &
!                                     offsets=offsets, types=types)
!   CALL w%GetVtkUnstructuredValue(VALUE=VALUE)
!   CALL OK(SIZE(VALUE) .EQ. SIZE(points, 2), "one value for each point")
!   DO ii = 1, SIZE(VALUE)
!     VALUE(ii) = VALUE(ii) - f(points(1:2, ii))
!   END DO
!   CALL OK(MAXVAL(ABS(VALUE)) .LT. tol, "w = f at VTK points")
! END BLOCK
!
! CALL w%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof2%DEALLOCATE()
! CALL geofedof2%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
!
! CONTAINS
!
! PURE FUNCTION f(x) RESULT(ans)
!   REAL(DFP), INTENT(IN) :: x(2)
!   REAL(DFP) :: ans
!   ans = 1.0_DFP + 2.0_DFP * x(1) + 3.0_DFP * x(2) + 0.1_DFP * x(1) * x(2)
! END FUNCTION f
!
! END PROGRAM main
!```
!
! [test 1](|classes|ScalarField/examples/SetLagrangeDOF_test_1.F90)
!
! See also: [[obj_SetByFunction]]

INTERFACE
  MODULE SUBROUTINE obj_SetLagrangeDOF(obj, obj2)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field with Lagrange polynomials; receives the values
    CLASS(ScalarField_), INTENT(INOUT) :: obj2
    !! scalar field to interpolate; its values do not change
  END SUBROUTINE obj_SetLagrangeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get a single entry of the scalar field
!
! Returns the entry of node `globalNode` in `VALUE`.
!
! `globalNode` is a local node number. Pass `islocal = .TRUE.`.
! Debug builds raise an error otherwise.
!
! See also: [[obj_Set1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, islocal)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value of the entry
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be `.TRUE.`
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get all the entries of the scalar field
!
! Writes all entries of the field in `VALUE(1:tsize)`.
! `tsize` is the number of DOFs.
!
! `SIZE(VALUE)` must be at least `obj%Size()`.
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, tsize)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values of all entries, `SIZE(VALUE) >= obj%Size()`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in `VALUE`
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the selected entries of the scalar field
!
! Writes the entry of node `globalNode(i)` in `VALUE(i)`.
! `tsize` returns `SIZE(globalNode)`.
!
! `globalNode` holds local node numbers. Pass `islocal = .TRUE.`.
! Debug builds raise an error otherwise.
!
! See also: [[obj_Set3]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode, islocal, tsize)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values of the selected entries,
    !! `SIZE(VALUE) >= SIZE(globalNode)`
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be `.TRUE.`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in `VALUE`
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Get the selected entries in an FEVariable
!
! Writes the entries of the nodes in `globalNode` in `VALUE`.
! `VALUE` becomes a scalar, nodal, space [[FEVariable_]]. Its length is
! `SIZE(globalNode)`.
!
! @note
! `VALUE` must be initiated before the call. It needs room for
! `SIZE(globalNode)` values. For example, create it by
! `NodalVariable(n, TypeFEVariableScalar, TypeFEVariableSpace)`.
! Debug builds check that `VALUE` is initiated.
! @endnote
!
! `globalNode` holds local node numbers. Pass `islocal = .TRUE.`.
! Debug builds raise an error otherwise.
!
! See also: [[obj_Set6]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, globalNode, islocal)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! initiated FEVariable; returns the nodal values
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be `.TRUE.`
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get the selected entries in an FEVariable
!
! Does the same as [[obj_Get4]]. A scalar field has one physical
! variable, so `ivar` is not used.
!
! See also: [[obj_Set6]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be `.TRUE.`
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! initiated FEVariable; returns the nodal values
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ivar
    !! not used
  END SUBROUTINE obj_GetFeVariable
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-25
! summary: Returns the number of values of the scalar field
!
! Returns the number of values stored on this process. For a serial
! field, this is the total number of DOFs of `fedof`.
!
! Returns:
!   ans : INTEGER(I4B) - number of values
!
! See also: [[obj_Set8]] (contains an example), [[obj_Initiate2]]

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(ScalarField_), INTENT(IN) :: obj
    !! scalar field
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used
    INTEGER(I4B) :: ans
    !! number of values on this process
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetMeshField@MeshFieldMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-29
! summary: Get the mesh field of the interpolated values
!
! Interpolates the field into `meshField`. For each element, it does
! these steps.
!
! 1. It finds the interpolation points of the given `order` and
!    `ipType`.
! 2. It interpolates the field at these points.
! 3. It inserts the values in `meshField` for this element.
!
! If `meshField` is not initiated, this method initiates it. The mesh
! field gets the name and the engine of `obj`. It holds scalar, nodal,
! space values. Each element has room for the largest number of
! interpolation points over all elements.
!
! See also: [[MeshField_]]

INTERFACE
  MODULE SUBROUTINE obj_GetMeshField(obj, meshField, order, ipType)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CLASS(MeshField_), INTENT(INOUT) :: meshField
    !! mesh field; receives the values of each element
    INTEGER(I4B), INTENT(IN) :: order(3), ipType(3)
    !! order and type of the interpolation points in each direction
  END SUBROUTINE obj_GetMeshField
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetVtkUnstructuredValue@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Values at the points of the VTK unstructured grid
!
! Returns the value of the field at each point of the VTK grid.
! [[AbstractNodeField_:obj_GetVtkUnstructuredGridData1]] gives this
! grid. The values follow the same point order.
!
! Each cell has its own copy of its points. Thus `SIZE(VALUE)` is
! `fedof%GetSumElemTotalDOF()`. This method reallocates `VALUE`.
!
! [[obj_GetVtkUnstructuredValue2]] describes the steps and the limits.
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
!                            "GetVtkUnstructuredGridData_test_1.toml"
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
! [test 1](|classes|ScalarField/examples/GetVtkUnstructuredGridValue_test_1.F90)
! [test 2](|classes|ScalarField/examples/GetVtkUnstructuredGridValue_test_2.F90)
!
! See also: [[obj_GetVtkUnstructuredValue2]],
! [[VtkHDF5File_:obj_WritePointData1]]

INTERFACE
  MODULE SUBROUTINE obj_GetVtkUnstructuredValue1(obj, VALUE)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values at the points of the grid; reallocated
  END SUBROUTINE obj_GetVtkUnstructuredValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetVtkUnstructuredValue@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Values at the points of the VTK unstructured grid
!   (no allocation)
!
! Does the same as [[obj_GetVtkUnstructuredValue1]], but the caller
! gives `VALUE` and the work arrays. This method allocates nothing.
!
! It skips inactive elements. For each active element, it does these
! steps.
!
! 1. It takes the VTK points of the cell. These are the equidistant
!    interpolation points of the cell order.
! 2. It interpolates the nodal values of the element at these points.
! 3. It writes the values in `VALUE`, after those of the previous
!    cells.
!
! @note
! The method fixes the orientation of each element on its
! connectivity. This works only for Lagrange elements on quadrangles
! and triangles.
! @endnote
!
! Debug builds check the sizes of `VALUE` and the work arrays.
!
! Changes:
!   - 2026-10-29: vtkindx removed; the number of points of a cell is the
!     number of interpolation points of its order
!   - 2026-10-29: local shape functions and FixOrientOfNptrs; elemCoord
!     removed; shape data reused for the same topology and order
!
! See also: [[obj_GetVtkUnstructuredValue1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetVtkUnstructuredValue2(obj, VALUE, tsize, &
                                                 refElemCoord, xij, &
                                                 nodalValue, fecon)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values at the points of the grid,
    !! `SIZE(VALUE) >= fedof%GetSumElemTotalDOF()`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in `VALUE`
    REAL(DFP), INTENT(INOUT) :: refElemCoord(:, :)
    !! work array for the nodes of the reference element,
    !! shape at least `(3, geofedof%GetMaxTotalConnectivity())`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! work array for the VTK points of a cell,
    !! shape at least `(4, fedof%GetMaxTotalConnectivity())`
    REAL(DFP), INTENT(INOUT) :: nodalValue(:)
    !! work array for the nodal values of an element,
    !! size at least `fedof%GetMaxTotalConnectivity()`
    INTEGER(I4B), INTENT(INOUT) :: fecon(:)
    !! work array for the connectivity of an element,
    !! size at least `fedof%GetMaxTotalConnectivity()`
  END SUBROUTINE obj_GetVtkUnstructuredValue2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a Dirichlet boundary condition
!
! Sets the entries at the nodes of `dbc` to the values of `dbc`.
! The new values replace the old ones. `dbc` evaluates its values at
! time `times`.
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ApplyDirichletBC_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(DirichletBCPointer_), ALLOCATABLE :: extDBC(:)
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL DirichletBCImportFromToml(obj=extDBC, dom=dom, tomlName="extDBC", &
!                                filename=tomlFileName)
!
! ! Test 1: internal list of Dirichlet BCs
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%ApplyDirichletBC(times=0.0_DFP)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   want = [1.0_DFP, 2.0_DFP, 2.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(ALL(ABS(found - want) .LT. tol), "internal dbc")
! END BLOCK
!
! ! Test 2: vector of DirichletBCPointer_
! BLOCK
!   CALL OK(SIZE(extDBC) .EQ. 1, "size of extDBC")
!   CALL u%Set(VALUE=-1.0_DFP)
!   CALL u%ApplyDirichletBC(dbc=extDBC, times=0.0_DFP)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   want = [7.0_DFP, 7.0_DFP, -1.0_DFP, -1.0_DFP, 7.0_DFP, -1.0_DFP]
!   CALL OK(ALL(ABS(found - want) .LT. tol), "dbc vector")
! END BLOCK
!
! ! Test 3: single DirichletBC_
! BLOCK
!   CALL u%Set(VALUE=3.0_DFP)
!   CALL u%ApplyDirichletBC(dbc=extDBC(1)%ptr, times=0.0_DFP)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   want = [7.0_DFP, 7.0_DFP, 3.0_DFP, 3.0_DFP, 7.0_DFP, 3.0_DFP]
!   CALL OK(ALL(ABS(found - want) .LT. tol), "single dbc")
! END BLOCK
!
! CALL DirichletBCDeallocate(extDBC)
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! [test 1](|classes|ScalarField/examples/ApplyDirichletBC_test_1.F90)
!
! See also: [[obj_ApplyDirichletBC2]], [[obj_ApplyDirichletBC3]],
! [[obj_ApplyZeroDirichletBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    !! Dirichlet boundary condition
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time at which `dbc` gives its values
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a list of Dirichlet boundary conditions
!
! Applies each condition in `dbc`, in order, as
! [[obj_ApplyDirichletBC1]] does. It skips pointers that are not
! associated. If two conditions share a node, the last one sets it.
!
! See also: [[obj_ApplyDirichletBC1]] (contains an example),
! [[obj_ApplyDirichletBC3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary conditions
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time at which the conditions give their values
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply the Dirichlet boundary conditions of the field
!
! Applies the Dirichlet boundary conditions stored in the field,
! `obj%dbc`. [[obj_ImportFromToml1]] reads them from the toml table.
! See [[obj_ApplyDirichletBC2]].
!
! See also: [[obj_ApplyDirichletBC1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC3(obj, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time at which the conditions give their values
  END SUBROUTINE obj_ApplyDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyZeroDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Apply a zero Dirichlet boundary condition
!
! Sets the entries at the nodes of `dbc` to zero. It does not use the
! values of `dbc`.
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ApplyDirichletBC_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(DirichletBCPointer_), ALLOCATABLE :: extDBC(:)
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL DirichletBCImportFromToml(obj=extDBC, dom=dom, tomlName="extDBC", &
!                                filename=tomlFileName)
!
! ! Test 1: internal list of Dirichlet BCs
! BLOCK
!   CALL u%Set(VALUE=3.0_DFP)
!   CALL u%ApplyZeroDirichletBC()
!   CALL u%Get(VALUE=found, tsize=tsize)
!   want = [0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 3.0_DFP, 3.0_DFP]
!   CALL OK(ALL(ABS(found - want) .LT. tol), "internal dbc")
! END BLOCK
!
! ! Test 2: vector of DirichletBCPointer_
! BLOCK
!   CALL u%Set(VALUE=3.0_DFP)
!   CALL u%ApplyZeroDirichletBC(dbc=extDBC)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   want = [0.0_DFP, 0.0_DFP, 3.0_DFP, 3.0_DFP, 0.0_DFP, 3.0_DFP]
!   CALL OK(ALL(ABS(found - want) .LT. tol), "dbc vector")
! END BLOCK
!
! ! Test 3: single DirichletBC_
! BLOCK
!   CALL u%Set(VALUE=-2.0_DFP)
!   CALL u%ApplyZeroDirichletBC(dbc=extDBC(1)%ptr)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   want = [0.0_DFP, 0.0_DFP, -2.0_DFP, -2.0_DFP, 0.0_DFP, -2.0_DFP]
!   CALL OK(ALL(ABS(found - want) .LT. tol), "single dbc")
! END BLOCK
!
! CALL DirichletBCDeallocate(extDBC)
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! [test 1](|classes|ScalarField/examples/ApplyZeroDirichletBC_test_1.F90)
!
! See also: [[obj_ApplyZeroDirichletBC2]],
! [[obj_ApplyZeroDirichletBC3]], [[obj_ApplyDirichletBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyZeroDirichletBC1(obj, dbc)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    !! Dirichlet boundary condition; gives only its nodes
  END SUBROUTINE obj_ApplyZeroDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyZeroDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Apply a list of zero Dirichlet boundary conditions
!
! Sets the entries at the nodes of each condition in `dbc` to zero,
! as [[obj_ApplyZeroDirichletBC1]] does. It skips pointers that are
! not associated.
!
! See also: [[obj_ApplyZeroDirichletBC1]] (contains an example),
! [[obj_ApplyZeroDirichletBC3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyZeroDirichletBC2(obj, dbc)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary conditions; give only their nodes
  END SUBROUTINE obj_ApplyZeroDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyZeroDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Apply zero values at the Dirichlet nodes of the field
!
! Sets the entries at the nodes of the Dirichlet boundary conditions
! of the field, `obj%dbc`, to zero. See [[obj_ApplyZeroDirichletBC2]].
!
! See also: [[obj_ApplyZeroDirichletBC1]] (contains an example)

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
! Adds the point Neumann boundary conditions of the field to `obj`.
! The field stores them in `obj%nbc_point`.
!
! For each condition, it adds `scale * value` to the entry of each of
! its nodes. Here `value` is the value of the condition at time
! `times`. It skips pointers that are not associated.
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = &
!                            "ApplyPointNeumannBC_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! ! Test 1: number of point Neumann BCs
! CALL OK(u%GetTotalPointNBC() .EQ. 2, "GetTotalPointNBC")
!
! ! Test 2: add the point loads with scale = 1
! BLOCK
!   CALL u%Set(VALUE=0.0_DFP)
!   CALL u%ApplyPointNeumannBC(scale=1.0_DFP, times=0.0_DFP)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   want = [2.0_DFP, 0.0_DFP, 4.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(ALL(ABS(found - want) .LT. tol), "scale = 1")
! END BLOCK
!
! ! Test 3: loads are added to the current values, with scale = 0.5
! BLOCK
!   CALL u%Set(VALUE=1.0_DFP)
!   CALL u%ApplyPointNeumannBC(scale=0.5_DFP, times=0.0_DFP)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   want = [2.0_DFP, 1.0_DFP, 3.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
!   CALL OK(ALL(ABS(found - want) .LT. tol), "add with scale = 0.5")
! END BLOCK
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! [test 1](|classes|ScalarField/examples/ApplyPointNeumannBC_test_1.F90)
!
! See also: [[obj_ApplySurfaceNeumannBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyPointNeumannBC(obj, scale, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! factor for the values of the conditions
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time at which the conditions give their values
  END SUBROUTINE obj_ApplyPointNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!                                     ApplySurfaceNeumannBC@SurfaceNBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add the surface Neumann boundary conditions of the field
!
! Adds the surface Neumann boundary conditions of the field to `obj`.
! The field stores them in `obj%nbc`. For each condition, it does
! these steps.
!
! 1. It sets `nbcField` to zero.
! 2. It applies this condition to `nbcField` as Dirichlet values. So
!    `nbcField` holds the boundary data of this condition only.
! 3. It integrates the test functions times `nbcField` over the
!    facets of this condition.
! 4. It adds `scale` times this integral to `obj`.
!
! Each facet gets only the data of its own condition. This holds also
! at nodes shared by two boundaries. The contributions of several
! conditions add up.
!
! It does nothing if the field has no Neumann boundary condition.
!
! @note
! `nbcField` is a work field. It must use the same FEDOF as `obj`.
! This method overwrites its values.
! @endnote
!
! Changes:
!   - 2026-11-02: integrate each Neumann boundary condition with its own
!     data only (before, the data of all conditions was used)
!
! Formula:
!   $$ u_I = u_I + s \int_{\Gamma} N_I g \, d\Gamma $$
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
! CHARACTER(*), PARAMETER :: tomlFileName = &
!                            "ApplySurfaceNeumannBC_test_1.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u, nbcField
! REAL(DFP) :: val, total, expected(6)
! INTEGER(I4B) :: inode
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL nbcField%Initiate(NAME="nbcField", engine="NATIVE_SERIAL", &
!                        fedof=fedof, geofedof=geofedof)
!
! CALL OK(u%GetTotalNBC() .EQ. 2, "GetTotalNBC")
!
! CALL u%Set(VALUE=0.0_DFP)
! CALL u%ApplySurfaceNeumannBC(nbcField=nbcField, scale=1.0_DFP, &
!                              times=0.0_DFP)
!
! expected = [12.5_DFP, 22.5_DFP, 10.0_DFP, 0.0_DFP, 25.0_DFP, 0.0_DFP]
!
! total = 0.0_DFP
! DO inode = 1, 6
!   CALL u%Get(VALUE=val, globalNode=inode, islocal=math%yes)
!   CALL OK(ABS(val - expected(inode)) .LT. tol, &
!           "node "//CHAR(48 + inode))
!   total = total + val
! END DO
! CALL OK(ABS(total - 70.0_DFP) .LT. tol, "total flux = (2 + 5) * 10")
!
! CALL nbcField%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! [test 1](|classes|ScalarField/examples/ApplySurfaceNeumannBC_test_1.F90)
!
! See also: [[obj_ApplySurfaceNeumannBC2]], [[obj_ApplyPointNeumannBC]]

INTERFACE
  MODULE SUBROUTINE obj_ApplySurfaceNeumannBC1( &
    obj, nbcField, scale, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field, for example a right-hand side
    CLASS(ScalarField_), INTENT(INOUT) :: nbcField
    !! work field with the same FEDOF as `obj`; overwritten
    REAL(DFP), INTENT(IN) :: scale
    !! factor for the integral
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time at which the conditions give their values
  END SUBROUTINE obj_ApplySurfaceNeumannBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                     ApplySurfaceNeumannBC@SurfaceNBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add a list of surface Neumann boundary conditions
!
! Does the same as [[obj_ApplySurfaceNeumannBC1]], but uses the
! conditions in `nbc`. It does not use `obj%nbc`.
!
! It skips pointers that are not associated. It does nothing if
! `SIZE(nbc)` is zero.
!
! Changes:
!   - 2026-10-29: use nbc(ibc)%ptr, not the boundary conditions of obj
!   - 2026-11-02: integrate each Neumann boundary condition with its own
!     data only (before, the data of all conditions was used)
!
! See also: [[obj_ApplySurfaceNeumannBC1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_ApplySurfaceNeumannBC2(obj, nbc, nbcField, scale, &
                                               times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field, for example a right-hand side
    TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbc(:)
    !! Neumann boundary conditions to apply
    CLASS(ScalarField_), INTENT(INOUT) :: nbcField
    !! work field with the same FEDOF as `obj`; overwritten
    REAL(DFP), INTENT(IN) :: scale
    !! factor for the integral
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time at which the conditions give their values
  END SUBROUTINE obj_ApplySurfaceNeumannBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                           ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-05
! summary: Add the body source given by a user function
!
! Adds `scale` times the integral of the test functions times the body
! source to `obj`. It integrates over each element by quadrature.
!
! `bodySource` is a scalar user function of `(x, y, z, t)`. In 1D and
! 2D, the unused coordinates are 0. `times` gives `t`. The default of
! `times` is 0.
!
! Formula:
!   $$ u_I = u_I + s \int_{\Omega} N_I f(x, y, z, t) \, d\Omega $$
!
! Examples
!
! The test is long. This short example shows only the calls.
!
!```fortran
! ! u: scalar field, f: scalar user function of (x, y, z, t)
! CALL u%Set(VALUE=0.0_DFP)
! CALL u%ApplyBodySource(bodySource=f, scale=1.0_DFP, times=0.0_DFP)
!
! ! v: scalar field with the same FEDOF as u
! CALL u%ApplyBodySource(bodySource=v, scale=1.0_DFP)
!```
!
! [test 1](|classes|ScalarField/examples/ApplyBodySource_test_1.F90)
!
! See also: [[obj_ApplyBodySource2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource1(obj, bodySource, scale, times)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field, for example a right-hand side
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! scalar user function with four arguments `(x, y, z, t)`
    REAL(DFP), INTENT(IN) :: scale
    !! factor for the integral
    REAL(DFP), INTENT(IN), OPTIONAL :: times
    !! time `t` passed to `bodySource`, default 0
  END SUBROUTINE obj_ApplyBodySource1
END INTERFACE

!----------------------------------------------------------------------------
!                                           ApplyBodySource@BodySourceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-05
! summary: Add the body source given by a scalar field
!
! Does the same as [[obj_ApplyBodySource1]], but the body source is the
! scalar field `bodySource`. In each element, it interpolates the
! values of `bodySource` with the shape functions of `obj`.
!
! @note
! `bodySource` must use the same FEDOF as `obj`.
! @endnote
!
! Formula:
!   $$ u_I = u_I + s \int_{\Omega} N_I f_h \, d\Omega $$
!
! See also: [[obj_ApplyBodySource1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource2(obj, bodySource, scale)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field; its shape functions are the test functions
    CLASS(ScalarField_), INTENT(INOUT) :: bodySource
    !! body source, a scalar field with the same FEDOF as `obj`
    REAL(DFP), INTENT(IN) :: scale
    !! factor for the integral
  END SUBROUTINE obj_ApplyBodySource2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Initiate a scalar field from a toml table
!
! Initiates `obj` from the toml table `table`. It does these steps.
!
! 1. It reads `name`, `engine` and `fieldType` from `table`.
! 2. It initiates `fedof` and `geofedof` from `table`. It skips each
!    one that is already initiated.
! 3. It calls [[obj_Initiate2]].
! 4. It reads the Dirichlet, Neumann and point Neumann boundary
!    conditions from `table`.
!
! It ignores the keys `spaceCompo` and `timeCompo`. It does not use
! `timefedof`.
!
! The other `ImportFromToml` methods of [[AbstractNodeField_]] read the
! table from a file or by its name. Then they call this method.
!
! See also: [[obj_Initiate2]] (its example uses `ImportFromToml`),
! [[AbstractNodeField_:obj_ReadOptsFromToml]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, fedof, geofedof, &
                                        timefedof, dom)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table of the field
    CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof, geofedof
    !! FEDOF of the field and of the geometry; each one is initiated
    !! from `table` if it is not initiated
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(INOUT) :: timefedof
    !! not used
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! domain, needed when `fedof` or `geofedof` is not initiated
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-10
! summary: Set the values of the field from a toml table
!
! Sets the values of `obj` from the keys of `table`.
!
! 1. It reads `isUserFunction`. The default is false.
! 2. If `isUserFunction` is true, it reads the user function in the
!    sub-table `function`. Then it calls [[obj_SetByFunction]].
! 3. Otherwise it reads `nodalValueType`. The default is `"SPACE"`.
! 4. If `nodalValueType` is `"CONSTANT"`, it sets every entry to the
!    key `value`. The default of `value` is 0.
!
! @warning
! `nodalValueType = "SPACE"` is not implemented yet. It is also the
! default. Debug builds raise an error. Release builds do not change
! `obj`.
! @endwarning
!
! See also: [[obj_SetByFunction]], [[obj_ImportFromToml1]]

INTERFACE
  MODULE SUBROUTINE obj_SetFromToml(obj, table)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! initiated scalar field
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table with the keys above
  END SUBROUTINE obj_SetFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-01
! summary: Import the scalar field from an HDF5 file
!
! Reads the field from the group `group` of `hdf5`. If `obj` is
! initiated, it deallocates `obj` first. [[obj_Export]] writes the data
! that this method reads.
!
! It does not use `timefedof` and `timefedofs`.
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
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
! CHARACTER(*), PARAMETER :: filename = "Import_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x, y, z
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: src(6), found(6), areal
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL z%Initiate(NAME="z", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! src = [(REAL(ii, DFP), ii=1, 6)]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
! CALL z%SetMultiple(istart=1, iend=6, stride=1, VALUE=10.0_DFP * src)
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL x%Export(hdf5=h5, group="/x")
! CALL z%Export(hdf5=h5, group="/z")
! CALL h5%DEALLOCATE()
!
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
!
! ! Test 1: data and values of the imported field
! BLOCK
!   CALL y%IMPORT(hdf5=h5, group="/x", fedof=fedof, geofedof=geofedof)
!   CALL OK(y%IsInitiated(), "Import: IsInitiated")
!   CALL OK(y%SIZE() .EQ. 6, "Import: Size")
!   CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - src) .LT. tol), "Import: values")
! END BLOCK
!
! ! Test 2: Get by node on the imported field
! BLOCK
!   CALL y%Get(VALUE=areal, globalNode=3, islocal=math%yes)
!   CALL OK(ABS(areal - 3.0_DFP) .LT. tol, "Import: Get node")
! END BLOCK
!
! ! Test 3: import into an initiated field, from another group
! BLOCK
!   CALL y%IMPORT(hdf5=h5, group="/z", fedof=fedof, geofedof=geofedof)
!   CALL OK(y%SIZE() .EQ. 6, "Import again: Size")
!   CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - 10.0_DFP * src) .LT. tol), &
!           "Import again: values")
! END BLOCK
!
! CALL h5%DEALLOCATE()
!
! CALL z%DEALLOCATE()
! CALL y%DEALLOCATE()
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! [test 1](|classes|ScalarField/examples/Import_test_1.F90)
!
! See also: [[obj_Export]], [[AbstractNodeField_:obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! open HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the file
    CLASS(FEDOF_), TARGET, INTENT(IN), OPTIONAL :: fedof, geofedof
    !! FEDOFs of the field and of the geometry
    TYPE(FEDOFPointer_), INTENT(IN), OPTIONAL :: fedofs(:), geofedofs(:)
    !! FEDOFs of the physical variables
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! not used
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedofs(:)
    !! not used
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-01
! summary: Export the scalar field to an HDF5 file
!
! Writes the field in the group `group` of `hdf5`. [[obj_Import]]
! reads it back.
!
! See also: [[obj_Import]] (contains an example),
! [[AbstractNodeField_:obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! open HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ExportToVTK@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-01
! summary: Write the vertex values in a VTK file
!
! Writes the values of the vertex DOFs as a data array with one
! component. These are the first `fedof%GetTotalVertexDOF()` values.
!
! The array name is `<fieldname>_<dofname>`, for example `u_u` for a
! field named `u`.
!
! Changes:
!   - 2026-11-18: the array name includes the field name; it was only
!     the name of the physical variable (#659).
!
! [[AbstractNodeField_:obj_WriteData_vtk1]] calls this method after it
! writes the mesh.
!
! See also: [[AbstractNodeField_:obj_WriteData_vtk1]]

INTERFACE
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtk)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    !! scalar field
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! open VTK file
  END SUBROUTINE obj_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarField_Class
