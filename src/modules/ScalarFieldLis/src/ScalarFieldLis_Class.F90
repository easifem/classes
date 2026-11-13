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
! date: 2023-03-22
! summary: Scalar field data type with LIS engine is defined

MODULE ScalarFieldLis_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE GlobalData, ONLY: DFP, I4B, LGT
USE HDF5File_Class, ONLY: HDF5File_
USE ScalarField_Class, ONLY: ScalarField_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_
IMPLICIT NONE
PRIVATE

PUBLIC :: ScalarFieldLis_
PUBLIC :: ScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                           ScalarFieldLis_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Scalar field with the LIS engine (engine = "LIS_OMP")
!
! ScalarFieldLis_ is a ScalarField_ whose values are stored in a LIS
! vector (lis_ptr) instead of the Fortran vector realVec. The DOF data,
! the FEDOFs and the boundary conditions are the same as in
! ScalarField_, and all methods of ScalarField_ (Set, Get, SetByFunction,
! ApplyDirichletBC, ApplyBodySource, ...) can be used: they reach the
! values through the methods based on the index of a value (SetSingle,
! SetAll, SetMultiple, GetSingle, GetMultiple), which this class
! overrides. The BLAS methods (AXPY, SCAL, COPY, Swap, norms,
! DOT_PRODUCT, PMUL, Reciprocal) call the LIS vector routines; their
! field arguments must also use the LIS engine.
!
! realVec is allocated but it is not kept up to date: it is filled from
! the LIS vector (lis_vector_gather) only by Display and Export, and
! copied into the LIS vector (lis_vector_scatter) by Import.
! GetPointer is not available.
!
! The LIS library must be started before a field is initiated, by
! LisOmpEngine_%Initiate (lis_initialize), and stopped after the fields
! are deallocated, by LisOmpEngine_%DEALLOCATE (lis_finalize).
!
! Changes:
!   - 2026-11-12: The SetMultiple, GetMultiple and AXPY bindings are
!     PUBLIC, so that they override the bindings of AbstractNodeField_.
!
! See also: [[ScalarField_]], [[AbstractNodeField_]]

TYPE, EXTENDS(ScalarField_) :: ScalarFieldLis_
CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate an instance by copying another node field
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate an instance of ScalarFieldLis_ by passing arguments
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Destroy the LIS vector and deallocate the object
  FINAL :: obj_Final
  !! Finalizer

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the object
  ! @HDF5Methods
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the field to an HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the field from an HDF5 file

  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  !! Set single entry
  PROCEDURE, PUBLIC, PASS(obj) :: SetAll => obj_SetAll
  !! Set all the values to a constant scalar value
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple1 => obj_SetMultiple1
  !! Set multiple entries using indices
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple2 => obj_SetMultiple2
  !! Set multiple entries in a range
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple3 => obj_SetMultiple3
  !! Set multiple entries in a range from a range of value
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple4 => obj_SetMultiple4
  !! Set multiple entries in a range to one value
  PROCEDURE, PUBLIC, PASS(obj) :: Set8 => obj_Set8
  !! obj = obj + scale * obj2

  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! Not available for ScalarFieldLis_, returns NULL
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Number of values of the field
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
  !! Get single entry
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple1 => obj_GetMultiple1
  !! Get many values from indices
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple2 => obj_GetMultiple2
  !! Get many values in a range
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple3 => obj_GetMultiple3
  !! Get many values in a range into a range of value

  ! @BlasMethods
  PROCEDURE, PUBLIC, PASS(obj) :: AXPY1 => obj_AXPY1
  !! obj = obj + scale * x
  PROCEDURE, PUBLIC, PASS(obj) :: AXPY2 => obj_AXPY2
  !! obj = obj + a1 * x1 + a2 * x2
  PROCEDURE, PUBLIC, PASS(obj) :: AXPY3 => obj_AXPY3
  !! obj = obj + a1 * x1 + a2 * x2 + a3 * x3
  PROCEDURE, PUBLIC, PASS(obj) :: SCAL => obj_SCAL
  !! obj = scale * obj
  PROCEDURE, PUBLIC, PASS(obj) :: COPY => obj_Copy
  !! obj = obj2
  PROCEDURE, PUBLIC, PASS(obj) :: Swap => obj_Swap
  !! Swap the values of obj and obj2
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => obj_Norm2
  !! Returns the L2 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => obj_Norm1
  !! Returns the L1 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => obj_Normi
  !! Returns the infinity norm
  PROCEDURE, PUBLIC, PASS(obj) :: DOT_PRODUCT => obj_DOT_PRODUCT
  !! Dot product of obj and obj2
  PROCEDURE, PUBLIC, PASS(obj) :: PMUL => obj_PMUL
  !! obj = obj1 * obj2, entry by entry
  PROCEDURE, PUBLIC, PASS(obj) :: Reciprocal => obj_Reciprocal
  !! obj = 1 / obj, entry by entry
END TYPE ScalarFieldLis_

!----------------------------------------------------------------------------
!                                                    ScalarFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldLisPointer_
  CLASS(ScalarFieldLis_), POINTER :: ptr => NULL()
  !! pointer to a LIS scalar field
END TYPE ScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-25
! summary: Initiate a LIS scalar field by copying another node field
!
! The data of obj2 (name, engine, sizes, DOF data, FEDOF pointers,
! boundary condition pointers and realVec) are copied into obj by
! AbstractNodeFieldInitiate. Then a new LIS vector is created for obj
! (lis_vector_create, lis_vector_set_size, lis_vector_get_range) and the
! values of obj2 are copied into it: from the LIS vector of obj2 when
! obj2 is a ScalarFieldLis_ (lis_vector_copy), otherwise from the copied
! realVec (lis_vector_scatter). obj and obj2 have separate LIS vectors.
!
! copyStructure and usePointer are not used.
!
! Changes:
!   - 2026-11-12: The values of obj2 are copied into the new LIS vector
!     (before, the new vector was empty).
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
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE ScalarField_Class, ONLY: ScalarField_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarFieldLis_) :: u, w, w2
! TYPE(ScalarField_) :: n
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL u%Set(VALUE=want)
! CALL w%Initiate(obj2=u, copyFull=math%yes)
!
! ! Test 1: structure is copied
! CALL OK(w%IsInitiated(), "w: IsInitiated")
! CALL OK(w%SIZE() .EQ. u%SIZE(), "w: Size = u%Size")
!
! ! Test 2: values are copied
! BLOCK
!   CALL w%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "w: values of u")
! END BLOCK
!
! ! Test 3: w has its own LIS vector
! BLOCK
!   CALL w%Set(VALUE=0.0_DFP)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
! END BLOCK
!
! ! Test 4: copy of a native scalar field
! BLOCK
!   CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
!                   geofedof=geofedof)
!   CALL n%Set(VALUE=-want)
!   CALL w2%Initiate(obj2=n, copyFull=math%yes)
!   CALL w2%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found + want) .LT. tol), "w2: values of n")
! END BLOCK
!
! CALL w2%DEALLOCATE()
! CALL n%DEALLOCATE()
! CALL w%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field to be initiated
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! field to copy, it should be a child of AbstractNodeField_
    !! (ScalarFieldLis_ or a native ScalarField_)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyFull
    !! passed to AbstractNodeFieldInitiate
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyStructure
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: usePointer
    !! not used
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-03
! summary: Initiate an instance of ScalarFieldLis_ by arguments
!
! The scalar field data are set by the Initiate method of ScalarField_
! (one physical variable, one space and one time component, one value
! for each DOF of fedof). Then the LIS vector of local size local_n and
! global size global_n is created (lis_vector_create,
! lis_vector_set_size) and the local range is, ie is read
! (lis_vector_get_range). The values of a new vector are zero.
!
! ImportFromToml with engine = "LIS_OMP" also calls this method. The
! LIS library must be started before (LisOmpEngine_%Initiate).
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
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarFieldLis_) :: u, v
! REAL(DFP) :: found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL v%Initiate(NAME="v", engine="LIS_OMP", fedof=fedof, &
!                 geofedof=geofedof)
!
! ! Test 1: both fields are initiated, with one value for each DOF
! CALL OK(u%IsInitiated(), "u: IsInitiated")
! CALL OK(v%IsInitiated(), "v: IsInitiated")
! CALL OK(u%SIZE() .EQ. 6, "u: Size")
! CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF(), "v: Size = total DOF")
! CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "v: one physical variable")
!
! ! Test 2: u and v have their own LIS vectors
! BLOCK
!   CALL u%Set(VALUE=1.0_DFP)
!   CALL v%Set(VALUE=2.0_DFP)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "u: values")
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - 2.0_DFP) .LT. tol), "v: values")
! END BLOCK
!
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate1]], [[ScalarField_:obj_Initiate2]]

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
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field to be initiated
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine, "LIS_OMP"
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
    !! FEDOF of the field and FEDOF of the geometry
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-03
! summary: Destroy the LIS vector and deallocate the object
!
! The LIS vector is destroyed (lis_vector_destroy), then the data of the
! node field are deallocated (AbstractNodeFieldDeallocate). Call it
! before LisOmpEngine_%DEALLOCATE (lis_finalize).
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-03
! summary: Finalizer, calls Deallocate
!
! See also: [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-15
! summary: Display the object
!
! Displays whether the LIS vector is available. If it is, the values of
! the LIS vector are gathered into realVec (lis_vector_gather) and the
! node field is displayed by AbstractNodeFieldDisplay.
!
! See also: [[obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field, realVec is updated from the LIS vector
    CHARACTER(*), INTENT(IN) :: msg
    !! message displayed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-15
! summary: Export the field to an HDF5 file
!
! The values of the LIS vector are gathered into realVec
! (lis_vector_gather), then the field is written in group of hdf5 by
! AbstractNodeFieldExport, in the same layout as a native node field.
! The group can be read by Import of ScalarFieldLis_ or of ScalarField_.
! In debug mode an error is raised when the LIS vector is not
! available.
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
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE ScalarField_Class, ONLY: ScalarField_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
! CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarFieldLis_) :: u
! TYPE(ScalarField_) :: y
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: want(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL u%Set(VALUE=want)
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL u%Export(hdf5=h5, group="/u")
! CALL h5%DEALLOCATE()
!
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL y%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
! CALL h5%DEALLOCATE()
!
! ! Test 1: the exported data is read back
! CALL OK(y%IsInitiated(), "Import: IsInitiated")
! CALL OK(y%SIZE() .EQ. 6, "Import: Size")
!
! ! Test 2: the exported values are the values of the LIS vector
! BLOCK
!   CALL y%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "Export: values")
! END BLOCK
!
! CALL y%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field, realVec is updated from the LIS vector
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened for writing
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-15
! summary: Import the field from an HDF5 file
!
! The data of the node field (name, sizes, DOF data and realVec) are
! read from group of hdf5 by AbstractNodeFieldImport, and the FEDOF
! pointers are set from the arguments. Then a new LIS vector is created
! (lis_vector_create, lis_vector_set_size, lis_vector_get_range) and the
! values of realVec are copied into it (lis_vector_scatter). If obj
! already has a LIS vector, it is destroyed first.
!
! The group can be written by a ScalarFieldLis_ or by a native
! ScalarField_ (same layout); the engine of obj is always "LIS_OMP".
! The LIS library must be started before (LisOmpEngine_%Initiate).
!
! Changes:
!   - 2026-11-13: The routine is available (before, it raised a WIP
!     error). geofedof, geofedofs, timefedof and timefedofs are used,
!     and the engine is set to "LIS_OMP".
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
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE ScalarField_Class, ONLY: ScalarField_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
! CHARACTER(*), PARAMETER :: filename = "Import_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarFieldLis_) :: u, v, w
! TYPE(ScalarField_) :: n
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: want(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL u%Set(VALUE=want)
! CALL n%Set(VALUE=-want)
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL u%Export(hdf5=h5, group="/u")
! CALL n%Export(hdf5=h5, group="/n")
! CALL h5%DEALLOCATE()
!
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL v%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
! CALL w%IMPORT(hdf5=h5, group="/n", fedof=fedof, geofedof=geofedof)
! CALL h5%DEALLOCATE()
!
! ! Test 1: field exported by a LIS field
! CALL OK(v%IsInitiated(), "v: IsInitiated")
! CALL OK(v%SIZE() .EQ. 6, "v: Size")
! CALL OK(v%GetEngineName() .EQ. "LIS_OMP", "v: engine")
!
! ! Test 2: values of v are the values of u
! BLOCK
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "v: values of u")
! END BLOCK
!
! ! Test 3: v has its own LIS vector
! BLOCK
!   CALL v%Set(VALUE=0.0_DFP)
!   CALL u%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
! END BLOCK
!
! ! Test 4: field exported by a native field
! BLOCK
!   CALL OK(w%IsInitiated(), "w: IsInitiated")
!   CALL OK(w%SIZE() .EQ. 6, "w: Size")
!   CALL OK(w%GetEngineName() .EQ. "LIS_OMP", "w: engine")
!   CALL w%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found + want) .LT. tol), "w: values of n")
! END BLOCK
!
! CALL w%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL n%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened for reading
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
    CLASS(FEDOF_), TARGET, INTENT(IN), OPTIONAL :: fedof, geofedof
    !! FEDOF of the field and FEDOF of the geometry
    TYPE(FEDOFPointer_), INTENT(IN), OPTIONAL :: fedofs(:), geofedofs(:)
    !! FEDOFs for multiple physical variables
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! TimeFEDOF object
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedofs(:)
    !! TimeFEDOFs for multiple physical variables
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetSingle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Set single entry
!
! Sets entry indx of the LIS vector to scale * VALUE
! (lis_vector_set_value with LIS_INS_VALUE), or adds scale * VALUE when
! addContribution is true (LIS_ADD_VALUE).
!
! Examples
!
!```fortran
! CALL x%SetSingle(indx=3, VALUE=2.0_DFP, scale=4.0_DFP)
! CALL x%GetSingle(indx=3, VALUE=val)
! CALL OK(ABS(val - 8.0_DFP) .LT. tol, "SetSingle: scale * value")
!```
!
! Full example: src/modules/ScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetSingle]], [[obj_SetAll]]

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: indx
    !! index of the entry, 1 <= indx <= obj%Size()
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-03
! summary: Set all the values
!
! Sets all entries to scale * VALUE (lis_vector_set_all), or adds
! scale * VALUE to each entry when addContribution is true. Set(VALUE)
! of ScalarField_ calls this method.
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
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarFieldLis_) :: u
! REAL(DFP) :: val, found(6)
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! ! Test 1: all entries
! BLOCK
!   CALL u%Set(VALUE=1.0_DFP)
!   DO ii = 1, 6
!     CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
!   END DO
!   CALL OK(ALL(ABS(found - 1.0_DFP) .LE. tol), "Set all")
!
!   CALL u%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
!   CALL u%Get(VALUE=val, globalNode=4, islocal=math%yes)
!   CALL OK(ABS(val - 3.0_DFP) .LE. tol, "Set all: add 2 * 1")
! END BLOCK
!
! ! Test 2: single entry
! BLOCK
!   CALL u%Set(globalNode=2, islocal=math%yes, VALUE=5.0_DFP)
!   CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
!   CALL OK(ABS(val - 5.0_DFP) .LT. tol, "Set single")
!
!   CALL u%Set(globalNode=2, islocal=math%yes, VALUE=5.0_DFP, &
!              scale=2.0_DFP)
!   CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
!   CALL OK(ABS(val - 10.0_DFP) .LT. tol, "Set single: scale")
!
!   CALL u%Set(globalNode=2, islocal=math%yes, VALUE=1.0_DFP, &
!              scale=3.0_DFP, addContribution=math%yes)
!   CALL u%Get(VALUE=val, globalNode=2, islocal=math%yes)
!   CALL OK(ABS(val - 13.0_DFP) .LT. tol, "Set single: add 3 * 1")
!
!   CALL u%Get(VALUE=val, globalNode=1, islocal=math%yes)
!   CALL OK(ABS(val - 3.0_DFP) .LT. tol, "other entries unchanged")
! END BLOCK
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetSingle]]

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set or add
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Set multiple entries using indices
!
! Sets entry indx(i) to scale * VALUE(i) for i = 1, ..., SIZE(indx)
! (lis_vector_set_values4), or adds scale * VALUE(i) when
! addContribution is true.
!
! Examples
!
!```fortran
! CALL x%SetMultiple(indx=[2, 4], VALUE=[10.0_DFP, 20.0_DFP], &
!                    scale=2.0_DFP)
! CALL x%GetMultiple(indx=[4, 2], VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found(1:2) - [40.0_DFP, 20.0_DFP]) .LT. tol), &
!         "SetMultiple indx: scale * value")
!```
!
! Full example: src/modules/ScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_GetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple1(obj, indx, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! indices, SIZE(indx) = SIZE(VALUE)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values, obj(indx(i)) = scale * VALUE(i)
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Set multiple entries in a range
!
! Sets the entries istart, istart + stride, ..., iend to scale * VALUE(1),
! scale * VALUE(2), ... (lis_vector_set_values5), or adds them when
! addContribution is true. The number of entries is
! (iend - istart) / stride + 1.
!
! Examples
!
!```fortran
! CALL x%SetMultiple(istart=1, iend=5, stride=2, &
!                    VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], scale=3.0_DFP)
!```
!
! Full example: src/modules/ScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple1]], [[obj_GetMultiple2]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple2(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values, SIZE(VALUE) >= (iend - istart) / stride + 1
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-02
! summary: Set multiple entries in a range from a range of value
!
! Sets the entries istart:iend:stride of obj to scale times the entries
! istart_value:iend_value:stride_value of VALUE
! (lis_vector_set_values8), or adds them when addContribution is true.
! Both ranges have (iend - istart) / stride + 1 entries.
!
! Examples
!
!```fortran
! ! x(2:6:2) = -1 * src(4:6)
! CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=src, &
!                    istart_value=4, iend_value=6, stride_value=1, &
!                    scale=-1.0_DFP)
!```
!
! Full example: src/modules/ScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_GetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of entries of VALUE, same number of entries as obj range
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Set multiple entries in a range to one value
!
! Sets the entries istart:iend:stride to scale * VALUE
! (lis_vector_set_values6), or adds scale * VALUE when addContribution
! is true.
!
! Examples
!
!```fortran
! ! x(1:6:3) = 0.5 * 8
! CALL x%SetMultiple(istart=1, iend=6, stride=3, VALUE=8.0_DFP, &
!                    scale=0.5_DFP)
!```
!
! Full example: src/modules/ScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_SetAll]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple4(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! one value for all entries in the range
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Set or add another scalar field: obj = obj + scale * VALUE
!
! If addContribution is true, then scale * VALUE is added to obj.
! Otherwise obj is set to scale * VALUE. VALUE can be
!
!- a ScalarFieldLis_: the LIS vector of VALUE is used
!  (lis_vector_set_values9),
!- a native ScalarField_: its Fortran vector is used (SetMultiple).
!
! The assignment obj = VALUE (Set7 of ScalarField_) calls this method.
! Both fields must have the same FEDOF. An error is raised (in debug
! mode) if obj or VALUE is not initiated, or VALUE is another type.
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
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE ScalarField_Class, ONLY: ScalarField_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarFieldLis_) :: u, v
! TYPE(ScalarField_) :: n
! REAL(DFP) :: found(6), want(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL v%Initiate(NAME="v", engine="LIS_OMP", fedof=fedof, &
!                 geofedof=geofedof)
! CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL u%Set(VALUE=want)
!
! ! Test 1: assignment from a LIS field
! BLOCK
!   CALL v%Set(VALUE=0.0_DFP)
!   v = u
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "v = u")
! END BLOCK
!
! ! Test 2: LIS field, set and add
! BLOCK
!   CALL v%Set(VALUE=9.0_DFP)
!   CALL v%Set(VALUE=u, scale=-3.0_DFP, addContribution=math%no)
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found + 3.0_DFP * want) .LT. tol), "v = -3 * u")
!
!   CALL v%Set(VALUE=u, scale=2.0_DFP, addContribution=math%yes)
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found + want) .LT. tol), "v = v + 2 * u")
! END BLOCK
!
! ! Test 3: native field, set and add
! BLOCK
!   CALL n%Set(VALUE=10.0_DFP * want)
!   CALL v%Set(VALUE=n, scale=0.5_DFP, addContribution=math%no)
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - 5.0_DFP * want) .LT. tol), "v = 0.5 * n")
!
!   CALL v%Set(VALUE=n, scale=-1.0_DFP, addContribution=math%yes)
!   CALL v%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found + 5.0_DFP * want) .LT. tol), "v = v - n")
! END BLOCK
!
! ! Test 4: u is not changed
! BLOCK
!   CALL u%Get(VALUE=found, tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
! END BLOCK
!
! CALL n%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[ScalarField_:obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    CLASS(ScalarField_), INTENT(IN) :: VALUE
    !! other scalar field (ScalarFieldLis_ or ScalarField_)
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! add (true) or set (false)
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Set

! INTERFACE
!   MODULE SUBROUTINE obj_Set9(obj, ivar, idof, VALUE, ivar_value, &
!                              idof_value, scale, addContribution)
!     CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: ivar
!     !! physical variable of obj
!     INTEGER(I4B), INTENT(IN) :: idof
!     !! local degree of freedom of physical variable ivar
!     CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
!     !! right hand side in obj = value
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     !! physical variable of value
!     INTEGER(I4B), INTENT(IN) :: idof_value
!     !! local degree of freedom of physical variable ivar_value
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     !! scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!     !! add or set
!   END SUBROUTINE obj_Set9
! END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-04
! summary: Not available, returns NULL
!
! The values are stored in the LIS vector, not in realVec, so no
! pointer to them can be returned. The result is NULL and in debug mode
! an error is raised. Use Get or GetMultiple instead.
!
! Returns:
!   ans : REAL(DFP), POINTER - NULL
!
! See also: [[obj_GetMultiple2]]

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), TARGET, INTENT(IN) :: obj
    !! LIS scalar field
    REAL(DFP), POINTER :: ans(:)
    !! always NULL
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-04
! summary: Number of values of the field
!
! Returns local_n, the number of values stored on this process. For a
! serial field it is the total number of DOFs of fedof.
!
! Returns:
!   ans : INTEGER(I4B) - number of values
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used
    INTEGER(I4B) :: ans
    !! number of values
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get single entry
!
! Returns entry indx of the LIS vector (lis_vector_get_value).
!
! See also: [[obj_SetSingle]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: indx
    !! index of the entry, 1 <= indx <= obj%Size()
    REAL(DFP), INTENT(OUT) :: VALUE
    !! value of the entry
  END SUBROUTINE obj_GetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get multiple entries using indices
!
! VALUE(i) is entry indx(i) of the LIS vector, i = 1, ..., SIZE(indx)
! (lis_vector_get_values_from_index); tsize = SIZE(indx).
!
! See also: [[obj_SetMultiple1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple1(obj, indx, VALUE, tsize)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! indices of the entries
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! values, SIZE(VALUE) >= SIZE(indx)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE, SIZE(indx)
  END SUBROUTINE obj_GetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get multiple entries in a range
!
! VALUE(1:tsize) are the entries istart:iend:stride of the LIS vector
! (lis_vector_get_values_from_range), with
! tsize = (iend - istart) / stride + 1. Get(VALUE, tsize) of
! ScalarField_ calls this method.
!
! See also: [[obj_SetMultiple2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple2(obj, istart, iend, stride, VALUE, tsize)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! values, SIZE(VALUE) >= tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE
  END SUBROUTINE obj_GetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-02
! summary: Get multiple entries in a range into a range of value
!
! The entries istart:iend:stride of the LIS vector are written in
! VALUE(istart_value:iend_value:stride_value)
! (lis_vector_get_values_from_range2); the other entries of VALUE are
! not changed. tsize = (iend - istart) / stride + 1.
!
! Examples
!
!```fortran
! ! found(4:6) = x(1:3)
! CALL x%GetMultiple(istart=1, iend=3, stride=1, VALUE=found, &
!                    istart_value=4, iend_value=6, stride_value=1, &
!                    tsize=tsize)
!```
!
! Full example: src/modules/ScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     tsize)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! values, entries istart_value:iend_value:stride_value are written
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of entries of VALUE, same number of entries as obj range
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE
  END SUBROUTINE obj_GetMultiple3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: obj = obj + scale * x
!
! Calls lis_vector_axpy. x must use the LIS engine and have the same
! size as obj.
!
! Formula:
!   $$ obj = obj + s \, x $$
!
! Examples
!
!```fortran
! CALL w%SetAll(VALUE=0.0_DFP)
! CALL w%AXPY(x=x, scale=2.0_DFP)
! CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - 2.0_DFP * src) .LT. tol), "AXPY: w + s * x")
!```
!
! Full example: src/modules/ScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY2]], [[obj_AXPY3]], [[obj_SCAL]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY1(obj, x, scale)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field, obj = obj + scale * x
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x
    !! node field with the LIS engine
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_AXPY1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-29
! summary: obj = obj + a1 * x1 + a2 * x2
!
! Calls lis_vector_axpy twice. x1 and x2 must use the LIS engine.
!
! Formula:
!   $$ obj = obj + a_1 x_1 + a_2 x_2 $$
!
! See also: [[obj_AXPY1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_AXPY2(obj, x1, x2, a1, a2)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x1
    !! first node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x2
    !! second node field with the LIS engine
    REAL(DFP), INTENT(IN) :: a1
    !! scale of x1
    REAL(DFP), INTENT(IN) :: a2
    !! scale of x2
  END SUBROUTINE obj_AXPY2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-29
! summary: obj = obj + a1 * x1 + a2 * x2 + a3 * x3
!
! Calls lis_vector_axpy three times. x1, x2 and x3 must use the LIS
! engine.
!
! Formula:
!   $$ obj = obj + a_1 x_1 + a_2 x_2 + a_3 x_3 $$
!
! See also: [[obj_AXPY1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_AXPY3(obj, x1, x2, x3, a1, a2, a3)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x1
    !! first node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x2
    !! second node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x3
    !! third node field with the LIS engine
    REAL(DFP), INTENT(IN) :: a1
    !! scale of x1
    REAL(DFP), INTENT(IN) :: a2
    !! scale of x2
    REAL(DFP), INTENT(IN) :: a3
    !! scale of x3
  END SUBROUTINE obj_AXPY3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SCAL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Scaling, obj = scale * obj
!
! Calls lis_vector_scale.
!
! See also: [[obj_AXPY1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_SCAL(obj, scale)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_SCAL
END INTERFACE

!----------------------------------------------------------------------------
!                                                          COPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Copy the values, obj = obj2
!
! Copies the LIS vector of obj2 into the LIS vector of obj
! (lis_vector_copy). obj2 must use the LIS engine and have the same size.
!
! See also: [[obj_AXPY1]] (contains an example), [[obj_Swap]]

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field, obj = obj2
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! node field with the LIS engine
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Swap@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Swap the values of two node fields, obj <=> obj2
!
! Exchange the nodal values stored in the LIS vectors of obj and obj2
! by calling lis_vector_swap. Only the values are exchanged; the other
! components (name, fedof, dof data, ...) of obj and obj2 are not
! changed. This method overrides the Swap method of AbstractNodeField_.
!
! Both fields must be initiated with the LIS engine and have the same
! size. In debug mode an error is raised when the LIS vector of obj or
! obj2 is null, or when obj%Size() and obj2%Size() differ.
!
! See also: [[obj_AXPY1]] (contains an example), [[obj_Copy]]

INTERFACE
  MODULE SUBROUTINE obj_Swap(obj, obj2)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! first node field, on return it contains the values of obj2
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj2
    !! second node field (LIS engine), on return it contains the values
    !! of obj
  END SUBROUTINE obj_Swap
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Returns the L2 norm
!
! Calls lis_vector_nrm2.
!
! Returns:
!   ans : REAL(DFP) - L2 norm of the values
!
! Formula:
!   $$ ans = \sqrt{\sum_i x_i^2} $$
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
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarFieldLis_) :: x, y, z
! REAL(DFP) :: src(6), ysrc(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL y%Initiate(NAME="y", engine="LIS_OMP", fedof=fedof, &
!                 geofedof=geofedof)
! CALL z%Initiate(NAME="z", engine="LIS_OMP", fedof=fedof, &
!                 geofedof=geofedof)
!
! src = [1.0_DFP, -2.0_DFP, 3.0_DFP, -4.0_DFP, 5.0_DFP, -6.0_DFP]
! ysrc = [1.0_DFP, 1.0_DFP, 1.0_DFP, 2.0_DFP, 2.0_DFP, 2.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
! CALL y%SetMultiple(istart=1, iend=6, stride=1, VALUE=ysrc)
!
! ! Test 1: norms
! CALL OK(ABS(x%Norm1() - 21.0_DFP) .LT. tol, "Norm1")
! CALL OK(ABS(x%Norm2() - SQRT(91.0_DFP)) .LT. tol, "Norm2")
! CALL OK(ABS(x%Normi() - 6.0_DFP) .LT. tol, "Normi")
!
! ! Test 2: dot product
! CALL OK(ABS(x%DOT_PRODUCT(obj2=y) + 8.0_DFP) .LT. tol, "DOT_PRODUCT")
!
! ! Test 3: PMUL, z = x * y
! BLOCK
!   CALL z%PMUL(obj1=x, obj2=y)
!   CALL z%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - src * ysrc) .LT. tol), "PMUL")
! END BLOCK
!
! ! Test 4: Reciprocal, z = 1 / z
! BLOCK
!   CALL z%Reciprocal()
!   CALL z%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - 1.0_DFP / (src * ysrc)) .LT. tol), &
!           "Reciprocal")
! END BLOCK
!
! CALL z%DEALLOCATE()
! CALL y%DEALLOCATE()
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Norm1]], [[obj_Normi]]

INTERFACE
  MODULE FUNCTION obj_Norm2(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    REAL(DFP) :: ans
    !! L2 norm
  END FUNCTION obj_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Norm1@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Returns the L1 norm (sum of absolute values)
!
! Calls lis_vector_nrm1.
!
! Returns:
!   ans : REAL(DFP) - L1 norm of the values
!
! Formula:
!   $$ ans = \sum_i |x_i| $$
!
! See also: [[obj_Norm2]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Norm1(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    REAL(DFP) :: ans
    !! L1 norm
  END FUNCTION obj_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Normi@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Returns the infinity norm (largest absolute value)
!
! Calls lis_vector_nrmi.
!
! Changes:
!   - 2026-11-14: Correct with the OpenMP build of LIS. Before, the
!     thread private maximum in lis_vector_nrmi was not initialised and
!     the result was wrong; fixed in the easifem fork of LIS
!     (lis-latest). Needs a LIS built after this fix.
!
! Returns:
!   ans : REAL(DFP) - infinity norm of the values
!
! Formula:
!   $$ ans = \max_i |x_i| $$
!
! See also: [[obj_Norm2]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Normi(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    REAL(DFP) :: ans
    !! infinity norm
  END FUNCTION obj_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                   DOT_PRODUCT@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary: Dot product of obj and obj2
!
! Calls lis_vector_dot. obj2 must use the LIS engine and have the same
! size.
!
! Returns:
!   ans : REAL(DFP) - dot product
!
! Formula:
!   $$ ans = \sum_i x_i y_i $$
!
! See also: [[obj_Norm2]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_DOT_PRODUCT(obj, obj2) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    !! LIS scalar field
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! node field with the LIS engine
    REAL(DFP) :: ans
    !! dot product
  END FUNCTION obj_DOT_PRODUCT
END INTERFACE

!----------------------------------------------------------------------------
!                                                          PMUL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary: Entry by entry product, obj = obj1 * obj2
!
! Calls lis_vector_pmul. obj1 and obj2 must use the LIS engine and have
! the same size as obj.
!
! Formula:
!   $$ obj_i = (obj1)_i (obj2)_i $$
!
! See also: [[obj_Norm2]] (contains an example), [[obj_Reciprocal]]

INTERFACE
  MODULE SUBROUTINE obj_PMUL(obj, obj1, obj2)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field, result
    CLASS(AbstractNodeField_), INTENT(IN) :: obj1
    !! first node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! second node field with the LIS engine
  END SUBROUTINE obj_PMUL
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Reciprocal@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary: Entry by entry reciprocal, obj = 1 / obj
!
! Calls lis_vector_reciprocal. The entries must not be zero.
!
! Formula:
!   $$ obj_i = 1 / obj_i $$
!
! See also: [[obj_Norm2]] (contains an example), [[obj_PMUL]]

INTERFACE
  MODULE SUBROUTINE obj_Reciprocal(obj)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS scalar field
  END SUBROUTINE obj_Reciprocal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarFieldLis_Class
