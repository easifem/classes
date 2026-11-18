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
! summary: Vector field data type with LIS engine is defined

MODULE VectorFieldLis_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
USE VectorField_Class, ONLY: VectorField_

IMPLICIT NONE
PRIVATE

PUBLIC :: VectorFieldLis_
PUBLIC :: VectorFieldLisPointer_

!----------------------------------------------------------------------------
!                                                             VectorFieldLis_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Vector field with the LIS engine
!
! VectorFieldLis_ is a VectorField_ whose values are stored in a LIS
! vector (lis_ptr of AbstractField_), so that the field can be used
! with the LIS linear solvers. It has no components of its own. The
! engine of the field is "LIS_OMP".
!
! A vector field has one physical variable with spaceCompo components.
! The values are stored in DOF format: component 1 of all nodes, then
! component 2 of all nodes, and so on, so the value of node I and
! space component c has the index (c-1)*tNodes + I.
!
! Initiate, ImportFromToml and Import create the LIS vector (local_n
! values). SetSingle, SetAll, SetMultiple, GetSingle, GetMultiple and
! the BLAS methods work on the LIS vector; the methods of VectorField_
! (Set, Get, ApplyDirichletBC, ...) call them, so they work on the LIS
! vector too. The values are not stored in realVec: GetPointer is not
! available, Display and Export gather the LIS vector into realVec
! first.
!
! The LIS library must be started before a field is initiated, by
! LisOmpEngine_%Initiate (lis_initialize), and stopped after the fields
! are deallocated, by LisOmpEngine_%DEALLOCATE (lis_finalize).
!
! Changes:
!   - 2026-11-15: Initiate and Import create the LIS vector; storage,
!     Get and BLAS methods work on the LIS vector (#571)
!
! See also: [[VectorField_]], [[STScalarFieldLis_]], [[AbstractNodeField_]]

TYPE, EXTENDS(VectorField_) :: VectorFieldLis_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Copy another node field and create the LIS vector
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate the field and create the LIS vector
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Destroy the LIS vector and deallocate the object
  FINAL :: obj_Final
  !! Finalizer

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the content
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the content

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  !! Set or add one value
  PROCEDURE, PUBLIC, PASS(obj) :: SetAll => obj_SetAll
  !! Set or add one value to all entries
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple1 => obj_SetMultiple1
  !! Set or add values at given indices
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple2 => obj_SetMultiple2
  !! Set or add values in a range
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple3 => obj_SetMultiple3
  !! Set or add a range of an array in a range
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple4 => obj_SetMultiple4
  !! Set or add one value in a range
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple5 => obj_SetMultiple5
  !! Set or add a range of another node field in a range
  PROCEDURE, PUBLIC, PASS(obj) :: Set13 => obj_Set13
  !! Copy another vector field (overrides Set13 of VectorField_)

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Number of values of the field
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! Not available, returns NULL
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
  !! Get one value
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple1 => obj_GetMultiple1
  !! Get the values at given indices
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple2 => obj_GetMultiple2
  !! Get the values in a range
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple3 => obj_GetMultiple3
  !! Get the values in a range into a range of an array
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple4 => obj_GetMultiple4
  !! Set or add scale times a range into a range of an array

  ! BLAS:
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
  !! L2 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => obj_Norm1
  !! L1 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => obj_Normi
  !! Infinity norm
  PROCEDURE, PUBLIC, PASS(obj) :: DOT_PRODUCT => obj_DOT_PRODUCT
  !! Dot product of obj and obj2
  PROCEDURE, PUBLIC, PASS(obj) :: PMUL => obj_PMUL
  !! obj = obj1 * obj2, entry by entry
  PROCEDURE, PUBLIC, PASS(obj) :: Reciprocal => obj_Reciprocal
  !! obj = 1 / obj, entry by entry
END TYPE VectorFieldLis_

!----------------------------------------------------------------------------
!                                                     VectorFieldLisPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Pointer to a VectorFieldLis_, used to make arrays of fields

TYPE :: VectorFieldLisPointer_
  CLASS(VectorFieldLis_), POINTER :: ptr => NULL()
  !! pointer to a vector field with LIS engine
END TYPE VectorFieldLisPointer_

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Initiate a LIS vector field by copying another node field
!
! The data of obj2 are copied into obj by the Initiate1 method of
! VectorField_ (node field data, spaceCompo and idofs when obj2 is a
! vector field). Then a new LIS vector is created for obj
! (lis_vector_create, lis_vector_set_size, lis_vector_get_range) and
! the values of obj2 are copied into it: from the LIS vector of obj2
! when obj2 uses the LIS engine (lis_vector_copy), otherwise from the
! copied realVec (lis_vector_scatter), and the engine of obj is set to
! "LIS_OMP". obj and obj2 have separate LIS vectors.
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
! USE GlobalData, ONLY: INT64
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE Test_Method, ONLY: OK
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorFieldLis_) :: u, v, w
! REAL(DFP) :: raw(12)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL v%Initiate(NAME="v", engine="LIS_OMP", spaceCompo=2, &
!                 fedof=fedof, geofedof=geofedof)
!
! ! Test 1: the fields are initiated, with spaceCompo values per node
! CALL OK(u%IsInitiated(), "u: IsInitiated")
! CALL OK(v%IsInitiated(), "v: IsInitiated")
! CALL OK(u%spaceCompo .EQ. 2, "u: spaceCompo")
! CALL OK(u%SIZE() .EQ. 12, "u: Size = 12")
! CALL OK(v%SIZE() .EQ. 2 * fedof%GetTotalDOF(), "v: Size = 2 * total DOF")
! CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "v: one physical variable")
!
! ! Test 2: each field has its own LIS vector
! CALL OK(u%lis_ptr .NE. 0_INT64, "u: LIS vector is created")
! CALL OK(v%lis_ptr .NE. 0_INT64, "v: LIS vector is created")
! CALL OK(u%lis_ptr .NE. v%lis_ptr, "u and v: different LIS vectors")
!
! ! Test 3: Initiate(obj2) makes a new LIS vector with the values of v
! BLOCK
!   CALL v%Set(VALUE=3.0_DFP)
!   CALL w%Initiate(obj2=v)
!   CALL OK(w%spaceCompo .EQ. 2, "Initiate(obj2): spaceCompo")
!   CALL OK(w%SIZE() .EQ. 12, "Initiate(obj2): Size")
!   CALL OK(w%lis_ptr .NE. 0_INT64 .AND. w%lis_ptr .NE. v%lis_ptr, &
!           "Initiate(obj2): own LIS vector")
!   CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(raw - 3.0_DFP) .LT. tol), "Initiate(obj2): values")
! END BLOCK
!
! CALL w%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! Full example: src/modules/VectorFieldLis/examples/Initiate_test_1.F90
!
! See also: [[obj_Initiate2]], [[VectorField_:obj_Initiate1]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field to be initiated
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! field to copy, it should be a child of AbstractNodeField_
    !! (VectorFieldLis_ or a native VectorField_)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyFull
    !! passed to the Initiate1 method of VectorField_
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyStructure
    !! passed to the Initiate1 method of VectorField_
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: usePointer
    !! passed to the Initiate1 method of VectorField_
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Initiate a LIS vector field by arguments
!
! The vector field data are set by the Initiate method of VectorField_
! (spaceCompo components). Then the LIS vector of local size
! local_n and global size global_n is created (lis_vector_create,
! lis_vector_set_size) and the local range is, ie is read
! (lis_vector_get_range). The values of a new vector are zero.
!
! The engine of the field is always "LIS_OMP", whatever engine is
! given, because the values are stored in the LIS vector.
!
! ImportFromToml with engine = "LIS_OMP" also calls this method. The
! LIS library must be started before (LisOmpEngine_%Initiate).
!
! Changes:
!   - 2026-11-15: the engine is set to LIS_OMP (#625)
!
! Examples
!
!```fortran
! CALL v%Initiate(NAME="v", engine="LIS_OMP", spaceCompo=2, &
!                 fedof=fedof, geofedof=geofedof)
! CALL OK(v%lis_ptr .NE. 0_INT64, "v: LIS vector is created")
!```
!
! Full example: src/modules/VectorFieldLis/examples/Initiate_test_1.F90
!
! See also: [[obj_Initiate1]], [[VectorField_:obj_Initiate2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, name, engine, fedof, geofedof, &
                                  spaceCompo, fieldType, comm, local_n, &
                                  global_n)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    !! FEDOF of the field
    CLASS(FEDOF_), TARGET, INTENT(IN) :: geofedof
    !! FEDOF of the geometry
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! number of space components
    INTEGER(I4B), INTENT(IN), OPTIONAL :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), INTENT(IN), OPTIONAL :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: Returns the local size of the vector field
!
! Size returns local_n, the number of values of the field on the
! current processor. For a serial field this is the total number of
! nodes times spaceCompo. The argument dims is not used.
!
! Returns:
!   ans : INTEGER(I4B) - local number of values (obj%local_n)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE Test_Method, ONLY: OK
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorFieldLis_) :: u
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL OK(u%SIZE() .EQ. 12, "Size = 6 nodes x 2 components")
! CALL OK(u%SIZE(dims=1) .EQ. 12, "dims is not used")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    !! vector field
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used
    INTEGER(I4B) :: ans
    !! local number of values
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: Deallocate the data stored inside the VectorFieldLis_ obj
!
! DEALLOCATE destroys the LIS vector (lis_vector_destroy) and then
! deallocates the data of VectorField_.
! Afterwards the field is not initiated, lis_ptr and spaceCompo are
! zero and the field can be initiated again. Calling it on an empty
! field does nothing.
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
! USE GlobalData, ONLY: INT64
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE Test_Method, ONLY: OK
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorFieldLis_) :: u
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! ! Test 1: the field is empty after DEALLOCATE
! CALL u%DEALLOCATE()
! CALL OK(.NOT. u%IsInitiated(), "DEALLOCATE: not initiated")
! CALL OK(u%lis_ptr .EQ. 0_INT64, "DEALLOCATE: lis_ptr = 0")
! CALL OK(u%spaceCompo .EQ. 0, "DEALLOCATE: spaceCompo = 0")
! CALL OK(u%SIZE() .EQ. 0, "DEALLOCATE: Size = 0")
!
! ! Test 2: DEALLOCATE of an empty field does nothing
! CALL u%DEALLOCATE()
! CALL OK(.NOT. u%IsInitiated(), "DEALLOCATE twice: not initiated")
!
! ! Test 3: the field can be initiated again
! CALL u%Initiate(NAME="u", engine="LIS_OMP", spaceCompo=2, &
!                 fedof=fedof, geofedof=geofedof)
! CALL OK(u%IsInitiated(), "Initiate again: IsInitiated")
! CALL OK(u%SIZE() .EQ. 12, "Initiate again: Size")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! vector field
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: Finalizer of VectorFieldLis_
!
! Calls DEALLOCATE when a VectorFieldLis_ goes out of scope.
!
! See also: [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(VectorFieldLis_), INTENT(INOUT) :: obj
    !! vector field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: Display the content of [[VectorFieldLis_]]
!
! Display prints msg and whether the LIS vector is available. If it
! is, the values are gathered into realVec (lis_vector_gather) and the
! field is displayed by the Display method of VectorField_.
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
! USE Test_Method, ONLY: OK
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
! CHARACTER(*), PARAMETER :: outFileName = "Display_test_1.out"
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorFieldLis_) :: u
! INTEGER(I4B) :: unitno, ios, nlines
! CHARACTER(256) :: line, iomsg
! LOGICAL :: isMissing
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL u%Set(VALUE=1.0_DFP)
!
! OPEN (NEWUNIT=unitno, FILE=outFileName, STATUS="REPLACE", &
!       ACTION="READWRITE")
! CALL u%Display("u: ", unitno=unitno)
! REWIND (unitno)
!
! nlines = 0
! isMissing = .FALSE.
! readLines: DO
!   READ (unitno, "(A)", IOSTAT=ios, IOMSG=iomsg) line
!   IF (ios .NE. 0) EXIT readLines
!   nlines = nlines + 1
!   IF (INDEX(line, "lis_ptr not available") .GT. 0) isMissing = .TRUE.
! END DO readLines
! CLOSE (unitno, STATUS="DELETE")
!
! ! Test 1: the LIS vector is found and the field data is printed
! CALL OK(.NOT. isMissing, "Display: LIS vector is available")
! CALL OK(nlines .GT. 2, "Display: field data is printed")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! vector field, realVec is filled from the LIS vector
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output file, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: Import the vector field from an HDF5 file
!
! An initiated obj is deallocated first (its LIS vector is destroyed).
! The field is read by the Import method of VectorField_ (node field
! data, spaceCompo and idofs), and the engine is set to LIS_OMP, so
! the file can come from a native field. Then the LIS vector is
! created and realVec is copied into it (lis_vector_scatter).
!
! Changes:
!   - 2026-11-15: Import is available; it calls the Import method of
!     VectorField_ and creates the LIS vector (#571)
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
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
! CHARACTER(*), PARAMETER :: filename = "Import_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorFieldLis_) :: u, w
! TYPE(VectorField_) :: n
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: found(12), want(12)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", spaceCompo=2, &
!                 fedof=fedof, geofedof=geofedof)
!
! want = [(REAL(ii, DFP), ii=1, 12)]
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
! CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=-want)
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL u%Export(hdf5=h5, group="/u")
! CALL n%Export(hdf5=h5, group="/n")
! CALL h5%DEALLOCATE()
!
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
!
! ! Test 1: import a field exported by a VectorFieldLis_
! BLOCK
!   CALL w%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
!   CALL OK(w%IsInitiated(), "Import LIS: IsInitiated")
!   CALL OK(w%spaceCompo .EQ. 2, "Import LIS: spaceCompo")
!   CALL OK(w%SIZE() .EQ. 12, "Import LIS: Size")
!   CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "Import LIS: values")
! END BLOCK
!
! ! Test 2: import again into w (old LIS vector is destroyed), the file
! ! comes from a native VectorField_
! BLOCK
!   CALL w%IMPORT(hdf5=h5, group="/n", fedof=fedof, geofedof=geofedof)
!   CALL OK(w%SIZE() .EQ. 12, "Import native: Size")
!   CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found + want) .LT. tol), "Import native: values")
! END BLOCK
!
! CALL h5%DEALLOCATE()
!
! CALL w%DEALLOCATE()
! CALL n%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! Full example: src/modules/VectorFieldLis/examples/Import_test_1.F90
!
! See also: [[obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! vector field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, open for reading
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
    CLASS(FEDOF_), TARGET, INTENT(IN), OPTIONAL :: fedof, geofedof
    !! FEDOF of the field and of the geometry
    TYPE(FEDOFPointer_), INTENT(IN), OPTIONAL :: fedofs(:), geofedofs(:)
    !! FEDOFs of the physical variables, not used for a vector field
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! time FEDOF, passed to the Import method of VectorField_
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedofs(:)
    !! time FEDOFs, passed to the Import method of VectorField_
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: Export the vector field to an HDF5 file
!
! Export gathers the values of the LIS vector into realVec
! (lis_vector_gather) and then writes the field with the Export method
! of VectorField_ (node field data, spaceCompo, idofs and values) in
! the group of the HDF5 file. In the debug build it raises an error if
! the LIS vector is not created.
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
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
! CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorFieldLis_) :: u
! TYPE(VectorField_) :: y
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: want(12), found(12)
! INTEGER(I4B) :: tsize, ii
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
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
! CALL y%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof)
! CALL h5%DEALLOCATE()
!
! ! Test 1: the exported data is read back
! CALL OK(y%IsInitiated(), "Import: IsInitiated")
! CALL OK(y%spaceCompo .EQ. 2, "Import: spaceCompo")
! CALL OK(y%SIZE() .EQ. 12, "Import: Size")
!
! ! Test 2: the exported values are the values of the LIS vector
! BLOCK
!   CALL y%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
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
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! vector field, realVec is filled from the LIS vector
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, open for writing
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Copy another vector field, obj = VALUE
!
! For a VectorFieldLis_ VALUE the LIS vector is copied
! (lis_vector_copy). For a native VectorField_ VALUE all its values are
! copied into the LIS vector by SetMultiple. Both fields must have the
! same size. In debug mode an error is raised for another type of
! VALUE.
!
! Changes:
!   - 2026-11-15: override of Set13 of VectorField_ (#571)
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
! USE Test_Method, ONLY: OK
! USE VectorField_Class, ONLY: VectorField_
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(VectorFieldLis_) :: u, w
! TYPE(VectorField_) :: n
! REAL(DFP) :: want(12), found(12)
! INTEGER(I4B) :: tsize, ii
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL w%Initiate(NAME="w", engine="LIS_OMP", spaceCompo=2, &
!                 fedof=fedof, geofedof=geofedof)
! CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", spaceCompo=2, &
!                 fedof=fedof, geofedof=geofedof)
!
! want = [(REAL(ii, DFP), ii=1, 12)]
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
!
! ! Test 1: native from LIS
! BLOCK
!   CALL n%Set(VALUE=0.0_DFP)
!   CALL n%Set(VALUE=u)
!   CALL n%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "n = u")
! END BLOCK
!
! ! Test 2: LIS from LIS
! BLOCK
!   CALL w%Set(VALUE=0.0_DFP)
!   CALL w%Set(VALUE=u)
!   CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "w = u")
! END BLOCK
!
! ! Test 3: LIS from native
! BLOCK
!   CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=-want)
!   CALL w%Set(VALUE=n)
!   CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found + want) .LT. tol), "w = n")
! END BLOCK
!
! ! Test 4: u is not changed
! BLOCK
!   CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
! END BLOCK
!
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
! Full example: src/modules/VectorFieldLis/examples/Set_test_2.F90

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, VALUE)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field, obj = VALUE
    CLASS(VectorField_), INTENT(INOUT) :: VALUE
    !! vector field to copy
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetSingle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set or add one value, obj(indx) = [obj(indx) +] scale * VALUE
!
! Calls lis_vector_set_value with LIS_INS_VALUE (set) or LIS_ADD_VALUE
! (add). indx is the index of the value in the LIS vector:
! (c-1)*tNodes + I for node I and space component c.
!
! Examples
!
!```fortran
! CALL u%SetSingle(indx=8, VALUE=3.0_DFP)
! CALL u%GetSingle(indx=8, VALUE=areal)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetSingle]], [[obj_SetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
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
!                                                           SetAll@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set or add one value to all entries
!
! Set: lis_vector_set_all. Add: lis_vector_shift with -scale * VALUE
! (LIS computes x_i - sigma on the local block of each process).
!
! Changes:
!   - 2026-11-17: add with lis_vector_shift; before, a loop of
!     lis_vector_set_value over indices 1..local_n, which was slow and
!     used local indices as global ones under MPI
!
! Examples
!
!```fortran
! CALL u%SetAll(VALUE=1.0_DFP)
! CALL u%SetAll(VALUE=2.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetSingle]], [[obj_SetMultiple4]]

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set or add
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set or add values at given indices
!
! obj(indx(i)) = [obj(indx(i)) +] scale * VALUE(i), by
! lis_vector_set_values4.
!
! Examples
!
!```fortran
! CALL u%SetMultiple(indx=[2, 7, 12], VALUE=[1.0_DFP, 1.0_DFP, 1.0_DFP], &
!                    scale=10.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple1(obj, indx, VALUE, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
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
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set or add values in a range
!
! obj(istart:iend:stride) = [obj(...) +] scale * VALUE(1:tsize), by
! lis_vector_set_values5, tsize = (iend - istart) / stride + 1.
!
! Examples
!
!```fortran
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetMultiple2]], [[obj_SetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple2(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
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
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set or add a range of an array in a range
!
! obj(istart:iend:stride) = [obj(...) +] scale *
! VALUE(istart_value:iend_value:stride_value), by
! lis_vector_set_values8.
!
! Examples
!
!```fortran
! CALL u%SetMultiple(istart=7, iend=12, stride=1, VALUE=want, &
!                    istart_value=1, iend_value=6, stride_value=1)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_GetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
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
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set or add one value in a range
!
! obj(istart:iend:stride) = [obj(...) +] scale * VALUE, by
! lis_vector_set_values6.
!
! Examples
!
!```fortran
! CALL u%SetMultiple(istart=7, iend=12, stride=1, VALUE=-1.0_DFP)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetAll]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple4(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
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
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set or add a range of another node field in a range
!
! obj(istart:iend:stride) = [obj(...) +] scale *
! VALUE(istart_value:iend_value:stride_value).
!
! - VALUE is native: its values (GetPointer) are passed to
!   SetMultiple3, no copy.
! - VALUE uses the LIS engine: scale times the range of VALUE is set
!   (or added) in the range of obj (lis_vector_set_values9), no
!   copy. obj and VALUE can be the same field when the two ranges do
!   not overlap.
! - otherwise (other engine) the range of VALUE is copied into a
!   temporary array
!   (GetMultiple), then set by SetMultiple3.
!
! Examples
!
!```fortran
! CALL v%SetMultiple(istart=7, iend=12, stride=1, VALUE=u, istart_value=1, &
!                    iend_value=6, stride_value=1)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple3]], [[obj_GetMultiple4]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple5(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(VectorFieldLis_), TARGET, INTENT(INOUT) :: obj
    !! LIS vector field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: VALUE
    !! node field from which the values are taken
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of entries of VALUE
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Not available, returns NULL
!
! The values are stored in the LIS vector, not in realVec. The
! result is NULL; in debug mode an error is raised. Use GetMultiple
! instead.
!
! See also: [[obj_GetMultiple2]]

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(VectorFieldLis_), TARGET, INTENT(IN) :: obj
    !! LIS vector field
    REAL(DFP), POINTER :: ans(:)
    !! always NULL
  END FUNCTION obj_GetPointer
END INTERFACE


!----------------------------------------------------------------------------
!                                                        GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Get one value, VALUE = obj(indx)
!
! Calls lis_vector_get_value.
!
! Examples
!
!```fortran
! CALL u%GetSingle(indx=8, VALUE=areal)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetSingle]]

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
    INTEGER(I4B), INTENT(IN) :: indx
    !! index of the entry, 1 <= indx <= obj%Size()
    REAL(DFP), INTENT(OUT) :: VALUE
    !! value of the entry
  END SUBROUTINE obj_GetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Get the values at given indices
!
! VALUE(i) = obj(indx(i)), by lis_vector_get_values_from_index.
!
! Examples
!
!```fortran
! CALL u%GetMultiple(indx=[2, 7, 12], VALUE=found, tsize=tsize)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple1(obj, indx, VALUE, tsize)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! indices of the entries
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! values, SIZE(VALUE) >= SIZE(indx)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE, SIZE(indx)
  END SUBROUTINE obj_GetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Get the values in a range
!
! VALUE(1:tsize) = obj(istart:iend:stride), by
! lis_vector_get_values_from_range.
!
! Examples
!
!```fortran
! CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_GetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple2(obj, istart, iend, stride, VALUE, tsize)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! values, SIZE(VALUE) >= tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE
  END SUBROUTINE obj_GetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Get the values in a range into a range of an array
!
! VALUE(istart_value:iend_value:stride_value) =
! obj(istart:iend:stride), by lis_vector_get_values_from_range2.
!
! Examples
!
!```fortran
! CALL u%GetMultiple(istart=7, iend=12, stride=1, VALUE=found, &
!                    istart_value=1, iend_value=6, stride_value=1, &
!                    tsize=tsize)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetMultiple2]], [[obj_GetMultiple4]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     tsize)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
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
!                                                      GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set or add scale times a range into a range of an array
!
! VALUE(range_v) = [VALUE(range_v) +] scale * obj(range), where
! range = istart:iend:stride and range_v = istart_value:iend_value:
! stride_value. Set: lis_vector_get_values_from_range2 writes in
! VALUE, then the range is scaled. Add: the values are copied into a
! temporary array first. SetMultiple5 of a node field calls this
! method when VALUE is a LIS field.
!
! Examples
!
!```fortran
! CALL u%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    istart_value=7, iend_value=12, stride_value=1, &
!                    scale=2.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/VectorFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetMultiple3]], [[obj_SetMultiple5]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple4(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values are set or added in the range of VALUE
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of entries of VALUE
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_GetMultiple4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: obj = obj + scale * x
!
! Calls lis_vector_axpy. x must use the LIS engine and have the same
! size as obj.
!
! Examples
!
!```fortran
! CALL w%AXPY(x=x, scale=2.0_DFP)
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY2]], [[obj_AXPY3]], [[obj_SCAL]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY1(obj, x, scale)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field, obj = obj + scale * x
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x
    !! node field with the LIS engine
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_AXPY1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: obj = obj + a1 * x1 + a2 * x2
!
! Calls lis_vector_axpy twice. x1 and x2 must use the LIS engine.
!
! Examples
!
!```fortran
! CALL w%AXPY(x1=x, x2=y, a1=1.0_DFP, a2=-1.0_DFP)
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY1]], [[obj_AXPY3]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY2(obj, x1, x2, a1, a2)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
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
!                                                            AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: obj = obj + a1 * x1 + a2 * x2 + a3 * x3
!
! Calls lis_vector_axpy three times. x1, x2 and x3 must use the LIS
! engine.
!
! Examples
!
!```fortran
! CALL w%AXPY(x1=y, x2=z, x3=x, a1=1.0_DFP, a2=0.1_DFP, a3=-1.0_DFP)
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY1]], [[obj_AXPY2]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY3(obj, x1, x2, x3, a1, a2, a3)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
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
!                                                            SCAL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: obj = scale * obj
!
! Calls lis_vector_scale.
!
! Examples
!
!```fortran
! CALL w%SCAL(scale=-2.0_DFP)
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY1]]

INTERFACE
  MODULE SUBROUTINE obj_SCAL(obj, scale)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_SCAL
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Copy@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: obj = obj2
!
! Calls lis_vector_copy. obj2 must use the same engine (LIS_OMP) and
! have the same size as obj; the engine is checked in debug mode. To
! copy a field of another engine use Set(VALUE=obj2).
!
! Changes:
!   - 2026-11-15: debug check of the engine of obj2 (#617)
!
! Examples
!
!```fortran
! CALL w%COPY(obj2=x)
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Swap]]

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field, obj = obj2
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! node field of the same size and engine
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Swap@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Swap the values of obj and obj2
!
! Calls lis_vector_swap. obj2 must use the LIS engine and have the
! same size as obj.
!
! Examples
!
!```fortran
! CALL w%Swap(obj2=y)
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Copy]]

INTERFACE
  MODULE SUBROUTINE obj_Swap(obj, obj2)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! first node field, on return it contains the values of obj2
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj2
    !! second node field (LIS engine), on return it contains the values
    !! of obj
  END SUBROUTINE obj_Swap
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: L2 norm of the values
!
! Calls lis_vector_nrm2.
!
! Examples
!
!```fortran
! CALL OK(ABS(x%Norm2() - SQRT(650.0_DFP)) .LT. tol, "Norm2")
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Norm1]], [[obj_Normi]]

INTERFACE
  MODULE FUNCTION obj_Norm2(obj) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    !! LIS vector field
    REAL(DFP) :: ans
    !! L2 norm
  END FUNCTION obj_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Norm1@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: L1 norm of the values
!
! Calls lis_vector_nrm1.
!
! Examples
!
!```fortran
! CALL OK(ABS(x%Norm1() - 78.0_DFP) .LT. tol, "Norm1")
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Norm2]], [[obj_Normi]]

INTERFACE
  MODULE FUNCTION obj_Norm1(obj) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    !! LIS vector field
    REAL(DFP) :: ans
    !! L1 norm
  END FUNCTION obj_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Normi@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Infinity norm of the values
!
! Calls lis_vector_nrmi.
!
! Examples
!
!```fortran
! CALL OK(ABS(x%Normi() - 12.0_DFP) .LT. tol, "Normi")
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Norm1]], [[obj_Norm2]]

INTERFACE
  MODULE FUNCTION obj_Normi(obj) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    !! LIS vector field
    REAL(DFP) :: ans
    !! infinity norm
  END FUNCTION obj_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                     DOT_PRODUCT@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Dot product of obj and obj2
!
! Calls lis_vector_dot. obj2 must use the LIS engine and have the
! same size as obj.
!
! Examples
!
!```fortran
! CALL OK(ABS(x%DOT_PRODUCT(obj2=z) - 780.0_DFP) .LT. tol, "DOT_PRODUCT")
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Norm2]]

INTERFACE
  MODULE FUNCTION obj_DOT_PRODUCT(obj, obj2) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    !! LIS vector field
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! node field with the LIS engine
    REAL(DFP) :: ans
    !! dot product
  END FUNCTION obj_DOT_PRODUCT
END INTERFACE

!----------------------------------------------------------------------------
!                                                            PMUL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Entry by entry product, obj = obj1 * obj2
!
! Calls lis_vector_pmul. obj1 and obj2 must use the LIS engine and
! have the same size as obj.
!
! Examples
!
!```fortran
! CALL w%PMUL(obj1=x, obj2=z)
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Reciprocal]]

INTERFACE
  MODULE SUBROUTINE obj_PMUL(obj, obj1, obj2)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field, result
    CLASS(AbstractNodeField_), INTENT(IN) :: obj1
    !! first node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! second node field with the LIS engine
  END SUBROUTINE obj_PMUL
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Reciprocal@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Entry by entry reciprocal, obj = 1 / obj
!
! Calls lis_vector_reciprocal. The entries must not be zero.
!
! Examples
!
!```fortran
! CALL w%Reciprocal()
!```
!
! Full example: src/modules/VectorFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_PMUL]]

INTERFACE
  MODULE SUBROUTINE obj_Reciprocal(obj)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    !! LIS vector field
  END SUBROUTINE obj_Reciprocal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VectorFieldLis_Class
