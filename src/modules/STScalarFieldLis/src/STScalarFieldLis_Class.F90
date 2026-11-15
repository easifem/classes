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
! summary: Space-time scalar field with the LIS engine is defined

MODULE STScalarFieldLis_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: FEVariable_
USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE GlobalData, ONLY: DFP, I4B, LGT
USE HDF5File_Class, ONLY: HDF5File_
USE STScalarField_Class, ONLY: STScalarField_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_

IMPLICIT NONE
PRIVATE

PUBLIC :: STScalarFieldLis_
PUBLIC :: STScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         STScalarFieldLis_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-23
! summary: Space-time scalar field for the LIS_OMP engine
!
! STScalarFieldLis_ is a STScalarField_ whose values are stored in a
! LIS vector (lis_ptr), as ScalarFieldLis_ is for ScalarField_. The
! layout of the values is the one of STScalarField_: for each time
! component a, the values of the space DOFs, so the value of space
! node I and time component a has the index (a-1)*tNodes + I.
!
! Initiate and ImportFromToml create the LIS vector (local_n values).
! SetSingle, SetAll, SetMultiple, GetSingle, GetMultiple and the BLAS
! methods work on the LIS vector; the methods of STScalarField_ (Set,
! Get, ApplyDirichletBC, ...) call them, so they work on the LIS
! vector too. The values are not stored in realVec: GetPointer is not
! available, Display and Export gather the LIS vector into realVec
! first.
!
! The storage, Get and BLAS routines are shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis).
!
! Changes:
!   - 2026-11-14: Initiate creates the LIS vector; storage, Get and
!     BLAS methods work on the LIS vector
!
! See also: [[STScalarField_]], [[ScalarFieldLis_]]

TYPE, EXTENDS(STScalarField_) :: STScalarFieldLis_
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
END TYPE STScalarFieldLis_

!----------------------------------------------------------------------------
!                                                    STScalarFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldLisPointer_
  CLASS(STScalarFieldLis_), POINTER :: ptr => NULL()
  !! pointer to a LIS space-time scalar field
END TYPE STScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Initiate a LIS space-time scalar field by copying another node
!   field
!
! The data of obj2 are copied into obj by the Initiate1 method of
! STScalarField_ (node field data, timeCompo and idofs when obj2 is a
! space-time scalar field). Then a new LIS vector is created for obj
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
! USE LisOmpEngine_Class, ONLY: LisOmpEngine_
! USE STScalarField_Class, ONLY: STScalarField_
! USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarFieldLis_) :: u, v, c, c2
! TYPE(STScalarField_) :: n
! REAL(DFP) :: found(12), want(12)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
! CALL v%Initiate(NAME="v", engine="LIS_OMP", fedof=fedof, &
!                 geofedof=geofedof, timefedof=timefedof)
!
! ! Test 1: fields from the toml file and from Initiate
! CALL OK(u%IsInitiated(), "u: IsInitiated")
! CALL OK(u%timeCompo .EQ. 2, "u: timeCompo")
! CALL OK(u%SIZE() .EQ. 12, "u: Size")
! CALL OK(u%lis_ptr .NE. 0, "u: LIS vector created")
! CALL OK(v%SIZE() .EQ. 12, "v: Size")
! CALL OK(v%lis_ptr .NE. 0, "v: LIS vector created")
!
! ! Test 2: u and v have their own LIS vectors
! CALL u%SetAll(VALUE=1.0_DFP)
! CALL v%SetAll(VALUE=2.0_DFP)
! CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "u: values")
! CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 2.0_DFP) .LT. tol), "v: values")
!
! ! Test 3: copy of a LIS field, with its own LIS vector
! want = [(REAL(ii, DFP), ii=1, 12)]
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
! CALL c%Initiate(obj2=u, copyFull=math%yes)
! CALL OK(c%timeCompo .EQ. u%timeCompo, "copy: timeCompo")
! CALL c%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - want) .LT. tol), "copy: values")
! CALL c%SetAll(VALUE=0.0_DFP)
! CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - want) .LT. tol), "copy: u unchanged")
!
! ! Test 4: copy of a native space-time field
! CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof, timefedof=timefedof)
! CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=-want)
! CALL c2%Initiate(obj2=n, copyFull=math%yes)
! CALL OK(c2%lis_ptr .NE. 0, "native copy: LIS vector created")
! CALL OK(c2%GetEngineName() .EQ. "LIS_OMP", "native copy: engine")
! CALL c2%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found + want) .LT. tol), "native copy: values")
!
! CALL c2%DEALLOCATE()
! CALL n%DEALLOCATE()
! CALL c%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate2]], [[STScalarField_:obj_Initiate1]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field to be initiated
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! field to copy, it should be a child of AbstractNodeField_
    !! (STScalarFieldLis_ or a native STScalarField_)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyFull
    !! passed to the Initiate1 method of STScalarField_
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyStructure
    !! passed to the Initiate1 method of STScalarField_
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: usePointer
    !! passed to the Initiate1 method of STScalarField_
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Initiate a LIS space-time scalar field by arguments
!
! The space-time scalar field data are set by the Initiate method of
! STScalarField_ (timeCompo from timefedof, or from timeCompo(1) when
! there is no timefedof). Then the LIS vector of local size local_n and
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
! USE STScalarField_Class, ONLY: STScalarField_
! USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarFieldLis_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarFieldLis_) :: u, v, c, c2
! TYPE(STScalarField_) :: n
! REAL(DFP) :: found(12), want(12)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
! CALL v%Initiate(NAME="v", engine="LIS_OMP", fedof=fedof, &
!                 geofedof=geofedof, timefedof=timefedof)
!
! ! Test 1: fields from the toml file and from Initiate
! CALL OK(u%IsInitiated(), "u: IsInitiated")
! CALL OK(u%timeCompo .EQ. 2, "u: timeCompo")
! CALL OK(u%SIZE() .EQ. 12, "u: Size")
! CALL OK(u%lis_ptr .NE. 0, "u: LIS vector created")
! CALL OK(v%SIZE() .EQ. 12, "v: Size")
! CALL OK(v%lis_ptr .NE. 0, "v: LIS vector created")
!
! ! Test 2: u and v have their own LIS vectors
! CALL u%SetAll(VALUE=1.0_DFP)
! CALL v%SetAll(VALUE=2.0_DFP)
! CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "u: values")
! CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 2.0_DFP) .LT. tol), "v: values")
!
! ! Test 3: copy of a LIS field, with its own LIS vector
! want = [(REAL(ii, DFP), ii=1, 12)]
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
! CALL c%Initiate(obj2=u, copyFull=math%yes)
! CALL OK(c%timeCompo .EQ. u%timeCompo, "copy: timeCompo")
! CALL c%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - want) .LT. tol), "copy: values")
! CALL c%SetAll(VALUE=0.0_DFP)
! CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - want) .LT. tol), "copy: u unchanged")
!
! ! Test 4: copy of a native space-time field
! CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof, timefedof=timefedof)
! CALL n%SetMultiple(istart=1, iend=12, stride=1, VALUE=-want)
! CALL c2%Initiate(obj2=n, copyFull=math%yes)
! CALL OK(c2%lis_ptr .NE. 0, "native copy: LIS vector created")
! CALL OK(c2%GetEngineName() .EQ. "LIS_OMP", "native copy: engine")
! CALL c2%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found + want) .LT. tol), "native copy: values")
!
! CALL c2%DEALLOCATE()
! CALL n%DEALLOCATE()
! CALL c%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate1]], [[STScalarField_:obj_Initiate2]]

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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field to be initiated
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine, "LIS_OMP"
    INTEGER(I4B), INTENT(IN), OPTIONAL :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    INTEGER(I4B), INTENT(IN), OPTIONAL :: storageFMT
    !! storage format, not used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: comm
    !! communication group, only for a parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_n
    !! local size of the field, only for a parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: global_n
    !! global size of the field, only for a parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: spaceCompo(:)
    !! space components, not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSpaceCompo
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSpaceCompoScalar
    !! not used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: timeCompo(:)
    !! time components, timeCompo(1) is used when there is no timefedof
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTimeCompo
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTimeCompoScalar
    !! not used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tPhysicalVarNames
    !! not used
    CHARACTER(*), INTENT(IN), OPTIONAL :: physicalVarNames(:)
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isPhysicalVarNames
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isPhysicalVarNamesScalar
    !! not used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tNodes(:)
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTNodes
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTNodesScalar
    !! not used
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tSize
    !! not used
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof, geofedof
    !! FEDOF of the field and FEDOF of the geometry
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! TimeFEDOF, its total DOF is timeCompo
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-23
! summary: Finalizer, calls Deallocate
!
! See also: [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-25
! summary: Destroy the LIS vector and deallocate the object
!
! The LIS vector is destroyed (lis_vector_destroy), then the data of
! STScalarField_ are deallocated. The generic name
! STScalarFieldLisDeallocate is not public.

INTERFACE STScalarFieldLisDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
  END SUBROUTINE obj_Deallocate
END INTERFACE STScalarFieldLisDeallocate

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-26
! summary: Display the content of [[STScalarFieldLis_]]
!
! Displays msg and whether the LIS vector is available. If it is, the
! values are gathered into realVec (lis_vector_gather) and the field is
! displayed by the Display method of STScalarField_.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
    CHARACTER(*), INTENT(IN) :: msg
    !! message displayed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Import the field from an HDF5 file
!
! The LIS vector of obj is destroyed, if any. The field is read by the
! Import method of STScalarField_ (node field data, timeCompo and
! idofs), and the engine is set to LIS_OMP, so the file can come from
! a native field. Then the LIS vector is created and realVec is copied
! into it (lis_vector_scatter).
!
! Changes:
!   - 2026-11-14: Import is available; it calls the Import method of
!     STScalarField_ and creates the LIS vector
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
! USE STScalarField_Class, ONLY: STScalarField_
! USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
! USE Test_Method, ONLY: OK
! USE TimeFEDOF_Class, ONLY: TimeFEDOF_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "STScalarFieldLis_test.toml"
! CHARACTER(*), PARAMETER :: filename = "Import_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(TimeFEDOF_), TARGET :: timefedof
! TYPE(STScalarFieldLis_) :: u, w
! TYPE(STScalarField_) :: n
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: found(12), want(12)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL lis%Initiate()
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
!                       fedof=fedof, geofedof=geofedof, &
!                       timefedof=timefedof, dom=dom)
! CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof, timefedof=timefedof)
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
! ! Test 1: import a field exported by a STScalarFieldLis_
! BLOCK
!   CALL w%IMPORT(hdf5=h5, group="/u", fedof=fedof, geofedof=geofedof, &
!                 timefedof=timefedof)
!   CALL OK(w%IsInitiated(), "Import LIS: IsInitiated")
!   CALL OK(w%SIZE() .EQ. 12, "Import LIS: Size")
!   CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, &
!                      tsize=tsize)
!   CALL OK(ALL(ABS(found - want) .LT. tol), "Import LIS: values")
! END BLOCK
!
! ! Test 2: import again into w (old LIS vector is destroyed), the file
! ! comes from a native STScalarField_
! BLOCK
!   CALL w%IMPORT(hdf5=h5, group="/n", fedof=fedof, geofedof=geofedof, &
!                 timefedof=timefedof)
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
! CALL timefedof%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL lis%DEALLOCATE()
! END PROGRAM main
!```
!
! Full example: src/modules/STScalarFieldLis/examples/Import_test_1.F90
!
! See also: [[obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened for reading
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
    CLASS(FEDOF_), TARGET, INTENT(IN), OPTIONAL :: fedof, geofedof
    !! FEDOF of the field and FEDOF of the geometry
    TYPE(FEDOFPointer_), INTENT(IN), OPTIONAL :: fedofs(:), geofedofs(:)
    !! FEDOFs for multiple physical variables
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! TimeFEDOF (not used)
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedofs(:)
    !! TimeFEDOFs (not used)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Export the field to an HDF5 file
!
! The values of the LIS vector are gathered into realVec
! (lis_vector_gather), then the field is written by the Export method
! of STScalarField_. In debug mode an error is raised when the LIS
! vector is not available.
!
! See also: [[STScalarField_:obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened for writing
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
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
! (a-1)*tNodes + I for space node I and time component a.
!
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/SetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%SetSingle(indx=8, VALUE=3.0_DFP)
! CALL u%GetSingle(indx=8, VALUE=areal)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetSingle]], [[obj_SetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! Set: lis_vector_set_all. Add: lis_vector_set_value with
! LIS_ADD_VALUE for each entry.
!
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/SetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%SetAll(VALUE=1.0_DFP)
! CALL u%SetAll(VALUE=2.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetSingle]], [[obj_SetMultiple4]]

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/SetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%SetMultiple(indx=[2, 7, 12], VALUE=[1.0_DFP, 1.0_DFP, 1.0_DFP], &
!                    scale=10.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple1(obj, indx, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/SetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetMultiple2]], [[obj_SetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple2(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/SetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%SetMultiple(istart=7, iend=12, stride=1, VALUE=want, &
!                    istart_value=1, iend_value=6, stride_value=1)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_GetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/SetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%SetMultiple(istart=7, iend=12, stride=1, VALUE=-1.0_DFP)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetAll]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple4(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/SetMethods.F90).
!
! Examples
!
!```fortran
! CALL v%SetMultiple(istart=7, iend=12, stride=1, VALUE=u, istart_value=1, &
!                    iend_value=6, stride_value=1)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple3]], [[obj_GetMultiple4]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple5(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(STScalarFieldLis_), TARGET, INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/GetMethods.F90).
!
! See also: [[obj_GetMultiple2]]

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), TARGET, INTENT(IN) :: obj
    !! LIS space-time scalar field
    REAL(DFP), POINTER :: ans(:)
    !! always NULL
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Number of values of the field
!
! Returns local_n, the number of values stored on this process
! (number of space DOFs times timeCompo for a serial field).
!
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/GetMethods.F90).
!
! Examples
!
!```fortran
! CALL OK(u%SIZE() .EQ. 12, "u: Size")
!```
!
! Full example: src/modules/STScalarFieldLis/examples/Initiate_test_1.F90
!
! See also: [[obj_GetMultiple2]]

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used
    INTEGER(I4B) :: ans
    !! number of values
  END FUNCTION obj_Size
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/GetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%GetSingle(indx=8, VALUE=areal)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetSingle]]

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/GetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%GetMultiple(indx=[2, 7, 12], VALUE=found, tsize=tsize)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple1(obj, indx, VALUE, tsize)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/GetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=found, tsize=tsize)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_GetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple2(obj, istart, iend, stride, VALUE, tsize)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/GetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%GetMultiple(istart=7, iend=12, stride=1, VALUE=found, &
!                    istart_value=1, iend_value=6, stride_value=1, &
!                    tsize=tsize)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetMultiple2]], [[obj_GetMultiple4]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     tsize)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/GetMethods.F90).
!
! Examples
!
!```fortran
! CALL u%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    istart_value=7, iend_value=12, stride_value=1, &
!                    scale=2.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetMultiple3]], [[obj_SetMultiple5]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple4(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL w%AXPY(x=x, scale=2.0_DFP)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY2]], [[obj_AXPY3]], [[obj_SCAL]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY1(obj, x, scale)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field, obj = obj + scale * x
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL w%AXPY(x1=x, x2=y, a1=1.0_DFP, a2=-1.0_DFP)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY1]], [[obj_AXPY3]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY2(obj, x1, x2, a1, a2)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL w%AXPY(x1=y, x2=z, x3=x, a1=1.0_DFP, a2=0.1_DFP, a3=-1.0_DFP)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY1]], [[obj_AXPY2]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY3(obj, x1, x2, x3, a1, a2, a3)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL w%SCAL(scale=-2.0_DFP)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY1]]

INTERFACE
  MODULE SUBROUTINE obj_SCAL(obj, scale)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
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
! Calls lis_vector_copy. obj2 must use the LIS engine and have the
! same size as obj.
!
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL w%COPY(obj2=x)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Swap]]

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field, obj = obj2
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! node field with the LIS engine
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL w%Swap(obj2=y)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Copy]]

INTERFACE
  MODULE SUBROUTINE obj_Swap(obj, obj2)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL OK(ABS(x%Norm2() - SQRT(650.0_DFP)) .LT. tol, "Norm2")
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Norm1]], [[obj_Normi]]

INTERFACE
  MODULE FUNCTION obj_Norm2(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL OK(ABS(x%Norm1() - 78.0_DFP) .LT. tol, "Norm1")
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Norm2]], [[obj_Normi]]

INTERFACE
  MODULE FUNCTION obj_Norm1(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL OK(ABS(x%Normi() - 12.0_DFP) .LT. tol, "Normi")
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Norm1]], [[obj_Norm2]]

INTERFACE
  MODULE FUNCTION obj_Normi(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL OK(ABS(x%DOT_PRODUCT(obj2=z) - 780.0_DFP) .LT. tol, "DOT_PRODUCT")
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Norm2]]

INTERFACE
  MODULE FUNCTION obj_DOT_PRODUCT(obj, obj2) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! LIS space-time scalar field
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL w%PMUL(obj1=x, obj2=z)
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_Reciprocal]]

INTERFACE
  MODULE SUBROUTINE obj_PMUL(obj, obj1, obj2)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field, result
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
! The routine is shared with ScalarFieldLis_
! (src/submodules/include/NodeFieldLis/BlasMethods.F90).
!
! Examples
!
!```fortran
! CALL w%Reciprocal()
!```
!
! Full example: src/modules/STScalarFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_PMUL]]

INTERFACE
  MODULE SUBROUTINE obj_Reciprocal(obj)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field
  END SUBROUTINE obj_Reciprocal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STScalarFieldLis_Class
