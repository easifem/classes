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
USE BaseType, ONLY: FEVariable_
USE DirichletBC_Class, ONLY: DirichletBC_
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: NODES_FMT
USE GlobalData, ONLY: NodesToDOF
USE HDF5File_Class, ONLY: HDF5File_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
USE UserFunction_Class, ONLY: UserFunction_
USE VectorField_Class, ONLY: VectorField_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "VectorFieldLis_Class"

PUBLIC :: VectorFieldLis_
PUBLIC :: VectorFieldLisPointer_

!----------------------------------------------------------------------------
!                                                           VectorFieldLis_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Vector field with the LIS engine
!
! VectorFieldLis_ is a VectorField_ whose values are meant to be kept in
! a LIS vector (lis_ptr of AbstractField_), so that the field can be used
! with the LIS linear solvers. It has no components of its own. The
! engine of the field is "LIS_OMP".
!
! A vector field has one physical variable with spaceCompo components.
! The values are stored in DOF format: component 1 of all nodes, then
! component 2 of all nodes, and so on.
!
! The class overrides Size, DEALLOCATE, Display, Import, Export and
! GetPointer. Display and Export first fill realVec from the LIS vector
! (lis_vector_gather). All other methods, including Initiate, Set and
! Get, are those of VectorField_; they work on realVec. Import is under
! development. GetPointer is not available.
!
! The LIS library must be started before a field is initiated, by
! LisOmpEngine_%Initiate (lis_initialize), and stopped after the fields
! are deallocated, by LisOmpEngine_%DEALLOCATE (lis_finalize).
!
! See also: [[VectorField_]], [[ScalarFieldLis_]], [[AbstractNodeField_]]

TYPE, EXTENDS(VectorField_) :: VectorFieldLis_
CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Get the local size of the vector field
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Destroy the LIS vector and deallocate the vector field
  FINAL :: obj_Final
  !! Finalizer, calls DEALLOCATE

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Gather the LIS vector and display the vector field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the vector field from HDF5 (under development)
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Gather the LIS vector and export the vector field to HDF5

  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! Get pointer of the vector field
  !! This method is not callable
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
!                                                    Size@ConstructorMethods
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
! deallocates the data of VectorField_ (VectorFieldDeallocate).
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
! CALL u%Initiate(NAME="u", engine="LIS_OMP", spaceCompo=[2], &
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
! Display gathers the values of the LIS vector into realVec
! (lis_vector_gather) and then prints the data of the node field
! (AbstractNodeFieldDisplay). If the LIS vector is not created, it
! prints only msg and "obj%lis_ptr not available.".
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
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: Import the vector field from an HDF5 file (under development)
!
! Import reads the node field data (AbstractNodeFieldImport) and
! spaceCompo from the group of the HDF5 file, creates the LIS vector of
! size local_n and copies realVec into it (lis_vector_scatter).
!
! This routine is under development: in the debug build it raises a
! [WIP ERROR]. The arguments timefedof and timefedofs are not used. To
! read a field exported by VectorFieldLis_%Export, import it into a
! native VectorField_ (see the Examples of Export).
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
    !! time FEDOF, not used
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedofs(:)
    !! time FEDOFs, not used
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
! (lis_vector_gather) and then writes the field with VectorFieldExport
! (node field data, spaceCompo, idofs and values) in the group of the
! HDF5 file. In the debug build it raises an error if the LIS vector is
! not created.
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
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

! INTERFACE
!   MODULE SUBROUTINE obj_Set13(obj, ivar, idof, VALUE, ivar_value, &
!                               idof_value, scale, addContribution)
!     CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: ivar
!     !! Physical variable in obj
!     INTEGER(I4B), INTENT(IN) :: idof
!     !! local degree of freedom of ivar
!     CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
!     !! value
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     !! Physical variable in VALUE
!     INTEGER(I4B), INTENT(IN) :: idof_value
!     !! local degree of freedom of ivar in VALUE
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     !! scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!     !! add or set
!   END SUBROUTINE obj_Set13
! END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

! INTERFACE
!   MODULE SUBROUTINE obj_Set14(obj, VALUE)
!     CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
!     CLASS(VectorField_), INTENT(IN) :: VALUE
!   END SUBROUTINE obj_Set14
! END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: This routine cannot be called
!
! The values of a LIS field are kept in the LIS vector, so a pointer to
! realVec would not give them. GetPointer raises an error and returns a
! null pointer. Use Get, GetMultiple or Display instead.
!
! Returns:
!   ans : REAL(DFP), POINTER - always NULL()

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(VectorFieldLis_), TARGET, INTENT(IN) :: obj
    !! vector field
    REAL(DFP), POINTER :: ans(:)
    !! null pointer
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VectorFieldLis_Class
