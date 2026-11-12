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

CHARACTER(*), PARAMETER :: modName = "STScalarFieldLis_Class"

PUBLIC :: STScalarFieldLis_
PUBLIC :: STScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         STScalarFieldLis_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-23
! summary: Space-time scalar field for the LIS_OMP engine
!
! STScalarFieldLis_ is meant to be a STScalarField_ whose values are
! stored in a LIS vector (lis_ptr), like ScalarFieldLis_ for
! ScalarField_. The class is not finished:
!
!- it has no Initiate of its own, so no LIS vector is created
!  (lis_ptr stays 0) and Initiate and ImportFromToml work as for
!  STScalarField_,
!- SetSingle, SetAll, SetMultiple, GetSingle, GetMultiple and the BLAS
!  methods are not overridden, so the values are stored in realVec,
!- Display and Export gather the values from the LIS vector, which
!  does not exist; Import is not available,
!- the Set13 binding is private and does not override the Set13 of
!  STScalarField_.
!
! See bugs-report/STScalarFieldLis_bug_report.md. Use STScalarField_
! with the native engine until the class is implemented.
!
! See also: [[STScalarField_]], [[ScalarFieldLis_]]

TYPE, EXTENDS(STScalarField_) :: STScalarFieldLis_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
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
  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
  !! Copy another space-time scalar field (does not override the
  !! private Set13 of STScalarField_)

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Number of values of the field
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! Not available, returns NULL
END TYPE STScalarFieldLis_

!----------------------------------------------------------------------------
!                                                    STScalarFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldLisPointer_
  CLASS(STScalarFieldLis_), POINTER :: ptr => NULL()
  !! pointer to a LIS space-time scalar field
END TYPE STScalarFieldLisPointer_

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
! summary: Import the field from an HDF5 file (not yet available)
!
! The node field data are read by AbstractNodeFieldImport and
! timeCompo from group/timeCompo; then the LIS vector is created and
! realVec is copied into it (lis_vector_scatter). The routine is under
! development: in debug mode it raises an error (see
! bugs-report/STScalarFieldLis_bug_report.md, M-02).
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
! vector is not available, which is currently always the case (see
! bugs-report/STScalarFieldLis_bug_report.md, H-01).
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
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-23
! summary: Number of values of the field
!
! Returns local_n, the number of values stored on this process
! (number of space DOFs times timeCompo for a serial field).
!
! Returns:
!   ans : INTEGER(I4B) - number of values

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
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Copy another space-time scalar field, obj = VALUE
!
! For a STScalarFieldLis_ VALUE the LIS vector is copied
! (lis_vector_copy). For a native STScalarField_ VALUE the routine
! calls obj%Set(VALUE=realvec), which is Set2 of STScalarField_ (one
! value for each time component), not a copy of all values. The
! binding is private and does not override the Set13 of STScalarField_,
! so Set(VALUE=field) does not call this routine (see
! bugs-report/STScalarFieldLis_bug_report.md, M-01).

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, VALUE)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! LIS space-time scalar field, obj = VALUE
    CLASS(STScalarField_), INTENT(IN) :: VALUE
    !! space-time scalar field to copy
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-23
! summary: Not available, returns NULL
!
! The result is NULL and an error is raised. Use Get instead.
!
! Returns:
!   ans : REAL(DFP), POINTER - NULL

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), TARGET, INTENT(IN) :: obj
    !! LIS space-time scalar field
    REAL(DFP), POINTER :: ans(:)
    !! always NULL
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STScalarFieldLis_Class
