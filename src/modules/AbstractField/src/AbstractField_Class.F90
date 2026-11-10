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
!

!> author: Vikas Sharma, Ph. D.
! date: 16 Jul 2021
! summary: Abstract field is designed to handle fields in FEM
!
!# Introduction
! - In FEM, we use variables of different ranks. These varibles will be
! designated as the field. These fields can be defined at:
!   - Spatial-temporal nodal points
!   - Quadrature points inside the element
! - In addition, global matrices can also be described as the field.
! - In this way, Fields are high level objects in finite element modeling.
!
! `AbstractField_` defines an abstract class. This class will be extended
! to `AbstractNodeField_`, `AbstractElementField_`, `AbstractMatrixField_`.

MODULE AbstractField_Class
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: RealVector_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE GlobalData, ONLY: DFP, I4B, INT64, LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE String_Class, ONLY: String
USE VTKFile_Class, ONLY: VTKFile_
IMPLICIT NONE

PRIVATE
PUBLIC :: AbstractFieldInitiate
PUBLIC :: AbstractFieldDisplay
PUBLIC :: AbstractFieldImport
PUBLIC :: AbstractFieldExport
PUBLIC :: AbstractFieldDeallocate
PUBLIC :: AbstractField_

!----------------------------------------------------------------------------
!                                                           AbstractField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Jul 2021
! summary: Abstract field is designed to handle fields in FEM
!
! Changes:
!   - 2026-11-10: Removed saveErrorNorm, plotWithResult, plotErrorNorm,
!     errorType and exact. They were not used by any field.
!   - 2026-11-11: Moved dof, the FEDOF pointers, the boundary conditions
!     and the methods that use them to [[AbstractNodeField_]].

TYPE, ABSTRACT :: AbstractField_
  LOGICAL(LGT) :: isInit = math%no
  !! It is true if the object is initiated
  INTEGER(I4B) :: fieldType = TypeField%normal
  !! fieldType can be normal, constant, can vary in space and/ or both.
  INTEGER(I4B) :: comm = math%zero_i
  !! communication group (MPI)
  INTEGER(I4B) :: myRank = math%zero_i
  !! rank of current processor (MPI)
  INTEGER(I4B) :: numProcs = math%one_i
  !! Total number of processors (MPI)
  INTEGER(I4B) :: global_n = math%zero_i
  !! total number of nodes on all processors (MPI)
  INTEGER(I4B) :: local_n = math%zero_i
  !! local number of nodes on a given processor (MPI)
  INTEGER(I4B) :: is = math%zero_i
  !! starting index (MPI)
  INTEGER(I4B) :: ie = math%zero_i
  !! end index + 1 (MPI)
  INTEGER(INT64) :: lis_ptr = 0_INT64
  !! lis_ptr is pointer returned by the LIS library
  !! It is used when engine is LIS_OMP or LIS_MPI
  TYPE(String) :: name
  !! name of the field
  TYPE(String) :: engine
  !! Engine of the field, for example
  !! NATIVE_SERIAL, NATIVE_OMP, NATIVE_MPI, PETSC, LIS_OMP, LIS_MPI

CONTAINS

  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by copying other fields, and different options
  GENERIC, PUBLIC :: Initiate => Initiate1
  !! Generic method to initiate the field
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the field

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data in hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk => obj_WriteData_vtk
  !! Write data in vtk file
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_hdf5 => obj_WriteData_hdf5
  !! Write data in hdf5 file
  GENERIC, PUBLIC :: WriteData => WriteData_vtk, WriteData_hdf5

  ! @GetMethods
  ! DEFERRED:
  ! These are deferred methods which should be implemented
  ! by child classes
  PROCEDURE(obj_GetTotalPhysicalVars), DEFERRED, PUBLIC, PASS(obj) :: &
    GetTotalPhysicalVars
  !! Returns the total number of physical variables
  !! For block matrices the physical variables are more than one,
  !! for example, presesure and velocity.
  PROCEDURE(obj_GetPhysicalNames), DEFERRED, PUBLIC, PASS(obj) :: &
    GetPhysicalNames
  !! Returns the names of physical variables
  PROCEDURE(obj_GetSpaceCompo), DEFERRED, PUBLIC, PASS(obj) :: GetSpaceCompo
  !! Return space components of each physical variables
  PROCEDURE(obj_GetTimeCompo), DEFERRED, PUBLIC, PASS(obj) :: GetTimeCompo
  !! Returns time component of each physical variable
  PROCEDURE(obj_GetStorageFMT), DEFERRED, PUBLIC, PASS(obj) :: GetStorageFMT
  !! Return storage format of the abstractfield

  ! @GetMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsInitiated => &
    obj_IsInitiated
  !! Returns isInit
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters of AbstractField
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  !! Returns the name
  PROCEDURE, PUBLIC, PASS(obj), NON_OVERRIDABLE :: IsConstant => &
    obj_IsConstant
  !! It returns true if the field is constant field
  !!  This routine should be implemented by child classes
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetEngineName => &
    obj_GetEngineName
  !! Get the engine name
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshField => obj_GetMeshField
  !! Get the mesh field corresponding to abstract field

  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetName => obj_SetName
  PROCEDURE, PUBLIC, PASS(obj) :: SetAll => obj_SetAll
  !! Set all the values to a constant scalar value
END TYPE AbstractField_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate by copying other fields, and different options

INTERFACE AbstractFieldInitiate
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! Another child of AbstractField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    !! copy full data
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    !! copy only structure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
    !! do not allocate pointers data, just point them to targets in obj2
  END SUBROUTINE obj_Initiate1
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate the data common to all fields
!
! This routine sets isInit, name, engine, fieldType, comm, local_n and
! global_n. It does not deallocate obj. The child classes call it from
! their own Initiate method, see [[AbstractNodeField_:Initiate]].
!
! Changes:
!   - 2026-11-11: Only the common data is set. The FEDOF pointers and
!     the other arguments moved to [[AbstractNodeField_]].
!
! Examples
!
!```fortran
! CALL AbstractFieldInitiate(obj=obj, name="u", engine="NATIVE_SERIAL")
!```
!
! This example still needs a matching test.

INTERFACE AbstractFieldInitiate
  MODULE SUBROUTINE obj_Initiate2(obj, name, engine, fieldType, comm, &
                                  local_n, global_n)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    !! field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is TypeField%normal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group, default is 0
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of the field, default is 0
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of the field, default is 0
  END SUBROUTINE obj_Initiate2
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractFieldDeallocate

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE AbstractFieldDisplay

!----------------------------------------------------------------------------
!                                                         IMPORT@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-01
! summary: Import the data common to all fields from HDF5 file
!
! This routine reads the data written by [[AbstractFieldExport]]: name,
! engine, fieldType, comm, myRank, numProcs, global_n, local_n, is and
! ie. It sets isInit to true. Entries that are absent in the file are
! skipped, so files written by older versions can be read.
!
! Changes:
!   - 2026-11-11: The FEDOF arguments and the DOF moved to the Import
!     method of [[AbstractNodeField_]].

INTERFACE AbstractFieldImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    !! field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, opened for reading
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Import
END INTERFACE AbstractFieldImport

!----------------------------------------------------------------------------
!                                                          Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-01
! summary: Export AbstractField to HDF5 file
!
! Export AbstractField data to HDF5 file.
!
INTERFACE AbstractFieldExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE AbstractFieldExport

!----------------------------------------------------------------------------
!                                                     WriteData@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_WriteData_hdf5(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_WriteData_hdf5
END INTERFACE

!----------------------------------------------------------------------------
!                                                      WriteData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data in [[VTKFile_]]

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk(obj, vtk)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
  END SUBROUTINE obj_WriteData_vtk
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-25
! summary:  Set field variables of abstract field
!
! Changes:
!   - 2026-11-11: Removed the fedof and fedofs arguments.

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, isInitiated, fieldType, name, engine, comm, myRank, numProcs, &
    global_n, local_n, is, ie, lis_ptr, tSize, realVec, dof, &
    isPMatInitiated)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isInitiated
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: myRank
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numProcs
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: is
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ie
    INTEGER(INT64), OPTIONAL, INTENT(IN) :: lis_ptr
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSize
    TYPE(RealVector_), OPTIONAL, INTENT(IN) :: realVec
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: dof
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPMatInitiated
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-20
! summary: Set the name of the field

INTERFACE
  MODULE SUBROUTINE obj_SetName(obj, name)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetAll@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-27
! summary: Set all the values

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set or add
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Get obj%isInit

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-25
! summary:  Get the field variables
!
! Changes:
!   - 2026-11-11: Removed the fedof and fedofs arguments.

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, isInitiated, fieldType, name, engine, comm, myRank, numProcs, &
    global_n, local_n, is, ie, lis_ptr, tSize, realVec, dof, &
    isPMatInitiated)
    CLASS(AbstractField_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: fieldType
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: myRank
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: numProcs
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: global_n
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: is
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ie
    INTEGER(INT64), OPTIONAL, INTENT(OUT) :: lis_ptr
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tSize
    TYPE(RealVector_), OPTIONAL, INTENT(OUT) :: realVec
    TYPE(DOF_), OPTIONAL, INTENT(OUT) :: dof
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isPMatInitiated
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalPhysicalVars@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-03
! summary:  Returns the total number of physical variables

ABSTRACT INTERFACE
  FUNCTION obj_GetTotalPhysicalVars(obj) RESULT(ans)
    IMPORT :: AbstractField_, I4B
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalPhysicalVars
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetSpaceCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Returns total number of space components
!
!# Introduction
!
! This method returns the number of space components in each
! physical variables. It is a deferred type method. It should
! be implemented in the child classes.

ABSTRACT INTERFACE
  FUNCTION obj_GetSpaceCompo(obj, tPhysicalVars) RESULT(ans)
    IMPORT :: AbstractField_, I4B
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
      !! Total number of physical variables
      !! This can be obtained From GetTotalPhysicalVars method
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetSpaceCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTimeCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Return the total number of time components in each phy vars
!
!# Introduction
!   This method returns the number of time components in each
!   physical variables. It is a deferred type method. It should
!   be implemented in the child classes.

ABSTRACT INTERFACE
  FUNCTION obj_GetTimeCompo(obj, tPhysicalVars) RESULT(ans)
    IMPORT :: AbstractField_, I4B
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTimeCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetStorageFMT@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Returns the storage format of the AbstractField
!
!# Introduction
!
! This method returns the storage format of the AbstractField.
! There are two types of storage formats: DOF_FMT, and NODES_FMT.
! This method is a deferred type method. It should be implemented
! in the child classes.

ABSTRACT INTERFACE
  FUNCTION obj_GetStorageFMT(obj) RESULT(ans)
    IMPORT :: AbstractField_, I4B
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetStorageFMT
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetPhysicalNames@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns the names of physical variables

ABSTRACT INTERFACE
  SUBROUTINE obj_GetPhysicalNames(obj, ans)
    IMPORT :: AbstractField_
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(INOUT) :: ans(:)
  END SUBROUTINE obj_GetPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns the name of the field

INTERFACE
  MODULE FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IsConstant@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns true if the field is constant
INTERFACE
  MODULE FUNCTION obj_IsConstant(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetEngineName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-19
! summary:  Get the engine name of the field

INTERFACE
  MODULE FUNCTION obj_GetEngineName(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetEngineName
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetMeshField@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-29
! summary: Get mesh field

INTERFACE
  MODULE SUBROUTINE obj_GetMeshField(obj, meshField, order, ipType)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CLASS(MeshField_), INTENT(INOUT) :: meshField
    INTEGER(I4B), INTENT(IN) :: order(3), ipType(3)
  END SUBROUTINE obj_GetMeshField
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractField_Class
