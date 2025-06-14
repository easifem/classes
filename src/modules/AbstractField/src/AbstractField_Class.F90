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

!> authors: Vikas Sharma, Ph. D.
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
! [[AbstractField_]] defines an abstract class. This class will be extended
! to [[AbstractNodeField_]], [[AbstractElementField_]],
! [[AbstractMatrixField_]].

MODULE AbstractField_Class
USE GlobalData, ONLY: DFP, I4B, LGT, stdout, stdin, INT64, Constant, &
                      Space, Time, SpaceTime
USE BaseType, ONLY: RealVector_, DOF_
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE VTKFile_Class, ONLY: VTKFile_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE TxtFile_Class, ONLY: TxtFile_
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE EngineOpt_Class, ONLY: TypeEngineName => TypeEngineOpt

USE tomlf, ONLY: toml_table

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractField_Class"
CHARACTER(*), PARAMETER :: myprefix = "AbstractField"

PUBLIC :: AbstractFieldInitiate
PUBLIC :: AbstractFieldDisplay
PUBLIC :: AbstractFieldImport
PUBLIC :: AbstractFieldExport
PUBLIC :: AbstractFieldDeallocate
PUBLIC :: SetAbstractFieldParam
PUBLIC :: AbstractFieldCheckEssentialParam
PUBLIC :: AbstractField_
PUBLIC :: AbstractFieldInitiate2

!----------------------------------------------------------------------------
!                                                           AbstractField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Jul 2021
! summary: Abstract field is designed to handle fields in FEM

TYPE, ABSTRACT :: AbstractField_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is true if the object is initiated
  INTEGER(I4B) :: fieldType = TypeField%normal
  !! fieldType can be normal, constant, can vary in space and/ or both.
  TYPE(String) :: name
  !! name of the field
  TYPE(String) :: engine
  !! Engine of the field, for example
  !! NATIVE_SERIAL
  !! NATIVE_OMP,
  !! NATIVE_MPI,
  !! PETSC,
  !! LIS_OMP,
  !! LIS_MPI
  INTEGER(I4B) :: comm = 0_I4B
  !! communication group (MPI)
  INTEGER(I4B) :: myRank = 0_I4B
  !! rank of current processor (MPI)
  INTEGER(I4B) :: numProcs = 1_I4B
  !! Total number of processors (MPI)
  INTEGER(I4B) :: global_n = 0_I4B
  !! total number of nodes on all processors (MPI)
  INTEGER(I4B) :: local_n = 0_I4B
  !! local number of nodes on a given processor (MPI)
  INTEGER(I4B) :: is = 0_I4B
  !! starting index (MPI)
  INTEGER(I4B) :: ie = 0_I4B
  !! end index + 1 (MPI)
  INTEGER(INT64) :: lis_ptr = 0_INT64
  !! lis_ptr is pointer returned by the LIS library
  !! It is used when engine is LIS_OMP or LIS_MPI
  CLASS(FEDOF_), POINTER :: fedof => NULL()
  !! pointer to FEDOF
  TYPE(FEDOFPointer_), ALLOCATABLE :: fedofs(:)
  !! pointer to FEDOF

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE(obj_CheckEssentialParam), DEFERRED, PUBLIC, PASS(obj) :: &
    CheckEssentialParam
  !! Check essential parameters

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate the field by reading param and given domain
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by copying other fields, and different options
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate  block fields (different physical variables) defined
  !! over different order of meshes.
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the field

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data in hdf5 file
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import data from toml file
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import data from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Generic method to import data from toml file

  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk => obj_WriteData_vtk
  !! Write data in vtk file
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_hdf5 => obj_WriteData_hdf5
  !! Write data in hdf5 file
  GENERIC, PUBLIC :: WriteData => WriteData_vtk, WriteData_hdf5

  ! GET:
  ! @GetMethods
  PROCEDURE, PASS(obj), NON_OVERRIDABLE, PUBLIC :: GetParam => obj_GetParam
  !! Get the parameters of AbstractField

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalPhysicalVars => &
    obj_GetTotalPhysicalVars
  !! Returns the total number of physical variables
  !! INFO: This routine should be implemented by child classes
  !! For block matrices the physical variables are more than one,
  !! for example, presesure and velocity.

  PROCEDURE, PUBLIC, PASS(obj) :: GetPhysicalNames => obj_GetPhysicalNames
  !! Returns the names of physical variables
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetSpaceCompo => obj_GetSpaceCompo
  !! Return space component
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeCompo => obj_GetTimeCompo
  !! Return time component
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetStorageFMT => obj_GetStorageFMT
  !! Return storage format
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalDOF => obj_GetTotalDOF
  !! Returns the total number of degree of freedoms
  !! This is same as calling Size
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalVertexDOF => &
    obj_GetTotalVertexDOF
  !! Returns the total number of vertex degree of freedoms
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEdgeDOF => obj_GetTotalEdgeDOF
  !! Returns the total number of edge degree of freedoms
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFaceDOF => obj_GetTotalFaceDOF
  !! Returns the total number of face degree of freedoms
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCellDOF => obj_GetTotalCellDOF
  !! Returns the total number of cell degree of freedoms
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj), NON_OVERRIDABLE :: isConstant => &
    obj_isConstant
  !! It returns true if the field is constant field
  !! INFO: This routine should be implemented by child classes

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Get the prefix of the field, it is necessary for Setting essential param
  !! INFO: This routine should be implemented by child classes

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj), NON_OVERRIDABLE, PUBLIC :: SetParam => obj_SetParam

END TYPE AbstractField_

!----------------------------------------------------------------------------
!                                   CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Check the essential parameters in param.

ABSTRACT INTERFACE
  SUBROUTINE obj_CheckEssentialParam(obj, param)
    IMPORT :: AbstractField_, ParameterList_
    CLASS(AbstractField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                  SetAbstractFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-04
! summary:  Set AbstractField_ parameters

INTERFACE
  MODULE SUBROUTINE SetAbstractFieldParam(param, prefix, name, engine, &
                                          fieldType, comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    !! prefix
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
  END SUBROUTINE SetAbstractFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Check essential param

INTERFACE
  MODULE SUBROUTINE AbstractFieldCheckEssentialParam(obj, param, prefix)
    CLASS(AbstractField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE AbstractFieldCheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate the field by reading param and given domain

INTERFACE AbstractFieldInitiate
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
  END SUBROUTINE obj_Initiate1
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate by copying other fields, and different options

INTERFACE AbstractFieldInitiate2
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE AbstractFieldInitiate2

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate by reading options from [[ParameterList_]]

INTERFACE AbstractFieldInitiate
  MODULE SUBROUTINE obj_Initiate3(obj, param, fedof)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(FEDOFPointer_), INTENT(IN) :: fedof(:)
  END SUBROUTINE obj_Initiate3
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
!                                                         IMPORT@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE AbstractFieldImport

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE AbstractFieldExport

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, fedof)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof
    !! if fedof is not initiated then it will be initiated by
    !! calling fedof%ImportFromToml(node) method.
    !! where node is the table field called "space".
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, fedof, &
                                        afile, filename, printToml)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the key
    CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof
    !! if fedof is not initiated then it will be initiated by
    !! calling fedof%ImportFromToml(node) method.
    !! where node is the table field called "space".
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! txt file where toml config is stored
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! you can pass the filename, then we will make
    !! the file, open it and read the toml config and
    !! close the file.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! if it is true then we will print the toml config
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     WriteData@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldWriteData
  MODULE SUBROUTINE obj_WriteData_hdf5(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_WriteData_hdf5
END INTERFACE AbstractFieldWriteData

!----------------------------------------------------------------------------
!                                                      WriteData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data in vrkfile

INTERFACE AbstractFieldWriteData
  MODULE SUBROUTINE obj_WriteData_vtk(obj, vtk)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
  END SUBROUTINE obj_WriteData_vtk
END INTERFACE AbstractFieldWriteData

!----------------------------------------------------------------------------
!                                                       SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-25
! summary:  Set field variables of abstract field

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, isInitiated, fieldType, name, &
         engine, comm, myRank, numProcs, global_n, local_n, is, ie, lis_ptr, &
                          tSize, realVec, dof, isPMatInitiated, fedof, fedofs)
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
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-25
! summary:  Get the field variables

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, isInitiated, fieldType, name, &
         engine, comm, myRank, numProcs, global_n, local_n, is, ie, lis_ptr, &
                          tSize, realVec, dof, isPMatInitiated, fedof, fedofs)
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
    CLASS(FEDOF_), OPTIONAL, POINTER, INTENT(OUT) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(OUT) :: fedofs(:)
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalPhysicalVars@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-03
! summary:  Returns the total number of physical variables

INTERFACE
  MODULE FUNCTION obj_GetTotalPhysicalVars(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalPhysicalVars
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetPhysicalNames@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns the names of physical variables

INTERFACE
  MODULE SUBROUTINE obj_GetPhysicalNames(obj, ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(INOUT) :: ans(:)
  END SUBROUTINE obj_GetPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetSpaceCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns space components

INTERFACE
  MODULE FUNCTION obj_GetSpaceCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
      !! Total number of physical variables
      !! This can be obtained from GetTotalPhysicalVars method
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetSpaceCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTimeCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns Time components

INTERFACE
  MODULE FUNCTION obj_GetTimeCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTimeCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetStorageFMT@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns storage format

INTERFACE
  MODULE FUNCTION obj_GetStorageFMT(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetStorageFMT
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of degree of freedoms
!
!# Introduction
! This method is same as calling the size function.

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of vertex degree of freedoms

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Edge degree of freedoms

INTERFACE
  MODULE FUNCTION obj_GetTotalEdgeDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalEdgeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Face degree of freedoms

INTERFACE
  MODULE FUNCTION obj_GetTotalFaceDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalFaceDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalCellDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Cell degree of freedoms

INTERFACE
  MODULE FUNCTION obj_GetTotalCellDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      isConstant@GetMethods
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
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

END MODULE AbstractField_Class
