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
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE VTKFile_Class, ONLY: VTKFile_
USE ExceptionHandler_Class, ONLY: e
USE Domain_Class
IMPLICIT NONE
PRIVATE
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_NORMAL = 1
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT = 2
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT_SPACE = 3
INTEGER(I4B), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT_TIME = 4
CHARACTER(*), PARAMETER :: modName = "AbstractField_Class"
PUBLIC :: AbstractFieldInitiate
PUBLIC :: AbstractFieldDisplay
PUBLIC :: AbstractFieldImport
PUBLIC :: AbstractFieldExport
PUBLIC :: AbstractFieldDeallocate
PUBLIC :: FIELD_TYPE_NUMBER
PUBLIC :: SetAbstractFieldParam
PUBLIC :: AbstractFieldCheckEssentialParam
PUBLIC :: AbstractField_
PUBLIC :: AbstractFieldInitiate2
PUBLIC :: FIELD_TYPE_NAME

!----------------------------------------------------------------------------
!                                                           AbstractField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Jul 2021
! summary: Abstract field is designed to handle fields in FEM

TYPE, ABSTRACT :: AbstractField_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is true if the object is initiated
  INTEGER(I4B) :: fieldType = FIELD_TYPE_NORMAL
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
  TYPE(Domain_), POINTER :: domain => NULL()
  !! Domain contains the information of the finite element meshes.
  TYPE(DomainPointer_), ALLOCATABLE :: domains(:)
  !! Domain for each physical variables
  !! The size of `domains` should be equal to the total number of
  !! physical variables.
  !! It is used in the case of BlockNodeField
  !! and BlockMatrixField
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE(aField_CheckEssentialParam), DEFERRED, PUBLIC, PASS(obj) :: &
    & CheckEssentialParam
  !! Check essential parameters
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => aField_Initiate1
  !! Initiate the field by reading param and given domain
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => aField_Initiate2
  !! Initiate by copying other fields, and different options
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => aField_Initiate3
  !! Initiate  block fields (different physical variables) defined
  !! over different order of meshes.
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3
  GENERIC, PUBLIC :: Copy => Initiate2
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => aField_Deallocate
  !! Deallocate the field

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => aField_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => aField_Import
  !! Import data from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => aField_Export
  !! Export data in hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk => afield_WriteData_vtk
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_hdf5 => afield_WriteData_hdf5
  GENERIC, PUBLIC :: WriteData => WriteData_vtk, WriteData_hdf5

  ! GET:
  ! @GetMethods
  PROCEDURE, PASS(obj), NON_OVERRIDABLE, PUBLIC :: GetParam
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalPhysicalVars  &
    & => aField_GetTotalPhysicalVars
  !! Returns the total number of physical variables
  !! INFO: This routine should be implemented by child classes
  !! For block matrices the physical variables are more than one,
  !! for example, presesure and velocity.
  PROCEDURE, PUBLIC, PASS(obj) :: GetPhysicalNames => aField_GetPhysicalNames
  !! Returns the names of physical variables
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetSpaceCompo => aField_GetSpaceCompo
  !! Return space component
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeCompo => aField_GetTimeCompo
  !! Return time component
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetStorageFMT => aField_GetStorageFMT
  !! Return storage format
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalDOF => aField_GetTotalDOF
  !! Returns the total number of degree of freedoms
  !! This is same as calling Size
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalVertexDOF =>  &
   & aField_GetTotalVertexDOF
  !! Returns the total number of vertex degree of freedoms
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEdgeDOF => aField_GetTotalEdgeDOF
  !! Returns the total number of edge degree of freedoms
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFaceDOF => aField_GetTotalFaceDOF
  !! Returns the total number of face degree of freedoms
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCellDOF => aField_GetTotalCellDOF
  !! Returns the total number of cell degree of freedoms
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj), NON_OVERRIDABLE :: isConstant  &
    & => aField_isConstant
  !! It returns true if the field is constant field
  !! INFO: This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => aField_GetPrefix

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj), NON_OVERRIDABLE, PUBLIC :: SetParam

END TYPE AbstractField_

!----------------------------------------------------------------------------
!                                   CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Check the essential parameters in param.

ABSTRACT INTERFACE
  SUBROUTINE aField_CheckEssentialParam(obj, param)
    IMPORT :: AbstractField_, ParameterList_
    CLASS(AbstractField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE aField_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                  SetAbstractFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-04
! summary:  Set AbstractField_ parameters

INTERFACE
  MODULE SUBROUTINE SetAbstractFieldParam(param, prefix, name, engine,  &
    & fieldType, comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    !! prefix
    CHARACTER(*), INTENT(IN) :: name
    !! name of the variable
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of scalar field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of scalar field on distributed on processors
    !! Only needed for parallel environment
  END SUBROUTINE SetAbstractFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

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

INTERFACE
  MODULE SUBROUTINE aField_Initiate1(obj, param, dom)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE aField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate by copying other fields, and different options

INTERFACE AbstractFieldInitiate2
  MODULE SUBROUTINE aField_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE aField_Initiate2
END INTERFACE AbstractFieldInitiate2

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate by reading options from [[ParameterList_]]

INTERFACE
  MODULE SUBROUTINE aField_Initiate3(obj, param, dom)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE aField_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary: Initiate the field by reading param and given domain

INTERFACE AbstractFieldInitiate
  MODULE SUBROUTINE AbstractFieldInitiate_1(obj, param, dom, prefix)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE AbstractFieldInitiate_1
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary: Initiate the field by reading param and given domain

INTERFACE AbstractFieldInitiate
  MODULE SUBROUTINE AbstractFieldInitiate_2(obj, param, dom, prefix)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE AbstractFieldInitiate_2
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldDeallocate
  MODULE SUBROUTINE aField_Deallocate(obj)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
  END SUBROUTINE aField_Deallocate
END INTERFACE AbstractFieldDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION FIELD_TYPE_NUMBER(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION FIELD_TYPE_NUMBER
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION FIELD_TYPE_NAME(id) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: id
    CHARACTER(20) :: ans
  END FUNCTION FIELD_TYPE_NAME
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldDisplay
  MODULE SUBROUTINE aField_Display(obj, msg, unitNo)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE aField_Display
END INTERFACE AbstractFieldDisplay

!----------------------------------------------------------------------------
!                                                         IMPORT@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldImport
  MODULE SUBROUTINE aField_Import(obj, hdf5, group, dom, domains)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE aField_Import
END INTERFACE AbstractFieldImport

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldExport
  MODULE SUBROUTINE aField_Export(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE aField_Export
END INTERFACE AbstractFieldExport

!----------------------------------------------------------------------------
!                                                     WriteData@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractFieldWriteData
  MODULE SUBROUTINE aField_WriteData_hdf5(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE aField_WriteData_hdf5
END INTERFACE AbstractFieldWriteData

!----------------------------------------------------------------------------
!                                                      WriteData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data in vrkfile

INTERFACE AbstractFieldWriteData
  MODULE SUBROUTINE aField_WriteData_vtk(obj, vtk)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
  END SUBROUTINE aField_WriteData_vtk
END INTERFACE AbstractFieldWriteData

!----------------------------------------------------------------------------
!                                                       SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-25
! summary:  Set field variables of abstract field

INTERFACE
  MODULE SUBROUTINE SetParam(obj, &
    & isInitiated, &
    & fieldType, &
    & name, &
    & engine, &
    & comm, &
    & myRank, &
    & numProcs, &
    & global_n, &
    & local_n, &
    & is, &
    & ie,  &
    & lis_ptr, &
    & domain, &
    & domains, &
    & tSize, &
    & realVec, &
    & dof, &
    & isPMatInitiated)
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
    TYPE(Domain_), OPTIONAL, TARGET, INTENT(IN) :: domain
    TYPE(DomainPointer_), OPTIONAL, INTENT(IN) :: domains(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSize
    TYPE(RealVector_), OPTIONAL, INTENT(IN) :: realVec
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: dof
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPMatInitiated
  END SUBROUTINE SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-25
! summary:  Get the field variables

INTERFACE
  MODULE SUBROUTINE GetParam(obj, &
    & isInitiated, &
    & fieldType, &
    & name, &
    & engine, &
    & comm, &
    & myRank, &
    & numProcs, &
    & global_n, &
    & local_n, &
    & is, &
    & ie,  &
    & lis_ptr, &
    & domain, &
    & domains, &
    & tSize, &
    & realVec, &
    & dof, &
    & isPMatInitiated)
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
    TYPE(Domain_), OPTIONAL, POINTER, INTENT(OUT) :: domain
    TYPE(DomainPointer_), OPTIONAL, INTENT(OUT) :: domains(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tSize
    TYPE(RealVector_), OPTIONAL, INTENT(OUT) :: realVec
    TYPE(DOF_), OPTIONAL, INTENT(OUT) :: dof
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isPMatInitiated
  END SUBROUTINE GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalPhysicalVars@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-03
! summary:  Returns the total number of physical variables

INTERFACE
  MODULE FUNCTION aField_GetTotalPhysicalVars(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION aField_GetTotalPhysicalVars
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetPhysicalNames@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns the names of physical variables

INTERFACE
  MODULE SUBROUTINE aField_GetPhysicalNames(obj, ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(INOUT) :: ans(:)
  END SUBROUTINE aField_GetPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetSpaceCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns space components

INTERFACE
  MODULE FUNCTION aField_GetSpaceCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
      !! Total number of physical variables
      !! This can be obtained from GetTotalPhysicalVars method
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION aField_GetSpaceCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTimeCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns Time components

INTERFACE
  MODULE FUNCTION aField_GetTimeCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION aField_GetTimeCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetStorageFMT@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns storage format

INTERFACE
  MODULE FUNCTION aField_GetStorageFMT(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION aField_GetStorageFMT
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
  MODULE FUNCTION aField_GetTotalDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION aField_GetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of vertex degree of freedoms

INTERFACE
  MODULE FUNCTION aField_GetTotalVertexDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION aField_GetTotalVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Edge degree of freedoms

INTERFACE
  MODULE FUNCTION aField_GetTotalEdgeDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION aField_GetTotalEdgeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Face degree of freedoms

INTERFACE
  MODULE FUNCTION aField_GetTotalFaceDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION aField_GetTotalFaceDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalCellDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Cell degree of freedoms

INTERFACE
  MODULE FUNCTION aField_GetTotalCellDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION aField_GetTotalCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      isConstant@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns true if the field is constant
INTERFACE
  MODULE FUNCTION aField_IsConstant(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aField_IsConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION aField_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION aField_GetPrefix
END INTERFACE

END MODULE AbstractField_Class
