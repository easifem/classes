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

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-22
! summary: Scalar field data type with LIS engine is defined

MODULE ScalarFieldLis_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ScalarField_Class, ONLY: ScalarField_
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "ScalarFieldLis_Class"

PUBLIC :: ScalarFieldLis_
PUBLIC :: ScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                              ScalarField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: LIS Scalar field
!
!{!pages/docs-api/ScalarFieldLis/ScalarFieldLis_.md!}

TYPE, EXTENDS(ScalarField_) :: ScalarFieldLis_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate an instance of ScalarField_ by passing arguments
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the object
  FINAL :: obj_Final
  !! Finalizer

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the object

  !IO:
  ! @HDF5Methods
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the object
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the object

  ! SET:
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  !! Set single entry
  PROCEDURE, PUBLIC, PASS(obj) :: SetAll => obj_SetAll
  !! Set all the values to a constant scalar value
  PROCEDURE, PASS(obj) :: SetMultiple1 => obj_SetMultiple1
  !! Set multiple entries using indices
  PROCEDURE, PASS(obj) :: SetMultiple2 => obj_SetMultiple2
  !! Set multiple entries using range
  PROCEDURE, PASS(obj) :: SetMultiple3 => obj_SetMultiple3
  !! Set multiple entries using range
  PROCEDURE, PASS(obj) :: SetMultiple4 => obj_SetMultiple4
  !! Set multiple entries using range
  PROCEDURE, PASS(obj) :: Set8 => obj_Set8
  !! obj = obj + scale*obj2

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! This method is not avaiable in ScalarFieldList
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Get the size of the object
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
  !! Get single entry
  PROCEDURE, PASS(obj) :: GetMultiple1 => obj_GetMultiple1
  !! get many values from indices
  PROCEDURE, PASS(obj) :: GetMultiple2 => obj_GetMultiple2
  !! get many values from trides
  PROCEDURE, PASS(obj) :: GetMultiple3 => obj_GetMultiple3
  !! get many values from trides

  ! GET:
  ! @BlasMethods
  PROCEDURE, PASS(obj) :: AXPY1 => obj_AXPY1
  PROCEDURE, PASS(obj) :: AXPY2 => obj_AXPY2
  PROCEDURE, PASS(obj) :: AXPY3 => obj_AXPY3
  !! Y = Y + scale * X ...
  PROCEDURE, PUBLIC, PASS(obj) :: SCAL => obj_SCAL
  !! X = scale * X
  PROCEDURE, PUBLIC, PASS(obj) :: COPY => obj_Copy
  !! Y = X
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => obj_Norm2
  !! Returns the L2 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => obj_Norm1
  !! Returns the L1 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => obj_Normi
  !! Returns the infinity norm
  PROCEDURE, PUBLIC, PASS(obj) :: DOT_PRODUCT => obj_DOT_PRODUCT
  !! dot product
  PROCEDURE, PUBLIC, PASS(obj) :: PMUL => obj_PMUL
  !! z = x * y
  PROCEDURE, PUBLIC, PASS(obj) :: Reciprocal => obj_Reciprocal
  !! y = 1/x
END TYPE ScalarFieldLis_

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldLisPointer_
  CLASS(ScalarFieldLis_), POINTER :: ptr => NULL()
END TYPE ScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-03
! summary: Initiate an instance of ScalarFieldLis_ by arguments

INTERFACE
  MODULE SUBROUTINE obj_Initiate4( &
    obj, name, engine, fieldType, storageFMT, comm, local_n, global_n, &
    spaceCompo, isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo, &
    isTimeCompoScalar, tPhysicalVarNames, physicalVarNames, &
    isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes, &
    isTNodesScalar, tSize, fedof, geofedof, timefedof)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: storageFMT
    !! storage format of the scalar field
    !! Not required.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! space components
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    !! Not required
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! Time components
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    !! Not required
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    !! Not required
    CHARACTER(*), OPTIONAL, INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNamesScalar
    !! if true then physicalVarNames is scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tNodes(:)
    !! total number of nodes in each physical variable
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTNodes
    !! if true we will try to access tNodes
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTNodesScalar
    !! is tNodes scalar
    !! Not required
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSize
    !! total size of node field
    !! not required
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof, geofedof
    !! FEDOF object
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(IN) :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-15
! summary: Display the object

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-15
! summary:  Export the data into hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-15
! summary: Import data from hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof, geofedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetSingle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetAll@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-03
! summary: Set all the values

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set or add
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set multiple entries using indices

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple1( &
    obj, indx, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values which will be use din obj=value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple2( &
    obj, istart, iend, stride, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! obj = value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-06-02
! summary: Get multiple entries using trides

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple3( &
    obj, istart, iend, stride, VALUE, istart_value, iend_value, &
    stride_value, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of values
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple4( &
    obj, istart, iend, stride, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(IN) :: VALUE
    !! obj = value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: obj=obj+scalar*VALUE

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
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
! summary: Get the pointer

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-04
! summary: Get the size of the object

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE obj_GetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get multiple entries using indices

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple1(obj, indx, VALUE, tsize)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total number of data written in value
  END SUBROUTINE obj_GetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get multiple enties using range

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple2(obj, istart, iend, stride, VALUE, tsize)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total number of data written in value
  END SUBROUTINE obj_GetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-06-02
! summary: Get multiple entries using trides

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple3( &
    obj, istart, iend, stride, VALUE, istart_value, iend_value, &
    stride_value, tsize)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of values
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total number of data written in value
  END SUBROUTINE obj_GetMultiple3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-17
! summary:  obj = obj + s * x

INTERFACE
  MODULE SUBROUTINE obj_AXPY1(obj, x, scale)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_AXPY1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-29
! summary:  obj = obj + a1 * x1 + a2 * x2

INTERFACE
  MODULE SUBROUTINE obj_AXPY2(obj, x1, x2, a1, a2)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x1
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x2
    REAL(DFP), INTENT(IN) :: a1
    REAL(DFP), INTENT(IN) :: a2
  END SUBROUTINE obj_AXPY2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-29
! summary:  obj = obj + a1 * x1 + a2 * x2 + a3 * x3

INTERFACE
  MODULE SUBROUTINE obj_AXPY3(obj, x1, x2, x3, a1, a2, a3)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x1
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x2
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x3
    REAL(DFP), INTENT(IN) :: a1
    REAL(DFP), INTENT(IN) :: a2
    REAL(DFP), INTENT(IN) :: a3
  END SUBROUTINE obj_AXPY3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          ACAL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-17
! summary: scaling y = s * y

INTERFACE
  MODULE SUBROUTINE obj_SCAL(obj, scale)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_SCAL
END INTERFACE

!----------------------------------------------------------------------------
!                                                         COPY@BlasMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Copy obj=obj2

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Jan 2022
! summary: This function returns NORM2

INTERFACE
  MODULE FUNCTION obj_Norm2(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm1@BlasMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Jan 2022
! summary: This function returns first norm (absolute some)

INTERFACE
  MODULE FUNCTION obj_Norm1(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Normi@BlasMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Jan 2022
! summary: This function returns infinity norm

INTERFACE
  MODULE FUNCTION obj_Normi(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                    DOT_PRODUCT@BlasMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary:  performs dot product

INTERFACE
  MODULE FUNCTION obj_DOT_PRODUCT(obj, obj2) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    REAL(DFP) :: ans
  END FUNCTION obj_DOT_PRODUCT
END INTERFACE

!----------------------------------------------------------------------------
!                                                           PMUL@BlasMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary:  obj = obj1 * obj2

INTERFACE
  MODULE SUBROUTINE obj_PMUL(obj, obj1, obj2)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: obj1
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
  END SUBROUTINE obj_PMUL
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reciprocal@BlasMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary:  obj = 1.0 / obj

INTERFACE
  MODULE SUBROUTINE obj_Reciprocal(obj)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Reciprocal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarFieldLis_Class
