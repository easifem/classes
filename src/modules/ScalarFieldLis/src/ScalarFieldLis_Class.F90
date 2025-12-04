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

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! This method is not avaiable in ScalarFieldList
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Get the size of the object
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

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(ScalarFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Size@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarFieldLis_Class
