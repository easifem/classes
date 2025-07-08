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
! date: 28 June 2021
! summary: Scalar field data type is defined

MODULE BlockNodeFieldLis_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "BlockNodeFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "BlockNodeField"

PUBLIC :: BlockNodeFieldLis_
PUBLIC :: BlockNodeFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         BlockNodeFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This nodal field is designed for the multiphysics applications
!
!{!pages/docs-api/BlockNodeField/BlockNodeFieldLis_.md!}

TYPE, EXTENDS(BlockNodeField_) :: BlockNodeFieldLis_

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! SET:
  ! @SetMethods

  PROCEDURE, PASS(obj) :: Set18 => obj_Set18

  ! GET:
  ! @GetMethods

END TYPE BlockNodeFieldLis_

!----------------------------------------------------------------------------
!                                                  BlockNodeFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: BlockNodeFieldLisPointer_
  CLASS(BlockNodeFieldLis_), POINTER :: ptr => NULL()
END TYPE BlockNodeFieldLisPointer_

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This subroutine initiates the BlockNodeFieldLis_ object
!
!# Introduction
!
! This routine initiate the [[BlockNodeFieldLis_]] object.
! `param` contains the information of parameters required to initiate the
! instance of BlockNodeFieldLis_ .
!
! - It is better to make `param` by calling
! [[BlockNodeFieldLis_::SetBlockNodeFieldParam]]
! - The size of `dom` should be equal to the number of physical variables
! present in the block node field.
! - `dom` contains the pointer to [[Domain_]] class.

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, param, fedof, timefedof)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(FEDOFPointer_), INTENT(IN) :: fedof(:)
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedof(:)
  END SUBROUTINE obj_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockNodeFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: Returns the size

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-08
! summary: Set the values

INTERFACE
  MODULE SUBROUTINE obj_Set18(obj, ivar, idof, VALUE, ivar_value, &
                              idof_value, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    !! AbstractNodeField_
    INTEGER(I4B), INTENT(IN) :: ivar_value
    !! physical variable number in value
    INTEGER(I4B), INTENT(IN) :: idof_value
    !! local degree of freedom number in value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set18
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockNodeFieldLis_Class
