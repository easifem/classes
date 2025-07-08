! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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
! date: 2023-03-25
! summary: STVector field data type is defined

MODULE STVectorFieldLis_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE STVectorField_Class, ONLY: STVectorField_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "STVectorFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "STVectorField"

PUBLIC :: STVectorFieldLis_
PUBLIC :: STVectorFieldLisPointer_
PUBLIC :: STVectorFieldLis
PUBLIC :: STVectorFieldLis_Pointer
PUBLIC :: STVectorFieldLisInitiate1

!----------------------------------------------------------------------------
!                                                          STVectorFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: STVector field
!
!{!pages/docs-api/STVectorFieldLis/STVectorFieldLis_.md!}

TYPE, EXTENDS(STVectorField_) :: STVectorFieldLis_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export

  ! SET:
  ! @SetMethods

  PROCEDURE, PASS(obj) :: Set16 => obj_Set16

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => &
    obj_GetPointer
  !! Get the entries of STVector field

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Get prefix

END TYPE STVectorFieldLis_

!---------------------------------------------------------------------------
!                                                   STVectorFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: STVectorFieldLisPointer_
  CLASS(STVectorFieldLis_), POINTER :: ptr => NULL()
END TYPE STVectorFieldLisPointer_

!----------------------------------------------------------------------------
!                                                      STVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-26
! summary: This function returns an instance of [[STVectorFieldLis_]]

INTERFACE STVectorFieldLis
  MODULE FUNCTION obj_Constructor1(param, fedof, timefedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(STVectorFieldLis_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE STVectorFieldLis

!----------------------------------------------------------------------------
!                                       STVectorFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-26
! summary:         This function returns an instance of [[STVectorFieldLis_]]

INTERFACE STVectorFieldLis_Pointer
  MODULE FUNCTION obj_Constructor_1(param, fedof, timefedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    CLASS(STVectorFieldLis_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE STVectorFieldLis_Pointer

!----------------------------------------------------------------------------
!                                                    Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: Returns the size

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-26
! summary: This subroutine initiates the space-time vector field
!
!# Introduction
!
! This routine initiate the space-time vector field object.
! `param` contains the information of parameters required to initiate the
! this field.
! There are essential and optional information.
! Essential information are described below.
! - `name`  character defining the name of STvector field
! - `spaceCompo` is the total degree of freedom or components
! - `timeCompo` is the total degree of freedom or components
! - `fieldType` type of field type; FIELD_TYPE_CONSTANT, FIELD_TYPE_NORMAL

INTERFACE STVectorFieldLisInitiate1
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof, timefedof)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
  END SUBROUTINE obj_Initiate1
END INTERFACE STVectorFieldLisInitiate1

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Deallocates data stored inside the STVectorFieldLis_ obj

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STVectorFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STVectorFieldLis_]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
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
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

INTERFACE
  MODULE SUBROUTINE obj_Set16(obj, ivar, idof, VALUE, ivar_value, &
                              idof_value, scale, addContribution)
    CLASS(STVectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable in obj
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom in obj
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    !! obj = value
    INTEGER(I4B), INTENT(IN) :: ivar_value
    !! physical variable in value
    INTEGER(I4B), INTENT(IN) :: idof_value
    !! local degree of freedom in value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set16
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine cannot be called

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(STVectorFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(STVectorFieldLis_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STVectorFieldLis_Class
