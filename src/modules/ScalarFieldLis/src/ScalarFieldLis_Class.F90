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
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "ScalarFieldLis_Class"
CHARACTER(*), PARAMETER :: myPrefix = "ScalarField"

PUBLIC :: ScalarFieldLis_
PUBLIC :: ScalarFieldLisPointer_
PUBLIC :: ScalarFieldLis
PUBLIC :: ScalarFieldLis_Pointer

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
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle

  PROCEDURE, PUBLIC, PASS(obj) :: SetAll => obj_SetAll
  !! Set all nodal values to a scalar constant values

  PROCEDURE, PASS(obj) :: SetMultiple1 => obj_SetMultiple1
    !! Set all values to a scalar values

    !! This method is used for assignment operator
  PROCEDURE, PASS(obj) :: Set9 => obj_Set9
    !! obj(ivar, idof) = obj(ivar, idof) + scalar*obj2(ivar, idof)

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! This method is not avaiable in ScalarFieldList

  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
  !! Get single entry

  PROCEDURE, PASS(obj) :: Get1 => obj_Get1
  !! Get single entry
  PROCEDURE, PASS(obj) :: Get2 => obj_Get2
  !! Get all values in Real vector
  PROCEDURE, PASS(obj) :: Get3 => obj_Get3
  !! Get selected values
  PROCEDURE, PASS(obj) :: Get5 => obj_Get5
    !! Get selected values in FEVariable

END TYPE ScalarFieldLis_

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldLisPointer_
  CLASS(ScalarFieldLis_), POINTER :: ptr => NULL()
END TYPE ScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

INTERFACE ScalarFieldLis
  MODULE FUNCTION obj_Constructor1(param, fedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    TYPE(ScalarFieldLis_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE ScalarFieldLis

!----------------------------------------------------------------------------
!                                         ScalarFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

INTERFACE ScalarFieldLis_Pointer
  MODULE FUNCTION obj_Constructor_1(param, fedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(ScalarFieldLis_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE ScalarFieldLis_Pointer

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

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

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Size@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-22
! summary: This subroutine initiates the ScalarFieldLis_ object

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSingle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-28
! summary: Set a single entry in the Scalar field

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, &
                                  addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    !! index to be set
    REAL(DFP), INTENT(IN) :: VALUE
    !! a scalar value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE obj_SetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-28
! summary: Set all the values of the Scalar field to a scalar value

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be set
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-28
! summary: Set multiple entries in the Scalar field

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple1(obj, indx, VALUE, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! indices to be set
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Set

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, ivar, idof, VALUE, ivar_value, &
                             idof_value, scale, addContribution)
    CLASS(ScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of obj
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom of physical variable ivar
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    !! right hand side in obj = value
    INTEGER(I4B), INTENT(IN) :: ivar_value
    !! physical variable of value
    INTEGER(I4B), INTENT(IN) :: idof_value
    !! local degree of freedom of physical variable ivar_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set9
END INTERFACE

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
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    !! index
    REAL(DFP), INTENT(OUT) :: VALUE
    !! scalar value
  END SUBROUTINE obj_GetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, islocal)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! single returned entry
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then the global nodes are local nodes
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns all the entries by using given scalar field

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, tsize)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode, islocal, tsize)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE)
    CLASS(ScalarFieldLis_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarFieldLis_Class
