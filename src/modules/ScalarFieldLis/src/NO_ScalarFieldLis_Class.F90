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
USE String_Class, ONLY: String
USE BaseType, ONLY: FEVariable_
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
  FINAL :: obj_Final
END TYPE ScalarFieldLis_

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldLisPointer_
  CLASS(ScalarFieldLis_), POINTER :: ptr => NULL()
END TYPE ScalarFieldLisPointer_

CONTAINS

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

SUBROUTINE obj_Final(obj)
  TYPE(ScalarFieldLis_), INTENT(INOUT) :: obj
  CALL obj%DEALLOCATE()
END SUBROUTINE obj_Final

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

FUNCTION ScalarFieldLis(param, fedof) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(FEDOF_), TARGET, INTENT(IN) :: fedof
  TYPE(ScalarFieldLis_) :: ans
  CALL ans%Initiate(param=param, fedof=fedof)
END FUNCTION

!----------------------------------------------------------------------------
!                                         ScalarFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

FUNCTION ScalarFieldLis_Pointer(param, fedof) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(FEDOF_), TARGET, INTENT(IN) :: fedof
  CLASS(ScalarFieldLis_), POINTER :: ans
  ALLOCATE (ScalarFieldLis_ :: ans)
  CALL ans%Initiate(param=param, fedof=fedof)
END FUNCTION

END MODULE ScalarFieldLis_Class
