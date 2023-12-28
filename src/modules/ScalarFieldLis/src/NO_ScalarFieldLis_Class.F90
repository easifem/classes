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
USE GlobalData
USE String_Class
USE BaSetype
USE AbstractField_Class
USE AbstractNodeField_Class
USE ScalarField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
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
!
!----------------------------------------------------------------------------

TYPE(ScalarFieldLis_), PARAMETER, PUBLIC :: TypeScalarFieldLis = &
  & ScalarFieldLis_(domains=NULL())

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

FUNCTION ScalarFieldLis(param, dom) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(Domain_), TARGET, INTENT(IN) :: dom
  TYPE(ScalarFieldLis_) :: ans
  CALL ans%initiate(param, dom)
END FUNCTION obj_Constructor1

!----------------------------------------------------------------------------
!                                         ScalarFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[ScalarFieldLis_]]

FUNCTION ScalarFieldLis_Pointer(param, dom) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(Domain_), TARGET, INTENT(IN) :: dom
  CLASS(ScalarFieldLis_), POINTER :: ans
  CALL ans%initiate(param, dom)
END FUNCTION ScalarFieldLis_Pointer

END MODULE ScalarFieldLis_Class
