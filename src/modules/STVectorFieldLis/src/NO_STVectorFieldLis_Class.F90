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
! date: 2023-03-25
! summary: STVector field data type is defined

MODULE STVectorFieldLis_Class
USE GlobalData
USE Basetype
USE AbstractField_Class
USE AbstractNodeField_Class
USE STVectorField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "STVectorFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "STVectorField"
PUBLIC :: STVectorFieldLis_
PUBLIC :: TypeSTVectorFieldLis
PUBLIC :: STVectorFieldLisPointer_
PUBLIC :: STVectorFieldLis
PUBLIC :: STVectorFieldLis_Pointer

!----------------------------------------------------------------------------
!                                                          STVectorFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: STVector field
!
!{!pages/docs-api/STVectorFieldLis/STVectorFieldLis_.md!}

TYPE, EXTENDS(STVectorField_) :: STVectorFieldLis_
END TYPE STVectorFieldLis_

TYPE(STVectorFieldLis_), PARAMETER :: TypeSTVectorFieldLis =  &
  & STVectorFieldLis_(domains=NULL())

TYPE :: STVectorFieldLisPointer_
  CLASS(STVectorFieldLis_), POINTER :: ptr => NULL()
END TYPE STVectorFieldLisPointer_

CONTAINS

FUNCTION STVectorFieldLis(param, dom) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(Domain_), TARGET, INTENT(IN) :: dom
  TYPE(STVectorFieldLis_) :: ans
  CALL ans%Initiate(param, dom)
END FUNCTION

FUNCTION STVectorFieldLis_Pointer(param, dom) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(Domain_), TARGET, INTENT(IN) :: dom
  CLASS(STVectorFieldLis_), POINTER :: ans
  ALLOCATE (ans)
  CALL ans%initiate(param, dom)
END FUNCTION STVectorFieldLis_Pointer

END MODULE STVectorFieldLis_Class
