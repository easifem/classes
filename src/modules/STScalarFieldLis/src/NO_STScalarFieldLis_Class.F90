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
! date: 2024-05-31
! summary: STScalar field data type is defined

MODULE STScalarFieldLis_Class
USE STScalarField_Class, ONLY: STScalarField_
USE FPL, ONLY: ParameterList_
USE FEDOF_Class, ONLY: FEDOF_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "STScalarFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "STScalarField"

PUBLIC :: STScalarFieldLis_
PUBLIC :: STScalarFieldLisPointer_
PUBLIC :: STScalarFieldLis
PUBLIC :: STScalarFieldLis_Pointer

!----------------------------------------------------------------------------
!                                                         STScalarFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-23
! summary: STScalar field for LIS_OMP engine
!
!{!pages/docs-api/STScalarFieldLis/STScalarFieldLis_.md}

TYPE, EXTENDS(STScalarField_) :: STScalarFieldLis_
END TYPE STScalarFieldLis_

!----------------------------------------------------------------------------
!                                                   STScalarFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldLisPointer_
  CLASS(STScalarFieldLis_), POINTER :: ptr => NULL()
END TYPE STScalarFieldLisPointer_

CONTAINS

FUNCTION STScalarFieldLis(param, fedof) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(FEDOF_), TARGET, INTENT(IN) :: fedof
  TYPE(STScalarFieldLis_) :: ans
  CALL ans%Initiate(param=param, fedof=fedof)
END FUNCTION STScalarFieldLis

FUNCTION STScalarFieldLis_Pointer(param, fedof) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(FEDOF_), TARGET, INTENT(IN) :: fedof
  CLASS(STScalarFieldLis_), POINTER :: ans
  ALLOCATE (ans)
  CALL ans%initiate(param=param, fedof=fedof)
END FUNCTION STScalarFieldLis_Pointer

END MODULE STScalarFieldLis_Class
