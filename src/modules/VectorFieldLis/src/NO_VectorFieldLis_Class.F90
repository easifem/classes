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
! summary: Vector field data type is defined

MODULE VectorFieldLis_Class
USE GlobalData, ONLY: DFP, LGT, I4B
USE VectorField_Class, ONLY: VectorField_
USE FPL, ONLY: ParameterList_
USE FEDOF_Class, ONLY: FEDOF_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "VectorFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "VectorField"

PUBLIC :: VectorFieldLis_
PUBLIC :: VectorFieldLisPointer_
PUBLIC :: VectorFieldLis
PUBLIC :: VectorFieldLis_Pointer

!----------------------------------------------------------------------------
!                                                            VectorFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Vector field
!
!{!pages/docs-api/VectorFieldLis/VectorFieldLis_.md!}

TYPE, EXTENDS(VectorField_) :: VectorFieldLis_
CONTAINS
  PRIVATE
  FINAL :: obj_Final
END TYPE VectorFieldLis_

!----------------------------------------------------------------------------
!                                                     VectorFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: VectorFieldLisPointer_
  CLASS(VectorFieldLis_), POINTER :: ptr => NULL()
END TYPE VectorFieldLisPointer_

CONTAINS

SUBROUTINE obj_Final(obj)
  TYPE(VectorFieldLis_), INTENT(INOUT) :: obj
  CALL obj%DEALLOCATE()
END SUBROUTINE obj_Final

FUNCTION VectorFieldLis(param, fedof) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
  TYPE(VectorFieldLis_) :: ans
  CALL ans%Initiate(param=param, fedof=fedof)
END FUNCTION VectorFieldLis

FUNCTION VectorFieldLis_Pointer(param, fedof) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
  CLASS(VectorFieldLis_), POINTER :: ans

  ALLOCATE (ans)
  CALL ans%initiate(param=param, fedof=fedof)
END FUNCTION VectorFieldLis_Pointer

END MODULE VectorFieldLis_Class
