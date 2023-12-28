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
USE GlobalData
USE BaSetype
USE String_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE VectorField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "VectorFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "VectorField"
PUBLIC :: VectorFieldLis_
PUBLIC :: TypeVectorFieldLis
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

TYPE(VectorFieldLis_), PARAMETER :: TypeVectorFieldLis =  &
  & VectorFieldLis_(domains=NULL())

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

FUNCTION VectorFieldLis(param, dom) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(Domain_), TARGET, INTENT(IN) :: dom
  TYPE(VectorFieldLis_) :: ans
  CALL obj%Initiate(dom)
END FUNCTION VectorFieldLis

FUNCTION VectorFieldLis_Pointer(param, dom) RESULT(Ans)
  TYPE(ParameterList_), INTENT(IN) :: param
  TYPE(Domain_), TARGET, INTENT(IN) :: dom
  CLASS(VectorFieldLis_), POINTER :: ans

  ALLOCATE (ans)
  CALL ans%initiate(param, dom)
END FUNCTION VectorFieldLis_Pointer

END MODULE VectorFieldLis_Class
