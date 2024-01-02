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
USE GlobalData
USE Basetype
USE String_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE BlockNodeField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "BlockNodeFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "BlockNodeField"
PUBLIC :: BlockNodeFieldLis_
PUBLIC :: TypeBlockNodeFieldLis
PUBLIC :: BlockNodeFieldPointer_

!----------------------------------------------------------------------------
!                                                         BlockNodeFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: This nodal field is designed for the multiphysics applications
!
!{!pages/docs-api/BlockNodeFieldLis/BlockNodeFieldLis_.md!}

TYPE, EXTENDS(BlockNodeField_) :: BlockNodeFieldLis_
CONTAINS
  PRIVATE
  FINAL :: obj_Final
END TYPE BlockNodeFieldLis_

TYPE(BlockNodeFieldLis_), PARAMETER :: TypeBlockNodeFieldLis = &
& BlockNodeFieldLis_(domains=NULL())

!----------------------------------------------------------------------------
!                                                  BlockNodeFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: BlockNodeFieldLisPointer_
  CLASS(BlockNodeFieldLis_), POINTER :: ptr => NULL()
END TYPE BlockNodeFieldLisPointer_

CONTAINS

SUBROUTINE obj_Final(obj)
  TYPE(BlockNodeFieldLis_), INTENT(INOUT) :: obj
  CALL obj%DEALLOCATE()
END SUBROUTINE obj_Final

END MODULE BlockNodeFieldLis_Class
