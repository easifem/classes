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
! summary: Tensor field data type is defined

MODULE TensorField_Class
USE GlobalData
USE BaseType
USE AbstractNodeField_Class
USE ScalarField_Class, ONLY: ScalarField_
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "TensorField_Class"
TYPE(ExceptionHandler_) :: e

!----------------------------------------------------------------------------
!                                                              TensorField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Tensor field
!
!{!pages/TensorField.md}

TYPE, EXTENDS(AbstractNodeField_) :: TensorField_
  INTEGER(I4B) :: m = 0_I4B
  INTEGER(I4B) :: n = 0_I4B
END TYPE TensorField_

PUBLIC :: TensorField_
TYPE(TensorField_), PARAMETER, PUBLIC :: TYPE = TensorField_()

!----------------------------------------------------------------------------
!                                                       TensorFieldPointer_
!----------------------------------------------------------------------------

TYPE :: TensorFieldPointer_
  CLASS(TensorField_), POINTER :: ptr => NULL()
END TYPE TensorFieldPointer_

PUBLIC :: TensorFieldPointer_

END MODULE TensorField_Class
