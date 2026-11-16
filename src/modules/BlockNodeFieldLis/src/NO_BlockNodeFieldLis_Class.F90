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

#ifndef USE_LIS
MODULE BlockNodeFieldLis_Class
USE BlockNodeField_Class, ONLY: BlockNodeField_
IMPLICIT NONE
PRIVATE

PUBLIC :: BlockNodeFieldLis_
PUBLIC :: BlockNodeFieldLisPointer_

TYPE, EXTENDS(BlockNodeField_) :: BlockNodeFieldLis_
END TYPE BlockNodeFieldLis_

TYPE :: BlockNodeFieldLisPointer_
  CLASS(BlockNodeFieldLis_), POINTER :: ptr => NULL()
END TYPE BlockNodeFieldLisPointer_

END MODULE BlockNodeFieldLis_Class
#endif
