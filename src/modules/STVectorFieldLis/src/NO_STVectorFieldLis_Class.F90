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

#ifndef USE_LIS
MODULE STVectorFieldLis_Class
USE STVectorField_Class, ONLY: STVectorField_
IMPLICIT NONE
PRIVATE
PUBLIC :: STVectorFieldLis_
PUBLIC :: STVectorFieldLisPointer_

TYPE, EXTENDS(STVectorField_) :: STVectorFieldLis_
END TYPE STVectorFieldLis_

TYPE :: STVectorFieldLisPointer_
  CLASS(STVectorFieldLis_), POINTER :: ptr => NULL()
END TYPE STVectorFieldLisPointer_

END MODULE STVectorFieldLis_Class
#endif
