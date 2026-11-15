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

!> author: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Stub of VectorFieldLis_ for builds without LIS

#ifndef USE_LIS
MODULE VectorFieldLis_Class
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE
PRIVATE

PUBLIC :: VectorFieldLis_
PUBLIC :: VectorFieldLisPointer_

TYPE, EXTENDS(VectorField_) :: VectorFieldLis_
END TYPE VectorFieldLis_

TYPE :: VectorFieldLisPointer_
  CLASS(VectorFieldLis_), POINTER :: ptr => NULL()
END TYPE VectorFieldLisPointer_

END MODULE VectorFieldLis_Class
#endif
