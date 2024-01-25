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
!

PROGRAM main
USE easifemBase
USE easifemClasses
USE IntTreeData_Class
USE IntBinaryTree_Class

TYPE(IntBinaryTree_) :: obj
TYPE(IntTreeData_), POINTER :: VALUE
INTEGER(I4B) :: tsize

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[1, 2])
CALL obj%Insert(VALUE)

tsize = obj%SIZE()
CALL OK(tsize .EQ. 1, "size: ")

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[1, 7])
CALL obj%Insert(VALUE)
tsize = obj%SIZE()
CALL OK(tsize .EQ. 2, "size: ")

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[2, 3])
CALL obj%Insert(VALUE)
tsize = obj%SIZE()
CALL OK(tsize .EQ. 3, "size: ")

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[2, 8])
CALL obj%Insert(VALUE)
tsize = obj%SIZE()
CALL OK(tsize .EQ. 4, "size: ")

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[2, 8])
CALL obj%Insert(VALUE)
tsize = obj%SIZE()
CALL OK(tsize .EQ. 4, "size: ")
CALL Display(tsize, "tsize: ")

CALL obj%Display("btree: ")

END PROGRAM main
