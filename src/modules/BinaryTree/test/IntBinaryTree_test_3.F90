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
TYPE(IntBinaryTree_), POINTER :: anode, bnode

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[15, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[6, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[18, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[17, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[20, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[3, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[7, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[2, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[4, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[13, 0])
CALL obj%Insert(VALUE)

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[9, 0])
CALL obj%Insert(VALUE)

anode => obj%GetMaxPointer()
CALL anode%Display("max")

ALLOCATE (VALUE)
CALL VALUE%Initiate(VALUE=[13, 0])
anode => obj%GetNodePointer(VALUE)

bnode => anode%GetSuccessorPointer()
CALL bnode%Display("succ")

! CALL obj%Display("btree: ")

END PROGRAM main
