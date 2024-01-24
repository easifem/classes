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
TYPE(IntTreeData_), POINTER :: value_ptr
INTEGER(I4B) :: tsize

CALL obj%Initiate()

value_ptr => IntTreeData_Pointer(15)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(6)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(18)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(17)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(20)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(3)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(7)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(2)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(4)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(13)
CALL obj%Insert(value_ptr)

value_ptr => IntTreeData_Pointer(9)
CALL obj%Insert(value_ptr)

CALL obj%Display("Tree")

END PROGRAM main
