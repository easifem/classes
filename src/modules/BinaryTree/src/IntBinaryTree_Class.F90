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

#define  Binary_Tree_Type_Name IntBinaryTree_
MODULE IntBinaryTree_Class
USE IntTreeData_Class, ONLY: TreeData_ => IntTreeData_,  &
  & TreeData_Deallocate => IntTreeData_Deallocate,  &
  & TreeData_Display => IntTreeData_Display,  &
  & TreeData_lt => IntTreeData_lt,  &
  & TreeData_eq => IntTreeData_eq

#include "./BinaryTree.inc"
END MODULE IntBinaryTree_Class
