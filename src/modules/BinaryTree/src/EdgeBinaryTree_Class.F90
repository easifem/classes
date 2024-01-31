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

#define  Binary_Tree_Type_Name EdgeBinaryTree_
#define Binary_Tree_Activate_SetID_Method
MODULE EdgeBinaryTree_Class
USE EdgeTreeData_Class, ONLY: TreeData_ => EdgeTreeData_,  &
  & TreeData_Deallocate => EdgeTreeData_Deallocate,  &
  & TreeData_Display => EdgeTreeData_Display,  &
  & TreeData_lt => EdgeTreeData_lt,  &
  & TreeData_eq => EdgeTreeData_eq,  &
  & TreeData_SetID => EdgeTreeData_SetID

#include "./BinaryTree.inc"
END MODULE EdgeBinaryTree_Class

#undef Binary_Tree_Type_Name
#undef Binary_Tree_Activate_SetID_Method
