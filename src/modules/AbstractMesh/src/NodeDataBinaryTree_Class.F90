! This program is a part of EASIFEM library
! Copyright (C) Vikas Sharma, Ph.D
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

#define  Binary_Tree_Type_Name NodeDataBinaryTree_
#define Binary_Tree_Activate_SetID_Method
MODULE NodeDataBinaryTree_Class
USE NodeData_Class, ONLY: TreeData_ => NodeData_,  &
  & TreeData_Deallocate => NodeData_Deallocate,  &
  & TreeData_Display => NodeData_Display,  &
  & TreeData_lt => NodeData_lt,  &
  & TreeData_eq => NodeData_eq,  &
  & TreeData_SetID => NodeData_SetID

#include "../../BinaryTree/src/BinaryTree.inc"

END MODULE NodeDataBinaryTree_Class

#undef Binary_Tree_Type_Name
#undef Binary_Tree_Activate_SetID_Method
