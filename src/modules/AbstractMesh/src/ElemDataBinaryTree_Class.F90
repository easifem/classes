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

#define  Binary_Tree_Type_Name ElemDataBinaryTree_
#define Binary_Tree_Activate_SetID_Method

MODULE ElemDataBinaryTree_Class
USE ElemData_Class, ONLY: TreeData_ => ElemData_,  &
  & TreeData_Deallocate => ElemData_Deallocate,  &
  & TreeData_Display => ElemData_Display,  &
  & TreeData_lt => ElemData_lt,  &
  & TreeData_eq => ElemData_eq,  &
  & TreeData_SetID => ElemData_SetID

#include "../../BinaryTree/src/BinaryTree.inc"

END MODULE ElemDataBinaryTree_Class

#undef Binary_Tree_Type_Name
#undef Binary_Tree_Activate_SetID_Method
