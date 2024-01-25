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

MODULE EdgeBinaryTreeUtility
USE GlobalData, ONLY: I4B
USE EdgeBinaryTree_Class
USE EdgeTreeData_Class

IMPLICIT NONE

PRIVATE

PUBLIC :: EdgeBinaryTree_GetArray

CONTAINS

!----------------------------------------------------------------------------
!                                                   EdgeBinaryTree_GetArray
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE EdgeBinaryTree_GetArray(obj, VALUE)
  TYPE(EdgeBinaryTree_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(INOUT) :: VALUE(:, :)
    !! The number of rows in value should be 2
    !! The number of columns in value should be total number of edges

  TYPE(EdgeBinaryTree_) :: anode
  TYPE(EdgeTreeData_), POINTER :: value_ptr
  INTEGER(I4B) :: ii

  IF (.NOT. obj%ASSOCIATED()) RETURN

  ! Get left
  anode = obj%GetNode(opt=-1)
  IF (anode%ASSOCIATED()) THEN
    CALL EdgeBinaryTree_GetArray(anode, VALUE)
  END IF

  ! Get node value

  value_ptr => obj%GetValuePointer()
  IF (ASSOCIATED(value_ptr)) THEN
    DO ii = 1, SIZE(value_ptr%VALUE)
      VALUE(ii, value_ptr%id) = value_ptr%VALUE(ii)
    END DO
  END IF

  ! Get right
  anode = obj%GetNode(opt=1)
  IF (anode%ASSOCIATED()) THEN
    CALL EdgeBinaryTree_GetArray(anode, VALUE)
  END IF

  NULLIFY (value_ptr)
  CALL anode%Unlink()

END SUBROUTINE EdgeBinaryTree_GetArray

END MODULE EdgeBinaryTreeUtility
