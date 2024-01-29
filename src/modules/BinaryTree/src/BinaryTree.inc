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

! make sure to define following methods for TreeData_
! Display
! Deallocate
! eq
! lt
!
! Note: eq function is always called first
! Note: lt function is  called only after calling eq
! When you insert data, make sure the data is a valid target
!
! SUBROUTINE obj_Deallocate(obj)
!   CLASS(IntTreeData_), INTENT(INOUT) :: obj
! END SUBROUTINE obj_Deallocate
!
! SUBROUTINE obj_Display(obj, msg, unitno)
!   CLASS(IntTreeData_), INTENT(IN) :: obj
!   CHARACTER(*), INTENT(IN) :: msg
!   INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
! END SUBROUTINE obj_Display
!
! FUNCTION obj_lt(obj, obj2) RESULT(ans)
!   CLASS(IntTreeData_), INTENT(IN) :: obj
!   CLASS(IntTreeData_), INTENT(IN) :: obj2
!   LOGICAL(LGT) :: ans
! END FUNCTION obj_lt
!
! FUNCTION obj_eq(obj, obj2) RESULT(ans)
!   CLASS(IntTreeData_), INTENT(IN) :: obj
!   CLASS(IntTreeData_), INTENT(IN) :: obj2
!   LOGICAL(LGT) :: ans
! END FUNCTION obj_eq

! if you want to update the id then you sould define
! Binary_Tree_Activate_SetID_Method
! In this case TreeData_ should define TreeData_SetID method
! In this case SetID method is available in BinaryTree

#if !defined(Binary_Tree_Type_Name)
#error Binary_Tree_Type_Name must be defined
#endif

! USE IntTreeData_Class, ONLY: TreeData_ => IntTreeData_
USE GlobalData, ONLY: I4B, DFP, LGT
USE Display_Method, ONLY: Display, Tostring, BlankLines
USE InputUtility, ONLY: Input
IMPLICIT NONE
PRIVATE

PUBLIC :: Binary_Tree_Type_Name

TYPE TreeNode_
  TYPE(TreeNode_), POINTER :: left => NULL()
  TYPE(TreeNode_), POINTER :: right => NULL()
  TYPE(TreeNode_), POINTER :: parent => NULL()
  TYPE(TreeData_), POINTER :: VALUE => NULL()
END TYPE TreeNode_

!----------------------------------------------------------------------------
!                                                      Binary_Tree_Type_Name
!----------------------------------------------------------------------------

TYPE Binary_Tree_Type_Name
  PRIVATE
  INTEGER :: tsize = 0
  TYPE(TreeNode_), POINTER :: sentinel => NULL()

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: ASSOCIATED => obj_ASSOCIATED
  !! Returns true if sentinel is ASSOCIATED

  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Unlink => obj_Unlink
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  PROCEDURE, PASS(obj) :: SetSize => obj_SetSize

  PROCEDURE, PASS(obj) :: Insert_1 => obj_Insert_1
  PROCEDURE, PASS(obj) :: Insert_2 => obj_Insert_2
  GENERIC, PUBLIC :: Insert => Insert_1, Insert_2

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content

  PROCEDURE, PASS(obj) :: GetNode1 => obj_GetNode_1
  !! Get the node of the tree an an instance Binary_Tree_Type_Name (subtree)
  !! by specifying the key
  PROCEDURE, PASS(obj) :: GetNode2 => obj_GetNode_2
  !! Get the node of the tree an an instance Binary_Tree_Type_Name (subtree)
  GENERIC, PUBLIC :: GetNode => GetNode1, GetNode2

  PROCEDURE, PASS(obj) :: GetValuePointer1 => obj_GetValuePointer_1
  !! Get pointer to value from the tree by specifying the tree
  PROCEDURE, PASS(obj) :: GetValuePointer2 => obj_GetValuePointer_2
  !! Get pointer to value stored in the node
  GENERIC, PUBLIC :: GetValuePointer => GetValuePointer1,  &
    & GetValuePointer2

  PROCEDURE, PUBLIC, PASS(obj) :: GetMax => obj_GetMax
  PROCEDURE, PUBLIC, PASS(obj) :: GetMin => obj_GetMin
  PROCEDURE, PUBLIC, PASS(obj) :: GetSuccessor => obj_GetSuccessor
  PROCEDURE, PUBLIC, PASS(obj) :: GetPredecessor => obj_GetPredecessor
  PROCEDURE, PASS(obj) :: Remove_1 => obj_Remove_1
  PROCEDURE, PASS(obj) :: Remove_2 => obj_Remove_2
  GENERIC, PUBLIC :: Remove => Remove_1, Remove_2
  PROCEDURE, PUBLIC, PASS(obj) :: SetID => obj_SetID
  !! To update the ids of the node of binary tree
  !! This make sense when Binary_Tree_Activate_SetID_Method is defined
  !! In this case TreeData_SetID method is called for TreeData_ (value)

END TYPE Binary_Tree_Type_Name

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                             ASSOCIATED
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29 
! summary:  Returns the ASSOCIATED status of sentinel

FUNCTION obj_ASSOCIATED(obj) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  LOGICAL(LGT) :: ans
  ans = ASSOCIATED(obj%sentinel)
END FUNCTION obj_ASSOCIATED

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Empty Intitiate tree

SUBROUTINE obj_Initiate1(obj)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  !! Tree to be initiated
  ALLOCATE (obj%sentinel)
END SUBROUTINE obj_Initiate1

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-22
! summary:  Intitiate by copy

SUBROUTINE obj_Initiate2(obj, other)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  TYPE(Binary_Tree_Type_Name), INTENT(INOUT) :: other

  ! Internal variable
  CALL obj%Initiate()
  CALL obj%Insert(other)
END SUBROUTINE obj_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Deallocate entire tree

SUBROUTINE obj_Unlink(obj)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  obj%tsize = 0
  obj%sentinel => NULL()
END SUBROUTINE obj_Unlink

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Deallocate entire tree

SUBROUTINE obj_Deallocate(obj)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  obj%tsize = 0
  IF (ASSOCIATED(obj%sentinel)) THEN
    CALL TreeNodeDeallocate(obj%sentinel)
  END IF
END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                         TreeNodeDeallocate
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE TreeNodeDeallocate(obj)
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: obj

  IF (ASSOCIATED(obj%VALUE)) THEN
    !CALL TreeData_DEALLOCATE(obj%VALUE)
    !DEALLOCATE (obj%VALUE)
    obj%VALUE => NULL()
  END IF

  IF (ASSOCIATED(obj%left)) THEN
    CALL TreeNodeDeallocate(obj%left)
    obj%left => NULL()
  END IF

  IF (ASSOCIATED(obj%right)) THEN
    CALL TreeNodeDeallocate(obj%right)
    obj%right => NULL()
  END IF

  IF (ASSOCIATED(obj%parent)) THEN
    obj%parent => NULL()
  END IF

END SUBROUTINE TreeNodeDeallocate

!----------------------------------------------------------------------------
!                                                                  Size
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Return the size of the tree

FUNCTION obj_Size(obj) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%tsize
END FUNCTION obj_Size

!----------------------------------------------------------------------------
!                                                                  SetSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Set the size

SUBROUTINE obj_SetSize(obj)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  obj%tsize = TreeNodeSize(obj%sentinel)
END SUBROUTINE obj_SetSize

!----------------------------------------------------------------------------
!                                                               TreeNodeSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Return the size of the tree

RECURSIVE FUNCTION TreeNodeSize(obj) RESULT(ans)
  TYPE(TreeNode_), POINTER, INTENT(IN) :: obj
  INTEGER(I4B) :: ans

  ans = 0
  IF (.NOT. ASSOCIATED(obj)) RETURN
  IF (ASSOCIATED(obj%left)) ans = ans + TreeNodeSIZE(obj%left)
  IF (ASSOCIATED(obj%VALUE)) ans = ans + 1
  IF (ASSOCIATED(obj%right)) ans = ans + TreeNodeSize(obj%right)
END FUNCTION TreeNodeSize

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Display the content

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(msg, unitno=unitno)
  CALL Display(obj%tsize, "tsize: ", unitno=unitno)
  CALL Display("Tree Data:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
  CALL TreeNodeDisplay(obj%sentinel, msg="node%root:", unitno=unitno)

END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                           TreeNodeDisplay
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE TreeNodeDisplay(obj, msg, unitno)
  TYPE(TreeNode_), POINTER, INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  ! internal variables
  LOGICAL(LGT) :: bool1

  ! CALL Display(msg, unitno=unitno)
  IF (.NOT. ASSOCIATED(obj)) RETURN

  IF (ASSOCIATED(obj%left)) THEN
    CALL TreeNodeDisplay(obj=obj%left, msg="node%left:", unitno=unitno)
  END IF

  bool1 = ASSOCIATED(obj%VALUE)
  IF (bool1) THEN 
    CALL TreeData_Display(obj%VALUE, msg, unitno=unitno)
    CALL BlankLines(nol=1, unitno=unitno)
  END IF

  IF (ASSOCIATED(obj%right)) THEN
    CALL TreeNodeDisplay(obj=obj%right, msg="node%right:", unitno=unitno)
  END IF
END SUBROUTINE TreeNodeDisplay

!----------------------------------------------------------------------------
!                                                                Insert
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Insert

SUBROUTINE obj_Insert_1(obj, z)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: z

  INTEGER(I4B) :: tsize

  IF (.NOT. ASSOCIATED(obj%sentinel)) RETURN
  IF (.NOT. ASSOCIATED(z%sentinel)) RETURN

  CALL TreeNodeInsert(obj%sentinel, z%sentinel, tsize)

  obj%tsize = obj%tsize + tsize * z%tsize

END SUBROUTINE obj_Insert_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE TreeNodeInsert(obj, z, tsize)
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: obj
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: z
  INTEGER(I4B), INTENT(INOUT) :: tsize

  ! internal variables
  TYPE(TreeNode_), POINTER :: x, y
  LOGICAL(LGT) :: is_nil
  TYPE(TreeData_), POINTER :: key1, key2

  tsize = 1

  is_nil = .NOT. ASSOCIATED(obj%VALUE)
  IF (is_nil) THEN
    obj => z
    RETURN
  END IF

  y => obj

  key1 => z%VALUE
  key2 => obj%VALUE

  IF (TreeData_eq(key1, key2)) THEN
    NULLIFY (x, y)
    tsize = 0
    RETURN
  END IF

  IF (TreeData_lt(key1, key2)) THEN
    x => obj%left
  ELSE
    x => obj%right
  END IF

  DO

    is_nil = .NOT. ASSOCIATED(x)
    IF (is_nil) EXIT

    y => x
    key1 => z%VALUE
    key2 => x%VALUE

    IF (TreeData_eq(key1, key2)) THEN
      NULLIFY (x, y)
      tsize = 0
      RETURN
    END IF

    IF (TreeData_lt(key1, key2)) THEN
      x => x%left
    ELSE
      x => x%right
    END IF

  END DO

  z%parent => y
  key1 => z%VALUE
  key2 => y%VALUE

  IF (TreeData_eq(key1, key2)) THEN
    NULLIFY (x, y)
    tsize = 0
    RETURN
  END IF

  IF (TreeData_lt(key1, key2)) THEN
    y%left => z

  ELSE
    y%right => z

  END IF

  x => NULL()
  y => NULL()

END SUBROUTINE TreeNodeInsert

!----------------------------------------------------------------------------
!                                                                Insert
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Insert

SUBROUTINE obj_Insert_2(obj, VALUE)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  TYPE(TreeData_), TARGET, INTENT(IN) :: VALUE

  ! internal variables
  TYPE(Binary_Tree_Type_Name), POINTER :: newNode

  ALLOCATE (newNode)
  CALL newNode%Initiate()
  newNode%sentinel%VALUE => VALUE
  newNode%tsize = 1
  CALL obj%Insert(newNode)
  newNode => NULL()

END SUBROUTINE obj_Insert_2

!----------------------------------------------------------------------------
!                                                             GetNode
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Get node by specifying the value

FUNCTION obj_GetNode_1(obj, VALUE) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  TYPE(TreeData_), INTENT(IN) :: VALUE
  TYPE(Binary_Tree_Type_Name) :: ans

  CALL ans%Initiate()
  IF (.NOT. ASSOCIATED(obj%sentinel)) RETURN
  ans%sentinel => TreeNodeGetNodePointer(obj%sentinel, VALUE)
  CALL ans%SetSize()
END FUNCTION obj_GetNode_1

!----------------------------------------------------------------------------
!                                                             GetNode
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Get node stored at the sentinel level

FUNCTION obj_GetNode_2(obj, opt) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: opt
  !! -2 for parent
  !! -1 for left
  !! 0 for sentinel
  !! 1 for right
  TYPE(Binary_Tree_Type_Name) :: ans

  CALL ans%Initiate()
  IF (.NOT. ASSOCIATED(obj%sentinel)) RETURN

  SELECT CASE (opt)
  CASE (-2)
    ans%sentinel => obj%sentinel%parent
  CASE (-1)
    ans%sentinel => obj%sentinel%left
  CASE (0)
    ans%sentinel => obj%sentinel
  CASE (1)
    ans%sentinel => obj%sentinel%right
  END SELECT
END FUNCTION obj_GetNode_2

!----------------------------------------------------------------------------
!                                                             GetValuePointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Get the pointer to the value by specifying the key

FUNCTION obj_GetValuePointer_1(obj, VALUE) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  TYPE(TreeData_), INTENT(IN) :: VALUE
  TYPE(TreeData_), POINTER :: ans

  ans => NULL()
  IF (.NOT. ASSOCIATED(obj%sentinel)) RETURN
  ans => TreeNodeGetValuePointer(obj%sentinel, VALUE)
END FUNCTION obj_GetValuePointer_1

!----------------------------------------------------------------------------
!                                                             GetValuePointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Get the pointer to the value stored at the node level

FUNCTION obj_GetValuePointer_2(obj) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  TYPE(TreeData_), POINTER :: ans

  ans => NULL()
  IF (.NOT. ASSOCIATED(obj%sentinel)) RETURN
  ans => obj%sentinel%VALUE
END FUNCTION obj_GetValuePointer_2

!----------------------------------------------------------------------------
!                                                   TreeNodeGetValuePointer
!----------------------------------------------------------------------------

FUNCTION TreeNodeGetValuePointer(obj, VALUE) RESULT(ans)
  TYPE(TreeNode_), POINTER, INTENT(IN) :: obj
  TYPE(TreeData_), INTENT(IN) :: VALUE
  TYPE(TreeData_), POINTER :: ans

  ! internal variable
  LOGICAL(LGT) :: isok, is_nil
  TYPE(TreeNode_), POINTER :: x

  x => NULL()
  ans => NULL()

  is_nil = .NOT. ASSOCIATED(obj%VALUE)
  IF (is_nil) RETURN

  isok = TreeData_eq(VALUE, obj%VALUE)
  IF (isok) THEN
    ans => obj%VALUE
    RETURN
  END IF

  IF (TreeData_lt(VALUE, obj%VALUE)) THEN
    x => obj%left
  ELSE
    x => obj%right
  END IF

  DO

    isok = ASSOCIATED(x)
    IF (isok) THEN
      ans => x%VALUE
    ELSE
      EXIT
    END IF

    isok = TreeData_eq(VALUE, ans)
    IF (isok) EXIT

    IF (TreeData_lt(VALUE, ans)) THEN
      x => x%left

    ELSE
      x => x%right

    END IF

  END DO

  NULLIFY (x)

END FUNCTION TreeNodeGetValuePointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION TreeNodeGetNodePointer(obj, VALUE) RESULT(ans)
  TYPE(TreeNode_), POINTER, INTENT(IN) :: obj
  TYPE(TreeData_), INTENT(IN) :: VALUE
  TYPE(TreeNode_), POINTER :: ans

  ! internal variable
  LOGICAL(LGT) :: isok, is_nil

  ans => NULL()

  is_nil = .NOT. ASSOCIATED(obj%VALUE)
  IF (is_nil) RETURN

  isok = TreeData_eq(VALUE, obj%VALUE)
  IF (isok) THEN
    ans => obj
    RETURN
  END IF

  IF (TreeData_lt(VALUE, obj%VALUE)) THEN
    ans => obj%left
  ELSE
    ans => obj%right
  END IF

  DO

    isok = ASSOCIATED(ans)
    IF (.NOT. isok) EXIT

    isok = TreeData_eq(VALUE, ans%VALUE)
    IF (isok) EXIT

    IF (TreeData_lt(VALUE, ans%VALUE)) THEN
      ans => ans%left

    ELSE
      ans => ans%right

    END IF

  END DO

END FUNCTION TreeNodeGetNodePointer

!----------------------------------------------------------------------------
!                                                                    GetMin
!----------------------------------------------------------------------------

FUNCTION obj_GetMin(obj) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  TYPE(Binary_Tree_Type_Name) :: ans

  CALL ans%Initiate()
  IF (ASSOCIATED(obj%sentinel)) THEN
    ans%sentinel => TreeNodeGetMinPointer(obj%sentinel)
    CALL ans%SetSize()
  END IF

END FUNCTION obj_GetMin

!----------------------------------------------------------------------------
!                                                         GetMinPointer
!----------------------------------------------------------------------------

FUNCTION TreeNodeGetMinPointer(obj) RESULT(ans)
  TYPE(TreeNode_), POINTER, INTENT(IN) :: obj
  TYPE(TreeNode_), POINTER :: ans

  ! Internal variables
  LOGICAL(LGT) :: isok

  ans => obj
  DO
    isok = ASSOCIATED(ans%left)
    IF (.NOT. isok) EXIT
    ans => ans%left
  END DO

END FUNCTION TreeNodeGetMinPointer

!----------------------------------------------------------------------------
!                                                                    GetMin
!----------------------------------------------------------------------------

FUNCTION obj_GetMax(obj) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(IN) :: obj
  TYPE(Binary_Tree_Type_Name) :: ans

  CALL ans%Initiate()
  IF (ASSOCIATED(obj%sentinel)) THEN
    ans%sentinel => TreeNodeGetMaxPointer(obj%sentinel)
    CALL ans%SetSize()
  END IF

END FUNCTION obj_GetMax

!----------------------------------------------------------------------------
!                                                         GetMaxPointer
!----------------------------------------------------------------------------

FUNCTION TreeNodeGetMaxPointer(obj) RESULT(ans)
  TYPE(TreeNode_), POINTER, INTENT(IN) :: obj
  TYPE(TreeNode_), POINTER :: ans

  ! Internal variables
  LOGICAL(LGT) :: isok

  ans => obj
  DO
    isok = ASSOCIATED(ans%right)
    IF (.NOT. isok) EXIT
    ans => ans%right
  END DO

END FUNCTION TreeNodeGetMaxPointer

!----------------------------------------------------------------------------
!                                                              GetSuccessor
!----------------------------------------------------------------------------

FUNCTION obj_GetSuccessor(obj) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  TYPE(Binary_Tree_Type_Name) :: ans

  CALL ans%Initiate()
  IF (ASSOCIATED(obj%sentinel)) THEN
    ans%sentinel => TreeNodeGetSuccessorPointer(obj%sentinel)
    CALL ans%SetSize()
  END IF
END FUNCTION obj_GetSuccessor

!----------------------------------------------------------------------------
!                                               TreeNodeGetSuccessorPointer
!----------------------------------------------------------------------------

FUNCTION TreeNodeGetSuccessorPointer(obj) RESULT(ans)
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: obj
  TYPE(TreeNode_), POINTER :: ans

  ! internal varibles
  LOGICAL(LGT) :: isok
  TYPE(TreeNode_), POINTER :: x

  isok = ASSOCIATED(obj%right)

  IF (isok) THEN
    ans => TreeNodeGetMinPointer(obj%right)
    RETURN
  END IF

  x => obj
  ans => x%parent

  DO
    isok = ASSOCIATED(ans)
    IF (.NOT. isok) EXIT

    ! isok = x%VALUE%eq(ans%right%VALUE)
    isok = .NOT. ASSOCIATED(x, ans%right)
    IF (isok) EXIT

    x => ans
    ans => ans%parent

  END DO

  x => NULL()

END FUNCTION TreeNodeGetSuccessorPointer

!----------------------------------------------------------------------------
!                                                             GetPredecessor
!----------------------------------------------------------------------------

FUNCTION obj_GetPredecessor(obj) RESULT(ans)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  TYPE(Binary_Tree_Type_Name) :: ans

  CALL ans%Initiate()
  IF (ASSOCIATED(obj%sentinel)) THEN
    ans%sentinel => TreeNodeGetPredecessorPointer(obj%sentinel)
    CALL ans%SetSize()
  END IF
END FUNCTION obj_GetPredecessor

!----------------------------------------------------------------------------
!                                             TreeNodeGetPredecessorPointer
!----------------------------------------------------------------------------

FUNCTION TreeNodeGetPredecessorPointer(obj) RESULT(ans)
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: obj
  TYPE(TreeNode_), POINTER :: ans

  ! internal varibles
  LOGICAL(LGT) :: isok
  TYPE(TreeNode_), POINTER :: x

  isok = ASSOCIATED(obj%left)

  IF (isok) THEN
    ans => TreeNodeGetMaxPointer(obj%left)
    RETURN
  END IF

  x => obj
  ans => x%parent

  DO
    isok = ASSOCIATED(ans)
    IF (.NOT. isok) EXIT

    ! isok = .NOT. x%VALUE%eq(ans%left%VALUE)
    isok = .NOT. ASSOCIATED(x, ans%left)
    IF (isok) EXIT

    x => ans
    ans => ans%parent

  END DO

  x => NULL()

END FUNCTION TreeNodeGetPredecessorPointer

!----------------------------------------------------------------------------
!                                                           Transplant
!----------------------------------------------------------------------------

SUBROUTINE TreeNodeTransplant(obj, u, v)
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: obj
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: u
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: v

  ! internal variables
  LOGICAL(LGT) :: case1, case2

  case1 = .NOT. ASSOCIATED(u%parent)

  IF (case1) THEN
    ! CALL TreeNodeSetRoot(obj, v)
    obj => v

  ELSE

    case2 = ASSOCIATED(u%parent%left, u)

    IF (case2) THEN
      u%parent%left => v

    ELSE
      u%parent%right => v
    END IF

  END IF

  IF (ASSOCIATED(v)) v%parent => u%parent

END SUBROUTINE TreeNodeTransplant

!----------------------------------------------------------------------------
!                                                                   Remove
!----------------------------------------------------------------------------

SUBROUTINE obj_Remove_1(obj, z)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  TYPE(Binary_Tree_Type_Name), INTENT(INOUT) :: z

  IF (.NOT. ASSOCIATED(obj%sentinel)) RETURN
  IF (.NOT. ASSOCIATED(z%sentinel)) RETURN

  CALL TreeNodeRemove(obj%sentinel, z%sentinel)
  obj%tsize = obj%tsize - 1
END SUBROUTINE obj_Remove_1

!----------------------------------------------------------------------------
!                                                                   Remove
!----------------------------------------------------------------------------

SUBROUTINE obj_Remove_2(obj, VALUE)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  TYPE(TreeData_), INTENT(IN) :: VALUE

  TYPE(Binary_Tree_Type_Name) :: x
  x = obj%GetNode(VALUE)
  CALL obj%Remove(x)
  CALL x%Unlink()
END SUBROUTINE obj_Remove_2

!----------------------------------------------------------------------------
!                                                                 Remove
!----------------------------------------------------------------------------

SUBROUTINE TreeNodeRemove(obj, z)
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: obj
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: z

  ! Internal variable
  TYPE(TreeNode_), POINTER :: y
  LOGICAL(LGT) :: isok, case1, case2

  case1 = .NOT. ASSOCIATED(z%left)
  case2 = .NOT. ASSOCIATED(z%right)

  IF (case1) THEN
    CALL TreeNodeTransplant(obj, z, z%right)

  ELSEIF (case2) THEN
    CALL TreeNodeTransplant(obj, z, z%left)

  ELSE
    y => TreeNodeGetMinPointer(z%right)
    ! isok = .NOT. y%VALUE%eq(z%right%VALUE)
    isok = .NOT. ASSOCIATED(y, z%right)

    IF (isok) THEN
      CALL TreeNodeTransplant(obj, y, y%right)
      y%right => z%right
      y%right%parent => y
    END IF

    CALL TreeNodeTransplant(obj, z, y)
    y%left => z%left
    y%left%parent => y
  END IF

  y => NULL()
END SUBROUTINE TreeNodeRemove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_SetID(obj)
  CLASS(Binary_Tree_Type_Name), INTENT(INOUT) :: obj
  INTEGER(I4B) :: id
  IF (.NOT. ASSOCIATED(obj%sentinel)) RETURN
  id = 0
  CALL TreeNode_SetID(obj%sentinel, id)
END SUBROUTINE obj_SetID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE TreeNode_SetID(obj, id)
  TYPE(TreeNode_), POINTER, INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(INOUT) :: id

#ifdef Binary_Tree_Activate_SetID_Method
  IF (.NOT. ASSOCIATED(obj)) RETURN
  IF (ASSOCIATED(obj%left)) CALL TreeNode_SetID(obj%left, id)
  id = id + 1
  CALL TreeData_SetID(obj%VALUE, id)
  IF (ASSOCIATED(obj%right)) CALL TreeNode_SetID(obj%right, id)
#endif

END SUBROUTINE TreeNode_SetID
