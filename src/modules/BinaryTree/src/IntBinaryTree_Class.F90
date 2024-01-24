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

MODULE IntBinaryTree_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE Display_Method, ONLY: Display, Tostring
USE IntTreeData_Class, ONLY: TreeData_ => IntTreeData_
USE ExceptionHandler_Class, ONLY: e
USE InputUtility, ONLY: Input
IMPLICIT NONE

PRIVATE
PUBLIC :: IntBinaryTree_
CHARACTER(*), PARAMETER :: modName = "IntBinaryTree_Class()"

!----------------------------------------------------------------------------
!                                                             IntBinaryTree_
!----------------------------------------------------------------------------

TYPE IntBinaryTree_
  PRIVATE
  CLASS(IntBinaryTree_), POINTER :: left => NULL()
  CLASS(IntBinaryTree_), POINTER :: right => NULL()
  CLASS(IntBinaryTree_), POINTER :: parent => NULL()
  CLASS(TreeData_), POINTER :: VALUE => NULL()

CONTAINS
  PRIVATE
  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size

  PROCEDURE, PASS(obj) :: Insert_1 => obj_Insert_1
  PROCEDURE, PASS(obj) :: Insert_2 => obj_Insert_2
  GENERIC, PUBLIC :: Insert => Insert_1, Insert_2

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodePointer => obj_GetNodePointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxPointer => obj_GetMaxPointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetMinPointer => obj_GetMinPointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetSuccessorPointer =>  &
    & obj_GetSuccessorPointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetPredecessorPointer =>  &
    & obj_GetPredecessorPointer
  PROCEDURE, PUBLIC, PASS(obj) :: Transplant => obj_Transplant
  PROCEDURE, PASS(obj) :: SetRoot => obj_SetRoot
  PROCEDURE, PUBLIC, PASS(obj) :: Delete => obj_Delete

END TYPE IntBinaryTree_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Intitiate tree

SUBROUTINE obj_Initiate1(obj, VALUE)
  CLASS(IntBinaryTree_), INTENT(INOUT) :: obj
  TYPE(TreeData_), TARGET, INTENT(IN) :: VALUE
  !! value should be a target to a pointer obj%value

  CALL obj%DEALLOCATE()
  obj%left => NULL()
  obj%right => NULL()
  obj%parent => NULL()
  obj%VALUE => VALUE
END SUBROUTINE obj_Initiate1

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Intitiate tree

SUBROUTINE obj_Initiate2(obj, anode)
  CLASS(IntBinaryTree_), INTENT(INOUT) :: obj
  CLASS(IntBinaryTree_), INTENT(IN) :: anode

  CALL obj%DEALLOCATE()
  obj%left => anode%left
  obj%right => anode%right
  obj%parent => anode%parent
  obj%VALUE => anode%VALUE
END SUBROUTINE obj_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Deallocate entire tree

RECURSIVE SUBROUTINE obj_Deallocate(obj)
  CLASS(IntBinaryTree_), INTENT(INOUT) :: obj

  TYPE(IntBinaryTree_), POINTER :: anode

  IF (ASSOCIATED(obj%VALUE)) THEN
    CALL obj%VALUE%DEALLOCATE()
  END IF

  anode => NULL()

  anode => obj%left
  IF (ASSOCIATED(anode)) THEN
    CALL anode%DEALLOCATE()
  END IF

  anode => obj%right
  IF (ASSOCIATED(anode)) THEN
    CALL anode%DEALLOCATE()
  END IF

  anode => obj%parent
  IF (ASSOCIATED(anode)) THEN
    CALL anode%DEALLOCATE()
  END IF

  anode => NULL()

END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                  Size
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Return the size of the tree

RECURSIVE FUNCTION obj_Size(obj) RESULT(ans)
  CLASS(IntBinaryTree_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans

  LOGICAL(LGT) :: bool1

  ans = 0

  IF (ASSOCIATED(obj%left)) THEN
    ans = ans + obj%left%SIZE()
  END IF

  bool1 = ASSOCIATED(obj%VALUE)
  IF (bool1) ans = ans + 1

  IF (ASSOCIATED(obj%right)) THEN
    ans = ans + obj%right%SIZE()
  END IF

END FUNCTION obj_Size

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Display the content

RECURSIVE SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(IntBinaryTree_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  ! internal variables
  LOGICAL(LGT) :: bool1

  ! CALL Display(msg, unitno=unitno)

  IF (ASSOCIATED(obj%left)) THEN
    CALL obj%left%Display(msg="node%left:", unitno=unitno)
  END IF

  bool1 = ASSOCIATED(obj%VALUE)
  IF (bool1) CALL obj%VALUE%Display(msg, unitno=unitno)

  IF (ASSOCIATED(obj%right)) THEN
    CALL obj%right%Display(msg="node()%right:", unitno=unitno)
  END IF

END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                                Insert
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Insert

SUBROUTINE obj_Insert_1(obj, z)
  CLASS(IntBinaryTree_), TARGET, INTENT(INOUT) :: obj
  CLASS(IntBinaryTree_), TARGET, INTENT(INOUT) :: z

  ! internal variables
  CLASS(IntBinaryTree_), POINTER :: x, y
  LOGICAL(LGT) :: isAssociated
  CLASS(TreeData_), POINTER :: key1, key2

  isAssociated = ASSOCIATED(obj%VALUE)
  IF (.NOT. isAssociated) THEN
    CALL obj%Initiate(z)
    RETURN
  END IF

  y => obj

  key1 => z%VALUE
  key2 => obj%VALUE

  IF (key1%eq(key2)) THEN
    NULLIFY (x, y)
    RETURN
  END IF

  IF (key1%lt(key2)) THEN
    x => obj%left
  ELSE
    x => obj%right
  END IF

  DO

    isAssociated = ASSOCIATED(x)
    IF (.NOT. isAssociated) EXIT
    y => x
    key1 => z%VALUE
    key2 => x%VALUE

    IF (key1%eq(key2)) THEN
      NULLIFY (x, y)
      RETURN
    END IF

    IF (key1%lt(key2)) THEN
      x => x%left
    ELSE
      x => x%right
    END IF

  END DO

  z%parent => y
  key1 => z%VALUE
  key2 => y%VALUE

  IF (key1%eq(key2)) THEN
    NULLIFY (x, y)
    RETURN
  END IF

  IF (key1%lt(key2)) THEN
    y%left => z

  ELSE
    y%right => z

  END IF

  x => NULL()
  y => NULL()

END SUBROUTINE obj_Insert_1

!----------------------------------------------------------------------------
!                                                                Insert
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Insert

SUBROUTINE obj_Insert_2(obj, VALUE)
  CLASS(IntBinaryTree_), TARGET, INTENT(INOUT) :: obj
  CLASS(TreeData_), TARGET, INTENT(IN) :: VALUE

  ! internal variables
  CLASS(IntBinaryTree_), POINTER :: newNode

  ALLOCATE (newNode)
  CALL newNode%Initiate(VALUE=VALUE)
  CALL obj%Insert(newNode)

  newNode => NULL()

END SUBROUTINE obj_Insert_2

!----------------------------------------------------------------------------
!                                                             GetNodePointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Get node pointer

FUNCTION obj_GetNodePointer(obj, VALUE) RESULT(ans)
  CLASS(IntBinaryTree_), TARGET, INTENT(IN) :: obj
  CLASS(TreeData_), INTENT(IN) :: VALUE
  CLASS(IntBinaryTree_), POINTER :: ans

  ! internal variable
  LOGICAL(LGT) :: isok

  ans => NULL()

  isok = ASSOCIATED(obj%VALUE)
  IF (.NOT. isok) RETURN

  isok = VALUE%eq(obj%VALUE)
  IF (isok) THEN
    ans => obj
    RETURN
  END IF

  IF (VALUE%lt(obj%VALUE)) THEN
    ans => obj%left
  ELSE
    ans => obj%right
  END IF

  DO

    isok = ASSOCIATED(ans)
    IF (.NOT. isok) EXIT

    isok = VALUE%eq(ans%VALUE)
    IF (isok) EXIT

    IF (VALUE%lt(ans%VALUE)) THEN
      ans => ans%left

    ELSE
      ans => ans%right

    END IF

  END DO

END FUNCTION obj_GetNodePointer

!----------------------------------------------------------------------------
!                                                         GetMinPointer
!----------------------------------------------------------------------------

FUNCTION obj_GetMinPointer(obj) RESULT(ans)
  CLASS(IntBinaryTree_), TARGET, INTENT(IN) :: obj
  CLASS(IntBinaryTree_), POINTER :: ans

  ! Internal variables
  LOGICAL(LGT) :: isok

  ans => obj
  DO
    isok = ASSOCIATED(ans%left)
    IF (.NOT. isok) EXIT
    ans => ans%left
  END DO

END FUNCTION obj_GetMinPointer

!----------------------------------------------------------------------------
!                                                         GetMaxPointer
!----------------------------------------------------------------------------

FUNCTION obj_GetMaxPointer(obj) RESULT(ans)
  CLASS(IntBinaryTree_), TARGET, INTENT(IN) :: obj
  CLASS(IntBinaryTree_), POINTER :: ans

  ! Internal variables
  LOGICAL(LGT) :: isok

  ans => obj
  DO
    isok = ASSOCIATED(ans%right)
    IF (.NOT. isok) EXIT
    ans => ans%right
  END DO

END FUNCTION obj_GetMaxPointer

!----------------------------------------------------------------------------
!                                                       GetSuccessorPointer
!----------------------------------------------------------------------------

FUNCTION obj_GetSuccessorPointer(obj) RESULT(ans)
  CLASS(IntBinaryTree_), TARGET, INTENT(IN) :: obj
  CLASS(IntBinaryTree_), POINTER :: ans

  ! internal varibles
  LOGICAL(LGT) :: isok
  CLASS(IntBinaryTree_), POINTER :: x

  isok = ASSOCIATED(obj%right)

  IF (isok) THEN
    ans => obj%right%GetMinPointer()
    RETURN
  END IF

  x => obj
  ans => x%parent

  DO
    isok = ASSOCIATED(ans)
    IF (.NOT. isok) EXIT

    isok = x%VALUE%eq(ans%right%VALUE)
    IF (.NOT. isok) EXIT

    x => ans
    ans => ans%parent

  END DO

  x => NULL()

END FUNCTION obj_GetSuccessorPointer

!----------------------------------------------------------------------------
!                                                      GetPredecessorPointer
!----------------------------------------------------------------------------

FUNCTION obj_GetPredecessorPointer(obj) RESULT(ans)
  CLASS(IntBinaryTree_), TARGET, INTENT(IN) :: obj
  CLASS(IntBinaryTree_), POINTER :: ans

  ! internal varibles
  LOGICAL(LGT) :: isok
  CLASS(IntBinaryTree_), POINTER :: x

  isok = ASSOCIATED(obj%left)

  IF (isok) THEN
    ans => obj%left%GetMaxPointer()
    RETURN
  END IF

  x => obj
  ans => x%parent

  DO
    isok = ASSOCIATED(ans)
    IF (.NOT. isok) EXIT

    isok = x%VALUE%eq(ans%left%VALUE)
    IF (.NOT. isok) EXIT

    x => ans
    ans => ans%parent

  END DO

  x => NULL()

END FUNCTION obj_GetPredecessorPointer

!----------------------------------------------------------------------------
!                                                           Transplant
!----------------------------------------------------------------------------

SUBROUTINE obj_Transplant(obj, u, v)
  CLASS(IntBinaryTree_), TARGET, INTENT(INOUT) :: obj
  CLASS(IntBinaryTree_), TARGET, INTENT(INOUT) :: u
  CLASS(IntBinaryTree_), POINTER, INTENT(INOUT) :: v

  ! internal variables
  LOGICAL(LGT) :: case1, case2

  case1 = .NOT. ASSOCIATED(u%parent)

  IF (case1) THEN
    CALL obj%Set(v)
    obj%parent => NULL()

  ELSE

    case2 = ASSOCIATED(u%parent%left, u)

    IF (case2) THEN
      u%parent%left => v

    ELSE
      u%parent%right => v
    END IF

  END IF

  IF (ASSOCIATED(v)) v%parent => u%parent

END SUBROUTINE obj_Transplant

!----------------------------------------------------------------------------
!                                                                 Set
!----------------------------------------------------------------------------

SUBROUTINE obj_SetRoot(obj, x)
  CLASS(IntBinaryTree_), INTENT(INOUT) :: obj
  CLASS(IntBinaryTree_), INTENT(IN) :: x

  obj%left => x%left
  obj%right => x%right
  obj%parent => x%parent
  obj%VALUE => x%VALUE

END SUBROUTINE obj_SetRoot

!----------------------------------------------------------------------------
!                                                                 Delete
!----------------------------------------------------------------------------

SUBROUTINE obj_Delete(obj, z)
  CLASS(IntBinaryTree_), TARGET, INTENT(INOUT) :: obj
  CLASS(IntBinaryTree_), TARGET, INTENT(INOUT) :: z

  ! Internal variable
  CLASS(IntBinaryTree_), POINTER :: y
  LOGICAL(LGT) :: isok, case1, case2

  case1 = .NOT. ASSOCIATED(z%left)
  case2 = .NOT. ASSOCIATED(z%right)

  IF (case1) THEN
    CALL obj%Transplant(z, z%right)

  ELSEIF (case2) THEN
    CALL obj%Transplant(z, z%left)

  ELSE
    y => z%right%GetMinPointer()
    ! isok = .NOT. y%VALUE%eq(z%right%VALUE)
    isok = .NOT. ASSOCIATED(y, z%right)

    IF (isok) THEN
      CALL obj%Transplant(y, y%right)
      y%right => z%right
      y%right%parent => y
    END IF

    CALL obj%Transplant(z, y)
    y%left => z%left
    y%left%parent => y
  END IF

  y => NULL()
END SUBROUTINE obj_Delete

!----------------------------------------------------------------------------
!                                                           GetNodePointer
!----------------------------------------------------------------------------

END MODULE IntBinaryTree_Class
