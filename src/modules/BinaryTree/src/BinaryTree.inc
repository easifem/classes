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

TYPE BINARY_TREE
  TYPE(BINARY_TREE), POINTER :: left
  TYPE(BINARY_TREE), POINTER :: right
  TYPE(TREE_DATA) :: DATA
END TYPE BINARY_TREE

!
! Define the subroutines and functions
!
CONTAINS

! btree_create --
!     Create and initialise a binary tree
! Arguments:
!     btree      Pointer to new binary tree
!     data       The data for the first element
! Note:
!     This version assumes a shallow copy is enough
!     (that is, there are no pointers within the data
!     to be stored)
!     It also assumes the argument btree does not already
!     refer to a btree. Use btree_destroy first to
!     destroy up an old btree.
!
SUBROUTINE btree_create(btree, DATA)
  TYPE(BINARY_TREE), POINTER :: btree
  TYPE(TREE_DATA), INTENT(in) :: DATA

  ALLOCATE (btree)
  btree%left => NULL()
  btree%right => NULL()
  btree%DATA = DATA
END SUBROUTINE btree_create

! btree_destroy --
!     Destroy an entire btree
! Arguments:
!     btree       Pointer to the btree to be destroyed
! Note:
!     This version assumes that there are no
!     pointers within the data that need deallocation
!
RECURSIVE SUBROUTINE btree_destroy(btree)
  TYPE(BINARY_TREE), POINTER :: btree

  TYPE(BINARY_TREE), POINTER :: left
  TYPE(BINARY_TREE), POINTER :: right

  left => btree%left
  right => btree%right

  IF (ASSOCIATED(left)) THEN
    CALL btree_destroy(left)
  END IF

  IF (ASSOCIATED(right)) THEN
    CALL btree_destroy(right)
  END IF
  DEALLOCATE (btree)
END SUBROUTINE btree_destroy

! btree_count --
!     Count the number of nodes in the btree
! Arguments:
!     btree       Pointer to the btree
!
RECURSIVE FUNCTION btree_count(btree) RESULT(count)
  TYPE(BINARY_TREE), POINTER :: btree
  INTEGER :: count

  IF (ASSOCIATED(btree)) THEN
    count = 1 + btree_count(btree%left) + btree_count(btree%right)
  ELSE
    count = 0
  END IF
END FUNCTION btree_count

! btree_child_node
!     Return the left or right child node
! Arguments:
!     btree      Some node in the tree
!     right      Return the right node if true,
!                otherwise the left node
! Result:
!
FUNCTION btree_child_node(btree, right) RESULT(child)
  TYPE(BINARY_TREE), POINTER :: btree
  LOGICAL, INTENT(in) :: right

  TYPE(BINARY_TREE), POINTER :: child

  IF (right) THEN
    child => btree%right
  ELSE
    child => btree%left
  END IF

END FUNCTION btree_child_node

! btree_append_data
!     Append a new node (left or right) if
!     possible. Otherwise nothing is done
! Arguments:
!     btree      Some node in the tree
!     data       The data for the new node
!     right      Append right or left
!
SUBROUTINE btree_append_data(btree, DATA, right)
  TYPE(BINARY_TREE), POINTER :: btree
  TYPE(TREE_DATA), INTENT(in) :: DATA
  LOGICAL, INTENT(in) :: right

  TYPE(BINARY_TREE), POINTER :: new

  IF (right .AND. ASSOCIATED(btree%right)) THEN
    RETURN
  END IF
  IF (.NOT. right .AND. ASSOCIATED(btree%left)) THEN
    RETURN
  END IF

  CALL btree_create(new, DATA)

  IF (right) THEN
    btree%right => new
  ELSE
    btree%left => new
  END IF
END SUBROUTINE btree_append_data

! btree_append_subtree
!     Append a new subtree (left or right) if
!     possible. Otherwise nothing is done
! Arguments:
!     btree      Some node in the tree
!     subtree    Subtree to be appended
!     right      Append right or left
!
SUBROUTINE btree_append_subtree(btree, subtree, right)
  TYPE(BINARY_TREE), POINTER :: btree
  TYPE(BINARY_TREE), POINTER :: subtree
  LOGICAL, INTENT(in) :: right

  IF (right .AND. ASSOCIATED(btree%right)) THEN
    RETURN
  END IF
  IF (.NOT. right .AND. ASSOCIATED(btree%left)) THEN
    RETURN
  END IF

  IF (right) THEN
    btree%right => subtree
  ELSE
    btree%left => subtree
  END IF
END SUBROUTINE btree_append_subtree

! btree_remove_subtree
!     Remove the subtree on the left or right) if
!     possible. Otherwise nothing is done
! Arguments:
!     btree      Some node in the tree
!     subtree    Subtree that was removed (returned!)
!     right      Append right or left
!
SUBROUTINE btree_remove_subtree(btree, subtree, right)
  TYPE(BINARY_TREE), POINTER :: btree
  TYPE(BINARY_TREE), POINTER :: subtree
  LOGICAL, INTENT(in) :: right

  subtree => NULL()

  IF (right .AND. ASSOCIATED(btree%right)) THEN
    RETURN
  END IF
  IF (.NOT. right .AND. ASSOCIATED(btree%left)) THEN
    RETURN
  END IF

  IF (right) THEN
    subtree => btree%right
    btree%right => NULL()
  ELSE
    subtree => btree%left
    btree%left => NULL()
  END IF
END SUBROUTINE btree_remove_subtree

! btree_get_data
!     Get the data stored with a tree node
! Arguments:
!     node       Tree node
!
FUNCTION btree_get_data(node) RESULT(DATA)
  TYPE(BINARY_TREE), POINTER :: node

  TYPE(TREE_DATA) :: DATA

  DATA = node%DATA
END FUNCTION btree_get_data

! btree_put_data
!     Store new data with a tree node
! Arguments:
!     node       Tree node
!     data       The data to be stored
!
SUBROUTINE btree_put_data(node, DATA)
  TYPE(BINARY_TREE), POINTER :: node
  TYPE(TREE_DATA), INTENT(in) :: DATA

  node%DATA = DATA
END SUBROUTINE btree_put_data
