!> Implements singly-linked list (sll) node with generic data container
!>
MODULE fhash_sll
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64
USE fhash_key_base, ONLY: fhash_key_t
USE fhash_data_container, ONLY: fhash_container_t
IMPLICIT NONE

!> Node type for hash table singly linked list
TYPE fhash_node_t

  CLASS(fhash_key_t), ALLOCATABLE :: key
  TYPE(fhash_container_t) :: VALUE
  TYPE(fhash_node_t), POINTER :: next => NULL()

END TYPE fhash_node_t

CONTAINS

!> Append node to SLL
RECURSIVE SUBROUTINE sll_push_node(node, key, VALUE, POINTER)

  !> Node to which to add data
  TYPE(fhash_node_t), INTENT(inout) :: node

  !> Key to add
  CLASS(fhash_key_t), INTENT(in) :: key

  !> Value to add
  CLASS(*), INTENT(in), TARGET :: VALUE

  !> Store only a point if .true.
  LOGICAL, INTENT(in), OPTIONAL :: POINTER

  IF (ALLOCATED(node%key)) THEN

    IF (node%key == key) THEN

      CALL sll_node_set(node, VALUE, POINTER)
      RETURN

    END IF

    IF (.NOT. ASSOCIATED(node%next)) THEN
      ALLOCATE (node%next)
    END IF

    CALL sll_push_node(node%next, key, VALUE, POINTER)

  ELSE

    node%key = key
    CALL sll_node_set(node, VALUE, POINTER)

  END IF

END SUBROUTINE sll_push_node

!> Set container value in node
!>
SUBROUTINE sll_node_set(node, VALUE, POINTER)

  !> Node to which to add data
  TYPE(fhash_node_t), INTENT(inout) :: node

  !> Value to set
  CLASS(*), INTENT(in), TARGET :: VALUE

  !> Store only a pointer if .true.
  LOGICAL, INTENT(in), OPTIONAL :: POINTER

  IF (PRESENT(POINTER)) THEN
    IF (POINTER) THEN
      node%VALUE%scalar_ptr => VALUE
      RETURN
    END IF
  END IF

  IF (ALLOCATED(node%VALUE%scalar_data)) DEALLOCATE (node%VALUE%scalar_data)
  ALLOCATE (node%VALUE%scalar_data, source=VALUE)

END SUBROUTINE sll_node_set

!> Search for a node with a specific key.
!> Returns a pointer to the 'data' component of the corresponding node.
!> Pointer is not associated if node cannot be found
RECURSIVE SUBROUTINE sll_find_in(node, key, DATA, found)

  !> Node to search in
  TYPE(fhash_node_t), INTENT(in), TARGET :: node

  !> Key to look for
  CLASS(fhash_key_t) :: key

  !> Pointer to value container if found.
  !> (Unassociated if the key is not found in node)
  TYPE(fhash_container_t), POINTER, INTENT(out) :: DATA

  LOGICAL, INTENT(out), OPTIONAL :: found

  DATA => NULL()

  IF (PRESENT(found)) found = .FALSE.

  IF (.NOT. ALLOCATED(node%key)) THEN

    RETURN

  ELSE IF (node%key == key) THEN

    IF (PRESENT(found)) found = .TRUE.
    DATA => node%VALUE
    RETURN

  ELSE IF (ASSOCIATED(node%next)) THEN

    CALL sll_find_in(node%next, key, DATA, found)

  END IF

END SUBROUTINE sll_find_in

!> Return a node at a specific depth in the sll
RECURSIVE SUBROUTINE sll_get_at(node, depth, key, DATA, found)

  !> Node to search in
  TYPE(fhash_node_t), INTENT(in), TARGET :: node

  !> Node depth to access
  INTEGER, INTENT(in) :: depth

  !> Key of found item
  !>  (Unallocated if no node is found at specified depth)
  CLASS(fhash_key_t), INTENT(out), ALLOCATABLE :: key

  !> Pointer to value container if found.
  !> (Unassociated if no node is found at specified depth)
  TYPE(fhash_container_t), POINTER, INTENT(out) :: DATA

  LOGICAL, INTENT(out), OPTIONAL :: found

  DATA => NULL()

  IF (PRESENT(found)) found = .FALSE.

  IF (.NOT. ALLOCATED(node%key)) THEN

    RETURN

  ELSE IF (depth == 1) THEN

    IF (PRESENT(found)) found = .TRUE.
    key = node%key
    DATA => node%VALUE
    RETURN

  ELSE IF (ASSOCIATED(node%next)) THEN

    CALL sll_get_at(node%next, depth - 1, key, DATA, found)

  END IF

END SUBROUTINE sll_get_at

!> Search for a node with a specific key and remove
RECURSIVE SUBROUTINE sll_remove(node, key, found, parent_node)

  !> Node to remove from
  TYPE(fhash_node_t), INTENT(inout) :: node

  !> Key to remove
  CLASS(fhash_key_t) :: key

  !> Indicates if the key was found in node and removed
  LOGICAL, OPTIONAL, INTENT(out) :: found

  !> Used internally
  TYPE(fhash_node_t), INTENT(inout), OPTIONAL :: parent_node

  TYPE(fhash_node_t), POINTER :: next_temp

  IF (PRESENT(found)) THEN
    found = .FALSE.
  END IF

  IF (.NOT. ALLOCATED(node%key)) THEN

    RETURN

  ELSE IF (node%key == key) THEN

    IF (PRESENT(found)) THEN
      found = .TRUE.
    END IF

    IF (.NOT. PRESENT(parent_node)) THEN
      ! This is the top-level node
      IF (ASSOCIATED(node%next)) THEN
        ! Replace with next
        next_temp => node%next
        node = next_temp
        DEALLOCATE (next_temp)
        RETURN
      ELSE
        ! No children, just deallocate
        DEALLOCATE (node%key)
        RETURN
      END IF

    ELSE
      ! Not top-level node
      IF (ASSOCIATED(node%next)) THEN
        ! Join previous with next
        next_temp => node%next
        DEALLOCATE (parent_node%next)
        parent_node%next => next_temp
        RETURN
      ELSE
        ! No children, just deallocate
        DEALLOCATE (node%key)
        DEALLOCATE (parent_node%next)
        RETURN
      END IF
    END IF

  ELSE IF (ASSOCIATED(node%next)) THEN
    ! Look further down
    CALL sll_remove(node%next, key, found, node)

  END IF

END SUBROUTINE sll_remove

!> Deallocate node components and those of its children
RECURSIVE SUBROUTINE sll_clean(node)

  !> Node to search in
  TYPE(fhash_node_t), INTENT(inout) :: node

  IF (ASSOCIATED(node%next)) THEN

    CALL sll_clean(node%next)
    DEALLOCATE (node%next)

  END IF

END SUBROUTINE sll_clean

!> Determine depth of SLL
FUNCTION node_depth(node) RESULT(depth)

  !> Node to check depth
  TYPE(fhash_node_t), INTENT(in), TARGET :: node

  INTEGER :: depth

  TYPE(fhash_node_t), POINTER :: current

  IF (.NOT. ALLOCATED(node%key)) THEN

    depth = 0
    RETURN

  ELSE

    depth = 1
    current => node
    DO WHILE (ASSOCIATED(current%next))
      depth = depth + 1
      current => current%next
    END DO

  END IF

END FUNCTION node_depth

END MODULE fhash_sll
