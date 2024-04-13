MODULE fhash_tbl_iter
USE fhash_tbl, ONLY: fhash_tbl_t
USE fhash_key_base, ONLY: fhash_key_t
USE fhash_data_container, ONLY: fhash_container_t
USE fhash_sll
IMPLICIT NONE

PRIVATE
PUBLIC fhash_iter_t

!> Iterator type for iterating over hash table items
TYPE fhash_iter_t

  TYPE(fhash_tbl_t), POINTER :: tbl => NULL()

  INTEGER :: bucket = 1
  INTEGER :: depth = 1

CONTAINS
  PROCEDURE :: next => fhash_iter_next
  PROCEDURE :: reset => fhash_iter_reset
END TYPE fhash_iter_t

INTERFACE fhash_iter_t
  MODULE PROCEDURE :: fhash_iter_init
END INTERFACE fhash_iter_t

CONTAINS

!> Initialise fhash iterator
FUNCTION fhash_iter_init(tbl) RESULT(iter)
  TYPE(fhash_tbl_t), INTENT(in), TARGET :: tbl
  TYPE(fhash_iter_t) :: iter

  iter%tbl => tbl

END FUNCTION fhash_iter_init

!> Return next item from iterator
FUNCTION fhash_iter_next(iter, key, DATA) RESULT(found)
  CLASS(fhash_iter_t), INTENT(inout) :: iter
  CLASS(fhash_key_t), INTENT(out), ALLOCATABLE :: key
  CLASS(*), ALLOCATABLE, INTENT(out) :: DATA
  LOGICAL :: found

  TYPE(fhash_container_t), POINTER :: data_container
  CLASS(*), POINTER :: data_out

  found = .FALSE.

  IF (.NOT. ASSOCIATED(iter%tbl)) RETURN

  DO WHILE (.NOT. found)
    IF (iter%bucket > SIZE(iter%tbl%buckets)) RETURN
    IF (.NOT. ALLOCATED(iter%tbl%buckets(iter%bucket)%key)) THEN
      iter%bucket = iter%bucket + 1
      CYCLE
    END IF
      call sll_get_at(iter%tbl%buckets(iter%bucket),iter%depth,key,data_container,found)
    IF (iter%depth > node_depth(iter%tbl%buckets(iter%bucket))) THEN
      iter%bucket = iter%bucket + 1
      iter%depth = 1
    ELSE
      iter%depth = iter%depth + 1
    END IF
  END DO

  IF (found) THEN
    CALL data_container%get(raw=DATA) ! Extract underlying polymorphic data
  END IF

END FUNCTION fhash_iter_next

!> Reset iterator to beginning
SUBROUTINE fhash_iter_reset(iter)
  CLASS(fhash_iter_t), INTENT(inout) :: iter

  iter%bucket = 1
  iter%depth = 1

END SUBROUTINE fhash_iter_reset

END MODULE fhash_tbl_iter
