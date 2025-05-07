! fhash module i taken from
! https://github.com/LKedward/fhash
!
! I  have modified the naming convention which adhere with the naming
! convention of EASIFEM

MODULE HashTableIter_Class
USE HashTable_Class, ONLY: HashTable_
USE Hashkey_Class, ONLY: Hashkey_
USE HashDataContainer_Class, ONLY: HashDataContainer_
USE fhash_sll, ONLY: sll_get_at, node_depth

IMPLICIT NONE

PRIVATE

PUBLIC :: HashTableIter_
PUBLIC :: HashTableIter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Iterator type for iterating over hash table items
TYPE HashTableIter_
  TYPE(HashTable_), POINTER :: tbl => NULL()

  INTEGER :: bucket = 1
  INTEGER :: depth = 1

CONTAINS
  PROCEDURE :: next => fhash_iter_next
  PROCEDURE :: reset => fhash_iter_reset
END TYPE HashTableIter_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HashTableIter
  MODULE PROCEDURE :: fhash_iter_init
END INTERFACE HashTableIter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!> Initialise fhash iterator
FUNCTION fhash_iter_init(tbl) RESULT(iter)
  TYPE(HashTable_), INTENT(in), TARGET :: tbl
  TYPE(HashTableIter_) :: iter

  iter%tbl => tbl

END FUNCTION fhash_iter_init

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return next item from iterator
FUNCTION fhash_iter_next(iter, key, DATA) RESULT(found)
  CLASS(HashTableIter_), INTENT(inout) :: iter
  CLASS(Hashkey_), INTENT(out), ALLOCATABLE :: key
  CLASS(*), ALLOCATABLE, INTENT(out) :: DATA
  LOGICAL :: found

  TYPE(HashDataContainer_), POINTER :: data_container
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Reset iterator to beginning
SUBROUTINE fhash_iter_reset(iter)
  CLASS(HashTableIter_), INTENT(inout) :: iter

  iter%bucket = 1
  iter%depth = 1

END SUBROUTINE fhash_iter_reset

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HashTableIter_Class
