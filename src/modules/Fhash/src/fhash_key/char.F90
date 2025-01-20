!> Implements a concrete type for scalar int32 hash keys
!>
MODULE fhash_key_char
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64
USE fhash_key_base, ONLY: fhash_key_t
USE fhash_fnv, ONLY: fnv_1a
IMPLICIT NONE

PRIVATE
PUBLIC fhash_key_char_t
PUBLIC fhash_key

!> Hash table key container
TYPE, EXTENDS(fhash_key_t) :: fhash_key_char_t
  PRIVATE
  CHARACTER(:), ALLOCATABLE :: VALUE
CONTAINS
  PROCEDURE, PASS :: hash => key_hash_char
  PROCEDURE, PASS :: equals => key_equal_char
  PROCEDURE, PASS :: to_string => key_char_to_string
END TYPE fhash_key_char_t

INTERFACE fhash_key
  MODULE PROCEDURE :: key_from_char
END INTERFACE fhash_key

CONTAINS

!> Check if two keys are equal
PURE FUNCTION key_equal_char(key1, key2) RESULT(keys_equal)
  CLASS(fhash_key_char_t), INTENT(in) :: key1
  CLASS(fhash_key_t), INTENT(in) :: key2
  LOGICAL :: keys_equal

  keys_equal = .FALSE.

  SELECT TYPE (k2 => key2)
  TYPE is (fhash_key_char_t)
    IF (ALLOCATED(key1%VALUE) .AND. ALLOCATED(k2%VALUE)) THEN
      IF (key1%VALUE == k2%VALUE) THEN
        keys_equal = .TRUE.
        RETURN
      END IF
    END IF
  END SELECT

END FUNCTION key_equal_char

!> Generate hash of key
PURE FUNCTION key_hash_char(key) RESULT(hash)
  CLASS(fhash_key_char_t), INTENT(in) :: key
  INTEGER(INT64) :: hash

  hash = fnv_1a(key%VALUE)

END FUNCTION key_hash_char

!> Generate string representation of hash
FUNCTION key_char_to_string(key) RESULT(str)
  CLASS(fhash_key_char_t), INTENT(in) :: key
  CHARACTER(:), ALLOCATABLE :: str

  str = key%VALUE

END FUNCTION key_char_to_string

!> Create new key container from a scalar int32
FUNCTION key_from_char(source) RESULT(key)
  CHARACTER(*), INTENT(in) :: source
  TYPE(fhash_key_char_t) :: key

  key%VALUE = source

END FUNCTION key_from_char

END MODULE fhash_key_char
