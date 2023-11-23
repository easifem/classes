!> Implements a concrete type for scalar int64 hash keys
!>
MODULE fhash_key_int64
USE ISO_FORTRAN_ENV, ONLY: INT64
USE fhash_key_base, ONLY: fhash_key_t
USE fhash_fnv, ONLY: fnv_1a
IMPLICIT NONE

PRIVATE
PUBLIC fhash_key_int64_t
PUBLIC fhash_key

!> Hash table key container
TYPE, EXTENDS(fhash_key_t) :: fhash_key_int64_t
  PRIVATE
  INTEGER(INT64) :: VALUE
CONTAINS
  PROCEDURE, PASS :: hash => key_hash_int64
  PROCEDURE, PASS :: equals => key_equal_int64
  PROCEDURE, PASS :: to_string => key_int64_to_string
END TYPE fhash_key_int64_t

INTERFACE fhash_key
  MODULE PROCEDURE :: key_from_int64
END INTERFACE fhash_key

CONTAINS

!> Check if two keys are equal
PURE FUNCTION key_equal_int64(key1, key2) RESULT(keys_equal)
  CLASS(fhash_key_int64_t), INTENT(in) :: key1
  CLASS(fhash_key_t), INTENT(in) :: key2
  LOGICAL :: keys_equal

  keys_equal = .FALSE.

  SELECT TYPE (k2 => key2)
  TYPE is (fhash_key_int64_t)
    IF (key1%VALUE == k2%VALUE) THEN
      keys_equal = .TRUE.
      RETURN
    END IF
  END SELECT

END FUNCTION key_equal_int64

!> Generate hash of key
PURE FUNCTION key_hash_int64(key) RESULT(hash)
  CLASS(fhash_key_int64_t), INTENT(in) :: key
  INTEGER(INT64) :: hash

  hash = fnv_1a(key%VALUE)

END FUNCTION key_hash_int64

!> Generate string representation of hash
PURE FUNCTION key_int64_to_string(key) RESULT(str)
  CLASS(fhash_key_int64_t), INTENT(in) :: key
  CHARACTER(:), ALLOCATABLE :: str

  ALLOCATE (CHARACTER(1024) :: str)
  WRITE (str, *) key%VALUE
  str = TRIM(str)

END FUNCTION key_int64_to_string

!> Create new key container from a scalar int64
FUNCTION key_from_int64(source) RESULT(key)
  INTEGER(INT64), INTENT(in) :: source
  TYPE(fhash_key_int64_t) :: key

  key%VALUE = source

END FUNCTION key_from_int64

END MODULE fhash_key_int64
