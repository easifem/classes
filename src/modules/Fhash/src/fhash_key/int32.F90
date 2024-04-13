!> Implements a concrete type for scalar int32 hash keys
!>
MODULE fhash_key_int32
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64
USE fhash_key_base, ONLY: fhash_key_t
USE fhash_fnv, ONLY: fnv_1a
IMPLICIT NONE

PRIVATE
PUBLIC fhash_key_int32_t
PUBLIC fhash_key

!> Hash table key container
TYPE, EXTENDS(fhash_key_t) :: fhash_key_int32_t
  PRIVATE
  INTEGER(INT32) :: VALUE
CONTAINS
  PROCEDURE, PASS :: hash => key_hash_int32
  PROCEDURE, PASS :: equals => key_equal_int32
  PROCEDURE, PASS :: to_string => key_int32_to_string
END TYPE fhash_key_int32_t

INTERFACE fhash_key
  MODULE PROCEDURE :: key_from_int32
END INTERFACE fhash_key

CONTAINS

!> Check if two keys are equal
PURE FUNCTION key_equal_int32(key1, key2) RESULT(keys_equal)
  CLASS(fhash_key_int32_t), INTENT(in) :: key1
  CLASS(fhash_key_t), INTENT(in) :: key2
  LOGICAL :: keys_equal

  keys_equal = .FALSE.

  SELECT TYPE (k2 => key2)
  TYPE is (fhash_key_int32_t)
    IF (key1%VALUE == k2%VALUE) THEN
      keys_equal = .TRUE.
      RETURN
    END IF
  END SELECT

END FUNCTION key_equal_int32

!> Generate hash of key
PURE FUNCTION key_hash_int32(key) RESULT(hash)
  CLASS(fhash_key_int32_t), INTENT(in) :: key
  INTEGER(INT64) :: hash

  hash = fnv_1a(key%VALUE)

END FUNCTION key_hash_int32

!> Generate string representation of hash
PURE FUNCTION key_int32_to_string(key) RESULT(str)
  CLASS(fhash_key_int32_t), INTENT(in) :: key
  CHARACTER(:), ALLOCATABLE :: str

  ALLOCATE (CHARACTER(1024) :: str)
  WRITE (str, *) key%VALUE
  str = TRIM(str)

END FUNCTION key_int32_to_string

!> Create new key container from a scalar int32
FUNCTION key_from_int32(source) RESULT(key)
  INTEGER(INT32), INTENT(in) :: source
  TYPE(fhash_key_int32_t) :: key

  key%VALUE = source

END FUNCTION key_from_int32

END MODULE fhash_key_int32
