!> Implements a concrete type for scalar int64 hash keys
!>
MODULE HashkeyInt64_Class
USE ISO_FORTRAN_ENV, ONLY: INT64
USE Hashkey_Class, ONLY: Hashkey_
USE fhash_fnv, ONLY: fnv_1a
IMPLICIT NONE

PRIVATE

PUBLIC :: HashkeyInt64_
PUBLIC :: Hashkey

!> Hash table key container
TYPE, EXTENDS(Hashkey_) :: HashkeyInt64_
  PRIVATE
  INTEGER(INT64) :: VALUE
CONTAINS
  PROCEDURE, PASS :: hash => key_hash_int64
  PROCEDURE, PASS :: equals => key_equal_int64
  PROCEDURE, PASS :: to_string => key_int64_to_string
END TYPE HashkeyInt64_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Hashkey
  MODULE PROCEDURE :: key_from_int64
END INTERFACE Hashkey

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!> Check if two keys are equal
PURE FUNCTION key_equal_int64(key1, key2) RESULT(keys_equal)
  CLASS(HashkeyInt64_), INTENT(in) :: key1
  CLASS(Hashkey_), INTENT(in) :: key2
  LOGICAL :: keys_equal

  keys_equal = .FALSE.

  SELECT TYPE (k2 => key2)
  TYPE is (HashkeyInt64_)
    IF (key1%VALUE == k2%VALUE) THEN
      keys_equal = .TRUE.
      RETURN
    END IF
  END SELECT

END FUNCTION key_equal_int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generate hash of key
PURE FUNCTION key_hash_int64(key) RESULT(hash)
  CLASS(HashkeyInt64_), INTENT(in) :: key
  INTEGER(INT64) :: hash

  hash = fnv_1a(key%VALUE)

END FUNCTION key_hash_int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generate string representation of hash
PURE FUNCTION key_int64_to_string(key) RESULT(str)
  CLASS(HashkeyInt64_), INTENT(in) :: key
  CHARACTER(:), ALLOCATABLE :: str

  ALLOCATE (CHARACTER(1024) :: str)
  WRITE (str, *) key%VALUE
  str = TRIM(str)

END FUNCTION key_int64_to_string

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Create new key container from a scalar int64
FUNCTION key_from_int64(source) RESULT(key)
  INTEGER(INT64), INTENT(in) :: source
  TYPE(HashkeyInt64_) :: key

  key%VALUE = source

END FUNCTION key_from_int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HashkeyInt64_Class
