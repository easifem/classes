!> Implements a concrete type for scalar int32 hash keys
!>
MODULE HashkeyChar_Class
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64
USE Hashkey_Class, ONLY: Hashkey_
USE fhash_fnv, ONLY: fnv_1a
IMPLICIT NONE

PRIVATE

PUBLIC :: HashkeyChar_
PUBLIC :: Hashkey

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Hash table key container
TYPE, EXTENDS(Hashkey_) :: HashkeyChar_
  PRIVATE
  CHARACTER(:), ALLOCATABLE :: VALUE
CONTAINS
  PROCEDURE, PASS :: hash => key_hash_char
  PROCEDURE, PASS :: equals => key_equal_char
  PROCEDURE, PASS :: to_string => key_char_to_string
END TYPE HashkeyChar_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Hashkey
  MODULE PROCEDURE :: key_from_char
END INTERFACE Hashkey

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Check if two keys are equal
PURE FUNCTION key_equal_char(key1, key2) RESULT(keys_equal)
  CLASS(HashkeyChar_), INTENT(in) :: key1
  CLASS(Hashkey_), INTENT(in) :: key2
  LOGICAL :: keys_equal

  keys_equal = .FALSE.

  SELECT TYPE (k2 => key2)
  TYPE is (HashkeyChar_)
    IF (ALLOCATED(key1%VALUE) .AND. ALLOCATED(k2%VALUE)) THEN
      IF (key1%VALUE == k2%VALUE) THEN
        keys_equal = .TRUE.
        RETURN
      END IF
    END IF
  END SELECT

END FUNCTION key_equal_char

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generate hash of key
PURE FUNCTION key_hash_char(key) RESULT(hash)
  CLASS(HashkeyChar_), INTENT(in) :: key
  INTEGER(INT64) :: hash

  hash = fnv_1a(key%VALUE)

END FUNCTION key_hash_char

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generate string representation of hash
FUNCTION key_char_to_string(key) RESULT(str)
  CLASS(HashkeyChar_), INTENT(in) :: key
  CHARACTER(:), ALLOCATABLE :: str

  str = key%VALUE

END FUNCTION key_char_to_string

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Create new key container from a scalar int32
FUNCTION key_from_char(source) RESULT(key)
  CHARACTER(*), INTENT(in) :: source
  TYPE(HashkeyChar_) :: key

  key%VALUE = source

END FUNCTION key_from_char

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HashkeyChar_Class
