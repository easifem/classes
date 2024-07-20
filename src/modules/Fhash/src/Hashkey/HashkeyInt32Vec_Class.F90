!> Implements a concrete type for 1D int32 array hash keys
!>
MODULE HashkeyInt32Vec_Class
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64
USE Hashkey_Class, ONLY: Hashkey_
USE fhash_fnv, ONLY: fnv_1a
IMPLICIT NONE

PRIVATE
PUBLIC :: HashkeyInt32Vec_
PUBLIC :: Hashkey

!> Hash table key container
TYPE, EXTENDS(Hashkey_) :: HashkeyInt32Vec_
  PRIVATE
  INTEGER(INT32), ALLOCATABLE :: VALUE(:)
CONTAINS
  PROCEDURE, PASS :: hash => key_hash_int32_1d
  PROCEDURE, PASS :: equals => key_equal_int32_1d
  PROCEDURE, PASS :: to_string => key_int32_1d_to_string
END TYPE HashkeyInt32Vec_

INTERFACE Hashkey
  MODULE PROCEDURE :: key_from_int32_1d
END INTERFACE Hashkey

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!> Check if two keys are equal
PURE FUNCTION key_equal_int32_1d(key1, key2) RESULT(keys_equal)
  CLASS(HashkeyInt32Vec_), INTENT(in) :: key1
  CLASS(Hashkey_), INTENT(in) :: key2
  LOGICAL :: keys_equal

  keys_equal = .FALSE.

  SELECT TYPE (k2 => key2)
  TYPE is (HashkeyInt32Vec_)
    IF (.NOT. (ALLOCATED(key1%VALUE) .AND. ALLOCATED(k2%VALUE))) THEN
      RETURN
    END IF
    IF (SIZE(key1%VALUE) /= SIZE(k2%VALUE)) THEN
      RETURN
    END IF
    IF (ALL(key1%VALUE == k2%VALUE)) THEN
      keys_equal = .TRUE.
      RETURN
    END IF
  END SELECT

END FUNCTION key_equal_int32_1d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generate hash of key
PURE FUNCTION key_hash_int32_1d(key) RESULT(hash)
  CLASS(HashkeyInt32Vec_), INTENT(in) :: key
  INTEGER(INT64) :: hash

  hash = fnv_1a(key%VALUE)

END FUNCTION key_hash_int32_1d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generate string representation of hash
PURE FUNCTION key_int32_1d_to_string(key) RESULT(str)
  CLASS(HashkeyInt32Vec_), INTENT(in) :: key
  CHARACTER(:), ALLOCATABLE :: str

  ALLOCATE (CHARACTER(1024) :: str)
  WRITE (str, *) key%VALUE
  str = TRIM(str)

END FUNCTION key_int32_1d_to_string

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Create new key container from a scalar int32
FUNCTION key_from_int32_1d(source) RESULT(key)
  INTEGER(INT32), INTENT(in) :: source(:)
  TYPE(HashkeyInt32Vec_) :: key

  key%VALUE = source

END FUNCTION key_from_int32_1d

END MODULE HashkeyInt32Vec_Class
