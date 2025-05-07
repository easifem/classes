!> Implements a concrete type for 1D int64 array hash keys
!>

MODULE HashkeyInt64Vec_Class
USE ISO_FORTRAN_ENV, ONLY: INT64
USE Hashkey_Class, ONLY: Hashkey_
USE fhash_fnv, ONLY: fnv_1a
IMPLICIT NONE

PRIVATE

PUBLIC :: HashkeyInt64Vec_
PUBLIC :: Hashkey

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Hash table key container
TYPE, EXTENDS(Hashkey_) :: HashkeyInt64Vec_
  PRIVATE
  INTEGER(INT64), ALLOCATABLE :: VALUE(:)
CONTAINS
  PROCEDURE, PASS :: hash => key_hash_int64_1d
  PROCEDURE, PASS :: equals => key_equal_int64_1d
  PROCEDURE, PASS :: to_string => key_int64_1d_to_string
END TYPE HashkeyInt64Vec_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Hashkey
  MODULE PROCEDURE :: key_from_int64_1d
END INTERFACE Hashkey

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!> Check if two keys are equal
PURE FUNCTION key_equal_int64_1d(key1, key2) RESULT(keys_equal)
  CLASS(HashkeyInt64Vec_), INTENT(in) :: key1
  CLASS(Hashkey_), INTENT(in) :: key2
  LOGICAL :: keys_equal

  keys_equal = .FALSE.

  SELECT TYPE (k2 => key2)
  TYPE is (HashkeyInt64Vec_)
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

END FUNCTION key_equal_int64_1d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generate hash of key
PURE FUNCTION key_hash_int64_1d(key) RESULT(hash)
  CLASS(HashkeyInt64Vec_), INTENT(in) :: key
  INTEGER(INT64) :: hash

  hash = fnv_1a(key%VALUE)
END FUNCTION key_hash_int64_1d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generate string representation of hash
PURE FUNCTION key_int64_1d_to_string(key) RESULT(str)
  CLASS(HashkeyInt64Vec_), INTENT(in) :: key
  CHARACTER(:), ALLOCATABLE :: str

  ALLOCATE (CHARACTER(1024) :: str)
  WRITE (str, *) key%VALUE
  str = TRIM(str)
END FUNCTION key_int64_1d_to_string

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Create new key container from a scalar int64
FUNCTION key_from_int64_1d(source) RESULT(key)
  INTEGER(INT64), INTENT(in) :: source(:)
  TYPE(HashkeyInt64Vec_) :: key
  key%VALUE = source
END FUNCTION key_from_int64_1d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HashkeyInt64Vec_Class
