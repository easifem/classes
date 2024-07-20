!> Implements an abstract type for hash keys
!>
MODULE Hashkey_Class
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64
IMPLICIT NONE

PRIVATE

PUBLIC :: Hashkey_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Abstract base type for defining hash keys
TYPE, ABSTRACT :: Hashkey_
CONTAINS
  PROCEDURE(hash_proc), DEFERRED :: hash
  PROCEDURE(equality_proc), DEFERRED :: equals
  PROCEDURE(to_string_proc), DEFERRED :: to_string
  GENERIC, PUBLIC :: OPERATOR(==) => equals
END TYPE Hashkey_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE

  PURE FUNCTION equality_proc(key1, key2) RESULT(keys_equal)
    IMPORT
    CLASS(Hashkey_), INTENT(in) :: key1
    CLASS(Hashkey_), INTENT(in) :: key2
    LOGICAL :: keys_equal
  END FUNCTION equality_proc

  PURE FUNCTION hash_proc(key) RESULT(hash)
    IMPORT
    CLASS(Hashkey_), INTENT(in) :: key
    INTEGER(INT64) :: hash
  END FUNCTION hash_proc

  FUNCTION to_string_proc(key) RESULT(str)
    IMPORT
    CLASS(Hashkey_), INTENT(in) :: key
    CHARACTER(:), ALLOCATABLE :: str
  END FUNCTION to_string_proc

END INTERFACE

END MODULE Hashkey_Class
