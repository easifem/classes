!> Implements an abstract type for hash keys
!>
MODULE fhash_key_base
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64
IMPLICIT NONE

PRIVATE
PUBLIC fhash_key_t

!> Abstract base type for defining hash keys
TYPE, ABSTRACT :: fhash_key_t
CONTAINS
  PROCEDURE(hash_proc), DEFERRED :: hash
  PROCEDURE(equality_proc), DEFERRED :: equals
  PROCEDURE(to_string_proc), DEFERRED :: to_string
  GENERIC, PUBLIC :: OPERATOR(==) => equals
END TYPE fhash_key_t

ABSTRACT INTERFACE

  PURE FUNCTION equality_proc(key1, key2) RESULT(keys_equal)
    IMPORT
    CLASS(fhash_key_t), INTENT(in) :: key1
    CLASS(fhash_key_t), INTENT(in) :: key2
    LOGICAL :: keys_equal
  END FUNCTION equality_proc

  PURE FUNCTION hash_proc(key) RESULT(hash)
    IMPORT
    CLASS(fhash_key_t), INTENT(in) :: key
    INTEGER(INT64) :: hash
  END FUNCTION hash_proc

  FUNCTION to_string_proc(key) RESULT(str)
    IMPORT
    CLASS(fhash_key_t), INTENT(in) :: key
    CHARACTER(:), ALLOCATABLE :: str
  END FUNCTION to_string_proc

END INTERFACE

END MODULE fhash_key_base
