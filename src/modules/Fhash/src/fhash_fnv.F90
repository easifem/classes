!> A module for Fowler–Noll–Vo (FNV) hashing
!>
!> Implements the FNV 1a algorithm for 32bit hashes
!>
!> Supports hashing of:
!>  - 32bit integers (scalar & 1D array)
!>  - 64bit integers (scalar & 1D array)
!>  - character(*), default kind
!>
!>  The lack of unsigned arithmetic in Fortran means that
!>   64bit arithmetic is needed to perform 32bit hashing.
!>  Hashes are therefore returned as int64.
!>
MODULE fhash_fnv
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64
USE ISO_C_BINDING, ONLY: C_CHAR
IMPLICIT NONE

PRIVATE
PUBLIC :: fnv_1a, hash_string

!> Starting seed
INTEGER(INT64), PARAMETER :: FNV_OFFSET_32 = 2166136261_INT64

!> Hashing prime
INTEGER(INT64), PARAMETER :: FNV_PRIME_32 = 16777619_INT64

!> Generic interface to perform hashing
!>
!> Usage:
!>```fortran
!> fnv_1a([seed],input)
!>```
!> where `input` is any of the supported types
INTERFACE fnv_1a
  MODULE PROCEDURE fnv_1a_char_scalar
  MODULE PROCEDURE fnv_1a_char_scalar_seed
  MODULE PROCEDURE fnv_1a_int32_scalar
  MODULE PROCEDURE fnv_1a_int32_scalar_seed
  MODULE PROCEDURE fnv_1a_int32_1d
  MODULE PROCEDURE fnv_1a_int32_1d_seed
  MODULE PROCEDURE fnv_1a_int64_scalar
  MODULE PROCEDURE fnv_1a_int64_scalar_seed
  MODULE PROCEDURE fnv_1a_int64_1d
  MODULE PROCEDURE fnv_1a_int64_1d_seed
END INTERFACE fnv_1a

CONTAINS

!> Hash a single default kind character variable
PURE FUNCTION fnv_1a_char_scalar(input) RESULT(hash)
  CHARACTER(*), INTENT(in) :: input
  INTEGER(INT64) :: hash

  hash = fnv_1a(FNV_OFFSET_32, input)

END FUNCTION fnv_1a_char_scalar

!> Hash a character(*) string of default kind
PURE FUNCTION fnv_1a_char_scalar_seed(seed, input) RESULT(hash)
  INTEGER(INT64), INTENT(in) :: seed
  CHARACTER(*), INTENT(in) :: input
  INTEGER(INT64) :: hash

  INTEGER :: i
  INTEGER(INT64) :: item

  hash = seed

  DO i = 1, LEN(input)
    item = TRANSFER([IACHAR(input(i:i), INT32), 0_INT32], item)
    hash = IEOR(hash, item) * fnv_prime_32
  END DO

END FUNCTION fnv_1a_char_scalar_seed

!> Hash a single 32bit integer
PURE FUNCTION fnv_1a_int32_scalar(input) RESULT(hash)
  INTEGER(INT32), INTENT(in) :: input
  INTEGER(INT64) :: hash

  hash = fnv_1a(FNV_OFFSET_32, input)

END FUNCTION fnv_1a_int32_scalar

!> Hash a single 32bit integer with a starting seed
PURE FUNCTION fnv_1a_int32_scalar_seed(seed, input) RESULT(hash)
  INTEGER(INT64), INTENT(in) :: seed
  INTEGER(INT32), INTENT(in) :: input
  INTEGER(INT64) :: hash

  CHARACTER(len=4, kind=C_CHAR) :: chars

  chars = TRANSFER(input, chars)

  hash = fnv_1a(seed, chars)

END FUNCTION fnv_1a_int32_scalar_seed

!> Hash a 1D array of 32bit integers
PURE FUNCTION fnv_1a_int32_1d(input) RESULT(hash)
  INTEGER(INT32), INTENT(in) :: input(:)
  INTEGER(INT64) :: hash

  hash = fnv_1a(FNV_OFFSET_32, input)

END FUNCTION fnv_1a_int32_1d

!> Hash a 1D array of 32bit integers with a starting seed
PURE FUNCTION fnv_1a_int32_1d_seed(seed, input) RESULT(hash)
  INTEGER(INT64), INTENT(in) :: seed
  INTEGER(INT32), INTENT(in) :: input(:)
  INTEGER(INT64) :: hash

  INTEGER :: i

  hash = seed
  DO i = 1, SIZE(input)
    hash = fnv_1a(hash, input(i))
  END DO

END FUNCTION fnv_1a_int32_1d_seed

!> Hash a single 64bit integer
PURE FUNCTION fnv_1a_int64_scalar(input) RESULT(hash)
  INTEGER(INT64), INTENT(in) :: input
  INTEGER(INT64) :: hash

  hash = fnv_1a(FNV_OFFSET_32, input)

END FUNCTION fnv_1a_int64_scalar

!> Hash a single 64bit integer with a starting seed
PURE FUNCTION fnv_1a_int64_scalar_seed(seed, input) RESULT(hash)
  INTEGER(INT64), INTENT(in) :: seed
  INTEGER(INT64), INTENT(in) :: input
  INTEGER(INT64) :: hash

  CHARACTER(len=8, kind=C_CHAR) :: chars

  chars = TRANSFER(input, chars)

  hash = fnv_1a(seed, chars)

END FUNCTION fnv_1a_int64_scalar_seed

!> Hash a 1D array of 64bit integers
PURE FUNCTION fnv_1a_int64_1d(input) RESULT(hash)
  INTEGER(INT64), INTENT(in) :: input(:)
  INTEGER(INT64) :: hash

  hash = fnv_1a(FNV_OFFSET_32, input)

END FUNCTION fnv_1a_int64_1d

!> Hash a 1D array of 64bit integers with a starting seed
PURE FUNCTION fnv_1a_int64_1d_seed(seed, input) RESULT(hash)
  INTEGER(INT64), INTENT(in) :: seed
  INTEGER(INT64), INTENT(in) :: input(:)
  INTEGER(INT64) :: hash

  INTEGER :: i

  hash = seed
  DO i = 1, SIZE(input)
    hash = fnv_1a(hash, input(i))
  END DO

END FUNCTION fnv_1a_int64_1d_seed

!> Help fcn to convert hash to hex representation
FUNCTION hash_string(hash_value) RESULT(str)
  INTEGER(INT64), INTENT(in) :: hash_value
  CHARACTER(:), ALLOCATABLE :: str

  ALLOCATE (CHARACTER(len=10) :: str)
  WRITE (str, '(Z0)') INT(hash_value, INT32)

END FUNCTION hash_string

END MODULE fhash_fnv
