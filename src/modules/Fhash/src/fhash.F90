! fhash module is taken from
! https://github.com/LKedward/fhash

MODULE fhash
USE fhash_tbl, ONLY: fhash_tbl_t
USE fhash_tbl_iter, ONLY: fhash_iter_t
USE fhash_key_base, ONLY: fhash_key_t
USE fhash_key_char, ONLY: fhash_key_char_t, fhash_key
USE fhash_key_int32, ONLY: fhash_key_int32_t, fhash_key
USE fhash_key_int64, ONLY: fhash_key_int64_t, fhash_key
USE fhash_key_int32_1d, ONLY: fhash_key_int32_1d_t, fhash_key
USE fhash_key_int64_1d, ONLY: fhash_key_int64_1d_t, fhash_key
IMPLICIT NONE
END MODULE fhash
