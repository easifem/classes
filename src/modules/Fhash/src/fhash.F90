! fhash module is taken from
! https://github.com/LKedward/fhash
!
! I  have modified the naming convention which adhere with the naming
! convention of EASIFEM

MODULE fhash
USE HashTable_Class, ONLY: HashTable_
USE HashTableIter_Class, ONLY: HashTableIter_
USE Hashkey_Class, ONLY: Hashkey_
USE HashkeyChar_Class, ONLY: HashkeyChar_, Hashkey
USE HashkeyInt32_Class, ONLY: HashkeyInt32_, Hashkey
USE HashkeyInt64_Class, ONLY: HashkeyInt64_, Hashkey
USE HashkeyInt32Vec_Class, ONLY: HashkeyInt32Vec_, Hashkey
USE HashkeyInt64Vec_Class, ONLY: HashkeyInt64Vec_, Hashkey
IMPLICIT NONE
END MODULE fhash
