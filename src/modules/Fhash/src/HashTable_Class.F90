! fhash module i taken from
! https://github.com/LKedward/fhash
!
! I  have modified the naming convention which adhere with the naming
! convention of EASIFEM
!
! I have renamed the HashTable_ to HashTable_

MODULE HashTable_Class
USE ISO_FORTRAN_ENV, ONLY: INT32, INT64, sp => REAL32, dp => REAL64
USE HashDataContainer_Class, ONLY: HashDataContainer_, HashDataContainer
USE fhash_sll, ONLY: HashTableNode_, sll_find_in, sll_push_node, &
                     node_depth, sll_remove

USE Hashkey_Class, ONLY: Hashkey_

IMPLICIT NONE

PRIVATE

PUBLIC :: HashTable_

!> This condition should be unreachable by the public interface
INTEGER, PARAMETER, PUBLIC :: FHASH_INTERNAL_ERROR = -4

!> Error flag for operating on an unallocated table
INTEGER, PARAMETER, PUBLIC :: FHASH_EMPTY_TABLE = -3

!> Error flag for when retrieved data-type does not
!>  match that expected by the invoked getter function
!>  (`get_int32`,`get_int63`,`get_float`,'get_double`,`get_char`)
INTEGER, PARAMETER, PUBLIC :: FHASH_FOUND_WRONG_TYPE = -2

!> Error flag for when specified key is not found in the hash table
INTEGER, PARAMETER, PUBLIC :: FHASH_KEY_NOT_FOUND = -1

!> Default allocation size
INTEGER, PARAMETER :: FHASH_DEFAULT_ALLOCATION = 127

!----------------------------------------------------------------------------
!                                                                HashTable_
!----------------------------------------------------------------------------

TYPE HashTable_

  TYPE(HashTableNode_), ALLOCATABLE :: buckets(:)

CONTAINS
  PRIVATE

  PROCEDURE, PUBLIC :: ALLOCATE => fhash_tbl_allocate
  PROCEDURE, PUBLIC :: unset => fhash_tbl_unset
  PROCEDURE, PUBLIC :: check_key => fhash_tbl_check_key
  PROCEDURE, PUBLIC :: stats => fhash_tbl_stats

  PROCEDURE :: fhash_tbl_set_scalar
  GENERIC, PUBLIC :: set => fhash_tbl_set_scalar

  PROCEDURE :: fhash_tbl_set_scalar_ptr
  GENERIC, PUBLIC :: set_ptr => fhash_tbl_set_scalar_ptr

  PROCEDURE :: fhash_tbl_get_int32, fhash_tbl_get_int64
  PROCEDURE :: fhash_tbl_get_float, fhash_tbl_get_double
  PROCEDURE :: fhash_tbl_get_char, fhash_tbl_get_logical
  PROCEDURE :: fhash_tbl_get_data, fhash_tbl_get_raw

  GENERIC, PUBLIC :: get => fhash_tbl_get_int32, &
    fhash_tbl_get_int64, &
    fhash_tbl_get_float, &
    fhash_tbl_get_double, &
    fhash_tbl_get_char, &
    fhash_tbl_get_logical, &
    fhash_tbl_get_data, &
    fhash_tbl_get_raw

  PROCEDURE :: fhash_tbl_get_int32_ptr, fhash_tbl_get_int64_ptr
  PROCEDURE :: fhash_tbl_get_float_ptr, fhash_tbl_get_double_ptr
  PROCEDURE :: fhash_tbl_get_char_ptr, fhash_tbl_get_logical_ptr
  PROCEDURE :: fhash_tbl_get_raw_ptr

  GENERIC, PUBLIC :: get_ptr => fhash_tbl_get_int32_ptr, &
    fhash_tbl_get_int64_ptr, &
    fhash_tbl_get_float_ptr, &
    fhash_tbl_get_double_ptr, &
    fhash_tbl_get_char_ptr, &
    fhash_tbl_get_logical_ptr

  GENERIC, PUBLIC :: get_raw_ptr => fhash_tbl_get_raw_ptr

  PROCEDURE, PUBLIC :: DEALLOCATE => fhash_tbl_deallocate

  FINAL :: fhash_tbl_cleanup

END TYPE HashTable_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Allocate hash table
SUBROUTINE fhash_tbl_allocate(tbl, size)

  !> Table object to allocate
  CLASS(HashTable_), INTENT(inout) :: tbl

  !> Number of buckets in hash table
  !> If ommited, `tbl` is allocated with `FHASH_DEFAULT_ALLOCATION`
  INTEGER, INTENT(in), OPTIONAL :: size

  IF (PRESENT(size)) THEN
    ALLOCATE (tbl%buckets(size))
  ELSE
    ALLOCATE (tbl%buckets(FHASH_DEFAULT_ALLOCATION))
  END IF

END SUBROUTINE fhash_tbl_allocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Finalizer for HashTable_
SUBROUTINE fhash_tbl_deallocate(tbl)
  !> Table object to allocate
  CLASS(HashTable_), INTENT(INOUT) :: tbl

  INTEGER :: i

  IF (.NOT. ALLOCATED(tbl%buckets)) RETURN

  DO i = 1, SIZE(tbl%buckets)

    CALL sll_clean(tbl%buckets(i))

  END DO

END SUBROUTINE fhash_tbl_deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE fhash_tbl_cleanup(tbl)
  !> Table object to allocate
  TYPE(HashTable_), INTENT(inout) :: tbl
  CALL tbl%DEALLOCATE()
END SUBROUTINE fhash_tbl_cleanup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Unset a value in the table
!>
SUBROUTINE fhash_tbl_unset(tbl, key, stat)

  !> Hash table object
  CLASS(HashTable_), INTENT(inout) :: tbl

  !> Key to remove
  CLASS(Hashkey_), INTENT(in) :: key

  !> Status flag. Zero if successful.
  !> Unsuccessful: FHASH_EMPTY_TABLE | `FHASH_KEY_NOT_FOUND`
  INTEGER, INTENT(out), OPTIONAL :: stat

  INTEGER :: index
  LOGICAL :: found

  IF (PRESENT(stat)) stat = 0

  IF (.NOT. ALLOCATED(tbl%buckets)) THEN
    IF (PRESENT(stat)) stat = FHASH_EMPTY_TABLE
    RETURN
  END IF

  index = MODULO(key%hash(), SIZE(tbl%buckets, kind=INT64)) + 1
  CALL sll_remove(tbl%buckets(index), key, found)

  IF (PRESENT(stat)) stat = MERGE(0, FHASH_KEY_NOT_FOUND, found)

END SUBROUTINE fhash_tbl_unset

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Check if key exists in table
SUBROUTINE fhash_tbl_check_key(tbl, key, stat)

  !> Hash table object
  CLASS(HashTable_), INTENT(in) :: tbl

  !> Key to retrieve
  CLASS(Hashkey_), INTENT(in) :: key

  !> Status flag. Zero if key is found.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | `FHASH_KEY_NOT_FOUND`
  INTEGER, INTENT(out) :: stat

  INTEGER :: index
  LOGICAL :: found
  TYPE(HashDataContainer_), POINTER :: DATA

  IF (.NOT. ALLOCATED(tbl%buckets)) THEN
    stat = FHASH_EMPTY_TABLE
    RETURN
  END IF

  stat = 0

  index = MODULO(key%hash(), SIZE(tbl%buckets, kind=INT64)) + 1

  CALL sll_find_in(tbl%buckets(index), key, DATA, found)

  stat = MERGE(0, FHASH_KEY_NOT_FOUND, found)

  RETURN

END SUBROUTINE fhash_tbl_check_key

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get stats about the hash table
subroutine fhash_tbl_stats(tbl,num_buckets,num_items,num_collisions,max_depth)

  !> Hash table object
  CLASS(HashTable_), INTENT(in) :: tbl

  !> Number of buckets allocated in table
  INTEGER, INTENT(out), OPTIONAL :: num_buckets

  !> Number of key-value pairs stored in table
  INTEGER, INTENT(out), OPTIONAL :: num_items

  !> Number of hash collisions
  INTEGER, INTENT(out), OPTIONAL :: num_collisions

  !> Maximum depth of bucket in table
  INTEGER, INTENT(out), OPTIONAL :: max_depth

  INTEGER :: i, depth

  ! Initialise stats
  IF (PRESENT(num_items)) num_items = 0
  IF (PRESENT(num_collisions)) num_collisions = 0
  IF (PRESENT(max_depth)) max_depth = 0
  IF (PRESENT(num_buckets)) num_buckets = 0

  IF (.NOT. ALLOCATED(tbl%buckets)) RETURN

  IF (PRESENT(num_buckets)) THEN
    num_buckets = SIZE(tbl%buckets)
  END IF

  DO i = 1, SIZE(tbl%buckets)

    depth = node_depth(tbl%buckets(i))

    IF (PRESENT(num_items)) num_items = num_items + depth

    IF (PRESENT(num_collisions)) num_collisions = num_collisions + &
                                                MERGE(depth - 1, 0, depth > 1)

    IF (PRESENT(max_depth)) max_depth = MAX(max_depth, depth)

  END DO

END SUBROUTINE fhash_tbl_stats

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set/update a polymorphic scalar value in the table
!>
!> `tbl` is allocated with default size if not already allocated
SUBROUTINE fhash_tbl_set_scalar(tbl, key, VALUE, POINTER)

  !> Hash table object
  CLASS(HashTable_), INTENT(inout) :: tbl

  !> Key to set/update
  CLASS(Hashkey_), INTENT(in) :: key

  !> Value for key
  CLASS(*), INTENT(in), TARGET :: VALUE

  !> If .true., store a pointer to value instead of copying
  LOGICAL, INTENT(in), OPTIONAL :: POINTER

  INTEGER :: index

  IF (.NOT. ALLOCATED(tbl%buckets)) CALL fhash_tbl_allocate(tbl)

  index = MODULO(key%hash(), SIZE(tbl%buckets, kind=INT64)) + 1

  CALL sll_push_node(tbl%buckets(index), key, VALUE, POINTER)

END SUBROUTINE fhash_tbl_set_scalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper routine for generic 'set_ptr'
!>
!> `tbl` is allocated with default size if not already allocated
SUBROUTINE fhash_tbl_set_scalar_ptr(tbl, key, VALUE)

  !> Hash table object
  CLASS(HashTable_), INTENT(inout) :: tbl

  !> Key to set/update
  CLASS(Hashkey_), INTENT(in) :: key

  !> Value for key
  CLASS(*), INTENT(in), TARGET :: VALUE

  CALL fhash_tbl_set_scalar(tbl, key, VALUE, POINTER=.TRUE.)

END SUBROUTINE fhash_tbl_set_scalar_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Retrieve data container from the hash table
SUBROUTINE fhash_tbl_get_data(tbl, key, DATA, stat)

  !> Hash table object
  CLASS(HashTable_), INTENT(in) :: tbl

  !> Key to retrieve
  CLASS(Hashkey_), INTENT(in) :: key

  !> Copy of value retrieved for key
  TYPE(HashDataContainer_), POINTER :: DATA

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | `FHASH_KEY_NOT_FOUND`
  INTEGER, INTENT(out), OPTIONAL :: stat

  INTEGER :: index
  LOGICAL :: found

  IF (.NOT. ALLOCATED(tbl%buckets)) THEN
    IF (PRESENT(stat)) stat = FHASH_EMPTY_TABLE
    RETURN
  END IF

  IF (PRESENT(stat)) stat = 0

  index = MODULO(key%hash(), SIZE(tbl%buckets, kind=INT64)) + 1

  CALL sll_find_in(tbl%buckets(index), key, DATA, found)

  IF (.NOT. found) THEN

    IF (PRESENT(stat)) stat = FHASH_KEY_NOT_FOUND
    RETURN

  END IF

END SUBROUTINE fhash_tbl_get_data

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to retrieve a scalar intrinsic type value
subroutine fhash_tbl_get_intrinsic_scalar(tbl,key,i32,i64,r32,r64,char,raw,bool,stat)

  !> Hash table object
  CLASS(HashTable_), INTENT(in) :: tbl

  !> Key to retrieve
  CLASS(Hashkey_), INTENT(in) :: key

  !> Value to retrieve
  INTEGER(INT32), INTENT(out), OPTIONAL :: i32
  INTEGER(INT64), INTENT(out), OPTIONAL :: i64
  REAL(sp), INTENT(out), OPTIONAL :: r32
  REAL(dp), INTENT(out), OPTIONAL :: r64
  CHARACTER(:), ALLOCATABLE, INTENT(out), OPTIONAL :: char
  LOGICAL, INTENT(out), OPTIONAL :: bool
  CLASS(*), ALLOCATABLE, INTENT(out), OPTIONAL :: raw

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` |
  !>  `FHASH_FOUND_WRONG_TYPE` | `FHASH_KEY_NOT_FOUND`
  INTEGER, INTENT(out), OPTIONAL :: stat

  LOGICAL :: type_match
  INTEGER :: local_stat
  TYPE(HashDataContainer_), POINTER :: DATA

  CHARACTER(:), ALLOCATABLE :: char_temp

  IF (PRESENT(stat)) stat = 0

  CALL fhash_tbl_get_data(tbl, key, DATA, local_stat)

  IF (local_stat /= 0) THEN
    IF (PRESENT(stat)) stat = local_stat
    RETURN
  END IF

  IF (PRESENT(char)) THEN ! (Work-around for weird gfortran bug re char dummy)

    CALL DATA%get(i32, i64, r32, r64, char_temp, bool, raw, type_match)

    IF (type_match) char = char_temp

  ELSE

    CALL DATA%get(i32, i64, r32, r64, bool=bool, raw=raw, match=type_match)

  END IF

  IF (.NOT. type_match) THEN
    IF (PRESENT(stat)) stat = FHASH_FOUND_WRONG_TYPE
    RETURN
  END IF

END SUBROUTINE fhash_tbl_get_intrinsic_scalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to retrieve a scalar intrinsic type pointer
subroutine fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,i32,i64,r32,r64,char,bool,raw,stat)

  !> Hash table object
  CLASS(HashTable_), INTENT(in) :: tbl

  !> Key to retrieve
  CLASS(Hashkey_), INTENT(in) :: key

  !> Value to retrieve
  INTEGER(INT32), POINTER, INTENT(out), OPTIONAL :: i32
  INTEGER(INT64), POINTER, INTENT(out), OPTIONAL :: i64
  REAL(sp), POINTER, INTENT(out), OPTIONAL :: r32
  REAL(dp), POINTER, INTENT(out), OPTIONAL :: r64
  CHARACTER(:), POINTER, INTENT(out), OPTIONAL :: char
  LOGICAL, POINTER, INTENT(out), OPTIONAL :: bool
  CLASS(*), POINTER, INTENT(out), OPTIONAL :: raw

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` |
  !>  `FHASH_FOUND_WRONG_TYPE` | `FHASH_KEY_NOT_FOUND`
  INTEGER, INTENT(out), OPTIONAL :: stat

  LOGICAL :: type_match
  INTEGER :: local_stat
  TYPE(HashDataContainer_), POINTER :: DATA

  CHARACTER(:), POINTER :: char_temp

  IF (PRESENT(stat)) stat = 0

  CALL fhash_tbl_get_data(tbl, key, DATA, local_stat)

  IF (local_stat /= 0) THEN
    IF (PRESENT(stat)) stat = local_stat
    RETURN
  END IF

  IF (PRESENT(char)) THEN ! (Work-around for weird gfortran bug re char dummy)

    CALL DATA%get_ptr(i32, i64, r32, r64, char_temp, bool, raw, type_match)

    IF (type_match) char => char_temp

  ELSE

   CALL DATA%get_ptr(i32, i64, r32, r64, bool=bool, raw=raw, match=type_match)

  END IF

  IF (.NOT. type_match) THEN
    IF (PRESENT(stat)) stat = FHASH_FOUND_WRONG_TYPE
    RETURN
  END IF

END SUBROUTINE fhash_tbl_get_intrinsic_scalar_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar int32 value
SUBROUTINE fhash_tbl_get_int32(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  INTEGER(INT32), INTENT(out) :: VALUE !! Output value
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar(tbl, key, i32=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar int64 value
SUBROUTINE fhash_tbl_get_int64(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  INTEGER(INT64), INTENT(out) :: VALUE !! Output value
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar(tbl, key, i64=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar float value
SUBROUTINE fhash_tbl_get_float(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  REAL(sp), INTENT(out) :: VALUE !! Output value
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar(tbl, key, r32=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_float

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar double value
SUBROUTINE fhash_tbl_get_double(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  REAL(dp), INTENT(out) :: VALUE !! Output value
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar(tbl, key, r64=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_double

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar character value
SUBROUTINE fhash_tbl_get_char(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  CHARACTER(:), ALLOCATABLE, INTENT(out) :: VALUE !! Output value
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar(tbl, key, char=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_char

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar logical value
SUBROUTINE fhash_tbl_get_logical(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  LOGICAL, INTENT(out) :: VALUE !! Output value
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar(tbl, key, bool=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_logical

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve underlying polymorhpic scalar value
SUBROUTINE fhash_tbl_get_raw(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  CLASS(*), ALLOCATABLE, INTENT(out) :: VALUE !! Output value
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar(tbl, key, raw=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_raw

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar int32 value
SUBROUTINE fhash_tbl_get_int32_ptr(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  INTEGER(INT32), POINTER, INTENT(out) :: VALUE !! Output value pointer
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar_ptr(tbl, key, i32=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_int32_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar int64 value
SUBROUTINE fhash_tbl_get_int64_ptr(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  INTEGER(INT64), POINTER, INTENT(out) :: VALUE !! Output value pointer
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar_ptr(tbl, key, i64=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_int64_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar float value
SUBROUTINE fhash_tbl_get_float_ptr(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  REAL(sp), POINTER, INTENT(out) :: VALUE !! Output value pointer
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar_ptr(tbl, key, r32=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_float_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar double value
SUBROUTINE fhash_tbl_get_double_ptr(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  REAL(dp), POINTER, INTENT(out) :: VALUE !! Output value pointer
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar_ptr(tbl, key, r64=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_double_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar character value
SUBROUTINE fhash_tbl_get_char_ptr(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  CHARACTER(:), POINTER, INTENT(out) :: VALUE !! Output value pointer
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar_ptr(tbl, key, char=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_char_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve a scalar logical value
SUBROUTINE fhash_tbl_get_logical_ptr(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  LOGICAL, POINTER, INTENT(out) :: VALUE !! Output value pointer
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar_ptr(tbl, key, bool=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_logical_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get wrapper to directly retrieve underlying polymorhpic scalar value
SUBROUTINE fhash_tbl_get_raw_ptr(tbl, key, VALUE, stat)
  CLASS(HashTable_), INTENT(in) :: tbl !! Hash table object
  CLASS(Hashkey_), INTENT(in) :: key !! Key to retrieve
  CLASS(*), POINTER, INTENT(out) :: VALUE !! Output value
  INTEGER, INTENT(out), OPTIONAL :: stat !! Status flag. Zero if successful.

  CALL fhash_tbl_get_intrinsic_scalar_ptr(tbl, key, raw=VALUE, stat=stat)

END SUBROUTINE fhash_tbl_get_raw_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HashTable_Class
