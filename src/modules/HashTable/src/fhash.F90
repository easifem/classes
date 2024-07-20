#ifdef __DO_NOT_PREPROCESS_DOC__
! Hash table implementation imitating to GCC STL (with singly linked list).
! DO NOT COMPILE THIS TEMPLATE FILE DIRECTLY.
! Use a wrapper module and include this file instead, e.g. fhash_modules.f90.
! Remove is not implemented since not needed currently.
!
! #define                         | meaning
! --------------------------------+-----------------------------------------------------
! SHORTNAME <Name>                | (OPTIONAL) The name of the type this FHASH table is
!                                 | for. If set, it overrides all settings that have
!                                 | have possibly been made for FHASH_MODULE_NAME,
!                                 | FHASH_TYPE_NAME and FHASH_TYPE_ITERATOR_NAME.
!                                 |
! FHASH_MODULE_NAME <Name>        | The name of the module that encapsulates the FHASH
!                                 | types and FUNCTIONality
! FHASH_TYPE_NAME <Name>          | The name of the actual FHASH type
! FHASH_TYPE_ITERATOR_NAME <Name> | The name of the FHASH type that can iterate through
!                                 | the whole FHASH
!                                 |
! KEY_USE <use stmt>              | (OPTIONAL) A use statement that is required to use
!                                 | a specific type as a key for the FHASH
! KEY_TYPE <typename>             | The type of the keys. May require KEY_USE to be
!                                 | accessible.
!                                 |
! VALUE_USE <use stmt>            | (OPTIONAL) A use statement that is required to use
!                                 | a specific type as a value for the FHASH
! VALUE_TYPE <typename>           | The type of the values. May require VALUE_USE to be
!                                 | accessible.
!                                 |
! VALUE_VALUE                     | Flag indicating that the values in FHASH are value
!                                 | values. This is the default. (see VALUE_POINTER)
! VALUE_POINTER                   | Flag indicating that the values in FHASH are value
!                                 | pointers.
! VALUE_ASSIGNMENT                | (internal) The assignment operator, DO not set it
!                                 | anywhere, it is configured based on VALUE_VALUE or
!                                 | VALUE_POINTER
#endif

#ifdef SHORTNAME
#undef FHASH_MODULE_NAME
#undef FHASH_TYPE_NAME
#undef FHASH_TYPE_ITERATOR_NAME

#ifdef __GFORTRAN__
#define PASTE(a) a
#define CONCAT(a,b) PASTE(a)b
#else
#define PASTE(a,b) a ## b
#define CONCAT(a,b) PASTE(a,b)
#endif
#define FHASH_MODULE_NAME CONCAT(fhash_module__,SHORTNAME)
#define FHASH_TYPE_NAME CONCAT(fhash_type__,SHORTNAME)
#define FHASH_TYPE_ITERATOR_NAME CONCAT(fhash_type_iterator__,SHORTNAME)
#endif

#undef VALUE_ASSIGNMENT
#ifndef VALUE_VALUE
#ifndef VALUE_POINTER
#define VALUE_VALUE
#endif
#endif

#ifdef VALUE_POINTER
#define VALUE_ASSIGNMENT =>
#else
#define VALUE_ASSIGNMENT =
#endif

!> author: Vikas Sharma, Ph. D.
! summary:         Hash table in fortran

MODULE FHASH_MODULE_NAME

#undef FHASH_MODULE_NAME

#ifdef KEY_USE
KEY_USE
#undef KEY_USE

#endif

#ifdef VALUE_USE
VALUE_USE
#undef VALUE_USE

#endif

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE
PRIVATE
PUBLIC :: FHASH_TYPE_NAME
PUBLIC :: FHASH_TYPE_ITERATOR_NAME

TYPE kv_type
  KEY_TYPE :: key
  VALUE_TYPE :: VALUE
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE node_type
  TYPE(kv_type), ALLOCATABLE :: kv
  TYPE(node_type), POINTER :: next => NULL()

CONTAINS
  ! If kv is not ALLOCATED, ALLOCATE and set to the key, value passed in.
  ! If key is PRESENT and the same as the key passed in, overwrite the value.
  ! Otherwise, defer to the next node (ALLOCATE IF not ALLOCATED)
  PROCEDURE :: node_set

  ! If kv is not ALLOCATED, fail and return 0.
  ! If key is PRESENT and the same as the key passed in, return the value in kv.
  ! If next pointer is ASSOCIATED, delegate to it.
  ! Otherwise, fail and return 0.
  PROCEDURE :: node_get

  ! If kv is not ALLOCATED, fail and return
  ! If key is PRESENT and node is first in bucket, set first node in bucket to
  !   the next node of first. Return success
  ! If key is PRESENT and the node is another member of the linked list, link the
  !   previous node's next node to this node's next node, DEALLOCATE this node,
  !   return success
  ! Otherwise, fail and return 0
  PROCEDURE :: node_remove

  ! Deallocate kv is ALLOCATED.
  ! Call the clear method of the next node IF the next pointer ASSOCIATED.
  ! Deallocate and NULLIFY the next pointer.
  PROCEDURE :: node_clear

  ! Return the length of the linked list start from the current node.
  PROCEDURE :: node_depth
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE FHASH_TYPE_NAME
  PRIVATE

  INTEGER(I4B) :: n_buckets = 0
  INTEGER(I4B) :: n_keys = 0
  TYPE(node_type), ALLOCATABLE :: buckets(:)

CONTAINS
  ! Returns the number of buckets.
  PROCEDURE, PUBLIC :: BucketSize => obj_BucketSize

  ! Return the number of collisions.
  PROCEDURE, PUBLIC :: GetCollisions => obj_GetCollisions

  ! Reserve certain number of buckets.
  PROCEDURE, PUBLIC :: Initiate => obj_Initiate

  ! Returns number of keys.
  PROCEDURE, PUBLIC :: GetTotalKeys => obj_GetTotalKeys
  GENERIC, PUBLIC :: Size => GetTotalKeys

  ! Set the value at a given a key.
  PROCEDURE, PUBLIC :: Set => obj_Set

  ! Get the value at the given key.
  PROCEDURE, PUBLIC :: Get => obj_Get

  ! Remove the value with the given key.
  PROCEDURE, PUBLIC :: Remove => obj_Remove

  ! Clear all the ALLOCATED memory (must be called to prevent memory leak).
  PROCEDURE, PUBLIC :: DEALLOCATE => obj_Deallocate
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE FHASH_TYPE_ITERATOR_NAME
  PRIVATE
  INTEGER(I4B) :: bucket_id
  TYPE(node_type), POINTER :: node_ptr => NULL()
  TYPE(FHASH_TYPE_NAME), POINTER :: fhash_ptr => NULL()

CONTAINS
  ! Set the iterator to the beginning of a hash table.
  PROCEDURE, PUBLIC :: begin

  ! Get the key value of the next element and advance the iterator.
  PROCEDURE, PUBLIC :: next
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION obj_BucketSize(this)
  CLASS(FHASH_TYPE_NAME), INTENT(INOUT) :: this
  INTEGER(I4B) :: obj_BucketSize

  obj_BucketSize = this%n_buckets
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION obj_GetCollisions(this)
  CLASS(FHASH_TYPE_NAME), INTENT(INOUT) :: this
  INTEGER(I4B) :: obj_GetCollisions
  INTEGER(I4B) :: i

  obj_GetCollisions = 0
  DO i = 1, this%n_buckets
    obj_GetCollisions = obj_GetCollisions + node_depth(this%buckets(i)) - 1
  END DO
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

RECURSIVE FUNCTION node_depth(this) RESULT(depth)
  CLASS(node_type), INTENT(INOUT) :: this
  INTEGER(I4B) :: depth

  IF (.NOT. ASSOCIATED(this%next)) THEN
    depth = 1
  ELSE
    depth = 1 + node_depth(this%next)
  END IF
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_Initiate(this, n_buckets)
  CLASS(FHASH_TYPE_NAME), INTENT(INOUT) :: this
  INTEGER(I4B), INTENT(IN) :: n_buckets
  INTEGER(I4B), DIMENSION(29) :: sizes
  INTEGER(I4B) :: i

  IF (this%GetTotalKeys() > 0) STOP 'Cannot reserve when fhash is not empty.'

  sizes = (/5, 11, 23, 47, 97, 199, 409, 823, 1741, 3469, 6949, 14033, &
    & 28411, 57557, 116731, 236897, 480881, 976369, 1982627, 4026031, &
    & 8175383, 16601593, 33712729, 68460391, 139022417, 282312799, &
    & 573292817, 1164186217, 2147483647/)
  DO i = 1, SIZE(sizes)
    IF (sizes(i) >= n_buckets) THEN
      this%n_buckets = sizes(i)
      ALLOCATE (this%buckets(this%n_buckets))
      RETURN
    END IF
  END DO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION obj_GetTotalKeys(this)
  CLASS(FHASH_TYPE_NAME), INTENT(INOUT) :: this
  INTEGER(I4B) :: obj_GetTotalKeys

  obj_GetTotalKeys = this%n_keys
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_Set(this, key, VALUE)
  CLASS(FHASH_TYPE_NAME), INTENT(INOUT) :: this
  KEY_TYPE, INTENT(IN) :: key
  VALUE_TYPE, INTENT(IN) :: VALUE
  INTEGER(I4B) :: bucket_id
  LOGICAL(LGT) :: is_new

  bucket_id = MODULO(hash_value(key), this%n_buckets) + 1

  CALL this%buckets(bucket_id)%node_set(key, VALUE, is_new)

  IF (is_new) this%n_keys = this%n_keys + 1
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE node_set(this, key, VALUE, is_new)
  CLASS(node_type), INTENT(INOUT) :: this
  KEY_TYPE, INTENT(IN) :: key
  VALUE_TYPE, INTENT(IN) :: VALUE
  LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: is_new

  IF (.NOT. ALLOCATED(this%kv)) THEN
    ALLOCATE (this%kv)
    this%kv%key = key
    this%kv%VALUE VALUE_ASSIGNMENT VALUE
    IF (PRESENT(is_new)) is_new = .TRUE.
  ELSE IF (this%kv%key == key) THEN
    this%kv%VALUE VALUE_ASSIGNMENT VALUE
    IF (PRESENT(is_new)) is_new = .FALSE.
  ELSE
    IF (.NOT. ASSOCIATED(this%next)) ALLOCATE (this%next)
    CALL this%next%node_set(key, VALUE, is_new)
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_Get(this, key, VALUE, success)
  CLASS(FHASH_TYPE_NAME), INTENT(INOUT) :: this
  KEY_TYPE, INTENT(IN) :: key
  VALUE_TYPE, INTENT(OUT) :: VALUE
  LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: success
  INTEGER(I4B) :: bucket_id

  bucket_id = MODULO(hash_value(key), this%n_buckets) + 1
  CALL this%buckets(bucket_id)%node_get(key, VALUE, success)
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE node_get(this, key, VALUE, success)
  CLASS(node_type), INTENT(INOUT) :: this
  KEY_TYPE, INTENT(IN) :: key
  VALUE_TYPE, INTENT(OUT) :: VALUE
  LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: success

  IF (.NOT. ALLOCATED(this%kv)) THEN
    ! Not found. (Initial node in the bucket not set)
    IF (PRESENT(success)) success = .FALSE.
  ELSE IF (this%kv%key == key) THEN
    VALUE VALUE_ASSIGNMENT this%kv%VALUE
    IF (PRESENT(success)) success = .TRUE.
  ELSE IF (ASSOCIATED(this%next)) THEN
    CALL this%next%node_get(key, VALUE, success)
  ELSE
    IF (PRESENT(success)) success = .FALSE.
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_Remove(this, key, success)
  CLASS(FHASH_TYPE_NAME), INTENT(INOUT) :: this
  KEY_TYPE, INTENT(IN) :: key
  LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: success

  INTEGER(I4B) :: bucket_id
  TYPE(node_type) :: first
  LOGICAL(LGT) :: locSuccess

  bucket_id = MODULO(hash_value(key), this%n_buckets) + 1
  first = this%buckets(bucket_id)

  IF (ALLOCATED(first%kv)) THEN
    IF (first%kv%key == key) THEN
      IF (ASSOCIATED(first%next)) THEN
        this%buckets(bucket_id)%kv%key = this%buckets(bucket_id)%next%kv%key
          this%buckets(bucket_id)%kv%value VALUE_ASSIGNMENT this%buckets(bucket_id)%next%kv%value
        DEALLOCATE (first%next%kv)
        this%buckets(bucket_id)%next => this%buckets(bucket_id)%next%next
      ELSE
        DEALLOCATE (this%buckets(bucket_id)%kv)
      END IF
      locSuccess = .TRUE.
    ELSE
      CALL node_remove(first%next, key, locSuccess, first)
    END IF
  ELSE
    locSuccess = .FALSE.
  END IF

  IF (locSuccess) this%n_keys = this%n_keys - 1
  IF (PRESENT(success)) success = locSuccess

END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE node_remove(this, key, success, last)
  CLASS(node_type), INTENT(INOUT) :: this, last
  KEY_TYPE, INTENT(IN) :: key
  LOGICAL(LGT), INTENT(OUT) :: success

  IF (.NOT. ALLOCATED(this%kv)) THEN
    ! Not found. (Initial node in the bucket not set)
    success = .FALSE.
  ELSE IF (this%kv%key == key) THEN
    last%next => this%next
    NULLIFY (this%next)
    DEALLOCATE (this%kv)
    success = .TRUE.
  ELSE IF (ASSOCIATED(this%next)) THEN
    CALL this%next%node_remove(key, success, this)
  ELSE
    success = .FALSE.
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_Deallocate(this)
  CLASS(FHASH_TYPE_NAME), INTENT(INOUT) :: this
  INTEGER(I4B) :: i

  IF (.NOT. ALLOCATED(this%buckets)) RETURN

  DO i = 1, SIZE(this%buckets)
    IF (ASSOCIATED(this%buckets(i)%next)) THEN
      CALL this%buckets(i)%next%node_clear()
      DEALLOCATE (this%buckets(i)%next)
    END IF
  END DO
  DEALLOCATE (this%buckets)
  this%n_keys = 0
  this%n_buckets = 0
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE node_clear(this)
  CLASS(node_type), INTENT(INOUT) :: this

  IF (ASSOCIATED(this%next)) THEN
    CALL this%next%node_clear()
    DEALLOCATE (this%next)
    NULLIFY (this%next)
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE begin(this, fhash_target)
  CLASS(FHASH_TYPE_ITERATOR_NAME), INTENT(INOUT) :: this
  TYPE(FHASH_TYPE_NAME), TARGET, INTENT(IN) :: fhash_target

  this%bucket_id = 1
  this%node_ptr => fhash_target%buckets(1)
  this%fhash_ptr => fhash_target
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE next(this, key, VALUE, status)
  CLASS(FHASH_TYPE_ITERATOR_NAME), INTENT(INOUT) :: this
  KEY_TYPE, INTENT(OUT) :: key
  VALUE_TYPE, INTENT(OUT) :: VALUE
  INTEGER(I4B), OPTIONAL, INTENT(OUT) :: status

  DO

    IF (ASSOCIATED(this%node_ptr)) THEN
      IF (ALLOCATED(this%node_ptr%kv)) EXIT
    END IF

    IF (this%bucket_id < this%fhash_ptr%n_buckets) THEN

      this%bucket_id = this%bucket_id + 1
      this%node_ptr => this%fhash_ptr%buckets(this%bucket_id)

    ELSE

      IF (PRESENT(status)) status = -1

#ifdef VALUE_TYPE_INIT
      VALUE VALUE_ASSIGNMENT VALUE_TYPE_INIT
#endif

      RETURN

    END IF

  END DO

  key = this%node_ptr%kv%key

  VALUE VALUE_ASSIGNMENT this%node_ptr%kv%VALUE

  IF (PRESENT(status)) status = 0
  this%node_ptr => this%node_ptr%next

END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE

#undef KEY_TYPE
#undef VALUE_TYPE
#undef VALUE_TYPE_INIT
#undef VALUE_ASSIGNMENT
#undef FHASH_TYPE_NAME
#undef FHASH_TYPE_ITERATOR_NAME
#undef SHORTNAME
#undef CONCAT
#undef PASTE
