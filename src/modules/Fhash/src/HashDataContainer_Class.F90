! This library is taken from
! https://github.com/LKedward/fhash
!> Implements simple container type
!>  for polymorphic scalars and 1D arrays

MODULE HashDataContainer_Class
USE ISO_FORTRAN_ENV, ONLY: sp => REAL32, dp => REAL64, INT32, INT64
IMPLICIT NONE

PRIVATE

PUBLIC :: HashDataContainer_
PUBLIC :: HashDataContainer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generic container for scalar and 1D data
TYPE HashDataContainer_

  CLASS(*), ALLOCATABLE :: scalar_data
  CLASS(*), POINTER :: scalar_ptr => NULL()

CONTAINS

  PROCEDURE :: allocated => obj_allocated
  PROCEDURE :: get => obj_get
  PROCEDURE :: get_ptr => obj_get_ptr

END TYPE HashDataContainer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Create a fhash_container object from a polymorphic value
INTERFACE HashDataContainer
  MODULE PROCEDURE fhash_container_scalar
END INTERFACE HashDataContainer

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Helper to initialise a polymorphic data container with scalar
FUNCTION fhash_container_scalar(VALUE, POINTER) RESULT(container)

  !> Value to store
  CLASS(*), INTENT(in), TARGET :: VALUE

  !> If .true., store pointer to value instead of copying
  LOGICAL, INTENT(in), OPTIONAL :: POINTER

  TYPE(HashDataContainer_) :: container

  IF (PRESENT(POINTER)) THEN
    IF (POINTER) THEN
      container%scalar_ptr => VALUE
    ELSE
      IF (ALLOCATED(container%scalar_data)) DEALLOCATE (container%scalar_data)
      ALLOCATE (container%scalar_data, source=VALUE)
    END IF
  ELSE
    IF (ALLOCATED(container%scalar_data)) DEALLOCATE (container%scalar_data)
    ALLOCATE (container%scalar_data, source=VALUE)
  END IF

END FUNCTION fhash_container_scalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Helper to determine if container contains anything
FUNCTION obj_allocated(container) RESULT(alloc)
  CLASS(HashDataContainer_), INTENT(in) :: container
  LOGICAL :: alloc

  alloc = ALLOCATED(container%scalar_data) .OR. &
          ASSOCIATED(container%scalar_ptr)

END FUNCTION obj_allocated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Helper to return container value as intrinsic type
SUBROUTINE obj_get(container, i32, i64, r32, r64, &
                   char, bool, raw, match, type_string)
  CLASS(HashDataContainer_), INTENT(in), TARGET :: container
  INTEGER(INT32), INTENT(out), OPTIONAL :: i32
  INTEGER(INT64), INTENT(out), OPTIONAL :: i64
  REAL(sp), INTENT(out), OPTIONAL :: r32
  REAL(dp), INTENT(out), OPTIONAL :: r64
  CHARACTER(:), ALLOCATABLE, INTENT(out), OPTIONAL :: char
  LOGICAL, INTENT(out), OPTIONAL :: bool
  CLASS(*), ALLOCATABLE, INTENT(out), OPTIONAL :: raw
  LOGICAL, INTENT(out), OPTIONAL :: match
  CHARACTER(:), ALLOCATABLE, INTENT(out), OPTIONAL :: type_string

  CLASS(*), POINTER :: DATA

  IF (PRESENT(match)) match = .FALSE.

  IF (.NOT. container%ALLOCATED()) RETURN

  IF (ALLOCATED(container%scalar_data)) THEN
    DATA => container%scalar_data
  ELSE
    DATA => container%scalar_ptr
  END IF

  IF (PRESENT(raw)) THEN
    IF (PRESENT(match)) match = .TRUE.
    ALLOCATE (raw, source=DATA)
  END IF

  SELECT TYPE (d => DATA)
  TYPE is (INTEGER(INT32))
    IF (PRESENT(type_string)) type_string = 'integer32'
    IF (PRESENT(i32)) THEN
      IF (PRESENT(match)) match = .TRUE.
      i32 = d
      RETURN
    END IF

  TYPE is (INTEGER(INT64))
    IF (PRESENT(type_string)) type_string = 'integer64'
    IF (PRESENT(i64)) THEN
      IF (PRESENT(match)) match = .TRUE.
      i64 = d
      RETURN
    END IF

  TYPE is (REAL(sp))
    IF (PRESENT(type_string)) type_string = 'real32'
    IF (PRESENT(r32)) THEN
      IF (PRESENT(match)) match = .TRUE.
      r32 = d
      RETURN
    END IF

  TYPE is (REAL(dp))
    IF (PRESENT(type_string)) type_string = 'real64'
    IF (PRESENT(r64)) THEN
      IF (PRESENT(match)) match = .TRUE.
      r64 = d
      RETURN
    END IF

  TYPE is (CHARACTER(*))
    IF (PRESENT(type_string)) type_string = 'character*'
    IF (PRESENT(char)) THEN
      IF (PRESENT(match)) match = .TRUE.
      char = d
      RETURN
    END IF

  TYPE is (LOGICAL)
    IF (PRESENT(type_string)) type_string = 'logical'
    IF (PRESENT(bool)) THEN
      IF (PRESENT(match)) match = .TRUE.
      bool = d
      RETURN
    END IF

  CLASS default
    IF (PRESENT(type_string)) type_string = 'unknown'

  END SELECT

END SUBROUTINE obj_get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Helper to return pointer to container value as intrinsic type
SUBROUTINE obj_get_ptr(container, i32, i64, r32, r64, &
                       char, bool, raw, match, type_string)
  CLASS(HashDataContainer_), INTENT(in), TARGET :: container
  INTEGER(INT32), POINTER, INTENT(out), OPTIONAL :: i32
  INTEGER(INT64), POINTER, INTENT(out), OPTIONAL :: i64
  REAL(sp), POINTER, INTENT(out), OPTIONAL :: r32
  REAL(dp), POINTER, INTENT(out), OPTIONAL :: r64
  CHARACTER(:), POINTER, INTENT(out), OPTIONAL :: char
  LOGICAL, POINTER, INTENT(out), OPTIONAL :: bool
  CLASS(*), POINTER, INTENT(out), OPTIONAL :: raw
  LOGICAL, INTENT(out), OPTIONAL :: match
  CHARACTER(:), ALLOCATABLE, INTENT(out), OPTIONAL :: type_string

  CLASS(*), POINTER :: DATA

  IF (PRESENT(match)) match = .FALSE.

  IF (.NOT. container%ALLOCATED()) RETURN

  IF (ALLOCATED(container%scalar_data)) THEN
    DATA => container%scalar_data
  ELSE
    DATA => container%scalar_ptr
  END IF

  IF (PRESENT(raw)) THEN
    IF (PRESENT(match)) match = .TRUE.
    raw => DATA
  END IF

  SELECT TYPE (d => DATA)
  TYPE is (INTEGER(INT32))
    IF (PRESENT(i32)) THEN
      IF (PRESENT(match)) match = .TRUE.
      IF (PRESENT(type_string)) type_string = 'integer32'
      i32 => d
      RETURN
    END IF

  TYPE is (INTEGER(INT64))
    IF (PRESENT(i64)) THEN
      IF (PRESENT(match)) match = .TRUE.
      IF (PRESENT(type_string)) type_string = 'integer64'
      i64 => d
      RETURN
    END IF

  TYPE is (REAL(sp))
    IF (PRESENT(r32)) THEN
      IF (PRESENT(match)) match = .TRUE.
      IF (PRESENT(type_string)) type_string = 'real32'
      r32 => d
      RETURN
    END IF

  TYPE is (REAL(dp))
    IF (PRESENT(r64)) THEN
      IF (PRESENT(match)) match = .TRUE.
      IF (PRESENT(type_string)) type_string = 'real64'
      r64 => d
      RETURN
    END IF

  TYPE is (CHARACTER(*))
    IF (PRESENT(char)) THEN
      IF (PRESENT(match)) match = .TRUE.
      IF (PRESENT(type_string)) type_string = 'character*'
      char => d
      RETURN
    END IF

  TYPE is (LOGICAL)
    IF (PRESENT(bool)) THEN
      IF (PRESENT(match)) match = .TRUE.
      IF (PRESENT(type_string)) type_string = 'logical'
      bool => d
      RETURN
    END IF

  CLASS DEFAULT
    IF (PRESENT(type_string)) type_string = 'unknown'

  END SELECT

END SUBROUTINE obj_get_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HashDataContainer_Class
