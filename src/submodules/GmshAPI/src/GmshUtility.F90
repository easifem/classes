! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE GmshUtility
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64

USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_CHAR
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_F_POINTER

IMPLICIT NONE

PRIVATE

INTEGER(I4B), PARAMETER :: GMSH_API_MAX_STR_LEN = 512

PUBLIC :: optval_c_int
!  gmsh_opt_cint

PUBLIC :: optval_c_size_t

PUBLIC :: optval_c_double
!  gmsh_opt_cdouble

PUBLIC :: optval_c_bool

PUBLIC :: optval_c_str

PUBLIC :: optval_str_array
! gmsh_InputStr

PUBLIC :: size_gmsh_int

PUBLIC :: size_gmsh_size

PUBLIC :: size_gmsh_double

PUBLIC :: size_gmsh_str

PUBLIC :: size_gmsh_pair

PUBLIC :: size_gmsh_str_array
! gmsh_strArraySize

PUBLIC :: istring_
! gmsh_CString

PUBLIC :: ivectorint_

PUBLIC :: ivectorsize_

PUBLIC :: ivectordouble_

PUBLIC :: ivectorstring_
! gmsh_GetCharArray_cPtr

PUBLIC :: ivectorpair_

PUBLIC :: ivectorvectorint_

PUBLIC :: ivectorvectorsize_

PUBLIC :: ivectorvectordouble_

PUBLIC :: ostring_

PUBLIC :: ovectorint_
! PUBLIC :: gmsh_intvec_c2f

PUBLIC :: ovectorsize_

PUBLIC :: ovectordouble_
! PUBLIC :: gmsh_realvec_c2f

PUBLIC :: ovectorstring_
! gmsh_cStrings2CharArray

PUBLIC :: ovectorpair_
! PUBLIC :: gmsh_dimtag_c2f

PUBLIC :: ovectorvectorint_

PUBLIC :: ovectorvectorsize_

PUBLIC :: ovectorvectordouble_

PUBLIC :: ovectorvectorpair_

PUBLIC :: gmshFree

PUBLIC :: gmsh_c_strlen

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE cstr_
  CHARACTER(:), ALLOCATABLE :: s
END TYPE cstr_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, PRIVATE :: c_array_
  TYPE(C_PTR) :: s
END TYPE c_array_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION optval_c_int(default, option) RESULT(ans)
  CLASS(*), INTENT(in) :: default
  CLASS(*), OPTIONAL, INTENT(in) :: option
  INTEGER(C_INT) :: ans

  LOGICAL(LGT) :: isok

  isok = PRESENT(option)

  IF (isok) THEN
    ans = gmsh_cint(option)
  ELSE
    ans = gmsh_cint(default)
  END IF
END FUNCTION optval_c_int

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION optval_c_size_t(default, option) RESULT(ans)
  CLASS(*), INTENT(IN) :: default
  CLASS(*), OPTIONAL, INTENT(IN) :: option
  INTEGER(C_SIZE_T) :: ans

  IF (PRESENT(option)) THEN
    ans = gmsh_csize(option)
  ELSE
    ans = gmsh_csize(default)
  END IF
END FUNCTION optval_c_size_t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION optval_c_double(default, option) RESULT(ans)
  CLASS(*), INTENT(IN) :: default
  CLASS(*), OPTIONAL, INTENT(IN) :: option
  REAL(C_DOUBLE) :: ans

  IF (PRESENT(option)) THEN
    ans = gmsh_cdouble(option)
  ELSE
    ans = gmsh_cdouble(default)
  END IF
END FUNCTION optval_c_double

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE INTEGER(C_INT) FUNCTION optval_c_bool(default, option) RESULT(res)
  LOGICAL, INTENT(IN) :: default
  LOGICAL, OPTIONAL, INTENT(IN) :: option
  res = MERGE(1_C_INT, 0_C_INT, default)
  IF (PRESENT(option)) res = MERGE(1_C_INT, 0_C_INT, option)
END FUNCTION optval_c_bool

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!
PURE FUNCTION optval_c_str(def, val) RESULT(res)
  CHARACTER(len=*), INTENT(in) :: def
  CHARACTER(len=*), OPTIONAL, INTENT(in) :: val
  CHARACTER(len=:), ALLOCATABLE :: res
  IF (PRESENT(val)) THEN
    res = val
  ELSE
    res = def
  END IF
END FUNCTION optval_c_str

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION optval_str_array(def, val) RESULT(res)
  CHARACTER(len=*), INTENT(in) :: def(:)
  CHARACTER(len=*), OPTIONAL, INTENT(in) :: val(:)
  CHARACTER(len=:), ALLOCATABLE :: res(:)
  IF (PRESENT(val)) THEN
    res = val
  ELSE
    res = def
  END IF
END FUNCTION optval_str_array

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE INTEGER(C_SIZE_T) FUNCTION size_gmsh_int(v) RESULT(n)
  INTEGER(C_INT), OPTIONAL, INTENT(in) :: v(:)
  n = 0
  IF (PRESENT(v)) n = SIZE(v, kind=C_SIZE_T)
END FUNCTION size_gmsh_int

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE INTEGER(C_SIZE_T) FUNCTION size_gmsh_size(v) RESULT(n)
  INTEGER(C_SIZE_T), OPTIONAL, INTENT(in) :: v(:)
  n = 0
  IF (PRESENT(v)) n = SIZE(v, kind=C_SIZE_T)
END FUNCTION size_gmsh_size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE INTEGER(C_SIZE_T) FUNCTION size_gmsh_double(v) RESULT(n)
  REAL(C_DOUBLE), OPTIONAL, INTENT(in) :: v(:)
  n = 0
  IF (PRESENT(v)) n = SIZE(v, kind=C_SIZE_T)
END FUNCTION size_gmsh_double

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! earlier it was named gmsh_size_str
PURE INTEGER(C_SIZE_T) FUNCTION size_gmsh_str(v) RESULT(n)
  CHARACTER(len=*), OPTIONAL, INTENT(in) :: v(:)
  n = 0
  IF (PRESENT(v)) n = SIZE(v, kind=C_SIZE_T)
END FUNCTION size_gmsh_str

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE INTEGER(C_SIZE_T) FUNCTION size_gmsh_pair(v) RESULT(n)
  INTEGER(C_INT), OPTIONAL, INTENT(in) :: v(:, :)
  n = 0
  IF (PRESENT(v)) n = SIZE(v, kind=C_SIZE_T)
END FUNCTION size_gmsh_pair

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE INTEGER(C_INT) FUNCTION size_gmsh_str_array(v) RESULT(n)
  CHARACTER(len=*), OPTIONAL, INTENT(in) :: v(:)
  n = 1
  ! can't have 0-length commands
  IF (PRESENT(v)) n = SIZE(v, kind=C_INT)
END FUNCTION size_gmsh_str_array

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Calculates the length of a C string.
FUNCTION gmsh_c_strlen(carray) RESULT(res)
  CHARACTER(kind=C_CHAR, len=1), INTENT(IN) :: carray(:)
  INTEGER :: res

  ! Internal variable
  INTEGER :: i

  DO i = 1, SIZE(carray)
    IF (carray(i) == C_NULL_CHAR) THEN
      res = i - 1
      RETURN
    END IF
  END DO
  res = i
END FUNCTION gmsh_c_strlen

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION gmsh_InputStr(default, option) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: default(:)
  CHARACTER(*), OPTIONAL, INTENT(IN) :: option(:)
  CHARACTER(:), ALLOCATABLE :: ans(:)
  IF (PRESENT(option)) THEN
    ans = option
  ELSE
    ans = default
  END IF
END FUNCTION gmsh_InputStr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: istring_
FUNCTION istring_(o) RESULT(v)
  CHARACTER(*), INTENT(IN) :: o
  CHARACTER(:, kind=C_CHAR), ALLOCATABLE :: v
  v = TRIM(o)//C_NULL_CHAR
END FUNCTION istring_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ivectorint_(o) RESULT(v)
  INTEGER(C_INT), INTENT(in) :: o(:)
  INTEGER(C_INT), DIMENSION(SIZE(o)) :: v
  v = o
END FUNCTION ivectorint_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ivectorsize_(o) RESULT(v)
  INTEGER(C_SIZE_T), INTENT(in) :: o(:)
  INTEGER(C_SIZE_T), DIMENSION(SIZE(o)) :: v
  v = o
END FUNCTION ivectorsize_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ivectordouble_(o) RESULT(v)
  REAL(C_DOUBLE), INTENT(in) :: o(:)
  REAL(C_DOUBLE), DIMENSION(SIZE(o)) :: v
  v = o
END FUNCTION ivectordouble_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ivectorpair_(o) RESULT(v)
  INTEGER(C_INT), INTENT(in) :: o(:, :)
  INTEGER(C_INT), DIMENSION(SIZE(o, 1), 2) :: v
  v = o
END FUNCTION ivectorpair_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ivectorvectorint_(v, dims, cptr1, cptr2, n)
  INTEGER(C_INT), TARGET, INTENT(in) :: v(:)
  INTEGER(C_SIZE_T), TARGET, INTENT(in) :: dims(:)
  TYPE(C_PTR), INTENT(out) :: cptr1, cptr2
  INTEGER(C_SIZE_T), INTENT(out) :: n

  n = SIZE(dims, kind=C_SIZE_T)
  cptr1 = C_LOC(v)
  cptr2 = C_LOC(dims)
END SUBROUTINE ivectorvectorint_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ivectorvectorsize_(v, dims, cptr1, cptr2, n)
  INTEGER(C_SIZE_T), TARGET, INTENT(in) :: v(:)
  INTEGER(C_SIZE_T), TARGET, INTENT(in) :: dims(:)
  TYPE(C_PTR), INTENT(out) :: cptr1, cptr2
  INTEGER(C_SIZE_T), INTENT(out) :: n

  n = SIZE(dims, kind=C_SIZE_T)
  cptr1 = C_LOC(v)
  cptr2 = C_LOC(dims)
END SUBROUTINE ivectorvectorsize_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ivectorvectordouble_(v, dims, cptr1, cptr2, n)
  REAL(C_DOUBLE), TARGET, INTENT(in) :: v(:)
  INTEGER(C_SIZE_T), TARGET, INTENT(in) :: dims(:)
  TYPE(C_PTR), INTENT(out) :: cptr1, cptr2
  INTEGER(C_SIZE_T), INTENT(out) :: n

  n = SIZE(dims, kind=C_SIZE_T)
  cptr1 = C_LOC(v)
  cptr2 = C_LOC(dims)
END SUBROUTINE ivectorvectordouble_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: ivectorstring_
SUBROUTINE ivectorstring_(o, cstrs, cptrs)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: o(:)
  CHARACTER(len=GMSH_API_MAX_STR_LEN, kind=C_CHAR), TARGET, ALLOCATABLE, &
    INTENT(out) :: cstrs(:)
  TYPE(C_PTR), ALLOCATABLE, INTENT(out) :: cptrs(:)
  INTEGER :: i

  IF (PRESENT(o)) THEN
    ALLOCATE (cstrs(SIZE(o)))
    ! Return to keep references from cptrs
    ALLOCATE (cptrs(SIZE(o)))
    DO i = 1, SIZE(o)
      cstrs(i) = istring_(o(i))
      cptrs(i) = C_LOC(cstrs(i))
    END DO
  ELSE
    ALLOCATE (cstrs(0))
    ALLOCATE (cptrs(0))
  END IF
END SUBROUTINE ivectorstring_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION gmsh_cdouble(VALUE) RESULT(ans)
  CLASS(*), INTENT(in) :: VALUE
  REAL(C_DOUBLE) :: ans

  SELECT TYPE (VALUE)
  TYPE is (INTEGER(INT8))
    ans = REAL(VALUE, kind=C_DOUBLE)
  TYPE is (INTEGER(INT16))
    ans = REAL(VALUE, kind=C_DOUBLE)
  TYPE is (INTEGER(INT32))
    ans = REAL(VALUE, kind=C_DOUBLE)
  TYPE is (INTEGER(INT64))
    ans = REAL(VALUE, kind=C_DOUBLE)
  TYPE is (REAL(REAL32))
    ans = REAL(VALUE, kind=C_DOUBLE)
  TYPE is (REAL(REAL64))
    ans = REAL(VALUE, kind=C_DOUBLE)
  END SELECT
END FUNCTION gmsh_cdouble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION gmsh_cint(VALUE) RESULT(ans)
  CLASS(*), INTENT(in) :: VALUE
  INTEGER(C_INT) :: ans

  SELECT TYPE (VALUE)
  TYPE is (INTEGER(INT8))
    ans = INT(VALUE, kind=C_INT)
  TYPE is (INTEGER(INT16))
    ans = INT(VALUE, kind=C_INT)
  TYPE is (INTEGER(INT32))
    ans = INT(VALUE, kind=C_INT)
  TYPE is (INTEGER(INT64))
    ans = INT(VALUE, kind=C_INT)
  TYPE is (REAL(REAL32))
    ans = INT(VALUE, kind=C_INT)
  TYPE is (REAL(REAL64))
    ans = INT(VALUE, kind=C_INT)
  END SELECT
END FUNCTION gmsh_cint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION gmsh_csize(VALUE) RESULT(ans)
  CLASS(*), INTENT(in) :: VALUE
  INTEGER(C_SIZE_T) :: ans

  SELECT TYPE (VALUE)
  TYPE is (INTEGER(INT8))
    ans = INT(VALUE, kind=C_SIZE_T)
  TYPE is (INTEGER(INT16))
    ans = INT(VALUE, kind=C_SIZE_T)
  TYPE is (INTEGER(INT32))
    ans = INT(VALUE, kind=C_SIZE_T)
  TYPE is (INTEGER(INT64))
    ans = INT(VALUE, kind=C_SIZE_T)
  TYPE is (REAL(REAL32))
    ans = INT(VALUE, kind=C_SIZE_T)
  TYPE is (REAL(REAL64))
    ans = INT(VALUE, kind=C_SIZE_T)
  END SELECT
END FUNCTION gmsh_csize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! gmsh_cStrings2CharArray
FUNCTION ovectorstring_(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  CHARACTER(len=GMSH_API_MAX_STR_LEN), ALLOCATABLE :: v(:)

  ! Internal variables
  INTEGER(C_SIZE_T) :: i
  TYPE(c_array_), POINTER :: c_array(:)

  CALL C_F_POINTER(cptr, c_array, [n])
  ALLOCATE (v(n))
  DO i = 1_C_SIZE_T, n
    v(i) = ostring_(c_array(i)%s)
  END DO
  CALL gmshFree(cptr)
END FUNCTION ovectorstring_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ostring_(cptr) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  CHARACTER(len=:), ALLOCATABLE :: v
  CHARACTER(len=GMSH_API_MAX_STR_LEN), POINTER :: fptr
  INTEGER(C_SIZE_T) :: i
  CALL C_F_POINTER(cptr, fptr)
  DO i = 1_C_SIZE_T, GMSH_API_MAX_STR_LEN
    IF (fptr(i:i) == C_NULL_CHAR) EXIT
  END DO
  v = fptr(:i)
  CALL gmshFree(cptr)
END FUNCTION ostring_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: ovectorint_
! gmsh_intvec_c2f
FUNCTION ovectorint_(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  INTEGER(C_INT), ALLOCATABLE :: v(:)

  ! Internal variables
  INTEGER(C_INT), POINTER :: v_(:)
  CALL C_F_POINTER(cptr, v_, [n])
  ALLOCATE (v, source=v_)
  CALL gmshFree(cptr)
END FUNCTION ovectorint_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ovectorsize_(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  INTEGER(C_SIZE_T), ALLOCATABLE :: v(:)
  INTEGER(C_SIZE_T), POINTER :: v_(:)
  CALL C_F_POINTER(cptr, v_, [n])
  ALLOCATE (v, source=v_)
  CALL gmshFree(cptr)
END FUNCTION ovectorsize_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: ovectordouble_
! gmsh_realvec_c2f
FUNCTION ovectordouble_(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  REAL(C_DOUBLE), ALLOCATABLE :: v(:)

  ! Internal variables
  REAL(C_DOUBLE), POINTER :: v_(:)

  CALL C_F_POINTER(cptr, v_, [n])
  ALLOCATE (v, source=v_)
  CALL gmshFree(cptr)
END FUNCTION ovectordouble_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Ref: ovectorpair
! gmsh_dimtag_c2f
FUNCTION ovectorpair_(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  INTEGER(C_INT), ALLOCATABLE :: v(:, :)

  ! Internal variables
  INTEGER(C_INT), POINTER :: v_(:, :)

  CALL C_F_POINTER(cptr, v_, [2_C_SIZE_T, n / 2_C_SIZE_T])
  ALLOCATE (v, source=v_)
  CALL gmshFree(cptr)
END FUNCTION ovectorpair_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ovectorvectorint_(cptr1, cptr2, n, v, dims)
  TYPE(C_PTR), INTENT(inout) :: cptr1, cptr2
  INTEGER(C_SIZE_T), INTENT(in) :: n
  INTEGER(C_INT), ALLOCATABLE, INTENT(out) :: v(:)
  INTEGER(C_SIZE_T), ALLOCATABLE, INTENT(out) :: dims(:)
  INTEGER(C_INT), POINTER :: v_(:)
  INTEGER(C_SIZE_T), POINTER :: dims_(:)
  TYPE(C_PTR), POINTER :: ptrs(:)
  INTEGER(C_SIZE_T) :: i, istart, iend
  CALL C_F_POINTER(cptr2, dims_, [n])
  ALLOCATE (dims, source=dims_)
  ALLOCATE (v(SUM(dims_)))
  istart = 1
  CALL C_F_POINTER(cptr1, ptrs, [n])
  DO i = 1_C_SIZE_T, n
    iend = INT(dims(i))
    CALL C_F_POINTER(ptrs(i), v_, [iend])
    v(istart:istart + iend - 1) = v_
    istart = istart + iend
  END DO
  DO i = 1_C_SIZE_T, n
    CALL gmshFree(ptrs(i))
  END DO
  CALL gmshFree(cptr1)
  CALL gmshFree(cptr2)
END SUBROUTINE ovectorvectorint_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ovectorvectorsize_(cptr1, cptr2, n, v, dims)
  TYPE(C_PTR), INTENT(inout) :: cptr1, cptr2
  INTEGER(C_SIZE_T), INTENT(in) :: n
  INTEGER(C_SIZE_T), ALLOCATABLE, INTENT(out) :: v(:)
  INTEGER(C_SIZE_T), ALLOCATABLE, INTENT(out) :: dims(:)
  INTEGER(C_SIZE_T), POINTER :: v_(:)
  INTEGER(C_SIZE_T), POINTER :: dims_(:)
  TYPE(C_PTR), POINTER :: ptrs(:)
  INTEGER(C_SIZE_T) :: i, istart, iend
  CALL C_F_POINTER(cptr2, dims_, [n])
  ALLOCATE (dims, source=dims_)
  ALLOCATE (v(SUM(dims_)))
  istart = 1
  CALL C_F_POINTER(cptr1, ptrs, [n])
  DO i = 1_C_SIZE_T, n
    iend = INT(dims(i))
    CALL C_F_POINTER(ptrs(i), v_, [iend])
    v(istart:istart + iend - 1) = v_
    istart = istart + iend
  END DO
  DO i = 1_C_SIZE_T, n
    CALL gmshFree(ptrs(i))
  END DO
  CALL gmshFree(cptr1)
  CALL gmshFree(cptr2)
END SUBROUTINE ovectorvectorsize_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ovectorvectordouble_(cptr1, cptr2, n, v, dims)
  TYPE(C_PTR), INTENT(inout) :: cptr1, cptr2
  INTEGER(C_SIZE_T), INTENT(in) :: n
  REAL(C_DOUBLE), ALLOCATABLE, INTENT(out) :: v(:)
  INTEGER(C_SIZE_T), ALLOCATABLE, INTENT(out) :: dims(:)
  REAL(C_DOUBLE), POINTER :: v_(:)
  INTEGER(C_SIZE_T), POINTER :: dims_(:)
  TYPE(C_PTR), POINTER :: ptrs(:)
  INTEGER(C_SIZE_T) :: i, istart, iend
  CALL C_F_POINTER(cptr2, dims_, [n])
  ALLOCATE (dims, source=dims_)
  ALLOCATE (v(SUM(dims_)))
  istart = 1
  CALL C_F_POINTER(cptr1, ptrs, [n])
  DO i = 1_C_SIZE_T, n
    iend = INT(dims(i))
    CALL C_F_POINTER(ptrs(i), v_, [iend])
    v(istart:istart + iend - 1) = v_
    istart = istart + iend
  END DO
  DO i = 1_C_SIZE_T, n
    CALL gmshFree(ptrs(i))
  END DO
  CALL gmshFree(cptr1)
  CALL gmshFree(cptr2)
END SUBROUTINE ovectorvectordouble_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ovectorvectorpair_(cptr1, cptr2, n, v, dims)
  TYPE(C_PTR), INTENT(inout) :: cptr1, cptr2
  INTEGER(C_SIZE_T), INTENT(in) :: n
  INTEGER(C_INT), ALLOCATABLE, INTENT(out) :: v(:, :)
  INTEGER(C_SIZE_T), ALLOCATABLE, INTENT(out) :: dims(:)
  INTEGER(C_INT), POINTER :: v_(:, :)
  INTEGER(C_SIZE_T), POINTER :: dims_(:)
  TYPE(C_PTR), POINTER :: ptrs(:)
  INTEGER(C_SIZE_T) :: i, istart, iend
  CALL C_F_POINTER(cptr2, dims_, [n])
  ALLOCATE (dims, source=dims_)
  ALLOCATE (v(2, INT(SUM(dims_) / 2)))
  istart = 1
  CALL C_F_POINTER(cptr1, ptrs, [n])
  DO i = 1_C_SIZE_T, n
    iend = INT(dims(i) / 2)
    CALL C_F_POINTER(ptrs(i), v_, [2_C_SIZE_T, iend])
    v(:, istart:istart + iend - 1) = v_
    istart = istart + iend
  END DO
  DO i = 1_C_SIZE_T, n
    CALL gmshFree(ptrs(i))
  END DO
  CALL gmshFree(cptr1)
  CALL gmshFree(cptr2)
END SUBROUTINE ovectorvectorpair_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Callback to C to free any reserved memory
SUBROUTINE gmshFree(p)

  INTERFACE
    SUBROUTINE C_API(ptr) BIND(C, name="gmshFree")
      IMPORT
      TYPE(C_PTR), VALUE :: ptr
    END SUBROUTINE C_API
  END INTERFACE

  TYPE(C_PTR) :: p
  CALL C_API(p)
END SUBROUTINE gmshFree

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshUtility
