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
USE GlobalData
USE CInterface
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
PRIVATE

INTEGER(I4B), PRIVATE, PARAMETER :: maxStrLen = 256
PUBLIC :: gmsh_InputStr
PUBLIC :: gmsh_GetCharArray_cPtr
PUBLIC :: gmsh_CString
PUBLIC :: gmsh_strArraySize
PUBLIC :: gmsh_cdouble
PUBLIC :: gmsh_opt_cdouble
PUBLIC :: gmsh_cint
PUBLIC :: gmsh_opt_cint
PUBLIC :: gmsh_csize
PUBLIC :: gmsh_cStrings2CharArray
PUBLIC :: gmsh_intvec_c2f
PUBLIC :: gmsh_realvec_c2f
PUBLIC :: gmsh_dimtag_c2f
PUBLIC :: gmsh_cstrlen
PUBLIC :: gmshFree
PUBLIC :: gmsh_size_str

TYPE cstr_
  CHARACTER(:), ALLOCATABLE :: s
END TYPE cstr_

TYPE, PRIVATE :: c_array_
  TYPE(C_PTR) :: s
END TYPE c_array_

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE INTEGER(C_SIZE_T) FUNCTION gmsh_size_str(v) RESULT(n)
  CHARACTER(*), OPTIONAL, INTENT(in) :: v(:)
  n = 0
  IF (PRESENT(v)) n = SIZE(v, kind=C_SIZE_T)
END FUNCTION gmsh_size_str

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION gmsh_InputStr(default, option) &
  & RESULT(ans)
  CHARACTER(*), INTENT(in) :: default(:)
  CHARACTER(*), OPTIONAL, INTENT(in) :: option(:)
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

! ref: ivectorstring_

SUBROUTINE gmsh_GetCharArray_cPtr(o, cstrs, cptrs)
  CHARACTER(*), INTENT(in) :: o(:)
  CHARACTER(len=maxStrLen, kind=C_CHAR), TARGET, ALLOCATABLE, &
  & INTENT(out) :: cstrs(:)
  TYPE(C_PTR), ALLOCATABLE, INTENT(out) :: cptrs(:)
    !!
  INTEGER :: i
  ALLOCATE (cstrs(SIZE(o))) ! Return to keep references from cptrs
  ALLOCATE (cptrs(SIZE(o)))
  DO i = 1, SIZE(o)
    cstrs(i) = gmsh_CString(o(i))
    cptrs(i) = C_LOC(cstrs(i))
  END DO
END SUBROUTINE gmsh_GetCharArray_cPtr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: istring_

FUNCTION gmsh_CString(o) RESULT(v)
  CHARACTER(*), INTENT(in) :: o
  CHARACTER(:, kind=C_CHAR), ALLOCATABLE :: v
  v = TRIM(o)//C_NULL_CHAR
END FUNCTION gmsh_CString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION gmsh_strArraySize(v) RESULT(n)
  CHARACTER(*), OPTIONAL, INTENT(in) :: v(:)
  INTEGER(C_INT) :: n
  n = 1 ! can't have 0-length commands
  IF (PRESENT(v)) n = SIZE(v, kind=C_INT)
END FUNCTION gmsh_strArraySize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION gmsh_cdouble(VALUE) RESULT(ans)
  CLASS(*), INTENT(in) :: VALUE
  REAL(C_DOUBLE) :: ans
  !!
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
  !!
END FUNCTION gmsh_cdouble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION gmsh_opt_cdouble(default, option) RESULT(ans)
  CLASS(*), INTENT(in) :: default
  CLASS(*), OPTIONAL, INTENT(in) :: option
  REAL(C_DOUBLE) :: ans
  !!
  IF (PRESENT(option)) THEN
    ans = gmsh_cdouble(option)
  ELSE
    ans = gmsh_cdouble(default)
  END IF
  !!
END FUNCTION gmsh_opt_cdouble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION gmsh_cint(VALUE) RESULT(ans)
  CLASS(*), INTENT(in) :: VALUE
  INTEGER(C_INT) :: ans
  !!
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
  !!
END FUNCTION gmsh_cint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION gmsh_opt_cint(default, option) RESULT(ans)
  CLASS(*), INTENT(in) :: default
  CLASS(*), OPTIONAL, INTENT(in) :: option
  INTEGER(C_INT) :: ans
  !!
  IF (PRESENT(option)) THEN
    ans = gmsh_cint(option)
  ELSE
    ans = gmsh_cint(default)
  END IF
  !!
END FUNCTION gmsh_opt_cint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION gmsh_csize(VALUE) RESULT(ans)
  CLASS(*), INTENT(in) :: VALUE
  INTEGER(C_SIZE_T) :: ans
  !!
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
  !!
END FUNCTION gmsh_csize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: ovectorstring

FUNCTION gmsh_cStrings2CharArray(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  CHARACTER(len=maxStrLen), ALLOCATABLE :: v(:)
  !!
  INTEGER(C_SIZE_T) :: i, c, lenstr
  TYPE(C_PTR), POINTER :: c_array(:)
  ! type(c_array_), pointer :: c_array(:)
  CHARACTER(kind=C_CHAR, len=1), POINTER :: fptr(:)
  !!
  CALL C_F_POINTER(cptr, c_array, [n])
  !!
  ALLOCATE (v(n))
  !!
  DO i = 1_C_SIZE_T, n
    !!
    CALL C_F_POINTER( &
      & c_array(i), fptr, &
      & [INT(maxStrLen)])
    !!
    lenstr = gmsh_cstrlen(fptr)
    v(i) = ""
    !!
    DO c = 1_C_SIZE_T, lenstr
      v(i) (c:c) = fptr(c)
    END DO
    !!
  END DO
  !!
  CALL gmshFree(cptr)
END FUNCTION gmsh_cStrings2CharArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: ovectorint_

FUNCTION gmsh_intvec_c2f(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  INTEGER(C_INT), ALLOCATABLE :: v(:)
  !!
  INTEGER(C_INT), POINTER :: v_(:)
  CALL C_F_POINTER(cptr, v_, [n])
  ALLOCATE (v, source=v_)
  CALL gmshFree(cptr)
END FUNCTION gmsh_intvec_c2f

! ref: ovectordouble_

FUNCTION gmsh_realvec_c2f(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  REAL(C_DOUBLE), ALLOCATABLE :: v(:)
  REAL(C_DOUBLE), POINTER :: v_(:)
  CALL C_F_POINTER(cptr, v_, [n])
  ALLOCATE (v, source=v_)
  CALL gmshFree(cptr)
END FUNCTION gmsh_realvec_c2f

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Ref: ovectorpair

FUNCTION gmsh_dimtag_c2f(cptr, n) RESULT(v)
  TYPE(C_PTR), INTENT(inout) :: cptr
  INTEGER(C_SIZE_T), INTENT(in) :: n
  INTEGER(C_INT), ALLOCATABLE :: v(:, :)
  !!
  INTEGER(C_INT), POINTER :: v_(:, :)
  !!
  CALL C_F_POINTER(cptr, v_, [2_C_SIZE_T, n / 2_C_SIZE_T])
  !!
  ALLOCATE (v, source=v_)
  !!
  CALL gmshFree(cptr)
  !!
END FUNCTION gmsh_dimtag_c2f

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Calculates the length of a C string.
FUNCTION gmsh_cstrlen(carray) RESULT(res)
  CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: carray(:)
  INTEGER :: res
  INTEGER :: i
  DO i = 1, SIZE(carray)
    IF (carray(i) == C_NULL_CHAR) THEN
      res = i - 1
      RETURN
    END IF
  END DO
  res = i
END FUNCTION gmsh_cstrlen

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Callback to C to free any reserved memory
SUBROUTINE gmshFree(p)
  INTERFACE
    SUBROUTINE C_API(ptr) BIND(C, name="gmshFree")
      USE, INTRINSIC :: ISO_C_BINDING
      TYPE(C_PTR), VALUE :: ptr
    END SUBROUTINE C_API
  END INTERFACE
  TYPE(C_PTR) :: p
  CALL C_API(p)
END SUBROUTINE gmshFree

END MODULE GmshUtility
