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
USE, INTRINSIC :: ISO_C_Binding
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

type cstr_
  character(len=:), allocatable :: s
end type cstr_

type, private :: c_array_
  type(c_ptr) :: s
end type c_array_

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure integer(c_size_t) function gmsh_size_str(v) result(n)
  character(len=*), optional, intent(in) :: v(:)
  n = 0
  if (present(v)) n = size(v, kind=c_size_t)
end function gmsh_size_str

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function gmsh_InputStr(default, option) &
  & result(ans)
  character(len=*), intent(in) :: default(:)
  character(len=*), optional, intent(in) :: option(:)
  character(len=:), allocatable :: ans(:)
  if (present(option)) then
    ans = option
  else
    ans = default
  end if
end function gmsh_InputStr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: ivectorstring_

subroutine gmsh_GetCharArray_cPtr(o, cstrs, cptrs)
  character(len=*), intent(in) :: o(:)
  character(len=maxStrLen, kind=c_char), target, allocatable, &
  & intent(out) :: cstrs(:)
  type(c_ptr), allocatable, intent(out) :: cptrs(:)
    !!
  integer :: i
  allocate (cstrs(size(o)))    ! Return to keep references from cptrs
  allocate (cptrs(size(o)))
  do i = 1, SIZE(o)
    cstrs(i) = gmsh_CString(o(i))
    cptrs(i) = C_LOC(cstrs(i))
  end do
end subroutine gmsh_GetCharArray_cPtr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: istring_

function gmsh_CString(o) result(v)
  character(len=*), intent(in) :: o
  character(len=:, kind=c_char), allocatable :: v
  v = trim(o)//c_null_char
end function gmsh_CString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function gmsh_strArraySize(v) result(n)
  character(len=*), optional, intent(in) :: v(:)
  integer(c_int) :: n
  n = 1   ! can't have 0-length commands
  if (present(v)) n = size(v, kind=c_int)
end function gmsh_strArraySize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

elemental function gmsh_cdouble(value) result(ans)
  class(*), intent(in) :: value
  real(C_DOUBLE) :: ans
  !!
  select type (value)
  type is (integer(int8))
    ans = real(value, kind=c_double)
  type is (integer(int16))
    ans = real(value, kind=c_double)
  type is (integer(int32))
    ans = real(value, kind=c_double)
  type is (integer(int64))
    ans = real(value, kind=c_double)
  type is (real(real32))
    ans = real(value, kind=c_double)
  type is (real(real64))
    ans = real(value, kind=c_double)
  end select

  !!
end function gmsh_cdouble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

elemental function gmsh_opt_cdouble(default, option) result(ans)
  class(*), intent(in) :: default
  class(*), optional, intent(in) :: option
  real(C_DOUBLE) :: ans
  !!
  if (present(option)) then
    ans = gmsh_cdouble(option)
  else
    ans = gmsh_cdouble(default)
  end if
  !!
end function gmsh_opt_cdouble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

elemental function gmsh_cint(value) result(ans)
  class(*), intent(in) :: value
  INTEGER(C_INT) :: ans
  !!
  select type (value)
  type is (integer(int8))
    ans = int(value, kind=c_int)
  type is (integer(int16))
    ans = int(value, kind=c_int)
  type is (integer(int32))
    ans = int(value, kind=c_int)
  type is (integer(int64))
    ans = int(value, kind=c_int)
  type is (real(real32))
    ans = int(value, kind=c_int)
  type is (real(real64))
    ans = int(value, kind=c_int)
  end select
  !!
end function gmsh_cint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

elemental function gmsh_opt_cint(default, option) result(ans)
  class(*), intent(in) :: default
  class(*), optional, intent(in) :: option
  integer(C_INT) :: ans
  !!
  if (present(option)) then
    ans = gmsh_cint(option)
  else
    ans = gmsh_cint(default)
  end if
  !!
end function gmsh_opt_cint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

elemental function gmsh_csize(value) result(ans)
  class(*), intent(in) :: value
  INTEGER(C_SIZE_T) :: ans
  !!
  select type (value)
  type is (integer(int8))
    ans = int(value, kind=c_size_t)
  type is (integer(int16))
    ans = int(value, kind=c_size_t)
  type is (integer(int32))
    ans = int(value, kind=c_size_t)
  type is (integer(int64))
    ans = int(value, kind=c_size_t)
  type is (real(real32))
    ans = int(value, kind=c_size_t)
  type is (real(real64))
    ans = int(value, kind=c_size_t)
  end select
  !!
end function gmsh_csize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: ovectorstring

function gmsh_cStrings2CharArray(cptr, n) result(v)
  type(c_ptr), intent(inout) :: cptr
  integer(c_size_t), intent(in) :: n
  character(len=maxStrLen), allocatable :: v(:)
  !!
  integer(c_size_t) :: i, c, lenstr
  type(c_ptr), pointer :: c_array(:)
  ! type(c_array_), pointer :: c_array(:)
  character(kind=c_char, len=1), pointer :: fptr(:)
  !!
  call c_f_pointer(cptr, c_array, [n])
  !!
  allocate (v(n))
  !!
  do i = 1_c_size_t, n
    !!
    call c_f_pointer( &
      & c_array(i), fptr, &
      & [int(maxStrLen)])
    !!
    lenstr = gmsh_cstrlen(fptr)
    v(i) = ""
    !!
    do c = 1_c_size_t, lenstr
      v(i) (c:c) = fptr(c)
    end do
    !!
  end do
  !!
  call gmshFree(cptr)
end function gmsh_cStrings2CharArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ref: ovectorint_

function gmsh_intvec_c2f(cptr, n) result(v)
  type(c_ptr), intent(inout) :: cptr
  integer(c_size_t), intent(in) :: n
  integer(c_int), allocatable :: v(:)
  !!
  integer(c_int), pointer :: v_(:)
  call c_f_pointer(cptr, v_, [n])
  allocate (v, source=v_)
  call gmshFree(cptr)
end function gmsh_intvec_c2f

! ref: ovectordouble_

function gmsh_realvec_c2f(cptr, n) result(v)
  type(c_ptr), intent(inout) :: cptr
  integer(c_size_t), intent(in) :: n
  real(c_double), allocatable :: v(:)
  real(c_double), pointer :: v_(:)
  call c_f_pointer(cptr, v_, [n])
  allocate (v, source=v_)
  call gmshFree(cptr)
end function gmsh_realvec_c2f

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Ref: ovectorpair

function gmsh_dimtag_c2f(cptr, n) result(v)
  type(c_ptr), intent(inout) :: cptr
  integer(c_size_t), intent(in) :: n
  integer(c_int), allocatable :: v(:, :)
  !!
  integer(c_int), pointer :: v_(:, :)
  !!
  call c_f_pointer(cptr, v_, [2_c_size_t, n / 2_c_size_t])
  !!
  allocate (v, source=v_)
  !!
  call gmshFree(cptr)
  !!
end function gmsh_dimtag_c2f

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Calculates the length of a C string.
function gmsh_cstrlen(carray) result(res)
  character(kind=c_char, len=1), intent(in) :: carray(:)
  integer :: res
  integer :: i
  do i = 1, size(carray)
    if (carray(i) == c_null_char) then
      res = i - 1
      return
    end if
  end do
  res = i
end function gmsh_cstrlen

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Callback to C to free any reserved memory
subroutine gmshFree(p)
  interface
    subroutine C_API(ptr) bind(C, name="gmshFree")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: ptr
    end subroutine C_API
  end interface
  type(c_ptr) :: p
  call C_API(p)
end subroutine gmshFree

END MODULE GmshUtility
