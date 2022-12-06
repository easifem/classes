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

MODULE GmshOption_Class
USE GlobalData, ONLY: DFP => Real64, I4B => Int32, LGT
USE GmshInterface
USE GmshUtility
USE CInterface
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "GMSHOPTION_CLASS"
INTEGER(C_INT) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER(I4B), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshOption_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => opt_Initiate
  PROCEDURE, PUBLIC, PASS(Obj) :: setNumber => opt_setNumber
  PROCEDURE, PUBLIC, PASS(Obj) :: getNumber => opt_getNumber
  PROCEDURE, PUBLIC, PASS(Obj) :: setString => opt_setString
  PROCEDURE, PUBLIC, PASS(Obj) :: getString => opt_getString
  PROCEDURE, PUBLIC, PASS(Obj) :: setColor => opt_setColor
  PROCEDURE, PUBLIC, PASS(Obj) :: getColor => opt_getColor
END TYPE GmshOption_

PUBLIC :: GmshOption_
TYPE(GmshOption_), PUBLIC, PARAMETER :: TypeGmshOption = GmshOption_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshOptionPointer_
  CLASS(GmshOption_), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshOptionPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE opt_Initiate(obj)
  CLASS(GmshOption_), INTENT(INOUT) :: obj
END SUBROUTINE opt_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_setNumber(obj, name, value) RESULT(ans)
  CLASS(GmshOption_), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: name
  CLASS(*), INTENT(IN) :: value
  INTEGER(I4B) :: ans

  ! Internal variables
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !!
  name_ = TRIM(name)//C_NULL_CHAR
  CALL gmshOptionSetNumber( &
    & name=C_LOC(name_), &
    & value=gmsh_cdouble(value), &
    & ierr=ans)
  ! ans = INT( ierr, KIND=I4B)
END FUNCTION opt_setNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getNumber(obj, name, value) RESULT(ans)
  CLASS(GmshOption_), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL(DFP), INTENT(OUT) :: value
  INTEGER(I4B) :: ans
  !!
  ! Internal variables
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  REAL(C_DOUBLE) :: val
  !!
  name_ = TRIM(name)//C_NULL_CHAR
  !!
  CALL gmshOptionGetNumber( &
    & name=C_LOC(name_), &
    & value=val, &
    & ierr=ans)
  !!
  value = REAL(val, KIND=DFP)
END FUNCTION opt_getNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_setString(obj, name, value) RESULT(ans)
  CLASS(GmshOption_), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: name
  CHARACTER(LEN=*), INTENT(IN) :: value
  INTEGER(I4B) :: ans
  !
  ! Internal variables
  CHARACTER(LEN=maxStrLen), TARGET :: name_, value_
  !
  name_ = TRIM(name)//C_NULL_CHAR
  value_ = TRIM(value)//C_NULL_CHAR
  !
  CALL gmshOptionSetString(name=C_LOC(name_), &
    & value=C_LOC(value_), ierr=ans)
  !
END FUNCTION opt_setString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getString(obj, name, value) RESULT(ans)
  CLASS(GmshOption_), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: name
  CHARACTER(LEN=*), INTENT(OUT) :: value
  INTEGER(I4B) :: ans
  !
  ! Internal variables
  !
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  TYPE(C_PTR) :: value_
  !
  name_ = TRIM(name)//C_NULL_CHAR
  CALL gmshOptionGetString(name=C_LOC(name_), &
    & value=value_, ierr=ans)
  CALL C2Fortran(C_String=value_, F_String=value)
  !
END FUNCTION opt_getString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_setColor(obj, name, r, g, b, a) RESULT(ans)
  CLASS(GmshOption_), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(I4B), INTENT(IN) :: r, g, b, a
  INTEGER(I4B) :: ans
  !
  ! Internal variables
  !
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  !
  name_ = gmsh_CString(name)
  CALL gmshOptionSetColor( &
    & name=C_LOC(name_), &
    & r=gmsh_cint(r), &
    & g=gmsh_cint(g), &
    & b=gmsh_cint(b), &
    & a=gmsh_cint(a), &
    & ierr=ans)
  !!
END FUNCTION opt_setColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getColor(obj, name, r, g, b, a) RESULT(ans)
  CLASS(GmshOption_), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(I4B), INTENT(OUT) :: r, g, b, a
  INTEGER(I4B) :: ans
  !
  ! Internal variables
  !
  CHARACTER(LEN=maxStrLen), TARGET :: name_
  INTEGER(C_INT) :: r0, g0, b0, a0
  !!
  name_ = gmsh_CString(name)
  CALL gmshOptionGetColor( &
    & name=C_LOC(name_), &
    & r=r0, g=g0, b=b0, a=a0, ierr=ans)
  !!
  r = INT(r0, KIND=I4B)
  g = INT(g0, KIND=I4B)
  b = INT(b0, KIND=I4B)
  a = INT(a0, KIND=I4B)
END FUNCTION opt_getColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshOption_Class
