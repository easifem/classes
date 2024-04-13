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
USE GlobalData, ONLY: DFP => REAL64, I4B => INT32, LGT
USE GmshInterface
USE GmshUtility
USE CInterface
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "GMSHOPTION_CLASS"
INTEGER(C_INT) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER(I4B), PARAMETER :: maxStrLen = 256
PUBLIC :: GmshOption_
PUBLIC :: GmshOptionPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshOption_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, NOPASS :: Initiate => opt_Initiate
  PROCEDURE, PUBLIC, NOPASS :: setNumber => opt_setNumber
  PROCEDURE, PUBLIC, NOPASS :: getNumber => opt_getNumber
  PROCEDURE, PUBLIC, NOPASS :: setString => opt_setString
  PROCEDURE, PUBLIC, NOPASS :: getString => opt_getString
  PROCEDURE, PUBLIC, NOPASS :: setColor => opt_setColor
  PROCEDURE, PUBLIC, NOPASS :: getColor => opt_getColor
END TYPE GmshOption_

TYPE(GmshOption_), PUBLIC, PARAMETER :: TypeGmshOption = GmshOption_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshOptionPointer_
  CLASS(GmshOption_), POINTER :: Ptr => NULL()
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE opt_Initiate()
END SUBROUTINE opt_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_setNumber(name, VALUE) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  CLASS(*), INTENT(IN) :: VALUE
  INTEGER(I4B) :: ans

  ! Internal variables
  CHARACTER(maxStrLen), TARGET :: name_

  name_ = TRIM(name)//C_NULL_CHAR
  CALL gmshOptionSetNumber( &
    & name=C_LOC(name_), &
    & VALUE=gmsh_cdouble(VALUE), &
    & ierr=ans)
  ! ans = INT( ierr, KIND=I4B)
END FUNCTION opt_setNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getNumber(name, VALUE) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  REAL(DFP), INTENT(OUT) :: VALUE
  INTEGER(I4B) :: ans

  ! Internal variables
  CHARACTER(maxStrLen), TARGET :: name_
  REAL(C_DOUBLE) :: val

  name_ = TRIM(name)//C_NULL_CHAR

  CALL gmshOptionGetNumber( &
    & name=C_LOC(name_), &
    & VALUE=val, &
    & ierr=ans)

  VALUE = REAL(val, KIND=DFP)
END FUNCTION opt_getNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_setString(name, VALUE) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(*), INTENT(IN) :: VALUE
  INTEGER(I4B) :: ans

  ! Internal variables
  CHARACTER(maxStrLen), TARGET :: name_, value_

  name_ = TRIM(name)//C_NULL_CHAR
  value_ = TRIM(VALUE)//C_NULL_CHAR

  CALL gmshOptionSetString(name=C_LOC(name_), &
    & VALUE=C_LOC(value_), ierr=ans)
END FUNCTION opt_setString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getString(name, VALUE) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(*), INTENT(OUT) :: VALUE
  INTEGER(I4B) :: ans
 
  ! Internal variables
  CHARACTER(maxStrLen), TARGET :: name_
  TYPE(C_PTR) :: value_
  
  name_ = TRIM(name)//C_NULL_CHAR
  CALL gmshOptionGetString(name=C_LOC(name_), &
    & VALUE=value_, ierr=ans)
  CALL C2Fortran(C_String=value_, F_String=VALUE)
END FUNCTION opt_getString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_setColor(name, r, g, b, a) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B), INTENT(IN) :: r, g, b, a
  INTEGER(I4B) :: ans
  
  ! Internal variables
  CHARACTER(maxStrLen), TARGET :: name_
  
  name_ = gmsh_CString(name)
  CALL gmshOptionSetColor( &
    & name=C_LOC(name_), &
    & r=gmsh_cint(r), &
    & g=gmsh_cint(g), &
    & b=gmsh_cint(b), &
    & a=gmsh_cint(a), &
    & ierr=ans)
END FUNCTION opt_setColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getColor(name, r, g, b, a) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B), INTENT(OUT) :: r, g, b, a
  INTEGER(I4B) :: ans
  
  ! Internal variables
  CHARACTER(maxStrLen), TARGET :: name_
  INTEGER(C_INT) :: r0, g0, b0, a0
 
  name_ = gmsh_CString(name)
  CALL gmshOptionGetColor( &
    & name=C_LOC(name_), &
    & r=r0, g=g0, b=b0, a=a0, ierr=ans)

  r = INT(r0, KIND=I4B)
  g = INT(g0, KIND=I4B)
  b = INT(b0, KIND=I4B)
  a = INT(a0, KIND=I4B)
END FUNCTION opt_getColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshOption_Class
