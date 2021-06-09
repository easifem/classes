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
USE GlobalData, ONLY: DFP=> Real64, I4B=>Int32, LGT
USE GmshInterface
USE C_Interface_MODULE, ONLY: C2FStringPtr => C_F_string_ptr
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "GMSHOPTION_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER( I4B ), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshOption_
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( Obj ) :: setNumber => opt_setNumber
  PROCEDURE, PUBLIC, PASS( Obj ) :: getNumber => opt_getNumber
  PROCEDURE, PUBLIC, PASS( Obj ) :: setString => opt_setString
  PROCEDURE, PUBLIC, PASS( Obj ) :: getString => opt_getString
  PROCEDURE, PUBLIC, PASS( Obj ) :: setColor  => opt_setColor
  PROCEDURE, PUBLIC, PASS( Obj ) :: getColor  => opt_getColor
END TYPE GmshOption_

PUBLIC :: GmshOption_
TYPE( GmshOption_ ), PUBLIC, PARAMETER :: TypeGmshOption = GmshOption_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshOptionPointer_
  CLASS( GmshOption_ ), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshOptionPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION opt_setNumber(obj, name, value ) RESULT( ans )
  CLASS( GmshOption_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  CLASS( * ), INTENT( IN ) :: value
  INTEGER( I4B ) ::  ans

  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  REAL( C_DOUBLE ) :: val

  SELECT TYPE( value )
  TYPE IS ( Integer(I4B) )
    val = REAL( value, KIND=C_DOUBLE )
  TYPE IS ( Real(DFP) )
    val = REAL( value, KIND=C_DOUBLE )
  END SELECT
  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshOptionSetNumber( name=C_LOC(name_),  val=val, ierr=ans)
  ! ans = INT( ierr, KIND=I4B)
END FUNCTION opt_setNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getNumber(obj, name, value ) RESULT( ans )
  CLASS( GmshOption_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  REAL( DFP ), INTENT( OUT) :: value
  INTEGER( I4B ) ::  ans

  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_

  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshOptionGetNumber( name=C_LOC(name_),  val=value, ierr=ans )
  ! ans = INT( ierr, KIND=I4B)
END FUNCTION opt_getNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_setString(obj, name, value) RESULT( ans )
  CLASS( GmshOption_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  CHARACTER( LEN = * ), INTENT( IN ) :: value
  INTEGER( I4B ) ::  ans

  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_, value_

  name_ = TRIM(name) // C_NULL_CHAR
  value_ = TRIM(value) // C_NULL_CHAR

  CALL gmshOptionSetString( name=C_LOC(name_),  val=C_LOC(value_), ierr=ans )
  ! ans = INT( ierr, KIND=I4B )
END FUNCTION opt_setString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getString(obj, name, value) RESULT( ans )
  CLASS( GmshOption_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  CHARACTER( LEN = * ), INTENT( OUT ) :: value
  INTEGER( I4B ) ::  ans

  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  TYPE( C_PTR ) :: value_

  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshOptionGetString( name=C_LOC(name_),  val=value_, ierr=ans )
  CALL C2FStringPtr( C_String=value_, F_String=value )
  ! ans = INT( ierr, KIND=I4B )
END FUNCTION opt_getString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_setColor(obj, name, r, g, b, a ) RESULT( ans )
  CLASS( GmshOption_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ), INTENT( IN ) :: r, g, b, a
  INTEGER( I4B ) :: ans
  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshOptionSetColor( C_LOC(name_), r, g, b, a, ierr=ans )
  ! ans = INT( ierr, KIND=I4B )
END FUNCTION opt_setColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION opt_getColor(obj, name, r, g, b, a ) RESULT( ans )
  CLASS( GmshOption_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ), INTENT( OUT ) :: r, g, b, a
  INTEGER( I4B ) :: ans
  ! Internal variables
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshOptionGetColor( C_LOC(name_), r, g, b, a, ierr=ans )
  ! ans = INT( ierr, KIND=I4B )
END FUNCTION opt_getColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


END MODULE GmshOption_Class