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

SUBMODULE(AbstractField_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Deallocate
  INTEGER( I4B ) :: ii
  obj%name=""
  obj%engine=""
  obj%isInitiated=.FALSE.
  obj%fieldType=0
  obj%domain => NULL()
  IF( ALLOCATED( obj%domains ) ) THEN
    DO ii = 1, SIZE( obj%domains )
      obj%domains(ii)%ptr => NULL()
    END DO
    DEALLOCATE( obj%domains )
  END IF
END PROCEDURE aField_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FIELD_TYPE_NUMBER
  !!
  SELECT CASE( TRIM(name) )
  CASE( "NORMAL" )
    ans = FIELD_TYPE_NORMAL
  CASE( "CONSTANT" )
    ans = FIELD_TYPE_CONSTANT
  CASE( "CONSTANT_SPACE" )
    ans = FIELD_TYPE_CONSTANT_SPACE
  CASE( "CONSTANT_TIME" )
    ans = FIELD_TYPE_CONSTANT_TIME
  END SELECT
END PROCEDURE FIELD_TYPE_NUMBER

END SUBMODULE Methods