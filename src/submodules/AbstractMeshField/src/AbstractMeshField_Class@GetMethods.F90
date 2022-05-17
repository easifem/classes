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

SUBMODULE(AbstractMeshField_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Get
  CHARACTER( LEN = * ), PARAMETER :: myName="aField_Get"
  INTEGER( I4B ) :: iel
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    !!
    fevar%val = obj%val( :, 1 )
    fevar%s=obj%s
    fevar%defineOn=obj%defineOn
    fevar%varType=obj%varType
    fevar%rank=obj%rank
    !!
  ELSE
    !!
    IF( .NOT. PRESENT( globalElement ) ) THEN
      CALL e%raiseError(modName //'::'//myName// ' - '// &
        & 'globalElement should be present, when mesh field is not constant')
    END IF
    !!
    iel = obj%mesh%getLocalElemNumber( globalElement )
    fevar%val = obj%val( :, iel )
    fevar%s=obj%s
    fevar%defineOn=obj%defineOn
    fevar%varType=obj%varType
    fevar%rank=obj%rank
  END IF
  !!
END PROCEDURE aField_Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods