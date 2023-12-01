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

SUBMODULE(AbstractMaterial_Class) GetMethods
USE BaseMethod
USE Fhash, ONLY: key => fhash_key
USE fhash_tbl, ONLY: FHASH_KEY_NOT_FOUND
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE am_GetPrefix
ans = myprefix
END PROCEDURE am_GetPrefix

!----------------------------------------------------------------------------
!                                                           GetMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE am_GetMaterialPointer
CHARACTER(*), PARAMETER :: myName = "am_GetMaterialPointer()"
LOGICAL(LGT) :: isOK
INTEGER(I4B) :: indx

matPtr => NULL()
isOK = obj%IsMaterialPresent(name)

IF (.NOT. isOK) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: '//name//" material not found"//  &
    & CHAR_LF//' please use AddMaterial first.')
  RETURN
END IF

indx = 0
CALL obj%tbl%Get(key(name), indx)

isOK = indx .LE. obj%tProperties

IF (.NOT. isOK) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: out of bound index.')
  RETURN
END IF

matPtr => obj%matProps(indx)%ptr

END PROCEDURE am_GetMaterialPointer

!----------------------------------------------------------------------------
!                                                       IsMaterialPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE am_IsMaterialPresent
INTEGER(I4B) :: stat
CALL obj%tbl%check_key(key=key(name), stat=stat)
IF (stat .EQ. 0_I4B) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE am_IsMaterialPresent
END SUBMODULE GetMethods
