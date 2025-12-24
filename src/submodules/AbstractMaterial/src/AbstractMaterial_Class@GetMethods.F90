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
USE Display_Method, ONLY: ToString
USE HashTables, ONLY: Hashkey
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                           GetMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaterialPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaterialPointer()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

matPtr => NULL()

#ifdef DEBUG_VER
isok = obj%IsMaterialPresent(name)
CALL AssertError1(isok, myName, &
                  name//' material not found please use AddMaterial first.')
#endif

indx = 0
CALL obj%tbl%Get(Hashkey(name), indx)

#ifdef DEBUG_VER
isok = indx .LE. obj%tProperties
CALL AssertError1(isok, myName, &
          'indx='//ToString(indx)//' is out of bound for obj%tProperties='// &
                  ToString(obj%tProperties))
#endif

matPtr => obj%matProps(indx)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaterialPointer

!----------------------------------------------------------------------------
!                                                       IsMaterialPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsMaterialPresent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsMaterialPresent()"
#endif
INTEGER(I4B) :: stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%tbl%check_key(key=Hashkey(name), stat=stat)
ans = stat .EQ. 0_I4B

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsMaterialPresent

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
