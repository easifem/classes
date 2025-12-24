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

SUBMODULE(AbstractMaterial_Class) SetMethods
USE Display_Method, only: ToString
USE HashTables, ONLY: Hashkey
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               AddMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddMaterial1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddMaterial1()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, &
                  'AbstractMaterial_::obj is not initated ')
#endif

#ifdef DEBUG_VER
isok = .NOT. (obj%IsMaterialPresent(name))
CALL AssertError1(isok, myName, &
                  'The material name='//name//' is already present!')
#endif

obj%tProperties = obj%tProperties + 1

CALL obj%tbl%set(Hashkey(name), obj%tProperties)

tsize = 0
isok = ALLOCATED(obj%matProps)
IF (isok) tsize = SIZE(obj%matProps)

isok = obj%tProperties .LE. tsize
IF (.NOT. isok) CALL obj%ExpandMatProps()

ALLOCATE (obj%matProps(obj%tProperties)%ptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddMaterial1

!----------------------------------------------------------------------------
!                                                                AddMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddMaterial2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddMaterial2()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(name)
DO ii = 1, tsize
  CALL obj%AddMaterial(name=name(ii)%chars())
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddMaterial2

!----------------------------------------------------------------------------
!                                                           ExpandMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExpandMatProps
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExpandMatProps()"
#endif

LOGICAL(LGT) :: isok
TYPE(UserFunctionPointer_), ALLOCATABLE :: temp(:)
INTEGER(I4B) :: oldSize, newSize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%matProps)
IF (.NOT. isok) THEN
  ALLOCATE (obj%matProps(thresholdSize))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

oldSize = SIZE(obj%matProps)

IF (oldSize .LE. thresholdSize) THEN
  newSize = INT(expandScale1 * oldSize, kind=I4B)
ELSE
  newSize = INT(expandScale2 * oldSize, kind=I4B)
END IF

ALLOCATE (temp(oldSize))

DO ii = 1, oldSize
  temp(ii)%ptr => obj%matProps(ii)%ptr
  obj%matProps(ii)%ptr => NULL()
END DO

DEALLOCATE (obj%matProps)
ALLOCATE (obj%matProps(newSize))

DO ii = 1, oldSize
  obj%matProps(ii)%ptr => temp(ii)%ptr
  temp(ii)%ptr => NULL()
END DO

DEALLOCATE (temp)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ExpandMatProps

!----------------------------------------------------------------------------
!                                                                     SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%name = name

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetName

!----------------------------------------------------------------------------
!                                                              Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
