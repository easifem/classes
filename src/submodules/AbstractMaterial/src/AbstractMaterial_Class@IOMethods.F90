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

SUBMODULE(AbstractMaterial_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
USE HashTables, ONLY: HashKey, &
                      HashTableIter_, &
                      HashKey_, &
                      HashTableIter

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: matalloc, isok
TYPE(HashTableIter_) :: iter
CLASS(HashKey_), ALLOCATABLE :: ikey
CLASS(*), ALLOCATABLE :: idata

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isInit, "isInitiated: ", unitNo=unitNo)
IF (.NOT. obj%isInit) RETURN

CALL Display("name: "//obj%name, unitNo=unitNo)
CALL Display(obj%tProperties, "tProperties: ", unitNo=unitNo)

iter = HashTableIter(obj%tbl)

DO WHILE (iter%next(ikey, idata))

  SELECT TYPE (d => idata)
  TYPE IS (INTEGER)
    CALL Display('property ('//tostring(d)//'): '//ikey%to_string(), &
                 unitNo=unitNo)
  END SELECT

END DO

matalloc = ALLOCATED(obj%matProps)

CALL Display(matalloc, "matProps ALLOCATED: ", unitNo=unitNo)

IF (.NOT. matalloc) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

tsize = SIZE(obj%matProps)

DO ii = 1, tsize
  isok = ASSOCIATED(obj%matProps(ii)%ptr)
  IF (isok) THEN
    CALL obj%matProps(ii)%ptr%Display("material Properties("// &
                                      ToString(ii)//"):", unitNo=unitNo)
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
