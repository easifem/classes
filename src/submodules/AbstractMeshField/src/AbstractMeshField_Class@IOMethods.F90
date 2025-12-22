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

SUBMODULE(AbstractMeshField_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
USE SafeSizeUtility, ONLY: SafeSize
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif
LOGICAL(LGT) :: bool1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(obj%isInit, 'isInit: ', unitno=unitno)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display('name: '//obj%name%chars(), unitno=unitno)
CALL Display('prefix: '//obj%GetPrefix(), unitno=unitno)

CALL Display('fieldType: '//typefield%ToString(obj%fieldType), &
             unitno=unitno)

CALL Display('engine: '//obj%engine%chars(), unitno=unitno)

CALL Display(obj%tSize, 'tSize: ', unitno=unitno)

IF (obj%defineOn .EQ. fevaropt%nodal) THEN
  CALL Display('defineOn: Nodal', unitno=unitno)
ELSE
  CALL Display('defineOn: Quadrature', unitno=unitno)
END IF

CALL Display('rank: '//typefield%RankToString(obj%rank), unitno=unitno)
CALL Display('varType: '//typefield%ToString(obj%varType), &
             unitno=unitno)

bool1 = ALLOCATED(obj%val)
CALL Display(bool1, 'val ALLOCATED: ', unitno=unitno)
CALL Display(SafeSize(obj%val), "Size of val:", unitno=unitno)
#ifdef DEBUG_VER
IF (bool1) THEN
  CALL Display(obj%val, 'val: ', unitno=unitno)
END IF
#endif

bool1 = ALLOCATED(obj%indxVal)
CALL Display(bool1, 'indxVal ALLOCATED: ', unitno=unitno)
CALL Display(SafeSize(obj%indxVal), "Size of indxVal:", unitno=unitno)
#ifdef DEBUG_VER
IF (bool1) THEN
  CALL Display(obj%indxVal, 'indxVal: ', unitno=unitno)
END IF
#endif

CALL Display(obj%totalShape, 'totalShape: ', unitno=unitno)

bool1 = ALLOCATED(obj%ss)
CALL Display(bool1, 'ss ALLOCATED: ', unitno=unitno)
CALL Display(SafeSize(obj%ss), "Size of ss:", unitno=unitno)
#ifdef DEBUG_VER
IF (bool1) THEN
  CALL Display(obj%ss, 'ss: ', unitno=unitno)
END IF
#endif

bool1 = ALLOCATED(obj%indxShape)
CALL Display(bool1, 'indxShape ALLOCATED: ', unitno=unitno)
CALL Display(SafeSize(obj%indxShape), "Size of indxShape:", unitno=unitno)
#ifdef DEBUG_VER
IF (bool1) THEN
  CALL Display(obj%indxShape, 'indxShape: ', unitno=unitno)
END IF
#endif

bool1 = ASSOCIATED(obj%mesh)
CALL Display(bool1, 'mesh ASSOCIATED: ', unitno=unitno)

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
