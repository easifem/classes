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

SUBMODULE(AbstractBC_Class) ConstructorMethods
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%isUserFunction = .FALSE.
obj%isNormal = .FALSE.
obj%isTangent = .FALSE.
obj%isUseExternal = .FALSE.
obj%isElemToFace = .FALSE.
obj%isElemToEdge = .FALSE.

obj%name = ''

obj%idof = 0
obj%nodalValueType = -1
obj%nrow = 0
obj%ncol = 0
obj%tElemToFace = 0
obj%tElemToEdge = 0

isok = ALLOCATED(obj%nodalValue)
IF (isok) DEALLOCATE (obj%nodalValue)

isok = ALLOCATED(obj%nodenum)
IF (isok) DEALLOCATE (obj%nodenum)

isok = ALLOCATED(obj%elemToFace)
IF (isok) DEALLOCATE (obj%elemToFace)

isok = ALLOCATED(obj%elemToEdge)
IF (isok) DEALLOCATE (obj%elemToEdge)

CALL obj%boundary%DEALLOCATE()

obj%dom => NULL()

isok = ASSOCIATED(obj%func)
IF (isok) THEN
  CALL obj%func%DEALLOCATE()
  DEALLOCATE (obj%func)
END IF

obj%func => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%boundary = boundary
obj%dom => dom

obj%name = Input(option=name, default=default_name)
obj%idof = Input(option=idof, default=default_idof)
obj%nodalValueType = Input(option=nodalValueType, &
                           default=default_nodalValueType)

obj%isNormal = Input(option=isNormal, default=default_isNormal)
obj%isTangent = Input(option=isTangent, default=default_isTangent)
obj%isUseExternal = Input(option=isUseExternal, default=default_useExternal)
obj%isUserFunction = Input(option=isUserFunction, &
                           default=default_isUserFunction)

#ifdef DEBUG_VER
isok = obj%isNormal .AND. (obj%idof .EQ. 0)
CALL AssertError1(isok, myName, &
                  'When isNormal is true, idof CANNOT be greater than 0.')
#endif

#ifdef DEBUG_VER
isok = obj%isTangent .AND. (obj%idof .EQ. 0)
CALL AssertError1(isok, myName, &
                  'When isTangent is true, idof CANNOT be greater than 0.')
#endif

#ifdef DEBUG_VER
CALL obj%boundary%GetParam(isSelectionByMeshID=isok)
isok = isok .AND. (.NOT. obj%isUserFunction)
isok = isok .AND. (.NOT. obj%isUseExternal)
isok = isok .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%constant)

IF (isok) THEN
  CALL e%RaiseWarning(modName//'::'//myName//" - "// &
                      "When meshSelection is by MeshID and `isUserFunction` &
                      &is false, then `nodalValueType` in `AbstractBC_` &
                      &object should be Constant.")
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
