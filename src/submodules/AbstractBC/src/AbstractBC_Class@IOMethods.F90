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

SUBMODULE(AbstractBC_Class) IOMethods
USE Display_Method, ONLY: Display
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)
REAL(DFP) :: real0
TYPE(String) :: strval
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(obj%isInit, "isInit: ", unitNo=unitNo)

IF (.NOT. obj%isInit) RETURN

CALL Display("name: "//obj%name%chars(), unitNo=unitNo)
CALL Display(obj%idof, "idof: ", unitNo=unitNo)
strval = TypeFieldOpt%ToString(obj%nodalValueType)
CALL Display("nodalValueType : "//strval%chars(), unitNo=unitNo)
CALL Display(obj%isUserFunction, "isUserFunction : ", unitNo=unitNo)
CALL Display(obj%isUseExternal, "isUseExternal: ", unitNo=unitNo)
CALL obj%boundary%Display(msg="Boundary: ", unitNo=unitNo)

IF (obj%isUserFunction) THEN
  isok = ASSOCIATED(obj%func)
  CALL Display(isok, "func Associated: ", unitNo=unitNo)
  IF (isok) CALL obj%func%Display("func: ", unitNo=unitNo)
END IF

IF (.NOT. obj%isUserFunction) THEN
  isok = ALLOCATED(obj%nodalValue)
  CALL Display(isok, "nodalValue ALLOCATED: ", unitNo=unitNo)

  IF (isok) THEN
    SELECT CASE (obj%nodalValueType)
    CASE (fevaropt%constant)
      real0 = obj%nodalValue(1, 1)
      CALL Display(real0, "nodalValue : ", unitNo=unitNo)

    CASE (fevaropt%space, fevaropt%time)
      real1 = obj%nodalValue(:, 1)
      CALL Display(real1, "nodalValue : ", unitNo=unitNo, orient="col")

    CASE (fevaropt%spaceTime)
      real2 = obj%nodalValue(:, :)
      CALL Display(real2, "nodalValue : ", unitNo=unitNo)
    END SELECT
  END IF
END IF

IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)
strval = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
