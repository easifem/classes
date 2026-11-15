! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(StfemAlgorithm2FactoryUtility) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE String_Class, ONLY: String
USE StringUtility, ONLY: UpperCase
USE TomlUtility, ONLY: GetValue
USE UStfemAlgorithm2_Class, ONLY: UStfemAlgorithm2_
USE VStfemAlgorithm2_Class, ONLY: VStfemAlgorithm2_

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "StfemAlgorithm2FactoryUtility@Methods.F90"
#endif

CONTAINS

MODULE PROCEDURE StfemAlgorithm2Factory1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "StfemAlgorithm2Factory1()"
#endif

TYPE(String) :: name
LOGICAL(LGT) :: isok
INTEGER(I4B) :: stat, origin
CHARACTER(*), PARAMETER :: default_value = "UStfemAlgorithm2"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading name...')
#endif

CALL GetValue(table=table, key="name", &
              VALUE=name, default_value=default_value, &
              origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
IF (.NOT. isok) THEN
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                'name not found in toml, using default value='//default_value)
END IF
#endif

ans => StfemAlgorithm2Factory(name=name%Chars())

name = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE StfemAlgorithm2Factory1

!----------------------------------------------------------------------------
!                                                    StfemAlgorithm2Factory2
!----------------------------------------------------------------------------

MODULE PROCEDURE StfemAlgorithm2Factory2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "StfemAlgorithm2Factory2()"
#endif

CHARACTER(3) :: name0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name0 = UpperCase(name(1:3))

SELECT CASE (name0)
CASE ("UST")
  ALLOCATE (UStfemAlgorithm2_ :: ans)
CASE ("VST")
  ALLOCATE (VStfemAlgorithm2_ :: ans)
CASE DEFAULT
  ALLOCATE (UStfemAlgorithm2_ :: ans)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE StfemAlgorithm2Factory2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
