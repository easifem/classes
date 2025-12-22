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

SUBMODULE(EngineFactoryUtility) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE StringUtility, ONLY: UpperCase
USE EngineOpt_Class, ONLY: TypeEngineOpt
USE NativeSerialEngine_Class, ONLY: NativeSerialEngine_
USE LisOmpEngine_Class, ONLY: LisOmpEngine_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              EngineFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE EngineFactory
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "EngineFactory()"
#endif

CHARACTER(:), ALLOCATABLE :: engine0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

engine0 = UpperCase(TRIM(engine))

SELECT CASE (engine0)

CASE (TypeEngineOpt%native_serial)
  ALLOCATE (NativeSerialEngine_ :: ans)

CASE (TypeEngineOpt%lis_omp)
  ALLOCATE (LisOmpEngine_ :: ans)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(math%no, myName, &
                    "No case found for engine "//engine0)
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE EngineFactory

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
