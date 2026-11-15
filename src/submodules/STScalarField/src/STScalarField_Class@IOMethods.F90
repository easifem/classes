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

SUBMODULE(STScalarField_Class) IOMethods
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldDisplay
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldReadBCFromToml
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldReadFEDOFFromToml
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldReadOptsFromToml
USE Display_Method, ONLY: Display
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "STScalarField_Class@IOMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
LOGICAL(LGT) :: isok
#endif

CHARACTER(:), ALLOCATABLE :: name, engine
INTEGER(I4B) :: fieldType, spaceCompo, timeCompo
LOGICAL(LGT) :: isSpaceCompo, isTimeCompo

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldReadOptsFromToml( &
  table=table, name=name, engine=engine, fieldType=fieldType, &
  spaceCompo=spaceCompo, isSpaceCompo=isSpaceCompo, timeCompo=timeCompo, &
  isTimeCompo=isTimeCompo)

#ifdef DEBUG_VER
isok = isTimeCompo .OR. PRESENT(timefedof)
CALL AssertError1(isok, myName, &
                  "timeCompo is not found in the table and timefedof &
                  &is not present")
#endif

CALL AbstractNodeFieldReadFEDOFFromToml( &
  table=table, fedof=fedof, geofedof=geofedof, timefedof=timefedof, &
  dom=dom)

CALL obj%Initiate(name=name, engine=engine, fedof=fedof, &
                  geofedof=geofedof, fieldType=fieldType, &
                  timeCompo=timeCompo, timefedof=timefedof)

CALL AbstractNodeFieldReadBCFromToml(obj=obj, table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldDisplay(obj=obj, msg=msg, unitno=unitno)
CALL Display(obj%timeCompo, msg="timeCompo: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
