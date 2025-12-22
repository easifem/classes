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

SUBMODULE(AbstractDomain_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(obj%isInit, "AbstractDomain_::obj Initiated: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL Display("engine: "//obj%engine, unitno=unitno)
CALL Display("majorVersion: "//ToString(obj%majorVersion), unitno=unitno)
CALL Display("minorVersion: "//ToString(obj%minorVersion), unitno=unitno)
CALL Display("version: "//ToString(obj%version), unitno=unitno)
CALL Display("nsd: "//ToString(obj%nsd), unitno=unitno)
CALL Display("maxNptrs: "//ToString(obj%maxNptrs), unitno=unitno)
CALL Display("minNptrs: "//ToString(obj%minNptrs), unitno=unitno)
CALL Display("tNodes: "//ToString(obj%tNodes), unitno=unitno)
CALL Display(obj%isNodeNumberSparse, "isNodeNumberSparse: ", unitno=unitno)
CALL Display("maxElemNum: "//ToString(obj%maxElemNum), unitno=unitno)
CALL Display("minElemNum: "//ToString(obj%minElemNum), unitno=unitno)
CALL Display(obj%isElemNumberSparse, "isElemNumberSparse: ", unitno=unitno)
CALL Display("tEntitiesForNodes: "//ToString(obj%tEntitiesForNodes), &
             unitno=unitno)
CALL Display("tEntitiesForElements: "//ToString(obj%tEntitiesForElements), &
             unitno=unitno)
CALL Display("tElements: "//ToString(obj%tElements), unitno=unitno)
CALL Display("tEntities: "//ToString(obj%tEntities), unitno=unitno)
abool = ALLOCATED(obj%nodeCoord)
CALL Display(abool, "nodeCoord Allocated: ", unitno=unitno)
abool = ASSOCIATED(obj%kdtree)
CALL Display(abool, "kdtree Associated: ", unitno=unitno)
abool = ALLOCATED(obj%kdresult)
CALL Display(abool, "kdresult Allocated: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                          DisplaDomainInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayDomainInfo
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_DisplayDomainInfo()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(obj%isInit, "AbstractDomain_::obj Initiated: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL Display(obj%isInit, "isInitiated: ", unitno=unitno)
CALL Display("engine: "//obj%engine, unitno=unitno)
CALL Display("version: "//ToString(obj%version), unitno=unitno)
CALL Display("nsd: "//ToString(obj%nsd), unitno=unitno)
CALL Display("minNptrs: "//ToString(obj%minNptrs), unitno=unitno)
CALL Display("maxNptrs: "//ToString(obj%maxNptrs), unitno=unitno)
CALL Display("minElemNum: "//ToString(obj%minElemNum), unitno=unitno)
CALL Display("maxElemNum: "//ToString(obj%maxElemNum), unitno=unitno)
CALL Display("tNodes: "//ToString(obj%tNodes), unitno=unitno)
CALL Display("tEntitiesForNodes: "//ToString(obj%tEntitiesForNodes), &
             unitno=unitno)
CALL Display("tEntitiesForElements: "//ToString(obj%tEntitiesForElements), &
             unitno=unitno)
CALL Display("tElements: "//ToString(obj%tElements), unitno=unitno)
CALL Display("total mesh of volume: "//ToString(obj%tEntities(3)), &
             unitno=unitno)
CALL Display("total mesh of surface: "//ToString(obj%tEntities(2)), &
             unitno=unitno)
CALL Display("total mesh of curve: "//ToString(obj%tEntities(1)), &
             unitno=unitno)
CALL Display("total mesh of point: "//ToString(obj%tEntities(0)), &
             unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_DisplayDomainInfo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
