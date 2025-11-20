! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(OneDimFEDOF_Class) IOMethods
USE Display_Method, ONLY: Display, ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInitiated: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL Display(obj%tdof, "tdof: ", unitno=unitno)
CALL Display(obj%maxTotalConnectivity, "maxTotalConnectivity: ", unitno=unitno)
CALL Display(obj%maxCellOrder, "maxCellOrder: ", unitno=unitno)

isok = ASSOCIATED(obj%mesh)
CALL Display(isok, "mesh ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%mesh%DisplayMeshInfo(msg="Mesh information: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%cellOrder)
CALL Display(isok, "cellOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellOrder), "cellOrder size: ", &
                       unitno=unitno)

isok = ALLOCATED(obj%cellIA)
CALL Display(isok, "cellIA ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellIA), "cellIA size: ", unitno=unitno)

isok = ASSOCIATED(obj%fe)
CALL Display(isok, "fe ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%fe%Display(msg="Finite element information: ", &
                      unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                           DisplayCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_DisplayCellOrder()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%cellOrder)
CALL Display(isok, "cellOrder ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(SIZE(obj%cellOrder), "cellOrder size: ", &
               unitno=unitno)
  CALL Display(obj%cellOrder, "cellOrder: ", unitno=unitno, full=full)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_DisplayCellOrder

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
