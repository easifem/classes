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

SUBMODULE(TimeFEDOF_Class) IOMethods
USE Display_Method, ONLY: Display
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
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

CALL Display(obj%isLagrange, "isLagrange: ", unitno=unitno)
CALL Display(obj%isMaxConSet, "isMaxConSet: ", unitno=unitno)
CALL Display(obj%isMaxQuadPointSet, "isMaxQuadPointSet: ", unitno=unitno)
CALL Display(obj%tdof, "tdof: ", unitno=unitno)
CALL Display(obj%maxCon, "maxCon: ", unitno=unitno)
CALL Display(obj%maxQuadPoint, "maxQuadPoint: ", unitno=unitno)
CALL Display(obj%baseContinuity, "baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation, "baseInterpolation: ", unitno=unitno)
CALL Display(obj%scaleForQuadOrder, "scaleForQuadOrder: ", unitno=unitno)
CALL Display(obj%cellOrder, "cellOrder: ", unitno=unitno)

isok = ASSOCIATED(obj%opt)
CALL Display(isok, "opt ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%opt%Display(msg="opt: ", unitno=unitno)
END IF

isok = ASSOCIATED(obj%fe)
CALL Display(isok, "fe ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%fe%Display(msg="fe: ", unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
