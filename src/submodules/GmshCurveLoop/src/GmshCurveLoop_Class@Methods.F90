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

SUBMODULE(GmshCurveLoop_Class) Methods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "GmshCurveLoop_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(curveId)
CALL Reallocate(obj%curveId, tsize)
obj%curveId(1:tsize) = curveId(1:tsize)
obj%indx = indx
obj%reorient = reorient

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 SetCurveId
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCurveId
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCurveId()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(curveId)
CALL Reallocate(obj%curveId, tsize)
obj%curveId(1:tsize) = curveId(1:tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCurveId

!----------------------------------------------------------------------------
!                                                                    SetIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIndx
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIndx()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%indx = indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIndx

!----------------------------------------------------------------------------
!                                                                SetReorient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetReorient
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetReorient()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%reorient = reorient

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetReorient

!----------------------------------------------------------------------------
!                                                                 GetCurveId
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCurveId
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCurveId()"
#endif

INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SafeSize(obj%curveId)
CALL Reallocate(ans, tsize)

DO ii = 1, tsize
  ans(ii) = obj%curveId(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCurveId

!----------------------------------------------------------------------------
!                                                                    GetIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndx
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetIndx()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetIndx

!----------------------------------------------------------------------------
!                                                                GetReorient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetReorient
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetReorient()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%reorient

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetReorient

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%indx, "indx: ", unitno=unitno)
CALL Display(obj%reorient, "reorient: ", unitno=unitno)
CALL Display(obj%curveId, "curveId: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                        Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%indx = obj2%indx
obj%reorient = obj2%reorient

tsize = SafeSize(obj2%curveId)
CALL Reallocate(obj%curveId, tsize)
DO ii = 1, tsize
  obj%curveId(ii) = obj2%curveId(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

END SUBMODULE Methods
