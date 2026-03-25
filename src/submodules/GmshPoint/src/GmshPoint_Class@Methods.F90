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

SUBMODULE(GmshPoint_Class) Methods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "GmshPoint_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%x = x
obj%y = y
obj%z = z
obj%indx = indx
obj%meshSize = meshSize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                       SetX
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetX
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetX()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%x = x

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetX

!----------------------------------------------------------------------------
!                                                                       SetY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetY
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetY()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%y = y

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetY

!----------------------------------------------------------------------------
!                                                                       SetZ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetZ
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetZ()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%z = z

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetZ

!----------------------------------------------------------------------------
!                                                                SetMeshSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMeshSize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%meshSize = meshSize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMeshSize

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
!                                                                       GetX
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetX
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetX()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%x

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetX

!----------------------------------------------------------------------------
!                                                                       GetY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetY
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetY()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%y

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetY

!----------------------------------------------------------------------------
!                                                                       GetZ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetZ
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetZ()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%z

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetZ

!----------------------------------------------------------------------------
!                                                                GetMeshSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshSize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%meshSize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshSize

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

CALL Display(obj%x, "x: ", unitno=unitno)
CALL Display(obj%y, "y: ", unitno=unitno)
CALL Display(obj%z, "z: ", unitno=unitno)
CALL Display(obj%meshSize, "meshSize: ", unitno=unitno)
CALL Display(obj%indx, "indx: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

END SUBMODULE Methods
