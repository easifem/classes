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

SUBMODULE(GmshPhysicalGroup_Class) Methods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "GmshPhysicalGroup_Class@Methods.F90"
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

obj%dim = dim
obj%name = name
obj%indx = indx

tsize = SIZE(tags)
CALL Reallocate(obj%tags, tsize)
obj%tags(1:tsize) = tags(1:tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                    SetDim
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetDim
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetDim()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%dim = dim

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetDim

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
!                                                                    SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%name = name

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetName

!----------------------------------------------------------------------------
!                                                                    SetTags
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTags
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTags()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(tags)
CALL Reallocate(obj%tags, tsize)
obj%tags(1:tsize) = tags(1:tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTags

!----------------------------------------------------------------------------
!                                                                     GetDim
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDim
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDim()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%dim

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDim

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
!                                                                    GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%name%Chars()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetName

!----------------------------------------------------------------------------
!                                                                    GetTags
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTags
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTags()"
#endif

INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SafeSize(obj%tags)
CALL Reallocate(ans, tsize)

DO ii = 1, tsize
  ans(ii) = obj%tags(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTags

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
CALL obj%name%Display("name: ", unitno=unitno)
CALL Display(obj%dim, "dim: ", unitno=unitno)
CALL Display(obj%tags, "tags: ", unitno=unitno)

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

obj%dim = obj2%dim
obj%indx = obj2%indx
obj%name = obj2%name

tsize = SafeSize(obj2%tags)
CALL Reallocate(obj%tags, tsize)
DO ii = 1, tsize
  obj%tags(ii) = obj2%tags(ii)
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
