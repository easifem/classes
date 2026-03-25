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

SUBMODULE(GmshFLTK_Class) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE GmshBasicInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshFLTKInterface, ONLY: GmshFltkInitialize
USE GmshFLTKInterface, ONLY: GmshFltkWait
USE GmshFLTKInterface, ONLY: GmshFltkUpdate
USE GmshFLTKInterface, ONLY: GmshFltkAwake
USE GmshFLTKInterface, ONLY: GmshFltkUnlock
USE GmshFLTKInterface, ONLY: GmshFltkLock
USE GmshFLTKInterface, ONLY: GmshFltkRun
USE GmshFLTKInterface, ONLY: GmshFltkIsAvailable
USE GmshFLTKInterface, ONLY: GmshFltkSelectEntities
USE GmshFLTKInterface, ONLY: GmshFltkSelectElements
USE GmshFLTKInterface, ONLY: GmshFltkSelectViews
USE GmshFLTKInterface, ONLY: GmshFltkSplitCurrentWindow
USE GmshFLTKInterface, ONLY: GmshFltkSetCurrentWindow
USE GmshFLTKInterface, ONLY: GmshFltkSetStatusMessage
USE GmshFLTKInterface, ONLY: GmshFltkShowContextWindow
USE GmshFLTKInterface, ONLY: GmshFltkOpenTreeItem
USE GmshFLTKInterface, ONLY: GmshFltkCloseTreeItem
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE GmshUtility, ONLY: optval_c_double
USE GmshUtility, ONLY: istring_
USE GmshUtility, ONLY: ovectorpair_
USE GmshUtility, ONLY: ovectorint_
USE GmshUtility, ONLY: optval_c_int
USE GmshUtility, ONLY: optval_c_str
USE GmshUtility, ONLY: optval_c_bool

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN
CHARACTER(*), PARAMETER :: modName = "GmshFLTK_Class@Methods.F90"
INTEGER(C_INT) :: ierr
INTEGER(C_INT) :: cintvar

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

INTEGER(I4B) :: ierr0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ierr0 = obj%Initialize()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initialize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initialize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshFltkInitialize(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initialize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Wait
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Wait()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshFltkWait( &
  time=optval_c_double(option=time0, default=math%minus_one_i), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Wait

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Update
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Update()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshFltkUpdate(ierr)
ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Update

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Awake
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Awake()"
#endif
CHARACTER(maxStrLen), TARGET :: action_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

action_ = istring_(Input(option=action, default=""))
CALL GmshFltkAwake(action=action_, ierr=ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Awake

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Lock
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Lock()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshFltkLock(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Lock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Unlock
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Unlock()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshFltkUnlock(ierr)
ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Unlock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Run
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Run()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshFltkRun( &
  optionFileName=istring_(optval_c_str("", optionFileName)), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Run

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsAvailable
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsAvailable()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshFltkIsAvailable(ierr)

ans = cintvar .EQ. 1_C_INT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsAvailable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SelectEntities
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SelectEntities()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: dimTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshFltkSelectEntities( &
          dimTags=cptr, dimTags_n=dimTags_n, &
          dim=optval_c_int(default=math%minus_one_i, option=dim), &
          ierr=ierr)

ans = ovectorpair_(cptr, dimTags_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SelectEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SelectElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SelectElements()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: elementTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshFltkSelectElements( &
          elementTags=cptr, elementTags_n=elementTags_n, ierr=ierr)

ans = ovectorint_(cptr, elementTags_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SelectElements

!----------------------------------------------------------------------------
!                                                                 SelectViews
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SelectViews
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SelectViews()"
#endif
TYPE(C_PTR) :: cptr
INTEGER(C_SIZE_T) :: viewTags_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cintvar = GmshFltkSelectViews(viewTags=cptr, viewTags_n=viewTags_n, &
                              ierr=ierr)
ans = ovectorint_(cptr, viewTags_n)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SelectViews

!----------------------------------------------------------------------------
!                                                         SplitCurrentWindow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SplitCurrentWindow
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SplitCurrentWindow()"
#endif
CHARACTER(maxStrLen), TARGET :: how_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

how_ = istring_(how)

CALL GmshFltkSplitCurrentWindow( &
  how=how_, ratio=optval_c_double(option=ratio, default=math%half), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SplitCurrentWindow

!----------------------------------------------------------------------------
!                                                           SetCurrentWindow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCurrentWindow
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCurrentWindow()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshFltkSetCurrentWindow( &
  windowIndex=optval_c_int(option=windowIndex, default=math%zero_i), &
  ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCurrentWindow

!----------------------------------------------------------------------------
!                                                           SetStatusMessage
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetStatusMessage
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetStatusMessage()"
#endif
CHARACTER(maxStrLen), TARGET :: message_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

message_ = istring_(message)

CALL GmshFltkSetStatusMessage(message=message_, &
                              graphics=optval_c_bool(math%no, graphics), &
                              ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetStatusMessage

!----------------------------------------------------------------------------
!                                                           ShowContextWindow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShowContextWindow
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ShowContextWindow()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GmshFltkShowContextWindow(dim=optval_c_int(default=dim), &
                               tag=optval_c_int(default=tag), &
                               ierr=ierr)

ans = INT(ierr, I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ShowContextWindow

!----------------------------------------------------------------------------
!                                                                OpenTreeItem
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_OpenTreeItem
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_OpenTreeItem()"
#endif
CHARACTER(maxStrLen) :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = istring_(name)
CALL GmshFltkOpenTreeItem(name=name_, ierr=ierr)
ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_OpenTreeItem

!----------------------------------------------------------------------------
!                                                              CloseTreeItem
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CloseTreeItem
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CloseTreeItem()"
#endif
CHARACTER(maxStrLen) :: name_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

name_ = istring_(name)
CALL GmshFltkCloseTreeItem(name=name_, ierr=ierr)
ans = INT(ierr, i4b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CloseTreeItem

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
