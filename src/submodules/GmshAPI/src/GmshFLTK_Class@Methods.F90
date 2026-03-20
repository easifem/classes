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
USE GmshInterface, ONLY: GMSH_API_MAX_STR_LEN
USE GmshInterface, ONLY: GmshFltkInitialize
USE GmshInterface, ONLY: GmshFltkWait
USE GmshInterface, ONLY: GmshFltkUpdate
USE GmshInterface, ONLY: GmshFltkAwake
USE GmshInterface, ONLY: GmshFltkUnlock
USE GmshInterface, ONLY: GmshFltkLock
USE GmshInterface, ONLY: GmshFltkRun
USE GmshInterface, ONLY: GmshFltkIsAvailable
USE GmshInterface, ONLY: GmshFltkSelectEntities
USE GmshInterface, ONLY: GmshFltkSelectElements
USE GmshInterface, ONLY: GmshFltkSelectViews
USE GmshInterface, ONLY: GmshFltkSplitCurrentWindow
USE GmshInterface, ONLY: GmshFltkSetCurrentWindow
USE GmshInterface, ONLY: GmshFltkSetStatusMessage
USE GmshInterface, ONLY: GmshFltkShowContextWindow
USE GmshInterface, ONLY: GmshFltkOpenTreeItem
USE GmshInterface, ONLY: GmshFltkCloseTreeItem
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE GmshUtility, ONLY: gmsh_opt_cdouble
USE GmshUtility, ONLY: gmsh_CString
USE GmshUtility, ONLY: gmsh_opt_cint
USE GmshUtility, ONLY: gmsh_dimtag_c2f
USE GmshUtility, ONLY: gmsh_intvec_c2f
USE GmshUtility, ONLY: gmsh_cint
USE CInterface, ONLY: optval_c_bool

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN
CHARACTER(*), PARAMETER :: modName = "GMSHFLTK_CLASS"
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

INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ierr = obj%Initialize()

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
  time=gmsh_opt_cdouble(option=time0, default=math%minus_one_i), &
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

action_ = gmsh_CString(Input(option=action, default=""))
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

CALL GmshFltkRun(ierr)
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
          dim=gmsh_opt_cint(default=math%minus_one_i, option=dim), &
          ierr=ierr)

ans = gmsh_dimtag_c2f(cptr, dimTags_n)

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

ans = gmsh_intvec_c2f(cptr, elementTags_n)

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
ans = gmsh_intvec_c2f(cptr, viewTags_n)

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

how_ = gmsh_CString(how)

CALL GmshFltkSplitCurrentWindow( &
  how=how_, ratio=gmsh_opt_cdouble(option=ratio, default=math%half), &
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
  windowIndex=gmsh_opt_cint(option=windowIndex, default=math%zero_i), &
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

message_ = gmsh_CString(message)

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

CALL GmshFltkShowContextWindow(dim=gmsh_cint(dim), &
                               tag=gmsh_cint(tag), &
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

name_ = gmsh_CString(name)
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

name_ = gmsh_CString(name)
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
