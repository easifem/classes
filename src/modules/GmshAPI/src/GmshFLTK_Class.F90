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

MODULE GmshFLTK_Class
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshFLTK_
PUBLIC :: TypeGmshFLTK
PUBLIC :: GmshFLTKPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshFLTK_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, NOPASS :: Initialize => obj_Initialize
  PROCEDURE, PUBLIC, NOPASS :: Wait => obj_Wait
  PROCEDURE, PUBLIC, NOPASS :: Update => obj_Update
  PROCEDURE, PUBLIC, NOPASS :: Awake => obj_Awake
  PROCEDURE, PUBLIC, NOPASS :: Lock => obj_Lock
  PROCEDURE, PUBLIC, NOPASS :: Unlock => obj_Unlock
  PROCEDURE, PUBLIC, NOPASS :: Run => obj_Run
  PROCEDURE, PUBLIC, NOPASS :: IsAvailable => obj_IsAvailable
  PROCEDURE, PUBLIC, NOPASS :: SelectEntities => obj_SelectEntities
  PROCEDURE, PUBLIC, NOPASS :: SelectElements => obj_SelectElements
  PROCEDURE, PUBLIC, NOPASS :: SelectViews => obj_SelectViews
  PROCEDURE, PUBLIC, NOPASS :: SplitCurrentWindow => &
    obj_SplitCurrentWindow
  PROCEDURE, PUBLIC, NOPASS :: SetStatusMessage => obj_SetStatusMessage
  PROCEDURE, PUBLIC, NOPASS :: ShowContextWindow => obj_ShowContextWindow
  PROCEDURE, PUBLIC, NOPASS :: OpenTreeItem => obj_OpenTreeItem
  PROCEDURE, PUBLIC, NOPASS :: CloseTreeItem => obj_CloseTreeItem
END TYPE GmshFLTK_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(GmshFLTK_), PARAMETER :: TypeGmshFLTK = GmshFLTK_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshFLTKPointer_
  CLASS(GmshFLTK_), POINTER :: Ptr => NULL()
END TYPE GmshFLTKPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(GmshFLTK_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Create the FLTK graphical user interface. Can only be called in the main
!  * thread.

INTERFACE
  MODULE FUNCTION obj_Initialize() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION obj_Initialize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Wait at most `time' seconds for user interface events and return. If `time'
! < 0, wait indefinitely. First automatically create the user interface if it
! has not yet been initialized. Can only be called in the main thread.

INTERFACE
  MODULE FUNCTION obj_Wait(time0) RESULT(ans)
    CLASS(*), OPTIONAL, INTENT(IN) :: time0
    INTEGER(I4B) :: ans
  END FUNCTION obj_Wait
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Update the user interface (potentially creating new widgets and windows).
! First automatically create the user interface if it has not yet been
! initialized. Can only be called in the main thread: use `awake("update")'
! to trigger an update of the user interface from another thread. */

INTERFACE
  MODULE FUNCTION obj_Update() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION obj_Update
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Awake the main user interface thread and process pending events, and
! optionally perform an action (currently the only `action' allowed is
! "update").

INTERFACE
  MODULE FUNCTION obj_Awake(action) RESULT(ans)
    INTEGER(I4B) :: ans
    CHARACTER(*), OPTIONAL, INTENT(IN) :: action
  END FUNCTION obj_Awake
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Block the current thread until it can safely modify the user interface. */

INTERFACE
  MODULE FUNCTION obj_Lock() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION obj_Lock
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Release the lock that was set using lock. */

INTERFACE
  MODULE FUNCTION obj_Unlock() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION obj_Unlock
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Run the event loop of the graphical user interface, i.e. repeatedly call
! `wait()'. First automatically create the user interface if it has not yet
! been initialized. Can only be called in the main thread. */

INTERFACE
  MODULE FUNCTION obj_Run(optionFileName) RESULT(ans)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: optionFileName
    INTEGER(I4B) :: ans
  END FUNCTION obj_Run
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Check if the user interface is available (e.g. to detect if it has been
! closed).

INTERFACE
  MODULE FUNCTION obj_IsAvailable() RESULT(ans)
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsAvailable
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select entities in the user interface. If `dim' is >= 0, return only the
! entities of the specified dimension (e.g. points if `dim' == 0).

INTERFACE
  MODULE FUNCTION obj_SelectEntities(dim) RESULT(ans)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! dimTags
  END FUNCTION obj_SelectEntities
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select elements in the user interface. */

INTERFACE
  MODULE FUNCTION obj_SelectElements() RESULT(ans)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !!  elementTags
  END FUNCTION obj_SelectElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select views in the user interface.

INTERFACE
  MODULE FUNCTION obj_SelectViews() RESULT(ans)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! viewTags
  END FUNCTION obj_SelectViews
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Split the current window horizontally (if `how' = "h") or vertically (if
! `how' = "v"), using ratio `ratio'. If `how' = "u", restore a single window.

INTERFACE
  MODULE FUNCTION obj_SplitCurrentWindow(how, ratio) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: how
    CLASS(*), OPTIONAL, INTENT(IN) :: ratio
    INTEGER(I4B) :: ans
  END FUNCTION obj_SplitCurrentWindow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the current window by speficying its index (starting at 0) in the list
! of all windows. When new windows are created by splits, new windows are
! appended at the end of the list.

INTERFACE
  MODULE FUNCTION obj_SetCurrentWindow(windowIndex) RESULT(ans)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: windowIndex
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetCurrentWindow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a status message in the current window. If `graphics' is set, display
! the message inside the graphic window instead of the status bar.

INTERFACE
  MODULE FUNCTION obj_SetStatusMessage(message, graphics) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: message
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: graphics
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetStatusMessage
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Show context window for the entity of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION obj_ShowContextWindow(dim, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_ShowContextWindow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Open the `name' item in the menu tree. */
!
! GMSH_API void gmshFltkOpenTreeItem(const char *name,
!                                    int *ierr);

INTERFACE
  MODULE FUNCTION obj_OpenTreeItem(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_OpenTreeItem
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Close the `name' item in the menu tree. */
!
! GMSH_API void gmshFltkCloseTreeItem(const char *name,
!                                     int *ierr);

INTERFACE
  MODULE FUNCTION obj_CloseTreeItem(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_CloseTreeItem
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshFLTK_Class
