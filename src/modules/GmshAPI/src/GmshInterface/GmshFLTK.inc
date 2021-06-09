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
! along with this program.  If not, see <https: //www.gnu.org/licenses/
!

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Create the FLTK graphical user interface. Can only be called in the main
!  * thread.
!
! GMSH_API void gmshFltkInitialize(int *ierr);

INTERFACE
SUBROUTINE gmshFltkInitialize(ierr) &
  & BIND(C, name="gmshFltkInitialize")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkInitialize
END INTERFACE

PUBLIC :: gmshFltkInitialize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Wait at most `time' seconds for user interface events and return. If `time'
! < 0, wait indefinitely. First automatically create the user interface if it
! has not yet been initialized. Can only be called in the main thread. */
!
! GMSH_API void gmshFltkWait(const double time,
!                            int *ierr);

INTERFACE
SUBROUTINE gmshFltkWait(time, ierr) &
  & BIND(C, name="gmshFltkWait")
  IMPORT
  _R_V_IN_ :: time
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkWait
END INTERFACE

PUBLIC :: gmshFltkWait

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Update the user interface (potentially creating new widgets and windows).
! First automatically create the user interface if it has not yet been
! initialized. Can only be called in the main thread: use `awake("update")'
! to trigger an update of the user interface from another thread. */
!
! GMSH_API void gmshFltkUpdate(int *ierr);

INTERFACE
SUBROUTINE gmshFltkUpdate(ierr) &
  & BIND(C, name="gmshFltkUpdate")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkUpdate
END INTERFACE

PUBLIC :: gmshFltkUpdate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Awake the main user interface thread and process pending events, and
! optionally perform an action (currently the only `action' allowed is
! "update").
!
! GMSH_API void gmshFltkAwake(const char *action,
!                             int *ierr);

INTERFACE
SUBROUTINE gmshFltkAwake(action, ierr) &
  & BIND(C, name="gmshFltkAwake")
  IMPORT
  _CPTR_V_IN_ :: action
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkAwake
END INTERFACE

PUBLIC :: gmshFltkAwake

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Block the current thread until it can safely modify the user interface. */
!
! GMSH_API void gmshFltkLock(int *ierr);

INTERFACE
SUBROUTINE gmshFltkLock(ierr) &
  & BIND(C, name="gmshFltkLock")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkLock
END INTERFACE

PUBLIC :: gmshFltkLock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Release the lock that was set using lock. */
!
! GMSH_API void gmshFltkUnlock(int *ierr);

INTERFACE
SUBROUTINE gmshFltkUnlock(ierr) &
  & BIND(C, name="gmshFltkUnlock")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkUnlock
END INTERFACE

PUBLIC :: gmshFltkUnlock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Run the event loop of the graphical user interface, i.e. repeatedly call
! `wait()'. First automatically create the user interface if it has not yet
! been initialized. Can only be called in the main thread. */
!
! GMSH_API void gmshFltkRun(int *ierr);

INTERFACE
SUBROUTINE gmshFltkRun(ierr) &
  & BIND(C, name="gmshFltkRun")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkRun
END INTERFACE

PUBLIC :: gmshFltkRun

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Check if the user interface is available (e.g. to detect if it has been
! closed).
!
! GMSH_API int gmshFltkIsAvailable(int *ierr);

INTERFACE
FUNCTION gmshFltkIsAvailable(ierr) RESULT( ans ) &
  & BIND(C, name="gmshFltkIsAvailable")
  IMPORT
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshFltkIsAvailable
END INTERFACE

PUBLIC :: gmshFltkIsAvailable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select entities in the user interface. If `dim' is >= 0, return only the
! entities of the specified dimension (e.g. points if `dim' == 0). */
!
! GMSH_API int gmshFltkSelectEntities(int **dimTags, size_t *dimTags_n,
!                                     const int dim,
!                                     int *ierr);

INTERFACE
FUNCTION gmshFltkSelectEntities(dimTags, dimTags_n, dim, ierr) &
  & RESULT( ans ) &
  & BIND(C, name="gmshFltkSelectEntities")
  IMPORT
  _ST_OUT_ :: dimTags_n
  _CPTR_IN_ :: dimTags
  _I_V_IN_ :: dim
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshFltkSelectEntities
END INTERFACE

PUBLIC :: gmshFltkSelectEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select elements in the user interface. */
!
! GMSH_API int gmshFltkSelectElements(size_t **elementTags, size_t *elementTags_n,
!                                     int *ierr);

INTERFACE
FUNCTION gmshFltkSelectElements(elementTags, elementTags_n, ierr) &
  & RESULT( ans ) &
  & BIND(C, name="gmshFltkSelectElements")
  IMPORT
  _CPTR_IN_ :: elementTags
  _ST_OUT_ :: elementTags_n
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshFltkSelectElements
END INTERFACE

PUBLIC :: gmshFltkSelectElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select views in the user interface.
!
! GMSH_API int gmshFltkSelectViews(int **viewTags, size_t *viewTags_n,
!                                  int *ierr);

INTERFACE
FUNCTION gmshFltkSelectViews(viewTags, viewTags_n, ierr) &
  & RESULT( ans ) &
  & BIND(C, name="gmshFltkSelectViews")
  IMPORT
  _CPTR_IN_ :: viewTags
  _ST_OUT_ :: viewTags_n
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshFltkSelectViews
END INTERFACE

PUBLIC :: gmshFltkSelectViews

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Split the current window horizontally (if `how' = "h") or vertically (if
! `how' = "v"), using ratio `ratio'. If `how' = "u", restore a single window.
!
! GMSH_API void gmshFltkSplitCurrentWindow(const char *how,
!                                          const double ratio,
!                                          int *ierr);

INTERFACE
SUBROUTINE gmshFltkSplitCurrentWindow(how, ratio, ierr) &
  & BIND(C, name="gmshFltkSplitCurrentWindow")
  IMPORT
  _CPTR_V_IN_ :: how
  _R_V_IN_ :: ratio
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkSplitCurrentWindow
END INTERFACE

PUBLIC :: gmshFltkSplitCurrentWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the current window by speficying its index (starting at 0) in the list
! of all windows. When new windows are created by splits, new windows are
! appended at the end of the list.
!
! GMSH_API void gmshFltkSetCurrentWindow(const int windowIndex,
!                                        int *ierr);

INTERFACE
SUBROUTINE gmshFltkSetCurrentWindow(windowIndex, ierr) &
  & BIND(C, name="gmshFltkSetCurrentWindow")
  IMPORT
  _I_V_IN_ :: windowIndex
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkSetCurrentWindow
END INTERFACE

PUBLIC :: gmshFltkSetCurrentWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a status message in the current window. If `graphics' is set, display
! the message inside the graphic window instead of the status bar. */
!
! GMSH_API void gmshFltkSetStatusMessage(const char *message,
!                                        const int graphics,
!                                        int *ierr);

INTERFACE
SUBROUTINE gmshFltkSetStatusMessage(message, graphics, ierr) &
  & BIND(C, name="gmshFltkSetStatusMessage")
  IMPORT
  _CPTR_V_IN_ :: message
  _I_V_IN_ :: graphics
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkSetStatusMessage
END INTERFACE

PUBLIC :: gmshFltkSetStatusMessage

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Show context window for the entity of dimension `dim' and tag `tag'.
!
! GMSH_API void gmshFltkShowContextWindow(const int dim,
!                                         const int tag,
!                                         int *ierr);

INTERFACE
SUBROUTINE gmshFltkShowContextWindow(dim, tag, ierr) &
  & BIND(C, name="gmshFltkShowContextWindow")
  IMPORT
  _I_V_IN_ :: dim, tag
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkShowContextWindow
END INTERFACE

PUBLIC :: gmshFltkShowContextWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Open the `name' item in the menu tree. */
!
! GMSH_API void gmshFltkOpenTreeItem(const char *name,
!                                    int *ierr);

INTERFACE
SUBROUTINE gmshFltkOpenTreeItem(name, ierr) &
  & BIND(C, name="gmshFltkOpenTreeItem")
  IMPORT
  _CPTR_V_IN_ :: name
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkOpenTreeItem
END INTERFACE

PUBLIC :: gmshFltkOpenTreeItem

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Close the `name' item in the menu tree. */
!
! GMSH_API void gmshFltkCloseTreeItem(const char *name,
!                                     int *ierr);

INTERFACE
SUBROUTINE gmshFltkCloseTreeItem(name, ierr) &
  & BIND(C, name="gmshFltkCloseTreeItem")
  IMPORT
  _CPTR_V_IN_ :: name
  _I_OUT_ :: ierr
END SUBROUTINE gmshFltkCloseTreeItem
END INTERFACE

PUBLIC :: gmshFltkCloseTreeItem