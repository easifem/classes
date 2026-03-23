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

MODULE GmshFLTKInterface
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_CHAR
IMPLICIT NONE

PRIVATE
PUBLIC :: gmshFltkInitialize
PUBLIC :: gmshFltkWait
PUBLIC :: gmshFltkUpdate
PUBLIC :: gmshFltkAwake
PUBLIC :: gmshFltkLock
PUBLIC :: gmshFltkUnlock
PUBLIC :: gmshFltkRun
PUBLIC :: gmshFltkIsAvailable
PUBLIC :: gmshFltkSelectEntities
PUBLIC :: gmshFltkSelectElements
PUBLIC :: gmshFltkSelectViews
PUBLIC :: gmshFltkSplitCurrentWindow
PUBLIC :: gmshFltkSetCurrentWindow
PUBLIC :: gmshFltkSetStatusMessage
PUBLIC :: gmshFltkShowContextWindow
PUBLIC :: gmshFltkOpenTreeItem
PUBLIC :: gmshFltkCloseTreeItem

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Create the FLTK graphical user interface. Can only be called in the main
!  * thread.
!
! GMSH_API void gmshFltkInitialize(int *ierr);

INTERFACE
  SUBROUTINE gmshFltkInitialize(ierr) &
    BIND(C, name="gmshFltkInitialize")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkInitialize
END INTERFACE

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
  SUBROUTINE gmshFltkWait(time, ierr) BIND(C, name="gmshFltkWait")
    IMPORT
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: time
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkWait
END INTERFACE

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
  SUBROUTINE gmshFltkUpdate(ierr) BIND(C, name="gmshFltkUpdate")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkUpdate
END INTERFACE

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
  SUBROUTINE gmshFltkAwake(action, ierr) BIND(C, name="gmshFltkAwake")
    IMPORT
    CHARACTER(LEN=1, KIND=C_CHAR), INTENT(IN) :: action(*)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkAwake
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Block the current thread until it can safely modify the user interface. */
!
! GMSH_API void gmshFltkLock(int *ierr);

INTERFACE
  SUBROUTINE gmshFltkLock(ierr) BIND(C, name="gmshFltkLock")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkLock
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Release the lock that was set using lock. */
!
! GMSH_API void gmshFltkUnlock(int *ierr);

INTERFACE
  SUBROUTINE gmshFltkUnlock(ierr) BIND(C, name="gmshFltkUnlock")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkUnlock
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Run the event loop of the graphical user interface, i.e. repeatedly call
! `wait()'. First automatically create the user interface if it has not yet
! been initialized. Can only be called in the main thread. */
!
! GMSH_API void gmshFltkRun(int *ierr);

INTERFACE
  SUBROUTINE gmshFltkRun(optionFileName, ierr) BIND(C, name="gmshFltkRun")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), INTENT(in), OPTIONAL :: &
      optionFileName(*)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkRun
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Check if the user interface is available (e.g. to detect if it has been
! closed).
!
! GMSH_API int gmshFltkIsAvailable(int *ierr);

INTERFACE
  FUNCTION gmshFltkIsAvailable(ierr) RESULT(ans) &
    BIND(C, name="gmshFltkIsAvailable")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshFltkIsAvailable
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshFltkSelectEntities(dimTags, dimTags_n, dim, ierr) &
    RESULT(ans) BIND(C, name="gmshFltkSelectEntities")
    IMPORT
    INTEGER(C_SIZE_T), INTENT(OUT) :: dimTags_n
    TYPE(C_PTR), INTENT(IN) :: dimTags
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshFltkSelectEntities
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshFltkSelectElements(elementTags, elementTags_n, ierr) &
    RESULT(ans) BIND(C, name="gmshFltkSelectElements")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: elementTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: elementTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshFltkSelectElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshFltkSelectViews(viewTags, viewTags_n, ierr) &
    RESULT(ans) BIND(C, name="gmshFltkSelectViews")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: viewTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: viewTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshFltkSelectViews
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshFltkSplitCurrentWindow(how, ratio, ierr) &
    BIND(C, name="gmshFltkSplitCurrentWindow")
    IMPORT
    ! _CPTR_V_IN_ :: howDFP
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: how(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: ratio
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkSplitCurrentWindow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshFltkSetCurrentWindow(windowIndex, ierr) &
    BIND(C, name="gmshFltkSetCurrentWindow")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: windowIndex
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkSetCurrentWindow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshFltkSetStatusMessage(message, graphics, ierr) &
    BIND(C, name="gmshFltkSetStatusMessage")
    IMPORT
    ! _CPTR_V_IN_ :: message
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: message(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: graphics
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkSetStatusMessage
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshFltkShowContextWindow(dim, tag, ierr) &
    BIND(C, name="gmshFltkShowContextWindow")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkShowContextWindow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshFltkOpenTreeItem(name, ierr) &
    BIND(C, name="gmshFltkOpenTreeItem")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: name(*)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkOpenTreeItem
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshFltkCloseTreeItem(name, ierr) &
    BIND(C, name="gmshFltkCloseTreeItem")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: name(*)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshFltkCloseTreeItem
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshFLTKInterface
