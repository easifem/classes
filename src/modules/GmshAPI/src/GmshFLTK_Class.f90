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
USE GlobalData, ONLY: DFP, I4B
USE Utility, ONLY: Reallocate
USE GmshInterface
USE C_Interface_MODULE, ONLY: C_F_STRING_PTR, C_PTR_TO_INT_VEC
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "GMSHFLTK_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER( I4B ), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshFLTK_
  CONTAINS
  PRIVATE
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initialize => fltk_Initialize
    PROCEDURE, PUBLIC, PASS( Obj ) :: Wait => fltk_Wait
    PROCEDURE, PUBLIC, PASS( Obj ) :: Update => fltk_Update
    PROCEDURE, PUBLIC, PASS( Obj ) :: Awake => fltk_Awake
    PROCEDURE, PUBLIC, PASS( Obj ) :: Lock => fltk_Lock
    PROCEDURE, PUBLIC, PASS( Obj ) :: Unlock => fltk_Unlock
    PROCEDURE, PUBLIC, PASS( Obj ) :: Run => fltk_Run
    PROCEDURE, PUBLIC, PASS( Obj ) :: IsAvailable => fltk_IsAvailable
    PROCEDURE, PUBLIC, PASS( Obj ) :: SelectEntities => fltk_SelectEntities
    PROCEDURE, PUBLIC, PASS( Obj ) :: SelectElements => fltk_SelectElements
    PROCEDURE, PUBLIC, PASS( Obj ) :: SelectViews => fltk_SelectViews
    PROCEDURE, PUBLIC, PASS( Obj ) :: SplitCurrentWindow => fltk_SplitCurrentWindow
    PROCEDURE, PUBLIC, PASS( Obj ) :: SetStatusMessage => fltk_SetStatusMessage
    PROCEDURE, PUBLIC, PASS( Obj ) ::  ShowContextWindow => Fltk_ShowContextWindow
    PROCEDURE, PUBLIC, PASS( Obj ) ::  OpenTreeItem => Fltk_OpenTreeItem
    PROCEDURE, PUBLIC, PASS( Obj ) ::  CloseTreeItem => Fltk_CloseTreeItem
END TYPE GmshFLTK_

PUBLIC :: GmshFLTK_
TYPE( GmshFLTK_ ), PUBLIC, PARAMETER :: TypeGmshFLTK = GmshFLTK_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshFLTKPointer_
  CLASS( GmshFLTK_ ), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshFLTKPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Create the FLTK graphical user interface. Can only be called in the main
!  * thread.
!
! GMSH_API void gmshFltkInitialize(int *ierr);

FUNCTION fltk_Initialize(obj) RESULT( ans )
  CLASS( GmshFltk_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshFltkInitialize(ierr);
  ans = int( ierr, i4b )
END FUNCTION fltk_Initialize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Wait at most `time' seconds for user interface events and return. If `time'
! < 0, wait indefinitely. First automatically create the user interface if it
! has not yet been initialized. Can only be called in the main thread. */
!
! GMSH_API void gmshFltkWait(const double time,
!                            int *ierr);

FUNCTION fltk_Wait(obj, time) RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: time
  INTEGER( I4B ) :: ans
  CALL gmshFltkWait(time, ierr)
  ans = int(ierr, i4b)
END FUNCTION fltk_Wait

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Update the user interface (potentially creating new widgets and windows).
! First automatically create the user interface if it has not yet been
! initialized. Can only be called in the main thread: use `awake("update")'
! to trigger an update of the user interface from another thread. */
!
! GMSH_API void gmshFltkUpdate(int *ierr);

FUNCTION fltk_Update(obj) RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshFltkUpdate(ierr)
  ans = int( ierr, i4b )
END FUNCTION fltk_Update

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Awake the main user interface thread and process pending events, and
! optionally perform an action (currently the only `action' allowed is
! "update").
!
! GMSH_API void gmshFltkAwake(const char *action,
!                             int *ierr);

FUNCTION fltk_Awake(obj, action) RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CHARACTER( LEN = * ), INTENT( IN ) :: action
  !internal
  CHARACTER( LEN = maxStrLen ), TARGET :: action_
  action_ = TRIM(action) // C_NULL_CHAR
  CALL gmshFltkAwake(C_LOC(action_), ierr)
  ans = int( ierr, i4b )
END FUNCTION fltk_Awake

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Block the current thread until it can safely modify the user interface. */
!
! GMSH_API void gmshFltkLock(int *ierr);

FUNCTION Fltk_Lock(obj) RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshFltkUnlock(ierr)
  ans = int(ierr, i4b)
END FUNCTION Fltk_Lock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Release the lock that was set using lock. */
!
! GMSH_API void gmshFltkUnlock(int *ierr);

FUNCTION Fltk_Unlock(obj) RESULT(ans)
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshFltkUnlock(ierr)
  ans = int( ierr, i4b )
END FUNCTION Fltk_Unlock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Run the event loop of the graphical user interface, i.e. repeatedly call
! `wait()'. First automatically create the user interface if it has not yet
! been initialized. Can only be called in the main thread. */
!
! GMSH_API void gmshFltkRun(int *ierr);

FUNCTION Fltk_Run(obj) RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshFltkRun(ierr)
  ans = int(ierr, i4b)
END FUNCTION Fltk_Run

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Check if the user interface is available (e.g. to detect if it has been
! closed).
!
! GMSH_API int gmshFltkIsAvailable(int *ierr);

FUNCTION Fltk_IsAvailable(obj) RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  ans = gmshFltkIsAvailable(ierr)
END FUNCTION Fltk_IsAvailable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select entities in the user interface. If `dim' is >= 0, return only the
! entities of the specified dimension (e.g. points if `dim' == 0). */
!
! GMSH_API int gmshFltkSelectEntities(int **dimTags, size_t *dimTags_n,
!                                     const int dim,
!                                     int *ierr);

FUNCTION Fltk_SelectEntities(obj, dimTags, dim) &
  RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: dimTags( : )
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ) :: ans
  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: dimTags_n
  ans = gmshFltkSelectEntities(cptr, dimTags_n, dim, ierr)
  CALL Reallocate( dimTags, int(dimTags_n, I4B) )
  CALL C_PTR_TO_INT_VEC( cptr = cptr, vec=dimTags )
END FUNCTION Fltk_SelectEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select elements in the user interface. */
!
! GMSH_API int gmshFltkSelectElements(size_t **elementTags, size_t *elementTags_n,
!                                     int *ierr);

FUNCTION Fltk_SelectElements(obj, elementTags) &
  & RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: elementTags(:)
  INTEGER( I4B ) :: ans
  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: elementTags_n
  ans = gmshFltkSelectElements(cptr, elementTags_n, ierr)
  CALL Reallocate( elementTags, int(elementTags_n, I4B) )
  CALL C_PTR_TO_INT_VEC( cptr = cptr, vec=elementTags )
END FUNCTION Fltk_SelectElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Select views in the user interface.
!
! GMSH_API int gmshFltkSelectViews(int **viewTags, size_t *viewTags_n,
!                                  int *ierr);

FUNCTION Fltk_SelectViews(obj, viewTags) &
  & RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: viewTags( : )
  INTEGER( I4B ) :: ans
  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: viewTags_n
  ans = gmshFltkSelectViews(cptr, viewTags_n, ierr)
  CALL Reallocate( viewTags, int(viewTags_n, I4B) )
  CALL C_PTR_TO_INT_VEC( cptr = cptr, vec=viewTags )
END FUNCTION Fltk_SelectViews

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Split the current window horizontally (if `how' = "h") or vertically (if
! `how' = "v"), using ratio `ratio'. If `how' = "u", restore a single window.
!
! GMSH_API void gmshFltkSplitCurrentWindow(const char *how,
!                                          const double ratio,
!                                          int *ierr);

FUNCTION Fltk_SplitCurrentWindow(obj, how, ratio) &
  & RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: how
  REAL( DFP ), INTENT( IN ) :: ratio
  INTEGER( I4B ) :: ans
  ! internal values
  CHARACTER( LEN = maxStrLen ), TARGET :: how_
  how_ = TRIM( how ) // C_NULL_CHAR
  CALL gmshFltkSplitCurrentWindow(C_LOC(how_), ratio, ierr)
  ans = int(ierr, i4b)
END FUNCTION Fltk_SplitCurrentWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the current window by speficying its index (starting at 0) in the list
! of all windows. When new windows are created by splits, new windows are
! appended at the end of the list.
!
! GMSH_API void gmshFltkSetCurrentWindow(const int windowIndex,
!                                        int *ierr);

FUNCTION Fltk_SetCurrentWindow(obj, windowIndex) &
  & RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: windowIndex
  INTEGER( I4B ) :: ans
  CALL gmshFltkSetCurrentWindow(windowIndex, ierr)
  ans = int(ierr, i4b)
END FUNCTION Fltk_SetCurrentWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a status message in the current window. If `graphics' is set, display
! the message inside the graphic window instead of the status bar. */
!
! GMSH_API void gmshFltkSetStatusMessage(const char *message,
!                                        const int graphics,
!                                        int *ierr);

FUNCTION Fltk_SetStatusMessage(obj, message, graphics) &
  & RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: message
  INTEGER( I4B ), INTENT( IN ) :: graphics
  INTEGER( I4B ) :: ans
  ! internal
  CHARACTER( LEN = maxStrLen ), TARGET :: message_
  message_ = TRIM(message) // C_NULL_CHAR
  CALL gmshFltkSetStatusMessage(C_LOC(message_), graphics, ierr)
  ans = int(ierr, i4b)
END FUNCTION Fltk_SetStatusMessage

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Show context window for the entity of dimension `dim' and tag `tag'.
!
! GMSH_API void gmshFltkShowContextWindow(const int dim,
!                                         const int tag,
!                                         int *ierr);

FUNCTION Fltk_ShowContextWindow(obj, dim, tag) &
  & RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  INTEGER( I4B ) :: ans
  ! internal
  CALL gmshFltkShowContextWindow(dim, tag, ierr)
  ans = int(ierr, i4b)
END FUNCTION Fltk_ShowContextWindow

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Open the `name' item in the menu tree. */
!
! GMSH_API void gmshFltkOpenTreeItem(const char *name,
!                                    int *ierr);

FUNCTION Fltk_OpenTreeItem(obj, name) &
  & RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) ::  name
  INTEGER( I4B ) :: ans
  ! internal
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshFltkOpenTreeItem(C_LOC(name_), ierr)
  ans = int( ierr, i4b )
END FUNCTION Fltk_OpenTreeItem

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Close the `name' item in the menu tree. */
!
! GMSH_API void gmshFltkCloseTreeItem(const char *name,
!                                     int *ierr);

FUNCTION Fltk_CloseTreeItem(obj, name) &
  & RESULT( ans )
  CLASS( GmshFLTK_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ) :: ans
! internal
  CHARACTER( LEN = maxStrLen ), TARGET :: name_
  name_ = TRIM(name) // C_NULL_CHAR
  CALL gmshFltkCloseTreeItem(C_LOC(name_), ierr)
  ans = int( ierr, i4b )
END FUNCTION Fltk_CloseTreeItem

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshFLTK_Class