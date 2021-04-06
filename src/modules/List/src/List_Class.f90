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

MODULE List_Class
USE GlobalData
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE

TYPE( ExceptionHandler_ ), PUBLIC, SAVE :: eListType

!----------------------------------------------------------------------------
!                                                                 ListNode_
!----------------------------------------------------------------------------

TYPE ListNode_
  CLASS( * ), POINTER :: data  => NULL( )
  TYPE( ListNode_ ), POINTER :: next => NULL()
END TYPE ListNode_

PUBLIC :: ListNode_

TYPE( ListNode_ ), PARAMETER, PUBLIC :: TypeListNode = ListNode_()

TYPE ListNodePointer_
  CLASS( ListNode_ ), POINTER :: ptr => NULL( )
END TYPE ListNodePointer_

PUBLIC :: ListNodePointer_

!----------------------------------------------------------------------------
!                                                                      List_
!----------------------------------------------------------------------------

TYPE List_
  TYPE( ListNode_ ), POINTER, PRIVATE :: head => NULL( )
  TYPE( ListNode_ ), POINTER, PRIVATE :: tail => NULL( )
  TYPE( ListNode_ ), POINTER, PRIVATE :: iter => NULL( )
  INTEGER( I4B ), PRIVATE :: tSize = 0
  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => list_final
    PROCEDURE, PUBLIC, PASS( Obj ) :: add => list_add
    PROCEDURE, PUBLIC, PASS( Obj ) :: nextIter => list_nextIter
    PROCEDURE, PUBLIC, PASS( Obj ) :: resetIter => list_resetIter
    PROCEDURE, PUBLIC, PASS( Obj ) :: size => list_size
    PROCEDURE, PUBLIC, PASS( Obj ) :: gethead => list_getHead
    PROCEDURE, PUBLIC, PASS( Obj ) :: getTail => list_getTail
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNth => list_getNth
    PROCEDURE, PUBLIC, PASS( Obj ) :: Push => list_push
    PROCEDURE, PUBLIC, PASS( Obj ) :: Pop => list_popTail
    PROCEDURE, PUBLIC, PASS( Obj ) :: PopTail => list_popTail
    PROCEDURE, PUBLIC, PASS( Obj ) :: PopHead => list_popHead

END TYPE List_

PUBLIC :: List_

TYPE( List_ ), PARAMETER, PUBLIC :: TypeList = List_()

TYPE ListPointer_
  CLASS( List_ ), POINTER :: ptr => NULL( )
END TYPE ListPointer_

PUBLIC :: ListPointer_

!----------------------------------------------------------------------------
!                                                       Finalize@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE list_final( Obj )
  CLASS( List_ ), INTENT( INOUT) :: Obj
END SUBROUTINE list_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_add( Obj, data )
  CLASS( List_ ), INTENT( INOUT ) :: Obj
  CLASS( * ), INTENT( IN ) :: data
END SUBROUTINE list_add
END INTERFACE

!----------------------------------------------------------------------------
!                                                       nextIter@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_nextIter( Obj, data )
  CLASS( List_ ), INTENT( INOUT) :: Obj
  CLASS( * ), POINTER, INTENT (OUT) :: data
END SUBROUTINE list_nextIter
END INTERFACE

!----------------------------------------------------------------------------
!                                                          resetIter@Method
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_resetIter( Obj )
  CLASS( List_ ), INTENT( INOUT) :: Obj
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION list_size( Obj ) RESULT( Ans )
  CLASS( List_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION list_size
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_gethead( Obj, data )
  CLASS( List_ ), INTENT( IN ) :: Obj
  CLASS( * ), POINTER, INTENT( OUT ) :: data
END SUBROUTINE list_gethead
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_gettail( Obj, data )
  CLASS( List_ ), INTENT( IN ) :: Obj
  CLASS( * ), POINTER, INTENT (OUT) :: data
END SUBROUTINE list_gettail
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_getNth( Obj, n, data )
  CLASS( List_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: n
  CLASS( * ), POINTER, INTENT (OUT) ::  data
END SUBROUTINE list_getNth
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_popTail( Obj, data )
  CLASS( List_ ), INTENT( INOUT ) :: Obj
  CLASS( * ), POINTER, INTENT( OUT ) :: data
END SUBROUTINE list_popTail
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_popHead( Obj, data )
  CLASS( List_ ), INTENT( INOUT ) :: Obj
  CLASS( * ), POINTER, INTENT( OUT ) :: data
END SUBROUTINE list_popHead
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_push( Obj, data )
  CLASS( List_ ), INTENT( INOUT) :: Obj
  CLASS( * ), INTENT( IN ) :: data
END SUBROUTINE list_push
END INTERFACE


END MODULE List_Class