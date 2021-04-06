# 1 "StringList_Class.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "StringList_Class.f90"
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

MODULE StringList_Class
USE GlobalData
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE

TYPE( ExceptionHandler_ ), PUBLIC, SAVE :: eStringListType





# 1 "../../List/src/TempList_Class.inc" 1

!----------------------------------------------------------------------------
!                                                            StringListNode_
!----------------------------------------------------------------------------

TYPE StringListNode_
  TYPE( String ) :: data
  TYPE( StringListNode_ ), POINTER :: next =>null()
END TYPE StringListNode_

PUBLIC :: StringListNode_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE StringList_
  TYPE( StringListNode_ ), POINTER, PRIVATE :: head => null()
  TYPE( StringListNode_ ), POINTER, PRIVATE :: tail => null()
  TYPE( StringListNode_ ), POINTER, PRIVATE :: iter => null()
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
END TYPE StringList_

PUBLIC :: StringList_

!----------------------------------------------------------------------------
!                                                           Finalize@Method
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE list_final( Obj )
  CLASS( StringList_ ), INTENT( INOUT) :: Obj
END SUBROUTINE list_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_add( Obj, data )
  CLASS( StringList_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: data
END SUBROUTINE list_add
END INTERFACE


!----------------------------------------------------------------------------
!                                                       nextIter@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_nextIter( Obj, data )
  CLASS( StringList_ ), INTENT( INOUT) :: Obj
  TYPE( String ), INTENT (OUT) :: data
END SUBROUTINE list_nextIter
END INTERFACE

!----------------------------------------------------------------------------
!                                                          resetIter@Method
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_resetIter( Obj )
  CLASS( StringList_ ), INTENT( INOUT) :: Obj
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION list_size( Obj ) RESULT( Ans )
  CLASS( StringList_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION list_size
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_gethead( Obj, data )
  CLASS( StringList_ ), INTENT( IN ) :: Obj
  TYPE( String ), INTENT( OUT ) :: data
END SUBROUTINE list_gethead
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_gettail( Obj, data )
  CLASS( StringList_ ), INTENT( IN ) :: Obj
  TYPE( String ), INTENT (OUT) :: data
END SUBROUTINE list_gettail
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_getNth( Obj, n, data )
  CLASS( StringList_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: n
  TYPE( String ), INTENT (OUT) ::  data
END SUBROUTINE list_getNth
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_popTail( Obj, data )
  CLASS( StringList_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( OUT ) :: data
END SUBROUTINE list_popTail
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_popHead( Obj, data )
  CLASS( StringList_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( OUT ) :: data
END SUBROUTINE list_popHead
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_push( Obj, data )
  CLASS( StringList_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: data
END SUBROUTINE list_push
END INTERFACE
# 30 "StringList_Class.f90" 2

TYPE( StringListNode_ ), PARAMETER, PUBLIC :: TypeStringListNode = StringListNode_(data=NULL)
TYPE( StringList_ ), PARAMETER, PUBLIC :: TypeStringList = StringList_()

TYPE StringListNodePointer_
  CLASS( StringListNode_ ), POINTER :: ptr => NULL()
END TYPE StringListNodePointer_

PUBLIC :: StringListNodePointer_


TYPE StringListPointer_
  CLASS( StringList_ ), POINTER :: ptr => NULL( )
END TYPE StringListPointer_

PUBLIC :: StringListPointer_

END MODULE StringList_Class

