# 1 "ftlListElement_Class.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "ftlListElement_Class.f90"
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













# 1 "../../ftlMacros/ftlList.inc" 1
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
!
! This program is taken from Fortran Template Library. https://github.com/SCM-NV/ftl/tree/master
!
! Copyright (c) 2016, 2017  Robert Rüger
!
! This file is part of of the Fortran Template Library.
!
! The Fortran Template Library is free software: you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! The Fortran Template Library is distributed in the hope that it will be
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
! General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License along
! with the Fortran Template Library.  If not, see <http://www.gnu.org/licenses/>.

















# 1 "../../ftlMacros/./ftlMacros.inc" 1
! Copyright (c) 2016, 2017  Robert Rüger
!
! This file is part of of the Fortran Template Library.
!
! The Fortran Template Library is free software: you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! The Fortran Template Library is distributed in the hope that it will be
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
! General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License along
! with the Fortran Template Library.  If not, see <http://www.gnu.org/licenses/>.





# 33 "../../ftlMacros/./ftlMacros.inc"

# 53 "../../ftlMacros/ftlList.inc" 2

# 63 "../../ftlMacros/ftlList.inc"



! Name of the MODULE will be ftlListInt_Class, ftlListString_Class, ...
MODULE ftlListElement_Classs

USE BaseType
USE BaseMethod


USE FE


IMPLICIT NONE
PRIVATE


!----------------------------------------------------------------------------
!                                                                 ListNode_
!----------------------------------------------------------------------------

TYPE :: ListNode_
  CLASS(ListNode_), POINTER :: prev => NULL()
  CLASS(ListNode_), POINTER :: next => NULL()
END TYPE

!----------------------------------------------------------------------------
!                                                                 DataNode_
!----------------------------------------------------------------------------


TYPE, EXTENDS(ListNode_) :: DataNode_


  CLASS(Element_), POINTER :: data => NULL()




END TYPE

!----------------------------------------------------------------------------
!                                       ftlListElement_
!----------------------------------------------------------------------------

!! example ftlListInt_

TYPE, PUBLIC :: ftlListElement_
  PRIVATE
  INTEGER :: psize = 0
  TYPE(ListNode_) :: sentinel
  CLASS(Element_), POINTER, PUBLIC :: front => NULL()
  CLASS(Element_), POINTER, PUBLIC :: back => NULL()

  CONTAINS
    PRIVATE
    GENERIC, PUBLIC :: New => NewDefault, NewCopyOther, NewFill, NewFromArray, NewFromIteratorPair
    GENERIC, PUBLIC :: Initiate => NewDefault, NewCopyOther, NewFill, NewFromArray, NewFromIteratorPair
    GENERIC, PUBLIC :: ASSIGNMENT(=) => AssignOther, AssignArray
    GENERIC, PUBLIC :: Insert => InsertSingle, InsertFill, InsertArray, InsertIteratorPair
    GENERIC, PUBLIC :: Add => PushBack
    GENERIC, PUBLIC :: Append => PushBack
    GENERIC, PUBLIC :: DeallocateData => Delete
    GENERIC, PUBLIC :: Erase => EraseSingle, EraseIteratorPair

    PROCEDURE, PASS( obj ) :: NewDefault
    PROCEDURE, PASS( obj ) :: NewCopyOther
    PROCEDURE, PASS( obj ) :: NewFill
    PROCEDURE, PASS( obj ) :: NewFromArray
    PROCEDURE, PASS( obj ) :: NewFromIteratorPair

    PROCEDURE, PASS( obj ) :: FixValuePtrs
    PROCEDURE, PUBLIC, PASS( obj ) :: PushBack
    PROCEDURE, PUBLIC, PASS( obj ) :: PushFront
    PROCEDURE, PUBLIC, PASS( obj ) :: PopFront
    PROCEDURE, PUBLIC, PASS( obj ) :: PopBack

    PROCEDURE, PUBLIC, PASS( obj ) :: Delete => Delete_obj
    PROCEDURE, PUBLIC, PASS( obj ) :: Size => SizeList
    PROCEDURE, PUBLIC, PASS( obj ) :: isEmpty => Empty
    FINAL :: Finalizer
    PROCEDURE, PUBLIC, PASS( obj ) :: Begin => BeginList
    PROCEDURE, PUBLIC, PASS( obj ) :: End => EndList

    PROCEDURE, PASS( obj ) :: AssignOther
    PROCEDURE, PASS( obj ) :: AssignArray

    PROCEDURE, PASS( obj ) :: InsertSingle
    PROCEDURE, PASS( obj ) :: InsertFill
    PROCEDURE, PASS( obj ) :: InsertArray
    PROCEDURE, PASS( obj ) :: InsertIteratorPair

    PROCEDURE, PASS( obj ) :: EraseSingle
    PROCEDURE, PASS( obj ) :: EraseIteratorPair

    PROCEDURE, PASS( obj ), PUBLIC :: Resize
    PROCEDURE, PASS( obj ), PUBLIC :: Clear

END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, PUBLIC :: ftlListElementIterator_
  PRIVATE
  CLASS( ListNode_ ), POINTER :: node => NULL()
  CLASS(Element_), POINTER, PUBLIC :: value => NULL()

  CONTAINS
  PRIVATE
  PROCEDURE :: NewItDefault
  PROCEDURE :: NewItCopyOther
  GENERIC, PUBLIC :: New => NewItDefault, NewItCopyOther
  PROCEDURE, PUBLIC :: Inc
  PROCEDURE, PUBLIC :: Dec
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE OPERATOR( .EQ. )
  MODULE PROCEDURE EqualOther
END INTERFACE
PUBLIC :: OPERATOR( .EQ. )

INTERFACE OPERATOR( .NE. )
  MODULE PROCEDURE UnequalOther
END INTERFACE
PUBLIC :: OPERATOR( .NE. )

INTERFACE Size
  MODULE PROCEDURE SizeList
END INTERFACE
PUBLIC :: Size

INTERFACE ftlSwap
  MODULE PROCEDURE SwapList
END INTERFACE
PUBLIC :: ftlSwap

INTERFACE ftlMove
  MODULE PROCEDURE ftlMoveList
END INTERFACE
PUBLIC :: ftlMove

INTERFACE Display
  MODULE PROCEDURE Display_Iterator, Display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewDefault( obj )
  CLASS( ftlListElement_ ), INTENT(INOUT), TARGET :: obj
  CALL obj%Delete()
  obj%sentinel%next => obj%sentinel
  obj%sentinel%prev => obj%sentinel
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE NewCopyOther(obj, other)
  CLASS(ftlListElement_), INTENT(INOUT) :: obj
  TYPE(ftlListElement_), INTENT(IN) :: other

  ! Internal variable
  TYPE(ftlListElementIterator_) :: it, iend

  CALL obj%New()
  it = other%Begin()
  iend = other%End()
  DO WHILE( it .NE. iend )
    CALL obj%Add(it%value)
    CALL it%Inc()
  ENDDO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewFill( obj, n, val )
  CLASS( ftlListElement_ ), INTENT(INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  CLASS(Element_) , TARGET, OPTIONAL, INTENT( IN ) :: val

  ! Internal variables
  INTEGER( I4B ) :: i

  CALL obj%New()
  IF( PRESENT( val ) ) THEN
    DO i = 1, n
      CALL obj%Add(val)
    END DO
  ELSE
    STOP 'TODO: Implement ftlList%NewFill without val'
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewFromArray(obj, array)
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  CLASS(Element_) , TARGET, INTENT( IN ) :: array(:)

  ! Internal variable
  INTEGER( I4B ) :: i, n

  CALL obj%New(); n = SIZE( array )
  DO i = 1, n
    CALL obj%Add(array(i))
  END DO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewFromIteratorPair( obj, first, last )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  TYPE(ftlListElementIterator_), INTENT( IN ) :: first
  TYPE(ftlListElementIterator_), INTENT( IN ) :: last

  CALL obj%New()
  CALL obj%Insert( obj%Begin(), first, last )
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertNodeAfter(afternode, val)
  CLASS( ListNode_ ), POINTER, INTENT( INOUT ) :: afternode
  CLASS(Element_) , TARGET, OPTIONAL, INTENT( IN ) :: val

  ! Define internal variable
  CLASS( ListNode_ ), POINTER :: oldnext, newnext


  oldnext => afternode%next
  ALLOCATE(DataNode_::afternode%next)
  newnext => afternode%next
  newnext%prev => afternode
  newnext%next => oldnext
  oldnext%prev => newnext
  IF( PRESENT( val ) )THEN
    SELECT TYPE( newnext )
    TYPE IS( DataNode_ ) ! always true

    newnext%data => val

    ! newnext%data => Factory(val)



  END SELECT
  ENDIF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE FixValuePtrs( obj )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj

  IF( obj%psize == 0 ) THEN
    NULLIFY( obj%front, obj%back )
  ELSE
    SELECT TYPE( first => obj%sentinel%next )
      TYPE IS( DataNode_ )
      obj%front => first%data
    END SELECT
    SELECT TYPE( last => obj%sentinel%prev )
      TYPE IS( DataNode_ )
      obj%back => last%data
    END SELECT
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE PushBack( obj, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  CLASS(Element_) , INTENT( IN ) :: val

  CALL InsertNodeAfter( obj%sentinel%prev, val )
  obj%psize = obj%psize + 1
  CALL obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE PushFront( obj, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ), TARGET :: obj
  CLASS(Element_) , TARGET, INTENT( IN ) :: val

  CALL InsertNodeBefore( obj%sentinel%next, val )
  obj%psize = obj%psize + 1
  CALL obj%FixValuePtrs()
END SUBROUTINE


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE UnlinkNode( node )
  CLASS( ListNode_ ), INTENT( INOUT ) :: node
  node%next%prev => node%prev
  node%prev%next => node%next
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION PopFront( obj ) RESULT( ans )
  CLASS( ftlListElement_ ), INTENT(INOUT), TARGET :: obj

  CLASS(Element_), POINTER :: ans




  ! Internal variables
  CLASS( ListNode_ ), POINTER :: oldfirst

  oldfirst => obj%sentinel%next


  ans => obj%front








  obj%psize = obj%psize - 1
  CALL UnlinkNode( oldfirst )
  DEALLOCATE( oldfirst )
  CALL obj%FixValuePtrs()
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION PopBack(obj) RESULT( ans )
  CLASS(ftlListElement_), INTENT(INOUT), TARGET :: obj

  CLASS(Element_), POINTER :: ans




  ! Define internal variable
  CLASS(ListNode_), POINTER :: oldlast
  oldlast => obj%sentinel%prev


  ans => obj%back








  obj%psize = obj%psize - 1
  CALL UnlinkNode(oldlast)
  DEALLOCATE(oldlast)
  CALL obj%FixValuePtrs()
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE Delete_obj( obj )
  CLASS( ftlListElement_ ), INTENT(INOUT), TARGET :: obj

  ! Internal variables
  CLASS( ListNode_ ), POINTER :: walker, deletor

  walker => obj%sentinel%next
  DO WHILE( ASSOCIATED( walker ) .AND. .NOT. ASSOCIATED(walker,obj%sentinel) )
    deletor => walker
    walker => walker%next
    DEALLOCATE(deletor)
  END DO
  obj%psize = 0
  NULLIFY( obj%sentinel%prev )
  NULLIFY( obj%sentinel%next )
  NULLIFY( obj%front )
  NULLIFY( obj%back )
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION SizeList( obj ) RESULT( Size )
  CLASS( ftlListElement_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Size
  Size = obj%psize
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION Empty(obj) RESULT( ans )
  CLASS(ftlListElement_), INTENT(in) :: obj
  LOGICAL( LGT ) :: ans
  ans = (obj%psize == 0)
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE Finalizer( obj )
  TYPE( ftlListElement_ ), INTENT( INOUT ) :: obj
  CALL obj%Delete()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION BeginList( obj ) RESULT( Begin )
  CLASS( ftlListElement_ ), INTENT( IN ), TARGET :: obj
  TYPE( ftlListElementIterator_ ) :: Begin

  Begin%node => obj%sentinel%next
  SELECT TYPE( node => Begin%node )
    TYPE IS ( DataNode_ )
    Begin%value => node%data
  END SELECT
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION EndList( obj ) result( End )
  CLASS( ftlListElement_ ), INTENT( IN ), TARGET :: obj
  TYPE( ftlListElementIterator_ ) :: End
  End%node => obj%sentinel
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! TODO: implement using existing list nodes instead of copy construction
!
IMPURE ELEMENTAL SUBROUTINE AssignOther(obj, other)
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  TYPE( ftlListElement_ ), INTENT( IN ) :: other

  ! Define internal variables
  TYPE( ftlListElementIterator_ ) :: it
  INTEGER( I4B ) :: i, n

  CALL obj%New()
  i = 1; n = other%Size()
  it = other%Begin()
  DO WHILE( i .LE. n )
    CALL obj%add( it%value )
    i = i + 1
    CALL it%Inc()
  END DO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE AssignArray(obj, array)
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  CLASS(Element_) , TARGET, INTENT( IN ) :: array(:)
  CALL obj%New(array)
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertSingle( obj, position, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  TYPE( ftlListElementIterator_ ) :: position
  CLASS(Element_), TARGET, INTENT( IN ) :: val

  call obj%InsertFill( position, 1, val )
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertNodeBefore(beforenode, val)
  CLASS(ListNode_), POINTER, INTENT( INOUT ) :: beforenode
  CLASS(Element_), TARGET, INTENT( IN ) , OPTIONAL :: val

  ! Define internal variables
  CLASS(ListNode_), POINTER :: oldprev, newprev

  oldprev => beforenode%prev
  ALLOCATE(DataNode_::beforenode%prev)
  newprev => beforenode%prev
  newprev%next => beforenode
  newprev%prev => oldprev
  oldprev%next => newprev
  IF( PRESENT( val ) )THEN
    SELECT TYPE ( newprev )
      TYPE IS( DataNode_ )

      newprev%data => val



    END SELECT
  ENDIF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertFill( obj, position, n, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  TYPE( ftlListElementIterator_ ) :: position
  INTEGER( I4B ), INTENT( IN ) :: n
  CLASS(Element_) , TARGET, INTENT( IN ) :: val

  INTEGER( I4B ) :: i

  DO i = 1, n
    CALL InsertNodeBefore(position%node, val)
  END DO
  obj%psize = obj%psize + n
  CALL obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertArray( obj, position, array )
  CLASS( ftlListElement_ ), INTENT(INOUT) :: obj
  TYPE( ftlListElementIterator_ ) :: position
  CLASS(Element_), INTENT( IN )    :: array(:)

  INTEGER( I4B ) :: i, n

  n = SIZE( array )
  DO i = 1, n
    CALL InsertNodeBefore( position%node, array(i) )
  END DO
  obj%psize = obj%psize + n
  CALL obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertIteratorPair( obj, position, first, last )
  CLASS( ftlListElement_ ) , INTENT( INOUT ) :: obj
  TYPE( ftlListElementIterator_ ) :: position
  TYPE( ftlListElementIterator_ ), INTENT( IN ) :: first
  TYPE( ftlListElementIterator_ ), INTENT( IN ) :: last

  ! Define internal variable
  TYPE( ftlListElementIterator_ ) :: it

  it = first
  DO WHILE( it .NE. last)
    CALL InsertNodeBefore( position%node, it%value )
    obj%psize = obj%psize + 1
    CALL it%Inc()
  END DO
  CALL obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE EraseSingle( obj, position )
  CLASS( ftlListElement_ ), INTENT(INOUT) :: obj
  TYPE( ftlListElementIterator_ ) :: position

  CALL UnlinkNode(position%node)
  DEALLOCATE(position%node)
  obj%psize = obj%psize - 1
  CALL obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE EraseIteratorPair( obj, first, last )
  CLASS( ftlListElement_ ) , INTENT( INOUT ) :: obj
  TYPE( ftlListElementIterator_ ) :: first
  TYPE(ftlListElementIterator_), INTENT( IN ) :: last

  ! Define internal variables
  TYPE(ftlListElementIterator_) :: deletor

  ASSOCIATE( walker => first )
    DO WHILE( walker .NE. last )
      deletor = walker
      CALL walker%Inc()
      CALL obj%EraseSingle(deletor)
    END DO
  END ASSOCIATE
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SwapList( obj, other )
  TYPE( ftlListElement_ ), INTENT( INOUT ), TARGET :: obj
  TYPE( ftlListElement_ ), INTENT( INOUT ), TARGET :: other

  ! Define internal variables
  INTEGER( I4B ) :: tmpSize
  TYPE( ListNode_ ) :: tmpNode
  !
  ! fix pointers from data nodes to the sentinels
  obj%sentinel%prev%next => other%sentinel
  obj%sentinel%next%prev => other%sentinel
  other%sentinel%prev%next => obj%sentinel
  other%sentinel%next%prev => obj%sentinel
  ! exchange sentinels themselves
  tmpNode = obj%sentinel
  tmpSize = obj%psize
  obj%sentinel = other%sentinel
  obj%psize = other%psize
  other%sentinel = tmpNode
  other%psize = tmpSize
  ! fix front/back pointers for both lists
  CALL obj%FixValuePtrs()
  CALL other%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Resize( obj, n, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) , INTENT( IN ) :: n
  CLASS(Element_) , TARGET, INTENT( IN ), OPTIONAL :: val
  !
  TYPE( ftlListElementIterator_ ) :: it
  INTEGER( I4B ) :: i
  !
  IF (n == obj%psize) THEN
    RETURN
  ELSE IF (n < obj%psize) THEN
    it = obj%Begin()
    DO i = 2, n
      CALL it%Inc()
    END DO
    CALL it%Inc()
    CALL obj%Erase(it,obj%End())
  ELSE ! n > obj%psize
    DO i = 1, n - obj%psize
      CALL InsertNodeAfter(obj%sentinel%prev, val)
    END DO
  END IF
  !
  obj%psize = n
  CALL obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Clear( obj )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: obj
  CALL obj%New()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ftlMoveList( src, dest )
  TYPE( ftlListElement_ ), INTENT(INOUT) :: src
  TYPE( ftlListElement_ ), INTENT( OUT ), TARGET :: dest

  dest%psize = src%psize
  dest%sentinel = src%sentinel
  IF(ASSOCIATED(dest%sentinel%next)) dest%sentinel%next%prev => dest%sentinel
  IF(ASSOCIATED(dest%sentinel%prev)) dest%sentinel%prev%next => dest%sentinel
  CALL dest%FixValuePtrs()
  NULLIFY(src%sentinel%prev)
  NULLIFY(src%sentinel%next)
  NULLIFY(src%front)
  NULLIFY(src%back)
  src%psize = 0
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewItDefault(self)
  CLASS(ftlListElementIterator_), INTENT(INOUT) :: self
  NULLIFY( self%node )
  NULLIFY( self%value )
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewItCopyOther( obj, other )
  CLASS(ftlListElementIterator_), INTENT( OUT ) :: obj
  CLASS(ftlListElementIterator_), INTENT( IN ) :: other

  obj%node => other%node
  SELECT TYPE( node => obj%node )
    TYPE IS( DataNode_ )
    obj%value => node%data
  END SELECT
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! todo
! check the bounds
! if bounds are crossed raise error
RECURSIVE SUBROUTINE Inc(obj, n)
  CLASS( ftlListElementIterator_ ), INTENT(INOUT) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n

  ! Define internal variables
  INTEGER( I4B ) :: i
  IF( PRESENT( n ) ) THEN
    DO i = 1, n
      CALL Inc( obj )
    END DO
  ELSE
    obj%node => obj%node%next
    SELECT TYPE( node => obj%node )
      TYPE IS( DataNode_ )
      obj%value => node%data
    END SELECT
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


! todo
! check the bounds
! if bounds are crossed raise error
RECURSIVE SUBROUTINE Dec(obj, n)
  CLASS( ftlListElementIterator_ ), INTENT(INOUT) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n

  ! Define internal variable
  INTEGER( I4B ) :: i

  IF( PRESENT( n ) ) THEN
    DO i = 1, n
      CALL Dec( obj )
    END DO
  ELSE
    obj%node => obj%node%prev
    SELECT TYPE( node => obj%node )
      TYPE IS( DataNode_ )
      obj%value => node%data
    END SELECT
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION EqualOther( obj, other ) RESULT( ans )
  CLASS(ftlListElementIterator_), INTENT(in) :: obj
  CLASS(ftlListElementIterator_), INTENT(in) :: other
  LOGICAL( LGT ) :: ans
  ans = ASSOCIATED( obj%node,other%node )
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION UnequalOther( obj, other ) RESULT( ans )
  CLASS( ftlListElementIterator_ ), INTENT( IN ) :: obj
  CLASS( ftlListElementIterator_ ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: ans
  ans = .NOT. ASSOCIATED( obj%node,other%node )
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE  Display_Iterator( obj, Msg, UnitNo )
  CLASS( ftlListElementIterator_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  IF( ASSOCIATED( obj%value ) ) THEN
    CALL Display( obj%value, Msg, UnitNo )
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE  Display_obj( obj, Msg, UnitNo )
  CLASS( ftlListElement_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo

  ! Define internal variables
  INTEGER( I4B ) :: i, ii
  TYPE( ftlListElementIterator_ ) :: it, last

  i = Input(stdout, UnitNo )
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( i, "(A)" ) "#" // TRIM( Msg )
  END IF

  IF( obj%isEmpty() ) THEN
    WRITE( i, "(A)" ) "List is Empty"
    RETURN
  END IF

  it = obj%Begin()
  last = obj%End()
  ii = 0
  DO WHILE( it .NE. last )
    ii = ii + 1
    CALL Display( it, "("// TRIM(Str(ii, no_sign=.true.)) // "):", i )
    CALL it%inc()
  END DO
END SUBROUTINE




!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE
# 30 "ftlListElement_Class.f90" 2

