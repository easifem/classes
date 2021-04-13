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

    PROCEDURE, PASS( Obj ) :: NewDefault
    PROCEDURE, PASS( Obj ) :: NewCopyOther
    PROCEDURE, PASS( Obj ) :: NewFill
    PROCEDURE, PASS( Obj ) :: NewFromArray
    PROCEDURE, PASS( Obj ) :: NewFromIteratorPair

    PROCEDURE, PASS( Obj ) :: FixValuePtrs
    PROCEDURE, PUBLIC, PASS( Obj ) :: PushBack
    PROCEDURE, PUBLIC, PASS( Obj ) :: PushFront
    PROCEDURE, PUBLIC, PASS( Obj ) :: PopFront
    PROCEDURE, PUBLIC, PASS( Obj ) :: PopBack

    PROCEDURE, PUBLIC, PASS( Obj ) :: Delete => Delete_Obj
    PROCEDURE, PUBLIC, PASS( Obj ) :: Size => SizeList
    PROCEDURE, PUBLIC, PASS( Obj ) :: isEmpty => Empty
    FINAL :: Finalizer
    PROCEDURE, PUBLIC, PASS( Obj ) :: Begin => BeginList
    PROCEDURE, PUBLIC, PASS( Obj ) :: End => EndList

    PROCEDURE, PASS( Obj ) :: AssignOther
    PROCEDURE, PASS( Obj ) :: AssignArray

    PROCEDURE, PASS( Obj ) :: InsertSingle
    PROCEDURE, PASS( Obj ) :: InsertFill
    PROCEDURE, PASS( Obj ) :: InsertArray
    PROCEDURE, PASS( Obj ) :: InsertIteratorPair

    PROCEDURE, PASS( Obj ) :: EraseSingle
    PROCEDURE, PASS( Obj ) :: EraseIteratorPair

    PROCEDURE, PASS( Obj ), PUBLIC :: Resize
    PROCEDURE, PASS( Obj ), PUBLIC :: Clear

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
  MODULE PROCEDURE Display_Iterator, Display_Obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewDefault( Obj )
  CLASS( ftlListElement_ ), INTENT(INOUT), TARGET :: Obj
  CALL Obj%Delete()
  Obj%sentinel%next => Obj%sentinel
  Obj%sentinel%prev => Obj%sentinel
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE NewCopyOther(Obj, other)
  CLASS(ftlListElement_), INTENT(INOUT) :: Obj
  TYPE(ftlListElement_), INTENT(IN) :: other

  ! Internal variable
  TYPE(ftlListElementIterator_) :: it, iend

  CALL Obj%New()
  it = other%Begin()
  iend = other%End()
  DO WHILE( it .NE. iend )
    CALL Obj%Add(it%value)
    CALL it%Inc()
  ENDDO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewFill( Obj, n, val )
  CLASS( ftlListElement_ ), INTENT(INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: n
  CLASS(Element_) , TARGET, OPTIONAL, INTENT( IN ) :: val

  ! Internal variables
  INTEGER( I4B ) :: i

  CALL Obj%New()
  IF( PRESENT( val ) ) THEN
    DO i = 1, n
      CALL Obj%Add(val)
    END DO
  ELSE
    STOP 'TODO: Implement ftlList%NewFill without val'
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewFromArray(Obj, array)
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  CLASS(Element_) , TARGET, INTENT( IN ) :: array(:)

  ! Internal variable
  INTEGER( I4B ) :: i, n

  CALL Obj%New(); n = SIZE( array )
  DO i = 1, n
    CALL Obj%Add(array(i))
  END DO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE NewFromIteratorPair( Obj, first, last )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  TYPE(ftlListElementIterator_), INTENT( IN ) :: first
  TYPE(ftlListElementIterator_), INTENT( IN ) :: last

  CALL Obj%New()
  CALL Obj%Insert( Obj%Begin(), first, last )
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

SUBROUTINE FixValuePtrs( Obj )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj

  IF( Obj%psize == 0 ) THEN
    NULLIFY( Obj%front, Obj%back )
  ELSE
    SELECT TYPE( first => Obj%sentinel%next )
      TYPE IS( DataNode_ )
      Obj%front => first%data
    END SELECT
    SELECT TYPE( last => Obj%sentinel%prev )
      TYPE IS( DataNode_ )
      Obj%back => last%data
    END SELECT
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE PushBack( Obj, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  CLASS(Element_) , INTENT( IN ) :: val

  CALL InsertNodeAfter( Obj%sentinel%prev, val )
  Obj%psize = Obj%psize + 1
  CALL Obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE PushFront( Obj, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ), TARGET :: Obj
  CLASS(Element_) , TARGET, INTENT( IN ) :: val

  CALL InsertNodeBefore( Obj%sentinel%next, val )
  Obj%psize = Obj%psize + 1
  CALL Obj%FixValuePtrs()
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

FUNCTION PopFront( Obj ) RESULT( Ans )
  CLASS( ftlListElement_ ), INTENT(INOUT), TARGET :: Obj

  CLASS(Element_), POINTER :: Ans




  ! Internal variables
  CLASS( ListNode_ ), POINTER :: oldfirst

  oldfirst => Obj%sentinel%next


  Ans => Obj%front








  Obj%psize = Obj%psize - 1
  CALL UnlinkNode( oldfirst )
  DEALLOCATE( oldfirst )
  CALL Obj%FixValuePtrs()
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION PopBack(Obj) RESULT( Ans )
  CLASS(ftlListElement_), INTENT(INOUT), TARGET :: Obj

  CLASS(Element_), POINTER :: Ans




  ! Define internal variable
  CLASS(ListNode_), POINTER :: oldlast
  oldlast => Obj%sentinel%prev


  Ans => Obj%back








  Obj%psize = Obj%psize - 1
  CALL UnlinkNode(oldlast)
  DEALLOCATE(oldlast)
  CALL Obj%FixValuePtrs()
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE Delete_Obj( Obj )
  CLASS( ftlListElement_ ), INTENT(INOUT), TARGET :: Obj

  ! Internal variables
  CLASS( ListNode_ ), POINTER :: walker, deletor

  walker => Obj%sentinel%next
  DO WHILE( ASSOCIATED( walker ) .AND. .NOT. ASSOCIATED(walker,Obj%sentinel) )
    deletor => walker
    walker => walker%next
    DEALLOCATE(deletor)
  END DO
  Obj%psize = 0
  NULLIFY( Obj%sentinel%prev )
  NULLIFY( Obj%sentinel%next )
  NULLIFY( Obj%front )
  NULLIFY( Obj%back )
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION SizeList( Obj ) RESULT( Size )
  CLASS( ftlListElement_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Size
  Size = Obj%psize
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION Empty(Obj) RESULT( Ans )
  CLASS(ftlListElement_), INTENT(in) :: Obj
  LOGICAL( LGT ) :: Ans
  Ans = (Obj%psize == 0)
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE Finalizer( Obj )
  TYPE( ftlListElement_ ), INTENT( INOUT ) :: Obj
  CALL Obj%Delete()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION BeginList( Obj ) RESULT( Begin )
  CLASS( ftlListElement_ ), INTENT( IN ), TARGET :: Obj
  TYPE( ftlListElementIterator_ ) :: Begin

  Begin%node => Obj%sentinel%next
  SELECT TYPE( node => Begin%node )
    TYPE IS ( DataNode_ )
    Begin%value => node%data
  END SELECT
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION EndList( Obj ) result( End )
  CLASS( ftlListElement_ ), INTENT( IN ), TARGET :: Obj
  TYPE( ftlListElementIterator_ ) :: End
  End%node => Obj%sentinel
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! TODO: implement using existing list nodes instead of copy construction
!
IMPURE ELEMENTAL SUBROUTINE AssignOther(Obj, other)
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  TYPE( ftlListElement_ ), INTENT( IN ) :: other

  ! Define internal variables
  TYPE( ftlListElementIterator_ ) :: it
  INTEGER( I4B ) :: i, n

  CALL Obj%New()
  i = 1; n = other%Size()
  it = other%Begin()
  DO WHILE( i .LE. n )
    CALL Obj%add( it%value )
    i = i + 1
    CALL it%Inc()
  END DO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE AssignArray(Obj, array)
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  CLASS(Element_) , TARGET, INTENT( IN ) :: array(:)
  CALL Obj%New(array)
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertSingle( Obj, position, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  TYPE( ftlListElementIterator_ ) :: position
  CLASS(Element_), TARGET, INTENT( IN ) :: val

  call Obj%InsertFill( position, 1, val )
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

SUBROUTINE InsertFill( Obj, position, n, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  TYPE( ftlListElementIterator_ ) :: position
  INTEGER( I4B ), INTENT( IN ) :: n
  CLASS(Element_) , TARGET, INTENT( IN ) :: val

  INTEGER( I4B ) :: i

  DO i = 1, n
    CALL InsertNodeBefore(position%node, val)
  END DO
  Obj%psize = Obj%psize + n
  CALL Obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertArray( Obj, position, array )
  CLASS( ftlListElement_ ), INTENT(INOUT) :: Obj
  TYPE( ftlListElementIterator_ ) :: position
  CLASS(Element_), INTENT( IN )    :: array(:)

  INTEGER( I4B ) :: i, n

  n = SIZE( array )
  DO i = 1, n
    CALL InsertNodeBefore( position%node, array(i) )
  END DO
  Obj%psize = Obj%psize + n
  CALL Obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertIteratorPair( Obj, position, first, last )
  CLASS( ftlListElement_ ) , INTENT( INOUT ) :: Obj
  TYPE( ftlListElementIterator_ ) :: position
  TYPE( ftlListElementIterator_ ), INTENT( IN ) :: first
  TYPE( ftlListElementIterator_ ), INTENT( IN ) :: last

  ! Define internal variable
  TYPE( ftlListElementIterator_ ) :: it

  it = first
  DO WHILE( it .NE. last)
    CALL InsertNodeBefore( position%node, it%value )
    Obj%psize = Obj%psize + 1
    CALL it%Inc()
  END DO
  CALL Obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE EraseSingle( Obj, position )
  CLASS( ftlListElement_ ), INTENT(INOUT) :: Obj
  TYPE( ftlListElementIterator_ ) :: position

  CALL UnlinkNode(position%node)
  DEALLOCATE(position%node)
  Obj%psize = Obj%psize - 1
  CALL Obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE EraseIteratorPair( Obj, first, last )
  CLASS( ftlListElement_ ) , INTENT( INOUT ) :: Obj
  TYPE( ftlListElementIterator_ ) :: first
  TYPE(ftlListElementIterator_), INTENT( IN ) :: last

  ! Define internal variables
  TYPE(ftlListElementIterator_) :: deletor

  ASSOCIATE( walker => first )
    DO WHILE( walker .NE. last )
      deletor = walker
      CALL walker%Inc()
      CALL Obj%EraseSingle(deletor)
    END DO
  END ASSOCIATE
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SwapList( Obj, other )
  TYPE( ftlListElement_ ), INTENT( INOUT ), TARGET :: Obj
  TYPE( ftlListElement_ ), INTENT( INOUT ), TARGET :: other

  ! Define internal variables
  INTEGER( I4B ) :: tmpSize
  TYPE( ListNode_ ) :: tmpNode
  !
  ! fix pointers from data nodes to the sentinels
  Obj%sentinel%prev%next => other%sentinel
  Obj%sentinel%next%prev => other%sentinel
  other%sentinel%prev%next => Obj%sentinel
  other%sentinel%next%prev => Obj%sentinel
  ! exchange sentinels themselves
  tmpNode = Obj%sentinel
  tmpSize = Obj%psize
  Obj%sentinel = other%sentinel
  Obj%psize = other%psize
  other%sentinel = tmpNode
  other%psize = tmpSize
  ! fix front/back pointers for both lists
  CALL Obj%FixValuePtrs()
  CALL other%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Resize( Obj, n, val )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ) , INTENT( IN ) :: n
  CLASS(Element_) , TARGET, INTENT( IN ), OPTIONAL :: val
  !
  TYPE( ftlListElementIterator_ ) :: it
  INTEGER( I4B ) :: i
  !
  IF (n == Obj%psize) THEN
    RETURN
  ELSE IF (n < Obj%psize) THEN
    it = Obj%Begin()
    DO i = 2, n
      CALL it%Inc()
    END DO
    CALL it%Inc()
    CALL Obj%Erase(it,Obj%End())
  ELSE ! n > Obj%psize
    DO i = 1, n - Obj%psize
      CALL InsertNodeAfter(Obj%sentinel%prev, val)
    END DO
  END IF
  !
  Obj%psize = n
  CALL Obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Clear( Obj )
  CLASS( ftlListElement_ ), INTENT( INOUT ) :: Obj
  CALL Obj%New()
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

SUBROUTINE NewItCopyOther( Obj, other )
  CLASS(ftlListElementIterator_), INTENT( OUT ) :: Obj
  CLASS(ftlListElementIterator_), INTENT( IN ) :: other

  Obj%node => other%node
  SELECT TYPE( node => Obj%node )
    TYPE IS( DataNode_ )
    Obj%value => node%data
  END SELECT
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! todo
! check the bounds
! if bounds are crossed raise error
RECURSIVE SUBROUTINE Inc(Obj, n)
  CLASS( ftlListElementIterator_ ), INTENT(INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n

  ! Define internal variables
  INTEGER( I4B ) :: i
  IF( PRESENT( n ) ) THEN
    DO i = 1, n
      CALL Inc( Obj )
    END DO
  ELSE
    Obj%node => Obj%node%next
    SELECT TYPE( node => Obj%node )
      TYPE IS( DataNode_ )
      Obj%value => node%data
    END SELECT
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


! todo
! check the bounds
! if bounds are crossed raise error
RECURSIVE SUBROUTINE Dec(Obj, n)
  CLASS( ftlListElementIterator_ ), INTENT(INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n

  ! Define internal variable
  INTEGER( I4B ) :: i

  IF( PRESENT( n ) ) THEN
    DO i = 1, n
      CALL Dec( Obj )
    END DO
  ELSE
    Obj%node => Obj%node%prev
    SELECT TYPE( node => Obj%node )
      TYPE IS( DataNode_ )
      Obj%value => node%data
    END SELECT
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION EqualOther( Obj, other ) RESULT( Ans )
  CLASS(ftlListElementIterator_), INTENT(in) :: Obj
  CLASS(ftlListElementIterator_), INTENT(in) :: other
  LOGICAL( LGT ) :: Ans
  Ans = ASSOCIATED( Obj%node,other%node )
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION UnequalOther( Obj, other ) RESULT( Ans )
  CLASS( ftlListElementIterator_ ), INTENT( IN ) :: Obj
  CLASS( ftlListElementIterator_ ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: Ans
  Ans = .NOT. ASSOCIATED( Obj%node,other%node )
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE  Display_Iterator( Obj, Msg, UnitNo )
  CLASS( ftlListElementIterator_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  IF( ASSOCIATED( Obj%value ) ) THEN
    CALL Display( Obj%value, Msg, UnitNo )
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE  Display_Obj( Obj, Msg, UnitNo )
  CLASS( ftlListElement_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo

  ! Define internal variables
  INTEGER( I4B ) :: i, ii
  TYPE( ftlListElementIterator_ ) :: it, last

  i = Input(stdout, UnitNo )
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( i, "(A)" ) "#" // TRIM( Msg )
  END IF

  IF( Obj%isEmpty() ) THEN
    WRITE( i, "(A)" ) "List is Empty"
    RETURN
  END IF

  it = Obj%Begin()
  last = Obj%End()
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

