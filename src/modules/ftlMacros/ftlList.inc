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
! This program is taken from Fortran Template Library.
! https://github.com/SCM-NV/ftl/tree/master
!
! Copyright (c) 2016, 2017  Robert Rüger
!
! This file is part of of the Fortran Template Library.
!
! The Fortran Template Library is free software: you can redistribute it
! and/or
! modify it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation,
! either version 3 of the License, or
! (at your option) any later version.
!
! The Fortran Template Library is distributed in the hope that it will be
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
! General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along
! with the Fortran Template Library.  If not, see
! <http://www.gnu.org/licenses/>.



! #define FTL_CONTAINER ftlList
! #define FTL_CONTAINER_PROVIDES_BIDIRECTIONAL_ITERATOR

#ifdef FTL_INSTANTIATE_TEMPLATE

#if !defined(FTL_TEMPLATE_TYPE)
#error FTL_TEMPLATE_TYPE must be defined when instantiating ftlList
#endif

#if !defined(FTL_TEMPLATE_TYPE_NAME)
#error FTL_TEMPLATE_TYPE_NAME must be defined when instantiating ftlList
#endif

#include "./ftlMacros.inc"

#ifdef FTL_TEMPLATE_TYPE_IS_DERIVED

#ifdef FTL_TEMPLATE_TYPE_IS_CLASS

#define FTL_TEMPLATE_TYPE_WRAP CLASS(FTL_TEMPLATE_TYPE)
#else
#define FTL_TEMPLATE_TYPE_WRAP TYPE(FTL_TEMPLATE_TYPE)
#endif

#else

#define FTL_TEMPLATE_TYPE_WRAP FTL_TEMPLATE_TYPE

#endif

IMPLICIT NONE
PRIVATE

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
  MODULE PROCEDURE obj_Size
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
  MODULE PROCEDURE obj_Display_Iterator, obj_Display
END INTERFACE Display

PUBLIC :: Display

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

#ifdef FTL_TEMPLATE_TYPE_IS_CLASS
  FTL_TEMPLATE_TYPE_WRAP, POINTER :: data => NULL()
#else
  FTL_TEMPLATE_TYPE_WRAP :: data
#endif

END TYPE DataNode_

!----------------------------------------------------------------------------
!                                       CAT3(FTL_TEMPLATE_TYPE_NAME,List,_)
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22-01-24
! summary: 	FTLList

TYPE, PUBLIC :: CAT3(FTL_TEMPLATE_TYPE_NAME,List,_)
  PRIVATE
  INTEGER :: psize = 0
  TYPE(ListNode_) :: sentinel
  FTL_TEMPLATE_TYPE_WRAP, POINTER, PUBLIC :: front => NULL()
  FTL_TEMPLATE_TYPE_WRAP, POINTER, PUBLIC :: back => NULL()

  CONTAINS
    PRIVATE

    PROCEDURE, PASS( obj ) :: obj_Initiate1
    !! Empty constructor
    PROCEDURE, PASS( obj ) :: obj_Initiate2
    !! Construction by copying
    PROCEDURE, PASS( obj ) :: obj_Initiate3
    !! Construct by specifying the size and set all values
    !! to a given scalar data value
    PROCEDURE, PASS( obj ) :: obj_Initiate4
    !! Construction by an array
    !! Convert an array to Linked list
    PROCEDURE, PASS( obj ) :: obj_Initiate5
    !! Construction by using an iterator
    GENERIC, PUBLIC :: Initiate => obj_Initiate1, obj_Initiate2, &
      & obj_Initiate3, obj_Initiate4, obj_Initiate5
    !! Generic method for construction

    PROCEDURE, PASS( obj ) :: FixValuePtrs
    !! It is a private routine, which is called after push and pop

    PROCEDURE, PUBLIC, PASS( obj ) :: PushBack => obj_PushBack
    !! Append an entry to the list at the tail
    GENERIC, PUBLIC :: Add => PushBack
    GENERIC, PUBLIC :: Append => PushBack

    PROCEDURE, PUBLIC, PASS( obj ) :: PushFront => obj_PushFront
    !! Append an entry to the list at the head

    PROCEDURE, PUBLIC, PASS( obj ) :: PopFront => obj_PopFront
    !! Pop the head

    PROCEDURE, PUBLIC, PASS( obj ) :: PopBack => obj_PopBack
    !! Pop the back

    PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => obj_Deallocate
    !! Deallocate the list
    PROCEDURE, PUBLIC, PASS( obj ) :: Size => obj_Size
    !! Returns the size of list
    PROCEDURE, PUBLIC, PASS( obj ) :: isEmpty => obj_isEmpty
    !! Returns true if the list is empty

    FINAL :: Finalizer

    PROCEDURE, PUBLIC, PASS( obj ) :: Begin => obj_Begin
    PROCEDURE, PUBLIC, PASS( obj ) :: End => obj_End

    PROCEDURE, PASS( obj ) :: AssignOther
    !! list1  = list2
    PROCEDURE, PASS( obj ) :: AssignArray
    !! list = array
    GENERIC, PUBLIC :: ASSIGNMENT(=) => AssignOther, AssignArray

    PROCEDURE, PASS( obj ) :: InsertSingle
    PROCEDURE, PASS( obj ) :: InsertFill
    PROCEDURE, PASS( obj ) :: InsertArray
    PROCEDURE, PASS( obj ) :: InsertIteratorPair
    GENERIC, PUBLIC :: Insert => InsertSingle, InsertFill, InsertArray,  &
      & InsertIteratorPair

    PROCEDURE, PASS( obj ) :: EraseSingle
    PROCEDURE, PASS( obj ) :: EraseIteratorPair
    GENERIC, PUBLIC :: Erase => EraseSingle, EraseIteratorPair

    PROCEDURE, PASS( obj ), PUBLIC :: Resize
    PROCEDURE, PASS( obj ), PUBLIC :: Clear
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => obj_Display

END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, PUBLIC :: CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)
  PRIVATE
  CLASS( ListNode_ ), POINTER :: node => NULL()
  FTL_TEMPLATE_TYPE_WRAP, POINTER, PUBLIC :: value => NULL()

  CONTAINS
  PRIVATE
  PROCEDURE :: obj_iter_Initiate1
  PROCEDURE :: obj_iter_Initiate2
  GENERIC, PUBLIC :: Initiate => obj_iter_Initiate1, obj_iter_Initiate2
  PROCEDURE, PUBLIC :: Inc
  PROCEDURE, PUBLIC :: Dec
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => obj_Display_Iterator
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-22
! summary: 	Empty constructor

SUBROUTINE obj_Initiate1( obj )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT(INOUT ), TARGET :: obj
  CALL obj%Deallocate()
  obj%sentinel%next => obj%sentinel
  obj%sentinel%prev => obj%sentinel
END SUBROUTINE obj_Initiate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2024-01-22
! summary: 	Construct by copy

IMPURE ELEMENTAL SUBROUTINE obj_Initiate2(obj, other)
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,List,_)), INTENT(INOUT ) :: obj
  TYPE(CAT3(FTL_TEMPLATE_TYPE_NAME,List,_)), INTENT(IN) :: other

  ! Internal variable
  TYPE(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)) :: it, iend

  CALL obj%Initiate()
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

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-22
! summary: Construct by giving size and set all values to val

SUBROUTINE obj_Initiate3( obj, n, val )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT(INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP , TARGET, OPTIONAL, INTENT( IN ) :: val

  ! Internal variables
  INTEGER( I4B ) :: i

  CALL obj%Initiate()

  IF( PRESENT( val ) ) THEN
    DO i = 1, n
      CALL obj%Add(val)
    END DO
  ELSE
    STOP 'TODO: Implement ftlList%obj_Initiate3 without val'
  END IF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2024-01-22
! summary: 	Construct by array

SUBROUTINE obj_Initiate4(obj, array)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  FTL_TEMPLATE_TYPE_WRAP , TARGET, INTENT( IN ) :: array(:)

  ! Internal variable
  INTEGER( I4B ) :: i, n

  CALL obj%Initiate()
  n = SIZE( array )

  DO i = 1, n
    CALL obj%Add(array(i))
  END DO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2024-01-22
! summary: 	Construction by iterator

SUBROUTINE obj_Initiate5( obj, first, last )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  TYPE(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)), INTENT( IN ) :: first
  TYPE(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)), INTENT( IN ) :: last

  CALL obj%Initiate()
  CALL obj%Insert( obj%Begin(), first, last )
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2024-01-22
! summary: 	Insert node after (This is private)

SUBROUTINE InsertNodeAfter(afternode, val)
  CLASS( ListNode_ ), POINTER, INTENT( INOUT ) :: afternode
  FTL_TEMPLATE_TYPE_WRAP , TARGET, OPTIONAL, INTENT( IN ) :: val

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
#ifdef FTL_TEMPLATE_TYPE_IS_CLASS
    newnext%data => val
#else
    newnext%data = val
#endif
  END SELECT
  ENDIF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2024-01-22
! summary: 	Private

SUBROUTINE FixValuePtrs( obj )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj

  IF( obj%psize == 0 ) THEN
    NULLIFY( obj%front, obj%back )
    RETURN
  END IF

  SELECT TYPE( first => obj%sentinel%next )
    TYPE IS( DataNode_ )
    obj%front => first%data
  END SELECT

  SELECT TYPE( last => obj%sentinel%prev )
    TYPE IS( DataNode_ )
    obj%back => last%data
  END SELECT

END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2024-01-22
! summary: 	Push back (append)

SUBROUTINE obj_PushBack( obj, val )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  FTL_TEMPLATE_TYPE_WRAP , INTENT( IN ) :: val

  CALL InsertNodeAfter( obj%sentinel%prev, val )
  obj%psize = obj%psize + 1
  CALL obj%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2024-01-22
! summary: 	Prepend

SUBROUTINE obj_PushFront( obj, val )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ), TARGET :: obj
  FTL_TEMPLATE_TYPE_WRAP , TARGET, INTENT( IN ) :: val

  CALL InsertNodeBefore( obj%sentinel%next, val )
  obj%psize = obj%psize + 1
  CALL obj%FixValuePtrs()
END SUBROUTINE obj_PushFront


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2024-01-22
! summary: 	Private

SUBROUTINE UnlinkNode( node )
  CLASS( ListNode_ ), INTENT( INOUT ) :: node
  node%next%prev => node%prev
  node%prev%next => node%next
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2023-01-22
! summary: 	Pop from front

FUNCTION obj_PopFront( obj ) RESULT( ans )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT(INOUT ), TARGET :: obj

#ifdef FTL_TEMPLATE_TYPE_IS_CLASS
  FTL_TEMPLATE_TYPE_WRAP, POINTER :: ans
#else
  FTL_TEMPLATE_TYPE_WRAP :: ans
#endif

  ! Internal variables
  CLASS( ListNode_ ), POINTER :: oldfirst

  oldfirst => obj%sentinel%next

#ifdef FTL_TEMPLATE_TYPE_IS_CLASS
  ! ans => Factory(obj%front)
  ans => obj%front

#else

#ifdef FTL_TEMPLATE_TYPE_PROVIDES_FTLMOVE
  CALL ftlMove( obj%front, ans )
#else
  ans = obj%front
#endif

#endif

  obj%psize = obj%psize - 1
  CALL UnlinkNode( oldfirst )
  DEALLOCATE( oldfirst )
  CALL obj%FixValuePtrs()

END FUNCTION obj_PopFront

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION obj_PopBack(obj) RESULT( ans )
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,List,_)), INTENT(INOUT ), TARGET :: obj

#ifdef FTL_TEMPLATE_TYPE_IS_CLASS
  FTL_TEMPLATE_TYPE_WRAP, POINTER :: ans
#else
  FTL_TEMPLATE_TYPE_WRAP :: ans
#endif

  ! Define internal variable
  CLASS(ListNode_), POINTER :: oldlast
  oldlast => obj%sentinel%prev

#ifdef FTL_TEMPLATE_TYPE_IS_CLASS
  ! ans => Factory(obj%back)
  ans => obj%back
#else

#ifdef FTL_TEMPLATE_TYPE_PROVIDES_FTLMOVE
  CALL ftlMove( obj%back, ans )
#else
  ans = obj%back
#endif

#endif

  obj%psize = obj%psize - 1
  CALL UnlinkNode(oldlast)
  DEALLOCATE(oldlast)
  CALL obj%FixValuePtrs()
END FUNCTION obj_PopBack

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE obj_Deallocate( obj )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT(INOUT ), TARGET :: obj

  ! Internal variables
  CLASS( ListNode_ ), POINTER :: walker, deletor

  walker => obj%sentinel%next

  DO WHILE( ASSOCIATED( walker ) .AND. .NOT. ASSOCIATED(walker,obj%sentinel) )
    deletor => walker
    walker => walker%next

    deletor%prev => null()
    deletor%next => null()

#ifdef FTL_TEMPLATE_TYPE_IS_CLASS
    SELECT TYPE( deletor )
    TYPE IS( DataNode_ )
      deletor%data => null()
    END SELECT
#endif

    DEALLOCATE(deletor)
  END DO

  obj%psize = 0
  NULLIFY( obj%sentinel%prev )
  NULLIFY( obj%sentinel%next )
  NULLIFY( obj%front )
  NULLIFY( obj%back )
END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION obj_Size( obj ) RESULT( Size )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Size
  Size = obj%psize
END FUNCTION obj_Size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION obj_isEmpty(obj) RESULT( ans )
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,List,_)), INTENT(in) :: obj
  LOGICAL( LGT ) :: ans
  ans = (obj%psize .eq. 0)
END FUNCTION obj_isEmpty

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE Finalizer( obj )
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  CALL obj%Deallocate()
END SUBROUTINE Finalizer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION obj_Begin( obj ) RESULT( Begin )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( IN ), TARGET :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: Begin

  Begin%node => obj%sentinel%next
  SELECT TYPE( node => Begin%node )
    TYPE IS ( DataNode_ )
    Begin%value => node%data
  END SELECT
END FUNCTION obj_Begin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION obj_End( obj ) result( End )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( IN ), TARGET :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: End
  End%node => obj%sentinel
END FUNCTION obj_End

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

IMPURE ELEMENTAL SUBROUTINE AssignOther(obj, other)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( IN ) :: other

  ! Define internal variables
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: it
  INTEGER( I4B ) :: i, n

  CALL obj%Initiate()
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
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  FTL_TEMPLATE_TYPE_WRAP , TARGET, INTENT( IN ) :: array(:)
  CALL obj%Initiate(array)
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertSingle( obj, position, val )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: position
  FTL_TEMPLATE_TYPE_WRAP, TARGET, INTENT( IN ) :: val

  CALL obj%InsertFill( position, 1, val )
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertNodeBefore(beforenode, val)
  CLASS(ListNode_), POINTER, INTENT( INOUT ) :: beforenode
  FTL_TEMPLATE_TYPE_WRAP, TARGET, INTENT( IN ) , OPTIONAL :: val

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

#ifdef FTL_TEMPLATE_TYPE_IS_CLASS
      newprev%data => val
#else
      newprev%data = val
#endif

    END SELECT
  ENDIF
END SUBROUTINE InsertNodeBefore

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertFill( obj, position, n, val )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: position
  INTEGER( I4B ), INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP , TARGET, INTENT( IN ) :: val

  INTEGER( I4B ) :: i

  DO i = 1, n
    CALL InsertNodeBefore(position%node, val)
  END DO
  obj%psize = obj%psize + n
  CALL obj%FixValuePtrs()

END SUBROUTINE InsertFill

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertArray( obj, position, array )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT(INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: position
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN )    :: array(:)

  INTEGER( I4B ) :: i, n

  n = SIZE( array )
  DO i = 1, n
    CALL InsertNodeBefore( position%node, array(i) )
  END DO
  obj%psize = obj%psize + n
  CALL obj%FixValuePtrs()
END SUBROUTINE InsertArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE InsertIteratorPair( obj, position, first, last )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ) , INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: position
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ), INTENT( IN ) :: first
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ), INTENT( IN ) :: last

  ! Define internal variable
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: it

  it = first
  DO WHILE( it .NE. last)
    CALL InsertNodeBefore( position%node, it%value )
    obj%psize = obj%psize + 1
    CALL it%Inc()
  END DO
  CALL obj%FixValuePtrs()
END SUBROUTINE InsertIteratorPair

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE EraseSingle( obj, position )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT(INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: position

  CALL UnlinkNode(position%node)
  DEALLOCATE(position%node)
  obj%psize = obj%psize - 1
  CALL obj%FixValuePtrs()
END SUBROUTINE EraseSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE EraseIteratorPair( obj, first, last )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ) , INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: first
  TYPE(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)), INTENT( IN ) :: last

  ! Define internal variables
  TYPE(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)) :: deletor

  ASSOCIATE( walker => first )
    DO WHILE( walker .NE. last )
      deletor = walker
      CALL walker%Inc()
      CALL obj%EraseSingle(deletor)
    END DO
  END ASSOCIATE
END SUBROUTINE EraseIteratorPair

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SwapList( obj, other )
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ), TARGET :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ), TARGET ::  &
  & other

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
END SUBROUTINE SwapList

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Resize( obj, n, val )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) , INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP , TARGET, INTENT( IN ), OPTIONAL :: val

  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: it
  INTEGER( I4B ) :: i

  IF (n == obj%psize) THEN
    RETURN
  END IF

  IF (n < obj%psize) THEN
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

  obj%psize = n
  CALL obj%FixValuePtrs()
END SUBROUTINE Resize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Clear( obj )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  CALL obj%Initiate()
END SUBROUTINE Clear

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ftlMoveList( src, dest )
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT(INOUT ) :: src
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( OUT ), TARGET :: dest

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
END SUBROUTINE ftlMoveList

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_iter_Initiate1(self)
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)), INTENT(INOUT ) :: self
  NULLIFY( self%node )
  NULLIFY( self%value )
END SUBROUTINE obj_iter_Initiate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_iter_Initiate2( obj, other )
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)), INTENT( OUT ) :: obj
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)), INTENT( IN ) :: other

  obj%node => other%node
  SELECT TYPE( node => obj%node )
    TYPE IS( DataNode_ )
    obj%value => node%data
  END SELECT
END SUBROUTINE obj_iter_Initiate2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! todo
! check the bounds
! if bounds are crossed raise error
RECURSIVE SUBROUTINE Inc(obj, n)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ), INTENT(INOUT ) :: obj
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
END SUBROUTINE Inc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! todo
! check the bounds
! if bounds are crossed raise error
RECURSIVE SUBROUTINE Dec(obj, n)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ), INTENT(INOUT ) :: obj
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
END SUBROUTINE Dec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION EqualOther( obj, other ) RESULT( ans )
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)), INTENT(in) :: obj
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_)), INTENT(in) :: other
  LOGICAL( LGT ) :: ans
  ans = ASSOCIATED( obj%node,other%node )
END FUNCTION EqualOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION UnequalOther( obj, other ) RESULT( ans )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ), INTENT( IN ) :: obj
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: ans
  ans = .NOT. ASSOCIATED( obj%node,other%node )
END FUNCTION UnequalOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE  obj_Display_Iterator( obj, msg, unitno )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
  IF( ASSOCIATED( obj%value ) ) THEN
    CALL Display( obj%value, msg, unitno=unitno)
  END IF
END SUBROUTINE obj_Display_Iterator

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE  obj_Display( obj, msg, unitno )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,List,_) ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno

  ! Define internal variables
  INTEGER( I4B ) :: ii
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,List,Iterator_) ) :: it, last

  CALL Display(msg, unitno=unitno)
  CALL Display(obj%isEmpty(), "Is List Empty: ", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
  IF( obj%isEmpty() ) RETURN

  it = obj%Begin()
  last = obj%End()
  ii = 0
  DO WHILE( it .NE. last )
    ii = ii + 1
    CALL Display( it, msg // "("// tostring(ii) // "):", &
      & unitno=unitno )
    CALL BlankLines(nol=1, unitno=unitno)
    CALL it%inc()
  END DO
END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#endif
