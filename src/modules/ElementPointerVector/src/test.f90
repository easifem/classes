# 1 "ElementPointerVector_Class.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "ElementPointerVector_Class.f90"
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








MODULE ElementPointerVector_Class

# 1 "../../ftlMacros/ftlPointerVector.inc" 1
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

! #define FTL_CONTAINER PointerVector












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

# 31 "../../ftlMacros/ftlPointerVector.inc" 2










USE BaseType
USE BaseMethod

USE ElementFactory

IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                              ElementPointerVector_
!----------------------------------------------------------------------------

TYPE :: ElementPointerVector_
  PRIVATE
  INTEGER( I4B ) :: psize = 0
  TYPE(ElementPointer_), ALLOCATABLE :: storage( : )
  TYPE(ElementPointer_), POINTER, PUBLIC :: data( : ) => NULL()
  TYPE(ElementPointer_), POINTER, PUBLIC :: front => NULL()
  TYPE(ElementPointer_), POINTER, PUBLIC :: back => NULL()
  CONTAINS
  PRIVATE
  GENERIC, PUBLIC :: Initiate => T_NewDefault, T_NewCopyOther, T_NewFill, T_NewFromArray
  GENERIC, PUBLIC :: ASSIGNMENT( = ) => T_NewFromArray, T_NewCopyOther
  PROCEDURE :: T_NewDefault
  PROCEDURE :: T_NewCopyOther
  PROCEDURE :: T_NewFill
  PROCEDURE :: T_NewFromArray
  PROCEDURE, PUBLIC :: DeallocateData => T_DeallocateData
  PROCEDURE, PUBLIC :: Delete => T_DeallocateData
  FINAL :: T_Finalizer

  PROCEDURE, PUBLIC :: Begin => T_Begin
  PROCEDURE, PUBLIC :: End => T_End
  PROCEDURE, PUBLIC :: Size => T_Size
  PROCEDURE, PUBLIC :: Capacity => T_Capacity
  PROCEDURE, PUBLIC :: isEmpty => T_isEmpty
  PROCEDURE, PUBLIC :: Reserve => T_Reserve
  PROCEDURE, PUBLIC :: ShrinkToFit => T_ShrinkToFit
  PROCEDURE, PUBLIC :: Resize => T_Resize
  PROCEDURE, PUBLIC :: Pushback => T_Pushback
  PROCEDURE, PUBLIC :: Popback => T_Popback

  GENERIC, PUBLIC :: Insert => T_InsertSingle, T_InsertFill, T_InsertArray, &
    & T_InsertIteratorPair, T_InsertSingleAtIndex, T_InsertFillAtIndex, &
    & T_InsertArrayAtIndex, T_InsertIteratorPairAtIndex
  PROCEDURE :: T_InsertSingle
  PROCEDURE :: T_InsertSingleAtIndex
  PROCEDURE :: T_InsertFill
  PROCEDURE :: T_InsertFillAtIndex
  PROCEDURE :: T_InsertArray
  PROCEDURE :: T_InsertArrayAtIndex
  PROCEDURE :: T_InsertIteratorPair
  PROCEDURE :: T_InsertIteratorPairAtIndex

  GENERIC  , PUBLIC :: Erase => T_EraseSingle, T_EraseSingleIndex, T_EraseIndexPair, T_EraseIteratorPair
  PROCEDURE :: T_EraseSingle
  PROCEDURE :: T_EraseSingleIndex
  PROCEDURE :: T_EraseIndexPair
  PROCEDURE :: T_EraseIteratorPair

  PROCEDURE, PUBLIC :: Clear => T_Clear
  PROCEDURE :: FixValuePtrs => T_FixValuePtrs
  PROCEDURE :: ShiftByN => T_ShiftByN
  PROCEDURE :: ChangeCapacity => T_ChangeCapacity
  PROCEDURE :: IncreaseCapacity => T_IncreaseCapacity

  ! PROCEDURE, PUBLIC :: Display => T_Display
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: ElementPointerVector_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE SIZE
  MODULE PROCEDURE T_Size
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


INTERFACE SWAP
  MODULE PROCEDURE T_SWAP
END INTERFACE SWAP

PUBLIC :: SWAP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE MOVE
  MODULE PROCEDURE T_MoveDynArray, T_MoveArrayToDynArray
END INTERFACE MOVE

PUBLIC :: MOVE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: ElementPointerVectorIterator_
  PRIVATE
  TYPE(ElementPointerVector_), POINTER :: dynArray => NUL()
  INTEGER( I4B ) :: index = 0
  TYPE(ElementPointer_), POINTER, PUBLIC :: value => NULL()
  CONTAINS
  PRIVATE
    PROCEDURE :: T_Iter_NewItDefault
    PROCEDURE :: T_Iter_NewItCopyOther
    GENERIC, PUBLIC :: Initiate => T_Iter_NewItDefault, T_Iter_NewItCopyOther

    PROCEDURE, PUBLIC :: Inc => T_Iter_Inc
    PROCEDURE, PUBLIC :: Dec => T_Iter_Dec

    ! PROCEDURE, PASS(lhs) :: T_Iter_AdvanceN
    ! GENERIC, PUBLIC :: OPERATOR(+) => T_Iter_AdvanceN

    ! PROCEDURE, PASS(lhs) :: T_Iter_ReverseN
    ! PROCEDURE, PASS(lhs) :: T_Iter_DiffOther
    ! GENERIC, PUBLIC :: OPERATOR(-) => T_Iter_ReverseN, T_Iter_DiffOther

    ! PROCEDURE, PASS(lhs) :: T_Iter_EqualOther
    ! GENERIC, PUBLIC :: OPERATOR(==) => T_Iter_EqualOther

    ! PROCEDURE, PASS(lhs) :: T_Iter_UnequalOther
    ! GENERIC, PUBLIC :: OPERATOR(/=) => T_Iter_UnequalOther

    ! PROCEDURE, PASS(lhs) :: SmallerOther
    ! GENERIC, PUBLIC :: OPERATOR(<) => SmallerOther

    ! PROCEDURE, PASS(lhs) :: SmallerEqualOther
    ! GENERIC, PUBLIC :: OPERATOR(<=) => SmallerEqualOther

    ! PROCEDURE, PASS(lhs) :: GreaterOther
    ! GENERIC, PUBLIC :: OPERATOR(>) => GreaterOther

    ! PROCEDURE, PASS(lhs) :: GreaterEqualOther
    ! GENERIC, PUBLIC :: OPERATOR(>=) => GreaterEqualOther

END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: ElementPointerVectorIterator_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_NewDefault(obj)
  CLASS(ElementPointerVector_), INTENT(INOUT) :: obj
  CALL obj%DeallocateData()
  ALLOCATE(obj%storage(0))
  CALL obj%FixValuePtrs()
END SUBROUTINE T_NewDefault

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_NewCopyOther(obj, anotherObj)
  CLASS(ElementPointerVector_), INTENT(INOUT) :: obj
  TYPE(ElementPointerVector_), INTENT(INOUT) :: anotherObj
  ! Internal variables
  INTEGER( I4B ) :: ii, n
  CALL obj%DeallocateData()
  n = anotherObj%psize
  ALLOCATE( obj%storage(n) )
  DO ii = 1, n
    obj%storage(ii)%ptr => anotherObj%storage(ii)%ptr
  END DO
  obj%psize = anotherObj%psize
  CALL obj%FixValuePtrs()
END SUBROUTINE T_NewCopyOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_NewFill(obj, n, val)
  CLASS(ElementPointerVector_), INTENT(INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  TYPE(ElementPointer_), OPTIONAL, INTENT( IN ) :: val
  ! internal variable
  INTEGER( I4B ) :: ii

  CALL obj%DeallocateData()
  ALLOCATE( obj%storage(n) )
  IF( PRESENT( val ) ) THEN
    DO ii = 1, n
      obj%storage(ii)%ptr => val%ptr
    END DO
  END IF
  obj%psize = n
  CALL obj%FixValuePtrs()
END SUBROUTINE T_NewFill

!----------------------------------------------------------------------------
!                                                               NewFromArray
!----------------------------------------------------------------------------

SUBROUTINE T_NewFromArray(obj, array)
  CLASS( ElementPointerVector_ ), INTENT( INOUT) :: obj
  TYPE(ElementPointer_), INTENT( IN ) :: array(:)
  ! Internal variables
  INTEGER( I4B ) :: ii,n

  CALL obj%DeallocateData()
  n = SIZE( array )
  ALLOCATE( obj%storage(n) )
  DO ii = 1, n
    obj%storage(ii)%ptr => array(ii)%ptr
  END DO
  obj%psize = n
  CALL obj%FixValuePtrs()
END SUBROUTINE T_NewFromArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_DeallocateData( obj )
  CLASS( ElementPointerVector_ ), INTENT( INOUT) :: obj
  obj%psize = 0
  IF(ALLOCATED(obj%storage)) DEALLOCATE(obj%storage)
  NULLIFY(obj%data)
  NULLIFY(obj%front)
  NULLIFY(obj%back)
END SUBROUTINE T_DeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Finalizer( obj )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  CALL obj%DeallocateData()
END SUBROUTINE T_Finalizer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION T_Begin(obj) RESULT(ans)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ) :: ans
  ans%dynArray => obj
  ans%index=1
  IF(obj%psize .NE. 0 ) ans%value%ptr => obj%storage(1)%ptr
END FUNCTION T_Begin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION T_End(obj) RESULT(ans)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ) :: ans
  ans%dynArray => obj
  ans%index = obj%psize + 1
END FUNCTION T_End

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Size( obj ) RESULT( ans )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  ans = obj%psize
END FUNCTION T_Size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Capacity(obj) RESULT( ans )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  IF( ALLOCATED( obj%storage ) ) THEN
    ans = SIZE(obj%storage)
  ELSE
    ans = 0
  END IF
END FUNCTION T_Capacity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_isEmpty(obj) RESULT(ans)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ) :: ans
  ans = (obj%psize .EQ. 0)
END FUNCTION T_isEmpty

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Reserve(obj, n)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  IF( n .GT. obj%Capacity()) THEN
    CALL obj%ChangeCapacity(n)
  END IF
  CALL obj%FixValuePtrs()
END SUBROUTINE T_Reserve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_ShrinkToFit(obj)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  CALL obj%ChangeCapacity(obj%psize)
  CALL obj%FixValuePtrs()
END SUBROUTINE T_ShrinkToFit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Resize(obj, n, val)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  TYPE(ElementPointer_), OPTIONAL, INTENT( IN ) :: val
  ! Internaal variables
  INTEGER( I4B ) :: ii
  IF( n .EQ. obj%psize ) RETURN

  IF( n .GT. obj%Capacity() ) CALL obj%ChangeCapacity( n )
  DO i = n+1, obj%psize
    CALL TriggerFinalizer(obj%storage(i))
  END DO
  IF( PRESENT( val ) .AND. ( n .GT. obj%psize ) ) THEN
    DO ii = obj%psize+1, n
      obj%storage(ii)%ptr => val
    END DO
  END IF
  obj%psize = n
  CALL obj%FixValuePtrs()
END SUBROUTINE T_Resize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Pushback( obj, val )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE(ElementPointer_), INTENT( IN ) :: val
  IF( (obj%psize + 1) .GT. obj%Capacity() ) CALL obj%IncreaseCapacity()
  obj%psize = obj%psize + 1
  obj%storage(obj%psize)%ptr => val%ptr
  CALL obj%FixValuePtrs()
END SUBROUTINE T_Pushback

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION T_Popback(obj) RESULT(ans)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE(ElementPointer_), INTENT( IN ) :: ans

  ans%ptr => obj%storage(obj%psize)%ptr
  obj%psize = obj%psize - 1
  call TriggerFinalizer(obj%storage(obj%psize))
  call obj%FixValuePtrs()
END FUNCTION T_Popback

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertSingleAtIndex(obj, pos, val)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  TYPE(ElementPointer_), INTENT( IN ) :: val
  CALL obj%ShiftByN(pos, 1)
  obj%storage(pos)%ptr => val%ptr
  obj%psize = obj%psize + 1
  CALL obj%FixValuePtrs()
END SUBROUTINE T_InsertSingleAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertSingle(obj, pos, val)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: pos
  TYPE(ElementPointer_), INTENT( IN ) :: val
  CALL obj%Insert(pos%index, val)
END SUBROUTINE T_InsertSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertFillAtIndex(obj, pos, n, val)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  INTEGER( I4B ), INTENT( IN ) :: n
  TYPE(ElementPointer_), INTENT( IN ) :: val
  ! Internal variables
  INTEGER( I4B ) :: ii
  IF( n .LE. 0 ) Return
  CALL obj%ShiftByN(pos,n)
  DO ii = 1, n
    obj%storage(pos-1+ii)%ptr => val%ptr
  END DO
  obj%psize = obj%psize+n
  CALL obj%FixValuePtrs()
END SUBROUTINE T_InsertFillAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


SUBROUTINE T_InsertFill(obj, pos, n, val)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: pos
  INTEGER( I4B ), INTENT( IN ) :: n
  TYPE(ElementPointer_), INTENT( IN ) :: val
  CALL obj%Insert(pos%index, n, val)
END SUBROUTINE T_InsertFill

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertArrayAtIndex( obj, pos, array )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  TYPE(ElementPointer_), INTENT( IN ) ::  array( : )
  ! Internal varaiables
  INTEGER( I4B ) :: ii, n
  n = SIZE( array )
  IF( n .EQ. 0 ) RETURN
  CALL obj%ShiftByN( pos, n )
  DO ii = 1, n
    obj%storage(pos-1+ii)%ptr => array(i)%ptr
  END DO
  obj%psize = obj%psize + size(array)
  CALL obj%FixValuePtrs()
END SUBROUTINE T_InsertArrayAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertArray( obj, pos, array )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: pos
  TYPE(ElementPointer_), INTENT( IN ) :: array(:)
  CALL obj%Insert( pos%index, array )
END SUBROUTINE T_InsertArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertIteratorPairAtIndex( obj, pos, first, last )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: pos
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: first
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: last
  CALL obj%Insert( pos%index, first%dynArray%data(first%index:last%index-1) )
END SUBROUTINE T_InsertIteratorPairAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_EraseIndexPair( obj, first, last )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: first
  INTEGER( I4B ), INTENT( IN ) :: last
  ! Internal variable
  INTEGER( I4B ) :: i, numerased

  IF( last .LE. first ) RETURN
  numerased = last - first

  DO i = first, obj%psize - numerased
    obj%storage( i )%ptr => obj%storage( i+numerased )%ptr
    CALL TriggerFinalizer( obj%storage(i+numerased) )
  END DO
  obj%psize = obj%psize - numerased
  CALL obj%FixValuePtrs()
END SUBROUTINE T_EraseIndexPair

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_EraseSingle( obj, pos )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: pos
  CALL obj%EraseIndexPair( pos%index, pos%index+1 )
END SUBROUTINE T_EraseSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_EraseSingleIndex( obj, pos )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  CALL obj%EraseIndexPair( pos, pos+1 )
END SUBROUTINE T_EraseSingleIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE EraseIteratorPair( obj, first, last )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: first
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: last
  CALL obj%EraseIndexPair(first%index, last%index)
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Clear( obj )
  CLASS(ftlDynArrayElement), intent(inout) :: obj
  CALL obj%EraseIndexPair( 1, obj%psize+1 )
END SUBROUTINE T_Clear

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_FixValuePtrs(obj)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj

  IF( allocated(obj%storage) ) THEN
    obj%data => obj%storage(1:obj%psize)
  ELSE
    NULLIFY(obj%data)
  ENDIF
  IF( obj%psize == 0 ) THEN
    NULLIFY(obj%front, obj%back)
  ELSE
    obj%front => obj%storage(1)
    obj%back => obj%storage(obj%psize)
  ENDIF
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_ShiftByN(obj, from, n)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: from
  INTEGER( I4B ), INTENT( IN ) :: n
  ! internal variables
  INTEGER( I4B ) :: i

  IF(( obj%psize + n ) > ( 2 * obj%Capacity() )) THEN
    CALL obj%ChangeCapacity( obj%psize + n )
  ELSE IF( (obj%psize + n) > obj%Capacity() ) THEN
    CALL obj%IncreaseCapacity()
  END IF

  DO i = obj%psize, from, -1
    obj%storage(i+n)%ptr => obj%storage(i)%ptr
  END DO
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_ChangeCapacity( obj, n )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) ::  n
  ! internal variables
  TYPE(ElementPointer_), ALLOCATABLE :: newstorage(:)
  INTEGER( I4B ) ::  i, numretain
  !
  IF( n .EQ. obj%Capacity() ) RETURN
  ALLOCATE(newstorage(n))
  IF( allocated(obj%storage) ) THEN
    numretain = min(obj%psize, n)
    DO i = 1, numretain
      newstorage(i) = obj%storage(i)
      CALL TriggerFinalizer(obj%storage(i))
    END DO
  END IF
  CALL MOVE_ALLOC(newstorage, obj%storage)
END SUBROUTINE T_ChangeCapacity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_IncreaseCapacity(obj)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj
  CALL obj%changeCapacity(max(2 * obj%Capacity(), 1))
END SUBROUTINE T_IncreaseCapacity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE TriggerFinalizer(instance)
  TYPE(ElementPointer_), INTENT( INOUT) :: instance
  return
  instance = instance
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SWAP( obj1, obj2 )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: obj2, obj1
  ! Internal variable
  TYPE(ElementPointer_), ALLOCATABLE :: tmpstorage(:)
  INTEGER( I4B ) :: tmppsize

  CALL MOVE_ALLOC( obj1%storage, tmpstorage )
  CALL MOVE_ALLOC( obj2%storage, obj1%storage )
  CALL MOVE_ALLOC( tmpstorage, obj2%storage )
  tmppsize = obj1%psize
  obj1%psize = obj2%psize
  obj2%psize = tmppsize
  CALL obj1%FixValuePtrs()
  CALL obj2%FixValuePtrs()
END SUBROUTINE T_SWAP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_MoveDynArray(src, dest)
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: src
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: dest
  CALL MOVE_ALLOC(src%storage, dest%storage)
  dest%psize = src%psize
  CALL dest%FixValuePtrs()
  CALL src%Delete()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ftlMoveArrayToDynArray(src, dest)
  TYPE(ElementPointer_), ALLOCATABLE, INTENT( INOUT ) :: src( : )
  CLASS( ElementPointerVector_ ), INTENT( INOUT ) :: dest
  CALL MOVE_ALLOC(src, dest%storage)
  dest%psize = SIZE(dest%storage)
  CALL dest%FixValuePtrs()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Iter_NewItDefault(obj)
  CLASS( ElementPointerVectorIterator_ ), INTENT( INOUT ) :: obj
  NULLIFY(obj%dynArray)
  obj%index = 0
  NULLIFY(obj%value)
END SUBROUTINE T_Iter_NewItDefault

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Iter_NewItCopyOther(obj, anotherobj)
  CLASS( ElementPointerVectorIterator_ ), INTENT( INOUT ) :: obj
  TYPE( ElementPointerVectorIterator_ ), INTENT( IN ) :: anotherobj

  obj%dynArray => anotherobj%dynArray
  obj%index = anotherobj%index
  IF( (obj%dynArray%psize .GT. 0) .AND. (obj%index .LE. obj%dynArray%psize) ) obj%value%ptr => obj%dynArray%storage(obj%index)%ptr
END SUBROUTINE T_Iter_NewItCopyOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Iter_Inc( obj )
  CLASS( ElementPointerVectorIterator_ ), INTENT( INOUT ) :: obj
  obj%index = obj%index + 1
  IF( obj%index .LE. obj%dynArray%psize )THEN
    obj%value%ptr => obj%dynArray%storage(obj%index)%ptr
  ELSE
    NULLIFY(obj%value)
  ENDIF
END SUBROUTINE T_Iter_Inc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Iter_dec( obj )
  CLASS( ElementPointerVectorIterator_ ), INTENT( INOUT ) :: obj
  obj%index = obj%index - 1
  IF(obj%index .GT. 0)THEN
    obj%value%ptr => obj%dynArray%storage(obj%index)%ptr
  ELSE
    NULLIFY(obj%value)
  END IF
END SUBROUTINE T_Iter_dec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE



! #define Element Element
! #define
! #define ElementPointer_ ElementPointer_
! #define USE ElementFactory ElementFactory
# 27 "ElementPointerVector_Class.f90" 2

