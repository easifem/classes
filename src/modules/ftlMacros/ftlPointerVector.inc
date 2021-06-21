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

#ifdef FTL_INSTANTIATE_TEMPLATE

#if !defined(FTL_TEMPLATE_TYPE)
#error FTL_TEMPLATE_TYPE must be defined when instantiating <T>PointerVector
#endif

#if !defined(FTL_TEMPLATE_TYPE_NAME)
#error FTL_TEMPLATE_TYPE_NAME must be defined when instantiating <T>PointerVector
#endif

#include "./ftlMacros.inc"

#ifdef FTL_TEMPLATE_TYPE_IS_DERIVED
#define FTL_TEMPLATE_TYPE_WRAP TYPE(FTL_TEMPLATE_TYPE)
#else
#define FTL_TEMPLATE_TYPE_WRAP FTL_TEMPLATE_TYPE
#endif

#ifdef MODULE_NAME
MODULE MODULE_NAME
#endif
USE BaseType
USE BaseMethod
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
#ifdef FTL_TEMPLATE_TYPE_MODULE
USE FTL_TEMPLATE_TYPE_MODULE
#endif
IMPLICIT NONE
PRIVATE
CHARACTER( LEN=* ), PARAMETER :: modName="<T>POINTERVECTOR"
TYPE( ExceptionHandler_ ), SAVE, PUBLIC :: ePointerVector
!$OMP THREADPRIVATE(ePointerVector)

!----------------------------------------------------------------------------
!                              CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_)
!----------------------------------------------------------------------------

TYPE :: CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_)
  PRIVATE
  INTEGER( I4B ) :: psize = 0
  FTL_TEMPLATE_TYPE_WRAP, ALLOCATABLE :: storage( : )
  FTL_TEMPLATE_TYPE_WRAP, POINTER, PUBLIC :: data( : ) => NULL()
  FTL_TEMPLATE_TYPE_WRAP, POINTER, PUBLIC :: front => NULL()
  FTL_TEMPLATE_TYPE_WRAP, POINTER, PUBLIC :: back => NULL()
  CONTAINS
  PRIVATE
  GENERIC, PUBLIC :: Initiate => &
    & T_NewDefault, T_NewCopyOther, T_NewFill, T_NewFromArray
  GENERIC, PUBLIC :: ASSIGNMENT( = ) => T_NewFromArray, T_NewCopyOther

  PROCEDURE, PASS( obj ) :: T_NewDefault
  PROCEDURE, PASS( obj ) :: T_NewCopyOther
  PROCEDURE, PASS( obj ) :: T_NewFill
  PROCEDURE, PASS( obj ) :: T_NewFromArray
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => T_DeallocateData
  PROCEDURE, PASS( obj ) :: Delete => T_DeallocateData
  FINAL :: T_Finalizer

  PROCEDURE, PUBLIC, PASS( obj ) :: Begin => T_Begin
  PROCEDURE, PUBLIC, PASS( obj ) :: End => T_End
  PROCEDURE, PUBLIC, PASS( obj ) :: Size => T_Size
  PROCEDURE, PUBLIC, PASS( obj ) :: Capacity => T_Capacity
  PROCEDURE, PUBLIC, PASS( obj ) :: isEmpty => T_isEmpty
  PROCEDURE, PUBLIC, PASS( obj ) :: Reserve => T_Reserve
  PROCEDURE, PUBLIC, PASS( obj ) :: ShrinkToFit => T_ShrinkToFit
  PROCEDURE, PUBLIC, PASS( obj ) :: Resize => T_Resize
  PROCEDURE, PUBLIC, PASS( obj ) :: Pushback => T_Pushback
  PROCEDURE, PUBLIC, PASS( obj ) :: Popback => T_Popback

  GENERIC, PUBLIC :: Insert => &
    & T_InsertSingle, T_InsertFill, T_InsertArray, &
    & T_InsertIteratorPair, T_InsertSingleAtIndex, T_InsertFillAtIndex, &
    & T_InsertArrayAtIndex, T_InsertIteratorPairAtIndex
  PROCEDURE, PASS( obj ) :: T_InsertSingle
  PROCEDURE, PASS( obj ) :: T_InsertSingleAtIndex
  PROCEDURE, PASS( obj ) :: T_InsertFill
  PROCEDURE, PASS( obj ) :: T_InsertFillAtIndex
  PROCEDURE, PASS( obj ) :: T_InsertArray
  PROCEDURE, PASS( obj ) :: T_InsertArrayAtIndex
  PROCEDURE, PASS( obj ) :: T_InsertIteratorPair
  PROCEDURE, PASS( obj ) :: T_InsertIteratorPairAtIndex

  GENERIC, PUBLIC :: Set => &
    & T_SetSingle, T_SetFill, T_SetArray, &
    & T_SetIteratorPair, T_SetSingleAtIndex, T_SetFillAtIndex, &
    & T_SetArrayAtIndex, T_SetIteratorPairAtIndex

  PROCEDURE, PASS( obj ) :: T_SetSingle
  PROCEDURE, PASS( obj ) :: T_SetSingleAtIndex
  PROCEDURE, PASS( obj ) :: T_SetFill
  PROCEDURE, PASS( obj ) :: T_SetFillAtIndex
  PROCEDURE, PASS( obj ) :: T_SetArray
  PROCEDURE, PASS( obj ) :: T_SetArrayAtIndex
  PROCEDURE, PASS( obj ) :: T_SetIteratorPair
  PROCEDURE, PASS( obj ) :: T_SetIteratorPairAtIndex

  GENERIC  , PUBLIC :: Erase => &
    & T_EraseSingle, T_EraseSingleIndex, &
    & T_EraseIndexPair, T_EraseIteratorPair
  PROCEDURE, PASS( obj ) :: T_EraseSingle
  PROCEDURE, PASS( obj ) :: T_EraseSingleIndex
  PROCEDURE, PASS( obj ) :: T_EraseIndexPair
  PROCEDURE, PASS( obj ) :: T_EraseIteratorPair

  PROCEDURE, PUBLIC, PASS( obj ) :: Clear => T_Clear
  PROCEDURE, PASS( obj ) :: FixValuePtrs => T_FixValuePtrs
  PROCEDURE, PASS( obj ) :: ShiftByN => T_ShiftByN
  PROCEDURE, PASS( obj ) :: ChangeCapacity => T_ChangeCapacity
  PROCEDURE, PASS( obj ) :: IncreaseCapacity => T_IncreaseCapacity
  PROCEDURE, PUBLIC :: Display => T_Display
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_)

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

INTERFACE Display
  MODULE PROCEDURE T_Display, iter_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_)
  PRIVATE
  TYPE(CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_)), POINTER :: dynArray => NULL()
  INTEGER( I4B ) :: index = 0
  FTL_TEMPLATE_TYPE_WRAP, PUBLIC :: value
  CONTAINS
  PRIVATE
    PROCEDURE, PASS( obj ) :: T_Iter_NewItDefault
    PROCEDURE, PASS( obj ) :: T_Iter_NewItCopyOther
    GENERIC, PUBLIC :: Initiate => T_Iter_NewItDefault, T_Iter_NewItCopyOther

    PROCEDURE, PUBLIC, PASS( obj ) :: Inc => T_Iter_Inc
    PROCEDURE, PUBLIC, PASS( obj ) :: Dec => T_Iter_Dec

    PROCEDURE, PASS( obj ) :: T_Iter_AdvanceN
    GENERIC, PUBLIC :: OPERATOR(+) => T_Iter_AdvanceN

    PROCEDURE, PASS( obj ) :: T_Iter_ReverseN
    PROCEDURE, PASS( obj ) :: T_Iter_DiffOther
    GENERIC, PUBLIC :: OPERATOR(-) => T_Iter_ReverseN, T_Iter_DiffOther

    PROCEDURE, PASS( obj ) :: T_Iter_EqualOther
    GENERIC, PUBLIC :: OPERATOR(.EQ.) => T_Iter_EqualOther

    PROCEDURE, PASS( obj ) :: T_Iter_UnequalOther
    GENERIC, PUBLIC :: OPERATOR(.NE.) => T_Iter_UnequalOther

    PROCEDURE, PASS( obj ) :: T_Iter_SmallerOther
    GENERIC, PUBLIC :: OPERATOR(.LT.) => T_Iter_SmallerOther

    PROCEDURE, PASS( obj ) :: T_Iter_SmallerEqualOther
    GENERIC, PUBLIC :: OPERATOR(.LE.) => T_Iter_SmallerEqualOther

    PROCEDURE, PASS( obj ) :: T_Iter_GreaterOther
    GENERIC, PUBLIC :: OPERATOR(.GT.) => T_Iter_GreaterOther

    PROCEDURE, PASS( obj ) :: T_Iter_GreaterEqualOther
    GENERIC, PUBLIC :: OPERATOR( .GE. ) => T_Iter_GreaterEqualOther

    PROCEDURE, PUBLIC, PASS( obj ) :: Display => iter_display
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_NewDefault(obj)
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_)), INTENT(INOUT) :: obj
  CALL obj%DeallocateData()
  ALLOCATE(obj%storage(0))
  CALL obj%FixValuePtrs()
END SUBROUTINE T_NewDefault

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_NewCopyOther(obj, anotherObj)
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_)), INTENT(INOUT) :: obj
  TYPE(CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_)), INTENT(IN) :: anotherObj
  ! Internal variables
  INTEGER( I4B ) :: ii, n
  CALL obj%DeallocateData()
  n = anotherObj%psize
  ALLOCATE( obj%storage(n) )
  DO ii = 1, n
    obj%storage(ii)%ptr => anotherObj%storage(ii)%ptr
  END DO
  obj%psize = n
  CALL obj%FixValuePtrs()
END SUBROUTINE T_NewCopyOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_NewFill(obj, n, val)
  CLASS(CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_)), INTENT(INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP, OPTIONAL, INTENT( IN ) :: val
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
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT) :: obj
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: array(:)
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
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT) :: obj
  ! CALL obj%clear(freeMem=.TRUE.)
  obj%psize = 0
  IF(ALLOCATED(obj%storage)) THEN
    CALL obj%clear(freeMem=.TRUE.)
  END IF
  NULLIFY(obj%data)
  NULLIFY(obj%front)
  NULLIFY(obj%back)
END SUBROUTINE T_DeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Finalizer( obj )
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  CALL obj%DeallocateData()
END SUBROUTINE T_Finalizer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION T_Begin(obj) RESULT(ans)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), TARGET, INTENT( IN ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ) :: ans
  ! Internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName="Begin()"

  IF( obj%isEmpty() )THEN
    CALL ePointerVector%RaiseError(modName//"::"//myName//" - "// &
      & "Vector is empty")
  ELSE
    ans%dynArray => obj
    ans%index=1
    ans%value%ptr => obj%storage(1)%ptr
  END IF
END FUNCTION T_Begin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION T_End(obj) RESULT(ans)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), TARGET, INTENT( IN ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ) :: ans
  ! Internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName="End()"
  IF( obj%isEmpty() )THEN
    CALL ePointerVector%RaiseError( modName//"::"//myName//" - "// &
      & "Vector is empty" )
  ELSE
    ans%dynArray => obj
    ans%index = obj%psize + 1
    ans%value%ptr => null()
  END IF
END FUNCTION T_End

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Size( obj ) RESULT( ans )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
  ans = obj%psize
END FUNCTION T_Size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Capacity(obj) RESULT( ans )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( IN ) :: obj
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
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
  ans = (obj%psize .EQ. 0)
END FUNCTION T_isEmpty

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Reserve(obj, n)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
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
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  CALL obj%ChangeCapacity(obj%psize)
  CALL obj%FixValuePtrs()
END SUBROUTINE T_ShrinkToFit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Resize(obj, n, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP, OPTIONAL, INTENT( IN ) :: val
  ! Internaal variables
  INTEGER( I4B ) :: ii
  IF( n .EQ. obj%psize ) RETURN
  IF( n .GT. obj%Capacity() ) CALL obj%ChangeCapacity( n )
  DO ii = n+1, obj%psize
    CALL TriggerFinalizer(obj%storage(ii))
  END DO
  IF( PRESENT( val ) .AND. ( n .GT. obj%psize ) ) THEN
    DO ii = obj%psize+1, n
      obj%storage(ii)%ptr => val%ptr
    END DO
  END IF
  obj%psize = n
  CALL obj%FixValuePtrs()
END SUBROUTINE T_Resize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Pushback( obj, val )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val
  ! Internal variable
  CHARACTER( LEN=* ), PARAMETER :: myName = "Pushback()"

  IF( ASSOCIATED(val%ptr) ) THEN
    IF( (obj%psize + 1) .GT. obj%Capacity() ) CALL obj%IncreaseCapacity()
    obj%psize = obj%psize + 1
    obj%storage(obj%psize)%ptr => val%ptr
    CALL obj%FixValuePtrs()
  ELSE
    CALL ePointerVector%RaiseError(modName//"::"//myName//" - "// &
      & "val%ptr should be associated.")
  END IF
END SUBROUTINE T_Pushback

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Popback( obj, val )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  FTL_TEMPLATE_TYPE_WRAP, INTENT( INOUT ) :: val
  ! Internal variables
  CHARACTER( LEN=* ), PARAMETER :: myName = "Popback()"

  IF( ASSOCIATED( val%ptr ) ) THEN
    CALL ePointerVector%RaiseError(modName//"::"//myName//" - "// &
        "val%ptr should be associated.")
  ELSE
    val%ptr => obj%storage(obj%psize)%ptr
    call TriggerFinalizer(obj%storage(obj%psize))
    obj%psize = obj%psize - 1
    call obj%FixValuePtrs()
  END IF
END SUBROUTINE T_Popback

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertSingleAtIndex(obj, pos, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val
  CALL obj%ShiftByN(pos, 1)
  obj%storage(pos)%ptr => val%ptr
  obj%psize = obj%psize + 1
  CALL obj%FixValuePtrs()
END SUBROUTINE T_InsertSingleAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertSingle(obj, pos, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val
  CALL obj%Insert(pos%index, val)
END SUBROUTINE T_InsertSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertFillAtIndex(obj, pos, n, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  INTEGER( I4B ), INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val

  ! Internal variables
  INTEGER( I4B ) :: ii
  IF( n .LE. 0 ) Return

  CALL obj%ShiftByN( pos, n )
  DO ii = 1, n
    obj%storage(pos-1+ii)%ptr => val%ptr
  END DO
  obj%psize = obj%psize + n
  CALL obj%FixValuePtrs()
END SUBROUTINE T_InsertFillAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertFill(obj, pos, n, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  INTEGER( I4B ), INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val
  CALL obj%Insert(pos%index, n, val)
END SUBROUTINE T_InsertFill

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertArrayAtIndex( obj, pos, array )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) ::  array( : )
  ! Internal varaiables
  INTEGER( I4B ) :: ii, n
  n = SIZE( array )
  IF( n .EQ. 0 ) RETURN
  CALL obj%ShiftByN( pos, n )
  DO ii = 1, n
    obj%storage(pos-1+ii)%ptr => array(ii)%ptr
  END DO
  obj%psize = obj%psize + size(array)
  CALL obj%FixValuePtrs()
END SUBROUTINE T_InsertArrayAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertIteratorPairAtIndex( obj, pos, first, last )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: first
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: last
  CALL obj%Insert( pos, first%dynArray%data(first%index:last%index-1) )
END SUBROUTINE T_InsertIteratorPairAtIndex


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SetSingleAtIndex(obj, pos, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val
  ! Internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName = "T_SetSingleAtIndex(obj, pos, val)"

  IF( pos .GT. obj%psize ) THEN
    CALL ePointerVector%RaiseError(modName//"::"//myName//" - "// &
      & "While setting the value pos is greater than the length of list")
  ELSE
    IF( ASSOCIATED( obj%storage(pos)%ptr ) ) THEN
      DEALLOCATE( obj%storage(pos)%ptr )
    END IF
    obj%storage(pos)%ptr => val%ptr
  END IF
  CALL obj%FixValuePtrs()
END SUBROUTINE T_SetSingleAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SetSingle(obj, pos, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val
  CALL obj%Set(pos%index, val)
END SUBROUTINE T_SetSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SetFillAtIndex(obj, pos, n, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  INTEGER( I4B ), INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val

  ! Internal variables
  INTEGER( I4B ) :: ii, pos0
  CHARACTER( LEN = * ), PARAMETER :: myName = "T_InsertFillAtIndex"

  IF( n .LE. 0 ) Return

  DO ii = 1, n
    pos0 = pos-1+ii
    IF( pos0 .GT. obj%psize ) THEN
      CALL ePointerVector%RaiseError(modName//"::"//myName//" - "// &
        & "While setting the value pos0 is greater than the length of list")
    ELSE
      IF( ASSOCIATED( obj%storage(pos0)%ptr ) ) THEN
        DEALLOCATE( obj%storage(pos0)%ptr )
      END IF
      obj%storage(pos0)%ptr => val%ptr
    END IF
  END DO
  CALL obj%FixValuePtrs()
END SUBROUTINE T_SetFillAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SetFill(obj, pos, n, val)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  INTEGER( I4B ), INTENT( IN ) :: n
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: val
  CALL obj%Set(pos%index, n, val)
END SUBROUTINE T_SetFill

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SetArrayAtIndex( obj, pos, array )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) ::  array( : )

  ! Internal varaiables
  INTEGER( I4B ) :: ii, pos0, n
  CHARACTER( LEN = * ), PARAMETER :: myName = "T_InsertFillAtIndex"

  n = SIZE( array )
  IF( n .EQ. 0 ) RETURN
  DO ii = 1, n
    pos0 = pos-1+ii
    IF( pos0 .GT. obj%psize ) THEN
      CALL ePointerVector%RaiseError(modName//"::"//myName//" - "// &
        & "While setting the value pos0 is greater than the length of list")
    ELSE
      IF( ASSOCIATED( obj%storage(pos0)%ptr ) ) THEN
        DEALLOCATE( obj%storage(pos0)%ptr )
      END IF
      obj%storage(pos0)%ptr => array(ii)%ptr
    END IF
  END DO
  CALL obj%FixValuePtrs()
END SUBROUTINE T_SetArrayAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertArray( obj, pos, array )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: array(:)
  CALL obj%Insert( pos%index, array )
END SUBROUTINE T_InsertArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SetArray( obj, pos, array )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  FTL_TEMPLATE_TYPE_WRAP, INTENT( IN ) :: array(:)
  CALL obj%Set( pos%index, array )
END SUBROUTINE T_SetArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_InsertIteratorPair( obj, pos, first, last )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: first
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: last
  CALL obj%Insert( pos%index, first%dynArray%data(first%index:last%index-1) )
END SUBROUTINE T_InsertIteratorPair

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SetIteratorPair( obj, pos, first, last )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: first
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: last
  CALL obj%Set( pos%index, first%dynArray%data(first%index:last%index-1) )
END SUBROUTINE T_SetIteratorPair

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SetIteratorPairAtIndex( obj, pos, first, last )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: first
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: last
  CALL obj%Set( pos, first%dynArray%data(first%index:last%index-1) )
END SUBROUTINE T_SetIteratorPairAtIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_EraseIndexPair( obj, first, last, freeMem )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: first
  INTEGER( I4B ), INTENT( IN ) :: last
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: freeMem
  ! Internal variable
  INTEGER( I4B ) :: i, numerased
  FTL_TEMPLATE_TYPE_WRAP :: dummy

  IF( last .GT. first ) THEN
    numerased = last - first
    DO i = first, obj%psize - numerased
      IF( ASSOCIATED( obj%storage( i )%ptr ) ) THEN
        dummy%ptr => obj%storage( i )%ptr
      ELSE
        dummy%ptr => NULL()
      END IF
      obj%storage( i )%ptr => obj%storage( i+numerased )%ptr
      obj%storage( i+numerased )%ptr => dummy%ptr
      CALL TriggerFinalizer( obj%storage(i+numerased), freeMem )
    END DO
    obj%psize = obj%psize - numerased
    CALL obj%FixValuePtrs()
  END IF
END SUBROUTINE T_EraseIndexPair

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_EraseSingle( obj, pos, freeMem )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: pos
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: freeMem

  CALL obj%Erase( pos%index, pos%index+1, freeMem )
END SUBROUTINE T_EraseSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_EraseSingleIndex( obj, pos, freeMem )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pos
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: freeMem

  CALL obj%Erase( pos, pos+1, freeMem )
END SUBROUTINE T_EraseSingleIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_EraseIteratorPair( obj, first, last, freeMem )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: first
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: last
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: freeMem

  CALL obj%Erase(first%index, last%index, freeMem)
END SUBROUTINE T_EraseIteratorPair

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Clear( obj, freeMem )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: freeMem
  CALL obj%Erase( 1, obj%psize+1, freeMem )
END SUBROUTINE T_Clear

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_FixValuePtrs(obj)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), TARGET, INTENT( INOUT ) :: obj

  IF( allocated(obj%storage) ) THEN
    obj%data => obj%storage(1:obj%psize)
  ELSE
    NULLIFY(obj%data)
  ENDIF
  IF( obj%psize .EQ. 0 ) THEN
    NULLIFY(obj%front, obj%back)
  ELSE
    obj%front => obj%storage(1)
    obj%back => obj%storage(obj%psize)
  ENDIF
END SUBROUTINE T_FixValuePtrs

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_ShiftByN(obj, from, n)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
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
END SUBROUTINE T_ShiftByN

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_ChangeCapacity( obj, n )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) ::  n
  ! internal variables
  FTL_TEMPLATE_TYPE_WRAP, ALLOCATABLE :: newstorage(:)
  INTEGER( I4B ) ::  i, numretain
  !
  IF( n .NE. (obj%Capacity()) ) THEN
    ALLOCATE(newstorage(n))
    IF( ALLOCATED(obj%storage) ) THEN
      numretain = MIN(obj%psize, n)
      DO i = 1, numretain
        newstorage(i)%ptr => obj%storage(i)%ptr
        CALL TriggerFinalizer( obj%storage(i) )
      END DO
    END IF
    CALL MOVE_ALLOC( newstorage, obj%storage )
  END IF
END SUBROUTINE T_ChangeCapacity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_IncreaseCapacity(obj)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj
  CALL obj%changeCapacity(max(2 * obj%Capacity(), 1))
END SUBROUTINE T_IncreaseCapacity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE TriggerFinalizer(obj, freeMem)
  FTL_TEMPLATE_TYPE_WRAP, INTENT( INOUT) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: freeMem
  IF( PRESENT(freeMem) )THEN
    IF( freeMem ) THEN
      DEALLOCATE(obj%ptr)
      NULLIFY(obj%ptr)
    ELSE
      NULLIFY(obj%ptr)
    END IF
  ELSE
    NULLIFY(obj%ptr)
  END IF
END SUBROUTINE TriggerFinalizer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_SWAP( obj1, obj2 )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: obj2, obj1
  ! Internal variable
  FTL_TEMPLATE_TYPE_WRAP, ALLOCATABLE :: tmpstorage(:)
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
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: src
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: dest
  CALL MOVE_ALLOC(src%storage, dest%storage)
  dest%psize = src%psize
  CALL dest%FixValuePtrs()
  CALL src%Delete()
END SUBROUTINE T_MoveDynArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_MoveArrayToDynArray(src, dest)
  FTL_TEMPLATE_TYPE_WRAP, ALLOCATABLE, INTENT( INOUT ) :: src( : )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( INOUT ) :: dest
  CALL MOVE_ALLOC(src, dest%storage)
  dest%psize = SIZE(dest%storage)
  CALL dest%FixValuePtrs()
END SUBROUTINE T_MoveArrayToDynArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Display(obj, msg, unitno)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerVector,_) ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
  ! Internal variables
  INTEGER( I4B ) :: ii
  DO ii = 1, obj%psize
    IF( ASSOCIATED( obj%storage(ii)%ptr ) ) THEN
      CALL Display(obj%storage(ii)%ptr, trim(msg)//"("//trim(str(ii, .true.))//")=", unitno)
    END IF
  END DO
END SUBROUTINE T_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Iter_NewItDefault(obj)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( INOUT ) :: obj
  NULLIFY(obj%dynArray)
  obj%index = 0
  NULLIFY(obj%value%ptr)
END SUBROUTINE T_Iter_NewItDefault

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Iter_NewItCopyOther(obj, anotherobj)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( INOUT ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: anotherobj

  obj%dynArray => anotherobj%dynArray
  obj%index = anotherobj%index
  IF( ASSOCIATED( anotherObj%value%ptr ) ) THEN
    obj%value%ptr => anotherobj%value%ptr
  ELSE
    IF( (obj%dynArray%psize .GT. 0) .AND. (obj%index .LE. obj%dynArray%psize) ) THEN
      obj%value%ptr => obj%dynArray%storage(obj%index)%ptr
    ELSE
      obj%value%ptr => NULL()
    END IF
  END IF
END SUBROUTINE T_Iter_NewItCopyOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Iter_Inc( obj )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( INOUT ) :: obj
  obj%index = obj%index + 1
  IF( obj%index .LE. obj%dynArray%psize )THEN
    obj%value%ptr => obj%dynArray%storage(obj%index)%ptr
  ELSE
    NULLIFY(obj%value%ptr)
  ENDIF
END SUBROUTINE T_Iter_Inc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE T_Iter_dec( obj )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( INOUT ) :: obj
  obj%index = obj%index - 1
  IF(obj%index .GT. 0)THEN
    obj%value%ptr => obj%dynArray%storage(obj%index)%ptr
  ELSE
    NULLIFY(obj%value%ptr)
  END IF
END SUBROUTINE T_Iter_dec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION T_Iter_AdvanceN( obj, n ) RESULT( ans )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ) :: ans

  CALL ans%Initiate(obj)
  ans%index = ans%index + n
  IF (ans%index .LE. ans%dynArray%psize) THEN
    ans%value%ptr => ans%dynArray%storage(ans%index)%ptr
  ELSE
    NULLIFY(ans%value%ptr)
  ENDIF
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION T_Iter_ReverseN( obj, n ) RESULT( ans )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ) :: ans

  CALL ans%Initiate(obj)
  ans%index = ans%index - n
  IF( ans%index > 0 )THEN
    ans%value%ptr => ans%dynArray%storage(ans%index)%ptr
  ELSE
    NULLIFY(ans%value%ptr)
  END IF
END FUNCTION T_Iter_ReverseN

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Iter_DiffOther(obj, other) RESULT(ans)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: other
  INTEGER( I4B ) :: ans
  !
  IF( ASSOCIATED( obj%dynArray, other%dynArray ) )THEN
    ans = obj%index - other%index
  ELSE
    ans = HUGE(0)
  ENDIF
END FUNCTION T_Iter_DiffOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Iter_EqualOther(obj, other) RESULT( ans )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: ans

  ans = ASSOCIATED( obj%dynArray, other%dynArray ) .AND. (obj%index .EQ. other%index)
END FUNCTION T_Iter_EqualOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Iter_UnequalOther(obj, other) RESULT(ans)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: ans

  ans = .NOT. ASSOCIATED( obj%dynArray, other%dynArray ) .OR. ( obj%index .NE. other%index )
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Iter_SmallerOther( obj, other ) RESULT( ans )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: ans

  ans = ASSOCIATED(obj%dynArray,other%dynArray) .AND. (obj%index .LT. other%index)
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Iter_SmallerEqualOther( obj, other ) RESULT(ans)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: ans
  ans = ASSOCIATED(obj%dynArray,other%dynArray) .and. (obj%index .LE. other%index)
END FUNCTION T_Iter_SmallerEqualOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Iter_GreaterOther( obj, other ) RESULT(ans)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: ans
  ans = ASSOCIATED(obj%dynArray,other%dynArray) .and. (obj%index .GT. other%index)
END FUNCTION T_Iter_GreaterOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION T_Iter_GreaterEqualOther( obj, other ) RESULT(ans)
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  TYPE( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: other
  LOGICAL( LGT ) :: ans
  ans = ASSOCIATED(obj%dynArray,other%dynArray) .and. (obj%index .GE. other%index)
END FUNCTION T_Iter_GreaterEqualOther

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE iter_display( obj, msg, unitno )
  CLASS( CAT3(FTL_TEMPLATE_TYPE_NAME,PointerIterator,_) ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno

  IF( ASSOCIATED(obj%value%ptr) ) THEN
    CALL Display(obj%value%ptr, msg, unitno)
  END IF
END SUBROUTINE iter_display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE
#endif


! #define FTL_TEMPLATE_TYPE_NAME Element
! #define FTL_TEMPLATE_TYPE_IS_DERIVED
! #define FTL_TEMPLATE_TYPE ElementPointer_
! #define USE FTL_TEMPLATE_TYPE_MODULE ElementFactory