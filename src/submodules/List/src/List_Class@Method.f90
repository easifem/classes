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

SUBMODULE( List_Class ) Method
IMPLICIT NONE
CHARACTER( LEN=* ), PARAMETER :: modName = "List_Class@Method.f90"

CONTAINS

!----------------------------------------------------------------------------
!                                                               Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE list_final
  TYPE( ListNode_ ), POINTER :: node => NULL(), tmp  => NULL()
  CHARACTER( LEN = * ), PARAMETER :: myname = "list_final"
  node => Obj%head
  DO WHILE( ASSOCIATED( node ) )
    tmp => node
    node => node%next
    SELECT TYPE( it=> tmp%data)
    CLASS IS( List_ )
      CALL it%Finalize()
    END SELECT
    DEALLOCATE( tmp%data )
    DEALLOCATE( tmp )
  END DO
  Obj%tail => NULL()
  Obj%head => NULL()
  Obj%tSize = 0
END PROCEDURE list_final

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE list_add
  IF( .NOT. ASSOCIATED( obj%tail ) ) THEN
    ALLOCATE( obj%head )
    obj%tail => obj%head
  ELSE
    ALLOCATE( obj%tail%next )
    obj%tail => obj%tail%next
  END IF
  ALLOCATE( obj%tail%data, source=data )
  obj%tsize = obj%tsize + 1
END PROCEDURE list_add

!----------------------------------------------------------------------------
!                                                                 nextIter
!----------------------------------------------------------------------------

MODULE PROCEDURE list_nextIter
  IF( ASSOCIATED( obj%iter ) ) THEN
    data => obj%iter%data
    obj%iter => obj%iter%next
  ELSE
    data => NULL()
  END IF
END PROCEDURE list_nextIter

!----------------------------------------------------------------------------
!                                                                 resetIter
!----------------------------------------------------------------------------

MODULE PROCEDURE list_resetIter
  obj%iter => obj%head
END PROCEDURE list_resetIter

!----------------------------------------------------------------------------
!                                                                       size
!----------------------------------------------------------------------------

MODULE PROCEDURE list_size
  Ans = Obj%tSize
END PROCEDURE list_size

!----------------------------------------------------------------------------
!                                                                    getHead
!----------------------------------------------------------------------------

MODULE PROCEDURE list_gethead
  data => Obj%head%data
END PROCEDURE list_gethead

!----------------------------------------------------------------------------
!                                                                    getTail
!----------------------------------------------------------------------------

MODULE PROCEDURE list_gettail
  data => Obj%tail%data
END PROCEDURE list_gettail

!----------------------------------------------------------------------------
!                                                                     getNth
!----------------------------------------------------------------------------

MODULE PROCEDURE list_getNth
  INTEGER( I4B ) :: i
  TYPE( ListNode_ ), POINTER :: node
  IF( ASSOCIATED( obj%head ) .AND. obj%tSize .GE. n ) THEN
    node => obj%head
    i = 1
    DO
      IF( i .EQ. n ) THEN
        data => node%data
        EXIT
      ELSE IF( i<n .AND. .NOT. ASSOCIATED( node%next ) ) THEN
        data => NULL()
        EXIT
      ELSE
        i = i + 1
        node => node%next
      END IF
    END DO
  ELSE
    data => NULL()
  END IF
END PROCEDURE list_getNth

!----------------------------------------------------------------------------
!                                                                    popTail
!----------------------------------------------------------------------------

MODULE PROCEDURE list_poptail
  TYPE( ListNode_ ), POINTER :: node, lastNode

  IF( ASSOCIATED( Obj%tail ) ) THEN
    data => Obj%tail%data
    lastNode => NULL()
    node => Obj%head

    DO While( ASSOCIATED( node%next ) )
      lastNode => node
      node => node%next
    END DO

    IF( ASSOCIATED( lastNode ) ) THEN
      DEALLOCATE( lastNode%next )
    ELSE
      DEALLOCATE( Obj%head )
    END IF

    Obj%tail =>lastNode
    Obj%tSize = Obj%tSize - 1
  ELSE
    data => NULL()
  END IF

  NULLIFY( node, lastnode )

END PROCEDURE list_poptail

!----------------------------------------------------------------------------
!                                                                    popHead
!----------------------------------------------------------------------------

MODULE PROCEDURE list_pophead
  TYPE( ListNode_ ), POINTER :: node
  IF( ASSOCIATED( obj%head ) ) THEN
    data => obj%head%data
    node => obj%head%next
    DEALLOCATE( obj%head )
    obj%head => node
    obj%tsize = Obj%tsize - 1
    IF( obj%tsize .EQ. 0 ) obj%head => NULL()
  ELSE
    data => NULL()
  END IF
  NULLIFY( node )
END PROCEDURE list_pophead

!----------------------------------------------------------------------------
!                                                                      Push
!----------------------------------------------------------------------------

MODULE PROCEDURE list_push
  CALL Obj%add(data)
END PROCEDURE list_push

END SUBMODULE Method

