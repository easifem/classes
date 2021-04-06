!----------------------------------------------------------------------------
!                                                               Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE list_final
  TYPE( LIST_NODE_NAME ), POINTER :: node => NULL(), tmp  => NULL()
  node => Obj%head
  DO WHILE( ASSOCIATED( node ) )
    tmp => node
    node => node%next
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
  obj%tail%data = data
  obj%tsize = obj%tsize + 1
END PROCEDURE list_add

!----------------------------------------------------------------------------
!                                                                 nextIter
!----------------------------------------------------------------------------

MODULE PROCEDURE list_nextIter
  CHARACTER( LEN = * ), PARAMETER :: myName = "list_nextIter"

  IF( ASSOCIATED( obj%iter ) ) THEN
    data = obj%iter%data
    obj%iter => obj%iter%next
  ELSE
    CALL EXCEPTION%raiseError( modName//"::"//myName//" - "// &
      & "Obj%iter is not associated; use obj%resetIter() first;")
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
  TYPE( LIST_NODE_NAME ), POINTER :: node
  node => Obj%head
  data = node%data
  nullify( node )
END PROCEDURE list_gethead

!----------------------------------------------------------------------------
!                                                                    getTail
!----------------------------------------------------------------------------

MODULE PROCEDURE list_gettail
  TYPE( LIST_NODE_NAME ), POINTER :: node
  node => Obj%tail
  data = node%data
  nullify( node )
END PROCEDURE list_gettail

!----------------------------------------------------------------------------
!                                                                     getNth
!----------------------------------------------------------------------------

MODULE PROCEDURE list_getNth
  INTEGER( I4B ) :: i
  CHARACTER( LEN = * ), PARAMETER :: myName = "list_getNth"
  TYPE( LIST_NODE_NAME ), POINTER :: node

  IF( ASSOCIATED( obj%head ) .AND. obj%tSize .GE. n ) THEN
    node => obj%head
    i = 1
    DO
      IF( i .EQ. n ) THEN
        data = node%data
        EXIT
      ELSE IF( i<n .AND. .NOT. ASSOCIATED( node%next ) ) THEN
        CALL EXCEPTION%raiseError( modName//"::"//myName//" - "// &
          & "Some of the entries are not associated.")
        ! data = NULL()
        EXIT
      ELSE
        i = i + 1
        node => node%next
      END IF
    END DO
  ELSE
    CALL EXCEPTION%raiseError( modName//"::"//myName//" - "// &
      & "It seems list is not initiated or nth is out of bound")
  END IF
END PROCEDURE list_getNth

!----------------------------------------------------------------------------
!                                                                    popTail
!----------------------------------------------------------------------------

MODULE PROCEDURE list_poptail
  TYPE( LIST_NODE_NAME ), POINTER :: node, lastNode
  CHARACTER( LEN = * ), PARAMETER :: myName = "list_poptail"

  IF( ASSOCIATED( Obj%tail ) ) THEN
    data = Obj%tail%data
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
    CALL EXCEPTION%raiseError( modName//"::"//myName//" - "// &
      & "It seems list is not initiated.")
    ! raise error
  END IF

  NULLIFY( node, lastnode )

END PROCEDURE list_poptail

!----------------------------------------------------------------------------
!                                                                    popHead
!----------------------------------------------------------------------------

MODULE PROCEDURE list_pophead
  TYPE( LIST_NODE_NAME ), POINTER :: node
  CHARACTER( LEN = * ), PARAMETER :: myName = "list_pophead"


  IF( ASSOCIATED( obj%head ) ) THEN
    data = obj%head%data
    node => obj%head%next
    DEALLOCATE( obj%head )
    obj%head => node
    obj%tsize = Obj%tsize - 1
    IF( obj%tsize .EQ. 0 ) obj%head => NULL()
  ELSE
    CALL EXCEPTION%raiseError( modName//"::"//myName//" - "// &
      & "It seems list is not initiated")
  END IF
  NULLIFY( node )
END PROCEDURE list_pophead

!----------------------------------------------------------------------------
!                                                                      Push
!----------------------------------------------------------------------------

MODULE PROCEDURE list_push
  CALL Obj%add(data)
END PROCEDURE list_push
