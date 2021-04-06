
!----------------------------------------------------------------------------
!                                                            LIST_NODE_NAME
!----------------------------------------------------------------------------

TYPE LIST_NODE_NAME
  DATA_TYPE_NAME :: data
  TYPE( LIST_NODE_NAME ), POINTER :: next =>null()
END TYPE LIST_NODE_NAME

PUBLIC :: LIST_NODE_NAME

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE LIST_NAME
  TYPE( LIST_NODE_NAME ), POINTER :: head => null()
  TYPE( LIST_NODE_NAME ), POINTER :: tail => null()
  TYPE( LIST_NODE_NAME ), POINTER :: iter => null()
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
END TYPE LIST_NAME

PUBLIC :: LIST_NAME

!----------------------------------------------------------------------------
!                                                           Finalize@Method
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE list_final( Obj )
  CLASS( LIST_NAME ), INTENT( INOUT) :: Obj
END SUBROUTINE list_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_add( Obj, data )
  CLASS( LIST_NAME ), INTENT( INOUT ) :: Obj
  DATA_TYPE_NAME, INTENT( IN ) :: data
END SUBROUTINE list_add
END INTERFACE


!----------------------------------------------------------------------------
!                                                       nextIter@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_nextIter( Obj, data )
  CLASS( LIST_NAME ), INTENT( INOUT) :: Obj
  DATA_TYPE_NAME, INTENT( INOUT ) :: data
END SUBROUTINE list_nextIter
END INTERFACE

!----------------------------------------------------------------------------
!                                                          resetIter@Method
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_resetIter( Obj )
  CLASS( LIST_NAME ), INTENT( INOUT) :: Obj
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION list_size( Obj ) RESULT( Ans )
  CLASS( LIST_NAME ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION list_size
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_gethead( Obj, data )
  CLASS( LIST_NAME ), INTENT( IN ) :: Obj
  DATA_TYPE_NAME, INTENT( INOUT ) :: data
END SUBROUTINE list_gethead
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_gettail( Obj, data )
  CLASS( LIST_NAME ), INTENT( IN ) :: Obj
  DATA_TYPE_NAME, INTENT( INOUT ) :: data
END SUBROUTINE list_gettail
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_getNth( Obj, n, data )
  CLASS( LIST_NAME ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: n
  DATA_TYPE_NAME, INTENT( INOUT ) ::  data
END SUBROUTINE list_getNth
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_popTail( Obj, data )
  CLASS( LIST_NAME ), INTENT( INOUT ) :: Obj
  DATA_TYPE_NAME, INTENT( INOUT ) :: data
END SUBROUTINE list_popTail
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_popHead( Obj, data )
  CLASS( LIST_NAME ), INTENT( INOUT ) :: Obj
  DATA_TYPE_NAME, INTENT( INOUT ) :: data
END SUBROUTINE list_popHead
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE list_push( Obj, data )
  CLASS( LIST_NAME ), INTENT( INOUT ) :: Obj
  DATA_TYPE_NAME, INTENT( IN ) :: data
END SUBROUTINE list_push
END INTERFACE
