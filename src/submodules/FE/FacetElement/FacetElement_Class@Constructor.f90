SUBMODULE( FacetElement_Class ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------
!                                                               Element
!------------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Obj % Initiate( Nptrs = Nptrs, Mat_Type = Mat_Type, RefElem = RefElem )
END PROCEDURE Constructor1

MODULE PROCEDURE Constructor2
  Obj % Mat_Type = -1
  Obj % Nptrs = [-1]
  Obj % RefElem => NULL( )
END PROCEDURE Constructor2

MODULE PROCEDURE Constructor3
  CALL Obj % Initiate( AnotherObj )
END PROCEDURE Constructor3

!------------------------------------------------------------------------------
!                                                                     Display
!------------------------------------------------------------------------------

MODULE PROCEDURE s_display_Obj
  INTEGER( I4B ) :: I
  !
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  !
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  !
  CALL Blanklines( NOL = 1, UnitNo = I )
  WRITE( I, "(A)" ) "  TYPE :: FACETELEMENT_"
  WRITE( I, "(A, I4)" ) "  MAT-TYPE :: ", Obj % Mat_Type
  CALL Display( Obj % Nptrs, "  NPTRS :: ")

  IF( ASSOCIATED( Obj % Cell ) ) THEN
    WRITE( I, "(A)" ) "  INNER CELL :: ASSOCIATED"
  ELSE
    WRITE( I, "(A)" ) "  INNER CELL :: NOT ASSOCIATED"
  END IF

  IF( ASSOCIATED( Obj % OuterCell ) ) THEN
    WRITE( I, "(A)" ) "  OUTER CELL :: ASSOCIATED"
  ELSE
    WRITE( I, "(A)" ) "  OUTER CELL :: NOT ASSOCIATED"
  END IF

  CALL Blanklines( NOL = 1, UnitNo = I )
  !
  IF( PRESENT( FullDisp ) ) THEN
    IF( FullDisp ) THEN
      IF( ASSOCIATED( Obj % RefElem ) ) THEN
        CALL Display( Obj % RefElem, "### Reference Element", I )
      END IF
      !
      CALL Blanklines( NOL = 1, UnitNo = I )
      IF( ASSOCIATED( Obj % Cell ) ) THEN
        CALL Display( Obj % Cell, "### Inner Cell", I )
      END IF
      !
      CALL Blanklines( NOL = 1, UnitNo = I )
      IF( ASSOCIATED( Obj % OuterCell ) ) THEN
        CALL Display( Obj % OuterCell, "### Outer Cell", I )
      END IF
    END IF
  END IF
END PROCEDURE s_display_Obj

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE m_display_obj
  INTEGER( I4B ) :: i
  LOGICAL :: full

  IF( PRESENT( UnitNo ) ) THEN
    i = UnitNo
  ELSE
    i = stdout
  END IF

  IF( PRESENT( FullDisp ) ) THEN
    full = FullDisp
  ELSE
    full = .false.
  END IF

  SELECT TYPE( Obj )
  TYPE IS ( FacetElement_ )
    CALL Display( Obj = Obj, Msg = Msg, UnitNo = i, FullDisp = Full )
  END SELECT
END PROCEDURE m_display_obj

!------------------------------------------------------------------------------
!                                                               DeallocateData
!------------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Data
  IF( ALLOCATED( Obj % Nptrs ) ) DEALLOCATE( Obj % Nptrs )
  Obj % MAT_Type = 0
  Obj % RefElem => NULL( )
  Obj % LocalId = 0
  Obj % Cell => NULL( )
  Obj % OuterCell => NULL( )
END PROCEDURE Deallocate_Data

!------------------------------------------------------------------------------
!                                                                 getCellNptrs
!------------------------------------------------------------------------------

MODULE PROCEDURE getCellNptrs
  Ans = Obj % Cell % Nptrs
END PROCEDURE getCellNptrs

!------------------------------------------------------------------------------
!                                                            SetPointerToCell
!------------------------------------------------------------------------------

MODULE PROCEDURE SetPointerToCell
  Obj % Cell => CellObj
END PROCEDURE SetPointerToCell

!------------------------------------------------------------------------------
!                                                             getPointerToCell
!------------------------------------------------------------------------------

MODULE PROCEDURE getPointerToCell
  CellObj => Obj % Cell
END PROCEDURE getPointerToCell

!------------------------------------------------------------------------------
!                                                            FreePointerToCell
!------------------------------------------------------------------------------

MODULE PROCEDURE FreePointerToCell
  Obj % Cell => NULL( )
END PROCEDURE FreePointerToCell

!------------------------------------------------------------------------------
!                                                            getFacetLocalID
!------------------------------------------------------------------------------

MODULE PROCEDURE getFacetLocalID
  Ans = Obj % LocalID
END PROCEDURE getFacetLocalID

!------------------------------------------------------------------------------
!                                                             setFacetLocalID
!------------------------------------------------------------------------------

MODULE PROCEDURE setFacetLocalID
  Obj % LocalID = Id
END PROCEDURE setFacetLocalID

!------------------------------------------------------------------------------
!                                                          getFacetLocalNptrs
!------------------------------------------------------------------------------

MODULE PROCEDURE getFacetLocalNptrs
  ! Define internal variables
  INTEGER( I4B ) :: i, j, b
  INTEGER( I4B ), ALLOCATABLE :: FM( :, : ), CellNptrs( : ), FacetNptrs( : ), &
    & DummyNptrs( : )

  ASSOCIATE( RefCellElem => Obj % Cell % RefElem )

  j = Obj % FacetLocalID( )
  FM = FacetMatrix( RefCellElem )
  b = FM( j, 3 ) + 3
  Nptrs = FM( j, 4 : b )
  CellNptrs = Obj % Cell % getNptrs( )
  FacetNptrs = Obj % getNptrs( )

  !<-- Not that these nptrs are such that area points outerward
  !<-- however we need to fix the ordering so that
  !<--- NptrsOfCell( LocalNptrs( 1 ) ) .eq. NptrsOfFacet( 1 )

  IF( ANY( FacetNptrs .NE. CellNptrs( Nptrs ) ) ) THEN
    b = SIZE( Nptrs ) ! tnodes in facet
    DummyNptrs = Nptrs ! copy of facet nptrs
    DO i = 1, b
      DO j = 1, b
        IF( FacetNptrs( i ) .EQ. CellNptrs( DummyNptrs( j ) )  ) THEN
          Nptrs( i ) = DummyNptrs( j )
        END IF
      END DO
    END DO
    DEALLOCATE( DummyNptrs )
  END IF

  DEALLOCATE( FM, CellNptrs, FacetNptrs )

  END ASSOCIATE

END PROCEDURE getFacetLocalNptrs

END SUBMODULE Constructor
