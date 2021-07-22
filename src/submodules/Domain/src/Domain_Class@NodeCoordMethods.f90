!> reading nodeCoord
  CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
      & 'reading nodeCoord' )
  IF( .NOT. meshFile%pathExists(TRIM(dsetname) // "/nodeCoord") ) THEN
    CALL eMesh%raiseError(modName//'::'//myName// " - "// &
      & TRIM(dsetname) // "/nodeCoord" // ' path does not exists' )
  ELSE
    CALL meshFile%read( TRIM(dsetname) // "/nodeCoord", &
      & obj%nodeCoord )
    IF( eMesh%isLogActive() ) THEN
      CALL eMesh%raiseInformation(modName//'::'//myName// " - "// &
        & 'nodal coordinates are given below = ' )
      CALL DISP( title="x", x=obj%nodeCoord( 1, : ), &
        & unit = eMesh%getLogFileUnit(), style="UNDERLINE", &
        & advance="NO" )
      CALL DISP( title="y", x=obj%nodeCoord( 2, : ), &
        & unit = eMesh%getLogFileUnit(), style="UNDERLINE", &
        & advance="NO" )
      CALL DISP( title="z", x=obj%nodeCoord( 3, : ), &
        & unit = eMesh%getLogFileUnit(), style="UNDERLINE", &
        & advance="DOUBLE" )
    END IF
  END IF


!----------------------------------------------------------------------------
!                                                             getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeCoord_1
  IF( obj%isNodePresent( GlobalNode ) ) THEN
    ans = obj%nodeCoord( 1:3, obj%getLocalNptrs( GlobalNode ) )
  ELSE
    ans = 0.0_DFP
  END IF
END PROCEDURE mesh_getNodeCoord_1


!----------------------------------------------------------------------------
!                                                             getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeCoord_2
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalNode )
    ans( 1:3, ii ) = mesh_getNodeCoord_1( obj, GlobalNode( ii ) )
  END DO
END PROCEDURE mesh_getNodeCoord_2

!----------------------------------------------------------------------------
!                                                             getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeCoord_3
  INTEGER( I4B ) :: ii
  IF( ALLOCATED( obj%nodeCoord ) ) THEN
    ans = obj%nodeCoord
  ELSE
    ALLOCATE( ans( 0, 0 ) )
  END IF
END PROCEDURE mesh_getNodeCoord_3


!----------------------------------------------------------------------------
!                                                              setNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setNodeCoord_1
  INTEGER( I4B ) :: ii
  IF( obj%isNodePresent( GlobalNode ) ) THEN
    ii = obj%getLocalNptrs( GlobalNode )
    obj%nodeCoord( 1:3, ii ) = obj%nodeCoord( 1:3, ii ) + nodeCoord(1:3)
  END IF
END PROCEDURE mesh_setNodeCoord_1

!----------------------------------------------------------------------------
!                                                              setNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setNodeCoord_2
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalNode )
    CALL mesh_setNodeCoord_1(obj, GlobalNode=GlobalNode( ii ), &
      & nodeCoord=nodeCoord( 1:3, ii ) )
  END DO
END PROCEDURE mesh_setNodeCoord_2

!----------------------------------------------------------------------------
!                                                              setNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setNodeCoord_3
  obj%nodeCoord = nodeCoord
END PROCEDURE mesh_setNodeCoord_3


!----------------------------------------------------------------------------
!                                                   getNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns nodal coordinates

INTERFACE
MODULE PURE FUNCTION mesh_getNodeCoord_1( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  REAL( DFP ) :: ans( 3 )
END FUNCTION mesh_getNodeCoord_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns nodal coordinates

INTERFACE
MODULE PURE FUNCTION mesh_getNodeCoord_2( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode( : )
  REAL( DFP ) :: ans( 3, SIZE( GlobalNode ) )
END FUNCTION mesh_getNodeCoord_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns nodal coordinates

INTERFACE
MODULE PURE FUNCTION mesh_getNodeCoord_3( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION mesh_getNodeCoord_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: set the nodal coordinates

INTERFACE
MODULE PURE SUBROUTINE mesh_setNodeCoord_1( obj, GlobalNode, nodeCoord )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  REAL( DFP ), INTENT( IN ) :: nodeCoord( 3 )
END SUBROUTINE mesh_setNodeCoord_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: set the nodal coordinates

INTERFACE
MODULE PURE SUBROUTINE mesh_setNodeCoord_2( obj, GlobalNode, nodeCoord )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode( : )
  REAL( DFP ), INTENT( IN ) :: nodeCoord( 3, SIZE( GlobalNode ) )
END SUBROUTINE mesh_setNodeCoord_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: set the nodal coordinates

INTERFACE
MODULE PURE SUBROUTINE mesh_setNodeCoord_3( obj, nodeCoord )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: nodeCoord( :, : )
END SUBROUTINE mesh_setNodeCoord_3
END INTERFACE
