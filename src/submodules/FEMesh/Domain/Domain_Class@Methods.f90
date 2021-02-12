SUBMODULE( Domain_Class ) Methods
  !! Contains methods related to [[Domain_]] or collection of [[Mesh_]]
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_obj

  IF( PRESENT( tOmega ) ) THEN
    IF( ALLOCATED( Obj % Omega ) ) THEN
      DEALLOCATE( Obj % Omega )
    END IF

    IF( tOmega .NE. 0 ) THEN
      ALLOCATE( Obj % Omega( tOmega ) )
    END IF

    IF( ALLOCATED( Obj % mdOmega ) ) THEN
      DEALLOCATE( Obj % mdOmega )
    END IF

    IF( tOmega .NE. 0 ) THEN
      ALLOCATE( Obj % mdOmega( tOmega ) )
    END IF
  END IF

  IF( PRESENT( tBoundary ) ) THEN
    IF( ALLOCATED( Obj % Boundary ) ) THEN
      DEALLOCATE( Obj % Boundary )
    END IF

    IF( tBoundary .NE. 0 ) THEN
      ALLOCATE( Obj % Boundary( tBoundary ) )
    END IF

    IF( ALLOCATED( Obj % mdBoundary ) ) THEN
      DEALLOCATE( Obj % mdBoundary )
    END IF

    IF( tBoundary .NE. 0 ) THEN
      ALLOCATE( Obj % mdBoundary( tBoundary ) )
    END IF
  END IF

  IF( PRESENT( tEdge ) ) THEN
    IF( ALLOCATED( Obj % Edge ) ) THEN
      DEALLOCATE( Obj % Edge )
    END IF

    IF( tEdge .NE. 0 ) THEN
      ALLOCATE( Obj % Edge( tEdge ) )
    END IF

    IF( ALLOCATED( Obj % mdEdge ) ) THEN
      DEALLOCATE( Obj % mdEdge )
    END IF

    IF( tEdge .NE. 0 ) THEN
      ALLOCATE( Obj % mdEdge( tEdge ) )
    END IF
  END IF

END PROCEDURE Initiate_obj

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_obj
  IF( ALLOCATED( Obj % Omega ) ) DEALLOCATE( Obj % Omega )
  IF( ALLOCATED( Obj % Boundary ) ) DEALLOCATE( Obj % Boundary )
  IF( ALLOCATED( Obj % Edge ) ) DEALLOCATE( Obj % Edge )
  IF( ALLOCATED( Obj % mdOmega ) ) DEALLOCATE( Obj % mdOmega )
  IF( ALLOCATED( Obj % mdBoundary ) ) DEALLOCATE( Obj % mdBoundary )
  IF( ALLOCATED( Obj % mdEdge ) ) DEALLOCATE( Obj % mdEdge )
  IF( ALLOCATED( Obj % omega_name ) ) DEALLOCATE( Obj % omega_name )
  IF( ALLOCATED( Obj % boundary_name ) ) DEALLOCATE( Obj % boundary_name )
  IF( ALLOCATED( Obj % edge_name ) ) DEALLOCATE( Obj % edge_name )

  IF( obj % allocateNodes ) THEN
    IF( ASSOCIATED( Obj % Nodes ) ) THEN
      DEALLOCATE( obj % Nodes )
    END IF
    obj % nodes => NULL( )
    obj % allocateNodes = .FALSE.
  ELSE
    obj % nodes => NULL( )
  END IF

  IF( ALLOCATED( Obj % NodalVelocity ) ) THEN
    DEALLOCATE( obj % NodalVelocity )
  END IF

  IF( ALLOCATED( Obj % NodalAcceleration ) ) THEN
    DEALLOCATE( obj % NodalAcceleration )
  END IF

END PROCEDURE deallocate_obj

!----------------------------------------------------------------------------
!                                                         ConnectFacetToCell
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_connect_facet_cell
  ! Check allocation
  IF( .NOT. ALLOCATED( Dom % Omega ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.f90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Dom % Omega not allocated" )
    STOP
  END IF

  ! Check allocation
  IF( .NOT. ALLOCATED( Dom % mdOmega ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.f90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Dom % mdOmega not allocated" )
    STOP
  END IF

  ! Check allocation
  IF( .NOT. ALLOCATED( Dom % Boundary ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.f90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Dom % Boundary not allocated" )
    STOP
  END IF

  !> check the validity index
  IF( OmegaIndx .GT. SIZE( Dom % Omega ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.f90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Out of bound error" )
    STOP
  END IF

  !> check the validity index
  IF( BoundaryIndx .GT. SIZE( Dom % Boundary ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.f90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Out of bound error" )
    STOP
  END IF

  !> Report error if already associated
  IF( .NOT. ASSOCIATED( Dom % Omega( OmegaIndx ) % Ptr ) ) THEN

    CALL Display( "ERROR :: Domain_Class@Methods.f90" )
    CALL Display( "         msh4_getelements_4()" )
    CALL Display( "            Dom % Omega(OmegaIndx)%Ptr is not associated")
    STOP
  END IF

  !> Report error if already associated
  IF( .NOT. ASSOCIATED( Dom % Boundary( BoundaryIndx ) % Ptr ) ) THEN

    CALL Display( "ERROR :: Domain_Class@Methods.f90" )
    CALL Display( "         msh4_getelements_4()" )
    CALL Display( "            Dom % Boundary(BoundaryIndx)%Ptr is not &
      & associated")
    STOP
  END IF

  !> Report error if already associated
  IF( .NOT. ASSOCIATED( Dom % mdOmega( OmegaIndx ) % Ptr ) ) THEN

    CALL Display( "IN :: Domain_Class@Methods.f90" )
    CALL Display( "         msh4_getelements_4()" )
    CALL Display( "            Initiating Dom % mdOmega( cellIndx )" )

    ALLOCATE( Dom % mdOmega( OmegaIndx ) % Ptr )

    CALL Dom % mdOmega( OmegaIndx ) % Ptr % Initiate( &
      & Dom % Omega( OmegaIndx ) % Ptr )
  END IF

  CALL Dom % mdOmega( OmegaIndx ) % Ptr % ConnectFacetToCell( &
    & CellMesh = Dom % Omega( OmegaIndx ) % Ptr, &
    & FacetMesh = Dom % Boundary( BoundaryIndx ) % Ptr )
END PROCEDURE mc_connect_facet_cell

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods