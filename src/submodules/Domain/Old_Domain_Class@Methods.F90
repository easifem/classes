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
    IF( ALLOCATED( obj % Omega ) ) THEN
      DEALLOCATE( obj % Omega )
    END IF

    IF( tOmega .NE. 0 ) THEN
      ALLOCATE( obj % Omega( tOmega ) )
    END IF

    IF( ALLOCATED( obj % mdOmega ) ) THEN
      DEALLOCATE( obj % mdOmega )
    END IF

    IF( tOmega .NE. 0 ) THEN
      ALLOCATE( obj % mdOmega( tOmega ) )
    END IF
  END IF

  IF( PRESENT( tBoundary ) ) THEN
    IF( ALLOCATED( obj % Boundary ) ) THEN
      DEALLOCATE( obj % Boundary )
    END IF

    IF( tBoundary .NE. 0 ) THEN
      ALLOCATE( obj % Boundary( tBoundary ) )
    END IF

    IF( ALLOCATED( obj % mdBoundary ) ) THEN
      DEALLOCATE( obj % mdBoundary )
    END IF

    IF( tBoundary .NE. 0 ) THEN
      ALLOCATE( obj % mdBoundary( tBoundary ) )
    END IF
  END IF

  IF( PRESENT( tEdge ) ) THEN
    IF( ALLOCATED( obj % Edge ) ) THEN
      DEALLOCATE( obj % Edge )
    END IF

    IF( tEdge .NE. 0 ) THEN
      ALLOCATE( obj % Edge( tEdge ) )
    END IF

    IF( ALLOCATED( obj % mdEdge ) ) THEN
      DEALLOCATE( obj % mdEdge )
    END IF

    IF( tEdge .NE. 0 ) THEN
      ALLOCATE( obj % mdEdge( tEdge ) )
    END IF
  END IF

END PROCEDURE Initiate_obj

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_obj
  IF( ALLOCATED( obj % Omega ) ) DEALLOCATE( obj % Omega )
  IF( ALLOCATED( obj % Boundary ) ) DEALLOCATE( obj % Boundary )
  IF( ALLOCATED( obj % Edge ) ) DEALLOCATE( obj % Edge )
  IF( ALLOCATED( obj % mdOmega ) ) DEALLOCATE( obj % mdOmega )
  IF( ALLOCATED( obj % mdBoundary ) ) DEALLOCATE( obj % mdBoundary )
  IF( ALLOCATED( obj % mdEdge ) ) DEALLOCATE( obj % mdEdge )
  IF( ALLOCATED( obj % omega_name ) ) DEALLOCATE( obj % omega_name )
  IF( ALLOCATED( obj % boundary_name ) ) DEALLOCATE( obj % boundary_name )
  IF( ALLOCATED( obj % edge_name ) ) DEALLOCATE( obj % edge_name )

  IF( obj % allocateNodes ) THEN
    IF( ASSOCIATED( obj % Nodes ) ) THEN
      DEALLOCATE( obj % Nodes )
    END IF
    obj % nodes => NULL( )
    obj % allocateNodes = .FALSE.
  ELSE
    obj % nodes => NULL( )
  END IF

  IF( ALLOCATED( obj % NodalVelocity ) ) THEN
    DEALLOCATE( obj % NodalVelocity )
  END IF

  IF( ALLOCATED( obj % NodalAcceleration ) ) THEN
    DEALLOCATE( obj % NodalAcceleration )
  END IF

END PROCEDURE deallocate_obj

!----------------------------------------------------------------------------
!                                                         ConnectFacetToCell
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_connect_facet_cell
  ! Check allocation
  IF( .NOT. ALLOCATED( Dom % Omega ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.F90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Dom % Omega not allocated" )
    STOP
  END IF

  ! Check allocation
  IF( .NOT. ALLOCATED( Dom % mdOmega ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.F90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Dom % mdOmega not allocated" )
    STOP
  END IF

  ! Check allocation
  IF( .NOT. ALLOCATED( Dom % Boundary ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.F90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Dom % Boundary not allocated" )
    STOP
  END IF

  !> check the validity index
  IF( OmegaIndx .GT. SIZE( Dom % Omega ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.F90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Out of bound error" )
    STOP
  END IF

  !> check the validity index
  IF( BoundaryIndx .GT. SIZE( Dom % Boundary ) ) THEN
    CALL Display( "ERROR :: Domain_Class@Methods.F90" )
    CALL Display( "         mc_connect_facet_cell()" )
    CALL Display( "            Out of bound error" )
    STOP
  END IF

  !> Report error if already associated
  IF( .NOT. ASSOCIATED( Dom % Omega( OmegaIndx ) % Ptr ) ) THEN

    CALL Display( "ERROR :: Domain_Class@Methods.F90" )
    CALL Display( "         msh4_getelements_4()" )
    CALL Display( "            Dom % Omega(OmegaIndx)%Ptr is not associated")
    STOP
  END IF

  !> Report error if already associated
  IF( .NOT. ASSOCIATED( Dom % Boundary( BoundaryIndx ) % Ptr ) ) THEN

    CALL Display( "ERROR :: Domain_Class@Methods.F90" )
    CALL Display( "         msh4_getelements_4()" )
    CALL Display( "            Dom % Boundary(BoundaryIndx)%Ptr is not &
      & associated")
    STOP
  END IF

  !> Report error if already associated
  IF( .NOT. ASSOCIATED( Dom % mdOmega( OmegaIndx ) % Ptr ) ) THEN

    CALL Display( "IN :: Domain_Class@Methods.F90" )
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