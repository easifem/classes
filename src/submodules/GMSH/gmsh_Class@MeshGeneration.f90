SUBMODULE( gmsh_Class ) MeshGeneration
USE gmshMesh_Class
USE mshEntity_Class
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_from_gmsh
  Ans = 0
  SELECT CASE( Obj % nsd )
  CASE( 2 )
    CALL remesh_domain_using_gmsh_2D( Obj=Obj%model%Mesh,&
      & Nodes=Nodes, gmsh=gmsh )
  CASE( 3 )
    CALL remesh_domain_using_gmsh_3D( Obj=Obj%model%Mesh,&
      & Nodes=Nodes, gmsh=gmsh )
  END SELECT
END PROCEDURE gmsh_from_gmsh

!----------------------------------------------------------------------------
!                                                                     Remesh
!----------------------------------------------------------------------------

SUBROUTINE remesh_domain_using_gmsh_3D( Obj, gmsh, Nodes )
  CLASS( gmshMesh_ ), TARGET, INTENT( INOUT) :: Obj
  CLASS( gmsh_ ), TARGET, INTENT( INOUT) :: gmsh
  REAL( DFP ), INTENT( IN ) :: Nodes( :, : )
END SUBROUTINE

!----------------------------------------------------------------------------
!                                                                     Remesh
!----------------------------------------------------------------------------

SUBROUTINE remesh_domain_using_gmsh_2D( Obj, gmsh, Nodes )
  CLASS( gmshMesh_ ), TARGET, INTENT( INOUT) :: Obj
  CLASS( gmsh_ ), TARGET, INTENT( INOUT) :: gmsh
  REAL( DFP ), INTENT( IN ) :: Nodes( :, : )

  ! Internal variables
  INTEGER( i4b ) :: ierr
  INTEGER( i4b ) :: ii, kk, jj, iel
  INTEGER( i4b ) :: tag
  INTEGER( i4b ) :: ient, ibent,countLine
  INTEGER( i4b ) :: nsd
  INTEGER( i4b ) :: tOmega, tNodes, loop(1), ps(1), telem
  INTEGER( I4B ), ALLOCATABLE :: Ent(:), bent(:), ll(:)
  INTEGER( I4B ), POINTER :: elemNum( : ), NodeNum( : ), Nptrs(:,:)
  CLASS( mshEntity_ ), POINTER :: SurfaceEntity, CurveEntity
  REAL(DFP) :: x(3)
  LOGICAL( LGT ), ALLOCATABLE :: NodePresent( : ), LinePresent( : )
  TYPE( String ), ALLOCATABLE :: omega_name( : ), boundary_name( : )

  ! main program
  IF( .NOT. ASSOCIATED( gmsh % model ) ) THEN
    CALL Display( "ERROR:: gmsh_Class@MeshGeneration.f90")
    CALL Display( "        remesh_domain_using_gmsh_2D()")
    CALL Display( "        new gmsh model is not initiated" )
    STOP
  END IF

  IF( .NOT. ASSOCIATED( gmsh % model % geo ) ) THEN
    CALL Display( "ERROR:: gmsh_Class@MeshGeneration.f90")
    CALL Display( "        remesh_domain_using_gmsh_2D()")
    CALL Display( "         no model found in gmsh" )
    CALL Display( "         run gmsh%model%add()" )
    STOP
  END IF

  nsd = gmsh % nsd
  NULLIFY( SurfaceEntity, CurveEntity, elemNum, NodeNum, Nptrs )
  x = 0.0_DFP
  tNodes = SIZE( Nodes, 2 )
  ALLOCATE( NodePresent( tNodes ) )
  NodePresent = .FALSE.

  tOmega = obj%PhysicalNames%TotalPhysicalSurfaces()

  IF( tOmega .EQ. 0 ) THEN
    CALL Display( "ERROR:: gmsh_Class@MeshGeneration.f90")
    CALL Display( "        remesh_domain_using_gmsh_2D()")
    CALL Display( "         Physical Surfaces not found" )
    STOP
  END IF

  omega_name = obj%PhysicalNames%PhysicalSurfaceNames()

  ! count total number of lines
  countLine = 0
  DO ii = 1, tOmega
    tag = obj % PhysicalNames % IndexOfPhysicalSurface( ii )
    Ent = obj % PhysicalNames % Entities(tag) % Val
    DO ient = 1, SIZE( Ent )
      SurfaceEntity => Obj%SurfaceEntities(Ent(ient))
      bent = SurfaceEntity%BoundingEntity
      DO ibent=1,SIZE(bent)
        CurveEntity => Obj%CurveEntities(ABS(bent(ibent)))
        countLine = countLine + SIZE( CurveEntity%elemNumber )
      END DO
    END DO
  END DO

  ALLOCATE( LinePresent( countLine ) )
  LinePresent = .FALSE.

  !--------------------------------------------------------------------------
  ! Adding physical surfaces
  !--------------------------------------------------------------------------
  countLine = 0
  DO ii = 1, tOmega

    tag = obj % PhysicalNames % IndexOfPhysicalSurface( ii )

    ! now get surface entities associated with the physical surface
    Ent = obj % PhysicalNames % Entities(tag) % Val

    ! loop over surface entities
    DO ient = 1, SIZE( Ent )
      SurfaceEntity => Obj%SurfaceEntities(Ent(ient))
      bent = SurfaceEntity%BoundingEntity

      DO ibent=1,SIZE(bent)

        !! get the boundary curve
        CurveEntity => Obj%CurveEntities(ABS(bent(ibent)))

        !! total number of end points in boundary curve
        tNodes = SIZE( CurveEntity%BoundingEntity )

        !! write boundary points
        DO kk = 1, tNodes
          jj = ABS( CurveEntity%BoundingEntity( kk ) )
          IF( .NOT. NodePresent( jj ) ) THEN
              x( 1:nsd ) = Nodes( 1:nsd, jj )
              ierr = gmsh%model%geo%addPoint( x(1), x(2), x(3), &
                & -1.0_DFP, jj )
              NodePresent( jj ) = .TRUE.
          END IF
        END DO

        elemNum => CurveEntity%elemNumber !! element numbers
        NodeNum => CurveEntity%NodeNumber !! node numbers
        Nptrs => CurveEntity%Nptrs !! connectivity

        ! Write points on boundary curve
        tNodes = SIZE( NodeNum )
        DO kk = 1, tNodes
          jj = NodeNum( kk )
          IF( .NOT. NodePresent( jj ) ) THEN
            x( 1:nsd ) = Nodes( 1:nsd, jj )
            ierr = gmsh % model % geo % addPoint( x(1), x(2), x(3), &
              & -1.0_DFP, jj )
            NodePresent( jj ) = .TRUE.
          END IF
        END DO

        !! append element numbers to line loop
        IF( bent(ibent) .LT. 0 ) THEN
          CALL Append(ll, -elemNum)
        ELSE
          CALL Append(ll, elemNum)
        END IF

        ! adding line elements
        DO iel = 1, SIZE( elemNum )
          jj = elemNum( iel )
          IF( .NOT. LinePresent( jj ) ) THEN
            ierr = gmsh%model%geo%addLine( Nptrs(1,iel), Nptrs(2,iel), jj )
            ! countLine = countLine + 1
            LinePresent( jj ) = .TRUE.
          END IF
        END DO

      END DO

      loop(1) = gmsh % model % geo % addCurveLoop( ll, -1 )
      ierr = gmsh % model % geo % addPlaneSurface( loop, Ent(ient) )
      IF( ALLOCATED( ll ) ) DEALLOCATE( ll )
    END DO

    ierr = gmsh%model%addPhysicalGroup(dim=nsd, tags=Ent, uid=ii )
    ierr = gmsh%model%setPhysicalName(dim=nsd,tag=ii,&
      & name=omega_name(ii)%chars())
  END DO

  !--------------------------------------------------------------------------
  !! Now we add the physical boundaries
  !--------------------------------------------------------------------------

  tOmega = obj%PhysicalNames%TotalPhysicalCurves()
  boundary_name = obj%PhysicalNames%PhysicalCurveNames()

  DO ii = 1, tOmega

    ! get the index of physical curve
    tag = obj%PhysicalNames%IndexOfPhysicalCurve( ii )

    ! get the curve entities associated with physical curve
    Ent = obj%PhysicalNames%Entities(tag)%Val

    ! loop over curve entities
    DO ient = 1, SIZE( Ent )
      CurveEntity => Obj%CurveEntities( Ent( ient ) )

      ! first write boundary nodes
      tNodes = SIZE( CurveEntity % BoundingEntity )
      DO kk = 1, tNodes
        jj = ABS( CurveEntity % BoundingEntity( kk ) )
        IF( .NOT. NodePresent( jj ) ) THEN
            x( 1:nsd ) = Nodes( 1:nsd, jj )
            ierr = gmsh % model % geo % addPoint( x(1), x(2), x(3), &
              & -1.0_DFP, jj )
            NodePresent( jj ) = .TRUE.
        END IF
      END DO

      elemNum => CurveEntity%elemNumber ! elements numbers in curve ent
      NodeNum => CurveEntity%NodeNumber ! node numbers in curve ent
      Nptrs => CurveEntity%Nptrs ! connectivity in curve entities

      tNodes = SIZE( NodeNum )
      DO kk = 1, tNodes
        jj = NodeNum( kk )
        IF( .NOT. NodePresent( jj ) ) THEN
          x( 1:nsd ) = Nodes( 1:nsd, jj )
          ierr = gmsh%model%geo%addPoint( x(1), x(2), x(3), &
            & -1.0_DFP, jj )
          NodePresent( jj ) = .TRUE.
        END IF
      END DO

      telem = SIZE( elemNum )
      !! bent is dummy int vec
      ! CALL Reallocate( bent, telem )
      ! bent = countLine
      ! adding line elements
      DO iel = 1, telem
        jj = elemNum(iel)
        IF( .NOT. LinePresent( jj ) ) THEN
        ! bent( iel ) = bent(iel) + iel
        ! countLine = countLine + 1
        ierr = gmsh%model%geo%addLine( Nptrs(1,iel), Nptrs(2,iel), &
          & elemNum(iel) )
        LinePresent( jj ) = .TRUE.
        END IF
      END DO

      CALL Append(ll, elemNum)

    END DO

    ierr = gmsh%model%addPhysicalGroup(dim=nsd-1, tags=ll, uid=ii )
    ierr = gmsh%model%setPhysicalName(dim=nsd-1,tag=ii,&
        & name=boundary_name(ii)%chars())
    IF( ALLOCATED( ll ) ) DEALLOCATE( ll )

  END DO

  !! Generate the mesh
  ierr = gmsh%model%mesh%generate(dim=nsd)

  NULLIFY( SurfaceEntity, CurveEntity, elemNum, NodeNum, Nptrs )
  IF( ALLOCATED( ll ) ) DEALLOCATE( ll )
  IF( ALLOCATED( Ent ) ) DEALLOCATE( Ent )
  IF( ALLOCATED( bent ) ) DEALLOCATE( bent )
  DEALLOCATE( NodePresent )

END SUBROUTINE remesh_domain_using_gmsh_2D

END SUBMODULE MeshGeneration