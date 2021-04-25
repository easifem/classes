SUBMODULE( gmshMesh_Class ) ElementsMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS


!----------------------------------------------------------------------------
!                                                              TotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_telements_1
  ans = obj % Elements % numElements
END PROCEDURE gmsh_mesh_telements_1

!----------------------------------------------------------------------------
!                                                              TotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_telements_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  ans = 0
  SELECT CASE( XiDim )
  CASE( 1 )
    IF( ALLOCATED( obj % CurveEntities ) &
      & .AND. SIZE( obj % CurveEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( obj % CurveEntities )
        ans = ans + obj % CurveEntities( i ) % TotalElements( )
      END DO
    END IF
  CASE( 2 )
    IF( ALLOCATED( obj % SurfaceEntities ) &
      & .AND. SIZE( obj % SurfaceEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( obj % SurfaceEntities )
        ans = ans + obj % SurfaceEntities( i ) % TotalElements( )
      END DO
    END IF
  CASE( 3 )
    IF( ALLOCATED( obj % VolumeEntities ) &
      & .AND. SIZE( obj % VolumeEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( obj % VolumeEntities )
        ans = ans + obj % VolumeEntities( i ) % TotalElements( )
      END DO
    END IF
  END SELECT
END PROCEDURE gmsh_mesh_telements_2

!----------------------------------------------------------------------------
!                                                               TotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_telements_3
  ! Define internal variables
  INTEGER( I4B ), ALLOCATABLE :: Indices( : ), Entities( : )
  INTEGER( I4B ) :: tSize, i, j, k
  !
  ans = 0_I4B
  SELECT CASE( XiDim )
  CASE( 0 )
    !
    Indices = obj % PhysicalNames % IndexOfPhysicalPoint( Tag )
    ! Loop over indices
    IF( ALLOCATED( Indices ) .AND. SIZE( Indices ) .NE. 0 ) THEN
      DO i = 1, SIZE( Indices )
        j = Indices( i )
        tSize = SIZE( obj % PhysicalNames % Entities( j ) )
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        DO k = 1, tSize
          j = Entities( k )
          ans = ans + obj % PointEntities( j ) % TotalElements( )
        END DO
      END DO
    END IF
  CASE( 1 )
    !
    Indices = obj % PhysicalNames % IndexOfPhysicalCurve( Tag )
    ! Loop over indices
    IF( ALLOCATED( Indices ) .AND. SIZE( Indices ) .NE. 0 ) THEN
      DO i = 1, SIZE( Indices )
        j = Indices( i )
        tSize = SIZE( obj % PhysicalNames % Entities( j ) )
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        DO k = 1, tSize
          j = Entities( k )
          ans = ans + obj % CurveEntities( j ) % TotalElements( )
        END DO
      END DO
    END IF
  CASE( 2 )
    !
    Indices = obj % PhysicalNames % IndexOfPhysicalSurface( Tag )
    ! Loop over indices
    IF( ALLOCATED( Indices ) .AND. SIZE( Indices ) .NE. 0 ) THEN
      DO i = 1, SIZE( Indices )
        j = Indices( i )
        ! Entities related to the physical tag
        tSize = SIZE( obj % PhysicalNames % Entities( j ) )
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        DO k = 1, tSize
          j = Entities( k )
          ans = ans + obj % SurfaceEntities( j ) % TotalElements( )
        END DO
      END DO
    END IF
  CASE( 3 )
    !
    Indices = obj % PhysicalNames % IndexOfPhysicalVolume( Tag )
    ! Loop over indices
    IF( ALLOCATED( Indices ) .AND. SIZE( Indices ) .NE. 0 ) THEN
      DO i = 1, SIZE( Indices )
        j = Indices( i )
        tSize = SIZE( obj % PhysicalNames % Entities( j ) )
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        DO k = 1, tSize
          j = Entities( k )
          ans = ans + obj % VolumeEntities( j ) % TotalElements( )
        END DO
      END DO
    END IF
  END SELECT
END PROCEDURE gmsh_mesh_telements_3

!----------------------------------------------------------------------------
!                                                                getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_1
  ! Define internal variable
  INTEGER( I4B ) :: i, tElements, ElemType, EntityTag, iel, Order
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  CLASS( ReferenceElement_ ), POINTER :: refelem_1, refelem_2
  CLASS( Element_ ), POINTER :: Elem

  refelem_1 => NULL( ); refelem_2 => NULL( ); Elem => NULL( )
  tElements = obj % TotalElements( )
  !
  ! Initiate the mesh obj
  CALL Meshobj % Initiate( NSD = obj % NSD, tElements = tElements )
  !
  ! Curved Entities
  IF( ALLOCATED( obj % CurveEntities ) ) THEN
    !
    IF( SIZE( obj % CurveEntities ) .NE. 0 ) THEN
      refelem_1 => ReferenceLine_Pointer( NSD = obj % NSD )
      refelem_2 => refelem_1
      DO i = 1, SIZE( obj % CurveEntities )
        ElemType = obj % CurveEntities( i ) % ElemType
        EntityTag = obj % CurveEntities( i )  % Uid
        tElements = obj % CurveEntities( i ) % TotalElements( )
        ! get the order of element
        ! if element is not linear then make lagrange element
        Order = ElementOrder( ElemType )
        IF( Order .NE. 1 ) THEN
          NULLIFY( refelem_2 )
          refelem_2 => refelem_1 % LagrangeElement( Order = Order )
          DEALLOCATE( refelem_1 )
        END IF
        ! now loop over all elements
        DO iel = 1, tElements
          Nptrs = obj % CurveEntities( i ) % Nptrs( :, iel )
          Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
          CALL Meshobj % Append( Elem )
        END DO
      END DO
    END IF
  END IF
  ! Surface Entities
  IF( ALLOCATED( obj % SurfaceEntities ) ) THEN
    !
    IF( SIZE( obj % SurfaceEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( obj % SurfaceEntities )
        ElemType = obj % SurfaceEntities( i ) % ElemType
        EntityTag = obj % SurfaceEntities( i )  % Uid
        tElements = obj % SurfaceEntities( i ) % TotalElements( )
        Order = ElementOrder( ElemType )
        !
        IF( isTriangle( ElemType ) ) THEN
          refelem_1 => ReferenceTriangle_Pointer( NSD = obj % NSD )
          IF( Order .NE. 1 ) THEN
            refelem_2 => refelem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( refelem_1 )
          ELSE
            refelem_2 => refelem_1
          END IF
        ELSE IF( isQuadrangle( ElemType ) ) THEN
          refelem_1 => ReferenceQuadrangle_Pointer( NSD = obj % NSD )
          IF( Order .NE. 1 ) THEN
            refelem_2 => refelem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( refelem_1 )
          ELSE
            refelem_2 => refelem_1
          END IF
        END IF
        !
        ! now loop over all elements
        DO iel = 1, tElements
          Nptrs = obj % SurfaceEntities( i ) % Nptrs( :, iel )
          Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
          CALL Meshobj % Append( Elem = Elem )
        END DO
      END DO
    END IF
  END IF
  ! Volume Entities
  IF( ALLOCATED( obj % VolumeEntities ) ) THEN
    IF( SIZE( obj % VolumeEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( obj % VolumeEntities )
        ElemType = obj % VolumeEntities( i ) % ElemType
        EntityTag = obj % VolumeEntities( i )  % Uid
        tElements = obj % VolumeEntities( i ) % TotalElements( )
        Order = ElementOrder( ElemType )
        !
        IF( isTetrahedron( ElemType ) ) THEN
          refelem_1 => ReferenceTetrahedron_Pointer( NSD = obj % NSD )
          IF( Order .NE. 1 ) THEN
            refelem_2 => refelem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( refelem_1 )
          ELSE
            refelem_2 => refelem_1
          END IF
        ELSE IF( isHexahedron( ElemType ) ) THEN
          refelem_1 => ReferenceHexahedron_Pointer( NSD = obj % NSD )
          IF( Order .NE. 1 ) THEN
            refelem_2 => refelem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( refelem_1 )
          ELSE
            refelem_2 => refelem_1
          END IF
        ELSE IF( isPrism( ElemType ) ) THEN
          refelem_1 => ReferencePrism_Pointer( NSD = obj % NSD )
          IF( Order .NE. 1 ) THEN
            refelem_2 => refelem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( refelem_1 )
          ELSE
            refelem_2 => refelem_1
          END IF
        ELSE IF( isPyramid( ElemType ) ) THEN
          refelem_1 => ReferencePyramid_Pointer( NSD = obj % NSD )
          IF( Order .NE. 1 ) THEN
            refelem_2 => refelem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( refelem_1 )
          ELSE
            refelem_2 => refelem_1
          END IF
        END IF

        ! now loop over all elements
        DO iel = 1, tElements
          Nptrs = obj % VolumeEntities( i ) % Nptrs( :, iel )
          Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
          CALL Meshobj % Append( Elem )
        END DO
      END DO
    END IF
  END IF

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  NULLIFY( Elem, refelem_1, refelem_2 )

END PROCEDURE gmsh_mesh_getelements_1

!----------------------------------------------------------------------------
!                                                                 getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_2
  ! Define internal variable
  INTEGER( I4B ) :: i, j, tElements, tag, ElemType, EntityTag, iel, Order
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : ), Indices( : )
  CLASS( ReferenceElement_ ), POINTER :: refelem_1, refelem_2
  CLASS( Element_ ), POINTER :: Elem
  !
  refelem_1 => NULL( ); refelem_2 => NULL( ); Elem => NULL( )
  iel = 0
  tElements = obj % TotalElements( XiDim )
  !
  IF( PRESENT( Offset ) ) THEN
    tag = 2
  ELSE
    ! Initiate the mesh obj
    tag = 1
    CALL Meshobj % Initiate( NSD = obj % NSD, tElements = tElements )
  END IF
  !
  SELECT CASE ( XiDim )
  CASE( 1 )
    IF( ALLOCATED( obj % CurveEntities ) ) THEN
      IF( SIZE( obj % CurveEntities ) .NE. 0 ) THEN
      refelem_1 => ReferenceLine_Pointer( NSD = obj % NSD )
      refelem_2 => refelem_1
        DO i = 1, SIZE( obj % CurveEntities )
          ElemType = obj % CurveEntities( i ) % ElemType
          EntityTag = obj % CurveEntities( i )  % Uid
          tElements = obj % CurveEntities( i ) % TotalElements( )
          ! get the order of element
          ! if element is not linear then make lagrange element
          Order = ElementOrder( ElemType )
          IF( Order .NE. 1 ) THEN
            !SELECT TYPE( refelem_1 )
              !TYPE IS ( ReferenceLine_ )
                refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                DEALLOCATE( refelem_1 )
            !END SELECT
          END IF
          ! now loop over all elements
          SELECT CASE( tag )
          CASE( 1 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = obj % CurveEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % Append( Elem )
            END DO
          CASE( 2 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = obj % CurveEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END IF
    END IF
  CASE( 2 )
    ! Surface Entities
    IF( ALLOCATED( obj % SurfaceEntities ) ) THEN
      IF( SIZE( obj % SurfaceEntities ) .NE. 0 ) THEN
        DO i = 1, SIZE( obj % SurfaceEntities )
          ElemType = obj % SurfaceEntities( i ) % ElemType
          EntityTag = obj % SurfaceEntities( i )  % Uid
          tElements = obj % SurfaceEntities( i ) % TotalElements( )
          Order = ElementOrder( ElemType )
          !
          IF( isTriangle( ElemType ) ) THEN
            refelem_1 => ReferenceTriangle_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( refelem_1 )
                !TYPE IS ( ReferenceTriangle_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              !END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          ELSE IF( isQuadrangle( ElemType ) ) THEN
            refelem_1 => ReferenceQuadrangle_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( refelem_1 )
                !TYPE IS ( ReferenceQuadrangle_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              !END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          END IF
          ! now loop over all elements
          SELECT CASE( tag )
          CASE( 1 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = obj % SurfaceEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % Append( Elem = Elem )
            END DO
          CASE( 2 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = obj % SurfaceEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END IF
    END IF
  CASE( 3 )
    ! Volume Entities
    IF( ALLOCATED( obj % VolumeEntities ) ) THEN
      IF( SIZE( obj % VolumeEntities ) .NE. 0 ) THEN
        DO i = 1, SIZE( obj % VolumeEntities )
          ElemType = obj % VolumeEntities( i ) % ElemType
          EntityTag = obj % VolumeEntities( i )  % Uid
          tElements = obj % VolumeEntities( i ) % TotalElements( )
          Order = ElementOrder( ElemType )
          !
          IF( isTetrahedron( ElemType ) ) THEN
            refelem_1 => ReferenceTetrahedron_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( refelem_1 )
              !  TYPE IS ( ReferenceTetrahedron_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              !END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          ELSE IF( isHexahedron( ElemType ) ) THEN
            refelem_1 => ReferenceHexahedron_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( refelem_1 )
              !  TYPE IS ( ReferenceHexahedron_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              !END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          ELSE IF( isPrism( ElemType ) ) THEN
            refelem_1 => ReferencePrism_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( refelem_1 )
              !  TYPE IS ( ReferencePrism_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              !END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          ELSE IF( isPyramid( ElemType ) ) THEN
            refelem_1 => ReferencePyramid_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( refelem_1 )
              !  TYPE IS ( ReferencePyramid_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
              !    DEALLOCATE( refelem_1 )
              !END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          END IF
          ! now loop over all elements
          SELECT CASE( tag )
          CASE( 1 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = obj % VolumeEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % Append( Elem )
            END DO
          CASE( 2 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = obj % VolumeEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END IF
    END IF
  END SELECT

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  IF( ALLOCATED( Indices ) ) DEALLOCATE( Indices )
  NULLIFY( Elem, refelem_1, refelem_2 )

END PROCEDURE gmsh_mesh_getelements_2

!----------------------------------------------------------------------------
!                                                                getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_2c

  IF( XiDim .EQ. obj % nsd ) THEN

    IF( .NOT. ALLOCATED( Dom % Omega ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_2c()" )
      CALL Display( "            Dom % Omega not allocated" )
      STOP
    END IF

    !> check the validity index
    IF( Indx .GT. SIZE( Dom % Omega ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_2c()" )
      CALL Display( "            Out of bound error" )
      STOP
    END IF

    !> Report error if already associated
    IF( ASSOCIATED( Dom % Omega( Indx ) % Ptr ) ) THEN

      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_2c()" )
      CALL Display( "            Dom % Omega( Indx ) % Ptr already &
        & associated to other mesh" )
      CALL Display( "            Nullify first before calling" )
      STOP
    END IF

    !>  get elements
    ALLOCATE( Dom % Omega( Indx ) % Ptr )

    IF( PRESENT( Offset ) ) THEN
      CALL obj % getElements( &
      & Meshobj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & FEobj = FEobj, &
      & Offset = Offset )
    ELSE
      CALL obj % getElements( &
      & Meshobj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & FEobj = FEobj )
    END IF

  END IF

  IF( XiDim .EQ. obj % nsd - 1 ) THEN

    IF( .NOT. ALLOCATED( Dom % Boundary ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_2c()" )
      CALL Display( "            Dom % Boundary not allocated" )
      STOP
    END IF

    !> check the validity index
    IF( Indx .GT. SIZE( Dom % Boundary ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_2c()" )
      CALL Display( "            Out of bound error" )
      STOP
    END IF

    !> Report error if already associated
    IF( ASSOCIATED( Dom % Boundary( Indx ) % Ptr ) ) THEN

      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_2c()" )
      CALL Display( "            Dom % Boundary( Indx ) % Ptr already &
        & associated to other mesh" )
      CALL Display( "            Nullify first before calling" )
      STOP
    END IF

    !>  get elements
    ALLOCATE( Dom % Boundary( Indx ) % Ptr )

    IF( PRESENT( Offset ) ) THEN
      CALL obj % getElements( &
        & Meshobj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & FEobj = FEobj, &
        & Offset = Offset )
    ELSE
      CALL obj % getElements( &
        & Meshobj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & FEobj = FEobj )
    END IF
  END IF

  IF( XiDim .LE. obj % nsd - 2 ) THEN
    CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
    CALL Display( "         gmsh_mesh_getelements_2c()" )
    CALL Display( "            currently does not support xidim = nsd-2" )
    STOP
  END IF

END PROCEDURE gmsh_mesh_getelements_2c

!----------------------------------------------------------------------------
!                                                                getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_3
  ! Define internal variable
  INTEGER( I4B ) :: i, j, k, l, tSize, tElements, AlgoTag, ElemType, &
    & EntityTag, iel, Order
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : ), Indices( : ), Entities( : )
  CLASS( ReferenceElement_ ), POINTER :: refelem_1, refelem_2
  CLASS( Element_ ), POINTER :: Elem
  !
  refelem_1 => NULL( ); refelem_2 => NULL( ); Elem => NULL( )
  iel = 0
  tElements = obj % TotalElements( XiDim, Tag )
  IF( PRESENT( Offset ) ) THEN
    AlgoTag = 2
  ELSE
    ! Initiate the mesh obj
    AlgoTag = 1
    CALL Meshobj % Initiate( NSD = obj % NSD, tElements = tElements )
  END IF
  !
  SELECT CASE( XiDim )
  CASE( 1 )
    IF( ALLOCATED( obj % CurveEntities ) &
      & .AND. SIZE( obj % CurveEntities ) .NE. 0 ) THEN
      !
      ! get the indices
      Indices = obj % PhysicalNames % IndexOfPhysicalCurve( Tag )
      !
      DO i = 1, SIZE( Indices )
        IF( Indices( i ) .EQ. 0 ) CYCLE
        j = Indices( i )
        tSize = SIZE( obj % PhysicalNames % Entities( j ) )
        !
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        !
        refelem_1 => ReferenceLine_Pointer( NSD = obj % NSD )
        refelem_2 => refelem_1
        !
        DO k = 1, tSize
          j = Entities( k )
          ElemType = obj % CurveEntities( j ) % ElemType
          EntityTag = obj % CurveEntities( j ) % UiD
          tElements = obj % CurveEntities( j ) % TotalElements( )
          ! get the order of element
          ! if element is not linear then make lagrange element
          Order = ElementOrder( ElemType )
          IF( Order .NE. 1 ) THEN
            ! SELECT TYPE( refelem_1 )
              ! TYPE IS ( ReferenceLine_ )
                NULLIFY( refelem_2 )
                refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                DEALLOCATE( refelem_1 )
            ! END SELECT
          END IF
          !
          SELECT CASE( AlgoTag )
          CASE( 1 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = obj % CurveEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % Append( Elem )
            END DO
          CASE( 2 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = obj % CurveEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % SetElement( Elem = Elem, iel = offset + iel )
            END DO
          END SELECT
        END DO
      END DO
    END IF
  CASE( 2 )
    IF( ALLOCATED( obj % SurfaceEntities ) &
      & .AND. SIZE( obj % SurfaceEntities ) .NE. 0 ) THEN
      !
      refelem_1 => ReferenceLine_Pointer( NSD = obj % NSD )
      refelem_2 => refelem_1
      ! get the indices
      Indices = obj % PhysicalNames % IndexOfPhysicalSurface( Tag )
      !
      DO i = 1, SIZE( Indices )
        IF( Indices( i ) .EQ. 0 ) CYCLE
        j = Indices( i )
        tSize = SIZE( obj % PhysicalNames % Entities( j ) )
        !
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        !
        DO k = 1, tSize
          j = Entities( k )
          ElemType = obj % SurfaceEntities( j ) % ElemType
          EntityTag = obj % SurfaceEntities( j ) % UiD
          tElements = obj % SurfaceEntities( j ) % TotalElements( )
          ! get the order of element
          Order = ElementOrder( ElemType )
          ! if element is not linear then make lagrange element
          IF( isTriangle( ElemType ) ) THEN
            refelem_1 => ReferenceTriangle_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( refelem_1 )
                ! TYPE IS ( ReferenceTriangle_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              ! END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          ELSE IF( isQuadrangle( ElemType ) ) THEN
            refelem_1 => ReferenceQuadrangle_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( refelem_1 )
                ! TYPE IS ( ReferenceQuadrangle_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              ! END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          END IF
          !
          SELECT CASE( AlgoTag )
          CASE( 1 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = obj % SurfaceEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % Append( Elem )
            END DO
          CASE( 2 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = obj % SurfaceEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END DO
    END IF
  CASE( 3 )
    IF( ALLOCATED( obj % VolumeEntities ) &
      & .AND. SIZE( obj % VolumeEntities ) .NE. 0 ) THEN
      !
      refelem_1 => ReferenceLine_Pointer( NSD = obj % NSD )
      refelem_2 => refelem_1
      ! get the indices
      Indices = obj % PhysicalNames % IndexOfPhysicalVolume( Tag )
      !
      DO i = 1, SIZE( Indices )
        IF( Indices( i ) .EQ. 0 ) CYCLE
        j = Indices( i )
        tSize = SIZE( obj % PhysicalNames % Entities( j ) )
        !
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        !
        DO k = 1, tSize
          j = Entities( k )
          ElemType = obj % VolumeEntities( j ) % ElemType
          EntityTag = obj % VolumeEntities( j ) % UiD
          tElements = obj % VolumeEntities( j ) % TotalElements( )
          ! get the order of element
          Order = ElementOrder( ElemType )
          ! if element is not linear then make lagrange element
          !
          IF( isTetrahedron( ElemType ) ) THEN
            refelem_1 => ReferenceTetrahedron_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( refelem_1 )
                ! TYPE IS ( ReferenceTetrahedron_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              ! END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          ELSE IF( isHexahedron( ElemType ) ) THEN
            refelem_1 => ReferenceHexahedron_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( refelem_1 )
                ! TYPE IS ( ReferenceHexahedron_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              ! END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          ELSE IF( isPrism( ElemType ) ) THEN
            refelem_1 => ReferencePrism_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( refelem_1 )
                ! TYPE IS ( ReferencePrism_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              ! END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          ELSE IF( isPyramid( ElemType ) ) THEN
            refelem_1 => ReferencePyramid_Pointer( NSD = obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( refelem_1 )
                ! TYPE IS ( ReferencePyramid_ )
                  refelem_2 => refelem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( refelem_1 )
              ! END SELECT
            ELSE
              refelem_2 => refelem_1
            END IF
          END IF
          !
          SELECT CASE( AlgoTag )
          CASE( 1 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = obj % VolumeEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % Append( Elem )
            END DO
          CASE( 2 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = obj % VolumeEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEobj, Nptrs, EntityTag, refelem_2 )
              CALL Meshobj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END DO
    END IF
  END SELECT

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  IF( ALLOCATED( Indices ) ) DEALLOCATE( Indices )
  IF( ALLOCATED( Entities ) ) DEALLOCATE( Entities )
  NULLIFY( refelem_2, refelem_1, Elem )

END PROCEDURE gmsh_mesh_getelements_3

!----------------------------------------------------------------------------
!                                                                getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_3c

  IF( XiDim .EQ. obj % nsd ) THEN

    IF( .NOT. ALLOCATED( Dom % Omega ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_3c()" )
      CALL Display( "            Dom % Omega not allocated" )
      STOP
    END IF

    !> check the validity index
    IF( Indx .GT. SIZE( Dom % Omega ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_3c()" )
      CALL Display( "            Out of bound error" )
      STOP
    END IF

    !> Report error if already associated
    IF( ASSOCIATED( Dom % Omega( Indx ) % Ptr ) ) THEN

      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_3c()" )
      CALL Display( "            Dom % Omega( Indx ) % Ptr already &
        & associated to other mesh" )
      CALL Display( "            Nullify first before calling" )
      STOP
    END IF

    !>  get elements
    ALLOCATE( Dom % Omega( Indx ) % Ptr )

    IF( PRESENT( Offset ) ) THEN
      CALL obj % getElements( &
      & Meshobj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & Tag = Tag, &
      & FEobj = FEobj, &
      & Offset = Offset )
    ELSE
      CALL obj % getElements( &
      & Meshobj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & Tag = Tag, &
      & FEobj = FEobj )
    END IF

  END IF


  IF( XiDim .EQ. obj % nsd - 1 ) THEN

    IF( .NOT. ALLOCATED( Dom % Boundary ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_3c()" )
      CALL Display( "            Dom % Boundary not allocated" )
      STOP
    END IF

    !> check the validity index
    IF( Indx .GT. SIZE( Dom % Boundary ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_3c()" )
      CALL Display( "            Out of bound error" )
      STOP
    END IF

    !> Report error if already associated
    IF( ASSOCIATED( Dom % Boundary( Indx ) % Ptr ) ) THEN

      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_3c()" )
      CALL Display( "            Dom % Boundary( Indx ) % Ptr already &
        & associated to other mesh" )
      CALL Display( "            Nullify first before calling" )
      STOP
    END IF

    !>  get elements
    ALLOCATE( Dom % Boundary( Indx ) % Ptr )

    IF( PRESENT( Offset ) ) THEN
      CALL obj % getElements( &
        & Meshobj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & Tag = Tag, &
        & FEobj = FEobj, &
        & Offset = Offset )
    ELSE
      CALL obj % getElements( &
        & Meshobj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & Tag = Tag, &
        & FEobj = FEobj )
    END IF
  END IF

  IF( XiDim .LE. obj % nsd - 2 ) THEN
    CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
    CALL Display( "         gmsh_mesh_getelements_3c()" )
    CALL Display( "            currently does not support xidim = nsd-2" )
    STOP
  END IF

END PROCEDURE gmsh_mesh_getelements_3c

!----------------------------------------------------------------------------
!                                                                GetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_4
  ! Define internal variables
  INTEGER( I4B ), ALLOCATABLE :: Tag( : ), Indices( : )
  !
  Indices = obj % PhysicalNames % getIndex( TagNames )
  Tag = obj % PhysicalNames % Tag( Indices )
  !
  IF( PRESENT( Offset ) ) THEN
    CALL obj % getElements( Meshobj = Meshobj, XiDim = XiDim, &
      & FEobj = FEobj, Offset = Offset, Tag = Tag )
  ELSE
    CALL obj % getElements( Meshobj = Meshobj, XiDim = XiDim, &
      & FEobj = FEobj, Tag = Tag )
  END IF
  IF( ALLOCATED( Tag ) ) DEALLOCATE( Tag )
  IF( ALLOCATED( Indices ) ) DEALLOCATE( Indices )
END PROCEDURE gmsh_mesh_getelements_4

!----------------------------------------------------------------------------
!                                                                getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_4c

  IF( XiDim .EQ. obj % nsd ) THEN

    IF( .NOT. ALLOCATED( Dom % Omega ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_4c()" )
      CALL Display( "            Dom % Omega not allocated" )
      STOP
    END IF

    !> check the validity index
    IF( Indx .GT. SIZE( Dom % Omega ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_4c()" )
      CALL Display( "            Out of bound error" )
      STOP
    END IF

    !> Report error if already associated
    IF( ASSOCIATED( Dom % Omega( Indx ) % Ptr ) ) THEN

      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_4c()" )
      CALL Display( "            Dom % Omega( Indx ) % Ptr already &
        & associated to other mesh" )
      CALL Display( "            Nullify first before calling" )
      STOP
    END IF

    !>  get elements
    ALLOCATE( Dom % Omega( Indx ) % Ptr )

    IF( PRESENT( Offset ) ) THEN
      CALL obj % getElements( &
      & Meshobj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & TagNames = TagNames, &
      & FEobj = FEobj, &
      & Offset = Offset )
    ELSE
      CALL obj % getElements( &
      & Meshobj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & TagNames = TagNames, &
      & FEobj = FEobj )
    END IF

  END IF


  IF( XiDim .EQ. obj % nsd - 1 ) THEN

    IF( .NOT. ALLOCATED( Dom % Boundary ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_4c()" )
      CALL Display( "            Dom % Boundary not allocated" )
      STOP
    END IF

    !> check the validity index
    IF( Indx .GT. SIZE( Dom % Boundary ) ) THEN
      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_4c()" )
      CALL Display( "            Out of bound error" )
      STOP
    END IF

    !> Report error if already associated
    IF( ASSOCIATED( Dom % Boundary( Indx ) % Ptr ) ) THEN

      CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
      CALL Display( "         gmsh_mesh_getelements_4c()" )
      CALL Display( "            Dom % Boundary( Indx ) % Ptr already &
        & associated to other mesh" )
      CALL Display( "            Nullify first before calling" )
      STOP
    END IF

    !>  get elements
    ALLOCATE( Dom % Boundary( Indx ) % Ptr )

    IF( PRESENT( Offset ) ) THEN
      CALL obj % getElements( &
        & Meshobj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & TagNames = TagNames, &
        & FEobj = FEobj, &
        & Offset = Offset )
    ELSE
      CALL obj % getElements( &
        & Meshobj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & TagNames = TagNames, &
        & FEobj = FEobj )
    END IF
  END IF

  IF( XiDim .LE. obj % nsd - 2 ) THEN
    CALL Display( "ERROR :: gmsh_mesh_Class@Methods.f90" )
    CALL Display( "         gmsh_mesh_getelements_4c()" )
    CALL Display( "            currently does not support xidim = nsd-2" )
    STOP
  END IF

END PROCEDURE gmsh_mesh_getelements_4c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE dom_init_from_gmshMesh
  ! Reallocate Omega
  INTEGER( I4B ) :: tOmega
  INTEGER( I4B ) :: tBoundary
  INTEGER( I4B ) :: tEdge
  INTEGER( I4B ) :: nsd
  INTEGER( I4B ) :: ii
  INTEGER( I4B ) :: jj
  INTEGER( I4B ) :: iel
  INTEGER( I4B ) :: id
  LOGICAL( LGT ) :: isFacet
  TYPE( String ) :: cellName
  TYPE( String ), ALLOCATABLE :: omega_name( : )
  TYPE( String ), ALLOCATABLE :: boundary_name( : )
  TYPE( String ), ALLOCATABLE :: edge_name( : )
  REAL( DFP ), ALLOCATABLE :: nodes( :, : )

  tOmega = 0; tBoundary = 0; tEdge = 0
  nsd = mshobj % nsd

  SELECT CASE( mshobj % nsd )

  CASE( 3 )

    tOmega = mshobj % PhysicalNames % TotalPhysicalVolumes( )
    tBoundary = mshobj % PhysicalNames % TotalPhysicalSurfaces( )
    tEdge = mshobj % PhysicalNames % TotalPhysicalCurves( )

    omega_name = mshobj % PhysicalNames % PhysicalVolumeNames( )
    boundary_name = mshobj % PhysicalNames % PhysicalSurfaceNames( )
    edge_name = mshobj % PhysicalNames % PhysicalCurveNames( )

  CASE( 2 )
    tOmega = mshobj % PhysicalNames % TotalPhysicalSurfaces( )
    tBoundary = mshobj % PhysicalNames % TotalPhysicalCurves( )
    tEdge = mshobj % PhysicalNames % TotalPhysicalPoints( )

    omega_name = mshobj % PhysicalNames % PhysicalSurfaceNames( )
    boundary_name = mshobj % PhysicalNames % PhysicalCurveNames( )
    edge_name = mshobj % PhysicalNames % PhysicalPointNames( )

  CASE( 1 )
    tOmega = mshobj % PhysicalNames % TotalPhysicalCurves( )
    tBoundary = mshobj % PhysicalNames % TotalPhysicalPoints( )

    omega_name = mshobj % PhysicalNames % PhysicalCurveNames( )
    boundary_name = mshobj % PhysicalNames % PhysicalPointNames( )
  END SELECT

  IF( tOmega .NE. 0 ) THEN
    ALLOCATE( obj % omega_name( tOmega ) )
    DO ii = 1, tOmega
      obj % omega_name( ii ) = omega_name( ii )
    END DO
  END IF

  IF( tboundary .NE. 0 ) THEN
    ALLOCATE( obj % boundary_name( tboundary ) )
    DO ii = 1, tboundary
      obj % boundary_name( ii ) = boundary_name( ii )
    END DO
  END IF

  IF( tedge .NE. 0 ) THEN
    ALLOCATE( obj % edge_name( tedge ) )
    DO ii = 1, tedge
      obj % edge_name( ii ) = edge_name( ii )
    END DO
  END IF

  ! Allocate memory

    CALL display( "  allocating memory for domain")
    CALL obj % initiate( tOmega = tOmega, tBoundary = tBoundary, &
      & tEdge = tEdge )

    CALL display( tOmega, "  Total Omega :: ")
    CALL display( tBoundary, "  Total Boundary :: ")
    CALL display( tEdge, "  Total Edge :: ")

    ! Construct omegas

    DO ii = 1, tOmega

      CALL display( "  Reading elements in")
      CALL display( "    Omega( " // trim( int2str( ii ) ) // " ) :: " &
        & // trim( omega_name( ii ) ) )

      CALL mshobj % getelements( Dom = obj, Indx = ii, &
        & xiDim = nsd, TagNames = [ omega_name( ii ) ], &
        & FEobj = TypeElement )

      CALL display( ii, "    Setting material properties to :: " )

      CALL obj % omega( ii ) % ptr % setMaterialType( ii )

      IF( .NOT. ASSOCIATED( obj % mdomega( ii ) % ptr ) ) THEN
        ALLOCATE( obj % mdomega( ii ) % ptr )
      END IF

      CALL obj % mdomega( ii ) % ptr % initiate( obj % omega( ii ) % ptr )

    END DO

    ! Construct Boundary

    IF( .NOT. PRESENT( facetmesh ) ) THEN

      DO ii = 1, tBoundary
        CALL display( "  Reading elements in")
        CALL display( "    Boundary( " // trim( int2str( ii ) ) // " ) :: " &
          & // trim( boundary_name( ii ) ) )

        CALL mshobj % getelements( Dom = obj, Indx = ii, &
          & xiDim = nsd-1, TagNames = [ boundary_name( ii ) ], &
          & FEobj = TypeElement )

        CALL display( ii, "    Setting material properties to :: " )

        CALL obj % boundary( ii ) % ptr % setMaterialType( ii )

        IF( .NOT. ASSOCIATED( obj % mdboundary( ii ) % ptr ) ) THEN
          ALLOCATE( obj % mdboundary( ii ) % ptr )
        END IF

        CALL obj % mdboundary( ii ) % ptr % initiate( obj%boundary(ii)%ptr )

      END DO

    END IF

    IF( PRESENT( facetmesh ) ) THEN

      IF( SIZE( facetmesh, 2 ) .NE. 2 ) THEN
        CALL display( "ERROR:: msh4_Class@Methods.f90")
        CALL display( "        dom_init_from_gmshMesh()")
        CALL display( "          size(facetmesh,2) should equal to 2")
        STOP
      END IF

      IF( SIZE( facetmesh, 1 ) .GT. tBoundary ) THEN
        CALL display( "ERROR:: msh4_Class@Methods.f90")
        CALL display( "        dom_init_from_gmshMesh()")
        CALL display( "          facet boundary cannot be more than")
        CALL display( "          total boundaries")
        STOP
      END IF

      DO ii = 1, tBoundary

        CALL display( "  Reading elements in")
        CALL display( "    Boundary( " // trim( int2str( ii ) ) // " ) :: " &
          & // trim( boundary_name( ii ) ) )

        isFacet = .FALSE.

        DO jj = 1, SIZE( facetmesh, 1 )
          IF( boundary_name( ii ) .EQ. facetmesh( jj, 1 ) ) THEN
            cellName = trim( facetmesh( jj, 2 ) )
            isFacet = .TRUE.
            EXIT
          END IF
        END DO

        IF( isFacet ) THEN

          CALL display("    Facetmesh, "//trim(boundary_name( ii ))//" found")
          CALL mshobj % getelements( Dom = obj, Indx = ii, &
            & xiDim = nsd-1, TagNames = [ boundary_name( ii ) ], &
            & FEobj = TypeFacetElement )

          IF( .NOT. ASSOCIATED( obj % mdboundary( ii ) % ptr ) ) THEN
            ALLOCATE( obj % mdboundary( ii ) % ptr )
          END IF
          CALL obj % mdboundary( ii ) % ptr % initiate( obj%boundary(ii)%ptr)

          ! now we search for the cell id which is parent of facet
          id = 0
          DO jj = 1, tOmega

            IF( omega_name( jj ) .EQ. cellName ) THEN
              id = jj
              EXIT
            END IF

          END DO

          IF( id .EQ. 0 ) THEN
            CALL display( "ERROR:: msh4_Class@Methods.f90")
            CALL display( "        dom_init_from_gmshMesh()")
            CALL display( "          facet boundary cannot be more than")
            CALL display( "          no cell found for a facet-mesh")
            STOP
          END IF

          CALL display( "      Connecting to, "//trim(omega_name( jj )) )
          CALL obj%connectFacetToCell( OmegaIndx=jj, BoundaryIndx=ii )

        ELSE

          CALL mshobj % getelements( Dom = obj, Indx = ii, &
            & xiDim = nsd-1, TagNames = [ boundary_name( ii ) ], &
            & FEobj = TypeElement )

          IF( .NOT. ASSOCIATED( obj % mdboundary( ii ) % ptr ) ) THEN
            ALLOCATE( obj % mdboundary( ii ) % ptr )
          END IF
          CALL obj % mdboundary( ii ) % ptr % initiate( obj%boundary(ii)%ptr)

          CALL display( ii, "    Setting material properties" )

          CALL obj % boundary( ii ) % ptr % setMaterialType( ii )

        END IF

      END DO

    END IF

    DO ii = 1, tEdge
      CALL display( "  Reading elements in" )
      CALL display( "    Edge( " // trim( int2str( ii ) ) // " ) :: " &
        & // trim( edge_name( ii ) ) )

      CALL mshobj % getelements( Dom = obj, Indx = ii, &
        & xiDim = nsd-2, TagNames = [ edge_name( ii ) ], &
        & FEobj = TypeElement )

      IF( .NOT. ASSOCIATED( obj % mdedge( ii ) % ptr ) ) THEN
        ALLOCATE( obj % mdedge( ii ) % ptr )
      END IF
      CALL obj % mdedge( ii ) % ptr % initiate( obj%edge(ii)%ptr)

      CALL display( ii, "    Setting material properties" )

      CALL obj % edge( ii ) % ptr % setMaterialType( ii )
    END DO

    IF( ALLOCATED( omega_name ) ) DEALLOCATE( omega_name )
    IF( ALLOCATED( boundary_name ) ) DEALLOCATE( boundary_name )
    IF( ALLOCATED( edge_name ) ) DEALLOCATE( edge_name )

    obj % allocateNodes = .TRUE.
    CALL display('  Reading nodes')
    ALLOCATE( obj % nodes( 1 : 3, 1 : mshobj % totalNodes( ) ) )
    CALL mshobj % getnodes( nodes )
    obj % nodes = nodes
    CALL Reallocate(obj%NodalVelocity, SIZE(nodes,1), SIZE(nodes,2))
    CALL Reallocate(obj%NodalAcceleration, SIZE(nodes,1), SIZE(nodes,2))
    IF( ALLOCATED( nodes ) ) DEALLOCATE( nodes )

END PROCEDURE dom_init_from_gmshMesh

END SUBMODULE ElementsMethods