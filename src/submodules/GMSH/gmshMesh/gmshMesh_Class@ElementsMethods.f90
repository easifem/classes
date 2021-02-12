SUBMODULE( gmshMesh_Class ) ElementsMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS


!----------------------------------------------------------------------------
!                                                              TotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_telements_1
  Ans = Obj % Elements % numElements
END PROCEDURE gmsh_mesh_telements_1

!----------------------------------------------------------------------------
!                                                              TotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_telements_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  Ans = 0
  SELECT CASE( XiDim )
  CASE( 1 )
    IF( ALLOCATED( Obj % CurveEntities ) &
      & .AND. SIZE( Obj % CurveEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( Obj % CurveEntities )
        Ans = Ans + Obj % CurveEntities( i ) % TotalElements( )
      END DO
    END IF
  CASE( 2 )
    IF( ALLOCATED( Obj % SurfaceEntities ) &
      & .AND. SIZE( Obj % SurfaceEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( Obj % SurfaceEntities )
        Ans = Ans + Obj % SurfaceEntities( i ) % TotalElements( )
      END DO
    END IF
  CASE( 3 )
    IF( ALLOCATED( Obj % VolumeEntities ) &
      & .AND. SIZE( Obj % VolumeEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( Obj % VolumeEntities )
        Ans = Ans + Obj % VolumeEntities( i ) % TotalElements( )
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
  Ans = 0_I4B
  SELECT CASE( XiDim )
  CASE( 0 )
    !
    Indices = Obj % PhysicalNames % IndexOfPhysicalPoint( Tag )
    ! Loop over indices
    IF( ALLOCATED( Indices ) .AND. SIZE( Indices ) .NE. 0 ) THEN
      DO i = 1, SIZE( Indices )
        j = Indices( i )
        tSize = SIZE( Obj % PhysicalNames % Entities( j ) )
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( Obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        DO k = 1, tSize
          j = Entities( k )
          Ans = Ans + Obj % PointEntities( j ) % TotalElements( )
        END DO
      END DO
    END IF
  CASE( 1 )
    !
    Indices = Obj % PhysicalNames % IndexOfPhysicalCurve( Tag )
    ! Loop over indices
    IF( ALLOCATED( Indices ) .AND. SIZE( Indices ) .NE. 0 ) THEN
      DO i = 1, SIZE( Indices )
        j = Indices( i )
        tSize = SIZE( Obj % PhysicalNames % Entities( j ) )
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( Obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        DO k = 1, tSize
          j = Entities( k )
          Ans = Ans + Obj % CurveEntities( j ) % TotalElements( )
        END DO
      END DO
    END IF
  CASE( 2 )
    !
    Indices = Obj % PhysicalNames % IndexOfPhysicalSurface( Tag )
    ! Loop over indices
    IF( ALLOCATED( Indices ) .AND. SIZE( Indices ) .NE. 0 ) THEN
      DO i = 1, SIZE( Indices )
        j = Indices( i )
        ! Entities related to the physical tag
        tSize = SIZE( Obj % PhysicalNames % Entities( j ) )
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( Obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        DO k = 1, tSize
          j = Entities( k )
          Ans = Ans + Obj % SurfaceEntities( j ) % TotalElements( )
        END DO
      END DO
    END IF
  CASE( 3 )
    !
    Indices = Obj % PhysicalNames % IndexOfPhysicalVolume( Tag )
    ! Loop over indices
    IF( ALLOCATED( Indices ) .AND. SIZE( Indices ) .NE. 0 ) THEN
      DO i = 1, SIZE( Indices )
        j = Indices( i )
        tSize = SIZE( Obj % PhysicalNames % Entities( j ) )
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( Obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        DO k = 1, tSize
          j = Entities( k )
          Ans = Ans + Obj % VolumeEntities( j ) % TotalElements( )
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
  CLASS( ReferenceElement_ ), POINTER :: RefElem_1, RefElem_2
  CLASS( Element_ ), POINTER :: Elem

  RefElem_1 => NULL( ); RefElem_2 => NULL( ); Elem => NULL( )
  tElements = Obj % TotalElements( )
  !
  ! Initiate the mesh obj
  CALL MeshObj % Initiate( NSD = Obj % NSD, tElements = tElements )
  !
  ! Curved Entities
  IF( ALLOCATED( Obj % CurveEntities ) ) THEN
    !
    IF( SIZE( Obj % CurveEntities ) .NE. 0 ) THEN
      RefElem_1 => ReferenceLine_Pointer( NSD = Obj % NSD )
      RefElem_2 => RefElem_1
      DO i = 1, SIZE( Obj % CurveEntities )
        ElemType = Obj % CurveEntities( i ) % ElemType
        EntityTag = Obj % CurveEntities( i )  % Uid
        tElements = Obj % CurveEntities( i ) % TotalElements( )
        ! get the order of element
        ! if element is not linear then make lagrange element
        Order = ElementOrder( ElemType )
        IF( Order .NE. 1 ) THEN
          NULLIFY( RefElem_2 )
          RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
          DEALLOCATE( RefElem_1 )
        END IF
        ! now loop over all elements
        DO iel = 1, tElements
          Nptrs = Obj % CurveEntities( i ) % Nptrs( :, iel )
          Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
          CALL MeshObj % Append( Elem )
        END DO
      END DO
    END IF
  END IF
  ! Surface Entities
  IF( ALLOCATED( Obj % SurfaceEntities ) ) THEN
    !
    IF( SIZE( Obj % SurfaceEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( Obj % SurfaceEntities )
        ElemType = Obj % SurfaceEntities( i ) % ElemType
        EntityTag = Obj % SurfaceEntities( i )  % Uid
        tElements = Obj % SurfaceEntities( i ) % TotalElements( )
        Order = ElementOrder( ElemType )
        !
        IF( isTriangle( ElemType ) ) THEN
          RefElem_1 => ReferenceTriangle_Pointer( NSD = Obj % NSD )
          IF( Order .NE. 1 ) THEN
            RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( RefElem_1 )
          ELSE
            RefElem_2 => RefElem_1
          END IF
        ELSE IF( isQuadrangle( ElemType ) ) THEN
          RefElem_1 => ReferenceQuadrangle_Pointer( NSD = Obj % NSD )
          IF( Order .NE. 1 ) THEN
            RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( RefElem_1 )
          ELSE
            RefElem_2 => RefElem_1
          END IF
        END IF
        !
        ! now loop over all elements
        DO iel = 1, tElements
          Nptrs = Obj % SurfaceEntities( i ) % Nptrs( :, iel )
          Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
          CALL MeshObj % Append( Elem = Elem )
        END DO
      END DO
    END IF
  END IF
  ! Volume Entities
  IF( ALLOCATED( Obj % VolumeEntities ) ) THEN
    IF( SIZE( Obj % VolumeEntities ) .NE. 0 ) THEN
      DO i = 1, SIZE( Obj % VolumeEntities )
        ElemType = Obj % VolumeEntities( i ) % ElemType
        EntityTag = Obj % VolumeEntities( i )  % Uid
        tElements = Obj % VolumeEntities( i ) % TotalElements( )
        Order = ElementOrder( ElemType )
        !
        IF( isTetrahedron( ElemType ) ) THEN
          RefElem_1 => ReferenceTetrahedron_Pointer( NSD = Obj % NSD )
          IF( Order .NE. 1 ) THEN
            RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( RefElem_1 )
          ELSE
            RefElem_2 => RefElem_1
          END IF
        ELSE IF( isHexahedron( ElemType ) ) THEN
          RefElem_1 => ReferenceHexahedron_Pointer( NSD = Obj % NSD )
          IF( Order .NE. 1 ) THEN
            RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( RefElem_1 )
          ELSE
            RefElem_2 => RefElem_1
          END IF
        ELSE IF( isPrism( ElemType ) ) THEN
          RefElem_1 => ReferencePrism_Pointer( NSD = Obj % NSD )
          IF( Order .NE. 1 ) THEN
            RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( RefElem_1 )
          ELSE
            RefElem_2 => RefElem_1
          END IF
        ELSE IF( isPyramid( ElemType ) ) THEN
          RefElem_1 => ReferencePyramid_Pointer( NSD = Obj % NSD )
          IF( Order .NE. 1 ) THEN
            RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
            DEALLOCATE( RefElem_1 )
          ELSE
            RefElem_2 => RefElem_1
          END IF
        END IF

        ! now loop over all elements
        DO iel = 1, tElements
          Nptrs = Obj % VolumeEntities( i ) % Nptrs( :, iel )
          Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
          CALL MeshObj % Append( Elem )
        END DO
      END DO
    END IF
  END IF

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  NULLIFY( Elem, RefElem_1, RefElem_2 )

END PROCEDURE gmsh_mesh_getelements_1

!----------------------------------------------------------------------------
!                                                                 getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_2
  ! Define internal variable
  INTEGER( I4B ) :: i, j, tElements, tag, ElemType, EntityTag, iel, Order
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : ), Indices( : )
  CLASS( ReferenceElement_ ), POINTER :: RefElem_1, RefElem_2
  CLASS( Element_ ), POINTER :: Elem
  !
  RefElem_1 => NULL( ); RefElem_2 => NULL( ); Elem => NULL( )
  iel = 0
  tElements = Obj % TotalElements( XiDim )
  !
  IF( PRESENT( Offset ) ) THEN
    tag = 2
  ELSE
    ! Initiate the mesh obj
    tag = 1
    CALL MeshObj % Initiate( NSD = Obj % NSD, tElements = tElements )
  END IF
  !
  SELECT CASE ( XiDim )
  CASE( 1 )
    IF( ALLOCATED( Obj % CurveEntities ) ) THEN
      IF( SIZE( Obj % CurveEntities ) .NE. 0 ) THEN
      RefElem_1 => ReferenceLine_Pointer( NSD = Obj % NSD )
      RefElem_2 => RefElem_1
        DO i = 1, SIZE( Obj % CurveEntities )
          ElemType = Obj % CurveEntities( i ) % ElemType
          EntityTag = Obj % CurveEntities( i )  % Uid
          tElements = Obj % CurveEntities( i ) % TotalElements( )
          ! get the order of element
          ! if element is not linear then make lagrange element
          Order = ElementOrder( ElemType )
          IF( Order .NE. 1 ) THEN
            !SELECT TYPE( RefElem_1 )
              !TYPE IS ( ReferenceLine_ )
                RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                DEALLOCATE( RefElem_1 )
            !END SELECT
          END IF
          ! now loop over all elements
          SELECT CASE( tag )
          CASE( 1 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = Obj % CurveEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % Append( Elem )
            END DO
          CASE( 2 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = Obj % CurveEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END IF
    END IF
  CASE( 2 )
    ! Surface Entities
    IF( ALLOCATED( Obj % SurfaceEntities ) ) THEN
      IF( SIZE( Obj % SurfaceEntities ) .NE. 0 ) THEN
        DO i = 1, SIZE( Obj % SurfaceEntities )
          ElemType = Obj % SurfaceEntities( i ) % ElemType
          EntityTag = Obj % SurfaceEntities( i )  % Uid
          tElements = Obj % SurfaceEntities( i ) % TotalElements( )
          Order = ElementOrder( ElemType )
          !
          IF( isTriangle( ElemType ) ) THEN
            RefElem_1 => ReferenceTriangle_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( RefElem_1 )
                !TYPE IS ( ReferenceTriangle_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              !END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          ELSE IF( isQuadrangle( ElemType ) ) THEN
            RefElem_1 => ReferenceQuadrangle_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( RefElem_1 )
                !TYPE IS ( ReferenceQuadrangle_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              !END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          END IF
          ! now loop over all elements
          SELECT CASE( tag )
          CASE( 1 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = Obj % SurfaceEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % Append( Elem = Elem )
            END DO
          CASE( 2 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = Obj % SurfaceEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END IF
    END IF
  CASE( 3 )
    ! Volume Entities
    IF( ALLOCATED( Obj % VolumeEntities ) ) THEN
      IF( SIZE( Obj % VolumeEntities ) .NE. 0 ) THEN
        DO i = 1, SIZE( Obj % VolumeEntities )
          ElemType = Obj % VolumeEntities( i ) % ElemType
          EntityTag = Obj % VolumeEntities( i )  % Uid
          tElements = Obj % VolumeEntities( i ) % TotalElements( )
          Order = ElementOrder( ElemType )
          !
          IF( isTetrahedron( ElemType ) ) THEN
            RefElem_1 => ReferenceTetrahedron_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( RefElem_1 )
              !  TYPE IS ( ReferenceTetrahedron_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              !END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          ELSE IF( isHexahedron( ElemType ) ) THEN
            RefElem_1 => ReferenceHexahedron_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( RefElem_1 )
              !  TYPE IS ( ReferenceHexahedron_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              !END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          ELSE IF( isPrism( ElemType ) ) THEN
            RefElem_1 => ReferencePrism_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( RefElem_1 )
              !  TYPE IS ( ReferencePrism_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              !END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          ELSE IF( isPyramid( ElemType ) ) THEN
            RefElem_1 => ReferencePyramid_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              !SELECT TYPE( RefElem_1 )
              !  TYPE IS ( ReferencePyramid_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
              !    DEALLOCATE( RefElem_1 )
              !END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          END IF
          ! now loop over all elements
          SELECT CASE( tag )
          CASE( 1 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = Obj % VolumeEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % Append( Elem )
            END DO
          CASE( 2 )
            DO j = 1, tElements
              iel = iel + 1
              Nptrs = Obj % VolumeEntities( i ) % Nptrs( :, j )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END IF
    END IF
  END SELECT

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  IF( ALLOCATED( Indices ) ) DEALLOCATE( Indices )
  NULLIFY( Elem, RefElem_1, RefElem_2 )

END PROCEDURE gmsh_mesh_getelements_2

!----------------------------------------------------------------------------
!                                                                getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_2c

  IF( XiDim .EQ. Obj % nsd ) THEN

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
      CALL Obj % getElements( &
      & MeshObj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & FEObj = FEObj, &
      & Offset = Offset )
    ELSE
      CALL Obj % getElements( &
      & MeshObj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & FEObj = FEObj )
    END IF

  END IF

  IF( XiDim .EQ. Obj % nsd - 1 ) THEN

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
      CALL Obj % getElements( &
        & MeshObj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & FEObj = FEObj, &
        & Offset = Offset )
    ELSE
      CALL Obj % getElements( &
        & MeshObj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & FEObj = FEObj )
    END IF
  END IF

  IF( XiDim .LE. Obj % nsd - 2 ) THEN
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
  CLASS( ReferenceElement_ ), POINTER :: RefElem_1, RefElem_2
  CLASS( Element_ ), POINTER :: Elem
  !
  RefElem_1 => NULL( ); RefElem_2 => NULL( ); Elem => NULL( )
  iel = 0
  tElements = Obj % TotalElements( XiDim, Tag )
  IF( PRESENT( Offset ) ) THEN
    AlgoTag = 2
  ELSE
    ! Initiate the mesh obj
    AlgoTag = 1
    CALL MeshObj % Initiate( NSD = Obj % NSD, tElements = tElements )
  END IF
  !
  SELECT CASE( XiDim )
  CASE( 1 )
    IF( ALLOCATED( Obj % CurveEntities ) &
      & .AND. SIZE( Obj % CurveEntities ) .NE. 0 ) THEN
      !
      ! get the indices
      Indices = Obj % PhysicalNames % IndexOfPhysicalCurve( Tag )
      !
      DO i = 1, SIZE( Indices )
        IF( Indices( i ) .EQ. 0 ) CYCLE
        j = Indices( i )
        tSize = SIZE( Obj % PhysicalNames % Entities( j ) )
        !
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( Obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        !
        RefElem_1 => ReferenceLine_Pointer( NSD = Obj % NSD )
        RefElem_2 => RefElem_1
        !
        DO k = 1, tSize
          j = Entities( k )
          ElemType = Obj % CurveEntities( j ) % ElemType
          EntityTag = Obj % CurveEntities( j ) % UiD
          tElements = Obj % CurveEntities( j ) % TotalElements( )
          ! get the order of element
          ! if element is not linear then make lagrange element
          Order = ElementOrder( ElemType )
          IF( Order .NE. 1 ) THEN
            ! SELECT TYPE( RefElem_1 )
              ! TYPE IS ( ReferenceLine_ )
                NULLIFY( RefElem_2 )
                RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                DEALLOCATE( RefElem_1 )
            ! END SELECT
          END IF
          !
          SELECT CASE( AlgoTag )
          CASE( 1 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = Obj % CurveEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % Append( Elem )
            END DO
          CASE( 2 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = Obj % CurveEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % SetElement( Elem = Elem, iel = offset + iel )
            END DO
          END SELECT
        END DO
      END DO
    END IF
  CASE( 2 )
    IF( ALLOCATED( Obj % SurfaceEntities ) &
      & .AND. SIZE( Obj % SurfaceEntities ) .NE. 0 ) THEN
      !
      RefElem_1 => ReferenceLine_Pointer( NSD = Obj % NSD )
      RefElem_2 => RefElem_1
      ! get the indices
      Indices = Obj % PhysicalNames % IndexOfPhysicalSurface( Tag )
      !
      DO i = 1, SIZE( Indices )
        IF( Indices( i ) .EQ. 0 ) CYCLE
        j = Indices( i )
        tSize = SIZE( Obj % PhysicalNames % Entities( j ) )
        !
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( Obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        !
        DO k = 1, tSize
          j = Entities( k )
          ElemType = Obj % SurfaceEntities( j ) % ElemType
          EntityTag = Obj % SurfaceEntities( j ) % UiD
          tElements = Obj % SurfaceEntities( j ) % TotalElements( )
          ! get the order of element
          Order = ElementOrder( ElemType )
          ! if element is not linear then make lagrange element
          IF( isTriangle( ElemType ) ) THEN
            RefElem_1 => ReferenceTriangle_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( RefElem_1 )
                ! TYPE IS ( ReferenceTriangle_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              ! END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          ELSE IF( isQuadrangle( ElemType ) ) THEN
            RefElem_1 => ReferenceQuadrangle_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( RefElem_1 )
                ! TYPE IS ( ReferenceQuadrangle_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              ! END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          END IF
          !
          SELECT CASE( AlgoTag )
          CASE( 1 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = Obj % SurfaceEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % Append( Elem )
            END DO
          CASE( 2 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = Obj % SurfaceEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END DO
    END IF
  CASE( 3 )
    IF( ALLOCATED( Obj % VolumeEntities ) &
      & .AND. SIZE( Obj % VolumeEntities ) .NE. 0 ) THEN
      !
      RefElem_1 => ReferenceLine_Pointer( NSD = Obj % NSD )
      RefElem_2 => RefElem_1
      ! get the indices
      Indices = Obj % PhysicalNames % IndexOfPhysicalVolume( Tag )
      !
      DO i = 1, SIZE( Indices )
        IF( Indices( i ) .EQ. 0 ) CYCLE
        j = Indices( i )
        tSize = SIZE( Obj % PhysicalNames % Entities( j ) )
        !
        IF( tSize .NE. 0 ) THEN
          Entities = ArrayValues( Obj % PhysicalNames % Entities( j ), &
            & TypeIntI4B )
        END IF
        !
        DO k = 1, tSize
          j = Entities( k )
          ElemType = Obj % VolumeEntities( j ) % ElemType
          EntityTag = Obj % VolumeEntities( j ) % UiD
          tElements = Obj % VolumeEntities( j ) % TotalElements( )
          ! get the order of element
          Order = ElementOrder( ElemType )
          ! if element is not linear then make lagrange element
          !
          IF( isTetrahedron( ElemType ) ) THEN
            RefElem_1 => ReferenceTetrahedron_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( RefElem_1 )
                ! TYPE IS ( ReferenceTetrahedron_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              ! END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          ELSE IF( isHexahedron( ElemType ) ) THEN
            RefElem_1 => ReferenceHexahedron_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( RefElem_1 )
                ! TYPE IS ( ReferenceHexahedron_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              ! END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          ELSE IF( isPrism( ElemType ) ) THEN
            RefElem_1 => ReferencePrism_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( RefElem_1 )
                ! TYPE IS ( ReferencePrism_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              ! END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          ELSE IF( isPyramid( ElemType ) ) THEN
            RefElem_1 => ReferencePyramid_Pointer( NSD = Obj % NSD )
            IF( Order .NE. 1 ) THEN
              ! SELECT TYPE( RefElem_1 )
                ! TYPE IS ( ReferencePyramid_ )
                  RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
                  DEALLOCATE( RefElem_1 )
              ! END SELECT
            ELSE
              RefElem_2 => RefElem_1
            END IF
          END IF
          !
          SELECT CASE( AlgoTag )
          CASE( 1 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = Obj % VolumeEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % Append( Elem )
            END DO
          CASE( 2 )
            DO l = 1, tElements
              iel = iel + 1
              Nptrs = Obj % VolumeEntities( j ) % Nptrs( :, l )
              Elem => getFEPointer( FEObj, Nptrs, EntityTag, RefElem_2 )
              CALL MeshObj % SetElement( Elem = Elem, iel = iel + offset )
            END DO
          END SELECT
        END DO
      END DO
    END IF
  END SELECT

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
  IF( ALLOCATED( Indices ) ) DEALLOCATE( Indices )
  IF( ALLOCATED( Entities ) ) DEALLOCATE( Entities )
  NULLIFY( RefElem_2, RefElem_1, Elem )

END PROCEDURE gmsh_mesh_getelements_3

!----------------------------------------------------------------------------
!                                                                getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_3c

  IF( XiDim .EQ. Obj % nsd ) THEN

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
      CALL Obj % getElements( &
      & MeshObj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & Tag = Tag, &
      & FEObj = FEObj, &
      & Offset = Offset )
    ELSE
      CALL Obj % getElements( &
      & MeshObj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & Tag = Tag, &
      & FEObj = FEObj )
    END IF

  END IF


  IF( XiDim .EQ. Obj % nsd - 1 ) THEN

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
      CALL Obj % getElements( &
        & MeshObj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & Tag = Tag, &
        & FEObj = FEObj, &
        & Offset = Offset )
    ELSE
      CALL Obj % getElements( &
        & MeshObj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & Tag = Tag, &
        & FEObj = FEObj )
    END IF
  END IF

  IF( XiDim .LE. Obj % nsd - 2 ) THEN
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
  Indices = Obj % PhysicalNames % getIndex( TagNames )
  Tag = Obj % PhysicalNames % Tag( Indices )
  !
  IF( PRESENT( Offset ) ) THEN
    CALL Obj % getElements( MeshObj = MeshObj, XiDim = XiDim, &
      & FEObj = FEObj, Offset = Offset, Tag = Tag )
  ELSE
    CALL Obj % getElements( MeshObj = MeshObj, XiDim = XiDim, &
      & FEObj = FEObj, Tag = Tag )
  END IF
  IF( ALLOCATED( Tag ) ) DEALLOCATE( Tag )
  IF( ALLOCATED( Indices ) ) DEALLOCATE( Indices )
END PROCEDURE gmsh_mesh_getelements_4

!----------------------------------------------------------------------------
!                                                                getElements
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getelements_4c

  IF( XiDim .EQ. Obj % nsd ) THEN

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
      CALL Obj % getElements( &
      & MeshObj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & TagNames = TagNames, &
      & FEObj = FEObj, &
      & Offset = Offset )
    ELSE
      CALL Obj % getElements( &
      & MeshObj = Dom % Omega( Indx ) % Ptr, &
      & XiDim = XiDim, &
      & TagNames = TagNames, &
      & FEObj = FEObj )
    END IF

  END IF


  IF( XiDim .EQ. Obj % nsd - 1 ) THEN

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
      CALL Obj % getElements( &
        & MeshObj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & TagNames = TagNames, &
        & FEObj = FEObj, &
        & Offset = Offset )
    ELSE
      CALL Obj % getElements( &
        & MeshObj = Dom % Boundary( Indx ) % Ptr, &
        & XiDim = XiDim, &
        & TagNames = TagNames, &
        & FEObj = FEObj )
    END IF
  END IF

  IF( XiDim .LE. Obj % nsd - 2 ) THEN
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
        & FEObj = TypeElement )

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
          & FEObj = TypeElement )

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
            & FEObj = TypeFacetElement )

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
            & FEObj = TypeElement )

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
        & FEObj = TypeElement )

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