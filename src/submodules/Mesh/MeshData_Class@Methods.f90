SUBMODULE( MeshData_Class ) Methods
  !! This module contains type bound procedures of [[MeshData_]]
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_meshdata_1
  ! Define internal variables
  INTEGER( I4B ) :: iel, Dummy
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  CLASS( Element_ ), POINTER :: Elem
  !
  Obj % MaxNptrs = 0_I4B
  Obj % isInitiated = .TRUE.

  ! find out max and min nptrs
  IF( .NOT. ALLOCATED( Obj % ElemToNode ) ) THEN
    ALLOCATE( Obj % ElemToNode( MeshObj % tElements ) )
  END IF

  Elem => Meshobj%Elem(1)%ptr
  Obj% refelem => elem%refelem
  Elem => NULL( )

  DO iel = 1, MeshObj % tElements
    Elem => MeshObj % Elem( iel ) % Ptr
    IF( .NOT. ASSOCIATED( Elem ) ) EXIT ! this is redundate
    Nptrs = .Nptrs. Elem
    Obj % ElemToNode( iel ) % Val = Nptrs
    Dummy = MAXVAL( Nptrs )
    IF( Dummy .GE. Obj % MaxNptrs ) Obj % MaxNptrs = Dummy
    Dummy = MINVAL( Nptrs )
    IF( iel .eq. 1 ) Obj % MinNptrs = Dummy
    IF( Dummy .LE. Obj % MinNptrs ) Obj % MinNptrs = Dummy
  END DO
  !
  IF( ALLOCATED( Obj % Local_Nptrs ) ) DEALLOCATE( Obj % Local_Nptrs )
  ALLOCATE( Obj % Local_Nptrs( Obj % MaxNptrs ) )
  Obj % Local_Nptrs = 0
  !
  DO iel =1, MeshObj % tElements
    Elem => MeshObj % Elem( iel ) % Ptr
    IF( ASSOCIATED( Elem ) ) THEN
      Nptrs = .Nptrs. Elem
      Obj % Local_Nptrs( Nptrs ) = Nptrs
    END IF
  END DO
  !
  Dummy = COUNT( Obj % Local_Nptrs .NE. 0 )
  IF( ALLOCATED( Obj % Nptrs ) ) DEALLOCATE( Obj % Nptrs )
  ALLOCATE( Obj % Nptrs( Dummy ) )
  !
  Dummy = 0
  DO iel = 1, Obj % MaxNptrs
    IF( Obj % Local_Nptrs( iel ) .EQ. 0 ) CYCLE
    Dummy = Dummy + 1
    Obj % Nptrs( Dummy ) = Obj % Local_Nptrs( iel )
    Obj % Local_Nptrs( iel ) = Dummy
  END DO
  !
  Obj % tNodes = SIZE( Obj % Nptrs )
  DEALLOCATE( Nptrs )
  NULLIFY( Elem )
END PROCEDURE init_meshdata_1

!----------------------------------------------------------------------------
!                                                                 MeshData
!----------------------------------------------------------------------------

MODULE PROCEDURE meshdata_1
  CALL Ans % Initiate( MeshObj )
END PROCEDURE meshdata_1

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_MeshData
  IF( ALLOCATED( Obj % NodeToElem ) ) DEALLOCATE( Obj % NodeToElem )
  IF( ALLOCATED( Obj % ElemToElem ) ) DEALLOCATE( Obj % ElemToElem )
  IF( ALLOCATED( Obj % NTN ) ) DEALLOCATE( Obj % NTN )
  IF( ALLOCATED( Obj % ElemToNode ) ) DEALLOCATE( Obj % ElemToNode )
  IF( ALLOCATED( Obj % BoundaryData ) ) DEALLOCATE( Obj % BoundaryData )
  IF( ALLOCATED( Obj % InternalBndyElemNum ) ) &
    & DEALLOCATE( Obj % InternalBndyElemNum )
  IF( ALLOCATED( Obj % InternalBoundaryData ) ) &
    & DEALLOCATE( Obj % InternalBoundaryData )
  IF( ALLOCATED( Obj % LBndyIndex ) ) DEALLOCATE( Obj % LBndyIndex )
  IF( ALLOCATED( Obj % Nptrs ) ) DEALLOCATE( Obj % Nptrs )
  IF( ALLOCATED( Obj % BoundaryNptrs ) ) DEALLOCATE( Obj % BoundaryNptrs )
  IF( ALLOCATED( Obj % InternalNptrs ) ) DEALLOCATE( Obj % InternalNptrs )
  IF( ALLOCATED( Obj % Local_Nptrs ) ) DEALLOCATE( Obj % Local_Nptrs )
  !
  Obj%refelem => NULL()
  Obj % isInitiated = .FALSE.
  Obj % tNodes = 0_I4B
  Obj % MaxNptrs = 0_I4B
  Obj % MinNptrs = 0_I4B
END PROCEDURE Deallocate_MeshData

!----------------------------------------------------------------------------
!                                                            getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_total_nodes
    Ans = Obj % tNodes
END PROCEDURE get_total_nodes

!----------------------------------------------------------------------------
!                                                      getTotalBoundaryNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tbndy_nodes
  IF( ALLOCATED( Obj % BoundaryNptrs )  ) THEN
    Ans = SIZE( Obj % BoundaryNptrs )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_tbndy_nodes

!----------------------------------------------------------------------------
!                                                      getTotalInternalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tint_nodes
  IF( ALLOCATED( Obj % InternalNptrs )  ) THEN
    Ans = SIZE( Obj % InternalNptrs )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_tint_nodes

!----------------------------------------------------------------------------
!                                                       TotalBoundaryElements
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tbndy_elems
  IF( ALLOCATED( Obj % BoundaryData ) ) THEN
    Ans = SIZE( Obj % BoundaryData )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_tbndy_elems

!----------------------------------------------------------------------------
!                                                              getBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE get_Bbox
  INTEGER( I4B ) :: ii, nsd
  REAL( DFP ) :: lim( 6 )

  lim = 0.0_DFP
  nsd = SIZE( nodes, 1 )

  SELECT CASE( nsd )
  CASE( 1 )

    DO ii = 1, Obj % tNodes
      lim( 1 ) = MIN( lim( 1 ), nodes( 1, Obj % Nptrs( ii  ) ) )
      lim( 2 ) = MAX( lim( 2 ), nodes( 1, Obj % Nptrs( ii  ) ) )
    END DO

  CASE( 2 )

    DO ii = 1, Obj % tNodes
      lim( 1 ) = MIN( lim( 1 ), nodes( 1, Obj % Nptrs( ii  ) ) )
      lim( 2 ) = MAX( lim( 2 ), nodes( 1, Obj % Nptrs( ii  ) ) )
      lim( 3 ) = MIN( lim( 3 ), nodes( 2, Obj % Nptrs( ii  ) ) )
      lim( 4 ) = MAX( lim( 4 ), nodes( 2, Obj % Nptrs( ii  ) ) )
    END DO

  CASE( 3 )

    DO ii = 1, Obj % tNodes
      lim( 1 ) = MIN( lim( 1 ), nodes( 1, Obj % Nptrs( ii  ) ) )
      lim( 2 ) = MAX( lim( 2 ), nodes( 1, Obj % Nptrs( ii  ) ) )
      lim( 3 ) = MIN( lim( 3 ), nodes( 2, Obj % Nptrs( ii  ) ) )
      lim( 4 ) = MAX( lim( 4 ), nodes( 2, Obj % Nptrs( ii  ) ) )
      lim( 5 ) = MIN( lim( 5 ), nodes( 3, Obj % Nptrs( ii  ) ) )
      lim( 6 ) = MAX( lim( 6 ), nodes( 3, Obj % Nptrs( ii  ) ) )
    END DO

  END SELECT

  CALL Initiate( Obj = Ans, nsd = nsd, lim = lim )

END PROCEDURE get_Bbox

!----------------------------------------------------------------------------
!                                                              getLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE local_from_global
  ! Define intent of dummy variables
  INTEGER( I4B ) :: i, n
  !
  n = SIZE( GlobalIndx )
  ALLOCATE( Ans( n ) )
  DO i =1, n
    IF( GlobalIndx( i ) .LT. Obj  % MinNptrs &
        & .OR. GlobalIndx( i ) .GT. Obj % MaxNptrs ) THEN
      Ans( i ) = 0
    ELSE
      Ans( i ) = Obj % Local_Nptrs( GlobalIndx( i ) )
    END IF
  END DO

END PROCEDURE local_from_global

!----------------------------------------------------------------------------
!                                                              getLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE local_from_global_scalar
  IF( GlobalIndx .LT. Obj  % MinNptrs .OR. GlobalIndx .GT. Obj % MaxNptrs ) THEN
    Ans = 0
  ELSE
    Ans = Obj % Local_Nptrs( GlobalIndx )
  END IF
END PROCEDURE local_from_global_scalar

!----------------------------------------------------------------------------
!                                                                GlobalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE global_from_local
  ! define internal variables
  INTEGER( I4B ) :: i, n

  n = SIZE( LocalIndx )
  ALLOCATE( Ans( n ) ); Ans = 0

  DO i = 1, n
    IF( LocalIndx( i ) .GT. Obj % tNodes ) THEN
      Ans( i ) = 0
    ELSE
      Ans( i ) = Obj % Nptrs( LocalIndx( i ) )
    END IF
  END DO

END PROCEDURE global_from_local

!----------------------------------------------------------------------------
!                                                             getGlobalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE global_from_local_scalar
  IF( LocalIndx .LE. Obj % tNodes ) THEN
    Ans = Obj % Nptrs( LocalIndx )
  ELSE
    Ans = 0
  END IF
END PROCEDURE global_from_local_scalar

!----------------------------------------------------------------------------
!                                                              isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE is_node_present
  IF( Nptrs .GT. Obj % MaxNptrs &
    & .OR. Nptrs .LT. Obj % MinNptrs &
    & .OR. Obj % Local_Nptrs( Nptrs ) .EQ. 0 ) THEN
    Ans = .FALSE.
  ELSE
    Ans = .TRUE.
  END IF
END PROCEDURE is_node_present

!----------------------------------------------------------------------------
!                                                     isNodeToNodesInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_node_nodes_initiated
  IF( ALLOCATED( Obj % NTN ) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_node_nodes_initiated

!----------------------------------------------------------------------------
!                                                  isNodeToElementsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_node_elements_initiated
  IF( ALLOCATED( Obj % NodeToElem ) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_node_elements_initiated

!----------------------------------------------------------------------------
!                                               isElementToElementsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_element_elements_initiated
  IF( ALLOCATED( Obj % ElemToElem ) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_element_elements_initiated


!----------------------------------------------------------------------------
!                                                  isElementToNodesInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_element_nodes_initiated
  IF( ALLOCATED( Obj % ElemToNode ) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_element_nodes_initiated

!----------------------------------------------------------------------------
!                                                    isBoundaryDataInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_boundarydata
  IF( ALLOCATED( Obj % BoundaryData ) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_boundarydata

!----------------------------------------------------------------------------
!                                                   isInternalNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_internalnptrs
  IF( ALLOCATED( Obj % InternalNptrs ) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_internalnptrs

!----------------------------------------------------------------------------
!                                                   isBoundaryNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_bndy_nptrs
  IF( ALLOCATED( Obj % BoundaryNptrs ) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_bndy_nptrs

!----------------------------------------------------------------------------
!                                                      isLocalNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_local_nptrs
  IF( ALLOCATED( Obj % Local_Nptrs ) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_local_nptrs

!----------------------------------------------------------------------------
!                                            isInternalBoundaryDataInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_int_bndy_data
  IF( ALLOCATED( Obj %  InternalBoundaryData) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_int_bndy_data

!----------------------------------------------------------------------------
!                                                     InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE init_node_elements
  ! Define internal  variables
  INTEGER( I4B ) :: iel, iNode
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  CLASS( Element_ ), POINTER :: Elem

  Elem => NULL( )

  IF( .NOT. ALLOCATED( Obj % NodeToElem )  ) THEN
    ALLOCATE( Obj % NodeToElem( Obj % tNodes ) )
    DO iel = 1, MeshObj % tElements
      Elem => MeshObj % Elem( iel ) % Ptr
      IF( .NOT. ASSOCIATED( Elem ) ) CYCLE
      local_nptrs = Obj % LocalNptrs( .Nptrs. Elem )
      DO iNode = 1, SIZE( local_nptrs )
        CALL Append( Obj % NodeToElem ( local_nptrs( iNode ) ), iel )
      END DO
    END DO
    DEALLOCATE( local_nptrs )
    NULLIFY( Elem )
  END IF
END PROCEDURE init_node_elements

!----------------------------------------------------------------------------
!                                                           getNodeToElement
!----------------------------------------------------------------------------

MODULE PROCEDURE node_elements
  Ans = Obj % NodeToElem( Obj % LocalNptrs( GlobalIndx = GlobalPt ) ) % Val
END PROCEDURE node_elements

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE init_node_nodes
  ! Define internal  variables
  INTEGER( I4B ) :: iel, iLocalNode, tSize, iGlobalNode
  INTEGER( I4B ), ALLOCATABLE ::  global_nptrs( : ), NearElements( : )
  CLASS( Element_ ), POINTER :: Elem

  Elem => NULL( )
  IF( .NOT. ALLOCATED( Obj % NTN ) ) THEN
    ALLOCATE( Obj % NTN( Obj % tNodes ) )
    CALL Obj % InitiateNodeToElements( MeshObj )
    DO iLocalNode = 1, Obj % tNodes
      tSize = SIZE ( Obj % NodeToElem( iLocalNode ) )
      IF( tSize .EQ. 0  ) CYCLE
      iGlobalNode = Obj % GlobalNptrs( LocalIndx = iLocalNode )
      NearElements = Obj % NodeToElements( GlobalPt = iGlobalNode )
      DO iel = 1, SIZE( NearElements )
        Elem => MeshObj % Elem( NearElements( iel ) ) % Ptr
        global_nptrs = .Nptrs. Elem
        global_nptrs = PACK( global_nptrs, global_nptrs .NE. iGlobalNode )
        CALL Append( Obj % NTN( iLocalNode ), global_nptrs )
      END DO
      CALL RemoveDuplicates( Obj % NTN( iLocalNode ) )
    END DO
    IF( ALLOCATED( global_nptrs ) ) DEALLOCATE( global_nptrs )
    IF( ALLOCATED( NearElements ) ) DEALLOCATE( NearElements )
    NULLIFY( Elem )
  END IF

END PROCEDURE init_node_nodes

!----------------------------------------------------------------------------
!                                                            get_n_to_n
!----------------------------------------------------------------------------

MODULE PROCEDURE get_node_nodes
  ! Define internal variable
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  INTEGER( I4B ) :: i

  i = Obj % LocalNptrs( GlobalIndx = GlobalNode )
  IF( IncludeSelf ) THEN
    Nptrs = Obj % NTN( i ) % Val
    i = SIZE( Nptrs )
    ALLOCATE( Ans( i + 1 ) )
    Ans( 1 ) = GlobalNode
    Ans( 2 : ) = Nptrs
    DEALLOCATE( Nptrs )
  ELSE
    Ans = Obj % NTN( i ) % Val
  END IF
END PROCEDURE get_node_nodes

!----------------------------------------------------------------------------
!                                                      init_element_elements
!----------------------------------------------------------------------------

MODULE PROCEDURE init_element_elements
  ! Define internal  variables
  INTEGER( I4B ) :: b, i, j, r,  iel1, tFace1, iFace1, NNS1, pt1, &
    & iel2, tFace2, iFace2, NNS2
  INTEGER( I4B ), ALLOCATABLE :: global_nptrs1( : ),  FM1( :, : ), &
    & global_nptrsFace1( : ), n2e1( : ), global_nptrs2( : ), FM2( :, : ), &
    & global_nptrsFace2( : )
  LOGICAL( LGT ) :: Found
  CLASS( Element_ ), POINTER :: Elem, Elem2

  Elem => NULL( )
  Elem2 => NULL( )

  IF( .NOT. ALLOCATED( Obj % ElemToElem ) ) THEN
    CALL Obj % InitiateNodeToElements( MeshObj )
    ALLOCATE( Obj % ElemToElem( MeshObj % tElements ) )
    DO iel1 = 1, MeshObj % tElements
      Elem => MeshObj % Elem( iel1 ) % Ptr
      global_nptrs1 = .Nptrs. Elem ! get nptrs
      FM1 = FacetMatrix( Elem % RefElem ) ! get facet matrix
      tFace1 = SIZE( FM1, 1 ) ! total number of faces

      DO iFace1 = 1, tFace1
        FOUND = .FALSE.

        !<--- Total number of nodes in a iFace1
        NNS1 = FM1( iFace1, 3 ); b = 3 + NNS1

        !<--- getting Node numbers of iFace1
        global_nptrsFace1 = global_nptrs1( FM1( iFace1, 4 : b ) )

        !<--- select a point on facet
        pt1 = global_nptrsFace1( 1 )

        !<--- get element connected to this
        n2e1 = Obj % NodeToElements( GlobalPt = pt1 )
        DO iel2 = 1, SIZE( n2e1 )
          IF( iel1 .EQ. n2e1( iel2 ) ) CYCLE

          Elem2 => MeshObj % Elem( n2e1( iel2 ) ) % Ptr
          global_nptrs2 = .Nptrs. Elem2
          FM2 = FacetMatrix( Elem2 % RefElem )
          tFace2 = SIZE( FM2, 1 )
          DO iFace2 = 1, tFace2
            !<--- getting total number of nodes in iFace2
            NNS2 = FM2( iFace2, 3 ); b = 3 + NNS2
            !<--- getting node number of iFace2
            global_nptrsFace2 = global_nptrs2( FM2( iFace2, 4:b ) )
            r = 0
            DO i = 1, NNS2
              DO j = 1, NNS1
                IF( global_nptrsFace2( i ) .EQ. global_nptrsFace1( j ) ) THEN
                  r = r + 1
                END IF
              END DO
            END DO
            IF( r .EQ. NNS1 ) THEN
              CALL APPEND( Obj % ElemToElem( iel1 ), &
                & [n2e1(iel2), iFace1, iFace2])
              FOUND = .TRUE.
              EXIT
            END IF
          END DO
          IF( FOUND ) THEN
            EXIT
          END IF
        END DO
      END DO
    END DO
    DEALLOCATE( global_nptrs1, FM1, global_nptrsFace1, &
      & n2e1, global_nptrs2, FM2, global_nptrsFace2 )
    NULLIFY( Elem, Elem2 )
  END IF
END PROCEDURE init_element_elements

!----------------------------------------------------------------------------
!                                                       getElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE get_elem_elems_1
  ! Define internal variables
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  INTEGER( I4B ) :: tSize

  Nptrs = Obj % ElemToElem( iel ) % Val
  tSize = SIZE( Nptrs ) / 3
  Ans = TRANSPOSE( RESHAPE( Nptrs, [3, tSize] ) )
  DEALLOCATE( Nptrs )
END PROCEDURE get_elem_elems_1

!----------------------------------------------------------------------------
!                                                        getElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE get_elem_elems_2
  ! Define internal variable
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )

  SELECT CASE( iel( 2 ) )
    CASE( 0 )
      Ans = Obj % ElementToElements( iel = iel( 1 ) )
    CASE DEFAULT
      Nptrs = Obj % ElemToElem( iel( 1 ) ) % Val
      ALLOCATE( Ans( SIZE( Nptrs )/3, 1 ) )
      Ans( :, 1 ) = Nptrs( 1::3 )
      DEALLOCATE( Nptrs )
  END SELECT
END PROCEDURE get_elem_elems_2

!----------------------------------------------------------------------------
!                                                     InitiateElementToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE init_elem_nodes
  ! Define internal  variables
  INTEGER( I4B ) :: iel
  CLASS( Element_ ), POINTER :: Elem
  !
  IF( .NOT. ALLOCATED( Obj % ElemToNode ) ) THEN
    ALLOCATE( Obj % ElemToNode( MeshObj % tElements ) )
    DO iel = 1, MeshObj % tElements
      Elem => MeshObj % Elem( iel ) % Ptr
      Obj % ElemToNode( iel ) % Val = .Nptrs. Elem
    END DO
    NULLIFY( Elem )
  END IF
  !
END PROCEDURE init_elem_nodes

!----------------------------------------------------------------------------
!                                                            ElementToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_elem_nodes
  Ans = Obj % ElemToNode( iel ) % Val
END PROCEDURE get_elem_nodes

!----------------------------------------------------------------------------
!                                                       InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE init_bndy_data
  ! Define internal variables
  INTEGER( I4B ) :: iel, tElements, te2e, tFace, tBndyElem, &
    & tBndyFace, i, j, k, b, dummy
  INTEGER( I4B ), ALLOCATABLE :: e2eData( : ), FM( :, : ), FaceVec( : ), &
    & DummyNptrs( : ), local_nptrs( : ), global_nptrs( : )
  CLASS( Element_ ), POINTER :: Elem

  IF( .NOT. ALLOCATED( Obj % BoundaryData ) ) THEN
    tElements = MeshObj % tElements
    CALL Reallocate( Obj % LBndyIndex,  tElements )
    Obj % LBndyIndex = 0_I4B
    IF( ALLOCATED( Obj % BoundaryNptrs ) ) DEALLOCATE( Obj % BoundaryNptrs )
    SELECT CASE( tElements )
    CASE( 1 )
      ALLOCATE( Obj % BoundaryData( 1 ) )
      Obj % LBndyIndex( 1 ) = 1
      Elem => MeshObj % Elem( 1 ) % Ptr
      Obj % BoundaryNptrs = .Nptrs. Elem
      FM = FacetMatrix( Elem % RefElem )
      tFace = SIZE( FM, 1 )
      CALL Initiate( Obj % BoundaryData( 1 ), tFace + 1 )
      Obj % BoundaryData( 1 ) % Val( 1 ) = 1
      Obj % BoundaryData( 1 ) % Val( 2: ) = [(i, i=1, tFace)]
    CASE DEFAULT
      CALL Obj % InitiateElementToElements( MeshObj = MeshObj )
      DO iel = 1, tElements
        dummy = SIZE( Obj % ElemToElem( iel ) )
        IF( dummy .EQ. 0 ) CYCLE
        e2eData = Obj % ElemToElem( iel ) % Val
        te2e = SIZE( e2eData ) / 3
        Elem => MeshObj % Elem( iel ) % Ptr
        FM = FacetMatrix( Elem % RefElem )
        tFace = SIZE( FM, 1 )
        IF( tFace .NE. te2e ) THEN
          Obj % LBndyIndex( iel ) = 1
        END IF
      END DO
      tBndyElem = COUNT( Obj % LBndyIndex .NE. 0 )
      ALLOCATE( Obj % BoundaryData( tBndyElem ) )
      ALLOCATE( DummyNptrs( Obj % MaxNptrs ) )
      DummyNptrs = 0; k = 0
      DO iel = 1, tElements
        IF( Obj % LBndyIndex( iel ) .EQ. 0 ) CYCLE
        k = k + 1
        Obj % LBndyIndex( iel ) = k
        e2eData = Obj % ElemToElem( iel ) % Val
        te2e = SIZE( e2eData ) / 3
        IF( ALLOCATED( FaceVec ) ) DEALLOCATE( FaceVec )
        ALLOCATE( FaceVec( te2e ) )
        DO i = 1, te2e
          FaceVec( i ) = e2eData( 3*(i-1) + 2 )
        END DO
        Elem => MeshObj % Elem( iel ) % Ptr
        FM = FacetMatrix( Elem % RefElem )
        tFace = SIZE( FM, 1 )
        tBndyFace = tFace - te2e
        CALL Initiate( Obj % BoundaryData( k ), tBndyFace + 1 )
        Obj % BoundaryData( k ) % Val( 1 ) = iel
        global_nptrs = .Nptrs. Elem
        j = 0
        DO i = 1, tFace
          IF( ANY( i .EQ. FaceVec ) ) CYCLE
          j = j + 1
          Obj % BoundaryData( k ) % Val( 1 + j ) = i
          ! get local_nptrs of the face
          b = 3 + FM( i, 3 )
          local_nptrs = FM( i, 4 : b )
          DummyNptrs( global_nptrs( local_nptrs ) ) = &
            & global_nptrs( local_nptrs )
        END DO
      END DO
      b = COUNT( DummyNptrs .NE. 0 )
      ALLOCATE( Obj % BoundaryNptrs( b ) )
      k = 0
      DO i = 1, Obj % MaxNptrs
        IF( DummyNptrs( i ) .EQ. 0 ) CYCLE
        k = k + 1
        Obj % BoundaryNptrs( k ) = DummyNptrs( i )
      END DO
    END SELECT
    NULLIFY( Elem )
    IF( ALLOCATED( FM ) ) DEALLOCATE( FM )
    IF( ALLOCATED( e2eData ) ) DEALLOCATE( e2eData )
    IF( ALLOCATED( FaceVec ) ) DEALLOCATE( FaceVec )
    IF( ALLOCATED( DummyNptrs ) ) DEALLOCATE( DummyNptrs )
    IF( ALLOCATED( local_nptrs ) ) DEALLOCATE( local_nptrs )
    IF( ALLOCATED( global_nptrs ) ) DEALLOCATE( global_nptrs )
  END IF
END PROCEDURE init_bndy_data

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE is_bndy_elem
  IF( Obj % LBndyIndex( iel ) .NE. 0 ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_bndy_elem

!----------------------------------------------------------------------------
!                                                        BoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE get_bndy_elem
  ! Define internal variables
  INTEGER( I4B ) :: LocalIndx

  LocalIndx = Obj % LBndyIndex( iel )
  IF( LocalIndx .NE. 0 ) THEN
    Ans = Obj % BoundaryData( LocalIndx ) % Val( 2: )
  ELSE
    Ans = [0]
  END IF
END PROCEDURE get_bndy_elem

!----------------------------------------------------------------------------
!                                                      InitiateInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE init_int_nptrs
  ! Define internal variables
  INTEGER( I4B ) :: iel, tElements, tNodes, iNode
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : ), DummyNptrs( : )

  IF( .NOT.  ALLOCATED( Obj % InternalNptrs ) ) THEN
    CALL Obj % InitiateBoundaryData( MeshObj = MeshObj )
    CALL Obj % InitiateElementToNodes( MeshObj = MeshObj )
    tElements = MeshObj % tElements
    tNodes = Obj % maxNptrs
    ALLOCATE( DummyNptrs( tNodes ) )
    DummyNptrs = 0

    DO iel = 1, tElements
      IF( Obj % isBoundaryElement( iel = iel ) ) CYCLE
      Nptrs = Obj % ElemToNode( iel ) % Val
      DummyNptrs( Nptrs ) = Nptrs
    END DO

    iNode = COUNT( DummyNptrs .NE. 0 )
    ALLOCATE( Obj % InternalNptrs( iNode ) )

    iel = 0
    DO iNode = 1, tNodes
        IF( DummyNptrs( iNode ) .EQ. 0 ) CYCLE
        iel = iel + 1
        Obj % InternalNptrs( iel ) = DummyNptrs( iNode )
    END DO

    DEALLOCATE( DummyNptrs, Nptrs )

  END IF
END PROCEDURE init_int_nptrs

!----------------------------------------------------------------------------
!                                               InitiateInternalBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE init_int_bndydata

END PROCEDURE init_int_bndydata

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE setSparsity_1
  INTEGER( I4B ) :: i, j, idof
  INTEGER( I4B ), allocatable :: n2n( : )

  IF( PRESENT( map ) ) THEN
    IF( .NOT. Obj % isInitiated ) THEN
      CALL Obj % Initiate( MeshObj = MeshObj )
    END IF
    CALL Obj % InitiateNodeToNodes( MeshObj = MeshObj )
    DO i = 1, Obj % tNodes
      j = Obj % GlobalNptrs( LocalIndx = i )
      IF( map( j ) .EQ. 0 ) CYCLE
      n2n = map( Obj % NodeToNodes( GlobalNode = j, IncludeSelf =.true. ) )
      CALL setSparsity( Obj = Mat, Row = map(j), Col = n2n )
    END DO
    IF( ALLOCATED( n2n ) ) DEALLOCATE( n2n )

  ELSE

    IF( .NOT. Obj % isInitiated ) THEN
      CALL Obj % Initiate( MeshObj = MeshObj )
    END IF
    CALL Obj % InitiateNodeToNodes( MeshObj = MeshObj )
    DO i = 1, Obj % tNodes
      j = Obj % GlobalNptrs( LocalIndx = i )
      ! IF( j .eq. 0 ) CYCLE !<--- by construction this will never happen
      n2n = Obj % NodeToNodes( GlobalNode = j, IncludeSelf =.true. )
      CALL setSparsity( Obj = Mat, Row = j, Col = n2n )
    END DO
    IF( ALLOCATED( n2n ) ) DEALLOCATE( n2n )

  END IF

END PROCEDURE setSparsity_1

!----------------------------------------------------------------------------
!                                                         ConnectFacetTpCell
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_connect_facet_cell
  !<--- intent of dummy variables
  LOGICAL( LGT ) :: found
  INTEGER( I4B ) :: ifacet, icell, elemNum, i, r, j, k
  CLASS( Element_ ), POINTER :: Elem, FacetElem
  INTEGER( I4B ), ALLOCATABLE :: FacetNptrs( : ), pt2elem( : ), bndyData( : )
  INTEGER( I4B ), ALLOCATABLE :: FM( :, : ), CellNptrs( : ),CellFacetNptrs(:)

  !<--- main program
  !<--- if cell mesh data not initiated then initiate it
  IF( .NOT. CellMeshData % isInitiated ) THEN
    CALL CellMeshData % Initiate( CellMesh )
  END IF

  !<--- generate node to elements and boundary data
  CALL CellMeshData % InitiateNodeToElements( CellMesh )
  CALL CellMeshData % InitiateBoundaryData( CellMesh )

  Elem => NULL( ); FacetElem => NULL( )

  DO ifacet = 1, FacetMesh % SIZE()
    found = .false.

    !<--- get facet element and its nptrs
    FacetElem => FacetMesh % Elem( ifacet ) % Ptr
    FacetNptrs = .Nptrs. FacetElem

    !<--- select an arbitrary node in FacetNptr; if this is not
    !<--- present inside the CellMesh then this means this iFacet is
    !<--- orphan and we go back to the next Facet Element
    IF( .NOT. CellMeshData % isNodePresent( FacetNptrs( 1 ) ) ) CYCLE

    !<--- Now we have ensured that iFacet can be a facet element
    !<--- so we get the element in Cell mesh surrouding this node
    pt2elem = CellMeshData % NodetoElements( FacetNptrs( 1 ) )

    DO icell = 1, SIZE( pt2elem )
      elemNum = pt2elem( icell )

      !<--- if this element is not a boundary element then discard it
      IF( .NOT. CellMeshData % isBoundaryElement( elemNum ) ) CYCLE

      bndyData = CellMeshData % BoundaryElementData( elemNum )
      Elem => CellMesh % Elem( elemNum ) % Ptr
      FM = FacetMatrix( Elem % RefElem )
      CellNptrs = .Nptrs. Elem

      DO i = 1, SIZE( bndyData )
        CellFacetNptrs = &
        & CellNptrs( FM( BndyData( i ), 4 : FM( BndyData( i ), 3 ) + 3 ) )
        r = 0

        DO j = 1, SIZE( FacetNptrs )
          DO k = 1, SIZE( CellFacetNptrs )
            IF( FacetNptrs( j ) .EQ. CellFacetNptrs( k ) ) THEN
              r = r + 1
            END IF
          END DO
        END DO

        IF( r .EQ. SIZE( FacetNptrs ) ) THEN
          Found = .TRUE.
          CALL FacetElem % setPointerToCell( CellObj = Elem )
          CALL FacetElem % setFacetLocalID( BndyData( i ) )
          CALL FacetElem % setMaterialType( Elem % Mat_Type )
          EXIT
        END IF
      END DO
      IF( Found ) EXIT
    END DO
  END DO

  NULLIFY( FacetElem, Elem )
  IF( ALLOCATED( FacetNptrs ) ) DEALLOCATE( FacetNptrs )
  IF( ALLOCATED( pt2elem ) ) DEALLOCATE( pt2elem )
  IF( ALLOCATED( bndyData ) ) DEALLOCATE( bndyData )
  IF( ALLOCATED( FM ) ) DEALLOCATE( FM )
  IF( ALLOCATED( CellNptrs ) ) DEALLOCATE( CellNptrs )
  IF( ALLOCATED( CellFacetNptrs ) ) DEALLOCATE( CellFacetNptrs )

END PROCEDURE mc_connect_facet_cell

!----------------------------------------------------------------------------
!                                                           getFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE get_facet_elements
  INTEGER( I4B ) :: tbe
  INTEGER( I4B ) :: iel
  INTEGER( I4B ) :: iface, tface, lfid, order, elemType
  INTEGER( I4B ), ALLOCATABLE :: lnptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: fnptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: bData( : )
  INTEGER( I4B ), ALLOCATABLE :: pNptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: FM( :, : )
  CLASS( Element_ ), POINTER :: pElem
  CLASS( Element_ ), POINTER :: fElem
  CLASS( ReferenceElement_ ), POINTER :: RefElem_1
  CLASS( ReferenceElement_ ), POINTER :: RefElem_2

  pElem => NULL()
  fElem => NULL()
  RefElem_1 => NULL()
  RefElem_2 => NULL()

  ! Init mesh data
  IF( .not. mdobj % isInitiated ) THEN
    CALL mdobj % initiate( meshobj = obj )
  END IF

  ! init boundary data
  CALL mdobj % initiateBoundaryData( meshobj = obj )

  ! total boundary elements
  tbe = mdobj % totalBoundaryElements( )

  ! return if tbe if zero
  IF( tbe .EQ. 0 ) RETURN

  ! allocate memory for the facet mesh
  CALL facetMesh % initiate( nsd = obj % nsd, tElements = tbe )

  ! Now we need to make mesh of surface elements
  DO iel = 1, obj % tElements

    ! If parent element is not boundary element then cycle
    IF( .NOT. mdobj % isBoundaryElement( iel )  ) CYCLE

    ! Found boundary parent element
    ! gets its boundary data
    bData = mdobj % boundaryElementData( iel )
    tface = SIZE( bData )

    ! Generate facet matrix
    pElem => Obj % Elem( iel ) % Ptr
    FM = FacetMatrix( pElem % RefElem )
    pNptrs = .Nptrs. pElem

    DO iface = 1, tface
      ! local face id
      lfid = bData( iface )
      ! Facet elemType
      elemType = FM( lfid, 1 )
      ! Facet local nptrs
      lnptrs = FM( lfid, 4 : 3 + FM( lfid, 3 ) )
      ! get global facet nptrs
      fnptrs = pnptrs( lnptrs )
      ! get order of facet element
      order = elementOrder( elemType )

      !--- this is require one time only just to get refelem
      IF( isTriangle( elemType ) ) THEN
        RefElem_1 => ReferenceTriangle_Pointer( NSD = Obj % NSD )
        IF( Order .NE. 1 ) THEN
          RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
          DEALLOCATE( RefElem_1 )
        ELSE
          RefElem_2 => RefElem_1
        END IF
      ELSE IF( isQuadrangle( elemType ) ) THEN
        RefElem_1 => ReferenceQuadrangle_Pointer( NSD = Obj % NSD )
        IF( Order .NE. 1 ) THEN
          RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
          DEALLOCATE( RefElem_1 )
        ELSE
          RefElem_2 => RefElem_1
        END IF
      ELSE IF( isLine( elemType ) ) THEN
        RefElem_1 => ReferenceLine_Pointer( NSD = Obj % NSD )
        IF( Order .NE. 1 ) THEN
          RefElem_2 => RefElem_1 % LagrangeElement( Order = Order )
          DEALLOCATE( RefElem_1 )
        ELSE
          RefElem_2 => RefElem_1
        END IF
      END IF
      !--------------------------------

      fElem => getFEPointer( Obj = FEObj, Nptrs = fNptrs, &
        & Mat_Type = pElem % Mat_Type, RefElem = RefElem_2 )
      CALL fElem % setPointerToCell( pElem )

      CALL facetMesh % Append( Elem = fElem )
    END DO

  END DO

  NULLIFY( RefElem_1, RefElem_2, pElem, fElem )
  IF( ALLOCATED( lnptrs ) ) DEALLOCATE( lnptrs )
  IF( ALLOCATED( pnptrs ) ) DEALLOCATE( pnptrs )
  IF( ALLOCATED( fnptrs ) ) DEALLOCATE( fnptrs )
  IF( ALLOCATED( bData ) ) DEALLOCATE( bData )
  IF( ALLOCATED( FM ) ) DEALLOCATE( FM )

END PROCEDURE get_facet_elements

!----------------------------------------------------------------------------
!                                                                MeshQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE md_quality
  INTEGER( I4B ) :: tsize, ii
  REAL( DFP ), ALLOCATABLE :: xij(:,:)
  CLASS(ReferenceElement_), POINTER :: refelem

  refelem => meshobj%elem(1)%ptr%refelem
  tsize = SIZE(obj%elemToNode)
  ALLOCATE(Ans(tsize))

  DO ii = 1, tsize
    xij = nodes(:, obj%elemToNode(ii)%Val)
    Ans(ii) = ElementQuality(refelem=refelem, xij=xij, measure=measure)
  END DO

  DEALLOCATE(xij)
  NULLIFY(refelem)

END PROCEDURE md_quality

!----------------------------------------------------------------------------
!                                                                 FinElement
!----------------------------------------------------------------------------

MODULE PROCEDURE md_findelement
  INTEGER( I4B ) :: iel, telem, ips, nsd
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  REAL( DFP ), ALLOCATABLE :: xij(:,:)

  CLASS( Element_ ), POINTER :: Elem
  LOGICAL( LGT ) :: hasit

  IF( .NOT. ASSOCIATED( obj%refelem ) ) THEN
    CALL Display("ERROR:: MeshData_Class@Methods.f90")
    CALL Display("        md_findelement()")
    CALL Display("          Obj%refelem not associated")
    CALL Display( "         Program stoped")
    STOP
  END IF

  Ans = 0

  DO iel = 1, SIZE(obj%ElemToNode)
    Elem => MeshObj%Elem(iel)%ptr
    nsd = Elem%refelem%nsd
    nptrs = obj%elemToNode(iel)%val
    xij = nodes(1:nsd, nptrs)
    DO ips = 1, SIZE(coord, 2)
      hasit = containsPoint( Elem%refElem, xij, coord(:, ips) )
      IF( hasit) THEN
        Ans(ips) = iel
      END IF
    END DO
  END DO
END PROCEDURE md_findelement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
