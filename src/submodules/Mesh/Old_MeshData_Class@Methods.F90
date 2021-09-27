SUBMODULE( MeshData_Class ) Methods
  !! This module contains type bound procedures of [[MeshData_]]
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_Initiate
  ! Define internal variables
  INTEGER( I4B ) :: iel, Dummy
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  CLASS( Element_ ), POINTER :: Elem
  !
  obj%MaxNptrs = 0_I4B
  obj%isInitiated = .TRUE.

  ! find out max and min nptrs
  IF( .NOT. ALLOCATED( obj%ElemToNode ) ) THEN
    ALLOCATE( obj%ElemToNode( Meshobj%tElements ) )
  END IF

  Elem => Meshobj%Elem(1)%ptr
  obj% refelem => elem%refelem
  Elem => NULL( )

  DO iel = 1, Meshobj%tElements
    Elem => Meshobj%Elem( iel )%Ptr
    IF( .NOT. ASSOCIATED( Elem ) ) EXIT ! this is redundate
    Nptrs = .Nptrs. Elem
    obj%ElemToNode( iel )%Val = Nptrs
    Dummy = MAXVAL( Nptrs )
    IF( Dummy .GE. obj%MaxNptrs ) obj%MaxNptrs = Dummy
    Dummy = MINVAL( Nptrs )
    IF( iel .eq. 1 ) obj%MinNptrs = Dummy
    IF( Dummy .LE. obj%MinNptrs ) obj%MinNptrs = Dummy
  END DO
  !
  IF( ALLOCATED( obj%Local_Nptrs ) ) DEALLOCATE( obj%Local_Nptrs )
  ALLOCATE( obj%Local_Nptrs( obj%MaxNptrs ) )
  obj%Local_Nptrs = 0
  !
  DO iel =1, Meshobj%tElements
    Elem => Meshobj%Elem( iel )%Ptr
    IF( ASSOCIATED( Elem ) ) THEN
      Nptrs = .Nptrs. Elem
      obj%Local_Nptrs( Nptrs ) = Nptrs
    END IF
  END DO
  !
  Dummy = COUNT( obj%Local_Nptrs .NE. 0 )
  IF( ALLOCATED( obj%Nptrs ) ) DEALLOCATE( obj%Nptrs )
  ALLOCATE( obj%Nptrs( Dummy ) )
  !
  Dummy = 0
  DO iel = 1, obj%MaxNptrs
    IF( obj%Local_Nptrs( iel ) .EQ. 0 ) CYCLE
    Dummy = Dummy + 1
    obj%Nptrs( Dummy ) = obj%Local_Nptrs( iel )
    obj%Local_Nptrs( iel ) = Dummy
  END DO
  !
  obj%tNodes = SIZE( obj%Nptrs )
  DEALLOCATE( Nptrs )
  NULLIFY( Elem )
END PROCEDURE meshData_Initiate

!----------------------------------------------------------------------------
!                                                                 MeshData
!----------------------------------------------------------------------------

MODULE PROCEDURE meshdata_1
  CALL ans%Initiate( Meshobj )
END PROCEDURE meshdata_1

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_DeallocateData
  IF( ALLOCATED( obj%NodeToElem ) ) DEALLOCATE( obj%NodeToElem )
  IF( ALLOCATED( obj%ElemToElem ) ) DEALLOCATE( obj%ElemToElem )
  IF( ALLOCATED( obj%NTN ) ) DEALLOCATE( obj%NTN )
  IF( ALLOCATED( obj%ElemToNode ) ) DEALLOCATE( obj%ElemToNode )
  IF( ALLOCATED( obj%BoundaryData ) ) DEALLOCATE( obj%BoundaryData )
  IF( ALLOCATED( obj%InternalBndyElemNum ) ) &
    & DEALLOCATE( obj%InternalBndyElemNum )
  IF( ALLOCATED( obj%InternalBoundaryData ) ) &
    & DEALLOCATE( obj%InternalBoundaryData )
  IF( ALLOCATED( obj%LBndyIndex ) ) DEALLOCATE( obj%LBndyIndex )
  IF( ALLOCATED( obj%Nptrs ) ) DEALLOCATE( obj%Nptrs )
  IF( ALLOCATED( obj%BoundaryNptrs ) ) DEALLOCATE( obj%BoundaryNptrs )
  IF( ALLOCATED( obj%InternalNptrs ) ) DEALLOCATE( obj%InternalNptrs )
  IF( ALLOCATED( obj%Local_Nptrs ) ) DEALLOCATE( obj%Local_Nptrs )
  !
  obj%refelem => NULL()
  obj%isInitiated = .FALSE.
  obj%tNodes = 0_I4B
  obj%MaxNptrs = 0_I4B
  obj%MinNptrs = 0_I4B
END PROCEDURE meshData_DeallocateData

!----------------------------------------------------------------------------
!                                                            getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_total_nodes
    ans = obj%tNodes
END PROCEDURE get_total_nodes

!----------------------------------------------------------------------------
!                                                      getTotalBoundaryNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tbndy_nodes
  IF( ALLOCATED( obj%BoundaryNptrs )  ) THEN
    ans = SIZE( obj%BoundaryNptrs )
  ELSE
    ans = 0
  END IF
END PROCEDURE get_tbndy_nodes

!----------------------------------------------------------------------------
!                                                      getTotalInternalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tint_nodes
  IF( ALLOCATED( obj%InternalNptrs )  ) THEN
    ans = SIZE( obj%InternalNptrs )
  ELSE
    ans = 0
  END IF
END PROCEDURE get_tint_nodes

!----------------------------------------------------------------------------
!                                                       TotalBoundaryElements
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tbndy_elems
  IF( ALLOCATED( obj%BoundaryData ) ) THEN
    ans = SIZE( obj%BoundaryData )
  ELSE
    ans = 0
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

    DO ii = 1, obj%tNodes
      lim( 1 ) = MIN( lim( 1 ), nodes( 1, obj%Nptrs( ii  ) ) )
      lim( 2 ) = MAX( lim( 2 ), nodes( 1, obj%Nptrs( ii  ) ) )
    END DO

  CASE( 2 )

    DO ii = 1, obj%tNodes
      lim( 1 ) = MIN( lim( 1 ), nodes( 1, obj%Nptrs( ii  ) ) )
      lim( 2 ) = MAX( lim( 2 ), nodes( 1, obj%Nptrs( ii  ) ) )
      lim( 3 ) = MIN( lim( 3 ), nodes( 2, obj%Nptrs( ii  ) ) )
      lim( 4 ) = MAX( lim( 4 ), nodes( 2, obj%Nptrs( ii  ) ) )
    END DO

  CASE( 3 )

    DO ii = 1, obj%tNodes
      lim( 1 ) = MIN( lim( 1 ), nodes( 1, obj%Nptrs( ii  ) ) )
      lim( 2 ) = MAX( lim( 2 ), nodes( 1, obj%Nptrs( ii  ) ) )
      lim( 3 ) = MIN( lim( 3 ), nodes( 2, obj%Nptrs( ii  ) ) )
      lim( 4 ) = MAX( lim( 4 ), nodes( 2, obj%Nptrs( ii  ) ) )
      lim( 5 ) = MIN( lim( 5 ), nodes( 3, obj%Nptrs( ii  ) ) )
      lim( 6 ) = MAX( lim( 6 ), nodes( 3, obj%Nptrs( ii  ) ) )
    END DO

  END SELECT

  CALL Initiate( obj = ans, nsd = nsd, lim = lim )

END PROCEDURE get_Bbox

!----------------------------------------------------------------------------
!                                                              getLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE local_from_global
  ! Define intent of dummy variables
  INTEGER( I4B ) :: i, n
  !
  n = SIZE( GlobalIndx )
  ALLOCATE( ans( n ) )
  DO i =1, n
    IF( GlobalIndx( i ) .LT. obj %MinNptrs &
        & .OR. GlobalIndx( i ) .GT. obj%MaxNptrs ) THEN
      ans( i ) = 0
    ELSE
      ans( i ) = obj%Local_Nptrs( GlobalIndx( i ) )
    END IF
  END DO

END PROCEDURE local_from_global

!----------------------------------------------------------------------------
!                                                              getLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE local_from_global_scalar
  IF( GlobalIndx .LT. obj %MinNptrs .OR. GlobalIndx .GT. obj%MaxNptrs ) THEN
    ans = 0
  ELSE
    ans = obj%Local_Nptrs( GlobalIndx )
  END IF
END PROCEDURE local_from_global_scalar

!----------------------------------------------------------------------------
!                                                                GlobalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE global_from_local
  ! define internal variables
  INTEGER( I4B ) :: i, n

  n = SIZE( LocalIndx )
  ALLOCATE( ans( n ) ); ans = 0

  DO i = 1, n
    IF( LocalIndx( i ) .GT. obj%tNodes ) THEN
      ans( i ) = 0
    ELSE
      ans( i ) = obj%Nptrs( LocalIndx( i ) )
    END IF
  END DO

END PROCEDURE global_from_local

!----------------------------------------------------------------------------
!                                                             getGlobalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE global_from_local_scalar
  IF( LocalIndx .LE. obj%tNodes ) THEN
    ans = obj%Nptrs( LocalIndx )
  ELSE
    ans = 0
  END IF
END PROCEDURE global_from_local_scalar

!----------------------------------------------------------------------------
!                                                              isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE is_node_present
  IF( Nptrs .GT. obj%MaxNptrs &
    & .OR. Nptrs .LT. obj%MinNptrs &
    & .OR. obj%Local_Nptrs( Nptrs ) .EQ. 0 ) THEN
    ans = .FALSE.
  ELSE
    ans = .TRUE.
  END IF
END PROCEDURE is_node_present

!----------------------------------------------------------------------------
!                                                     isNodeToNodesInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_node_nodes_initiated
  IF( ALLOCATED( obj%NTN ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_node_nodes_initiated

!----------------------------------------------------------------------------
!                                                  isNodeToElementsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_node_elements_initiated
  IF( ALLOCATED( obj%NodeToElem ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_node_elements_initiated

!----------------------------------------------------------------------------
!                                               isElementToElementsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_element_elements_initiated
  IF( ALLOCATED( obj%ElemToElem ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_element_elements_initiated


!----------------------------------------------------------------------------
!                                                  isElementToNodesInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_element_nodes_initiated
  IF( ALLOCATED( obj%ElemToNode ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_element_nodes_initiated

!----------------------------------------------------------------------------
!                                                    isBoundaryDataInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_boundarydata
  IF( ALLOCATED( obj%BoundaryData ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_boundarydata

!----------------------------------------------------------------------------
!                                                   isInternalNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_internalnptrs
  IF( ALLOCATED( obj%InternalNptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_internalnptrs

!----------------------------------------------------------------------------
!                                                   isBoundaryNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_bndy_nptrs
  IF( ALLOCATED( obj%BoundaryNptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_bndy_nptrs

!----------------------------------------------------------------------------
!                                                      isLocalNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_local_nptrs
  IF( ALLOCATED( obj%Local_Nptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_local_nptrs

!----------------------------------------------------------------------------
!                                            isInternalBoundaryDataInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE is_int_bndy_data
  IF( ALLOCATED( obj% InternalBoundaryData) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
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

  IF( .NOT. ALLOCATED( obj%NodeToElem )  ) THEN
    ALLOCATE( obj%NodeToElem( obj%tNodes ) )
    DO iel = 1, Meshobj%tElements
      Elem => Meshobj%Elem( iel )%Ptr
      IF( .NOT. ASSOCIATED( Elem ) ) CYCLE
      local_nptrs = obj%LocalNptrs( .Nptrs. Elem )
      DO iNode = 1, SIZE( local_nptrs )
        CALL Append( obj%NodeToElem ( local_nptrs( iNode ) ), iel )
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
  ans = obj%NodeToElem( obj%LocalNptrs( GlobalIndx = GlobalPt ) )%Val
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
  IF( .NOT. ALLOCATED( obj%NTN ) ) THEN
    ALLOCATE( obj%NTN( obj%tNodes ) )
    CALL obj%InitiateNodeToElements( Meshobj )
    DO iLocalNode = 1, obj%tNodes
      tSize = SIZE ( obj%NodeToElem( iLocalNode ) )
      IF( tSize .EQ. 0  ) CYCLE
      iGlobalNode = obj%GlobalNptrs( LocalIndx = iLocalNode )
      NearElements = obj%NodeToElements( GlobalPt = iGlobalNode )
      DO iel = 1, SIZE( NearElements )
        Elem => Meshobj%Elem( NearElements( iel ) )%Ptr
        global_nptrs = .Nptrs. Elem
        global_nptrs = PACK( global_nptrs, global_nptrs .NE. iGlobalNode )
        CALL Append( obj%NTN( iLocalNode ), global_nptrs )
      END DO
      CALL RemoveDuplicates( obj%NTN( iLocalNode ) )
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

  i = obj%LocalNptrs( GlobalIndx = GlobalNode )
  IF( IncludeSelf ) THEN
    Nptrs = obj%NTN( i )%Val
    i = SIZE( Nptrs )
    ALLOCATE( ans( i + 1 ) )
    ans( 1 ) = GlobalNode
    ans( 2 : ) = Nptrs
    DEALLOCATE( Nptrs )
  ELSE
    ans = obj%NTN( i )%Val
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

  IF( .NOT. ALLOCATED( obj%ElemToElem ) ) THEN
    CALL obj%InitiateNodeToElements( Meshobj )
    ALLOCATE( obj%ElemToElem( Meshobj%tElements ) )
    DO iel1 = 1, Meshobj%tElements
      Elem => Meshobj%Elem( iel1 )%Ptr
      global_nptrs1 = .Nptrs. Elem ! get nptrs
      FM1 = FacetMatrix( Elem%refelem ) ! get facet matrix
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
        n2e1 = obj%NodeToElements( GlobalPt = pt1 )
        DO iel2 = 1, SIZE( n2e1 )
          IF( iel1 .EQ. n2e1( iel2 ) ) CYCLE

          Elem2 => Meshobj%Elem( n2e1( iel2 ) )%Ptr
          global_nptrs2 = .Nptrs. Elem2
          FM2 = FacetMatrix( Elem2%refelem )
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
              CALL APPEND( obj%ElemToElem( iel1 ), &
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

  Nptrs = obj%ElemToElem( iel )%Val
  tSize = SIZE( Nptrs ) / 3
  ans = TRansPOSE( RESHAPE( Nptrs, [3, tSize] ) )
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
      ans = obj%ElementToElements( iel = iel( 1 ) )
    CASE DEFAULT
      Nptrs = obj%ElemToElem( iel( 1 ) )%Val
      ALLOCATE( ans( SIZE( Nptrs )/3, 1 ) )
      ans( :, 1 ) = Nptrs( 1::3 )
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
  IF( .NOT. ALLOCATED( obj%ElemToNode ) ) THEN
    ALLOCATE( obj%ElemToNode( Meshobj%tElements ) )
    DO iel = 1, Meshobj%tElements
      Elem => Meshobj%Elem( iel )%Ptr
      obj%ElemToNode( iel )%Val = .Nptrs. Elem
    END DO
    NULLIFY( Elem )
  END IF
END PROCEDURE init_elem_nodes

!----------------------------------------------------------------------------
!                                                            ElementToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_elem_nodes
  ans = obj%ElemToNode( iel )%Val
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

  IF( .NOT. ALLOCATED( obj%BoundaryData ) ) THEN
    tElements = Meshobj%tElements
    CALL Reallocate( obj%LBndyIndex,  tElements )
    obj%LBndyIndex = 0_I4B
    IF( ALLOCATED( obj%BoundaryNptrs ) ) DEALLOCATE( obj%BoundaryNptrs )
    SELECT CASE( tElements )
    CASE( 1 )
      ALLOCATE( obj%BoundaryData( 1 ) )
      obj%LBndyIndex( 1 ) = 1
      Elem => Meshobj%Elem( 1 )%Ptr
      obj%BoundaryNptrs = .Nptrs. Elem
      FM = FacetMatrix( Elem%refelem )
      tFace = SIZE( FM, 1 )
      CALL Initiate( obj%BoundaryData( 1 ), tFace + 1 )
      obj%BoundaryData( 1 )%Val( 1 ) = 1
      obj%BoundaryData( 1 )%Val( 2: ) = [(i, i=1, tFace)]
    CASE DEFAULT
      CALL obj%InitiateElementToElements( Meshobj = Meshobj )
      DO iel = 1, tElements
        dummy = SIZE( obj%ElemToElem( iel ) )
        IF( dummy .EQ. 0 ) CYCLE
        e2eData = obj%ElemToElem( iel )%Val
        te2e = SIZE( e2eData ) / 3
        Elem => Meshobj%Elem( iel )%Ptr
        FM = FacetMatrix( Elem%refelem )
        tFace = SIZE( FM, 1 )
        IF( tFace .NE. te2e ) THEN
          obj%LBndyIndex( iel ) = 1
        END IF
      END DO
      tBndyElem = COUNT( obj%LBndyIndex .NE. 0 )
      ALLOCATE( obj%BoundaryData( tBndyElem ) )
      ALLOCATE( DummyNptrs( obj%MaxNptrs ) )
      DummyNptrs = 0; k = 0
      DO iel = 1, tElements
        IF( obj%LBndyIndex( iel ) .EQ. 0 ) CYCLE
        k = k + 1
        obj%LBndyIndex( iel ) = k
        e2eData = obj%ElemToElem( iel )%Val
        te2e = SIZE( e2eData ) / 3
        IF( ALLOCATED( FaceVec ) ) DEALLOCATE( FaceVec )
        ALLOCATE( FaceVec( te2e ) )
        DO i = 1, te2e
          FaceVec( i ) = e2eData( 3*(i-1) + 2 )
        END DO
        Elem => Meshobj%Elem( iel )%Ptr
        FM = FacetMatrix( Elem%refelem )
        tFace = SIZE( FM, 1 )
        tBndyFace = tFace - te2e
        CALL Initiate( obj%BoundaryData( k ), tBndyFace + 1 )
        obj%BoundaryData( k )%Val( 1 ) = iel
        global_nptrs = .Nptrs. Elem
        j = 0
        DO i = 1, tFace
          IF( ANY( i .EQ. FaceVec ) ) CYCLE
          j = j + 1
          obj%BoundaryData( k )%Val( 1 + j ) = i
          ! get local_nptrs of the face
          b = 3 + FM( i, 3 )
          local_nptrs = FM( i, 4 : b )
          DummyNptrs( global_nptrs( local_nptrs ) ) = &
            & global_nptrs( local_nptrs )
        END DO
      END DO
      b = COUNT( DummyNptrs .NE. 0 )
      ALLOCATE( obj%BoundaryNptrs( b ) )
      k = 0
      DO i = 1, obj%MaxNptrs
        IF( DummyNptrs( i ) .EQ. 0 ) CYCLE
        k = k + 1
        obj%BoundaryNptrs( k ) = DummyNptrs( i )
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
  IF( obj%LBndyIndex( iel ) .NE. 0 ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE is_bndy_elem

!----------------------------------------------------------------------------
!                                                        BoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE get_bndy_elem
  ! Define internal variables
  INTEGER( I4B ) :: LocalIndx

  LocalIndx = obj%LBndyIndex( iel )
  IF( LocalIndx .NE. 0 ) THEN
    ans = obj%BoundaryData( LocalIndx )%Val( 2: )
  ELSE
    ans = [0]
  END IF
END PROCEDURE get_bndy_elem

!----------------------------------------------------------------------------
!                                                      InitiateInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE init_int_nptrs
  ! Define internal variables
  INTEGER( I4B ) :: iel, tElements, tNodes, iNode
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : ), DummyNptrs( : )

  IF( .NOT.  ALLOCATED( obj%InternalNptrs ) ) THEN
    CALL obj%InitiateBoundaryData( Meshobj = Meshobj )
    CALL obj%InitiateElementToNodes( Meshobj = Meshobj )
    tElements = Meshobj%tElements
    tNodes = obj%maxNptrs
    ALLOCATE( DummyNptrs( tNodes ) )
    DummyNptrs = 0

    DO iel = 1, tElements
      IF( obj%isBoundaryElement( iel = iel ) ) CYCLE
      Nptrs = obj%ElemToNode( iel )%Val
      DummyNptrs( Nptrs ) = Nptrs
    END DO

    iNode = COUNT( DummyNptrs .NE. 0 )
    ALLOCATE( obj%InternalNptrs( iNode ) )

    iel = 0
    DO iNode = 1, tNodes
        IF( DummyNptrs( iNode ) .EQ. 0 ) CYCLE
        iel = iel + 1
        obj%InternalNptrs( iel ) = DummyNptrs( iNode )
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
    IF( .NOT. obj%isInitiated ) THEN
      CALL obj%Initiate( Meshobj = Meshobj )
    END IF
    CALL obj%InitiateNodeToNodes( Meshobj = Meshobj )
    DO i = 1, obj%tNodes
      j = obj%GlobalNptrs( LocalIndx = i )
      IF( map( j ) .EQ. 0 ) CYCLE
      n2n = map( obj%NodeToNodes( GlobalNode = j, IncludeSelf =.true. ) )
      CALL setSparsity( obj = Mat, Row = map(j), Col = n2n )
    END DO
    IF( ALLOCATED( n2n ) ) DEALLOCATE( n2n )

  ELSE

    IF( .NOT. obj%isInitiated ) THEN
      CALL obj%Initiate( Meshobj = Meshobj )
    END IF
    CALL obj%InitiateNodeToNodes( Meshobj = Meshobj )
    DO i = 1, obj%tNodes
      j = obj%GlobalNptrs( LocalIndx = i )
      ! IF( j .eq. 0 ) CYCLE !<--- by construction this will never happen
      n2n = obj%NodeToNodes( GlobalNode = j, IncludeSelf =.true. )
      CALL setSparsity( obj = Mat, Row = j, Col = n2n )
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
  IF( .NOT. CellMeshData%isInitiated ) THEN
    CALL CellMeshData%Initiate( CellMesh )
  END IF

  !<--- generate node to elements and boundary data
  CALL CellMeshData%InitiateNodeToElements( CellMesh )
  CALL CellMeshData%InitiateBoundaryData( CellMesh )

  Elem => NULL( ); FacetElem => NULL( )

  DO ifacet = 1, FacetMesh%SIZE()
    found = .false.

    !<--- get facet element and its nptrs
    FacetElem => FacetMesh%Elem( ifacet )%Ptr
    FacetNptrs = .Nptrs. FacetElem

    !<--- select an arbitrary node in FacetNptr; if this is not
    !<--- present inside the CellMesh then this means this iFacet is
    !<--- orphan and we go back to the next Facet Element
    IF( .NOT. CellMeshData%isNodePresent( FacetNptrs( 1 ) ) ) CYCLE

    !<--- Now we have ensured that iFacet can be a facet element
    !<--- so we get the element in Cell mesh surrouding this node
    pt2elem = CellMeshData%NodetoElements( FacetNptrs( 1 ) )

    DO icell = 1, SIZE( pt2elem )
      elemNum = pt2elem( icell )

      !<--- if this element is not a boundary element then discard it
      IF( .NOT. CellMeshData%isBoundaryElement( elemNum ) ) CYCLE

      bndyData = CellMeshData%BoundaryElementData( elemNum )
      Elem => CellMesh%Elem( elemNum )%Ptr
      FM = FacetMatrix( Elem%refelem )
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
          CALL FacetElem%setPointerToCell( Cellobj = Elem )
          CALL FacetElem%setFacetLocalID( BndyData( i ) )
          CALL FacetElem%setMaterialType( Elem%Mat_Type )
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
  CLASS( ReferenceElement_ ), POINTER :: refelem_1
  CLASS( ReferenceElement_ ), POINTER :: refelem_2

  pElem => NULL()
  fElem => NULL()
  refelem_1 => NULL()
  refelem_2 => NULL()

  ! Init mesh data
  IF( .not. mdobj%isInitiated ) THEN
    CALL mdobj%initiate( meshobj = obj )
  END IF

  ! init boundary data
  CALL mdobj%initiateBoundaryData( meshobj = obj )

  ! total boundary elements
  tbe = mdobj%totalBoundaryElements( )

  ! return if tbe if zero
  IF( tbe .EQ. 0 ) RETURN

  ! allocate memory for the facet mesh
  CALL facetMesh%initiate( nsd = obj%nsd, tElements = tbe )

  ! Now we need to make mesh of surface elements
  DO iel = 1, obj%tElements

    ! If parent element is not boundary element then cycle
    IF( .NOT. mdobj%isBoundaryElement( iel )  ) CYCLE

    ! Found boundary parent element
    ! gets its boundary data
    bData = mdobj%boundaryElementData( iel )
    tface = SIZE( bData )

    ! Generate facet matrix
    pElem => obj%Elem( iel )%Ptr
    FM = FacetMatrix( pElem%refelem )
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
        refelem_1 => ReferenceTriangle_Pointer( NSD = obj%NSD )
        IF( Order .NE. 1 ) THEN
          refelem_2 => refelem_1%LagrangeElement( Order = Order )
          DEALLOCATE( refelem_1 )
        ELSE
          refelem_2 => refelem_1
        END IF
      ELSE IF( isQuadrangle( elemType ) ) THEN
        refelem_1 => ReferenceQuadrangle_Pointer( NSD = obj%NSD )
        IF( Order .NE. 1 ) THEN
          refelem_2 => refelem_1%LagrangeElement( Order = Order )
          DEALLOCATE( refelem_1 )
        ELSE
          refelem_2 => refelem_1
        END IF
      ELSE IF( isLine( elemType ) ) THEN
        refelem_1 => ReferenceLine_Pointer( NSD = obj%NSD )
        IF( Order .NE. 1 ) THEN
          refelem_2 => refelem_1%LagrangeElement( Order = Order )
          DEALLOCATE( refelem_1 )
        ELSE
          refelem_2 => refelem_1
        END IF
      END IF
      !--------------------------------

      fElem => getFEPointer( obj = FEobj, Nptrs = fNptrs, &
        & Mat_Type = pElem%Mat_Type, refelem = refelem_2 )
      CALL fElem%setPointerToCell( pElem )

      CALL facetMesh%Append( Elem = fElem )
    END DO

  END DO

  NULLIFY( refelem_1, refelem_2, pElem, fElem )
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
  ALLOCATE(ans(tsize))

  DO ii = 1, tsize
    xij = nodes(:, obj%elemToNode(ii)%Val)
    ans(ii) = ElementQuality(refelem=refelem, xij=xij, measure=measure)
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
    CALL Display("ERROR:: MeshData_Class@Methods.F90")
    CALL Display("        md_findelement()")
    CALL Display("          obj%refelem not associated")
    CALL Display( "         Program stoped")
    STOP
  END IF

  ans = 0

  DO iel = 1, SIZE(obj%ElemToNode)
    Elem => Meshobj%Elem(iel)%ptr
    nsd = Elem%refelem%nsd
    nptrs = obj%elemToNode(iel)%val
    xij = nodes(1:nsd, nptrs)
    DO ips = 1, SIZE(coord, 2)
      hasit = containsPoint( Elem%refelem, xij, coord(:, ips) )
      IF( hasit) THEN
        ans(ips) = iel
      END IF
    END DO
  END DO
END PROCEDURE md_findelement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
