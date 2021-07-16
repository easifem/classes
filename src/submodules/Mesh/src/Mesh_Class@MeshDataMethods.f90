! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE( Mesh_Class ) MeshDataMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         InitiateLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateLocalNptrs
  INTEGER( I4B ) :: ii, dummy
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_InitiateLocalNptrs"

  !> check
  IF( .NOT. ALLOCATED( obj%connectivity ) ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Connectivity matrix is not allocated.")
  END IF

  obj%maxNptrs = MAXVAL(obj%connectivity)
  obj%minNptrs = MINVAL(obj%connectivity)

  !> check
  IF( obj%maxNptrs .EQ. 0 ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Largest node number (maxNptrs) cannot be zero.")
  END IF

  CALL Reallocate( obj%Local_Nptrs, obj%maxNptrs )
  DO CONCURRENT ( ii =1:obj%tElements )
    obj%Local_Nptrs( obj%connectivity(:, ii) ) = obj%connectivity(:, ii)
    !! Here we are allowing race condition
    !! Because the RHS is unique for a given ii
  END DO

  obj%tNodes = COUNT( obj%Local_Nptrs .NE. 0 )

  CALL Reallocate( obj%nptrs, obj%tNodes, 2 )

  dummy = 0
  DO ii = 1, obj%maxNptrs
    IF( obj%Local_Nptrs( ii ) .NE. 0 ) THEN
      dummy = dummy + 1
      obj%nptrs( dummy, 1 ) = obj%Local_Nptrs( ii )
      obj%nptrs( dummy, 2 ) = dummy
      obj%Local_Nptrs( ii ) = dummy
    END IF
  END DO
END PROCEDURE mesh_InitiateLocalNptrs

!----------------------------------------------------------------------------
!                                               InitiateLocalElementNumbers
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateLocalElementNumbers
  INTEGER( I4B ) :: ii
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateLocalElementNumbers"

  !> check
  IF( .NOT. obj%isInitiated ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  !> check
  IF( obj%isLocalElementNumbersInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Local element numbers are already initiated.")
  ELSE
    obj%maxElemNum = MAXVAL(obj%ElemNumber)
    obj%minElemNum = MINVAL(obj%ElemNumber)
    CALL Reallocate( obj%local_elemNumber, obj%maxElemNum )
    DO CONCURRENT (ii = 1:obj%tElements)
      obj%local_elemNumber( obj%elemNumber( ii ) ) = ii
    END DO
  END IF
END PROCEDURE mesh_InitiateLocalElementNumbers

!----------------------------------------------------------------------------
!                                                    InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateNodeToElements
  ! Define internal  variables
  INTEGER( I4B ) :: ii, iNode
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateNodeToElements"

  !> check
  IF( .NOT. obj%isInitiated ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  !> check
  IF( obj%isNodeToElementsInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Node to Elements mapping is already initiated.")
  ELSE
    ALLOCATE( obj%NodeToElem( obj%tNodes ) )
    DO ii = 1, obj%tElements
      local_nptrs = obj%getLocalNptrs( obj%Connectivity( :, ii ) )
      DO iNode = 1, SIZE( local_nptrs )
        CALL Append( obj%NodeToElem ( local_nptrs( iNode ) ), ii )
      END DO
    END DO
  END IF

  IF( ALLOCATED( local_nptrs ) ) DEALLOCATE( local_nptrs )
END PROCEDURE mesh_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                   InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateNodetoNodes
  ! Define internal  variables
  INTEGER( I4B ) :: iel, iLocalNode, tSize, iGlobalNode
  INTEGER( I4B ), ALLOCATABLE ::  global_nptrs( : ), NearElements( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateNodetoNodes"

  !> check
  IF( .NOT. obj%isInitiated ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  !> check
  IF( obj%isNodeToNodesInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Node to Nodes mapping is already initiated.")
  ELSE
    IF( .NOT. obj%isNodeToElementsInitiated() ) &
      & CALL obj%InitiateNodeToElements( )

    ALLOCATE( obj%NTN( obj%tNodes ) )

    DO iLocalNode = 1, obj%tNodes
      iGlobalNode = obj%getGlobalNptrs( LocalNode = iLocalNode )
      IF( iGlobalNode .EQ. 0  ) CYCLE
      NearElements = obj%getNodeToElements( GlobalNode = iGlobalNode )
      tSize = SIZE ( NearElements )
      DO iel = 1, tSize
        global_nptrs = obj%getConnectivity( iel=NearElements(iel) )
        global_nptrs = PACK( global_nptrs, global_nptrs .NE. iGlobalNode )
        CALL Append( obj%NTN( iLocalNode ), global_nptrs )
      END DO
      CALL RemoveDuplicates( obj%NTN( iLocalNode ) )
    END DO
  END IF

  IF( ALLOCATED( global_nptrs ) ) DEALLOCATE( global_nptrs )
  IF( ALLOCATED( NearElements ) ) DEALLOCATE( NearElements )
END PROCEDURE mesh_InitiateNodetoNodes

!----------------------------------------------------------------------------
!                                                InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateElementToElements
  ! Define internal  variables
  INTEGER( I4B ) :: i, j, r,  iel1, tFace, iFace1, NNS1, pt1, &
    & iel2, iFace2, NNS2
  INTEGER( I4B ), ALLOCATABLE :: global_nptrs1( : ),   &
    & global_nptrsFace1( : ), n2e1( : ), global_nptrs2( : ), &
    & global_nptrsFace2( : )
  LOGICAL( LGT ) :: Found
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateElementToElements"

  IF( .NOT. obj%isInitiated ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  IF( .NOT. ASSOCIATED( obj%refelem ) ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Unable to identify the Reference element of the mesh, may be it is not set" )
  END IF

  IF( .NOT. ALLOCATED( obj%FacetElements ) ) THEN
    obj%FacetElements = FacetElements( obj%refelem )
  END IF

  tFace = SIZE(obj%FacetElements)
    !! Total number of facet elements

  IF( obj%isElementToElementsInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Element to Elements mapping is already initiated." )
  ELSE

    IF( .NOT. obj%isNodeToElementsInitiated() ) THEN
      CALL obj%InitiateNodeToElements()
    END IF

    ALLOCATE( obj%ElemToElem( obj%tElements ) )

    DO iel1 = 1, obj%tElements
      global_nptrs1 = obj%getConnectivity(iel1)
        !! getting node numbers of element iel1

      DO iFace1 = 1, tFace
        FOUND = .FALSE.

        global_nptrsFace1 = global_nptrs1( getConnectivity( obj%FacetElements( iFace1) ) )
        !! getting global node number in face iFace1
        NNS1 = SIZE( global_nptrsFace1 )
        !! number of nodes in iFace1

        pt1 = global_nptrsFace1( 1 )
        !! select a point on facet iFace1

        n2e1 = obj%getNodeToElements( GlobalNode = pt1 )
        !! get elements connected to the node pt1

        DO iel2 = 1, SIZE( n2e1 )
          IF( iel1 .EQ. n2e1( iel2 ) ) CYCLE

          global_nptrs2 = obj%getConnectivity( n2e1( iel2 ) )

          DO iFace2 = 1, tFace
            !! getting total number of nodes in iFace2
            global_nptrsFace2 = global_nptrs2( getConnectivity( obj%FacetElements(iFace2)) )
            NNS2 =SIZE(global_nptrsFace2)
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
          IF( FOUND ) EXIT
        END DO
      END DO
    END DO
    IF( ALLOCATED( global_nptrs1 ) ) DEALLOCATE( global_nptrs1 )
    IF( ALLOCATED( global_nptrs2 ) ) DEALLOCATE( global_nptrs2 )
    IF( ALLOCATED( global_nptrsFace1 ) ) DEALLOCATE( global_nptrsFace1 )
    IF( ALLOCATED( global_nptrsFace2 ) ) DEALLOCATE( global_nptrsFace2 )
    IF( ALLOCATED( n2e1 ) ) DEALLOCATE( n2e1 )
  END IF
END PROCEDURE mesh_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateBoundaryData
  ! Define internal variables
  INTEGER( I4B ) :: iel, tFace, i, j, k, DummyNptrs( obj%MaxNptrs )
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : ), &
    & global_nptrs( : ), ElemToElem( :, : )
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_InitiateBoundaryData"

  !> Check
  IF( .NOT. obj%isInitiated ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  !> Check
  IF( obj%isBoundaryDataInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Boundary data is already initiated." )
  END IF

  !> Check
  IF( obj%isBoundaryNptrsInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Boundary Nptrs are already initiated." )
  END IF

  !> Check
  IF( .NOT. obj%isElementToElementsInitiated() ) THEN
    CALL obj%InitiateElementToElements( )
  END IF

  !> Check
  IF( .NOT. ALLOCATED( obj%FacetElements ) ) THEN
    obj%FacetElements = FacetElements( obj%refelem )
  END IF
  tFace = SIZE( obj%FacetElements )

  !> Allocate
  CALL Reallocate( obj%LBndyIndex, obj%tElements )

  !> Case of single element in the mesh
  IF( obj%tElements .EQ. 1 ) THEN
    ALLOCATE( obj%BoundaryData( 1 ) )
    obj%LBndyIndex( 1 ) = 1
    obj%BoundaryNptrs = obj%getConnectivity(iel=1)
    tFace = SIZE( obj%FacetElements )
    CALL Initiate( obj%BoundaryData( 1 ), tFace + 1 )
    obj%BoundaryData( 1 ) = IntVector(H_CONCAT( [1], [(i, i=1, tFace)] ))
  ELSE

    DummyNptrs = 0
    k = 0
    DO iel = 1, obj%tElements
      IF( INT( SIZE( obj%ElemToElem( iel ) ) / 3 ) .EQ. SIZE( obj%FacetElements ) ) CYCLE
      !! Contiue only if element iel is a boundary element
      k = k + 1
      obj%LBndyIndex( iel ) = k
      ElemToElem = obj%getElementToElements( iel )
      global_nptrs = obj%getConnectivity(iel)
      j = 0
      DO i = 1, tFace
        IF( ANY( i .EQ. ElemToElem( :, 2 ) ) ) CYCLE
        j = j + 1
        ! get local_nptrs of the face
        local_nptrs = getConnectivity( obj%FacetElements( i ) )
        DummyNptrs( global_nptrs( local_nptrs ) ) = &
        & global_nptrs( local_nptrs )
      END DO
    END DO

    !> get boundary node numbers
    ALLOCATE( obj%BoundaryNptrs( COUNT( DummyNptrs .NE. 0 ) ) )
    j = 0
    DO i = 1, obj%MaxNptrs
      IF( DummyNptrs( i ) .EQ. 0 ) CYCLE
      j = j + 1
      obj%BoundaryNptrs( j ) = DummyNptrs( i )
    END DO

    !> store inforamtion of boundary nodes in second column of
    ! nptrs. For boundary nodes second column of nptrs is negative
    ! of the local node number
    FORALL (i=1:SIZE( obj%BoundaryNptrs ))
      ! j = obj%BoundaryNptrs( i )
      obj%Nptrs( obj%local_nptrs( obj%BoundaryNptrs( i ) ), 2 ) = - obj%local_nptrs( obj%BoundaryNptrs( i ) )
    END FORALL

    ! Now we will include those elements in boundary elements
    ! which contains the boundary nodes
    DO iel = 1, obj%tElements
      global_nptrs = obj%getConnectivity( iel )
      DO i = 1, SIZE( global_nptrs )
        j = obj%getLocalNptrs( global_nptrs( i ) )
        IF( obj%Nptrs( j, 2 ) .LT. 0 ) THEN
          obj%LBndyIndex( iel ) = 1
          EXIT
        END IF
      END DO
    END DO

    k = COUNT( obj%LBndyIndex .NE. 0 )
    ALLOCATE( obj%BoundaryData( k ) )
    k = 0
    DO iel = 1, obj%tElements
      IF( obj%LBndyIndex( iel ) .EQ. 0 ) CYCLE
      !! Continue only if element iel is a boundary element

      k = k + 1
      obj%LBndyIndex( iel ) = k

      ElemToElem = obj%getElementToElements( iel=iel, OnlyElements=.FALSE. )
      CALL Initiate( obj%BoundaryData( k ), tFace - SIZE( ElemToElem, 1 ) + 1 )
      CALL set( obj%BoundaryData( k ), Indx=1, value=iel )

      global_nptrs = obj%getConnectivity(iel)
      j = 0

      DO i = 1, tFace
        IF( ANY( i .EQ. ElemToElem( :, 2 ) ) ) CYCLE
        j = j + 1
        CALL set( obj%BoundaryData( k ), Indx=1+j, value=i )
      END DO
    END DO
  END IF

  IF( ALLOCATED( local_nptrs ) ) DEALLOCATE( local_nptrs )
  IF( ALLOCATED( global_nptrs ) ) DEALLOCATE( global_nptrs )
  IF( ALLOCATED( ElemToElem ) ) DEALLOCATE( ElemToElem )
END PROCEDURE mesh_InitiateBoundaryData

!----------------------------------------------------------------------------
!                                                     InitiateInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateInternalNptrs
  ! Define internal variables
  INTEGER( I4B ) :: i, j
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : ), DummyNptrs( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateInternalNptrs"

  IF( .NOT. obj%isInitiated ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  IF( .NOT. obj%isBoundaryDataInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Boundary data is not initiated." )
  END IF

  IF( .NOT. obj%isElementToElementsInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Element to Elements mapping not initiated, run obj%InitiateElementToElements()" )
  END IF

  IF( obj%isInternalNptrsInitiated() ) THEN
    CALL eMesh%raiseError(modName//"::"//myName//" - "// &
      & "Internal nptrs are already initiated." )
  END IF

  ALLOCATE( DummyNptrs( obj%maxNptrs ) )
  DummyNptrs = 0

  DO i = 1, obj%tElements
    IF( obj%isBoundaryElement( i ) ) CYCLE
    Nptrs = obj%getConnectivity( i )
    DummyNptrs( Nptrs ) = Nptrs
  END DO

  ALLOCATE( obj%InternalNptrs( COUNT( DummyNptrs .NE. 0 ) ) )

  i = 0
  DO j = 1, obj%maxNptrs
    IF( DummyNptrs( j ) .EQ. 0 ) CYCLE
    i = i + 1
    obj%InternalNptrs( i ) = DummyNptrs( j )
  END DO

  DO i = 1, SIZE( obj%InternalNptrs )
    j = obj%InternalNptrs( i )
    obj%Nptrs( obj%local_nptrs(j), 2 ) = obj%local_nptrs(j)
  END DO

  IF( ALLOCATED( DummyNptrs ) ) DEALLOCATE( DummyNptrs )
  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
END PROCEDURE mesh_InitiateInternalNptrs

!----------------------------------------------------------------------------
!                                                      isBoundaryNode
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isBoundaryNode
  INTEGER( I4B ) :: localnode
  localnode = obj%getLocalNptrs( GlobalNode )

  IF( obj%Nptrs(localnode,2) .LT. 0) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isBoundaryNode

!----------------------------------------------------------------------------
!                                            isLocalElementNumbersInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isLocalElementNumbersInitiated
  IF( ALLOCATED( obj%local_elemNumber ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isLocalElementNumbersInitiated

!----------------------------------------------------------------------------
!                                                           isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isNodePresent
  IF( GlobalNode .GT. obj%MaxNptrs &
    & .OR. GlobalNode .LT. obj%MinNptrs &
    & .OR. obj%Local_Nptrs( GlobalNode ) .EQ. 0 ) THEN
    ans = .FALSE.
  ELSE
    ans = .TRUE.
  END IF
END PROCEDURE mesh_isNodePresent

!----------------------------------------------------------------------------
!                                                    isNodeToNodesInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isNodeToNodesInitiated
  IF( ALLOCATED( obj%NTN ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isNodeToNodesInitiated

!----------------------------------------------------------------------------
!                                                  isNodeToElementsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isNodeToElementsInitiated
  IF( ALLOCATED( obj%NodeToElem ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isNodeToElementsInitiated


!----------------------------------------------------------------------------
!                                              isElementToElementsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isElementToElementsInitiated
    IF( ALLOCATED( obj%ElemToElem ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isElementToElementsInitiated

!----------------------------------------------------------------------------
!                                                   isConnectivityInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isConnectivityInitiated
  IF( ALLOCATED( obj%connectivity ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isConnectivityInitiated

!----------------------------------------------------------------------------
!                                                   isBoundaryDataInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isBoundaryDataInitiated
  IF( ALLOCATED( obj%BoundaryData ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isBoundaryDataInitiated

!----------------------------------------------------------------------------
!                                                  isInternalNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isInternalNptrsInitiated
  IF( ALLOCATED( obj%InternalNptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isInternalNptrsInitiated

!----------------------------------------------------------------------------
!                                                  isBoundaryNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isBoundaryNptrsInitiated
  IF( ALLOCATED( obj%BoundaryNptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isBoundaryNptrsInitiated

!----------------------------------------------------------------------------
!                                                     isLocalNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isLocalNptrsInitiated
  IF( ALLOCATED( obj%Local_Nptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isLocalNptrsInitiated

!----------------------------------------------------------------------------
!                                            isInternalBoundaryDataInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isInternalBoundaryDataInitiated
  IF( ALLOCATED( obj% InternalBoundaryData) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isInternalBoundaryDataInitiated

!----------------------------------------------------------------------------
!                                                 getTotalInternalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalInternalNodes
  ans = obj%tIntNodes
END PROCEDURE mesh_getTotalInternalNodes

!----------------------------------------------------------------------------
!                                                       getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalNodes
  ans = obj%tNodes
END PROCEDURE mesh_getTotalNodes

!----------------------------------------------------------------------------
!                                                   getTotalBoundaryNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalBoundaryNodes
  ans = obj%tNodes - obj%tIntNodes
END PROCEDURE mesh_getTotalBoundaryNodes

!----------------------------------------------------------------------------
!                                                 getTotalBoundaryElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getTotalBoundaryElements
  IF( ALLOCATED( obj%BoundaryData ) ) THEN
    ans = SIZE( obj%BoundaryData )
  ELSE
    ans = 0
  END IF
END PROCEDURE mesh_getTotalBoundaryElements

!----------------------------------------------------------------------------
!                                                            getBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundingBox
  REAL( DFP ) :: lim( 6 )

  lim(1) = obj%minX
  lim(2) = obj%maxX

  lim(3) = obj%minY
  lim(4) = obj%maxY

  lim(5) = obj%minZ
  lim(6) = obj%maxZ

  CALL Initiate( obj = ans, nsd = 3_I4B, lim = lim )
END PROCEDURE mesh_getBoundingBox

!----------------------------------------------------------------------------
!                                                        getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getConnectivity
  ans = obj%connectivity( :, iel )
END PROCEDURE mesh_getConnectivity

!----------------------------------------------------------------------------
!                                                          getLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalNodeNumber1
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalNode )
    ans( ii ) = mesh_getLocalNodeNumber2( obj, GlobalNode( ii ) )
  END DO
END PROCEDURE mesh_getLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                            getLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalNodeNumber2
  IF(      GlobalNode .LT. obj %MinNptrs &
    & .OR. GlobalNode .GT. obj%maxNptrs ) THEN
    ans = 0
  ELSE
    ans = obj%Local_Nptrs( GlobalNode )
  END IF
END PROCEDURE mesh_getLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                          getGlobalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalNodeNumber1
  INTEGER( I4B ) :: ii

  DO ii = 1, SIZE( LocalNode )
    ans( ii ) = mesh_getGlobalNodeNumber2( obj, LocalNode( ii ) )
  END DO
END PROCEDURE mesh_getGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                            getGlobalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalNodeNumber2
  IF( LocalNode .LE. obj%tNodes ) THEN
    ans = obj%Nptrs( LocalNode, 1 )
  ELSE
    ans = 0
  END IF
END PROCEDURE mesh_getGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                       getGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalElemNumber_1
  INTEGER( I4B ) :: ii

  DO ii = 1, SIZE( LocalElem )
    ans( ii ) = mesh_getGlobalElemNumber_2( obj, LocalElem( ii ) )
  END DO
END PROCEDURE mesh_getGlobalElemNumber_1

!----------------------------------------------------------------------------
!                                                        getGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalElemNumber_2
  IF( LocalElem .LE. obj%tElements ) THEN
    ans = obj%ElemNumber( LocalElem )
  ELSE
    ans = 0
  END IF
END PROCEDURE mesh_getGlobalElemNumber_2

!----------------------------------------------------------------------------
!                                                   getLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalElemNumber_1
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalElem )
    ans( ii ) = mesh_getLocalElemNumber_2( obj, GlobalElem( ii ) )
  END DO
END PROCEDURE mesh_getLocalElemNumber_1

!----------------------------------------------------------------------------
!                                                   getLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalElemNumber_2
  IF(      GlobalElem .LT. obj %MinElemNum &
    & .OR. GlobalElem .GT. obj%maxElemNum ) THEN
    ans = 0
  ELSE
    ans = obj%local_elemNumber( GlobalElem )
  END IF
END PROCEDURE mesh_getLocalElemNumber_2

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeToElements
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_getNodeToElements"
  IF( .NOT. obj%isNodePresent( GlobalNode ) ) THEN
    ALLOCATE( ans( 0 ) )
  ELSE
    ans = obj%NodeToElem( obj%getLocalNptrs( GlobalNode ) )
  END IF
END PROCEDURE mesh_getNodeToElements

!----------------------------------------------------------------------------
!                                                            getNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNodeToNodes
  ! Define internal variable
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  INTEGER( I4B ) :: i
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_getNodeToNodes"

  i = obj%getLocalNptrs( GlobalNode = GlobalNode )
  !> check
  IF( i .EQ. 0 ) THEN
    ALLOCATE( ans( 0 ) )
  ELSE
    IF( IncludeSelf ) THEN
      Nptrs = obj%NTN( i )
      i = SIZE( Nptrs )
      ALLOCATE( ans( i + 1 ) )
      ans( 1 ) = GlobalNode
      ans( 2 : ) = Nptrs
    ELSE
      ans = obj%NTN( i )
    END IF
  END IF

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
END PROCEDURE mesh_getNodeToNodes

!----------------------------------------------------------------------------
!                                                     getElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getElementToElements
  LOGICAL( LGT ) :: onlyElem
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  INTEGER( I4B ) :: tSize

  onlyElem = .FALSE.
  IF( PRESENT( onlyElements ) ) onlyElem = onlyElements
  IF( onlyElem ) THEN
    Nptrs = obj%ElemToElem( iel )
    ALLOCATE( ans( SIZE( Nptrs )/3, 1 ) )
    ans( :, 1 ) = Nptrs( 1::3 )
    DEALLOCATE( Nptrs )
  ELSE
    Nptrs = obj%ElemToElem( iel )
    ans = TRANSPOSE( RESHAPE( Nptrs, [3, SIZE( Nptrs ) / 3] ) )
  END IF
END PROCEDURE mesh_getElementToElements

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isBoundaryElement
  IF( obj%LBndyIndex( iel ) .NE. 0 ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isBoundaryElement

!----------------------------------------------------------------------------
!                                                    getBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundaryElementData
  ! Define internal variables
  INTEGER( I4B ) :: LocalIndx, n
  LocalIndx = obj%LBndyIndex( iel )

  n = SIZE( obj%BoundaryData( LocalIndx ) )
  IF( n .EQ. 1 ) THEN
    ALLOCATE( ans( 0 ) )
  ELSE
    ans = get( obj%BoundaryData( LocalIndx ), iStart=2, iEnd=n, Stride=1, DataType=TypeInt )
  END IF
END PROCEDURE mesh_getBoundaryElementData

!----------------------------------------------------------------------------
!                                                          getInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getInternalNptrs
  IF( obj%isInternalNptrsInitiated() ) THEN
    ans = obj%InternalNptrs
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE mesh_getInternalNptrs


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MeshDataMethods