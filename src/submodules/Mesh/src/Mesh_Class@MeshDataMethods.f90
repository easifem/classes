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
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshData_Final
  CALL obj%DeallocateData()
END PROCEDURE MeshData_Final

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshData_DeallocateData
  obj%uid = 0
  obj%xidim = 0
  obj%elemType = 0
  obj%nsd = 0
  obj%maxNptrs = 0
  obj%minNptrs = 0
  obj%tNodes = 0
  obj%tIntNodes = 0
  obj%minX = 0.0_DFP
  obj%maxX = 0.0_DFP
  obj%minY = 0.0_DFP
  obj%maxY = 0.0_DFP
  obj%minZ = 0.0_DFP
  obj%maxZ = 0.0_DFP
  obj%X=0.0_DFP
  obj%Y=0.0_DFP
  obj%Z=0.0_DFP
  obj%isInitiated = .FALSE.
  IF( ALLOCATED( obj%nodeCoord ) ) DEALLOCATE( obj%nodeCoord )
  IF( ALLOCATED( obj%physicalTag ) ) DEALLOCATE( obj%physicalTag )
  IF( ALLOCATED( obj%elemNumber ) ) DEALLOCATE( obj%elemNumber )
  IF( ALLOCATED( obj%local_elemNumber ) ) DEALLOCATE( obj%local_elemNumber )
  IF( ALLOCATED( obj%boundingEntity ) ) DEALLOCATE( obj%boundingEntity )
  IF( ALLOCATED( obj%connectivity ) ) DEALLOCATE( obj%connectivity )
  IF( ALLOCATED( obj%LBndyIndex ) ) DEALLOCATE( obj%LBndyIndex )
  IF( ALLOCATED( obj%Nptrs ) ) DEALLOCATE( obj%Nptrs )
  IF( ALLOCATED( obj%BoundaryNptrs ) ) DEALLOCATE( obj%BoundaryNptrs )
  IF( ALLOCATED( obj%InternalNptrs ) ) DEALLOCATE( obj%InternalNptrs )
  IF( ALLOCATED( obj%Local_Nptrs ) ) DEALLOCATE( obj%Local_Nptrs )
  obj%refelem => NULL()
  IF( ALLOCATED( obj%NodeToElem ) ) DEALLOCATE( obj%NodeToElem )
  IF( ALLOCATED( obj%ElemToElem ) ) DEALLOCATE( obj%ElemToElem )
  IF( ALLOCATED( obj%NTN ) ) DEALLOCATE( obj%NTN )
  IF( ALLOCATED( obj%BoundaryData ) ) DEALLOCATE( obj%BoundaryData )
  IF( ALLOCATED( obj%InternalBndyElemNum ) ) DEALLOCATE( obj%InternalBndyElemNum )
  IF( ALLOCATED( obj%InternalBoundaryData ) ) DEALLOCATE( obj%InternalBoundaryData )
END PROCEDURE MeshData_DeallocateData

!----------------------------------------------------------------------------
!                                                         InitiateLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_InitiateLocalNptrs
  INTEGER( I4B ) :: ii, dummy
  CHARACTER( LEN = * ), PARAMETER :: myName="meshData_InitiateLocalNptrs"

  IF( .NOT. ALLOCATED( obj%connectivity ) ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Connectivity matrix is not allocated.")
  END IF

  obj%maxNptrs = maxval(obj%connectivity)
  obj%minNptrs = minval(obj%connectivity)

  IF( obj%maxNptrs .EQ. 0 ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Largest node number (maxNptrs) cannot be zero.")
  END IF

  CALL Reallocate( obj%Local_Nptrs, obj%maxNptrs )
  DO ii =1, obj%tElements
    obj%Local_Nptrs( obj%connectivity(:, ii) ) = obj%connectivity(:, ii)
  END DO

  obj%tNodes = COUNT( obj%Local_Nptrs .NE. 0 )

  CALL Reallocate( obj%nptrs, obj%tNodes )
  dummy = 0
  DO ii = 1, obj%maxNptrs
    IF( obj%Local_Nptrs( ii ) .NE. 0 ) THEN
      dummy = dummy + 1
      obj%nptrs( dummy ) = obj%Local_Nptrs( ii )
      obj%Local_Nptrs( ii ) = dummy
    END IF
  END DO
END PROCEDURE meshData_InitiateLocalNptrs

!----------------------------------------------------------------------------
!                                                              InitiateNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_InitiateNptrs
  CALL meshData_InitiateLocalNptrs( obj )
END PROCEDURE meshData_InitiateNptrs

!----------------------------------------------------------------------------
!                                                 getTotalInternalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getTotalInternalNodes
  ans = obj%tIntNodes
END PROCEDURE meshData_getTotalInternalNodes

!----------------------------------------------------------------------------
!                                                       getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getTotalNodes
  ans = obj%tNodes
END PROCEDURE meshData_getTotalNodes

!----------------------------------------------------------------------------
!                                                   getTotalBoundaryNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getTotalBoundaryNodes
  ans = obj%tNodes - obj%tIntNodes
END PROCEDURE meshData_getTotalBoundaryNodes

!----------------------------------------------------------------------------
!                                                 getTotalBoundaryElements
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getTotalBoundaryElements
  IF( ALLOCATED( obj%BoundaryData ) ) THEN
    ans = SIZE( obj%BoundaryData )
  ELSE
    ans = 0
  END IF
END PROCEDURE meshData_getTotalBoundaryElements

!----------------------------------------------------------------------------
!                                                            getBoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getBoundingBox
  REAL( DFP ) :: lim( 6 )

  lim(1) = obj%minX
  lim(2) = obj%maxX

  lim(3) = obj%minY
  lim(4) = obj%maxY

  lim(5) = obj%minZ
  lim(6) = obj%maxZ

  CALL Initiate( obj = ans, nsd = 3_I4B, lim = lim )
END PROCEDURE meshData_getBoundingBox

!----------------------------------------------------------------------------
!                                                        getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getConnectivity
  ans = obj%connectivity( :, iel )
END PROCEDURE meshData_getConnectivity

!----------------------------------------------------------------------------
!                                                          getLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getLocalNptrs_1
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalNode )
    ans( ii ) = meshData_getGlobalNptrs_2( obj, GlobalNode( ii ) )
  END DO
END PROCEDURE meshData_getLocalNptrs_1

!----------------------------------------------------------------------------
!                                                            getLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getLocalNptrs_2
  IF(      GlobalNode .LT. obj %MinNptrs &
    & .OR. GlobalNode .GT. obj%maxNptrs ) THEN
    ans = 0
  ELSE
    ans = obj%Local_Nptrs( GlobalNode )
  END IF
END PROCEDURE meshData_getLocalNptrs_2

!----------------------------------------------------------------------------
!                                                          getGlobalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getGlobalNptrs_1
  INTEGER( I4B ) :: ii

  DO ii = 1, SIZE( LocalNode )
    ans( ii ) = meshData_getGlobalNptrs_2( obj, LocalNode( ii ) )
  END DO
END PROCEDURE meshData_getGlobalNptrs_1

!----------------------------------------------------------------------------
!                                                            getGlobalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getGlobalNptrs_2
  IF( LocalNode .LE. obj%tNodes ) THEN
    ans = obj%Nptrs( LocalNode )
  ELSE
    ans = 0
  END IF
END PROCEDURE meshData_getGlobalNptrs_2

!----------------------------------------------------------------------------
!                                                       getGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getGlobalElemNumber_1
  INTEGER( I4B ) :: ii

  DO ii = 1, SIZE( LocalElem )
    ans( ii ) = meshData_getGlobalElemNumber_2( obj, LocalElem( ii ) )
  END DO
END PROCEDURE meshData_getGlobalElemNumber_1

!----------------------------------------------------------------------------
!                                                        getGlobalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getGlobalElemNumber_2
  IF( LocalElem .LE. obj%tElements ) THEN
    ans = obj%ElemNumber( LocalElem )
  ELSE
    ans = 0
  END IF
END PROCEDURE meshData_getGlobalElemNumber_2

!----------------------------------------------------------------------------
!                                                   getLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getLocalElemNumber_1
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalElem )
    ans( ii ) = meshData_getLocalElemNumber_2( obj, GlobalElem( ii ) )
  END DO
END PROCEDURE meshData_getLocalElemNumber_1

!----------------------------------------------------------------------------
!                                                   getLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getLocalElemNumber_2
  IF( GlobalElem .LT. obj%tElements ) THEN
    ans = obj%local_elemNumber( GlobalElem )
  ELSE
    ans = 0
  END IF
END PROCEDURE meshData_getLocalElemNumber_2

!----------------------------------------------------------------------------
!                                            isLocalElementNumbersInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isLocalElementNumbersInitiated
  IF( ALLOCATED( obj%local_elemNumber ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isLocalElementNumbersInitiated

!----------------------------------------------------------------------------
!                                                           isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isNodePresent
  IF( GlobalNode .GT. obj%MaxNptrs &
    & .OR. GlobalNode .LT. obj%MinNptrs &
    & .OR. obj%Local_Nptrs( GlobalNode ) .EQ. 0 ) THEN
    ans = .FALSE.
  ELSE
    ans = .TRUE.
  END IF
END PROCEDURE meshData_isNodePresent

!----------------------------------------------------------------------------
!                                                    isNodeToNodesInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isNodeToNodesInitiated
  IF( ALLOCATED( obj%NTN ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isNodeToNodesInitiated

!----------------------------------------------------------------------------
!                                                  isNodeToElementsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isNodeToElementsInitiated
  IF( ALLOCATED( obj%NodeToElem ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isNodeToElementsInitiated

!----------------------------------------------------------------------------
!                                              isElementToElementsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isElementToElementsInitiated
    IF( ALLOCATED( obj%ElemToElem ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isElementToElementsInitiated

!----------------------------------------------------------------------------
!                                                   isConnectivityInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isConnectivityInitiated
  IF( ALLOCATED( obj%connectivity ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isConnectivityInitiated

!----------------------------------------------------------------------------
!                                                   isBoundaryDataInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isBoundaryDataInitiated
  IF( ALLOCATED( obj%BoundaryData ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isBoundaryDataInitiated

!----------------------------------------------------------------------------
!                                                  isInternalNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isInternalNptrsInitiated
  IF( ALLOCATED( obj%InternalNptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isInternalNptrsInitiated

!----------------------------------------------------------------------------
!                                                  isBoundaryNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isBoundaryNptrsInitiated
  IF( ALLOCATED( obj%BoundaryNptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isBoundaryNptrsInitiated

!----------------------------------------------------------------------------
!                                                     isLocalNptrsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isLocalNptrsInitiated
  IF( ALLOCATED( obj%Local_Nptrs ) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isLocalNptrsInitiated

!----------------------------------------------------------------------------
!                                            isInternalBoundaryDataInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_isInternalBoundaryDataInitiated
  IF( ALLOCATED( obj% InternalBoundaryData) ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE meshData_isInternalBoundaryDataInitiated

!----------------------------------------------------------------------------
!                                          InitiateLocalElementNumbers
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_InitiateLocalElementNumbers
  INTEGER( I4B ) :: ii
  CHARACTER( LEN = * ), PARAMETER :: myName = "meshData_InitiateLocalElementNumbers"

  IF( .NOT. obj%isInitiated ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  IF( obj%isLocalElementNumbersInitiated() ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Local element numbers are already initiated.")
  ELSE
    CALL Reallocate( obj%local_elemNumber, MAXVAL( obj%ElemNumber ) )
    DO ii = 1, obj%tElements
      obj%local_elemNumber( obj%elemNumber( ii ) ) = ii
    END DO
  END IF
END PROCEDURE meshData_InitiateLocalElementNumbers

!----------------------------------------------------------------------------
!                                                    InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_InitiateNodeToElements
  ! Define internal  variables
  INTEGER( I4B ) :: ii, iNode
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "meshData_InitiateNodeToElements"

  IF( .NOT. obj%isInitiated ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  IF( obj%isNodeToElementsInitiated() ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
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
END PROCEDURE meshData_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getNodeToElements
  CHARACTER( LEN = * ), PARAMETER :: myName="meshData_getNodeToElements"
  IF( .NOT. obj%isNodePresent( GlobalNode ) ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "The global node number "//str(GlobalNode, .true.)// " is not present inside the mesh.")
  ELSE
    ans = obj%NodeToElem( obj%getLocalNptrs( GlobalNode ) )
  END IF
END PROCEDURE meshData_getNodeToElements

!----------------------------------------------------------------------------
!                                                   InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_InitiateNodetoNodes
  ! Define internal  variables
  INTEGER( I4B ) :: iel, iLocalNode, tSize, iGlobalNode
  INTEGER( I4B ), ALLOCATABLE ::  global_nptrs( : ), NearElements( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "meshData_InitiateNodetoNodes"

  IF( .NOT. obj%isInitiated ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  IF( .NOT. obj%isNodeToElementsInitiated() ) THEN
    CALL obj%InitiateNodeToElements( )
  END IF

  IF( obj%isNodeToNodesInitiated() ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
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
    IF( ALLOCATED( global_nptrs ) ) DEALLOCATE( global_nptrs )
    IF( ALLOCATED( NearElements ) ) DEALLOCATE( NearElements )
  END IF
END PROCEDURE meshData_InitiateNodetoNodes

!----------------------------------------------------------------------------
!                                                            getNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getNodeToNodes
  ! Define internal variable
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  INTEGER( I4B ) :: i
  CHARACTER( LEN = * ), PARAMETER :: myName="meshData_getNodeToNodes"

  i = obj%getLocalNptrs( GlobalNode = GlobalNode )

  IF( i .EQ. 0 ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Global node number " // trim(str(GlobalNode, .true.)) // " not present inside the mesh" )
  END IF

  IF( IncludeSelf ) THEN
    Nptrs = obj%NTN( i )
    i = SIZE( Nptrs )
    ALLOCATE( ans( i + 1 ) )
    ans( 1 ) = GlobalNode
    ans( 2 : ) = Nptrs
  ELSE
    ans = obj%NTN( i )
  END IF

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
END PROCEDURE meshData_getNodeToNodes

!----------------------------------------------------------------------------
!                                                InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_InitiateElementToElements
  ! Define internal  variables
  INTEGER( I4B ) :: i, j, r,  iel1, tFace, iFace1, NNS1, pt1, &
    & iel2, iFace2, NNS2
  INTEGER( I4B ), ALLOCATABLE :: global_nptrs1( : ),   &
    & global_nptrsFace1( : ), n2e1( : ), global_nptrs2( : ), &
    & global_nptrsFace2( : )
  LOGICAL( LGT ) :: Found
  CHARACTER( LEN = * ), PARAMETER :: myName = "meshData_InitiateElementToElements"

  IF( .NOT. obj%isInitiated ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  IF( .NOT. ASSOCIATED( obj%refelem ) ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Unable to identify the Reference element of the mesh, may be it is not set" )
  END IF

  IF( .NOT. ALLOCATED( obj%FacetElements ) ) THEN
    obj%FacetElements = FacetElements( obj%refelem )
  END IF

  tFace = SIZE(obj%FacetElements)
    !! Total number of facet elements

  IF( obj%isElementToElementsInitiated() ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
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
END PROCEDURE meshData_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                     getElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_getElementToElements
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
END PROCEDURE meshData_getElementToElements

!----------------------------------------------------------------------------
!                                                InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE meshData_InitiateBoundaryData
  ! Define internal variables
  INTEGER( I4B ) :: iel, tElements, te2e, tFace, tBndyElem, &
    & tBndyFace, i, j, k, b, dummy
  INTEGER( I4B ), ALLOCATABLE :: e2eData( : ), FaceVec( : ), &
    & DummyNptrs( : ), local_nptrs( : ), global_nptrs( : )
  CHARACTER( LEN = * ), PARAMETER :: myName="meshData_InitiateBoundaryData"

  IF( .NOT. obj%isInitiated ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF

  IF( obj%isBoundaryDataInitiated() ) THEN
    CALL obj%e%raiseError(modName//"::"//myName//" - "// &
      & "Boundary data is already initiated." )
  ELSE
    CALL Reallocate( obj%LBndyIndex, obj%tElements )
    IF( ALLOCATED( obj%BoundaryNptrs ) ) DEALLOCATE( obj%BoundaryNptrs )

  !   obj%LBndyIndex = 0_I4B
  !   IF( ALLOCATED( obj%BoundaryNptrs ) ) DEALLOCATE( obj%BoundaryNptrs )
  !   SELECT CASE( tElements )
  !   CASE( 1 )
  !     ALLOCATE( obj%BoundaryData( 1 ) )
  !     obj%LBndyIndex( 1 ) = 1
  !     Elem => Meshobj%Elem( 1 )%Ptr
  !     obj%BoundaryNptrs = .Nptrs. Elem
  !     FM = FacetMatrix( Elem%refelem )
  !     tFace = SIZE( FM, 1 )
  !     CALL Initiate( obj%BoundaryData( 1 ), tFace + 1 )
  !     obj%BoundaryData( 1 )%Val( 1 ) = 1
  !     obj%BoundaryData( 1 )%Val( 2: ) = [(i, i=1, tFace)]
  !   CASE DEFAULT
  !     CALL obj%InitiateElementToElements( Meshobj = Meshobj )
  !     DO iel = 1, tElements
  !       dummy = SIZE( obj%ElemToElem( iel ) )
  !       IF( dummy .EQ. 0 ) CYCLE
  !       e2eData = obj%ElemToElem( iel )%Val
  !       te2e = SIZE( e2eData ) / 3
  !       Elem => Meshobj%Elem( iel )%Ptr
  !       FM = FacetMatrix( Elem%refelem )
  !       tFace = SIZE( FM, 1 )
  !       IF( tFace .NE. te2e ) THEN
  !         obj%LBndyIndex( iel ) = 1
  !       END IF
  !     END DO
  !     tBndyElem = COUNT( obj%LBndyIndex .NE. 0 )
  !     ALLOCATE( obj%BoundaryData( tBndyElem ) )
  !     ALLOCATE( DummyNptrs( obj%MaxNptrs ) )
  !     DummyNptrs = 0; k = 0
  !     DO iel = 1, tElements
  !       IF( obj%LBndyIndex( iel ) .EQ. 0 ) CYCLE
  !       k = k + 1
  !       obj%LBndyIndex( iel ) = k
  !       e2eData = obj%ElemToElem( iel )%Val
  !       te2e = SIZE( e2eData ) / 3
  !       IF( ALLOCATED( FaceVec ) ) DEALLOCATE( FaceVec )
  !       ALLOCATE( FaceVec( te2e ) )
  !       DO i = 1, te2e
  !         FaceVec( i ) = e2eData( 3*(i-1) + 2 )
  !       END DO
  !       Elem => Meshobj%Elem( iel )%Ptr
  !       FM = FacetMatrix( Elem%refelem )
  !       tFace = SIZE( FM, 1 )
  !       tBndyFace = tFace - te2e
  !       CALL Initiate( obj%BoundaryData( k ), tBndyFace + 1 )
  !       obj%BoundaryData( k )%Val( 1 ) = iel
  !       global_nptrs = .Nptrs. Elem
  !       j = 0
  !       DO i = 1, tFace
  !         IF( ANY( i .EQ. FaceVec ) ) CYCLE
  !         j = j + 1
  !         obj%BoundaryData( k )%Val( 1 + j ) = i
  !         ! get local_nptrs of the face
  !         b = 3 + FM( i, 3 )
  !         local_nptrs = FM( i, 4 : b )
  !         DummyNptrs( global_nptrs( local_nptrs ) ) = &
  !           & global_nptrs( local_nptrs )
  !       END DO
  !     END DO
  !     b = COUNT( DummyNptrs .NE. 0 )
  !     ALLOCATE( obj%BoundaryNptrs( b ) )
  !     k = 0
  !     DO i = 1, obj%MaxNptrs
  !       IF( DummyNptrs( i ) .EQ. 0 ) CYCLE
  !       k = k + 1
  !       obj%BoundaryNptrs( k ) = DummyNptrs( i )
  !     END DO
  !   END SELECT
  !   NULLIFY( Elem )
  !   IF( ALLOCATED( FM ) ) DEALLOCATE( FM )
  !   IF( ALLOCATED( e2eData ) ) DEALLOCATE( e2eData )
  !   IF( ALLOCATED( FaceVec ) ) DEALLOCATE( FaceVec )
  !   IF( ALLOCATED( DummyNptrs ) ) DEALLOCATE( DummyNptrs )
  !   IF( ALLOCATED( local_nptrs ) ) DEALLOCATE( local_nptrs )
  !   IF( ALLOCATED( global_nptrs ) ) DEALLOCATE( global_nptrs )
  END IF
END PROCEDURE meshData_InitiateBoundaryData
END SUBMODULE MeshDataMethods