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

SUBMODULE( Mesh_Class ) getMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_size
  ans = obj%tElements
END PROCEDURE mesh_size

!----------------------------------------------------------------------------
!                                                         getBoundingEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundingEntity
  IF( ALLOCATED( obj%boundingEntity ) ) THEN
    ans = obj%boundingEntity
  ELSE
    ALLOCATE( ans(0) )
  END IF
END PROCEDURE mesh_getBoundingEntity

!----------------------------------------------------------------------------
!                                                               getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getNptrs
  INTEGER( I4B ) :: ii
  ALLOCATE( ans( obj%tNodes ) )
  DO CONCURRENT (ii=1:SIZE(ans))
    ans(ii) = obj%nodeData(ii)%globalNodeNum
  END DO
END PROCEDURE mesh_getNptrs

!----------------------------------------------------------------------------
!                                                          getInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getInternalNptrs
  INTEGER( I4B ) :: ii, dummy
  ALLOCATE( ans( obj%getTotalInternalNodes() ) )
  dummy = 0
  DO ii = 1, obj%tNodes
    IF( obj%nodeData( ii )%nodeType .EQ. INTERNAL_NODE ) THEN
      dummy = dummy + 1
      ans( dummy ) = obj%nodeData( ii )%globalNodeNum
    END IF
  END DO
END PROCEDURE mesh_getInternalNptrs

!----------------------------------------------------------------------------
!                                                          getBoundaryNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundaryNptrs
  INTEGER( I4B ) :: ii, dummy
  ALLOCATE( ans( obj%getTotalBoundaryNodes() ) )
  dummy = 0
  DO ii = 1, obj%tNodes
    IF( obj%nodeData( ii )%nodeType .EQ. BOUNDARY_NODE ) THEN
      dummy = dummy + 1
      ans( dummy ) = obj%nodeData( ii )%globalNodeNum
    END IF
  END DO
END PROCEDURE mesh_getBoundaryNptrs

!----------------------------------------------------------------------------
!                                                            isBoundaryNode
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isBoundaryNode
  INTEGER( I4B ) :: localnode
  localnode = obj%getLocalNodeNumber( GlobalNode )
  IF( obj%nodeData(localnode)%nodeType .NE. INTERNAL_NODE) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isBoundaryNode

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
!                                                           isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isElementPresent
  IF( GlobalElement .GT. obj%maxElemNum &
    & .OR. GlobalElement .LT. obj%minElemNum &
    & .OR. obj%local_elemNumber( GlobalElement ) .EQ. 0 ) THEN
    ans = .FALSE.
  ELSE
    ans = .TRUE.
  END IF
END PROCEDURE mesh_isElementPresent

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_isBoundaryElement
  INTEGER( I4B ) :: iel
  iel = obj%getLocalElemNumber(globalElemNumber)
  IF( obj%elementData(iel)%elementType .EQ. BOUNDARY_ELEMENT ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE mesh_isBoundaryElement

!----------------------------------------------------------------------------
!                                                     getTotalInternalNodes
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
  ans=count(obj%elementData(:)%elementType==BOUNDARY_ELEMENT)
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
!                                                            getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getConnectivity
  ans = obj%elementData( obj%getLocalElemNumber(globalElemNumber) )%globalNodes
END PROCEDURE mesh_getConnectivity

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getLocalNodeNumber1
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( GlobalNode )
    ans( ii ) = mesh_getLocalNodeNumber2( obj, GlobalNode( ii ) )
  END DO
END PROCEDURE mesh_getLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                        getLocalNodeNumber
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
!                                                        getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalNodeNumber1
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( LocalNode )
    ans( ii ) = mesh_getGlobalNodeNumber2( obj, LocalNode( ii ) )
  END DO
END PROCEDURE mesh_getGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                       getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getGlobalNodeNumber2
  IF ( localNode .EQ. 0 ) THEN
    ans = 0
  ELSE IF( localNode .LE. obj%tNodes ) THEN
    ans = obj%nodeData(localNode)%globalNodeNum
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
  IF( localElem .EQ. 0 ) THEN
    ans = 0
  ELSE IF( localElem .LE. obj%tElements ) THEN
    ans = obj%elementData( localElem )%globalElemNum
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
  INTEGER( I4B ) :: ii
  IF( .NOT. obj%isNodePresent( GlobalNode ) ) THEN
    ALLOCATE( ans( 0 ) )
  ELSE
    ii = obj%getLocalNodeNumber( GlobalNode )
    ans = obj%nodeData(ii)%globalElements
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

  i = obj%getLocalNodeNumber( GlobalNode = GlobalNode )
  !> check
  IF( i .EQ. 0 ) THEN
    ALLOCATE( ans( 0 ) )
  ELSE
    IF( IncludeSelf ) THEN
      Nptrs = obj%nodeData(i)%globalNodes
      i = SIZE( Nptrs )
      ALLOCATE( ans( i + 1 ) )
      ans( 1 ) = GlobalNode
      ans( 2 : ) = Nptrs
    ELSE
      ans = obj%nodeData(i)%globalNodes
    END IF
  END IF
  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
END PROCEDURE mesh_getNodeToNodes

!----------------------------------------------------------------------------
!                                                     getElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getElementToElements
  LOGICAL( LGT ) :: onlyElem
  INTEGER( I4B ), ALLOCATABLE :: Indx( : )
  INTEGER( I4B ) :: tSize, iel

  onlyElem = .FALSE.
  IF( PRESENT( onlyElements ) ) onlyElem = onlyElements
  iel = obj%getLocalElemNumber( globalElemNumber )
  IF( onlyElem ) THEN
    Indx = obj%elementData(iel)%globalElements
    ALLOCATE( ans( SIZE( Indx )/3, 1 ) )
    ans( :, 1 ) = Indx( 1::3 )
    DEALLOCATE( Indx )
  ELSE
    Indx = obj%elementData(iel)%globalElements
    ans = TRANSPOSE( RESHAPE( Indx, [3, SIZE( Indx ) / 3] ) )
  END IF
END PROCEDURE mesh_getElementToElements

!----------------------------------------------------------------------------
!                                                    getBoundaryElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_getBoundaryElementData
  ! Define internal variables
  INTEGER( I4B ) :: iel
  IF( obj%isBoundaryElement(globalElemNumber) ) THEN
    iel = obj%getLocalElemNumber(globalElemNumber)
    ans = obj%elementData(iel)%boundaryData
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE mesh_getBoundaryElementData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE getMethod