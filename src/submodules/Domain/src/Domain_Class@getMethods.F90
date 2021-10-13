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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(Domain_Class) getMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             isNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_isNodePresent
  IF( globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs ) THEN
    ans=.FALSE.
  ELSE
    IF( obj%local_nptrs(globalNode) .EQ. 0 ) THEN
      ans=.FALSE.
    ELSE
      ans=.TRUE.
    END IF
  END IF
END PROCEDURE Domain_isNodePresent
!----------------------------------------------------------------------------
!                                                             getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getTotalNodes
  CHARACTER( LEN = * ), PARAMETER :: myName = "Domain_getTotalNodes"
  CLASS( Mesh_ ), POINTER :: meshPtr
  !>
  IF( PRESENT( entityNum ) .AND. PRESENT( dim ) ) THEN
    IF(obj%meshList(dim)%isEmpty()) &
      & CALL e%raiseError(modName//'::'//myName//'-'// &
      & 'The mesh of enitityNum='// TRIM(str(entityNum, .true.)) // &
      & " and dimension=" // TRIM(str(dim, .true.)) // " is empty." )
    IF( entityNum .GT. obj%meshList( dim )%size() ) &
      & CALL e%raiseError(modName//'::'//myName//'-'// &
      & 'The the enitityNum='// TRIM(str(entityNum, .true.)) &
      & // " for dimension=" // TRIM(str(dim, .true.)) // &
      & " is out of bound." )
    meshPtr => NULL()
    meshPtr => obj%getMeshPointer( dim=dim, entityNum=entityNum )
    IF( .NOT. ASSOCIATED( meshPtr ) ) THEN
      CALL e%raiseError(modName//'::'//myName//'-'// &
      & 'There is some issue in getting pointer to mesh' )
    ELSE
      ans = meshPtr%getTotalNodes()
      NULLIFY( meshPtr )
    END IF
  ELSE
    ans = obj%tNodes
  END IF
END PROCEDURE Domain_getTotalNodes

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getLocalNodeNumber1
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getLocalNodeNumber"
  IF( obj%isNodePresent( globalNode ) ) THEN
    ans = obj%local_nptrs( globalNode )
  ELSE
    ans = 0
  END IF
END PROCEDURE Domain_getLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getLocalNodeNumber2
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( globalNode )
    ans( ii ) = Domain_getLocalNodeNumber1( obj, globalNode(ii))
  END DO
END PROCEDURE Domain_getLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getGlobalNodeNumber1
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getGlobalNodeNumber"
  IF( localNode .LE. obj%tNodes ) THEN
    ans = getIndex( obj%local_nptrs, localNode )
  ELSE
    ans = 0
  END IF
END PROCEDURE Domain_getGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getGlobalNodeNumber2
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( localNode )
    ans( ii ) = Domain_getGlobalNodeNumber1( obj, localNode(ii))
  END DO
END PROCEDURE Domain_getGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                              getTotalMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getTotalMesh
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getTotalMesh"
  IF( dim .LT. 0 .OR. dim .GT. 3 ) THEN
    CALL e%raiseError( modName//"::"//myName//" - "// &
      & "dim of the mesh should be in [0,1,2,3]" )
  END IF
  IF( obj%meshList( dim )%isEmpty() ) THEN
    ans=0
  ELSE
    ans = obj%meshList( dim )%size()
  END IF
END PROCEDURE Domain_getTotalMesh

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getMeshPointer
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getMeshPointer"
  INTEGER( I4B ) :: tsize, imesh
  TYPE( MeshPointerIterator_ ) :: iterator
  !> main
  iterator%value%ptr => NULL()
  tsize = obj%getTotalMesh( dim=dim )
  IF( entityNum .GT. tsize ) THEN
    CALL e%raiseInformation( modName//"::"//myName//" - "// &
      & "entityNum are out of bound" )
  END IF
  iterator = obj%meshList( dim )%Begin()
  DO imesh = 2, entityNum
    CALL iterator%Inc()
  END DO
  ans => iterator%value%ptr
  CALL iterator%DeallocateData()
END PROCEDURE Domain_getMeshPointer

!----------------------------------------------------------------------------
!                                                               getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNodeCoord
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getNodeCoord"
  CLASS( Mesh_ ), POINTER :: meshPtr
  INTEGER( I4B ) :: np, ii, jj
  !> main, check
  IF( .NOT. ALLOCATED( obj%nodeCoord ) ) &
    & CALL e%raiseError( modName//"::"//myName//" - "// &
    & "Nodecoord is not allocated." )
  IF( PRESENT( dim ) .AND. PRESENT( entityNum ) ) THEN
    meshPtr => obj%getMeshPointer( dim=dim, entityNum=entityNum )
    np = meshPtr%getTotalNodes()
    CALL Reallocate( nodeCoord, 3_I4B, np )
    jj = SIZE( nodeCoord, 1 )
    DO ii = 1, np
      nodeCoord( 1:jj, ii ) = obj%nodeCoord( 1:jj, &
        & obj%getLocalNodeNumber( globalNode = &
        & meshPtr%getGlobalNodeNumber( localNode=ii ) ) )
    END DO
    NULLIFY( meshPtr )
  ELSE
    nodeCoord = obj%nodeCoord
  END IF
END PROCEDURE Domain_getNodeCoord

!----------------------------------------------------------------------------
!                                                        getNodeCoordPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNodeCoordPointer
  ans => obj%nodeCoord
END PROCEDURE Domain_getNodeCoordPointer

!----------------------------------------------------------------------------
!                                            getGlobalToLocalNodeNumPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getGlobalToLocalNodeNumPointer
  ans => obj%local_nptrs
END PROCEDURE Domain_getGlobalToLocalNodeNumPointer

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNptrs
  INTEGER( I4B ) :: ii
  CLASS( Mesh_ ), POINTER :: meshptr
  TYPE( IntVector_ ) :: intvec
  !>
  meshptr => NULL()
  DO ii = 1, SIZE( meshID )
    meshptr => obj%GetMeshPointer( dim=xidim, entityNum=meshID( ii ) )
    IF( ASSOCIATED( meshptr )  ) THEN
      CALL APPEND( intvec, meshptr%getNptrs() )
    END IF
  END DO
  CALL RemoveDuplicates( intvec )
  ans = intvec
  CALL DeallocateData( intvec )
  NULLIFY( meshptr )
END PROCEDURE Domain_getNptrs

!----------------------------------------------------------------------------
!                                                                     getNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNSD
  ans = obj%NSD
END PROCEDURE Domain_getNSD

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE getMethods