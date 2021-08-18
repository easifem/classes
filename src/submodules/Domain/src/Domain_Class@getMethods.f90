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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE( Domain_Class ) getMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       getTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getTotalNodes
  CHARACTER( LEN = * ), PARAMETER :: myName = "Domain_getTotalNodes"
  TYPE( MeshPointerIterator_ ) :: iterator

  IF( PRESENT( physicalName ) ) THEN
    ans = obj%getIndex( physicalName )
    IF( ans .NE. 0 ) THEN
      ans = obj%numNodes( ans )
    ELSE
      CALL eDomain%raiseError(modName//'::'//myName//'-'// &
        & 'The physical name '// physicalName // " not found" )
    END IF
  ELSE IF( PRESENT( physicalTag )  .AND. PRESENT( dim ) ) THEN
    ans = obj%getIndex( dim=dim, tag=physicalTag )
    IF( ans .NE. 0 ) THEN
      ans = obj%numNodes( ans )
    ELSE
      CALL eDomain%raiseError(modName//'::'//myName//'-'// &
        & 'The physical tag '// trim(str(physicalTag, .true.)) &
        & // " and dimension " // trim(str(dim, .true.)) // " not found" )
    END IF
  ELSE IF( PRESENT( entityNum )  .AND. PRESENT( dim ) ) THEN
    IF( obj%meshList( dim )%isEmpty() ) &
      & CALL eDomain%raiseError(modName//'::'//myName//'-'// &
      & 'The mesh of enitityNum '// trim(str(entityNum, .true.)) &
      & // " and dimension " // trim(str(dim, .true.)) // " is empty." )
    IF( entityNum .GT. obj%meshList( dim )%size() ) &
      & CALL eDomain%raiseError(modName//'::'//myName//'-'// &
      & 'The the enitityNum '// trim(str(entityNum, .true.)) &
      & // " for dimension " // trim(str(dim, .true.)) // &
      & " is out of bound." )
    iterator = obj%meshList( dim )%Begin() + (entityNum - 1)
    ans = iterator%value%ptr%getTotalNodes()
  ELSE
    ans = obj%tNodes
  END IF

END PROCEDURE Domain_getTotalNodes

!----------------------------------------------------------------------------
!                                                     getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getLocalNodeNumber1
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getLocalNodeNumber"
  IF( globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs ) &
    & CALL eDomain%raiseError(modName//'::'//myName//'-'// &
    & "globalNode : "// trim(str(globalNode, .true.))//" is not present inside the domain" )
  ans = obj%local_nptrs( globalNode )
END PROCEDURE Domain_getLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                     getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getLocalNodeNumber2
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getLocalNodeNumber2"
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( globalNode )
    ans( ii ) = Domain_getLocalNodeNumber1( obj, globalNode(ii))
  END DO
END PROCEDURE Domain_getLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                              getTotalMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getTotalMesh
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getTotalMesh"
  IF( dim .LT. 0 .OR. dim .GT. 3 ) THEN
    CALL eDomain%raiseError( modName//"::"//myName//" - "// &
      & "dim should be in [0,1,2,3]" )
  END IF

  IF( obj%meshList( dim )%isEmpty() ) THEN
    ans=0
  ELSE
    ans = obj%meshList( dim )%size()
  END IF

END PROCEDURE Domain_getTotalMesh

!----------------------------------------------------------------------------
!                                                           getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getMeshPointer
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_getMeshPointer"
  INTEGER( I4B ) :: tsize, imesh
  TYPE( MeshPointerIterator_ ) :: iterator

  iterator%value%ptr => NULL()
  tsize = obj%getTotalMesh( dim=dim )
  IF( tag .GT. tsize ) THEN
    CALL eDomain%raiseInformation( modName//"::"//myName//" - "// &
      & "tag are out of bound" )
  END IF
  iterator = obj%meshList( dim )%Begin()
  DO imesh = 2, tag
    CALL iterator%Inc()
  END DO

  ans => iterator%value%ptr
  CALL iterator%DeallocateData()
END PROCEDURE Domain_getMeshPointer

!----------------------------------------------------------------------------
!                                                              getNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNodeCoord
  IF( ALLOCATED( obj%nodeCoord ) ) nodeCoord = obj%nodeCoord
END PROCEDURE Domain_getNodeCoord

!----------------------------------------------------------------------------
!                                                        getNodeCoordPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNodeCoordPointer
  ans => obj%nodeCoord
END PROCEDURE Domain_getNodeCoordPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE getMethods