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

SUBMODULE(Mesh_Class) ElementDataMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateElementToElements
  ! Define internal  variables
  INTEGER( I4B ) :: i, j, r,  iel1, tFace, iFace1, NNS1, pt1, &
    & iel2, iFace2, NNS2, localElem1
  INTEGER( I4B ), ALLOCATABLE :: global_nptrs1( : ),   &
    & global_nptrsFace1( : ), n2e1( : ), global_nptrs2( : ), &
    & global_nptrsFace2( : )
  LOGICAL( LGT ) :: Found
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateElementToElements"
  !!
  !! check
  !!
  IF( .NOT. ASSOCIATED( obj%refelem ) ) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Unable to identify the Reference element of the mesh, &
      & may be it is not set" )
  END IF
  !!
  !! check
  !!
  IF( obj%isElementToElementsInitiated ) THEN
    CALL e%raiseWarning(modName//"::"//myName//" - "// &
      & "element to element information is already initiated. If you want to &
      & Reinitiate it then deallocate nodeData, first!!" )
    RETURN
  END IF
  !!
  IF( .NOT. ALLOCATED( obj%FacetElements ) ) THEN
    obj%FacetElements = FacetElements( obj%refelem )
  END IF
  !!
  tFace = SIZE(obj%FacetElements)
    !! Total number of facet elements
  obj%isElementToElementsInitiated = .TRUE.
  !!
  IF( .NOT. obj%isNodeToElementsInitiated ) THEN
    CALL obj%InitiateNodeToElements()
  END IF
  !!
  DO localElem1 = 1, obj%tElements
    iel1 = obj%getGlobalElemNumber( localElem1 )
    global_nptrs1 = obj%getConnectivity(iel1)
    !!
    !! getting node numbers of element iel1
    !!
    DO iFace1 = 1, tFace
      FOUND = .FALSE.
      global_nptrsFace1 = global_nptrs1( getConnectivity( &
        & obj%FacetElements( iFace1) ) )
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
          global_nptrsFace2 = global_nptrs2( &
            & getConnectivity( obj%FacetElements(iFace2)) )
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
            CALL APPEND( obj%elementData( localElem1 )%globalElements, &
              & [n2e1(iel2), iFace1, iFace2] )
            FOUND = .TRUE.
            EXIT
          END IF
        END DO
        IF( FOUND ) EXIT
      END DO
    END DO
    !!
    IF( INT( SIZE( obj%elementData( localElem1 )%globalElements ) / 3 ) &
      & .NE. tFace ) THEN
      obj%elementData( localElem1 )%elementType = BOUNDARY_ELEMENT
    END IF
  END DO
  !!
  IF( ALLOCATED( global_nptrs1 ) ) DEALLOCATE( global_nptrs1 )
  IF( ALLOCATED( global_nptrs2 ) ) DEALLOCATE( global_nptrs2 )
  IF( ALLOCATED( global_nptrsFace1 ) ) DEALLOCATE( global_nptrsFace1 )
  IF( ALLOCATED( global_nptrsFace2 ) ) DEALLOCATE( global_nptrsFace2 )
  IF( ALLOCATED( n2e1 ) ) DEALLOCATE( n2e1 )
END PROCEDURE mesh_InitiateElementToElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ElementDataMethods