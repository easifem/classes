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

SUBMODULE( Mesh_Class ) setMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setSparsity1
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_setSparsity1"
  INTEGER( I4B ) :: i, j, k
  INTEGER( I4B ), allocatable :: n2n( : )

  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF
  DO i = 1, obj%tNodes
    j = obj%getGlobalNodeNumber( LocalNode = i )
    k = localNodeNumber( j )
    IF( k .NE. 0 ) THEN
      n2n = localNodeNumber( &
        & obj%getNodeToNodes( GlobalNode = j, IncludeSelf =.TRUE. ) )
      CALL setSparsity( obj = Mat, Row = k, Col = n2n )
    END IF
  END DO
  IF( ALLOCATED( n2n ) ) DEALLOCATE( n2n )
END PROCEDURE mesh_setSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_setSparsity2
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_setSparsity2"
  INTEGER( I4B ) :: i, j, idof, k
  INTEGER( I4B ), allocatable :: n2n( : )

  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF
  DO i = 1, obj%tNodes
    j = obj%getGlobalNodeNumber( LocalNode = i )
    n2n = obj%getNodeToNodes( GlobalNode = j, IncludeSelf =.TRUE. )
    CALL setSparsity( obj = Mat, Row = j, Col = n2n )
  END DO
  IF( ALLOCATED( n2n ) ) DEALLOCATE( n2n )
END PROCEDURE mesh_setSparsity2

END SUBMODULE setMethod