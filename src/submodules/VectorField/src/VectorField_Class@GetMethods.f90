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

SUBMODULE( VectorField_Class ) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get1
  INTEGER( I4B ) :: localNode
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_get1"

  IF( PRESENT( globalNode ) ) THEN
    SELECT CASE( obj%fieldType )
    CASE( FIELD_TYPE_CONSTANT )
      CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
        & dofNo = arange(1,obj%spaceCompo), &
        & storageFMT=NODES_FMT, nptrs=[1] )
    CASE( FIELD_TYPE_NORMAL )
      localNode = obj%domain%getLocalNodeNumber( globalNode )
      !> check
      IF( localNode .GT. obj%domain%getTotalNodes() ) &
        & CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'The given global node num are out of bound' )
      CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
        & dofNo = arange(1,obj%spaceCompo), &
        & storageFMT=NODES_FMT, nptrs=[localNode] )
    END SELECT
  ELSE IF (PRESENT( spaceCompo ) ) THEN
    IF( spaceCompo .GT. obj%spaceCompo ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
    CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
      & dofNo = [spaceCompo], &
      & storageFMT=NODES_FMT )
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'either globalNode or space component should be present' )
  END IF
END PROCEDURE vField_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get2
  CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
    & dofNo = arange(1,obj%spaceCompo) )
END PROCEDURE vField_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get3
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_get3"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  REAL( DFP ), ALLOCATABLE :: v( : )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
  CALL getValue( v=v, val=obj%realVec, obj=obj%dof, &
    & dofNo = arange(1,obj%spaceCompo), &
    & storageFMT=NODES_FMT, nptrs=localNode )
  value = RESHAPE( v, [obj%spaceCompo, SIZE( localNode ) ])
  IF( ALLOCATED( v ) ) DEALLOCATE( v )
END PROCEDURE vField_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get4
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_get4"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  CALL getValue(v=value, val=obj%realVec, obj=obj%dof, &
    & dofNO=[spaceCompo], storageFMT=NODES_FMT, nptrs=localNode)
END PROCEDURE vField_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get5
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_get5"
  INTEGER( I4B ) :: localNode
  REAL( DFP ), ALLOCATABLE :: v( : )

  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )

  CALL obj%get( value = v, globalNode = globalNode )
  value = v( spaceCompo )

END PROCEDURE vField_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get6
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_get6"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%get( globalNode = globalNode, value=value )
END PROCEDURE vField_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get7
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_get7"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%get( globalNode = globalNode, value=value, spaceCompo=spaceCompo )
END PROCEDURE vField_get7

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_getPointerOfComponent
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_getPointerOfComponent"

  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  ans => getPointer( obj=obj%realVec, dofobj=obj%dof, dofno = spaceCompo )
END PROCEDURE vField_getPointerOfComponent

END SUBMODULE GetMethods