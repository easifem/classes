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

SUBMODULE(STVectorField_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get1
  ! INTEGER( I4B ) :: localNode
  ! CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_get1"

  ! IF( PRESENT( globalNode ) ) THEN
  !   SELECT CASE( obj%fieldType )
  !   CASE( FIELD_TYPE_CONSTANT )
  !     CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
  !       & dofNo = arange(1,obj%spaceCompo), &
  !       & storageFMT=NODES_FMT, nptrs=[1] )
  !   CASE( FIELD_TYPE_NORMAL )
  !     localNode = obj%domain%getLocalNodeNumber( globalNode )
  !     !> check
  !     IF( localNode .GT. obj%domain%getTotalNodes() ) &
  !       & CALL e%raiseError(modName//'::'//myName// " - "// &
  !       & 'The given global node num are out of bound' )
  !     CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
  !       & dofNo = arange(1,obj%spaceCompo), &
  !       & storageFMT=NODES_FMT, nptrs=[localNode] )
  !   END SELECT
  ! ELSE IF (PRESENT( spaceCompo ) ) THEN
  !   IF( spaceCompo .GT. obj%spaceCompo ) &
  !     & CALL e%raiseError(modName//'::'//myName// " - "// &
  !     & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  !   CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
  !     & dofNo = [spaceCompo], &
  !     & storageFMT=NODES_FMT )
  ! ELSE
  !   CALL e%raiseError(modName//'::'//myName// " - "// &
  !     & 'either globalNode or space component should be present' )
  ! END IF
END PROCEDURE stvField_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get2
  value = RESHAPE( get( obj=obj%realVec, datatype=1.0_DFP ), [obj%spaceCompo, obj%timeCompo, obj%domain%getTotalNodes() ] )
END PROCEDURE stvField_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get3
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_get3"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  REAL( DFP ), ALLOCATABLE :: v( : )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .EQ. 0 )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
  CALL getValue( v=v, val=obj%realVec, obj=obj%dof, &
    & dofNo = arange(1,obj%spaceCompo*obj%timeCompo), &
    & storageFMT=NODES_FMT, nptrs=localNode )
  value = RESHAPE( v, [obj%spaceCompo, obj%timeCompo, SIZE( localNode ) ])
  IF( ALLOCATED( v ) ) DEALLOCATE( v )
END PROCEDURE stvField_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get4
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_get4"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) ), idof

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .EQ. 0 ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
  IF( spaceCompo .GT. obj%spaceCompo .OR. timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo or timeCompo should be less than or equal to obj%spaceCompo or obj%timeCompo' )
  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  CALL getValue(v=value, val=obj%realVec, obj=obj%dof, &
    & dofNO=[idof], storageFMT=NODES_FMT, nptrs=localNode)
END PROCEDURE stvField_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get5
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_get5"
  INTEGER( I4B ) :: localNode
  REAL( DFP ), ALLOCATABLE :: v( :, : )

  IF( spaceCompo .GT. obj%spaceCompo .OR. timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError( modName//'::'//myName// " - "// &
    & 'given spaceCompo or timeCompo should be less than or equal to obj%spaceCompo or obj%timeCompo' )
  CALL obj%get( value = v, globalNode = globalNode )
  value = v( spaceCompo, timeCompo )
  IF( ALLOCATED( v ) ) DEALLOCATE( v )
END PROCEDURE stvField_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get6
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_get6"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%get( globalNode = globalNode, value=value )
END PROCEDURE stvField_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get7
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_get7"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%get( globalNode = globalNode, value=value, &
    & spaceCompo=spaceCompo, timeCompo=timeCompo )
END PROCEDURE stvField_get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get8
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_get8"
  REAL( DFP ), ALLOCATABLE :: val( :, :, : )
  CALL obj%get(value=val, globalNode=[globalNode])
  value = val( :, :, 1 )
  IF( ALLOCATED( val ) ) DEALLOCATE( val )
END PROCEDURE stvField_get8

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_getPointerOfComponent
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_getPointerOfComponent"
  INTEGER( I4B ) :: idof
  IF( spaceCompo .GT. obj%spaceCompo .OR. timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError( modName//'::'//myName// " - "// &
    & 'given spaceCompo or timeCompo should be less than or equal to obj%spaceCompo or obj%timeCompo' )
  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  ans => getPointer( obj=obj%realVec, dofobj=obj%dof, dofno = idof )
END PROCEDURE stvField_getPointerOfComponent

END SUBMODULE GetMethods