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

SUBMODULE( STScalarField_Class ) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get1
  INTEGER( I4B ) :: localNode
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_get1"

  IF( PRESENT( globalNode ) ) THEN
    SELECT CASE( obj%fieldType )
    CASE( FIELD_TYPE_CONSTANT )
      CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
        & dofNo = arange(1,obj%timeCompo), &
        & storageFMT=NODES_FMT, nptrs=[1] )
    CASE( FIELD_TYPE_NORMAL )
      localNode = obj%domain%getLocalNodeNumber( globalNode )
      !> check
      IF( localNode .GT. obj%domain%getTotalNodes() ) &
        & CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'The given global node num are out of bound' )
      CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
        & dofNo = arange(1,obj%timeCompo), &
        & storageFMT=NODES_FMT, nptrs=[localNode] )
    END SELECT
  ELSE IF (PRESENT( timeCompo ) ) THEN
    IF( timeCompo .GT. obj%timeCompo ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'given timeCompo should be less than or equal to obj%timeCompo' )
    CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
      & dofNo = [timeCompo], &
      & storageFMT=NODES_FMT )
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'either globalNode or space component should be present' )
  END IF
END PROCEDURE stsField_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get2
  CALL getValue( v=value, val=obj%realVec, obj=obj%dof, &
    & dofNo = arange(1,obj%timeCompo) )
END PROCEDURE stsField_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get3
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_get3"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  REAL( DFP ), ALLOCATABLE :: v( : )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
  CALL getValue( v=v, val=obj%realVec, obj=obj%dof, &
    & dofNo = arange(1,obj%timeCompo), &
    & storageFMT=NODES_FMT, nptrs=localNode )
  value = RESHAPE( v, [obj%timeCompo, SIZE( localNode ) ])
  IF( ALLOCATED( v ) ) DEALLOCATE( v )
END PROCEDURE stsField_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get4
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_get4"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  CALL getValue(v=value, val=obj%realVec, obj=obj%dof, &
    & dofNO=[timeCompo], storageFMT=NODES_FMT, nptrs=localNode)
END PROCEDURE stsField_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get5
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_get5"
  INTEGER( I4B ) :: localNode
  REAL( DFP ), ALLOCATABLE :: v( : )

  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )

  CALL obj%get( value = v, globalNode = globalNode )
  value = v( timeCompo )

END PROCEDURE stsField_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get6
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_get6"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%get( globalNode = globalNode, value=value )
END PROCEDURE stsField_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get7
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_get7"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%get( globalNode = globalNode, value=value, timeCompo=timeCompo )
END PROCEDURE stsField_get7

!----------------------------------------------------------------------------
!                                                                 getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_getPointer1
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_getPointer1"

  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  ans => getPointer( obj=obj%realVec, dofobj=obj%dof, dofno = timeCompo )
END PROCEDURE stsField_getPointer1

END SUBMODULE GetMethods