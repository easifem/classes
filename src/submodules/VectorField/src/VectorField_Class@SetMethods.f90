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

SUBMODULE( VectorField_Class ) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set1
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_set1"
  INTEGER( I4B ) :: localNode

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )

  IF( SIZE( value ) .NE. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to obj%spaceCompo' )

  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL set( obj%realVec, obj%dof, [1], value, [NONE]  )
  ELSE
    localNode = obj%domain%getLocalNodeNumber( globalNode )
    IF( obj%tSize .GE. localNode ) THEN
      CALL set( obj%realVec, obj%dof, [localNode], value, [NONE]  )
    ELSE
      CALL e%raiseError(modName//'::'//myName// " - " &
      & // 'globalNode :: '// trim(str(globalNode, .true.)) &
      & // " is out of bound for the domain." )
    END IF
  END IF
END PROCEDURE vField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set2
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set2"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  IF( SIZE( value ) .NE. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'size(value) should be same as obj%spaceCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL set( obj%realVec, obj%dof, [1], value, [NONE]  )
  ELSE
    vecPointer => NULL()
    DO idof = 1, obj%spaceCompo
      vecPointer => getPointer( obj%realVec, obj%dof, idof )
      vecPointer = value( idof )
    END DO
    vecPointer => NULL()
  END IF
END PROCEDURE vField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set3
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set3"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL set( obj%realVec, obj%dof, [1], [value], spaceCompo )
  ELSE
    vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
    vecPointer = value
    vecPointer => NULL()
  END IF
END PROCEDURE vField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set4
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_set4"
  INTEGER( I4B ) :: ii, tnodes, aa, jj

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )

  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )

  tnodes = obj%domain%getTotalNodes()
  IF( SIZE( value,1) .NE. obj%spaceCompo &
    & .OR. SIZE( value,2) .NE. tnodes ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value should be [ ' &
    & // trim(str(obj%spaceCompo, .true.)) &
    & // ', ' &
    & // trim(str(tnodes, .true.)) &
    & // ' ]' )

  aa = 0
  DO ii = 1, tnodes
    DO jj = 1, obj%spaceCompo
      aa = aa + 1
      obj%realVec%val( aa ) = value( jj, ii )
    END DO
  END DO
END PROCEDURE vField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set5
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set5"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
  IF( SIZE( value ) .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )

  vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
  vecPointer = value
  vecPointer => NULL()
END PROCEDURE vField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set6
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set5"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated .OR. .NOT. value%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
  IF( value%domain%getTotalNodes() .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )
  IF( value%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    vecPointer => getPointer( value%realVec, value%dof, 1 )
    CALL obj%set(value=vecPointer(1), spaceCompo=spaceCompo)
  ELSE
    vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
    vecPointer = get( value%realVec, 1.0_DFP)
    vecPointer => NULL()
  END IF
END PROCEDURE vField_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set7
  REAL( DFP ) :: val( SIZE( value ), size( globalNode ) )
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( globalNode )
    val( :, ii ) = value( : )
  END DO
  CALL obj%set(value=val, globalNode=globalNode)
END PROCEDURE vField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set8
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_set8"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  REAL( DFP ) :: val( SIZE( value ) )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine should not be called for constant vector field' )
  IF( SIZE( value, 1) .NE. obj%spaceCompo .OR. &
    & SIZE( value, 2 ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'SIZE( value, 1 ) not equal spaceCompo or SIZE( value, 2 ) not equal to the SIZE(globalNode)' )
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%tSize ) ) &
    & CALL e%raiseError( modName//'::'//myName// " - "// &
    & 'Some of the globalNode are out of bound' )
  val = RESHAPE( value, [ SIZE( value ) ] )
  CALL set( obj%realVec, obj%dof, localNode, val, [NONE]  )
END PROCEDURE vField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set9
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode( SIZE( globalNode ) )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
  IF( SIZE( value ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to size of globalNode' )
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
  vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
  vecPointer( localNode ) = value
  vecPointer => NULL()
END PROCEDURE vField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set10
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( localNode .GT. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The given global node num are out of bound' )
  vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
  vecPointer( localNode ) = value
  vecPointer => NULL()
END PROCEDURE vField_set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set11
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_set11"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%set( globalNode=globalNode, value=value )
END PROCEDURE vField_set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set12
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_set12"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%set( globalNode=globalNode, value=value )
END PROCEDURE vField_set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods