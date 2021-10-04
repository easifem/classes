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

SUBMODULE(STScalarField_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set1
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_set1"
  INTEGER( I4B ) :: localNode

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )

  IF( SIZE( value ) .NE. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to obj%timeCompo' )

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
END PROCEDURE stsField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set2
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set2"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  IF( SIZE( value ) .NE. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'size(value) should be same as obj%timeCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL set( obj%realVec, obj%dof, [1], value, [NONE]  )
  ELSE
    vecPointer => NULL()
    DO idof = 1, obj%timeCompo
      vecPointer => getPointer( obj%realVec, obj%dof, idof )
      vecPointer = value( idof )
    END DO
    vecPointer => NULL()
  END IF
END PROCEDURE stsField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set3
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set3"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL set( obj%realVec, obj%dof, [1], [value], timeCompo )
  ELSE
    vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
    vecPointer = value
    vecPointer => NULL()
  END IF
END PROCEDURE stsField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set4
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_set4"
  INTEGER( I4B ) :: ii, tnodes, aa, jj

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )

  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )

  tnodes = obj%domain%getTotalNodes()
  IF( SIZE( value,1) .NE. obj%timeCompo &
    & .OR. SIZE( value,2) .NE. tnodes ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value should be [ ' &
    & // trim(str(obj%timeCompo, .true.)) &
    & // ', ' &
    & // trim(str(tnodes, .true.)) &
    & // ' ]' )

  aa = 0
  DO ii = 1, tnodes
    DO jj = 1, obj%timeCompo
      aa = aa + 1
      obj%realVec%val( aa ) = value( jj, ii )
    END DO
  END DO
END PROCEDURE stsField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set5
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set5"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
  IF( SIZE( value ) .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )

  vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
  vecPointer = value
  vecPointer => NULL()
END PROCEDURE stsField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set6
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set5"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated .OR. .NOT. value%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
  IF( value%domain%getTotalNodes() .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )
  IF( value%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    vecPointer => getPointer( value%realVec, value%dof, 1 )
    CALL obj%set(value=vecPointer(1), timeCompo=timeCompo)
  ELSE
    vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
    vecPointer = get( value%realVec, 1.0_DFP)
    vecPointer => NULL()
  END IF
END PROCEDURE stsField_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set7
  REAL( DFP ) :: val( SIZE( value ), size( globalNode ) )
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( globalNode )
    val( :, ii ) = value( : )
  END DO
  CALL obj%set(value=val, globalNode=globalNode)
END PROCEDURE stsField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set8
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_set8"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  REAL( DFP ) :: val( SIZE( value ) )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine should not be called for constant STScalar field' )
  IF( SIZE( value, 1) .NE. obj%timeCompo .OR. &
    & SIZE( value, 2 ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'SIZE( value, 1 ) not equal timeCompo or SIZE( value, 2 ) not equal to the SIZE(globalNode)' )
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%tSize ) ) &
    & CALL e%raiseError( modName//'::'//myName// " - "// &
    & 'Some of the globalNode are out of bound' )
  val = RESHAPE( value, [ SIZE( value ) ] )
  CALL set( obj%realVec, obj%dof, localNode, val, [NONE]  )
END PROCEDURE stsField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set9
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode( SIZE( globalNode ) )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
  IF( SIZE( value ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to size of globalNode' )
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
  vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
  vecPointer( localNode ) = value
  vecPointer => NULL()
END PROCEDURE stsField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set10
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( localNode .GT. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The given global node num are out of bound' )
  vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
  vecPointer( localNode ) = value
  vecPointer => NULL()
END PROCEDURE stsField_set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set11
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_set11"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%set( globalNode=globalNode, value=value )
END PROCEDURE stsField_set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set12
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_set12"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%set( globalNode=globalNode, value=value )
END PROCEDURE stsField_set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods