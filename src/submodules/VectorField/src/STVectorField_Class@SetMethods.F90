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

SUBMODULE(STVectorField_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set1
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_set1"
  INTEGER( I4B ) :: localNode

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )

  IF( ANY(SHAPE( value ) .NE. [obj%spaceCompo, obj%timeCompo]) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value is not compatible, it should be equal to [obj%spaceCompo, obj%timeCompo]' )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( localNode .NE. 0 ) THEN
    CALL set( obj%realVec, obj%dof, [localNode], &
      & RESHAPE( value, [size(value)] ), [NONE]  )
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - " &
    & // 'globalNode :: '// trim(str(globalNode, .true.)) &
    & // " is out of bound for the domain." )
  END IF
END PROCEDURE stvField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set2
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set2"
  INTEGER( I4B ) :: ii, aa, idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )

  IF( ANY(SHAPE( value ) .NE. [obj%spaceCompo, obj%timeCompo]) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value is not compatible, it should be equal to [obj%spaceCompo, obj%timeCompo]' )

  idof =  0
  DO aa = 1, obj%timeCompo
    DO ii = 1, obj%spaceCompo
      idof = idof + 1
      vecPointer => getPointer( obj%realVec, obj%dof, idof )
      vecPointer = value( ii, aa )
    END DO
  END DO
  vecPointer => NULL()
END PROCEDURE stvField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set3
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set3"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )

  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal to obj%spaceCompo and obj%timeCompo' )

  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer( obj%realVec, obj%dof, idof )
  vecPointer = value
  vecPointer => NULL()
END PROCEDURE stvField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set4
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_set4"
  INTEGER( I4B ) :: ii, tnodes, aa, jj
  REAL( DFP ), ALLOCATABLE :: vec( : )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )

  tnodes = obj%domain%getTotalNodes()
  IF( ANY( SHAPE( value) .NE. [obj%spaceCompo, obj%timeCompo, tNodes] ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value is not compatible' )

  obj%realVec = RESHAPE( value, [SIZE( value )] )
  IF( ALLOCATED( vec ) ) DEALLOCATE( vec )
END PROCEDURE stvField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set5
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set5"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )

  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal to obj%spaceCompo and obj%timeCompo' )

  IF( SIZE( value ) .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )

  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer( obj%realVec, obj%dof, idof )
  vecPointer = value
  vecPointer => NULL()
END PROCEDURE stvField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set6
  REAL( DFP ), POINTER :: vecPointer( : )
  REAL( DFP ) :: vec( 1 )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set5"
  INTEGER( I4B ) :: idof

  IF( .NOT. obj%isInitiated .OR. .NOT. value%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )

  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal to obj%spaceCompo and obj%timeCompo' )

  IF( value%domain%getTotalNodes() .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )

  IF( value%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
    vecPointer => getPointer( obj%realVec, obj%dof, idof )
    vec = get(obj=value%realVec, indx=[1], datatype=1.0_DFP)
    vecPointer = vec( 1 )
  ELSE
    idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
    vecPointer => getPointer( obj%realVec, obj%dof, idof )
    vecPointer = get( value%realVec, 1.0_DFP)
    vecPointer => NULL()
  END IF
END PROCEDURE stvField_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set7
  REAL( DFP ) :: val( SIZE( value,1 ), SIZE( value,2 ), SIZE( globalNode ) )
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( globalNode )
    val( :, :, ii ) = value( :, : )
  END DO
  CALL obj%set(value=val, globalNode=globalNode)
END PROCEDURE stvField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set8
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_set8"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  REAL( DFP ) :: val( SIZE( value ) )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )

  IF( ANY( SHAPE( value ) .NE. [ obj%spaceCompo, obj%timeCompo, &
    & SIZE(globalNode) ] ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Incompatible shape and size of value' )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .EQ. 0 ) ) &
    & CALL e%raiseError( modName//'::'//myName// " - "// &
    & 'Some of the globalNode are out of bound' )

  val = RESHAPE( value, [ SIZE( value ) ] )
  CALL set( obj%realVec, obj%dof, localNode, val, [NONE]  )
END PROCEDURE stvField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set9
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode( SIZE( globalNode ) )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )

  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal to obj%spaceCompo and obj%timeCompo' )

  IF( SIZE( value ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to size of globalNode' )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )

  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer( obj%realVec, obj%dof, idof )
  vecPointer( localNode ) = value
  vecPointer => NULL()
END PROCEDURE stvField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set10
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )

  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal to obj%spaceCompo and obj%timeCompo' )

  localNode = obj%domain%getLocalNodeNumber( globalNode )
  IF( localNode .GT. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The given global node num are out of bound' )

  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer( obj%realVec, obj%dof, idof )
  vecPointer( localNode ) = value
  vecPointer => NULL()
END PROCEDURE stvField_set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set11
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_set11"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%set( globalNode=globalNode, value=value )
END PROCEDURE stvField_set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set12
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_set12"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  CALL obj%set( globalNode=globalNode, value=value )
END PROCEDURE stvField_set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods