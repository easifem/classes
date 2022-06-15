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
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )
  !!
  IF( ANY(SHAPE( value ) .NE. [obj%spaceCompo, obj%timeCompo]) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value is not compatible, it should be equal &
    & to [obj%spaceCompo, obj%timeCompo]' )
#endif
  !!
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  !!
#ifdef DEBUG_VER
  IF( localNode .NE. 0 ) THEN
#endif
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[localNode], &
      & value=RESHAPE( value, [size(value)] ), &
      & conversion=[NONE], &
      & scale=scale )
    !!
  ELSE
    !!
    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[localNode], &
      & value=RESHAPE( value, [size(value)] ), &
      & conversion=[NONE] )
    !!
  END IF
  !!
#ifdef DEBUG_VER
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - " &
    & // 'globalNode :: '// trim(str(globalNode, .true.)) &
    & // " is out of bound for the domain." )
  END IF
#endif
  !!
END PROCEDURE stvField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set2
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set2"
  INTEGER( I4B ) :: ii, aa, idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )
  !!
  IF( ANY(SHAPE( value ) .NE. [obj%spaceCompo, obj%timeCompo]) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value is not compatible, it should be equal &
    & to [obj%spaceCompo, obj%timeCompo]' )
#endif
  !!
  idof =  0
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    DO aa = 1, obj%timeCompo
      DO ii = 1, obj%spaceCompo
        idof = idof + 1
        vecPointer => getPointer( obj%realVec, obj%dof, idof )
        vecPointer = vecPointer + scale * value( ii, aa )
      END DO
    END DO
    !!
  ELSE
    !!
    DO aa = 1, obj%timeCompo
      DO ii = 1, obj%spaceCompo
        idof = idof + 1
        vecPointer => getPointer( obj%realVec, obj%dof, idof )
        vecPointer = value( ii, aa )
      END DO
    END DO
    !!
  END IF
  !!
  vecPointer => NULL()
END PROCEDURE stvField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set3
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set3"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  !!
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )
  !!
  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal &
    & to obj%spaceCompo and obj%timeCompo' )
#endif
  !!
  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer( obj%realVec, obj%dof, idof )
  !!
  IF( PRESENT( addContribution ) ) THEN
    vecPointer = vecPointer + scale * value
  ELSE
    vecPointer = value
  END IF
  !!
  vecPointer => NULL()
END PROCEDURE stvField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set4
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_set4"
  INTEGER( I4B ) :: ii, tnodes, aa, jj
  REAL( DFP ), ALLOCATABLE :: vec( : )
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )
#endif
  !!
  tnodes = obj%domain%getTotalNodes()
  !!
#ifdef DEBUG_VER
  IF( ANY( SHAPE( value) .NE. [obj%spaceCompo, obj%timeCompo, tNodes] ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value is not compatible' )
#endif
  !!
  vec = RESHAPE( value, [SIZE( value )] )
  !!
  IF( PRESENT( addContribution ) ) THEN
    CALL Add( obj=obj%realVec, value=vec, scale=scale )
  ELSE
    CALL Set( obj=obj%realVec, value=vec )
  END IF
  !!
  IF( ALLOCATED( vec ) ) DEALLOCATE( vec )
END PROCEDURE stvField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set5
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set5"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )
  !!
  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal &
    & to obj%spaceCompo and obj%timeCompo' )
  !!
  IF( SIZE( value ) .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )
#endif
  !!
  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer( obj%realVec, obj%dof, idof )
  !!
  IF( PRESENT( addContribution ) ) THEN
    vecPointer = vecPointer + scale * value
  ELSE
    vecPointer = value
  END IF
  !!
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
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated .OR. .NOT. value%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )
  !!
  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal &
    & to obj%spaceCompo and obj%timeCompo' )
  !!
  IF( value%domain%getTotalNodes() .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )
#endif
  !!
  IF( value%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    !!
    idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
    vecPointer => getPointer( obj%realVec, obj%dof, idof )
    vec = get(obj=value%realVec, nodenum=[1], datatype=1.0_DFP)
    IF( PRESENT( addContribution ) ) THEN
      vecPointer = vecPointer + scale * vec( 1 )
    ELSE
      vecPointer = vec( 1 )
    END IF
    !!
  ELSE
    !!
    idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
    vecPointer => getPointer( obj%realVec, obj%dof, idof )
    IF( PRESENT( addContribution ) ) THEN
      vecPointer = vecPointer + scale * get( value%realVec, 1.0_DFP)
    ELSE
      vecPointer = get( value%realVec, 1.0_DFP)
    END IF
    !!
  END IF
  !!
  vecPointer => NULL()
  !!
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
  CALL obj%set(value=val, globalNode=globalNode, scale=scale, &
    & addContribution=addContribution)
END PROCEDURE stvField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set8
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_set8"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  REAL( DFP ) :: val( SIZE( value ) )
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  !!
  IF( ANY( SHAPE( value ) .NE. [ obj%spaceCompo, obj%timeCompo, &
    & SIZE(globalNode) ] ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Incompatible shape and size of value' )
#endif
  !!
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  !!
#ifdef DEBUG_VER
  IF( ANY( localNode .EQ. 0 ) ) &
    & CALL e%raiseError( modName//'::'//myName// " - "// &
    & 'Some of the globalNode are out of bound' )
#endif
  !!
  val = RESHAPE( value, [ SIZE( value ) ] )
  !!
  IF( PRESENT( addContribution ) ) THEN
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=localNode, &
      & value=val, &
      & conversion=[NONE], &
      & scale=scale )
  ELSE
    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=localNode, &
      & value=val, &
      & conversion=[NONE])
  END IF
  !!
END PROCEDURE stvField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set9
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode( SIZE( globalNode ) )
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )
  !!
  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal &
    & to obj%spaceCompo and obj%timeCompo' )
  !!
  IF( SIZE( value ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to size of globalNode' )
#endif
  !!
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  !!
#ifdef DEBUG_VER
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
#endif
  !!
  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer( obj%realVec, obj%dof, idof )
  !!
  IF( PRESENT( addContribution ) ) THEN
    vecPointer( localNode ) = vecPointer( localNode )+scale*value
  ELSE
    vecPointer( localNode ) = value
  END IF
  !!
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
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STVector field object is not initiated' )
  !!
  IF( ANY( [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo] )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo and timeCompo should be less than or equal &
    & to obj%spaceCompo and obj%timeCompo' )
#endif
  !!
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  !!
#ifdef DEBUG_VER
  IF( localNode .GT. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The given global node num are out of bound' )
#endif
  !!
  idof = ( timeCompo - 1 ) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer( obj%realVec, obj%dof, idof )
  !!
  IF( PRESENT( addContribution ) ) THEN
    vecPointer( localNode ) = vecPointer( localNode )+scale*value
  ELSE
    vecPointer( localNode ) = value
  END IF
  !!
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
  CALL obj%set( &
    & globalNode=globalNode, &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
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
  CALL obj%set( &
    & globalNode=globalNode, &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
END PROCEDURE stvField_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set13
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_set13"
  !!
  SELECT CASE(value%vartype)
  CASE( Constant )
    !!
    !! CALL obj%set( &
    !!  & value=GET(value, TypeFEVariableVector, &
    !!  & TypeFEVariableConstant), &
    !!  & globalNode=globalNode )
    !!
  CASE( SpaceTime )
    !!
    CALL obj%set( &
      & value=GET(value, TypeFEVariableVector, &
      & TypeFEVariableSpaceTime), &
      & globalNode=globalNode, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END SELECT
  !!
END PROCEDURE stvField_set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set14
  IF( PRESENT( addContribution ) ) THEN
    CALL Add( obj=obj%realvec, value=value, scale=scale )
  ELSE
    CALL Set( obj=obj%realvec, value=value )
  END IF
END PROCEDURE stvField_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods