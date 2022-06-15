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

SUBMODULE(VectorField_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set1
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_set1"
  INTEGER( I4B ) :: localNode
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  !!
  IF( SIZE( value ) .NE. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to obj%spaceCompo' )
  !!
#endif
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    !!
    IF( PRESENT( addContribution ) ) THEN
      CALL add( &
        & obj=obj%realVec, &
        & dofobj=obj%dof, &
        & nodenum=[1], &
        & value=value, &
        & conversion=[NONE], &
        & scale=scale )
    ELSE
      CALL set( &
        & obj=obj%realVec, &
        & dofobj=obj%dof, &
        & nodenum=[1], &
        & value=value, &
        & conversion=[NONE])
    END IF
    !!
  ELSE
    !!
    localNode = obj%domain%getLocalNodeNumber( globalNode )
    !!
#ifdef DEBUG_VER
    IF( obj%tSize .GE. localNode ) THEN
#endif
    !!
    IF( PRESENT( addContribution ) ) THEN
      CALL add( &
        & obj=obj%realVec, &
        & dofobj=obj%dof, &
        & nodenum=[localNode], &
        & value=value, &
        & conversion = [NONE], &
        & scale=scale )
      !!
    ELSE
      !!
      CALL set( &
        & obj=obj%realVec, &
        & dofobj=obj%dof, &
        & nodenum=[localNode], &
        & value=value, &
        & conversion = [NONE]  )
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
  END IF
END PROCEDURE vField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set2
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set2"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  !!
  IF( SIZE( value ) .NE. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'size(value) should be same as obj%spaceCompo' )
  !!
#endif
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    IF( PRESENT( addContribution ) ) THEN
      CALL add( &
        & obj=obj%realVec, &
        & dofobj=obj%dof, &
        & nodenum=[1], &
        & value=value, &
        & conversion=[NONE], &
        & scale=scale )
    ELSE
      CALL set( &
        & obj=obj%realVec, &
        & dofobj=obj%dof, &
        & nodenum=[1], &
        & value=value, &
        & conversion=[NONE])
    END IF
  ELSE
    vecPointer => NULL()
    IF( PRESENT( addContribution ) ) THEN
      DO idof = 1, obj%spaceCompo
        vecPointer => getPointer( obj%realVec, obj%dof, idof )
        vecPointer = vecPointer + scale* value( idof )
      END DO
    ELSE
      DO idof = 1, obj%spaceCompo
        vecPointer => getPointer( obj%realVec, obj%dof, idof )
        vecPointer = value( idof )
      END DO
    END IF
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
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
#endif
  !!
  tnodes = obj%domain%getTotalNodes()
  !!
#ifdef DEBUG_VER
  IF( SIZE( value,2) .NE. tnodes ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value should be [ ' &
    & // trim(str(obj%spaceCompo, .true.)) &
    & // ', ' &
    & // trim(str(tnodes, .true.)) &
    & // ' ]' )
#endif
  !!
  aa = 0
  !!
  IF( PRESENT( addContribution ) ) THEN
    DO ii = 1, tnodes
      DO jj = 1, obj%spaceCompo
        aa = aa + 1
        obj%realVec%val( aa ) =  obj%realVec%val( aa ) + scale*value( jj, ii )
      END DO
    END DO
  ELSE
    DO ii = 1, tnodes
      DO jj = 1, obj%spaceCompo
        aa = aa + 1
        obj%realVec%val( aa ) = value( jj, ii )
      END DO
    END DO
  END IF
END PROCEDURE vField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set5
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set5"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  !!
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
  !!
  IF( SIZE( value ) .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )
#endif
  !!
  vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
  !!
  IF( PRESENT( addContribution ) ) THEN
    vecPointer = vecPointer + scale*value
  ELSE
    vecPointer = value
  END IF
  !!
  vecPointer => NULL()
END PROCEDURE vField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set6
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set5"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated .OR. .NOT. value%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  !!
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
  !!
  IF( value%domain%getTotalNodes() .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )
#endif
  !!
  IF( value%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    vecPointer => getPointer( value%realVec, value%dof, 1 )
    CALL obj%set(value=vecPointer(1), spaceCompo=spaceCompo, &
      & scale=scale, addContribution=addContribution)
  ELSE
    vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
    IF( PRESENT( addContribution ) ) THEN
      vecPointer = vecPointer + get( value%realVec, 1.0_DFP)
    ELSE
      vecPointer = get( value%realVec, 1.0_DFP)
    END IF
    vecPointer => NULL()
  END IF
  !!
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
  CALL obj%set(value=val, globalNode=globalNode, scale=scale, &
    & addContribution=addContribution)
END PROCEDURE vField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set8
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_set8"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  REAL( DFP ) :: val( SIZE( value ) )
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine should not be called for constant vector field' )
  !!
  IF( SIZE( value, 1) .NE. obj%spaceCompo .OR. &
    & SIZE( value, 2 ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'SIZE( value, 1 ) not equal spaceCompo or SIZE( value, 2 ) not &
    & equal to the SIZE(globalNode)' )
#endif
  !!
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  !!
#ifdef DEBUG_VER
  IF( ANY( localNode .GT. obj%tSize ) ) &
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
END PROCEDURE vField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set9
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "vField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode( SIZE( globalNode ) )
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  !!
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
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
  vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
  !!
  IF( PRESENT( addContribution ) ) THEN
    vecPointer( localNode ) = vecPointer( localNode ) + scale*value
  ELSE
    vecPointer( localNode ) = value
  END IF
  !!
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
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Vector field object is not initiated' )
  !!
  IF( spaceCompo .GT. obj%spaceCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given spaceCompo should be less than or equal to obj%spaceCompo' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant vector field' )
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
  vecPointer => getPointer( obj%realVec, obj%dof, spaceCompo )
  !!
  IF( PRESENT( addContribution ) ) THEN
    vecPointer( localNode ) = vecPointer( localNode ) + scale * value
  ELSE
    vecPointer( localNode ) = value
  END IF
  !!
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
  CALL obj%set( globalNode=globalNode, value=value, scale=scale, &
    & addContribution=addContribution )
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
  CALL obj%set( globalNode=globalNode, value=value, scale=scale, &
    & addContribution=addContribution )
END PROCEDURE vField_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set13
  CHARACTER( LEN = * ), PARAMETER :: myName="vField_set13"
  !!
#ifdef DEBUG_VER
  !!
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine should not be called for constant vector field' )
  !!
  IF( SIZE( value, 1) .NE. obj%spaceCompo .OR. &
    & SIZE( value, 2 ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'SIZE( value, 1 ) not equal spaceCompo or SIZE( value, 2 ) not &
    & equal to the SIZE(globalNode)' )
#endif
  !!
  SELECT CASE( value%vartype )
  CASE( Constant )
    !!
    CALL obj%Set( &
      & value = GET(value, TypeFEVariableVector, TypeFEVariableConstant), &
      & globalNode=globalNode, &
      & scale=scale, &
      & addContribution=addContribution)
    !!
  CASE( Space )
    !!
    CALL obj%Set( &
      & value = GET(value, TypeFEVariableVector, TypeFEVariableSpace), &
      & globalNode=globalNode, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END SELECT
  !!
END PROCEDURE vField_set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_set14
  IF( PRESENT( addContribution ) ) THEN
    CALL Add( obj=obj%realvec, value=value, scale=scale )
  ELSE
    CALL Set( obj=obj%realvec, value=value )
  END IF
END PROCEDURE vField_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods