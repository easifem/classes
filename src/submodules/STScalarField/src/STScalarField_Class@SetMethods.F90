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
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  !!
  IF( SIZE( value ) .NE. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to obj%timeCompo' )
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
        & conversion=[NONE] )
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
        & conversion=[NONE], &
        & scale=scale )
    ELSE
      CALL set( &
        & obj=obj%realVec, &
        & dofobj=obj%dof, &
        & nodenum=[localNode], &
        & value=value, &
        & conversion=[NONE])
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
!!
END PROCEDURE stsField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set2
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set2"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  !!
  IF( SIZE( value ) .NE. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'size(value) should be same as obj%timeCompo' )
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
    vecPointer => NULL()
    IF( PRESENT( addContribution ) ) THEN
      DO idof = 1, obj%timeCompo
        vecPointer => getPointer( obj%realVec, obj%dof, idof )
        vecPointer = vecPointer + scale * value( idof )
      END DO
    ELSE
      DO idof = 1, obj%timeCompo
        vecPointer => getPointer( obj%realVec, obj%dof, idof )
        vecPointer = value( idof )
      END DO
    END IF
    vecPointer => NULL()
  END IF
  !!
END PROCEDURE stsField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set3
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set3"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  !!
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
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
        & value=[value], &
        & idof=timeCompo, &
        & scale=scale )
    ELSE
      CALL set( &
        & obj=obj%realVec, &
        & dofobj=obj%dof, &
        & nodenum=[1], &
        & value=[value], &
        & idof=timeCompo )
    END IF
    !!
  ELSE
    !!
    vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
    !!
    IF( PRESENT( addContribution ) ) THEN
      vecPointer = vecPointer + scale*value
    ELSE
      vecPointer = value
    END IF
    !!
    vecPointer => NULL()
  END IF
!!
END PROCEDURE stsField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set4
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_set4"
  INTEGER( I4B ) :: ii, tnodes, aa, jj
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
#endif
  !!
  tnodes = obj%domain%getTotalNodes()
  !!
#ifdef DEBUG_VER
  IF( SIZE( value,1) .NE. obj%timeCompo &
    & .OR. SIZE( value,2) .NE. tnodes ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The shape of value should be [ ' &
    & // trim(str(obj%timeCompo, .true.)) &
    & // ', ' &
    & // trim(str(tnodes, .true.)) &
    & // ' ]' )
#endif
  !!
  aa = 0
  !!
  IF( PRESENT( addContribution ) ) THEN
    DO jj = 1, tnodes
      DO ii = 1, obj%timeCompo
        aa = aa + 1
        obj%realVec%val( aa ) =  obj%realVec%val( aa ) + scale*value( ii, jj )
      END DO
    END DO
  ELSE
    DO jj = 1, tnodes
      DO ii = 1, obj%timeCompo
        aa = aa + 1
        obj%realVec%val( aa ) = value( ii, jj )
      END DO
    END DO
  END IF
  !!
END PROCEDURE stsField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set5
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set5"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  !!
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
  !!
  IF( SIZE( value ) .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )
#endif
  !!
  vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
  IF( PRESENT( addContribution ) ) THEN
    vecPointer = vecPointer+scale*value
  ELSE
    vecPointer = value
  END IF
  vecPointer => NULL()
END PROCEDURE stsField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set6
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set5"
  INTEGER( I4B ) :: idof
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated .OR. .NOT. value%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  !!
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
  !!
  IF( value%domain%getTotalNodes() .NE. obj%domain%getTotalNodes() ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to the total number of nodes' )
  !!
#endif
  !!
  IF( value%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    vecPointer => getPointer( value%realVec, value%dof, 1 )
    CALL obj%set(value=vecPointer(1), timeCompo=timeCompo, &
      & scale=scale, addContribution=addContribution)
  ELSE
    vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
    IF( PRESENT( addContribution ) ) THEN
      vecPointer = vecPointer + scale * get( value%realVec, 1.0_DFP)
    ELSE
      vecPointer = get( value%realVec, 1.0_DFP)
    END IF
  END IF
  !!
  vecPointer => NULL()
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
  CALL obj%set( &
    & value=val, &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)
END PROCEDURE stsField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set8
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_set8"
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
    & 'This routine should not be called for constant STScalar field' )
  !!
  IF( SIZE( value, 1) .NE. obj%timeCompo .OR. &
    & SIZE( value, 2 ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'SIZE( value, 1 ) not equal timeCompo or SIZE( value, 2 ) not equal to &
    & the SIZE(globalNode)' )
  !!
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
      & obj = obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=localNode, &
      & value=val, &
      & conversion=[NONE], &
      & scale=scale )
  ELSE
    CALL set( &
      & obj = obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=localNode, &
      & value=val, &
      & conversion=[NONE])
  END IF
  !!
END PROCEDURE stsField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set9
  REAL( DFP ), POINTER :: vecPointer( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "stsField_set9"
  INTEGER( I4B ) :: idof
  INTEGER( I4B )  :: localNode( SIZE( globalNode ) )
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  !!
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
  !!
  IF( SIZE( value ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of value should be equal to size of globalNode' )
  !!
#endif
  !!
  localNode = obj%domain%getLocalNodeNumber( globalNode )
  !!
#ifdef DEBUG_VER
  IF( ANY( localNode .GT. obj%domain%getTotalNodes() )) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the global node num are out of bound' )
#endif
  vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
  IF( PRESENT( addContribution ) ) THEN
    vecPointer( localNode ) = vecPointer( localNode ) + scale * value
  ELSE
    vecPointer( localNode ) = value
  END IF
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
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'STScalar field object is not initiated' )
  !!
  IF( timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine is not callable for constant STScalar field' )
  !!
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
  vecPointer => getPointer( obj%realVec, obj%dof, timeCompo )
  IF( PRESENT( addContribution ) ) THEN
    vecPointer( localNode ) = vecPointer( localNode ) + scale*value
  ELSE
    vecPointer( localNode ) = value
  END IF
  !!
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
  CALL obj%set( globalNode=globalNode, value=value, &
    & scale=scale, addContribution=addContribution )
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
  CALL obj%set( globalNode=globalNode, value=value, &
    & scale=scale, addContribution=addContribution )
END PROCEDURE stsField_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set13
  CHARACTER( LEN = * ), PARAMETER :: myName="stsField_set13"
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine should not be called for constant STScalar field' )
  !!
  IF( SIZE( value, 1) .NE. obj%timeCompo .OR. &
    & SIZE( value, 2 ) .NE. SIZE( globalNode ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'SIZE( value, 1 ) not equal timeCompo or SIZE( value, 2 ) not equal &
    & to the SIZE(globalNode)' )
  !!
  IF( ANY( localNode .GT. obj%tSize ) ) &
    & CALL e%raiseError( modName//'::'//myName// " - "// &
    & 'Some of the globalNode are out of bound' )
#endif
  !!
  SELECT CASE( value%vartype )
  CASE( Constant )
    !!
    !!CALL obj%set( &
    !!  & value = GET(value, TypeFEVariableScalar, TypeFEVariableConstant), &
    !!  & globalNode=globalNode )
    !!
  CASE( SpaceTime )
    !!
    CALL obj%set( &
      & value = GET(value, TypeFEVariableScalar, TypeFEVariableSpaceTime), &
      & globalNode=globalNode, &
      & scale=scale, &
      & addContribution=addContribution )
    !!
  END SELECT
  !!
END PROCEDURE stsField_set13

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set14
  IF( PRESENT( addContribution ) ) THEN
    CALL Add( obj=obj%realvec, value=value, scale=scale )
  ELSE
    CALL Set( obj=obj%realvec, value=value )
  END IF
END PROCEDURE stsField_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods