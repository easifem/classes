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
  !!
  IF( PRESENT( globalnode ) ) THEN
    !!
    SELECT CASE( obj%fieldType )
    !!
    !!
    !!
    CASE( FIELD_TYPE_CONSTANT )
      !!
      CALL getValue( &
        & obj=obj%realvec, &
        & dofobj=obj%dof, &
        & idof = getIDOF(obj=obj%dof, ivar=1), &
        & value=value, &
        & storageFMT=NODES_FMT, &
        & nodenum=[1] )
      !!
      RETURN
    !!
    !!
    !!
    CASE( FIELD_TYPE_NORMAL )
      !!
      CALL getValue( &
        & obj=obj%realvec, &
        & dofobj=obj%dof, &
        & idof = getIDOF(obj=obj%dof, ivar=1), &
        & value=value, &
        & storageFMT=NODES_FMT, &
        & nodenum=obj%domain%getLocalNodeNumber( [globalnode] ) )
      !!
      RETURN
      !!
    END SELECT
    !!
  END IF
  !!
  !!
  !!
  !!
  IF( PRESENT( spacecompo ) .AND. PRESENT( timecompo ) ) THEN
    !!
    CALL getValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & ivar=1, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & value=value )
    !!
    RETURN
    !!
  END IF
  !!
  !!
  !!
  IF( PRESENT( spacecompo ) ) THEN
    !!
    CALL getValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=getIDOF( &
      & obj=obj%dof, &
      & ivar=1, &
      & spacecompo=spacecompo, &
      & timecompo=arange(1, obj%timecompo) ), &
      & storageFMT=NODES_FMT, &
      & value=value )
    !!
    RETURN
    !!
  END IF
  !!
  !!
  !!
  IF( PRESENT( timecompo ) ) THEN
    !!
    CALL getValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=getIDOF( &
      & obj=obj%dof, &
      & ivar=1, &
      & timecompo=timecompo, &
      & spacecompo=arange(1, obj%spacecompo) ), &
      & storageFMT=NODES_FMT, &
      & value=value )
    !!
    RETURN
    !!
  END IF
  !!
  !!
  !!
END PROCEDURE stvField_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get2
  !!
  value = RESHAPE( &
    & get( &
    & obj=obj%realVec, &
    & datatype=1.0_DFP ), &
    & [obj%spaceCompo, obj%timeCompo, obj%domain%getTotalNodes() ] )
  !!
END PROCEDURE stvField_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get3
  !!
  REAL( DFP ), ALLOCATABLE :: v( : )
  !!
  CALL getValue( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & idof = getIDOF(obj=obj%dof, ivar=1), &
    & value=v, &
    & storageFMT=NODES_FMT, &
    & nodenum=obj%domain%getLocalNodeNumber( globalNode ) )
  !!
  value = RESHAPE( v, [obj%spaceCompo, obj%timeCompo, SIZE( globalNode ) ])
  !!
  DEALLOCATE( v )
  !!
END PROCEDURE stvField_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get4
  !!
  CALL getValue( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & ivar=1, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & value=value, &
    & nodenum=obj%domain%getLocalNodeNumber( globalNode ))
  !!
END PROCEDURE stvField_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get5
  !!
  CALL getValue( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & ivar=1, &
    & idof=GetIDOF( &
    & obj=obj%dof, &
    & ivar=1, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ), &
    & value=value, &
    & nodenum=obj%domain%getLocalNodeNumber( globalNode ))
  !!
END PROCEDURE stvField_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get6
  !!
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  !!
  jj = 0
  !!
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  !!
  CALL obj%get( globalNode = globalNode, value=value )
  !!
END PROCEDURE stvField_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get7
  !!
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  !!
  jj = 0
  !!
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  !!
  CALL obj%get( &
    & globalNode = globalNode, &
    & value=value, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo )
  !!
END PROCEDURE stvField_get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get8
  !!
  REAL( DFP ), ALLOCATABLE :: val( :, :, : )
  !!
  CALL obj%get(value=val, globalNode=[globalNode])
  value = val( :, :, 1 )
  !!
  DEALLOCATE( val )
END PROCEDURE stvField_get8


!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get9
  REAL( DFP ), ALLOCATABLE :: m3a( :, :, : ), m3b( :, :, : )
  !!
  CALL obj%get( value=m3b, globalNode=globalNode )
  !!
  !! Here m3b is in (i, a, J) format,
  !! so we have to swap the dimensions to (i,J,a)
  !! We will call swap method from Utility.
  !!
  CALL SWAP(a=m3a, b=m3b, i1=1, i2=3, i3=2)
  !!
  value = NodalVariable( m3a, TypeFEVariableVector, &
    & TypeFEVariableSpacetime)
  !!
  DEALLOCATE( m3a, m3b )
END PROCEDURE stvField_get9

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get10
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_get10"
  INTEGER( I4B ) :: n, m
  !!
  !! check
  !!
  m = ( obj%dof .timecomponents. 1 )
  n = ( value%dof .timecomponents. 1)
  !!
  IF( m .NE. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & ( obj%dof .timecomponents. 1 )='//tostring(m)// &
    & ' is not equal to ( value%dof .timecomponents. 1)=' // tostring(n) )
  !!
  !! check
  !!
  n = (obj%dof .spacecomponents. 1)
  !!
  IF( spacecompo .GT. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .spacecomponents. 1)='//tostring(n)// &
    & ' is lesser than ' // &
    & ' spacecompo='//tostring(spacecompo) )
  !!
#endif
  !!
  !!
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idofobj= getIDOF(&
    & obj=obj%dof, &
    & ivar = 1, &
    & spacecompo=spacecompo, &
    & timecompo=arange(1, obj%timecompo)), &
    & value=value%realvec, &
    & dofvalue=value%dof, &
    & idofvalue=arange(1, obj%timecompo))
  !!
END PROCEDURE stvField_get10

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get11
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_get11"
  INTEGER( I4B ) :: n, m
  !!
  !! check
  !!
  m = ( obj%dof .spacecomponents. 1 )
  n = ( value%dof .spacecomponents. 1)
  !!
  IF( m .NE. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & ( obj%dof .spacecomponents. 1 )='//tostring(m)// &
    & ' is not equal to ( value%dof .spacecomponents. 1)=' // tostring(n) )
  !!
  !! check
  !!
  n = (obj%dof .timecomponents. 1)
  !!
  IF( timecompo .GT. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .timecomponents. 1)='//tostring(n)// &
    & ' is lesser than ' // &
    & ' timecompo='//tostring(timecompo) )
  !!
#endif
  !!
  !!
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idofobj= getIDOF(&
    & obj=obj%dof, &
    & ivar = 1, &
    & spacecompo=arange(1, obj%spacecompo), &
    & timecompo=timecompo), &
    & value=value%realvec, &
    & dofvalue=value%dof, &
    & idofvalue=arange(1, obj%spacecompo))
  !!
END PROCEDURE stvField_get11

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_get12
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="stvField_get12"
  INTEGER( I4B ) :: n
  !!
  !! check
  !!
  n = (obj%dof .timecomponents. 1)
  !!
  IF( timecompo .GT. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .timecomponents. 1)='//tostring(n)// &
    & ' is lesser than ' // &
    & ' timecompo='//tostring(timecompo) )
  !!
  !!
  !! check
  !!
  n = (obj%dof .spacecomponents. 1)
  !!
  IF( spacecompo .GT. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .spacecomponents. 1)='//tostring(n)// &
    & ' is lesser than ' // &
    & ' spacecompo='//tostring(spacecompo) )
  !!
#endif
  !!
  !!
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof= getIDOF(&
    & obj=obj%dof, &
    & ivar = 1, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value%realvec )
  !!
END PROCEDURE stvField_get12

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_getPointerOfComponent
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName = "stvField_getPointerOfComponent"
  !!
  IF( spaceCompo .GT. obj%spaceCompo .OR. timeCompo .GT. obj%timeCompo ) &
    & CALL e%raiseError( modName//'::'//myName// " - "// &
    & 'given spaceCompo or timeCompo should be less than or equal to obj%spaceCompo or obj%timeCompo' )
#endif
  !!
  ans => getPointer( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & idof = GetIDOF( &
    & obj=obj%dof, &
    & ivar=1, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ))
  !!
END PROCEDURE stvField_getPointerOfComponent

END SUBMODULE GetMethods
