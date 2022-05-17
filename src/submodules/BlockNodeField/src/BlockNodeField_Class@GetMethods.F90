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

SUBMODULE(BlockNodeField_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get1
  !!
  value = Get( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & idof=idof )
  !!
END PROCEDURE bnField_get1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get2
  value = Get( obj=obj%realVec )
END PROCEDURE bnField_get2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get3
  !!
  value = Get( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & idof=idof )
  !!
END PROCEDURE bnField_get3

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get4
  !!
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  !!
#ifdef DEBUG_VER
  !!
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_get4"
  !!
  !! check
  !!
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Field object is not initiated' )
  !!
  !! check
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'this routine is not callable for constant field type')
#endif
  !!
  jj = 0
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  !!
  CALL obj%get( &
    & globalNode=globalNode, &
    & value=value, &
    & ivar=ivar, &
    & idof=idof )
  !!
END PROCEDURE bnField_get4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get5
  !!
  value = NodalVariable( &
    & Get( obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode ), &
    & ivar=ivar, &
    & idof=idof ), &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpace )
  !!
END PROCEDURE bnField_get5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get6
  INTEGER( I4B ) :: localNode( size(globalNode) )
  INTEGER( I4B ) :: timeCompo, spaceCompo
  REAL( DFP ), ALLOCATABLE :: m3a( :, :, : ), m3b( :, :, : )
  !!
  timeCompo = obj%dof .TimeComponents. ivar
  spaceCompo = obj%dof .SpaceComponents. ivar
  localNode = obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode )
  !!
  !!
  !!
  !!
  IF( spaceCompo .GT. 1 ) THEN
    !!
    IF( timeCompo .GT. 1 ) THEN
      !!
      !! vector space-time
      !!
      m3b = RESHAPE( Get( obj=obj%realVec, dofobj=obj%dof, &
        & nodenum=localNode, ivar=ivar ), &
        & [size(localNode),spaceCompo,timeCompo])
      !!
      !! Here m3b is in (J, i, a) format, but we need (i,J,a) format
      !!
      CALL SWAP( a=m3a, b=m3b, i1=2, i2=1, i3=3 )
      !!
      value = NodalVariable( m3a, TypeFEVariableVector, &
        & TypeFEVariableSpaceTime )
      !!
    ELSE
      !!
      !! vector space
      !!
      value = NodalVariable( &
        & TRANSPOSE( RESHAPE( Get( obj=obj%realVec, dofobj=obj%dof, &
        & nodenum=localNode, ivar=ivar ), &
        & [size(localNode),spaceCompo]) ), &
        & TypeFEVariableVector, TypeFEVariableSpace )
      !!
    END IF
  !!
  !!
  !!
  !!
  ELSE
    !!
    IF( timeCompo .GT. 1 ) THEN
      !!
      !! scalar space-time
      !!
      value = NodalVariable(  &
        & RESHAPE( Get( obj=obj%realVec, dofobj=obj%dof, &
        & nodenum=localNode, ivar=ivar ), &
        & [size(localNode), timeCompo]),  &
        & TypeFEVariableScalar, &
        & TypeFEVariableSpaceTime )
      !!
    ELSE
      !!
      !! scalar space
      !!
      value = NodalVariable( &
        & Get( obj=obj%realVec, dofobj=obj%dof, &
        & nodenum=localNode, ivar=ivar ), &
        & TypeFEVariableScalar, TypeFEVariableSpace )
      !!
    END IF
  END IF
  !!
END PROCEDURE bnField_get6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get7
  INTEGER( I4B ) :: localNode( size(globalNode) ), idof
  !!
  localNode = obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode )
  !!
  idof = GetIDOF( spaceCompo=spaceCompo, timeCompo=timeCompo, &
    & tSpaceCompo=obj%dof .SpaceComponents. ivar )
  !!
  value = Get( obj=obj%realVec, dofobj=obj%dof, &
    & nodenum=localNode, ivar=ivar, idof=idof )
  !!
END PROCEDURE bnField_get7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get8
  INTEGER( I4B ) :: localNode( size(globalNode) ), idof
  !!
  localNode = obj%domains(ivar)%ptr%getLocalNodeNumber( globalNode )
  !!
  idof = GetIDOF( spaceCompo=spaceCompo, timeCompo=timeCompo, &
    & tSpaceCompo=obj%dof .SpaceComponents. ivar )
  !!
  value = NodalVariable( &
    & Get( obj=obj%realVec, dofobj=obj%dof, &
    & nodenum=localNode, ivar=ivar, idof=idof ), &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpace )
  !!
END PROCEDURE bnField_get8

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get9
  !!
  !! check:
  !! value is initiated
  !! obj is initiated
  !! tsize of value is equal to the tnodes of obj (ivar, idof)
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & value=value%realvec, &
    & ivar=ivar, &
    & idof=idof)
  !!
END PROCEDURE bnField_get9

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get10
  !!
  !! check:
  !! value is initiated
  !! obj is initiated
  !! tsize of value is equal to the tnodes of obj (ivar, idof)
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & value=value%realvec, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo )
  !!
END PROCEDURE bnField_get10

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get11
  !!
  !! check
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & ivarobj=ivar, &
    & spaceCompoObj=spaceCompo, &
    & timeCompoObj=timeCompo, &
    & value=value%realvec, &
    & dofvalue=value%dof, &
    & ivarvalue=1, &
    & spaceCompoValue=1, &
    & timeCompoValue=arange(1, size(timeCompo) ) )
  !!
END PROCEDURE bnField_get11

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get12
  !!
  !! check
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & ivarobj=ivar, &
    & spaceCompoObj=spaceCompo, &
    & timeCompoObj=timeCompo, &
    & value=value%realvec, &
    & dofvalue=value%dof, &
    & ivarvalue=1, &
    & spaceCompoValue=arange(1, size(spaceCompo) ), &
    & timeCompoValue=1 )
  !!
END PROCEDURE bnField_get12

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

!! ScalarField_

MODULE PROCEDURE bnField_get13
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_get13"
  INTEGER( I4B ) :: n
  !!
  !! check
  !!
  n = (obj%dof .spacecomponents. ivar)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .tspacecomponents. ivar)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  n = (obj%dof .timecomponents. ivar)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .timecomponents. ivar)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  n = (.tNames. value%dof)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (.tNames. value%dof)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  n = (value%dof .spacecomponents. 1)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .spacecomponents. 1)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  n = (value%dof .timecomponents. 1)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .timecomponents. 1)='//tostring(n)// &
    & ' is greater than 1')
  !!
#endif
  !!
  !! main
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & ivarobj=ivar, &
    & idofobj=1, &
    & value=value%realvec, &
    & dofvalue=value%dof, &
    & ivarvalue=1, &
    & idofvalue=1 )
  !!
END PROCEDURE bnField_get13

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

!! STScalarField_

MODULE PROCEDURE bnField_get14
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_get14"
  INTEGER( I4B ) :: m, n
  !!
  !! check
  !!
  n = (obj%dof .spacecomponents. ivar)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .spacecomponents. ivar)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  n = (.tNames. value%dof)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (.tNames. value%dof)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  n = (value%dof .spacecomponents. 1)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .spacecomponents. 1)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  m = (obj%dof .timecomponents. ivar)
  n = (value%dof .timecomponents. 1)
  !!
  IF( m .NE. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .timecomponents. ivar)='//tostring(m)// &
    & ' is not equal to ' // &
    & ' (value%dof .timecomponents. 1)='//tostring(n) )
  !!
#endif
  !!
  !! main
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idofobj=getIDOF(obj=obj%dof, ivar=ivar), &
    & value=value%realvec, &
    & dofvalue=value%dof, &
    & idofvalue=getIDOF( obj=value%dof, ivar=1) )
  !!
END PROCEDURE bnField_get14

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

!! VectorField_

MODULE PROCEDURE bnField_get15
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_get15"
  INTEGER( I4B ) :: m, n
  !!
  !! check
  !!
  n = (obj%dof .timecomponents. ivar)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .timecomponents. ivar)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  n = (.tNames. value%dof)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (.tNames. value%dof)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  n = (value%dof .timecomponents. 1)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (value%dof .timecomponents. 1)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  m = (obj%dof .spacecomponents. ivar)
  n = (value%dof .spacecomponents. 1)
  !!
  IF( m .NE. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .tspacecomponents. ivar)='//tostring(m)// &
    & ' is not equal to ' // &
    & ' (value%dof .spacecomponents. 1)='//tostring(n) )
  !!
#endif
  !!
  !! main
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idofobj=getIDOF(obj=obj%dof, ivar=ivar), &
    & value=value%realvec, &
    & dofvalue=value%dof, &
    & idofvalue=getIDOF(obj=value%dof, ivar=1) )
  !!
END PROCEDURE bnField_get15

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get16
!!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="bnField_get15"
  INTEGER( I4B ) :: m, n
  !!
  !! check
  !!
  n = (.tNames. value%dof)
  !!
  IF( n .GT. 1 ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (.tNames. value%dof)='//tostring(n)// &
    & ' is greater than 1')
  !!
  !! check
  !!
  m = (obj%dof .spacecomponents. ivar)
  n = (value%dof .spacecomponents. 1)
  !!
  IF( m .NE. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .spacecomponents. ivar)='//tostring(m)// &
    & ' is not equal to ' // &
    & ' (value%dof .spacecomponents. 1)='//tostring(n) )
  !!
  !!
  !! check
  !!
  m = (obj%dof .timecomponents. ivar)
  n = (value%dof .timecomponents. 1)
  !!
  IF( m .NE. n ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine is not callable as &
    & (obj%dof .timecomponents. ivar)='//tostring(m)// &
    & ' is not equal to ' // &
    & ' (value%dof .timecomponents. 1)='//tostring(n) )
  !!
#endif
  !!
  !! main
  !!
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idofobj=getIDOF(obj=obj%dof, ivar=ivar), &
    & value=value%realvec, &
    & dofvalue=value%dof, &
    & idofvalue=getIDOF(obj=value%dof, ivar=1) )
  !!
END PROCEDURE bnField_get16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods