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

SUBMODULE(ScalarField_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get1
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    !!
    value = get( &
      & obj=obj%realVec, &
      & nodenum=1, &
      & dataType= 1.0_DFP )
    !!
  ELSE
    !!
    value = get( &
      & obj=obj%realVec, &
      & nodenum=obj%domain%getLocalNodeNumber( globalNode ), &
      & dataType= 1.0_DFP )
    !!
  END IF
  !!
END PROCEDURE sField_get1

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get2
  !!
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    !!
    CALL reallocate( value, obj%tsize )
    !!
    value = get( &
      & obj=obj%realVec, &
      & nodenum=1, &
      & dataType= 1.0_DFP )
    !!
  ELSE
    !!
    CALL GetValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & value=value, &
      & idof=1 )
    !!
  END IF
  !!
END PROCEDURE sField_get2

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get3
  !!
  value = get( &
    & obj=obj%realVec, &
    & nodenum=obj%domain%getLocalNodeNumber( globalNode ), &
    & dataType= 1.0_DFP )
  !!
END PROCEDURE sField_get3

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get4
  !!
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj
  !!
  !!
  jj = 0
  !!
  DO ii = istart, iend, stride
    jj = jj + 1
    globalNode( jj ) = ii
  END DO
  !!
  CALL obj%get( globalNode=globalNode, value=value )
  !!
END PROCEDURE sField_get4

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get5
  !!
  value = NodalVariable( &
    & get( &
    & obj=obj%realVec, &
    & nodenum=obj%domain%getLocalNodeNumber( globalNode ), &
    & dataType= 1.0_DFP ), &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpace )
  !!
END PROCEDURE sField_get5

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get6
  CALL getValue( obj=obj%realVec, value=value%realVec )
END PROCEDURE sField_get6

END SUBMODULE GetMethods