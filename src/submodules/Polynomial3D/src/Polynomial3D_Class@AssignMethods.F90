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

SUBMODULE(Polynomial3D_Class) AssignMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjObj
  !!
  INTEGER( I4B ) :: ii, row
  !!
  IF( ALLOCATED( obj2%degree ) ) THEN
    obj%degree=obj2%degree
  END IF
  !!
  IF( ALLOCATED(obj2%coeff) ) THEN
    obj%coeff=obj2%coeff
  END IF
  !!
  obj%varname(1) = obj2%varname(1)
  obj%varname(2) = obj2%varname(2)
  obj%varname(3) = obj2%varname(3)
  !!
  IF( ALLOCATED( obj2%x ) ) THEN
    !!
    row = SIZE( obj2%x )
    !!
    IF( ALLOCATED( obj%x ) ) THEN
      IF( SIZE( obj%x ) .NE. row ) THEN
        DEALLOCATE( obj%x )
        ALLOCATE( obj%x( row ) )
      END IF
    ELSE
      ALLOCATE( obj%x( row ) )
    END IF
    !!
    DO ii = 1, row
      obj%x(ii) = obj2%x(ii)
    END DO
    !!
  END IF
  !!
END PROCEDURE func_AssignObjObj

!----------------------------------------------------------------------------
!                                                                 Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjMono
  INTEGER( I4B ) :: degree( 1, 3 )
  TYPE( String ) :: varname( 3 )
  REAL( DFP ) :: coeff( 1 )
  !!
  coeff = 1.0_DFP
  degree(1,:) = obj2%getDegree()
  varname = obj2%GetVarname()
  !!
  CALL obj%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & varname1= varname(1)%chars(), &
    & varname2= varname(2)%chars(), &
    & varname3=varname(3)%chars() )
  !!
END PROCEDURE func_AssignObjMono

!----------------------------------------------------------------------------
!                                                                 Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjInt8
  INTEGER( I4B ), PARAMETER :: degree( 1, 3 ) = 0
  REAL( DFP ) :: coeff( 1 )
  !!
  coeff = obj2
  CALL obj%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & varname1="x", &
    & varname2="y", &
    & varname3="z" )
  !!
END PROCEDURE func_AssignObjInt8

!----------------------------------------------------------------------------
!                                                                 Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjInt16
  INTEGER( I4B ), PARAMETER :: degree( 1, 3 ) = 0
  REAL( DFP ) :: coeff( 1 )
  !!
  coeff = obj2
  CALL obj%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & varname1="x", &
    & varname2="y", &
    & varname3="z" )
  !!
END PROCEDURE func_AssignObjInt16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjInt32
  INTEGER( I4B ), PARAMETER :: degree( 1, 3 ) = 0
  REAL( DFP ) :: coeff( 1 )
  !!
  coeff = obj2
  CALL obj%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & varname1="x", &
    & varname2="y", &
    & varname3="z" )
  !!
END PROCEDURE func_AssignObjInt32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjInt64
  INTEGER( I4B ), PARAMETER :: degree( 1, 3 ) = 0
  REAL( DFP ) :: coeff( 1 )
  !!
  coeff = obj2
  CALL obj%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & varname1="x", &
    & varname2="y", &
    & varname3="z" )
  !!
END PROCEDURE func_AssignObjInt64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjReal32
  INTEGER( I4B ), PARAMETER :: degree( 1, 3 ) = 0
  REAL( DFP ) :: coeff( 1 )
  !!
  coeff = obj2
  CALL obj%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & varname1="x", &
    & varname2="y", &
    & varname3="z" )
  !!
END PROCEDURE func_AssignObjReal32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjReal64
  INTEGER( I4B ), PARAMETER :: degree( 1, 3 ) = 0
  REAL( DFP ) :: coeff( 1 )
  !!
  coeff = obj2
  CALL obj%Initiate( &
    & coeff=coeff, &
    & degree=degree, &
    & varname1="x", &
    & varname2="y", &
    & varname3="z" )
  !!
END PROCEDURE func_AssignObjReal64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AssignMethods