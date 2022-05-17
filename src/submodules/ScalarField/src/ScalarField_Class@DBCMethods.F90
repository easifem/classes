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

SUBMODULE(ScalarField_Class) DBCMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_applyDirichletBC1
  REAL( DFP ), ALLOCATABLE :: nodalvalue(:,:)
  INTEGER( I4B ), ALLOCATABLE :: nodenum( : )
  !!
  CALL dbc%get( nodalvalue=nodalvalue, nodenum=nodenum )
  CALL obj%Set( globalNode=nodenum, value=nodalvalue(:,1) )
  !!
  IF( ALLOCATED( nodalvalue ) ) DEALLOCATE( nodalvalue )
  IF( ALLOCATED( nodenum ) ) DEALLOCATE( nodenum )
  !!
END PROCEDURE sField_applyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_applyDirichletBC2
  REAL( DFP ), ALLOCATABLE :: nodalvalue(:,:)
  INTEGER( I4B ), ALLOCATABLE :: nodenum( : )
  INTEGER( I4B ) :: ibc
  !!
  DO ibc = 1, SIZE( dbc )
    CALL dbc(ibc)%ptr%get( nodalvalue=nodalvalue, nodenum=nodenum )
    CALL obj%Set( globalNode=nodenum, value=nodalvalue(:,1) )
  END DO
  !!
  IF( ALLOCATED( nodalvalue ) ) DEALLOCATE( nodalvalue )
  IF( ALLOCATED( nodenum ) ) DEALLOCATE( nodenum )
  !!
END PROCEDURE sField_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods