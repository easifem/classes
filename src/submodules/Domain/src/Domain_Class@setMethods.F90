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

SUBMODULE( Domain_Class ) setMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setSparsity
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_setSparsity"
  INTEGER( I4B ) :: imesh, dim, tmesh, lb, ub
  CLASS( Mesh_ ), POINTER :: meshobj
  ! main
  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Mesh data is not initiated, first initiate")
  END IF
  meshobj => NULL()
  lb = LBOUND(obj%local_nptrs, 1)
  ub = UBOUND(obj%local_nptrs, 1)
  DO dim = 1, 3
    tmesh = obj%getTotalMesh( dim=dim )
    DO imesh = 1, tmesh
      meshobj => obj%getMeshPointer( dim=dim, entityNum=imesh )
      IF( ASSOCIATED( meshobj )  ) &
        & CALL meshobj%setSparsity( mat=mat, &
        & localNodeNumber=obj%local_nptrs, lbound=lb, ubound=ub )
    END DO
  END DO
  CALL setSparsity( mat )
  NULLIFY( meshobj )
END PROCEDURE Domain_setSparsity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE setMethods