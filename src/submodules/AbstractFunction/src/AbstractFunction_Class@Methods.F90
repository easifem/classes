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

SUBMODULE(AbstractFunction_Class) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate
END PROCEDURE func_Deallocate

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate1
  CALL AbstractFunctionDeallocate(obj)
  obj%varname=""
END PROCEDURE func_Deallocate1


!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate2
  CALL AbstractFunctionDeallocate(obj)
  obj%varname(1)=""
  obj%varname(2)=""
END PROCEDURE func_Deallocate2


!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate3
  CALL AbstractFunctionDeallocate(obj)
  obj%varname(1)=""
  obj%varname(2)=""
  obj%varname(3)=""
END PROCEDURE func_Deallocate3

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_DeallocateN
  CALL AbstractFunctionDeallocate(obj)
  IF( ALLOCATED(obj%varname) ) DEALLOCATE( obj%varname )
END PROCEDURE func_DeallocateN

!----------------------------------------------------------------------------
!                                                                 GetVarName
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetVarname1
  ans = obj%varname
END PROCEDURE func_GetVarname1

!----------------------------------------------------------------------------
!                                                                 GetVarName
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetVarname2
  ans = obj%varname
END PROCEDURE func_GetVarname2

!----------------------------------------------------------------------------
!                                                                 GetVarName
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetVarname3
  ans = obj%varname
END PROCEDURE func_GetVarname3

!----------------------------------------------------------------------------
!                                                                 GetVarName
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetVarnameN
  IF( ALLOCATED( obj%varname ) ) THEN
    ALLOCATE( ans( SIZE( obj%varname ) ) )
    ans = obj%varname
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE func_GetVarnameN

END SUBMODULE Methods