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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBMODULE(Polynomial1D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate
  obj%degree = 0_I4B
  obj%varname=""
  IF( ALLOCATED( obj%x ) ) DEALLOCATE( obj%x )
END PROCEDURE func_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
  CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                                               Polynomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Polynomial1D1
  INTEGER( I4B ) :: ii, tsize, n1, n2
  REAL( DFP ), ALLOCATABLE :: coeff0( : ), coeff1( : )
  INTEGER( I4B ), ALLOCATABLE :: degree0( : ), degree1( : )
#include "./inc/Polynomial1D/func_Polynomial1D.inc"
END PROCEDURE func_Polynomial1D1

!----------------------------------------------------------------------------
!                                                         Polynomial1D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Polynomial1D_Pointer1
  INTEGER( I4B ) :: ii, tsize, n1, n2
  REAL( DFP ), ALLOCATABLE :: coeff0( : ), coeff1( : )
  INTEGER( I4B ), ALLOCATABLE :: degree0( : ), degree1( : )
  ALLOCATE( Polynomial1D_ :: ans )
#include "./inc/Polynomial1D/func_Polynomial1D.inc"
END PROCEDURE func_Polynomial1D_Pointer1

END SUBMODULE ConstructorMethods