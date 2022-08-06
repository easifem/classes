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

SUBMODULE(ChebyshevFirstSpace1D_Class) ConstructorMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     ChebyshevFirstSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE ChebyshevFirstSpace1D1
  ans%x = ChebyshevFirst1D( varname=varname, n=n )
  ans%coeff = ans%x%GetRecurrenceCoeff( n=n )
END PROCEDURE ChebyshevFirstSpace1D1

!----------------------------------------------------------------------------
!                                                     ChebyshevFirstSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE ChebyshevFirstSpace1D_Pointer1
  ALLOCATE( ans )
  ans%x = ChebyshevFirst1D( varname=varname, n=n )
  ans%coeff = ans%x%GetRecurrenceCoeff( n=n )
END PROCEDURE ChebyshevFirstSpace1D_Pointer1

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE f_Deallocate
  IF( ALLOCATED( obj%coeff ) ) DEALLOCATE( obj%coeff )
  CALL obj%x%Deallocate()
END PROCEDURE f_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE f_Final
  CALL obj%x%Deallocate()
END PROCEDURE f_Final

END SUBMODULE ConstructorMethods