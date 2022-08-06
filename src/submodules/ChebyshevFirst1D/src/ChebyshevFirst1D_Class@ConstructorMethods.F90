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

SUBMODULE(ChebyshevFirst1D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Final
  CALL obj%Deallocate()
END PROCEDURE Orthopol_Final

!----------------------------------------------------------------------------
!                                                    ChebyshevFirst1D
!----------------------------------------------------------------------------

MODULE PROCEDURE ChebyshevFirst1D1
  REAL( DFP ) :: coeff( 0:n-1, 2 ), scale( 0:n-1, 2 )
  !!
  CALL ans%GetCoeffScale( n=n, coeff=coeff, scale=scale, &
    & isMonic=isMonic, isOrthonormal=isOrthonormal )
  !!
  CALL ans%Initiate( &
    & varname=varname, &
    & n=n, &
    & coeff=coeff, &
    & scale=scale )
  !!
END PROCEDURE ChebyshevFirst1D1

!----------------------------------------------------------------------------
!                                            ChebyshevFirst1D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE ChebyshevFirst1D_Pointer1
  REAL( DFP ) :: coeff( 0:n, 2 ), scale( 0:n-1, 2 )
  !!
  ALLOCATE( ans )
  !!
  CALL ans%GetCoeffScale( n=n, coeff=coeff, scale=scale, &
    & isMonic=isMonic, isOrthonormal=isOrthonormal )
  !!
  CALL ans%Initiate( &
    & varname=varname, &
    & n=n, &
    & coeff=coeff, &
    & scale=scale )
  !!
END PROCEDURE ChebyshevFirst1D_Pointer1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods