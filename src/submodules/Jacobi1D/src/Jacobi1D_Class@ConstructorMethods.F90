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

SUBMODULE(Jacobi1D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       Jacobi1D
!----------------------------------------------------------------------------

MODULE PROCEDURE Jacobi1D1
  !!
  REAL( DFP ) :: coeff( 0:n-1, 2 ), scale( 0:n-1, 2 )
  CLASS( AbstractOrthopol1D_ ), POINTER :: ptr => NULL()
  !!
  ans%alpha = alpha
  ans%beta = beta
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
  ptr => ans%GetJn1Pointer()
  !!
  DO
    !!
    IF( ASSOCIATED( ptr ) ) THEN
      SELECT TYPE( ptr ); TYPE IS( Jacobi1D_ )
        CALL ptr%SetJacobiParam( alpha=alpha, beta=beta )
      END SELECT
      ptr => ptr%GetJn1Pointer()
    ELSE
      EXIT
    END IF
  END DO
  !!
  NULLIFY( ptr )
  !!
END PROCEDURE Jacobi1D1

!----------------------------------------------------------------------------
!                                                Jacobi1D_Pointer1
!----------------------------------------------------------------------------

MODULE PROCEDURE Jacobi1D_Pointer1
  !!
  REAL( DFP ) :: coeff( 0:n-1, 2 ), scale( 0:n-1, 2 )
  CLASS( AbstractOrthopol1D_ ), POINTER :: ptr => NULL()
  !!
  ALLOCATE( Jacobi1D_:: ans )
  !!
  ans%alpha = alpha
  ans%beta = beta
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
  ptr => ans%GetJn1Pointer()
  !!
  DO
    !!
    IF( ASSOCIATED( ptr ) ) THEN
      SELECT TYPE( ptr ); TYPE IS( Jacobi1D_ )
        CALL ptr%SetJacobiParam( alpha=alpha, beta=beta )
      END SELECT
      ptr => ptr%GetJn1Pointer()
    ELSE
      EXIT
    END IF
  END DO
  !!
  NULLIFY( ptr )
  !!
END PROCEDURE Jacobi1D_Pointer1

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Final
  CALL obj%Deallocate()
END PROCEDURE Orthopol_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods