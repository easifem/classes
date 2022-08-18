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
  CALL AbstractFunction1DDeallocate( obj )
  IF( ALLOCATED( obj%degree ) ) DEALLOCATE( obj%degree )
  IF( ALLOCATED( obj%coeff ) ) DEALLOCATE( obj%coeff )
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
  REAL( DFP ), ALLOCATABLE :: coeff0( : )
  INTEGER( I4B ), ALLOCATABLE :: degree0( : )
  !!
  degree0 = degree
  coeff0 = coeff
  !!
  CALL QUICKSORT( degree0, coeff0, 1_I4B, SIZE(degree0, KIND=I4B ) )
  !!
  tsize = SIZE( degree )
  ans%degree= [degree0(1)]
  ans%coeff= [coeff0(1)]
  n1 = 1
  n2 = 1
  !!
  IF( tsize .GT. 1 ) THEN
    !!
    DO ii = 2, tsize
      IF( degree0( ii ) .NE. degree0( ii-1 ) ) THEN
        CALL Expand( vec=ans%degree, n=n1, chunk_size=MAX_CHUNK_SIZE, &
          & val=degree0( ii )  )
        CALL Expand( vec=ans%coeff, n=n2, chunk_size=MAX_CHUNK_SIZE, &
          & val=coeff0( ii )  )
      ELSE
        ans%coeff( n2 ) = ans%coeff( n2 ) + coeff0( ii )
      END IF
    END DO
    !!
    CALL Expand( vec=ans%degree, n=n1, chunk_size=MAX_CHUNK_SIZE, &
      & finished=.TRUE. )
    !!
    CALL Expand( vec=ans%coeff, n=n2, chunk_size=MAX_CHUNK_SIZE, &
      & finished=.TRUE. )
    !!
  END IF
  !!
  tsize = SIZE( ans%degree )
  IF( ALLOCATED( ans%x ) ) DEALLOCATE( ans%x )
  ALLOCATE( ans%x( tsize ) )
  !!
  DO ii = 1, tsize
    ans%x(ii) = Monomial1D(degree=ans%degree(ii), varname=varname)
  END DO
  !!
  IF( ALLOCATED( coeff0 ) ) DEALLOCATE( coeff0 )
  IF( ALLOCATED( degree0 ) ) DEALLOCATE( degree0 )
  !!
END PROCEDURE func_Polynomial1D1

!----------------------------------------------------------------------------
!                                                         Polynomial1D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Polynomial1D_Pointer1
  INTEGER( I4B ) :: ii, tsize, n1, n2
  REAL( DFP ), ALLOCATABLE :: coeff0( : )
  INTEGER( I4B ), ALLOCATABLE :: degree0( : )
  !!
  ALLOCATE( Polynomial1D_ :: ans )
  !!
  degree0 = degree
  coeff0 = coeff
  !!
  CALL QUICKSORT( degree0, coeff0, 1_I4B, SIZE(degree0, KIND=I4B ) )
  !!
  tsize = SIZE( degree )
  ans%degree= [degree0(1)]
  ans%coeff= [coeff0(1)]
  n1 = 1
  n2 = 1
  !!
  IF( tsize .GT. 1 ) THEN
    !!
    DO ii = 2, tsize
      IF( degree0( ii ) .NE. degree0( ii-1 ) ) THEN
        CALL Expand( vec=ans%degree, n=n1, chunk_size=MAX_CHUNK_SIZE, &
          & val=degree0( ii )  )
        CALL Expand( vec=ans%coeff, n=n2, chunk_size=MAX_CHUNK_SIZE, &
          & val=coeff0( ii )  )
      ELSE
        ans%coeff( n2 ) = ans%coeff( n2 ) + coeff0( ii )
      END IF
    END DO
    !!
    CALL Expand( vec=ans%degree, n=n1, chunk_size=MAX_CHUNK_SIZE, &
      & finished=.TRUE. )
    !!
    CALL Expand( vec=ans%coeff, n=n2, chunk_size=MAX_CHUNK_SIZE, &
      & finished=.TRUE. )
    !!
  END IF
  !!
  tsize = SIZE( ans%degree )
  IF( ALLOCATED( ans%x ) ) DEALLOCATE( ans%x )
  ALLOCATE( ans%x( tsize ) )
  !!
  DO ii = 1, tsize
    ans%x(ii) = Monomial1D(degree=ans%degree(ii), varname=varname)
  END DO
  !!
  IF( ALLOCATED( coeff0 ) ) DEALLOCATE( coeff0 )
  IF( ALLOCATED( degree0 ) ) DEALLOCATE( degree0 )
  !!
END PROCEDURE func_Polynomial1D_Pointer1

END SUBMODULE ConstructorMethods