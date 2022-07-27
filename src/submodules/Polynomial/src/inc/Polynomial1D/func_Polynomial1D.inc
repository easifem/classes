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
!!
degree0 = degree
coeff0 = coeff
!!
CALL QUICKSORT( degree0, coeff0, 1_I4B, SIZE(degree0, KIND=I4B ) )
!!
tsize = SIZE( degree )
degree1 = [degree0(1)]
coeff1 = [coeff0(1)]
n1 = 1
n2 = 1
!!
IF( tsize .GT. 1 ) THEN
  DO ii = 2, tsize
    IF( degree0( ii ) .NE. degree0( ii-1 ) ) THEN
      CALL Expand( vec=degree1, n=n1, chunk_size=MAX_CHUNK_SIZE, &
        & val=degree0( ii )  )
      CALL Expand( vec=coeff1, n=n2, chunk_size=MAX_CHUNK_SIZE, &
        & val=coeff0( ii )  )
    ELSE
      coeff1( n2 ) = coeff1( n2 ) + coeff0( ii )
    END IF
  END DO
  CALL Expand( vec=degree1, n=n1, chunk_size=MAX_CHUNK_SIZE, &
    & finished=.TRUE. )
  CALL Expand( vec=coeff1, n=n1, chunk_size=MAX_CHUNK_SIZE, &
    & finished=.TRUE. )
END IF
!!
tsize = SIZE( degree1 )
IF( ALLOCATED( ans%x ) ) DEALLOCATE( ans%x )
ALLOCATE( ans%x( tsize ) )
ans%varname=TRIM(varname)
!!
DO ii = 1, tsize
  ans%x(ii) = Monomial1D(coeff=coeff1(ii), degree=degree1(ii), &
    & varname=varname)
END DO
!!
IF( ALLOCATED( coeff0 ) ) DEALLOCATE( coeff0 )
IF( ALLOCATED( coeff1 ) ) DEALLOCATE( coeff1 )
IF( ALLOCATED( degree0 ) ) DEALLOCATE( degree0 )
IF( ALLOCATED( degree1 ) ) DEALLOCATE( degree1 )
!!