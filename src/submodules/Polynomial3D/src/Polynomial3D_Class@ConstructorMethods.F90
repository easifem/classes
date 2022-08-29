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

SUBMODULE(Polynomial3D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate
INTEGER(I4B) :: ii, n
CALL AbstractFunction3DDeallocate(obj)
IF (ALLOCATED(obj%degree)) DEALLOCATE (obj%degree)
IF (ALLOCATED(obj%coeff)) DEALLOCATE (obj%coeff)
IF (ALLOCATED(obj%x)) THEN
  n = SIZE(obj%x)
  DO ii = 1, n
    CALL obj%x(ii)%Deallocate()
  END DO
  DEALLOCATE (obj%x)
END IF
END PROCEDURE func_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Initiate
INTEGER(I4B) :: ii, tsize, n1, n2, n3, n4
REAL(DFP), ALLOCATABLE :: coeff0(:), c(:)
INTEGER(I4B), ALLOCATABLE :: d1(:), d2(:), d3(:)
INTEGER(I4B), ALLOCATABLE :: b1(:), b2(:), b3(:)
  !!
d1 = degree(:, 1)
d2 = degree(:, 2)
d3 = degree(:, 3)
coeff0 = coeff
  !!
CALL QUICKSORT( &
  & d1, &
  & d2, &
  & d3, &
  & coeff0, &
  & 1_I4B, &
  & SIZE(d1, KIND=I4B))
  !!
tsize = SIZE(coeff0)
b1 = [d1(1)]
b2 = [d2(1)]
b3 = [d3(1)]
c = [coeff0(1)]
n1 = 1; n2 = 1; n3 = 1; n4 = 1
  !!
IF (tsize .GT. 1) THEN
    !!
  DO ii = 2, tsize
    IF (ANY([d1(ii), d2(ii), d3(ii)] &
      & .NE. [d1(ii - 1), d2(ii - 1), d3(ii - 1)])) THEN
        !!
      CALL Expand(vec=c, n=n1, chunk_size=MAX_CHUNK_SIZE, &
        & val=coeff0(ii))
        !!
      CALL Expand(vec=b1, n=n2, chunk_size=MAX_CHUNK_SIZE, &
        & val=d1(ii))
        !!
      CALL Expand(vec=b2, n=n3, chunk_size=MAX_CHUNK_SIZE, &
        & val=d2(ii))
        !!
      CALL Expand(vec=b3, n=n4, chunk_size=MAX_CHUNK_SIZE, &
        & val=d3(ii))
    ELSE
      c(n1) = c(n1) + coeff0(ii)
    END IF
  END DO
    !!
  CALL Expand(vec=c, n=n1, chunk_size=MAX_CHUNK_SIZE, &
    & finished=.TRUE.)
    !!
  CALL Expand(vec=b1, n=n2, chunk_size=MAX_CHUNK_SIZE, &
    & finished=.TRUE.)
    !!
  CALL Expand(vec=b2, n=n3, chunk_size=MAX_CHUNK_SIZE, &
    & finished=.TRUE.)
    !!
  CALL Expand(vec=b3, n=n4, chunk_size=MAX_CHUNK_SIZE, &
    & finished=.TRUE.)
    !!
END IF
  !!
tsize = SIZE(b1)
  !!
IF (ALLOCATED(obj%x)) DEALLOCATE (obj%x)
ALLOCATE (obj%x(tsize))
  !!
DO ii = 1, tsize
  CALL obj%x(ii)%Initiate( &
    & n1=b1(ii), &
    & n2=b2(ii), &
    & n3=b3(ii), &
    & varname1=varname1, &
    & varname2=varname2, &
    & varname3=varname3)
END DO
  !!
CALL Reallocate(obj%degree, tsize, 3)
obj%degree(:, 1) = b1
obj%degree(:, 2) = b2
obj%degree(:, 3) = b3
obj%coeff = c
obj%varname(1) = TRIM(varname1)
obj%varname(2) = TRIM(varname2)
obj%varname(3) = TRIM(varname3)
  !!
IF (ALLOCATED(coeff0)) DEALLOCATE (coeff0)
IF (ALLOCATED(d1)) DEALLOCATE (d1)
IF (ALLOCATED(d2)) DEALLOCATE (d2)
IF (ALLOCATED(d3)) DEALLOCATE (d3)
IF (ALLOCATED(b1)) DEALLOCATE (b1)
IF (ALLOCATED(b2)) DEALLOCATE (b2)
IF (ALLOCATED(b3)) DEALLOCATE (b3)
IF (ALLOCATED(c)) DEALLOCATE (c)
END PROCEDURE func_Initiate

!----------------------------------------------------------------------------
!                                                               Polynomial3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Polynomial3D1
CALL ans%Initiate( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname1, &
  & varname2=varname2, &
  & varname3=varname3)
END PROCEDURE func_Polynomial3D1

!----------------------------------------------------------------------------
!                                                       Polynomial3D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Polynomial3D_Pointer1
ALLOCATE (ans)
CALL ans%Initiate( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname1, &
  & varname2=varname2, &
  & varname3=varname3)
END PROCEDURE func_Polynomial3D_Pointer1

END SUBMODULE ConstructorMethods
