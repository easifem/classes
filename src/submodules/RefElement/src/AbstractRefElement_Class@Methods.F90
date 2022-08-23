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

SUBMODULE(AbstractRefElement_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Copy
INTEGER(I4B) :: ii, n
  !!
CALL obj%Deallocate()
  !!
IF (ALLOCATED(obj2%xij)) obj%xij = obj2%xij
obj%entityCounts = obj2%entityCounts
obj%xidimension = obj2%xidimension
obj%name = obj2%name
obj%nameStr = obj2%nameStr
obj%nsd = obj2%nsd
IF (ALLOCATED(obj2%topology)) THEN
  n = SIZE(obj2%topology)
  ALLOCATE (obj%topology(n))
  DO ii = 1, n
    obj%topology(ii) = obj2%topology(ii)
  END DO
END IF
  !!
END PROCEDURE refelem_Copy

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Deallocate
INTEGER(I4B) :: ii, n
IF (ALLOCATED(obj%xij)) DEALLOCATE (obj%xij)
obj%entityCounts = 0
obj%xidimension = -1
obj%name = -1
obj%nameStr = ""
obj%nsd = -1
IF (ALLOCATED(obj%topology)) THEN
  n = SIZE(obj%topology)
  DO ii = 1, n
    CALL obj%topology(ii)%Deallocate()
  END DO
  DEALLOCATE (obj%topology)
END IF
END PROCEDURE refelem_Deallocate

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Display
  !! Define internal variable
INTEGER(I4B) :: I, j
  !!
CALL Display(msg, unitno=unitno)
  !!
CALL Display("element type : "//trim(ElementName(obj%name)), &
  & unitno=unitno)
  !!
CALL Display(obj%xidimension, "xidimension :: ", &
  & unitno=unitno)
  !!
CALL Display(obj%nsd, "nsd : ", unitno=unitno)
  !!
CALL Display(obj%entityCounts(1), "entityCounts(0) : ", &
  & unitno=unitno)
  !!
CALL Display(obj%entityCounts(2), "entityCounts(1) : ", &
  & unitno=unitno)
  !!
CALL Display(obj%entityCounts(3), "entityCounts(2) : ", &
  & unitno=unitno)
  !!
CALL Display(obj%entityCounts(4), "entityCounts(3) : ", &
  & unitno=unitno)
  !!
DO j = 1, SIZE(obj%xiJ, 2)
  CALL Display( &
    & obj%xiJ(:, j), &
    & "Node( "//tostring(j)//" ) : ", &
    & unitno=unitno)
END DO
  !!
DO j = 1, SIZE(obj%topology)
  CALL obj%topology(j)%Display( &
    & "topology( "//tostring(j)//" ) : ", &
    & unitno=unitno)
END DO
  !!
END PROCEDURE refelem_Display

!----------------------------------------------------------------------------
!                                                                     GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetNNE
IF (ALLOCATED(obj%xij)) THEN
  ans = SIZE(obj%xij, 2)
ELSE
  ans = 0
END IF
END PROCEDURE refelem_GetNNE

!----------------------------------------------------------------------------
!                                                                    GetNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetNSD
ans = obj%NSD
END PROCEDURE refelem_GetNSD

!----------------------------------------------------------------------------
!                                                            GetXidimension
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetXidimension
ans = obj%xidimension
END PROCEDURE refelem_GetXidimension

!----------------------------------------------------------------------------
!                                                         GetElementTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementTopology
ans = ElementTopology(obj%name)
END PROCEDURE refelem_GetElementTopology

!----------------------------------------------------------------------------
!                                                                  GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetNptrs
ans = obj%topology(SUM(obj%entityCounts))%GetNptrs()
END PROCEDURE refelem_GetNptrs

!----------------------------------------------------------------------------
!                                                            GetFacetMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetMatrix
  !!
INTEGER(I4B) :: xicell, T(4), i, istart, iend, max_nns, nns, tFacet
  !!
  !! main
  !!
T(1) = 0
  !!
DO i = 2, 4
  T(i) = SUM(obj%entityCounts(1:i - 1))
END DO
  !!
xicell = obj%xidimension
  !!
SELECT CASE (xicell)
  !!
CASE (0_I4B)
  !!
  ALLOCATE (ans(0, 0))
  !!
CASE (1_I4B)
    !!
  tFacet = 2; istart = 1; iend = 2; max_nns = 2
  ALLOCATE (ans(tFacet, max_nns + 3))
  ans = 0
    !!
  DO i = 0, tFacet - 1
    ans(i + 1, 1) = obj%topology(iStart + i)%GetName()
    ans(i + 1, 2) = obj%topology(iStart + i)%GetXiDimension()
    nns = obj%topology(iStart + i)%GetNNE()
    ans(i + 1, 3) = nns
    ans(i + 1, 4:(3 + nns)) = obj%topology(iStart + i)%GetNptrs()
  END DO
    !!
CASE (2_I4B, 3_I4B)
    !!
  tFacet = obj%entityCounts(xicell)
  istart = T(xicell) + 1
  iend = T(xicell) + tFacet
  max_nns = 0
    !!
  DO i = istart, iend
    nns = obj%topology(i)%GetNNE()
    IF (max_nns .LT. nns) max_nns = nns
  END DO
    !!
  ALLOCATE (ans(tFacet, max_nns + 3))
    !!
  ans = 0
    !!
  DO i = 0, tFacet - 1
    ans(i + 1, 1) = obj%topology(iStart + i)%GetName()
    ans(i + 1, 2) = obj%topology(iStart + i)%GetXiDimension()
    nns = obj%topology(iStart + i)%GetNNE()
    ans(i + 1, 3) = nns
    ans(i + 1, 4:(3 + nns)) = obj%topology(iStart + i)%GetNptrs()
  END DO
    !!
END SELECT
  !!
END PROCEDURE refelem_GetFacetMatrix

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetNodeCoord
IF (ALLOCATED(obj%xij)) THEN
  ans = obj%xij
ELSE
  ALLOCATE (ans(0, 0))
END IF
END PROCEDURE refelem_GetNodeCoord

!----------------------------------------------------------------------------
!                                                                 SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_SetParam
INTEGER(I4B) :: ii, n
!!
IF (PRESENT(xij)) obj%xij = xij
IF (PRESENT(entityCounts)) obj%entityCounts = entityCounts
IF (PRESENT(xidimension)) obj%xidimension = xidimension
IF (PRESENT(name)) obj%name = name
IF (PRESENT(nameStr)) obj%nameStr = nameStr
IF (PRESENT(nsd)) obj%nsd = nsd
!!
IF (PRESENT(topology)) THEN
  IF (ALLOCATED(obj%topology)) DEALLOCATE (obj%topology)
  n = SIZE(topology)
  ALLOCATE (obj%topology(n))
  DO ii = 1, n
    obj%topology(ii) = topology(ii)
  END DO
END IF
!!
END PROCEDURE refelem_SetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
