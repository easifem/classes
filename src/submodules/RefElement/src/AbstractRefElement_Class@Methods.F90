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
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
INTEGER(I4B) :: name
!!
name = obj%GetName()
!!
CALL obj%SetParam( &
  & xij=EquidistancePoint(order=1_I4B, elemType=name), &
  & entityCounts=TotalEntities(elemType=name), &
  & nsd=nsd, &
  & xidimension=Xidimension(elemType=name), &
  & name=name, &
  & nameStr=ElementName(name))
!!
CALL obj%GenerateTopology()
!!
END PROCEDURE refelem_Initiate

!----------------------------------------------------------------------------
!                                                               GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
INTEGER(I4B) :: ii, n
!!
IF (PRESENT(xidim)) THEN
  n = obj%entityCounts(xidim + 1)
  ALLOCATE (ans(n))
  !!
  SELECT CASE (xidim)
  CASE (0_I4B)
    DO ii = 1, n
      ans(ii) = obj%pointTopology(ii)
    END DO
  CASE (1_I4B)
    DO ii = 1, n
      ans(ii) = obj%edgeTopology(ii)
    END DO
  CASE (2_I4B)
    DO ii = 1, n
      ans(ii) = obj%faceTopology(ii)
    END DO
  CASE (3_I4B)
    DO ii = 1, n
      ans(ii) = obj%cellTopology(ii)
    END DO
  END SELECT
ELSE
  n = SUM(obj%entityCounts)
  ALLOCATE (ans(n))
  !!
  !! points
  !!
  DO ii = 1, obj%entityCounts(1)
    ans(ii) = obj%pointTopology(ii)
  END DO
  !!
  !! edge
  !!
  DO ii = 1, obj%entityCounts(2)
    ans(obj%entityCounts(1) + ii) = obj%edgeTopology(ii)
  END DO
  !!
  !! face
  !!
  DO ii = 1, obj%entityCounts(3)
    ans(obj%entityCounts(2) + ii) = obj%faceTopology(ii)
  END DO
  !!
  !! cell
  !!
  DO ii = 1, obj%entityCounts(4)
    ans(obj%entityCounts(3) + ii) = obj%cellTopology(ii)
  END DO
  !!
END IF
!!
END PROCEDURE refelem_GetTopology

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
!!
!! point topology
!!
IF (ALLOCATED(obj2%pointTopology)) THEN
  n = SIZE(obj2%pointTopology)
  ALLOCATE (obj%pointTopology(n))
  DO ii = 1, n
    obj%pointTopology(ii) = obj2%pointTopology(ii)
  END DO
END IF
!!
!! edge topology
!!
IF (ALLOCATED(obj2%edgeTopology)) THEN
  n = SIZE(obj2%edgeTopology)
  ALLOCATE (obj%edgeTopology(n))
  DO ii = 1, n
    obj%edgeTopology(ii) = obj2%edgeTopology(ii)
  END DO
END IF
!!
!! face topology
!!
IF (ALLOCATED(obj2%faceTopology)) THEN
  n = SIZE(obj2%faceTopology)
  ALLOCATE (obj%faceTopology(n))
  DO ii = 1, n
    obj%faceTopology(ii) = obj2%faceTopology(ii)
  END DO
END IF
!!
!! cell topology
!!
IF (ALLOCATED(obj2%cellTopology)) THEN
  n = SIZE(obj2%cellTopology)
  ALLOCATE (obj%cellTopology(n))
  DO ii = 1, n
    obj%cellTopology(ii) = obj2%cellTopology(ii)
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
!!
!! point topology
!!
IF (ALLOCATED(obj%pointTopology)) THEN
  n = SIZE(obj%pointTopology)
  DO ii = 1, n
    CALL obj%pointTopology(ii)%Deallocate()
  END DO
  DEALLOCATE (obj%pointTopology)
END IF
!!
!! edge topology
!!
IF (ALLOCATED(obj%edgeTopology)) THEN
  n = SIZE(obj%edgeTopology)
  DO ii = 1, n
    CALL obj%edgeTopology(ii)%Deallocate()
  END DO
  DEALLOCATE (obj%edgeTopology)
END IF
!!
!! face topology
!!
IF (ALLOCATED(obj%faceTopology)) THEN
  n = SIZE(obj%faceTopology)
  DO ii = 1, n
    CALL obj%faceTopology(ii)%Deallocate()
  END DO
  DEALLOCATE (obj%faceTopology)
END IF
!!
!! cell topology
!!
IF (ALLOCATED(obj%cellTopology)) THEN
  n = SIZE(obj%cellTopology)
  DO ii = 1, n
    CALL obj%cellTopology(ii)%Deallocate()
  END DO
  DEALLOCATE (obj%cellTopology)
END IF
END PROCEDURE refelem_Deallocate

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Display
!! Define internal variable
INTEGER(I4B) :: j
LOGICAL(LGT) :: notFull0
!!
notFull0 = INPUT(option=notFull, default=.FALSE.)
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
IF (notFull0) RETURN
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
!! pointTopology
!!
DO j = 1, obj%entityCounts(1)
  CALL obj%pointTopology(j)%Display( &
    & "pointTopology( "//tostring(j)//" ) : ", &
    & unitno=unitno)
END DO
!!
!! edgeTopology
!!
DO j = 1, obj%entityCounts(2)
  CALL obj%edgeTopology(j)%Display( &
    & "edgeTopology( "//tostring(j)//" ) : ", &
    & unitno=unitno)
END DO
!!
!! faceTopology
!!
DO j = 1, obj%entityCounts(3)
  CALL obj%faceTopology(j)%Display( &
    & "faceTopology( "//tostring(j)//" ) : ", &
    & unitno=unitno)
END DO
!!
!! cellTopology
!!
DO j = 1, obj%entityCounts(4)
  CALL obj%cellTopology(j)%Display( &
    & "cellTopology( "//tostring(j)//" ) : ", &
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
SELECT CASE (obj%xidimension)
CASE (0_I4B)
  ans = obj%pointTopology(1)%GetNptrs()
CASE (1_I4B)
  ans = obj%edgeTopology(1)%GetNptrs()
CASE (2_I4B)
  ans = obj%faceTopology(1)%GetNptrs()
CASE (3_I4B)
  ans = obj%cellTopology(1)%GetNptrs()
END SELECT
END PROCEDURE refelem_GetNptrs

!----------------------------------------------------------------------------
!                                                            GetFacetMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetMatrix
!!
INTEGER(I4B) :: xicell, i, max_nns, nns, tFacet
TYPE(Topology_), ALLOCATABLE :: faceTopology(:)
!!
!! main
!!
xicell = obj%xidimension
faceTopology = obj%GetTopology(xidim=xicell)
tFacet = obj%entityCounts(xicell)
max_nns = 0
!!
DO i = 1, tFacet
  nns = obj%faceTopology(i)%GetNNE()
  IF (max_nns .LT. nns) max_nns = nns
END DO
!!
ALLOCATE (ans(tFacet, max_nns + 3))
ans = 0
!!
DO i = 1, tFacet
  ans(i, 1) = faceTopology(i)%GetName()
  ans(i, 2) = faceTopology(i)%GetXiDimension()
  nns = faceTopology(i)%GetNNE()
  ans(i, 3) = nns
  ans(i, 4:(3 + nns)) = faceTopology(i)%GetNptrs()
END DO
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
!                                                      GetInterpolationPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetInterpolationPoint
ans = InterpolationPoint(order=order, ipType=ipType, elemType=obj%name)
END PROCEDURE refelem_GetInterpolationPoint

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
IF (PRESENT(pointTopology)) THEN
  IF (ALLOCATED(obj%pointTopology)) DEALLOCATE (obj%pointTopology)
  n = SIZE(pointTopology)
  ALLOCATE (obj%pointTopology(n))
  DO ii = 1, n
    obj%pointTopology(ii) = pointTopology(ii)
  END DO
END IF
!!
IF (PRESENT(edgeTopology)) THEN
  IF (ALLOCATED(obj%edgeTopology)) DEALLOCATE (obj%edgeTopology)
  n = SIZE(edgeTopology)
  ALLOCATE (obj%edgeTopology(n))
  DO ii = 1, n
    obj%edgeTopology(ii) = edgeTopology(ii)
  END DO
END IF
!!
IF (PRESENT(faceTopology)) THEN
  IF (ALLOCATED(obj%faceTopology)) DEALLOCATE (obj%faceTopology)
  n = SIZE(faceTopology)
  ALLOCATE (obj%faceTopology(n))
  DO ii = 1, n
    obj%faceTopology(ii) = faceTopology(ii)
  END DO
END IF
!!
IF (PRESENT(cellTopology)) THEN
  IF (ALLOCATED(obj%cellTopology)) DEALLOCATE (obj%cellTopology)
  n = SIZE(cellTopology)
  ALLOCATE (obj%cellTopology(n))
  DO ii = 1, n
    obj%cellTopology(ii) = cellTopology(ii)
  END DO
END IF
!!
END PROCEDURE refelem_SetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods