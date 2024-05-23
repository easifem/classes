! This program is a part of EASIFEM library
!) Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(FEDOF_Class) GetMethods
USE AbstractMesh_Class, ONLY: PARAM_MAX_CONNECTIVITY_SIZE
USE ElemData_Class, ONLY: ElemData_, &
                          ElemData_GetTotalEntities, &
                          ElemData_GetEdge, &
                          ElemData_GetFace, &
                          ElemData_GetCell

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               GetVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVertexDOF
tsize = 1
ans(1) = obj%mesh%GetLocalNodeNumber(globalNode, islocal=islocal)
END PROCEDURE obj_GetVertexDOF

!----------------------------------------------------------------------------
!                                                                 GetEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEdgeDOF
INTEGER(I4B) :: ii
tsize = 0
DO ii = obj%edgeIA(globalEdge), obj%edgeIA(globalEdge + 1) - 1
  tsize = tsize + 1
  ans(tsize) = ii
END DO
END PROCEDURE obj_GetEdgeDOF

!----------------------------------------------------------------------------
!                                                                 GetFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFaceDOF
INTEGER(I4B) :: ii
tsize = 0
DO ii = obj%faceIA(globalface), obj%faceIA(globalface + 1) - 1
  tsize = tsize + 1
  ans(tsize) = ii
END DO
END PROCEDURE obj_GetFaceDOF

!----------------------------------------------------------------------------
!                                                                 GetCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellDOF
INTEGER(I4B) :: ii, jj
jj = obj%mesh%GetLocalElemNumber(globalElement=globalCell, islocal=islocal)
tsize = 0
DO ii = obj%cellIA(jj), obj%cellIA(jj + 1) - 1
  tsize = tsize + 1
  ans(tsize) = ii
END DO
END PROCEDURE obj_GetCellDOF

!----------------------------------------------------------------------------
!                                                           GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity_
INTEGER(I4B) :: ent(4)
INTEGER(I4B) :: ii, jj, kk, a, b
INTEGER(I4B) :: temp(PARAM_MAX_CONNECTIVITY_SIZE)

ent = obj%mesh%GetTotalEntities(globalElement, islocal=islocal)
CALL obj%mesh%GetConnectivity_(globalElement=globalElement, islocal=islocal, &
                               opt=opt, tsize=jj, ans=temp)

! points
a = 1; b = ent(1)
jj = 1
DO ii = a, b
CALL obj%GetVertexDOF(globalNode=temp(ii), ans=ans(jj:), tsize=kk, islocal=.FALSE.)
  jj = jj + kk
END DO

! edges
a = b + 1; b = b + ent(2)
DO ii = a, b
  CALL obj%GetEdgeDOF(globalEdge=temp(ii), ans=ans(jj:), tsize=kk)
  jj = jj + kk
END DO

! faces
a = b + 1; b = b + ent(3)
DO ii = a, b
  CALL obj%GetFaceDOF(globalFace=temp(ii), ans=ans(jj:), tsize=kk)
  jj = jj + kk
END DO

! cell
a = b + 1; b = b + ent(4)
DO ii = a, b
  CALL obj%GetCellDOF(globalCell=temp(ii), ans=ans(jj:), tsize=kk, &
                      islocal=.FALSE.)
  jj = jj + kk
END DO

tsize = jj - 1

END PROCEDURE obj_GetConnectivity_

!----------------------------------------------------------------------------
!                                                           GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF1
ans = obj%tdof
END PROCEDURE obj_GetTotalDOF1

!----------------------------------------------------------------------------
!                                                           GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF2
TYPE(ElemData_), POINTER :: elemdata
INTEGER(I4B) :: ii, jj, ent(4)

elemdata => obj%mesh%GetElemDataPointer(globalElement=globalElement, &
                                        islocal=islocal)

ent = ElemData_GetTotalEntities(elemdata)

ans = ent(1)

DO ii = 1, ent(2)
  jj = ElemData_GetEdge(elemdata, ii)
  ans = ans + obj%edgeIA(jj + 1) - obj%edgeIA(jj)
END DO

DO ii = 1, ent(3)
  jj = ElemData_GetFace(elemdata, ii)
  ans = ans + obj%faceIA(jj + 1) - obj%faceIA(jj)
END DO

jj = ElemData_GetCell(elemdata, islocal=.TRUE.)
ans = ans + obj%cellIA(jj + 1) - obj%cellIA(jj)

END PROCEDURE obj_GetTotalDOF2

!----------------------------------------------------------------------------
!                                                         GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
INTEGER(I4B) :: tdof
tdof = obj%GetTotalDOF(globalElement=globalElement, isLocal=isLocal)
ALLOCATE (ans(tdof))
CALL obj%GetConnectivity_(ans=ans, tsize=tdof, opt=opt, &
                          globalElement=globalElement, islocal=islocal)
END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------
MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
