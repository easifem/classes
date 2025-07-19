! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
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

SUBMODULE(FEDOF_Class) IOMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: Display, ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInitiated: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL Display(obj%isLagrange, "isLagrange: ", unitno=unitno)
CALL Display(obj%tdof, "tdof: ", unitno=unitno)
CALL Display(obj%tNodes, "tNodes: ", unitno=unitno)
CALL Display(obj%tEdges, "tEdges: ", unitno=unitno)
CALL Display(obj%tFaces, "tFaces: ", unitno=unitno)
CALL Display(obj%tCells, "tCells: ", unitno=unitno)
CALL Display(obj%maxTotalConnectivity, "maxTotalConnectivity: ", unitno=unitno)
CALL Display(obj%baseContinuity, "baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation, "baseInterpolation: ", unitno=unitno)
CALL Display(obj%maxCellOrder, "maxCellOrder: ", unitno=unitno)
CALL Display(obj%maxFaceOrder, "maxFaceOrder: ", unitno=unitno)
CALL Display(obj%maxEdgeOrder, "maxEdgeOrder: ", unitno=unitno)

isok = ASSOCIATED(obj%mesh)
CALL Display(isok, "mesh ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%mesh%DisplayMeshInfo("Mesh information: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%cellOrder)
CALL Display(isok, "cellOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellOrder), "cellOrder size: ", &
                       unitno=unitno)

isok = ALLOCATED(obj%faceOrder)
CALL Display(isok, "faceOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%faceOrder), "faceOrder size: ", &
                       unitno=unitno)

isok = ALLOCATED(obj%edgeOrder)
CALL Display(isok, "edgeOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%edgeOrder), "edgeOrder size: ", &
                       unitno=unitno)

isok = ALLOCATED(obj%edgeIA)
CALL Display(isok, "edgeIA ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%edgeIA), "edgeIA size: ", unitno=unitno)

isok = ALLOCATED(obj%faceIA)
CALL Display(isok, "faceIA ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%faceIA), "faceIA size: ", unitno=unitno)

isok = ALLOCATED(obj%cellIA)
CALL Display(isok, "cellIA ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellIA), "cellIA size: ", unitno=unitno)

DO ii = 1, SIZE(obj%fe)
  isok = ASSOCIATED(obj%fe(ii)%ptr)
  CALL Display(isok, "fe(ii)%ptr ASSOCIATED: ", unitno=unitno)
END DO

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                           DisplayCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayCellOrder
LOGICAL(LGT) :: isok
isok = ALLOCATED(obj%cellOrder)
CALL Display(isok, "cellOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellOrder), "cellOrder size: ", &
                       unitno=unitno)
CALL Display(obj%cellOrder, "cellOrder: ", unitno=unitno, full=full)
END PROCEDURE obj_DisplayCellOrder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
