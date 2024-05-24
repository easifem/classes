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

SUBMODULE(FEDOF_Class) ConstructorMethods
USE ReallocateUtility, ONLY: Reallocate
USE ElemData_Class, ONLY: ElemData_, &
                          ElemData_GetTotalEntities, &
                          ElemData_GetEdge, &
                          ElemData_GetFace, &
                          ElemData_GetCell, &
                          ElemData_GetTotalEdgeDOF, &
                          ElemData_GetTotalFaceDOF, &
                          ElemData_GetTotalCellDOF, &
                          ElemData_GetElementToElements, &
                          ElemData_GetedgeConnectivity

USE ReferenceElement_Method, ONLY: PARAM_REFELEM_MAX_FACES

USE AbstractMesh_Class, ONLY: PARAM_MAX_NODE_TO_ELEM

USE IntegerUtility, ONLY: GetIntersection

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetFEDOFParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetFEDOFParam
CHARACTER(*), PARAMETER :: myName = "SetFEDOFParam()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE SetFEDOFParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
INTEGER(I4B) :: ent(4), tsize, iel, tedgedof, tfacedof, tcelldof, ii, jj, &
                tdof, myorder
LOGICAL(LGT), ALLOCATABLE :: foundEdges(:), foundFaces(:), foundCells(:)
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT) :: isok, isLagrange

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (baseInterpolation(1:3) == "Lag") THEN
  isLagrange = .TRUE.
ELSE
  isLagrange = .FALSE.
END IF

CALL obj%DEALLOCATE()

obj%mesh => mesh
obj%baseContinuity = baseContinuity
obj%baseInterpolation = baseInterpolation

CALL FEDOF_Initiate_Before(obj=obj, isLagrange=isLagrange)

DO CONCURRENT(ii=1:obj%tCells)
  obj%cellOrder(ii) = INT(order, kind=INT8)
END DO

DO CONCURRENT(ii=1:obj%tFaces)
  obj%faceOrder(ii) = INT(order, kind=INT8)
END DO

DO CONCURRENT(ii=1:obj%tEdges)
  obj%edgeOrder(ii) = INT(order, kind=INT8)
END DO

CALL FEDOF_Initiate_After(obj=obj, isLagrange=isLagrange)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
INTEGER(I4B) :: ent(4)
LOGICAL(LGT) :: isLaGrange

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

IF (baseInterpolation(1:3) == "Lag") THEN
  isLagrange = .TRUE.
ELSE
  isLagrange = .FALSE.
END IF

obj%mesh => mesh
obj%baseContinuity = baseContinuity
obj%baseInterpolation = baseInterpolation
CALL FEDOF_Initiate_Before(obj=obj, isLagrange=isLagrange)

CALL obj%SetCellOrder(cellOrder=order)
CALL obj%SetFaceOrder(cellOrder=order)
CALL obj%SetEdgeOrder(cellOrdeR=order)

CALL FEDOF_Initiate_After(obj=obj, isLagrange=isLagrange)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
INTEGER(I4B), ALLOCATABLE :: order0(:)
INTEGER(I4B) :: telems, tsize, globalElement, localElement, ii
LOGICAL(LGT) :: problem

CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

telems = mesh%GetTotalElements()
tsize = SIZE(order, 2)

problem = SIZE(order, 1) .NE. 2
IF (problem) THEN

  CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[INTERNAL ERROR] :: number of rows of order array is not equal to 2')
  RETURN
END IF

problem = tsize .NE. telems
IF (problem) THEN

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                   '[INTERNAL ERROR] :: number of cols of order array is '// &
                    'not equal to number of elements')
  RETURN
END IF

ALLOCATE (order0(telems))

DO ii = 1, telems
  globalElement = order(1, ii)
  localElement = mesh%GetLocalElemNumber(globalElement=globalElement, &
                                         islocal=.FALSE.)
  order0(localElement) = order(2, ii)
END DO

CALL obj%Initiate(mesh=mesh, baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, order=order0)

DEALLOCATE (order0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                     FEDOF_Initiate_Help
!----------------------------------------------------------------------------

SUBROUTINE FEDOF_Initiate_Before(obj, isLagrange)
  CLASS(FEDOF_), INTENT(INOUT) :: obj
  LOGICAL(LGT), INTENT(IN) :: isLagrange

  !! internal variables
  INTEGER(I4B) :: ent(4)

  ent = obj%mesh%GetTotalEntities()
  obj%tNodes = ent(1)

  IF (isLagrange) THEN
    obj%tEdges = 0
    obj%tFaces = 0
    obj%tCells = 0
  ELSE
    obj%tEdges = ent(2)
    obj%tFaces = ent(3)
    obj%tCells = ent(4)
  END IF

  CALL Reallocate(obj%cellOrder, obj%tCells)
  CALL Reallocate(obj%faceOrder, obj%tFaces)
  CALL Reallocate(obj%edgeOrder, obj%tEdges)

  CALL Reallocate(obj%edgeIA, obj%tEdges + 1)
  CALL Reallocate(obj%faceIA, obj%tFaces + 1)
  CALL Reallocate(obj%cellIA, obj%tCells + 1)

END SUBROUTINE FEDOF_Initiate_Before

!----------------------------------------------------------------------------
!                                                     FEDOF_Initiate_Help
!----------------------------------------------------------------------------

SUBROUTINE FEDOF_Initiate_After(obj, isLagrange)
  CLASS(FEDOF_), INTENT(INOUT) :: obj
  LOGICAL(LGT), INTENT(IN) :: isLagrange

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "FEDOF_Initiate_After()"
  INTEGER(I4B) :: ent(4), tsize, iel, tedgedof, tfacedof, tcelldof, ii, jj, &
                  tdof, myorder
  LOGICAL(LGT), ALLOCATABLE :: foundEdges(:), foundFaces(:), foundCells(:)
  TYPE(ElemData_), POINTER :: elemdata
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Reallocate(foundEdges, obj%tEdges)
  CALL Reallocate(foundFaces, obj%tFaces)
  CALL Reallocate(foundCells, obj%tCells)

  tedgedof = 0
  tfacedof = 0
  tcelldof = 0

  tdof = obj%tNodes

  obj%edgeIA(1) = 1
  obj%faceIA(1) = 1
  obj%cellIA(1) = 1

  DO iel = 1, obj%tCells

    isok = obj%mesh%IsElementActive(globalElement=iel, islocal=.TRUE.)
    IF (.NOT. isok) CYCLE

    elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=.TRUE.)

    ent = ElemData_GetTotalEntities(elemdata)

    ! edges
    DO ii = 1, ent(2)
      jj = ElemData_GetEdge(elemdata, ii)
      IF (.NOT. foundEdges(jj)) THEN
        myorder = INT(obj%edgeOrder(jj), kind=INT8)
        tsize = ElemData_GetTotalEdgeDOF(obj=elemdata, ii=ii, &
                                         order=myorder, &
                                         baseContinuity=obj%baseContinuity, &
                                      baseInterpolation=obj%baseInterpolation)
        tdof = tdof + tsize
        obj%edgeIA(jj + 1) = obj%edgeIA(jj) + tsize
        tedgedof = tedgedof + tsize

        foundEdges(jj) = .TRUE.
      END IF
    END DO

    ! faces
    DO ii = 1, ent(3)
      jj = ElemData_GetFace(elemdata, ii)

      IF (.NOT. foundFaces(jj)) THEN
        myorder = INT(obj%faceOrder(jj), kind=INT8)
        tsize = ElemData_GetTotalFaceDOF(obj=elemdata, ii=ii, &
                                         order=myorder, &
                                         baseContinuity=obj%baseContinuity, &
                                      baseInterpolation=obj%baseInterpolation)
        tdof = tdof + tsize
        foundFaces(jj) = .TRUE.

        obj%faceIA(jj + 1) = obj%faceIA(jj) + tsize
        tfacedof = tfacedof + tsize

      END IF
    END DO

    ! cell
    jj = ElemData_GetCell(obj=elemdata, islocal=.TRUE.)
    IF (.NOT. foundCells(jj)) THEN
      myorder = INT(obj%cellOrder(jj), kind=INT8)
      tsize = ElemData_GetTotalCellDOF(obj=elemdata, &
                                       order=myorder, &
                                       baseContinuity=obj%baseContinuity, &
                                      baseInterpolation=obj%baseInterpolation)
      tdof = tdof + tsize
      foundCells(jj) = .TRUE.

      obj%cellIA(jj + 1) = obj%cellIA(jj) + tsize

      tcelldof = tcelldof + tsize

    END IF

  END DO

  DO CONCURRENT(ii=1:obj%tEdges + 1)
    obj%edgeIA(ii) = obj%edgeIA(ii) + obj%tNodes
  END DO

  jj = obj%tNodes + tedgedof

  DO CONCURRENT(ii=1:obj%tFaces + 1)
    obj%faceIA(ii) = obj%faceIA(ii) + jj
  END DO

  jj = jj + tfacedof
  DO CONCURRENT(ii=1:obj%tCells + 1)
    obj%cellIA(ii) = obj%cellIA(ii) + jj
  END DO

  obj%tdof = tdof

  IF (ALLOCATED(foundEdges)) DEALLOCATE (foundEdges)
  IF (ALLOCATED(foundFaces)) DEALLOCATE (foundFaces)
  IF (ALLOCATED(foundCells)) DEALLOCATE (foundCells)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE FEDOF_Initiate_After

!----------------------------------------------------------------------------
!                                                             SetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellOrder
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok
CHARACTER(*), PARAMETER :: myName = "obj_SetCellOrder()"

tsize = SIZE(cellOrder)

isok = tsize .EQ. obj%tCells
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[ERROR] :: Size of order array is not equal to number of cells')
  RETURN
END IF

CALL Reallocate(obj%cellOrder, tsize)
DO CONCURRENT(ii=1:tsize)
  obj%cellOrder(ii) = INT(cellOrder(ii), kind=INT8)
END DO

END PROCEDURE obj_SetCellOrder

!----------------------------------------------------------------------------
!                                                             SetFaceOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFaceOrder
INTEGER(I4B) :: nrow, ncol, ii, iel, jj, kk, e2e(PARAM_REFELEM_MAX_FACES, 3)
LOGICAL(LGT) :: isok
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT), ALLOCATABLE :: foundFaces(:)

! main
CALL Reallocate(foundFaces, obj%tFaces)
CALL Reallocate(obj%faceOrder, obj%tFaces)

DO iel = 1, obj%tCells

  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=.TRUE.)
  isok = ASSOCIATED(elemdata)
  IF (.NOT. isok) CYCLE

  ! faces
  CALL ElemData_GetElementToElements(obj=elemdata, ans=e2e, nrow=nrow, &
                                     ncol=ncol, includeBoundaryElement=.TRUE.)

  DO ii = 1, nrow
    kk = ElemData_GetFace(elemdata, e2e(ii, 2))
    IF (foundFaces(kk)) CYCLE

    foundFaces(kk) = .TRUE.

   jj = obj%mesh%GetLocalElemNumber(globalElement=e2e(ii, 1), islocal=.FALSE.)
    obj%faceOrder(kk) = MIN(obj%cellOrder(iel), obj%cellOrder(jj))
  END DO

END DO

END PROCEDURE obj_SetFaceOrder

!----------------------------------------------------------------------------
!                                                               SetEdgeOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetEdgeOrder
INTEGER(I4B) :: tsize, ii, iel, ent(4), jj, &
                kk, edgeCon(2), n2e1(PARAM_MAX_NODE_TO_ELEM), &
                n2e2(PARAM_REFELEM_MAX_FACES), n2e(PARAM_MAX_NODE_TO_ELEM), &
                tsize1, tsize2
LOGICAL(LGT) :: isok
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT), ALLOCATABLE :: foundEdges(:)

! main
CALL Reallocate(foundEdges, obj%tEdges)
CALL Reallocate(obj%edgeOrder, obj%tEdges)

DO iel = 1, obj%tCells

  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=.TRUE.)
  IF (.NOT. isok) CYCLE

  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=.TRUE.)
  isok = ASSOCIATED(elemdata)
  IF (.NOT. isok) CYCLE

  ent = ElemData_GetTotalEntities(elemdata)

  !edges
  DO ii = 1, ent(2)
    jj = ElemData_GetEdge(elemdata, ii)
    IF (foundEdges(jj)) CYCLE

    foundEdges(jj) = .TRUE.
    CALL ElemData_GetEdgeConnectivity(obj=elemdata, ii=ii, ans=edgeCon, &
                                      tsize=tsize)

    CALL obj%mesh%GetNodeToElements_(globalNode=edgeCon(1), islocal=.FALSE., &
                                     ans=n2e1, tsize=tsize1)

    CALL obj%mesh%GetNodeToElements_(globalNode=edgeCon(2), islocal=.FALSE., &
                                     ans=n2e2, tsize=tsize2)

    CALL GetIntersection(a=n2e1(1:tsize1), b=n2e2(1:tsize2), c=n2e, &
                         tsize=tsize)

    DO kk = 1, tsize
      n2e(kk) = obj%mesh%GetLocalElemNumber(globalElement=n2e(kk), &
                                            islocal=.FALSE.)
    END DO

    obj%edgeOrder(jj) = MINVAL(obj%cellOrder(n2e(1:tsize)))

  END DO

END DO

IF (ALLOCATED(foundEdges)) DEALLOCATE (foundEdges)
elemdata => NULL()

END PROCEDURE obj_SetEdgeOrder

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%tdof = 0
obj%tNodes = 0
obj%tEdges = 0
obj%tFaces = 0
obj%tCells = 0
obj%baseContinuity = ""
obj%baseInterpolation = ""
obj%mesh => NULL()
IF (ALLOCATED(obj%cellOrder)) DEALLOCATE (obj%cellOrder)
IF (ALLOCATED(obj%faceOrder)) DEALLOCATE (obj%faceOrder)
IF (ALLOCATED(obj%edgeOrder)) DEALLOCATE (obj%edgeOrder)
IF (ALLOCATED(obj%edgeIA)) DEALLOCATE (obj%edgeIA)
IF (ALLOCATED(obj%faceIA)) DEALLOCATE (obj%faceIA)
IF (ALLOCATED(obj%cellIA)) DEALLOCATE (obj%cellIA)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
obj%tdof = obj2%tdof
obj%tNodes = obj2%tNodes
obj%tEdges = obj2%tEdges
obj%tFaces = obj2%tFaces
obj%tCells = obj2%tCells
obj%baseContinuity = obj2%baseContinuity
obj%baseInterpolation = obj2%baseInterpolation
obj%mesh => obj2%mesh
IF (ALLOCATED(obj2%cellOrder)) obj%cellOrder = obj2%cellOrder
IF (ALLOCATED(obj2%faceOrder)) obj%faceOrder = obj2%faceOrder
IF (ALLOCATED(obj2%edgeOrder)) obj%edgeOrder = obj2%edgeOrder
IF (ALLOCATED(obj2%edgeIA)) obj%edgeIA = obj2%edgeIA
IF (ALLOCATED(obj2%faceIA)) obj%faceIA = obj2%faceIA
IF (ALLOCATED(obj2%cellIA)) obj%cellIA = obj2%cellIA
END PROCEDURE obj_Copy

END SUBMODULE ConstructorMethods
