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
                          ElemData_GetTotalCellDOF

USE FPL_Method, ONLY: Set, GetValue, CheckEssentialParam

USE Display_Method, ONLY: Display

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CHARACTER(:), ALLOCATABLE :: keys

keys = "baseContinuity/baseInterpolation/orderFile"

CALL CheckEssentialParam(obj=param, keys=keys, prefix=myprefix, &
                         myName=myName, modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                             SetFEDOFParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetFEDOFParam
INTEGER(I4B) :: tsize

CALL Set(obj=param, prefix=myprefix, key="baseContinuity", &
         VALUE=baseContinuity, dataType=baseContinuity)

CALL Set(obj=param, prefix=myprefix, key="baseInterpolation", &
         VALUE=baseInterpolation, dataType=baseInterpolation)

CALL Set(obj=param, prefix=myprefix, key="orderFile", &
         VALUE=orderFile, dataType=orderFile)

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

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Cell loop')
#endif

  DO iel = 1, obj%tCells

    isok = obj%mesh%IsElementActive(globalElement=iel, islocal=.TRUE.)
    IF (.NOT. isok) CYCLE

    elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=.TRUE.)

#ifdef DEBUG_VER
    isok = ASSOCIATED(elemdata)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: elemdata is not allocated')
    END IF
#endif

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

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Edge loop')
#endif

  DO CONCURRENT(ii=1:obj%tEdges + 1)
    obj%edgeIA(ii) = obj%edgeIA(ii) + obj%tNodes
  END DO

  jj = obj%tNodes + tedgedof

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Face loop')
#endif

  DO CONCURRENT(ii=1:obj%tFaces + 1)
    obj%faceIA(ii) = obj%faceIA(ii) + jj
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Cell loop')
#endif

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
