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

SUBMODULE(FEDOF_Class) SetMethods
USE BaseType, ONLY: ReferenceElementInfo => TypeRefelemOpt
USE basetype, ONLY: TypeMeshOpt
USE Display_Method, ONLY: ToString
USE ElemData_Class, ONLY: ElemData_
USE ElemData_Class, ONLY: ElemData_GetTotalEntities
USE ElemData_Class, ONLY: ElemData_GetEdge
USE ElemData_Class, ONLY: ElemData_GetEdgeConnectivity
USE ElemData_Class, ONLY: ElemData_GetElementToElements
USE ElemData_Class, ONLY: ElemData_GetFace
USE IntegerUtility, ONLY: GetIntersection
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "FEDOF_Class@SetMethods.F90"
#endif
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCellOrder()"
#endif

INTEGER(I4B) :: tsize, ii, jj
LOGICAL(LGT) :: isok
INTEGER(INT8) :: int8_order

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%cellOrder)
CALL AssertError1(isok, myName, "obj%cellOrder is not allocated.")

CALL AssertError2(SIZE(obj%cellOrder), obj%tCells, myName, &
                  "a=size(obj%cellOrder), b=obj%tCells.")
#endif

! CALL Reallocate(obj%cellOrder, obj%tCells)
obj%cellOrder(1:obj%tCells) = 0

tsize = SIZE(order)

IF (tsize .EQ. 1) THEN
  int8_order = INT(order(1), KIND=INT8)
  obj%cellOrder(1:obj%tCells) = int8_order

ELSE

  DO ii = 1, tsize
    isok = obj%mesh%IsElementPresent(globalElement=ii, islocal=islocal)

    IF (isok) THEN
      jj = obj%mesh%GetLocalElemNumber(globalElement=ii, islocal=islocal)
      int8_order = INT(order(ii), KIND=INT8)
      ! IF (jj .NE. 0)
      obj%cellOrder(jj) = int8_order
    END IF

  END DO
END IF

obj%maxCellOrder = MAXVAL(obj%cellOrder)
obj%maxFaceOrder = obj%maxCellOrder
obj%maxEdgeOrder = obj%maxFaceOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCellOrder

!----------------------------------------------------------------------------
!                                                             SetFaceOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFaceOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFaceOrder()"
#endif

INTEGER(I4B) :: nrow, ncol, ii, iel, jj, kk
LOGICAL(LGT) :: isok
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT), ALLOCATABLE :: foundFaces(:)
INTEGER(I4B), ALLOCATABLE :: e2e(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%faceOrder)
CALL AssertError1(isok, myName, &
                  "obj%faceOrder is not allocated.")

CALL AssertError2(SIZE(obj%faceOrder), obj%tFaces, myName, &
                  "a=size(obj%faceOrder), b=obj%tFaces.")
#endif

! Handle brokenSpaces
IF (obj%isBrokenSpace) THEN
  obj%faceOrder = 0
  obj%maxFaceOrder = 0
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! Following code is executed for continuous case
! Allocation
CALL Reallocate(foundFaces, obj%tFaces)
CALL Reallocate(e2e, ReferenceElementInfo%maxFaces, 3)

obj%faceOrder(1:obj%tFaces) = 0

loop1: DO iel = 1, obj%tCells
  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE loop1
  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=math%yes)

#ifdef DEBUG_VER
  isok = ASSOCIATED(elemdata)
  IF (.NOT. isok) CYCLE loop1
#endif

  ! faces
  CALL ElemData_GetElementToElements( &
    obj=elemdata, ans=e2e, nrow=nrow, ncol=ncol, &
    includeBoundaryElement=math%yes)

  loop2: DO ii = 1, nrow
    kk = ElemData_GetFace(elemdata, e2e(ii, 2))
    IF (foundFaces(kk)) CYCLE loop2

    foundFaces(kk) = math%yes

    jj = obj%mesh%GetLocalElemNumber(globalElement=e2e(ii, 1), &
                                     islocal=math%no)

    obj%faceOrder(kk) = MIN(obj%cellOrder(iel), obj%cellOrder(jj))
  END DO loop2
END DO loop1

IF (ALLOCATED(foundFaces)) DEALLOCATE (foundFaces)
IF (ALLOCATED(e2e)) DEALLOCATE (e2e)
elemdata => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFaceOrder

!----------------------------------------------------------------------------
!                                                               SetEdgeOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetEdgeOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetEdgeOrder()"
#endif

INTEGER(I4B) :: tsize, ii, iel, jj, kk, tsize1, tsize2
INTEGER(I4B) :: ent(4), edgeCon(2)
LOGICAL(LGT) :: isok
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT), ALLOCATABLE :: foundEdges(:)
INTEGER(I4B), ALLOCATABLE :: n2e1(:), n2e2(:), n2e(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%edgeOrder)
CALL AssertError1(isok, myName, "obj%edgeOrder is not allocated.")
CALL AssertError2(SIZE(obj%edgeOrder), obj%tEdges, myName, &
                  "a=size(obj%edgeOrder), b=obj%tEdges.")
#endif

! Handle broken spaces
IF (obj%isBrokenSpace) THEN
  obj%edgeOrder = 0
  obj%maxEdgeOrder = 0
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! Following code is executed for continuous case
! Allocation
CALL Reallocate(foundEdges, obj%tEdges)
CALL Reallocate(n2e1, TypeMeshOpt%maxNodeToElem)
CALL Reallocate(n2e2, TypeMeshOpt%maxNodeToElem)
CALL Reallocate(n2e, TypeMeshOpt%maxNodeToElem)

obj%edgeOrder(1:obj%tEdges) = 0

loop1: DO iel = 1, obj%tCells
  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE loop1
  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=math%yes)

#ifdef DEBUG_VER
  isok = ASSOCIATED(elemdata)
  IF (.NOT. isok) CYCLE loop1
#endif

  ent = ElemData_GetTotalEntities(elemdata)

  !edges
  loop2: DO ii = 1, ent(2)
    jj = ElemData_GetEdge(elemdata, ii)
    IF (foundEdges(jj)) CYCLE loop2

    foundEdges(jj) = math%yes
    CALL ElemData_GetEdgeConnectivity(obj=elemdata, ii=ii, ans=edgeCon, &
                                      tsize=tsize)

    CALL obj%mesh%GetNodeToElements_(globalNode=edgeCon(1), &
                                     islocal=math%no, ans=n2e1, &
                                     tsize=tsize1)

    CALL obj%mesh%GetNodeToElements_(globalNode=edgeCon(2), &
                                     islocal=math%no, ans=n2e2, &
                                     tsize=tsize2)

    CALL GetIntersection(a=n2e1(1:tsize1), b=n2e2(1:tsize2), c=n2e, &
                         tsize=tsize)

    DO kk = 1, tsize
      n2e(kk) = obj%mesh%GetLocalElemNumber(globalElement=n2e(kk), &
                                            islocal=math%no)
    END DO

    obj%edgeOrder(jj) = MINVAL(obj%cellOrder(n2e(1:tsize)))
  END DO loop2
END DO loop1

IF (ALLOCATED(foundEdges)) DEALLOCATE (foundEdges)
IF (ALLOCATED(n2e1)) DEALLOCATE (n2e1)
IF (ALLOCATED(n2e2)) DEALLOCATE (n2e2)
IF (ALLOCATED(n2e)) DEALLOCATE (n2e)
elemdata => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetEdgeOrder

!----------------------------------------------------------------------------
!                                                                      SetFE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFE()"
#endif

INTEGER(I4B) :: ii, iel, cellOrder(3), &
                faceOrder(3, ReferenceElementInfo%maxEdges), &
                edgeOrder(ReferenceElementInfo%maxEdges), &
                faceOrient(3, ReferenceElementInfo%maxEdges), &
                edgeOrient(ReferenceElementInfo%maxEdges), &
                cellOrient(3), indx(10)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                  islocal=islocal)

ii = obj%mesh%GetElemTopologyIndx(globalElement=iel, islocal=math%yes)

CALL obj%GetOrders(globalElement=iel, islocal=math%yes, cellOrder=cellOrder, &
                   faceOrder=faceOrder, edgeOrder=edgeOrder, &
                   cellOrient=cellOrient, faceOrient=faceOrient, &
                   edgeOrient=edgeOrient, tCellOrder=indx(1), &
                   tFaceOrder=indx(2), tEdgeOrder=indx(3), &
                   tCellOrient=indx(4), tFaceOrient=indx(5:6), &
                   tEdgeOrient=indx(7))

IF (indx(1) .EQ. 1) THEN
  cellOrder(2:3) = cellOrder(1)
  indx(1) = 3
END IF

CALL obj%fe(ii)%ptr%SetOrder(order=cellOrder(1), cellOrder=cellOrder, &
                             faceOrder=faceOrder, edgeOrder=edgeOrder, &
                             errCheck=math%yes, tCell=indx(1), &
                             tFace=indx(2), tEdge=indx(3))

IF (.NOT. obj%isBrokenSpace) THEN
  CALL obj%fe(ii)%ptr%SetOrientation(cellOrient=cellOrient, &
                                     faceOrient=faceOrient, &
                                     edgeOrient=edgeOrient, &
                                     errCheck=math%yes, tCell=indx(1), &
                                     tFace=indx(2), tEdge=indx(3))
END IF

! Make order of integrand in cellOrder
cellOrder(1:3) = cellOrder(1:3) * obj%scaleForQuadOrder
CALL obj%fe(ii)%ptr%SetQuadratureOrder(order=cellOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFE

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
