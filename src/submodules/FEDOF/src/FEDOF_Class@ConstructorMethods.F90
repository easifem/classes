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
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE ElemData_Class, ONLY: ElemData_, &
                          ElemData_GetTotalEntities, &
                          ElemData_GetEdge, &
                          ElemData_GetFace, &
                          ElemData_GetCell, &
                          ElemData_GetTotalEdgeDOF, &
                          ElemData_GetTotalFaceDOF, &
                          ElemData_GetTotalCellDOF

USE StringUtility, ONLY: UpperCase
USE AbstractFE_Class, ONLY: AbstractFEDeallocate
USE BaseType, ONLY: TypeInterpolationOpt, &
                    TypePolynomialOpt
USE ReferenceElement_Method, ONLY: eleminfo => ReferenceElementInfo, &
                                   GetElementIndex
USE FEFactoryUtility, ONLY: FEFactory

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

INTEGER(I4B) :: order0(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order0 = order

CALL obj%Initiate(order=order0, dom=dom, baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, feType=feType, &
                  ipType=ipType, basisType=basisType, alpha=alpha, &
                  beta=beta, lambda=lambda, &
                  dofType=dofType, &
                  transformType=transformType, &
                  quadratureIsHomogeneous=quadratureIsHomogeneous, &
                  quadratureType=quadratureType, &
                  quadratureOrder=quadratureOrder, &
                  quadratureIsOrder=quadratureIsOrder, &
                  quadratureNips=quadratureNips, &
                  quadratureIsNips=quadratureIsNips, &
                  quadratureAlpha=quadratureAlpha, &
                  quadratureBeta=quadratureBeta, &
                  quadratureLambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, elemType, nsd, jj
INTEGER(I4B) :: totalTopo, topoList(8)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%baseInterpolation = UpperCase(baseInterpolation(1:4))
IF (obj%baseInterpolation == "LAGR") obj%isLagrange = .TRUE.

#ifdef DEBUG_VER
IF (obj%isLagrange) THEN
  isok = PRESENT(ipType)
  CALL AssertError1(isok, myName, "ipType should be present")
END IF
#endif

obj%baseContinuity = UpperCase(baseContinuity(1:2))

obj%dom => dom
obj%mesh => dom%GetMeshPointer()

CALL obj%AllocateSizes()
CALL obj%SetCellOrder(order=order, islocal=islocal)
CALL obj%SetFaceOrder()
CALL obj%SetEdgeOrder()
CALL obj%SetOrdersFromCellOrder()

topoList = obj%mesh%GetElemTopology()
totalTopo = obj%mesh%GetTotalTopology()
nsd = obj%mesh%GetNSD()

DO ii = 1, totalTopo
  elemType = topoList(ii)
  jj = GetElementIndex(elemType)

  obj%fe(jj)%ptr => FEFactory(elemType=elemType, &
                              baseContinuity=obj%baseContinuity, &
                              baseInterpolation=obj%baseInterpolation)

  CALL obj%fe(jj)%ptr%Initiate( &
    elemType=elemType, nsd=nsd, baseContinuity=obj%baseContinuity, &
    baseInterpolation=obj%baseInterpolation, ipType=ipType, &
    basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, &
    feType=feType, dofType=dofType, transformType=transformType, &
    quadratureIsHomogeneous=quadratureIsHomogeneous, &
    quadratureType=quadratureType, quadratureOrder=quadratureOrder, &
    quadratureIsOrder=quadratureIsOrder, quadratureNips=quadratureNips, &
    quadratureIsNips=quadratureIsNips, quadratureAlpha=quadratureAlpha, &
    quadratureBeta=quadratureBeta, quadratureLambda=quadratureLambda)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B), ALLOCATABLE :: order0(:)
INTEGER(I4B) :: telems, tsize, globalElement, localElement, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

telems = dom%GetTotalElements()
tsize = SIZE(order, 2)

#ifdef DEBUG_VER
isok = SIZE(order, 1) .NE. 2
CALL AssertError1(isok, myName, &
                  'number of rows of order array is not equal to 2')

isok = tsize .NE. telems
CALL AssertError1(isok, myName, &
           'number of cols of order array is not equal to number of elements')
#endif

ALLOCATE (order0(telems))

DO ii = 1, telems
  globalElement = order(1, ii)
  localElement = dom%GetLocalElemNumber(globalElement=globalElement, &
                                        islocal=.FALSE.)
  order0(localElement) = order(2, ii)
END DO

CALL obj%Initiate(dom=dom, baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, order=order0, &
                  ipType=ipType, feType=feType, basisType=basisType, &
                  alpha=alpha, beta=beta, lambda=lambda, islocal=.TRUE., &
                  dofType=dofType, &
                  transformType=transformType, &
                  quadratureIsHomogeneous=quadratureIsHomogeneous, &
                  quadratureType=quadratureType, &
                  quadratureOrder=quadratureOrder, &
                  quadratureIsOrder=quadratureIsOrder, &
                  quadratureNips=quadratureNips, &
                  quadratureIsNips=quadratureIsNips, &
                  quadratureAlpha=quadratureAlpha, &
                  quadratureBeta=quadratureBeta, &
                  quadratureLambda=quadratureLambda)

DEALLOCATE (order0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                     FEDOF_Initiate_Help
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateSizes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateSizes()"
#endif

INTEGER(I4B) :: ent(4)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!info: It returns total number of nodes, edges, faces, and cells in the mesh
ent = obj%mesh%GetTotalEntities()

!info: obj%tNodes is set to the total number of vertex nodes
! Read more about the totalNodes and totalVertexNodes in the
! AbstractMesh_Class. In future, tNodes will be termed as tVertexNodes
! for consistency.
obj%tNodes = obj%mesh%GetTotalVertexNodes()
obj%tEdges = ent(2)
obj%tFaces = ent(3)
obj%tCells = ent(4)

CALL Reallocate(obj%cellOrder, obj%tCells)
CALL Reallocate(obj%faceOrder, obj%tFaces)
CALL Reallocate(obj%edgeOrder, obj%tEdges)

CALL Reallocate(obj%edgeIA, obj%tEdges + 1)
CALL Reallocate(obj%faceIA, obj%tFaces + 1)
CALL Reallocate(obj%cellIA, obj%tCells + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateSizes

!----------------------------------------------------------------------------
!                                                     FEDOF_Initiate_Help
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrdersFromCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrdersFromCellOrder()"
#endif

! Internal variables
LOGICAL(LGT), PARAMETER :: yes = .TRUE.
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

! Check it all valid ElemDataPointer are associated
#ifdef DEBUG_VER
DO iel = 1, obj%tCells

  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=yes)
  IF (.NOT. isok) CYCLE

  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=yes)

  isok = ASSOCIATED(elemdata)
  CALL AssertError1(isok, myName, 'elemdata is not allocated')

END DO
#endif

cellLoop: DO iel = 1, obj%tCells

  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=yes)
  IF (.NOT. isok) CYCLE

  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, islocal=yes)

  ent = ElemData_GetTotalEntities(elemdata)

  ! edges
  edgeLoop: DO ii = 1, ent(2)
    jj = ElemData_GetEdge(elemdata, ii)
    isok = foundEdges(jj)

    ! if edge node found
    IF (.NOT. isok) THEN
      myorder = INT(obj%edgeOrder(jj), kind=INT8)
      tsize = ElemData_GetTotalEdgeDOF( &
              obj=elemdata, ii=ii, order=myorder, &
              baseContinuity=obj%baseContinuity, &
              baseInterpolation=obj%baseInterpolation)

      tdof = tdof + tsize
      obj%edgeIA(jj + 1) = obj%edgeIA(jj) + tsize
      tedgedof = tedgedof + tsize

      foundEdges(jj) = .TRUE.
    END IF

  END DO edgeLoop

  ! faces
  faceLoop: DO ii = 1, ent(3)

    jj = ElemData_GetFace(elemdata, ii)

    isok = foundFaces(jj)

    ! if face not found
    IF (.NOT. isok) THEN

      myorder = INT(obj%faceOrder(jj), kind=INT8)

      tsize = ElemData_GetTotalFaceDOF( &
              obj=elemdata, ii=ii, order=myorder, &
              baseContinuity=obj%baseContinuity, &
              baseInterpolation=obj%baseInterpolation)

      tdof = tdof + tsize
      foundFaces(jj) = .TRUE.

      obj%faceIA(jj + 1) = obj%faceIA(jj) + tsize
      tfacedof = tfacedof + tsize

    END IF

  END DO faceLoop

  ! cell
  jj = ElemData_GetCell(obj=elemdata, islocal=.TRUE.)
  isok = foundCells(jj)

  ! if cell not found
  IF (.NOT. isok) THEN
    myorder = INT(obj%cellOrder(jj), kind=INT8)
    tsize = ElemData_GetTotalCellDOF(obj=elemdata, order=myorder, &
                                     baseContinuity=obj%baseContinuity, &
                                     baseInterpolation=obj%baseInterpolation)
    tdof = tdof + tsize
    foundCells(jj) = .TRUE.

    obj%cellIA(jj + 1) = obj%cellIA(jj) + tsize

    tcelldof = tcelldof + tsize

  END IF

END DO cellLoop

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
END PROCEDURE obj_SetOrdersFromCellOrder

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

LOGICAL(LGT) :: abool
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%isLagrange = .FALSE.
obj%isMaxConSet = .FALSE.
obj%isMaxQuadPointSet = .FALSE.

obj%tdof = 0
obj%tNodes = 0
obj%tEdges = 0
obj%tFaces = 0
obj%tCells = 0
obj%maxCon = 0
obj%maxQuadPoint = 0

obj%baseContinuity = "H1"
obj%baseInterpolation = "LAGR"

obj%scaleForQuadOrder = 2_INT8
obj%maxCellOrder = 0_INT8
obj%maxFaceOrder = 0_INT8
obj%maxEdgeOrder = 0_INT8

obj%mesh => NULL()
obj%dom => NULL()
IF (ALLOCATED(obj%cellOrder)) DEALLOCATE (obj%cellOrder)
IF (ALLOCATED(obj%faceOrder)) DEALLOCATE (obj%faceOrder)
IF (ALLOCATED(obj%edgeOrder)) DEALLOCATE (obj%edgeOrder)
IF (ALLOCATED(obj%edgeIA)) DEALLOCATE (obj%edgeIA)
IF (ALLOCATED(obj%faceIA)) DEALLOCATE (obj%faceIA)
IF (ALLOCATED(obj%cellIA)) DEALLOCATE (obj%cellIA)

DO ii = 1, SIZE(obj%fe)
  abool = ASSOCIATED(obj%fe(ii)%ptr)
  IF (abool) THEN
    CALL obj%fe(ii)%ptr%DEALLOCATE()
    obj%fe(ii)%ptr => NULL()
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isLagrange = obj2%isLagrange
obj%isInit = obj2%isInit
obj%isMaxConSet = obj2%isMaxConSet
obj%isMaxQuadPointSet = obj2%isMaxQuadPointSet

obj%tdof = obj2%tdof
obj%tNodes = obj2%tNodes
obj%tEdges = obj2%tEdges
obj%tFaces = obj2%tFaces
obj%tCells = obj2%tCells
obj%maxCon = obj2%maxCon
obj%maxQuadPoint = obj2%maxQuadPoint

obj%baseContinuity = obj2%baseContinuity
obj%baseInterpolation = obj2%baseInterpolation
obj%scaleForQuadOrder = obj2%scaleForQuadOrder
obj%maxCellOrder = obj2%maxCellOrder
obj%maxFaceOrder = obj2%maxFaceOrder
obj%maxEdgeOrder = obj2%maxEdgeOrder

isok = ALLOCATED(obj2%cellOrder)
IF (isok) obj%cellOrder = obj2%cellOrder

isok = ALLOCATED(obj2%faceOrder)
IF (isok) obj%faceOrder = obj2%faceOrder

isok = ALLOCATED(obj2%edgeOrder)
IF (isok) obj%edgeOrder = obj2%edgeOrder

isok = ALLOCATED(obj2%edgeIA)
IF (isok) obj%edgeIA = obj2%edgeIA

isok = ALLOCATED(obj2%faceIA)
IF (isok) obj%faceIA = obj2%faceIA

isok = ALLOCATED(obj2%cellIA)
IF (isok) obj%cellIA = obj2%cellIA

DO ii = 1, SIZE(obj2%fe)
  obj%fe(ii)%ptr => obj2%fe(ii)%ptr
END DO

obj%mesh => obj2%mesh
obj%dom => obj2%dom

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateGeoFEDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateGeoFEDOF()"
#endif

INTEGER(I4B), ALLOCATABLE :: order(:)
INTEGER(I4B) :: telements
CLASS(AbstractMesh_), POINTER :: mesh
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mesh => dom%GetMeshPointer()
telements = mesh%GetTotalElements()
CALL Reallocate(order, telements)
CALL mesh%GetOrder_(order=order, tsize=telements)
mesh => NULL()

CALL obj%Initiate(order=order, dom=dom, baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, feType=feType, &
                  ipType=ipType, basisType=basisType, alpha=alpha, &
                  lambda=lambda, beta=beta, islocal=yes, dofType=dofType, &
                  transformType=transformType, &
                  quadratureIsHomogeneous=quadratureIsHomogeneous, &
                  quadratureType=quadratureType, &
                  quadratureOrder=quadratureOrder, &
                  quadratureIsOrder=quadratureIsOrder, &
                  quadratureNips=quadratureNips, &
                  quadratureIsNips=quadratureIsNips, &
                  quadratureAlpha=quadratureAlpha, &
                  quadratureBeta=quadratureBeta, &
                  quadratureLambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateGeoFEDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
