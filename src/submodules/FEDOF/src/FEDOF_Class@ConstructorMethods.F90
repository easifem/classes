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
USE AbstractFE_Class, ONLY: AbstractFEDeallocate
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypePolynomialOpt
USE Display_Method, ONLY: ToString
USE ElemData_Class, ONLY: ElemData_
USE ElemData_Class, ONLY: ElemData_GetTotalEntities
USE ElemData_Class, ONLY: ElemData_GetEdge
USE ElemData_Class, ONLY: ElemData_GetFace
USE ElemData_Class, ONLY: ElemData_GetCell
USE ElemData_Class, ONLY: ElemData_GetTotalEdgeDOF
USE ElemData_Class, ONLY: ElemData_GetTotalFaceDOF
USE ElemData_Class, ONLY: ElemData_GetTotalCellDOF
USE ElemData_Class, ONLY: ElemData_Name
USE FEFactoryUtility, ONLY: FEFactory
USE InterpolationUtility, ONLY: InterpolationUtilityGetTotalDOF => GetTotalDOF
USE ReallocateUtility, ONLY: Reallocate
USE ReferenceElement_Method, ONLY: GetElementIndex
USE StringUtility, ONLY: UpperCase
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "FEDOF_Class@ConstructorMethods.F90"
#endif

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
                  baseInterpolation=baseInterpolation, &
                  isBrokenSpace=isBrokenSpace, feType=feType, &
                  ipType=ipType, basisType=basisType, alpha=alpha, &
                  beta=beta, lambda=lambda, dofType=dofType, &
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
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B), PARAMETER :: topolist_size = 8
INTEGER(I4B) :: ii, elemType, nsd, jj
INTEGER(I4B) :: totalTopo, topoList(topolist_size)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = math%yes
IF (PRESENT(isBrokenSpace)) obj%isBrokenSpace = isBrokenSpace

obj%baseInterpolation = UpperCase(baseInterpolation(1:4))
IF (obj%baseInterpolation .EQ. "LAGR") obj%isLagrange = math%yes

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
CALL obj%Set()

topoList = obj%mesh%GetElemTopology()
totalTopo = obj%mesh%GetTotalTopology()
nsd = obj%mesh%GetNSD()

obj%ownFePointers = math%yes
DO ii = 1, totalTopo
  elemType = topoList(ii)
  jj = GetElementIndex(elemType)
  obj%fe(jj)%ptr => FEFactory(elemType=elemType, &
                              baseContinuity=obj%baseContinuity, &
                              baseInterpolation=obj%baseInterpolation, &
                              isBrokenSpace=obj%isBrokenSpace)

  CALL obj%fe(jj)%ptr%Initiate( &
    elemType=elemType, nsd=nsd, baseContinuity=obj%baseContinuity, &
    baseInterpolation=obj%baseInterpolation, isBrokenSpace=isBrokenSpace, &
    ipType=ipType, basisType=basisType, alpha=alpha, beta=beta, &
    lambda=lambda, feType=feType, dofType=dofType, &
    transformType=transformType, &
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

MODULE PROCEDURE obj_Initiate3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
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
isok = SIZE(order, 1) .EQ. 2
CALL AssertError1(isok, myName, &
                  'number of rows of order array is not equal to 2')

isok = tsize .EQ. telems
CALL AssertError1(isok, myName, &
                  'number of cols of order is not equal to number of elements')
#endif

ALLOCATE (order0(telems))

DO ii = 1, telems
  globalElement = order(1, ii)
  localElement = dom%GetLocalElemNumber(globalElement=globalElement, &
                                        islocal=math%no)
  order0(localElement) = order(2, ii)
END DO

CALL obj%Initiate(dom=dom, baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, &
                  isBrokenSpace=isBrokenSpace, order=order0, &
                  ipType=ipType, feType=feType, basisType=basisType, &
                  alpha=alpha, beta=beta, lambda=lambda, islocal=math%yes, &
                  dofType=dofType, transformType=transformType, &
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
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                          InitiateBrokenLagrangeFedofForVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateBrokenLagrangeFedofForVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "obj_InitiateBrokenLagrangeFedofForVTK()"
#endif

INTEGER(I4B) :: tCells, ii, jj, kk
INTEGER(I4B) :: basisType(3)
INTEGER(I4B), ALLOCATABLE :: cellOrders(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tCells = from%GetTotalCells()
CALL Reallocate(cellOrders, tCells)

jj = 0
elem_loop: DO ii = 1, tCells
  isok = from%mesh%IsElementActive(globalElement=ii, islocal=math%yes)
  IF (.NOT. isok) CYCLE elem_loop

  CALL from%GetCellOrder(ans=cellOrders(jj + 1:), tsize=kk, &
                         globalElement=ii, islocal=math%yes)
  jj = jj + kk
END DO elem_loop

basisType = TypePolynomialOpt%monomial
CALL obj%Initiate(order=cellOrders, dom=from%dom, &
                  baseContinuity="H1", baseInterpolation="LAGR", &
                  isBrokenSpace=math%yes, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  basisType=basisType, islocal=math%yes)

IF (ALLOCATED(cellOrders)) DEALLOCATE (cellOrders)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateBrokenLagrangeFedofForVTK

!----------------------------------------------------------------------------
!                                                               AllocateSizes
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

!info: Faces are always numbered by the mesh; edges of a 3D mesh only
! after InitiateEdgeConnectivity (it returns at once if done before)
IF (obj%mesh%GetXidimension() .EQ. 3) THEN
  CALL obj%mesh%InitiateEdgeConnectivity()
END IF

!info: It returns total number of nodes, edges, faces, and cells in the mesh
ent = obj%mesh%GetTotalEntities()

!info: obj%tNodes is set to the total number of vertex nodes
! Read more about the totalNodes and totalVertexNodes in the
! AbstractMesh_Class.
! In future, tNodes will be termed as tVertexNodes for consistency.
IF (obj%isBrokenSpace) THEN
  obj%tNodes = math%zero_i
ELSE
  obj%tNodes = obj%mesh%GetTotalVertexNodes()
END IF

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
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
INTEGER(I4B) :: iel
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Check it all valid ElemDataPointer are associated
#ifdef DEBUG_VER
elemdata => NULL()
loop1: DO iel = 1, obj%tCells

  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE loop1

  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, &
                                          islocal=math%yes)

  isok = ASSOCIATED(elemdata)
  CALL AssertError1(isok, myName, 'elemdata is not allocated')

END DO loop1
#endif

IF (obj%isBrokenSpace) THEN
  CALL obj%SetFedofForH1Broken()
ELSE
  CALL obj%SetFedofForH1()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                              SetFedofForH1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFedofForH1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFedofForH1()"
#endif

! Internal variables
INTEGER(I4B) :: ent(4), tsize, iel, ii, jj, myorder
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

foundEdges = math%no
foundFaces = math%no
foundCells = math%no

! Store counts in slots 2:n+1 before building the cumulative offsets.
! Entities not incident to an active cell have zero interior DOFs.
obj%edgeIA = 0
obj%faceIA = 0
obj%cellIA = 0

cellLoop: DO iel = 1, obj%tCells

  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE cellLoop

  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, &
                                          islocal=math%yes)

  ent = ElemData_GetTotalEntities(elemdata)

  ! edges
  edgeLoop: DO ii = 1, ent(2)
    jj = ElemData_GetEdge(elemdata, ii)
    isok = foundEdges(jj)
    ! if edge not found
    IF (.NOT. isok) THEN
      myorder = INT(obj%edgeOrder(jj), KIND=INT8)
      tsize = ElemData_GetTotalEdgeDOF(obj=elemdata, ii=ii, order=myorder, &
                                       baseContinuity=obj%baseContinuity, &
                                       baseInterpolation= &
                                       obj%baseInterpolation)

      obj%edgeIA(jj + 1) = tsize
      foundEdges(jj) = math%yes
    END IF
  END DO edgeLoop

  ! faces
  faceLoop: DO ii = 1, ent(3)
    jj = ElemData_GetFace(elemdata, ii)
    isok = foundFaces(jj)

    ! if face not found
    IF (.NOT. isok) THEN
      myorder = INT(obj%faceOrder(jj), KIND=INT8)
      tsize = ElemData_GetTotalFaceDOF( &
              obj=elemdata, ii=ii, order=myorder, &
              baseContinuity=obj%baseContinuity, &
              baseInterpolation=obj%baseInterpolation)
      foundFaces(jj) = math%yes
      obj%faceIA(jj + 1) = tsize
    END IF
  END DO faceLoop

  ! cell
  jj = ElemData_GetCell(obj=elemdata, islocal=math%yes)
  isok = foundCells(jj)

  ! if cell not found
  IF (.NOT. isok) THEN
    myorder = INT(obj%cellOrder(jj), KIND=INT8)
    tsize = ElemData_GetTotalCellDOF( &
            obj=elemdata, order=myorder, &
            baseContinuity=obj%baseContinuity, &
            baseInterpolation=obj%baseInterpolation)
    foundCells(jj) = math%yes
    obj%cellIA(jj + 1) = tsize
  END IF
END DO cellLoop

! Number entities in ID order, independently of cell traversal order.
! Each array ends with the first DOF of the following entity block.
obj%edgeIA(1) = obj%tNodes + 1
DO ii = 1, obj%tEdges
  obj%edgeIA(ii + 1) = obj%edgeIA(ii) + obj%edgeIA(ii + 1)
END DO

obj%faceIA(1) = obj%edgeIA(obj%tEdges + 1)
DO ii = 1, obj%tFaces
  obj%faceIA(ii + 1) = obj%faceIA(ii) + obj%faceIA(ii + 1)
END DO

obj%cellIA(1) = obj%faceIA(obj%tFaces + 1)
DO ii = 1, obj%tCells
  obj%cellIA(ii + 1) = obj%cellIA(ii) + obj%cellIA(ii + 1)
END DO

obj%tdof = obj%cellIA(obj%tCells + 1) - 1

! The DOF layout has changed, so the cached values are stale
obj%isMaxConSet = math%no
obj%isMaxQuadPointSet = math%no
obj%isSumElemTotalDOFSet = math%no

IF (ALLOCATED(foundEdges)) DEALLOCATE (foundEdges)
IF (ALLOCATED(foundFaces)) DEALLOCATE (foundFaces)
IF (ALLOCATED(foundCells)) DEALLOCATE (foundCells)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFedofForH1

!----------------------------------------------------------------------------
!                                                         SetFedofForH1Broken
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFedofForH1Broken
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFedofForH1Broken()"
#endif

! Internal variables
INTEGER(I4B) :: tsize, iel, ii, myorder, elemType
TYPE(ElemData_), POINTER :: elemdata
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Now we will store counts for edge, face, and cell in
!   obj%edgeIA, obj%faceIA, obj%cellIA from 2:n+1
!   before building the cumulative offsets.
!   Entities not incident to an active cell have zero interior DOFs.
obj%cellIA = 0
obj%edgeIA = 1
obj%faceIA = 1
! we do not have edge and face wise dof in case of broken spaces.
! That is why we have set edgeIA and faceIA to 1
! We also do not have vertexDOF too.
! All DOF are cellDOF

cellLoop: DO iel = 1, obj%tCells
  isok = obj%mesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE cellLoop
  elemdata => obj%mesh%GetElemDataPointer(globalElement=iel, &
                                          islocal=math%yes)

  myorder = INT(obj%cellOrder(iel), KIND=I4B)
  elemType = Elemdata_Name(elemdata)
  tsize = InterpolationUtilityGetTotalDOF( &
          elemType=elemType, order=myorder, &
          baseContinuity=obj%baseContinuity, &
          baseInterpolation=obj%baseInterpolation)

#ifdef DEBUG_VER
  CALL AssertError1(tsize .GT. math%zero_i, myName, &
                    "error in tsize returned from GetTotalDOF, tsize=" &
                    //ToString(tsize)//" elemType="//ToString(elemType))
#endif

  obj%cellIA(iel + 1) = tsize
END DO cellLoop

obj%cellIA(1) = obj%faceIA(obj%tFaces + 1)
DO ii = 1, obj%tCells
  obj%cellIA(ii + 1) = obj%cellIA(ii) + obj%cellIA(ii + 1)
END DO

obj%tdof = obj%cellIA(obj%tCells + 1) - 1

! The DOF layout has changed, so the cached values are stale
obj%isMaxConSet = math%no
obj%isMaxQuadPointSet = math%no
obj%isSumElemTotalDOFSet = math%no

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFedofForH1Broken

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

obj%isInit = math%no
obj%isLagrange = math%no
obj%isMaxConSet = math%no
obj%isMaxQuadPointSet = math%no
obj%isSumElemTotalDOFSet = math%no
obj%isTotalActiveCellsSet = math%no
obj%isBrokenSpace = math%no

obj%tdof = 0
obj%tNodes = 0
obj%tEdges = 0
obj%tFaces = 0
obj%tCells = 0
obj%maxCon = 0
obj%maxQuadPoint = 0
obj%sumElemTotalDOF = 0
obj%tActiveCells = 0

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

IF (obj%ownFePointers) THEN
  DO ii = 1, SIZE(obj%fe)
    abool = ASSOCIATED(obj%fe(ii)%ptr)
    IF (abool) THEN
      CALL obj%fe(ii)%ptr%DEALLOCATE()
      DEALLOCATE (obj%fe(ii)%ptr)
    END IF
  END DO
END IF

obj%ownFePointers = math%no

DO ii = 1, SIZE(obj%fe)
  obj%fe(ii)%ptr => NULL()
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

! Free the finite elements owned by obj, except those shared with obj2.
! obj keeps the ownership of a shared finite element.
isok = obj%ownFePointers
obj%ownFePointers = math%no

feLoop: DO ii = 1, SIZE(obj%fe)
  IF (.NOT. ASSOCIATED(obj%fe(ii)%ptr)) CYCLE feLoop

  IF (ASSOCIATED(obj%fe(ii)%ptr, obj2%fe(ii)%ptr)) THEN
    obj%ownFePointers = isok
  ELSE IF (isok) THEN
    CALL obj%fe(ii)%ptr%DEALLOCATE()
    DEALLOCATE (obj%fe(ii)%ptr)
  END IF
END DO feLoop

obj%isLagrange = obj2%isLagrange
obj%isInit = obj2%isInit
obj%isMaxConSet = obj2%isMaxConSet
obj%isMaxQuadPointSet = obj2%isMaxQuadPointSet
obj%isSumElemTotalDOFSet = obj2%isSumElemTotalDOFSet
obj%isBrokenSpace = obj2%isBrokenSpace
obj%isTotalActiveCellsSet = obj2%isTotalActiveCellsSet

obj%tdof = obj2%tdof
obj%tNodes = obj2%tNodes
obj%tEdges = obj2%tEdges
obj%tFaces = obj2%tFaces
obj%tCells = obj2%tCells
obj%maxCon = obj2%maxCon
obj%maxQuadPoint = obj2%maxQuadPoint
obj%sumElemTotalDOF = obj2%sumElemTotalDOF
obj%tActiveCells = obj2%tActiveCells

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
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
