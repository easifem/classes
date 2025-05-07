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

USE FPL_Method, ONLY: Set, GetValue, CheckEssentialParam

USE StringUtility, ONLY: UpperCase

USE AbstractFE_Class, ONLY: AbstractFEDeallocate

USE LagrangeFE_Class, ONLY: LagrangeFEPointer

USE HierarchicalFE_Class, ONLY: HierarchicalFEPointer

USE BaseType, ONLY: TypeInterpolationOpt, &
                    TypePolynomialOpt

USE ReferenceElement_Method, ONLY: eleminfo => ReferenceElementInfo, &
                                   GetElementIndex

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CHARACTER(*), PARAMETER :: keys = "baseContinuity/baseInterpolation/orderFile/&
&ipType/basisType/alpha/beta/lambda/"

CALL CheckEssentialParam(obj=param, keys=keys, prefix=myprefix, &
                         myName=myName, modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

CHARACTER(6) :: casename

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%baseInterpolation = UpperCase(baseInterpolation(1:4))

IF (obj%baseInterpolation == "LAGR") THEN
  obj%isLagrange = .TRUE.
ELSE
  obj%isLagrange = .FALSE.
END IF

obj%mesh => mesh
obj%baseContinuity = UpperCase(baseContinuity(1:2))

CALL FEDOF_Initiate_Before(obj=obj)

! Set order
casename = obj%GetCaseName()
SELECT CASE (casename)
CASE ("H1LAGR")
  CALL SetOrderH1Lagrange(obj=obj)

CASE ("H1HIER")
  CALL SetOrderH1Hierarchical1(obj=obj, order=order)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
 '[INTERNAL ERROR] :: No case found for baseContinuity and baseInterpolation')
  RETURN
END SELECT

CALL FEDOF_Initiate_After(obj=obj)

SELECT CASE (obj%baseInterpolation)

CASE ("LAGR")
  CALL handle_lagrange_fe(obj=obj, iptype=ipType, basistype=basisType, &
                          alpha=alpha, beta=beta, lambda=lambda)

CASE ("HIER", "HEIR")
  CALL handle_hierarchical_fe(obj=obj)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for baseInterpolation')
  RETURN

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                         SetOrderH1Lagrange
!----------------------------------------------------------------------------

SUBROUTINE SetOrderH1Lagrange(obj)
  CLASS(FEDOF_), INTENT(INOUT) :: obj

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "SetOrderH1Lagrange()"
  INTEGER(I4B) :: ii, order, tcell
  INTEGER(INT8) :: aint8

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  tcell = obj%mesh%GetTotalCells()
  CALL Reallocate(obj%cellOrder, tcell)

  DO ii = 1, tcell
    order = obj%mesh%GetOrder(globalElement=ii, islocal=.TRUE.)
    aint8 = INT(order, kind=INT8)
    obj%cellOrder(ii) = aint8
  END DO
  obj%maxCellOrder = MAXVAL(obj%cellOrder(1:tcell))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetOrderH1Lagrange

!----------------------------------------------------------------------------
!                                                 SetOrderH1Hierarchical1
!----------------------------------------------------------------------------

SUBROUTINE SetOrderH1Hierarchical1(obj, order)
  CLASS(FEDOF_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: order

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "SetOrderH1Hierarchical1()"
  INTEGER(INT8) :: aint8
  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  aint8 = INT(order, kind=INT8)
  DO CONCURRENT(ii=1:obj%tCells)
    obj%cellOrder(ii) = aint8
  END DO
  obj%maxcellOrder = aint8

  DO CONCURRENT(ii=1:obj%tFaces)
    obj%faceOrder(ii) = aint8
  END DO
  obj%maxFaceOrder = aint8

  DO CONCURRENT(ii=1:obj%tEdges)
    obj%edgeOrder(ii) = aint8
  END DO
  obj%maxEdgeOrder = aint8

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetOrderH1Hierarchical1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%baseInterpolation = UpperCase(baseInterpolation(1:4))

IF (obj%baseInterpolation == "LAGR") THEN
  obj%isLagrange = .TRUE.
ELSE
  obj%isLagrange = .FALSE.
END IF

obj%mesh => mesh
obj%baseContinuity = UpperCase(baseContinuity(1:2))

CALL FEDOF_Initiate_Before(obj=obj)

CALL obj%SetCellOrder(order=order)
CALL obj%SetFaceOrder()
CALL obj%SetEdgeOrder()

CALL FEDOF_Initiate_After(obj=obj)

SELECT CASE (obj%baseInterpolation)

CASE ("LAGR")
  CALL handle_lagrange_fe(obj=obj, iptype=ipType, basistype=basisType, &
                          alpha=alpha, beta=beta, lambda=lambda)

CASE ("HIER", "HEIR")
  CALL handle_hierarchical_fe(obj=obj)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for baseInterpolation')
  RETURN

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE handle_lagrange_fe(obj, iptype, basistype, alpha, beta, lambda)
  CLASS(FEDOF_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: iptype
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basistype(3)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(3)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(3)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(3)

  CHARACTER(*), PARAMETER :: myName = "handle_lagrange_fe()"
  LOGICAL(LGT) :: isok

  INTEGER(I4B) :: ii, elemType, nsd, jj
  INTEGER(I4B) :: totalTopo, topoList(8)

  isok = PRESENT(iptype)
  CALL AssertError1(isok, myname, "ipType should be present")

  topoList = obj%mesh%GetElemTopology()
  totalTopo = obj%mesh%GetTotalTopology()
  nsd = obj%mesh%GetNSD()

  DO ii = 1, totalTopo
    elemType = topoList(ii)
    jj = GetElementIndex(elemType)

    obj%fe(jj)%ptr => LagrangeFEPointer(elemType=elemType, nsd=nsd, &
                           baseContinuity=obj%baseContinuity, ipType=ipType, &
                   basisType=basisType, alpha=alpha, beta=beta, lambda=lambda)

  END DO

END SUBROUTINE handle_lagrange_fe

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE handle_hierarchical_fe(obj)
  CLASS(FEDOF_), INTENT(INOUT) :: obj

  INTEGER(I4B) :: ii, elemType, nsd, jj
  INTEGER(I4B) :: totalTopo, topoList(8)

  topoList = obj%mesh%GetElemTopology()
  totalTopo = obj%mesh%GetTotalTopology()
  nsd = obj%mesh%GetNSD()

  DO ii = 1, totalTopo
    elemType = topoList(ii)
    jj = GetElementIndex(elemType)
    obj%fe(jj)%ptr => HierarchicalFEPointer(elemType=elemType, nsd=nsd, &
                                            baseContinuity=obj%baseContinuity)
  END DO

END SUBROUTINE handle_hierarchical_fe

!----------------------------------------------------------------------------
!                                                             SetFEDOFParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetFEDOFParam
INTEGER(I4B) :: AINT(3)
INTEGER(I4B), PARAMETER :: ipType0 = TypeInterpolationOpt%Equidistance
INTEGER(I4B), PARAMETER :: basisType0(3) = TypePolynomialOpt%Monomial
REAL(DFP) :: areal(3)
REAL(DFP), PARAMETER :: three_zero(3) = [0.0, 0.0, 0.0]
REAL(DFP), PARAMETER :: three_half(3) = [0.5, 0.5, 0.5]

CALL Set(obj=param, prefix=myprefix, key="baseContinuity", &
         VALUE=baseContinuity, dataType=baseContinuity)

CALL Set(obj=param, prefix=myprefix, key="baseInterpolation", &
         VALUE=baseInterpolation, dataType=baseInterpolation)

CALL Set(obj=param, prefix=myprefix, key="orderFile", &
         VALUE=orderFile, dataType=orderFile)

CALL Set(obj=param, prefix=myprefix, key="ipType", &
         VALUE=Input(option=ipType, default=ipType0), &
         dataType=ipType0)

CALL make_a_int(a=basistype, a0=basistype0)
CALL set(obj=param, prefix=myprefix, key="basistype", &
         VALUE=aint, datatype=aint)

CALL make_a_real(a=alpha, a0=three_zero)
CALL set(obj=param, prefix=myprefix, key="alpha", &
         VALUE=areal, datatype=areal)

CALL make_a_real(a=beta, a0=three_zero)
CALL set(obj=param, prefix=myprefix, key="beta", &
         VALUE=areal, datatype=areal)

CALL make_a_real(a=lambda, a0=three_half)
CALL set(obj=param, prefix=myprefix, key="lambda", &
         VALUE=areal, datatype=areal)

CONTAINS
SUBROUTINE make_a_int(a, a0)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: a(:)
  INTEGER(I4B), INTENT(IN) :: a0(:)
  LOGICAL(LGT) :: isok

  isok = PRESENT(a)
  IF (isok) THEN
    SELECT CASE (SIZE(a))
    CASE (1)
      aint = a(1)
    CASE (2)
      AINT(1:2) = a(1:2)
    CASE (3)
      aint = a
    CASE DEFAULT
      aint = a(1:3)
    END SELECT
  ELSE
    aint = a0
  END IF

END SUBROUTINE make_a_int

SUBROUTINE make_a_real(a, a0)
  REAL(DFP), OPTIONAL, INTENT(IN) :: a(:)
  REAL(DFP), INTENT(IN) :: a0(:)
  LOGICAL(LGT) :: isok

  isok = PRESENT(a)
  IF (isok) THEN
    SELECT CASE (SIZE(a))
    CASE (1)
      areal = a(1)
    CASE (2)
      areal(1:2) = a(1:2)
    CASE (3)
      areal = a
    CASE DEFAULT
      areal = a(1:3)
    END SELECT
  ELSE
    areal = a0
  END IF

END SUBROUTINE

END PROCEDURE SetFEDOFParam

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
                  baseInterpolation=baseInterpolation, order=order0, &
                  ipType=ipType, basisType=basisType, alpha=alpha, &
                  beta=beta, lambda=lambda)

DEALLOCATE (order0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                     FEDOF_Initiate_Help
!----------------------------------------------------------------------------

SUBROUTINE FEDOF_Initiate_Before(obj)
  CLASS(FEDOF_), INTENT(INOUT) :: obj

  !! internal variables
  INTEGER(I4B) :: ent(4)
  LOGICAL(LGT) :: isLagrange

  isLagrange = obj%isLagrange

  !INFO: It returns total number of nodes, edges, faces, and cells in the mesh
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

SUBROUTINE FEDOF_Initiate_After(obj)
  CLASS(FEDOF_), INTENT(INOUT) :: obj

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

#ifdef DEBUG_VER
    isok = ASSOCIATED(elemdata)
    CALL AssertError1(isok, myName, 'elemdata is not allocated')
#endif

    ent = ElemData_GetTotalEntities(elemdata)

    ! edges
    DO ii = 1, ent(2)
      jj = ElemData_GetEdge(elemdata, ii)
      isok = foundEdges(jj)

      IF (.NOT. isok) THEN
        myorder = INT(obj%edgeOrder(jj), kind=INT8)
        tsize = ElemData_GetTotalEdgeDOF(obj=elemdata, ii=ii, order=myorder, &
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

      isok = foundFaces(jj)

      IF (.NOT. isok) THEN

        myorder = INT(obj%faceOrder(jj), kind=INT8)

        tsize = ElemData_GetTotalFaceDOF(obj=elemdata, ii=ii, order=myorder, &
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
    isok = foundCells(jj)

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

  obj%maxTotalConnectivity = obj%GetMaxTotalConnectivity()

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
LOGICAL(LGT) :: abool
INTEGER(I4B) :: ii

obj%isLagrange = .FALSE.
obj%tdof = 0
obj%tNodes = 0
obj%tEdges = 0
obj%tFaces = 0
obj%tCells = 0
obj%maxTotalConnectivity = 0

obj%baseContinuity = "H1"
obj%baseInterpolation = "LAGR"
obj%maxCellOrder = 0_INT8
obj%maxFaceOrder = 0_INT8
obj%maxEdgeOrder = 0_INT8

obj%mesh => NULL()
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

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

obj%isLagrange = obj2%isLagrange
obj%tdof = obj2%tdof
obj%tNodes = obj2%tNodes
obj%tEdges = obj2%tEdges
obj%tFaces = obj2%tFaces
obj%tCells = obj2%tCells
obj%baseContinuity = obj2%baseContinuity
obj%baseInterpolation = obj2%baseInterpolation
obj%maxCellOrder = obj2%maxCellOrder
obj%maxFaceOrder = obj2%maxFaceOrder
obj%maxEdgeOrder = obj2%maxEdgeOrder

obj%mesh => obj2%mesh
IF (ALLOCATED(obj2%cellOrder)) obj%cellOrder = obj2%cellOrder
IF (ALLOCATED(obj2%faceOrder)) obj%faceOrder = obj2%faceOrder
IF (ALLOCATED(obj2%edgeOrder)) obj%edgeOrder = obj2%edgeOrder
IF (ALLOCATED(obj2%edgeIA)) obj%edgeIA = obj2%edgeIA
IF (ALLOCATED(obj2%faceIA)) obj%faceIA = obj2%faceIA
IF (ALLOCATED(obj2%cellIA)) obj%cellIA = obj2%cellIA

DO ii = 1, SIZE(obj2%fe)
  isok = ASSOCIATED(obj2%fe(ii)%ptr)
  IF (isok) THEN
    obj%fe(ii)%ptr => obj2%fe(ii)%ptr
  END IF
END DO

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
