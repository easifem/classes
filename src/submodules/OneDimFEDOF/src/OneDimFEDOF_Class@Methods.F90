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

SUBMODULE(OneDimFEDOF_Class) Methods
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypeMeshOpt
USE BaseType, ONLY: TypePolynomialOpt
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE InputUtility, ONLY: Input
USE IntVector_Method, ONLY: IntegerCopy => Copy
USE OneDimHierarchicalFE_Class, ONLY: OneDimHierarchicalFEPointer
USE OneDimLagrangeFE_Class, ONLY: OneDimLagrangeFEPointer
USE OneDimOrthogonalFE_Class, ONLY: OneDimOrthogonalFEPointer
USE ReallocateUtility, ONLY: Reallocate
USE StringUtility, ONLY: UpperCase
USE FEFactoryUtility, ONLY: OneDimFEFactory
USE CSRMatrix_Method, ONLY: CSRMatrixSetSparsity => SetSparsity

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "OneDimFEDOF_Class@Methods.F90"
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

CALL obj%Initiate2( &
  order=order0, mesh=mesh, baseContinuity=baseContinuity, &
  baseInterpolation=baseInterpolation, fetype=fetype, ipType=ipType, &
  basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, &
  quadratureType=quadratureType, quadratureOrder=quadratureOrder, &
  quadratureNips=quadratureNips, quadratureAlpha=quadratureAlpha, &
  quadratureBeta=quadratureBeta, quadratureLambda=quadratureLambda, &
  scaleForQuadOrder=scaleForQuadOrder)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = math%yes
obj%mesh => mesh
obj%tCells = obj%mesh%GetTotalElements()
obj%tNodes = obj%mesh%GetTotalVertexNodes()

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%mesh)
CALL AssertError1(isok, myName, "mesh is not associated")
#endif

obj%scaleForQuadOrder = Input(option=scaleForQuadOrder, default=math%two_i)

CALL obj%AllocateSizes()
CALL obj%SetCellOrder(order=order, islocal=islocal)

! make fe
obj%fe => OneDimFEFactory(baseInterpolation=baseInterpolation, &
                          baseContinuity=baseContinuity)

CALL obj%fe%Initiate( &
  baseContinuity=baseContinuity, baseInterpolation=baseInterpolation, &
  feType=feType, ipType=ipType, basisType=basisType, alpha=alpha, &
  beta=beta, lambda=lambda, quadratureType=quadratureType, &
  quadratureOrder=quadratureOrder, quadratureNips=quadratureNips, &
  quadratureAlpha=quadratureAlpha, quadratureBeta=quadratureBeta, &
  quadratureLambda=quadratureLambda)

! get few things from obj%fe
obj%baseInterpolation = obj%fe%GetBaseInterpolation()
obj%baseContinuity = obj%fe%GetBaseContinuity()
IF (obj%baseInterpolation .EQ. "LAGR") obj%isLagrange = math%yes

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

telems = mesh%GetTotalElements()
tsize = SIZE(order, 2)

#ifdef DEBUG_VER
ii = SIZE(order, 1)
isok = ii .EQ. 2
CALL AssertError1(isok, myName, &
      'number of rows of order array is not equal &
      &to 2 found: '//ToString(ii))

isok = tsize .EQ. telems
CALL AssertError1(isok, myName, &
           'number of cols of order array is not equal &
          &to number of elements')
#endif

ALLOCATE (order0(telems))

DO ii = 1, telems
  globalElement = order(1, ii)
  localElement = mesh%GetLocalElemNumber(globalElement=globalElement, &
                                         islocal=math%no)
  order0(localElement) = order(2, ii)
END DO

CALL obj%Initiate(mesh=mesh, baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, &
                  order=order0, ipType=ipType, &
                  basisType=basisType, alpha=alpha, beta=beta, &
                  lambda=lambda, islocal=math%yes, &
                  scaleForQuadOrder=scaleForQuadOrder)

DEALLOCATE (order0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = math%no
obj%tdof = 0
obj%maxCon = 0
obj%maxCellOrder = 0
obj%mesh => NULL()
IF (ALLOCATED(obj%cellOrder)) DEALLOCATE (obj%cellOrder)
IF (ALLOCATED(obj%cellIA)) DEALLOCATE (obj%cellIA)

IF (ASSOCIATED(obj%fe)) THEN
  CALL obj%fe%DEALLOCATE()
END IF
obj%fe => NULL()

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

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = obj2%isInit
obj%tdof = obj2%tdof
obj%maxCon = obj2%maxCon
obj%maxCellOrder = obj2%maxCellOrder
isok = ALLOCATED(obj2%cellOrder)
IF (isok) THEN
  CALL IntegerCopy(x=obj%cellOrder, y=obj2%cellOrder)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                               IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                              AllocateSizes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateSizes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateSizes()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Reallocate(obj%cellOrder, obj%tCells)
CALL Reallocate(obj%cellIA, obj%tCells + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateSizes

!----------------------------------------------------------------------------
!                                                               GetCaseName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCaseName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetCaseName()'
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%baseContinuity//obj%baseInterpolation

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCaseName

!----------------------------------------------------------------------------
!                                                               GetVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVertexDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVertexDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 1
ans(1) = obj%mesh%GetLocalNodeNumber(globalNode=globalNode, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVertexDOF

!----------------------------------------------------------------------------
!                                                           GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%mesh%GetTotalVertexNodes()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalVertexDOF

!----------------------------------------------------------------------------
!                                                                 GetCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCellDOF()"
#endif
INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

jj = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                 islocal=islocal)
tsize = 0
DO ii = obj%cellIA(jj), obj%cellIA(jj + 1) - 1
  tsize = tsize + 1
  ans(tsize) = ii
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCellDOF

!----------------------------------------------------------------------------
!                                                            GetTotalCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalCellDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalCellDOF()"
#endif
INTEGER(I4B) :: jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

jj = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                 islocal=islocal)
ans = obj%cellIA(jj + 1) - obj%cellIA(jj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDOF1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDOF1

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetTotalDOF2()'
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                 islocal=islocal)
ans = 2 + obj%cellIA(ii + 1) - obj%cellIA(ii)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDOF2

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetTotalDOF3()'
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (opt(1:1))
CASE ('v', 'V')
  ans = 2
CASE ('c', 'C')
  ans = obj%GetTotalCellDOF(globalElement=globalElement, &
                            islocal=islocal)

CASE DEFAULT
  ans = obj%GetTotalDOF(globalElement=globalElement, &
                        islocal=islocal)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTotalDOF3

!----------------------------------------------------------------------------
!                                                         GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetConnectivity()'
#endif

INTEGER(I4B) :: tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tdof = obj%GetTotalDOF(globalElement=globalElement, isLocal=isLocal)
ALLOCATE (ans(tdof))
CALL obj%GetConnectivity_(ans=ans, tsize=tdof, opt=opt, &
                          globalElement=globalElement, islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                           GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetConnectivity_()'
#endif

INTEGER(I4B) :: ii, jj, kk, localElement
INTEGER(I4B) :: temp(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

localElement = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                           islocal=islocal)

CALL obj%mesh%GetConnectivity_(globalElement=localElement, &
                               islocal=math%yes, opt=opt, &
                               tsize=jj, ans=temp)

jj = 1

SELECT CASE (opt)
  ! vertex degree of freedom
CASE ("v", "V")
  DO ii = 1, 2
    CALL obj%GetVertexDOF(globalNode=temp(ii), ans=ans(jj:), tsize=kk, &
                          islocal=math%no)
    jj = jj + kk
  END DO

  ! cell degree of freedom
CASE ("c", "C")
  CALL obj%GetCellDOF(globalElement=localElement, ans=ans(jj:), tsize=kk, &
                      islocal=math%yes)
  jj = jj + kk

CASE DEFAULT

  ! vertex degree of freedom
  DO ii = 1, 2
    CALL obj%GetVertexDOF(globalNode=temp(ii), ans=ans(jj:), tsize=kk, &
                          islocal=math%no)
    jj = jj + kk
  END DO

  ! cell degree of freedom
  CALL obj%GetCellDOF(globalElement=localElement, ans=ans(jj:), tsize=kk, &
                      islocal=math%yes)
  jj = jj + kk

END SELECT

tsize = jj - 1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetConnectivity_

!----------------------------------------------------------------------------
!                                                           GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshPointer

!----------------------------------------------------------------------------
!                                                       GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseInterpolation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseInterpolation()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%baseInterpolation

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                               GetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCellOrder()"
#endif
INTEGER(I4B) :: jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

jj = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                 islocal=islocal)
ans = obj%cellOrder(jj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCellOrder

!----------------------------------------------------------------------------
!                                                   GetMaxTotalConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalConnectivity()"
#endif

INTEGER(I4B) :: ii, tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isMaxConSet) THEN
  ans = obj%maxCon

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

obj%isMaxConSet = math%yes

ans = 0

DO ii = 1, obj%tCells
  tdof = obj%GetTotalDOF(globalElement=ii, isLocal=math%yes)
  ans = MAX(ans, tdof)
END DO

obj%maxCon = ans

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalConnectivity

!----------------------------------------------------------------------------
!                                                 GetMaxTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetMaxTotalQuadraturePoints()'
#endif
INTEGER(I4B) :: cellOrder, iel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isMaxQuadPointSet) THEN
  ans = obj%maxQuadPoint

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

obj%isMaxQuadPointSet = math%yes

DO iel = 1, obj%tCells
  cellOrder = obj%GetCellOrder(globalElement=iel, islocal=math%yes)
  cellOrder = cellOrder * obj%scaleForQuadOrder
  CALL obj%fe%SetQuadratureOrder(order=cellOrder)
  ans = obj%fe%GetTotalQuadraturePoints()
END DO

obj%maxQuadPoint = ans

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                                               GetFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%fe

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFEPointer

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInit: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL Display(obj%isLagrange, "isLagrange: ", unitno=unitno)
CALL Display(obj%isMaxConSet, "isMaxConSet: ", unitno=unitno)
CALL Display(obj%isMaxQuadPointSet, "isMaxQuadPointSet: ", unitno=unitno)
CALL Display(obj%tdof, "tdof: ", unitno=unitno)
CALL Display(obj%tCells, "tCells: ", unitno=unitno)
CALL Display(obj%tNodes, "tNodes: ", unitno=unitno)
CALL Display(obj%maxCon, "maxCon: ", unitno=unitno)
CALL Display(obj%maxQuadPoint, "maxQuadPoint: ", unitno=unitno)
CALL Display(obj%maxCellOrder, "maxCellOrder: ", unitno=unitno)
CALL Display(obj%scaleForQuadOrder, "scaleForQuadOrder: ", unitno=unitno)
CALL Display(obj%baseContinuity, "baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation, "baseInterpolation: ", unitno=unitno)

isok = ALLOCATED(obj%cellOrder)
CALL Display(isok, "cellOrder ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellOrder), "cellOrder size: ", &
                       unitno=unitno)

isok = ALLOCATED(obj%cellIA)
CALL Display(isok, "cellIA ALLOCATED: ", unitno=unitno)
IF (isok) CALL Display(SIZE(obj%cellIA), "cellIA size: ", unitno=unitno)

isok = ASSOCIATED(obj%fe)
CALL Display(isok, "fe ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display("    ", unitno=unitno)
  CALL obj%fe%Display(msg="fe: ", unitno=unitno)
  CALL Display("    ", unitno=unitno)
END IF

isok = ASSOCIATED(obj%mesh)
CALL Display(isok, "mesh ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display("    ", unitno=unitno)
  CALL obj%mesh%Display(msg="mesh: ", unitno=unitno)
  CALL Display("    ", unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                           DisplayCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_DisplayCellOrder()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%cellOrder)
CALL Display(isok, "cellOrder ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(SIZE(obj%cellOrder), "cellOrder size: ", &
               unitno=unitno)
  CALL Display(obj%cellOrder, "cellOrder: ", unitno=unitno, full=full)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_DisplayCellOrder

!----------------------------------------------------------------------------
!                                                               SetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCellOrder()"
#endif
INTEGER(I4B) :: tsize, ii, jj
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(order)
IF (tsize .EQ. 1) THEN
  obj%cellOrder(1:obj%tCells) = order(1)

ELSE

#ifdef DEBUG_VER
  isok = tsize .LE. obj%tCells
  CALL AssertError1(isok, myName, "Size of order array is out of bounds.")
#endif

  DO ii = 1, tsize
    isok = obj%mesh%IsElementPresent(globalElement=ii, islocal=islocal)
    IF (.NOT. isok) CYCLE
    jj = obj%mesh%GetLocalElemNumber(globalElement=ii, islocal=islocal)
    obj%cellOrder(jj) = order(ii)
  END DO

END IF

obj%maxCellOrder = MAXVAL(obj%cellOrder)
obj%cellIA = obj%tNodes

obj%cellIA(1) = obj%tNodes + 1
obj%tdof = obj%tNodes
DO ii = 1, obj%tCells
  isok = obj%mesh%IsElementPresent(globalElement=ii, islocal=islocal)
  IF (.NOT. isok) CYCLE
  jj = obj%mesh%GetLocalElemNumber(globalElement=ii, islocal=islocal)

  tsize = MAX(obj%cellOrder(jj) - 1, 0)
  obj%tdof = obj%tdof + tsize

  obj%cellIA(jj + 1) = obj%cellIA(jj) + tsize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCellOrder

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity1()"
#endif
INTEGER(I4B) :: tsize, ii, iel
INTEGER(I4B), ALLOCATABLE :: conn(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%mesh)
CALL AssertError1(isok, myName, 'obj%mesh NOT ASSOCIATED')

isok = obj%tCells .NE. math%zero_i
CALL AssertError1(isok, myName, "Empty mesh found, returning")
#endif

tsize = obj%GetMaxTotalConnectivity()
CALL Reallocate(conn, tsize)

DO iel = 1, obj%tCells
  CALL obj%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                            ans=conn, tsize=tsize, opt="A")
  DO ii = 1, tsize
    CALL CSRMatrixSetSparsity(obj=mat, row=conn(ii), col=conn(1:tsize))
  END DO
END DO

CALL CSRMatrixSetSparsity(mat)

IF (ALLOCATED(conn)) DEALLOCATE (conn)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                      SetFE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFE()"
#endif

INTEGER(I4B) :: iel, order

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel=obj%mesh%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)
order = obj%GetCellOrder(globalElement=globalElement, islocal=islocal)
CALL obj%fe%SetOrder(order=order)
order = order * obj%scaleForQuadOrder
CALL obj%fe%SetQuadratureOrder(order=order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
