SUBMODULE(TDGAlgoParam) Methods

USE Lapack_Method, ONLY: GetInvMat

USE TomlUtility, ONLY: GetValue, GetValue_
USE StringUtility, ONLY: UpperCase
USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToInteger, &
                                    BaseType_ToInteger, &
                                    BaseType_ToChar, &
                                    BaseInterpolation_ToChar
USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_, &
                                    InterpolationPoint_Line_, &
                                    LagrangeGradientEvalAll_Line_
USE ReallocateUtility, ONLY: Reallocate
USE ProductUtility, ONLY: OuterProd_, OTimesTilda
USE BaseType, ONLY: elem => TypeElemNameOpt
USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate, &
                                  Quad_Size => Size, &
                                  Quad_Display => Display
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                OrthogonalElemShapeData
USE SwapUtility, ONLY: SWAP
USE LagrangePolynomialUtility, ONLY: InterpolationPoint_
USE InputUtility

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

CALL Display(msg, unitno=unitno)

CALL Display(obj%timeDomain, "timeDomain: ", unitno=unitno)

CALL Display(obj%totalTimeElements, "totalTimeElements: ", unitno=unitno)
CALL Display(obj%totalTimeNodes, "totalTimeNodes: ", unitno=unitno)

isok = ALLOCATED(obj%elemLength)
CALL Display(isok, "timeElemLength: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%elemLength, "timeElemLength: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%order)
CALL Display(isok, "order: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%order, "order: ", unitno=unitno)
END IF

CALL Display(obj%maxOrder, "maxOrder: ", unitno=unitno)

CALL Display(obj%baseContinuity, "baseContinuity: ", unitno=unitno)

CALL Display(obj%baseInterpolation, "baseInterpolation: ", unitno=unitno)

CALL Display(obj%baseType, "baseType: ", unitno=unitno)

CALL Display(obj%ipType, "ipType: ", unitno=unitno)

CALL Display(obj%quadType, "quadType: ", unitno=unitno)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"

REAL(DFP), ALLOCATABLE :: temprealvec(:)
INTEGER(I4B), ALLOCATABLE :: tempintvec(:)
INTEGER(I4B) :: origin, stat, tsize
LOGICAL(LGT) :: isok
TYPE(String) :: astr

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

IF (debug) CALL Display(myName//" totalTimeNodes")

CALL GetValue(table=table, key="totalTimeNodes", &
              VALUE=obj%totalTimeNodes, &
              default_value=0_I4B, origin=origin, stat=stat, isfound=isok)

IF (debug) CALL Display(myName//" totalTimeElements")

CALL GetValue(table=table, key="totalTimeElements", &
              VALUE=obj%totalTimeElements, &
              default_value=0_I4B, origin=origin, stat=stat, isfound=isok)

IF (debug) CALL Display(myName//" timeDomain")

CALL GetValue_(table=table, key="timeDomain", tsize=tsize, &
               VALUE=obj%timeDomain, origin=origin, stat=stat, isfound=isok)
isok = tsize .EQ. 2
CALL AssertError1(isok, myname, "timeDomain should have 2 values")

IF (debug) CALL Display(myName//" timeElemLength")

CALL GetValue(table=table, key="timeElemLength", VALUE=temprealvec, &
              origin=origin, stat=stat, isfound=isok)

CALL AssertError1(isok, myname, "timeElemLength not found")

CALL Reallocate(obj%elemLength, obj%totalTimeElements)

isok = SIZE(temprealvec) .EQ. 1
IF (isok) THEN
  obj%elemLength = temprealvec(1)
ELSE
  isok = SIZE(temprealvec) .EQ. obj%totalTimeElements
  CALL AssertError1(isok, myname, "elemLength should have "// &
                    "totalTimeElements values")
  obj%elemLength(:) = &
    temprealvec(1:obj%totalTimeElements)
END IF

IF (debug) CALL Display(myName//" timeOrder")

CALL ElementDataImportFromToml(table, "timeOrder", tempintvec, &
                               obj%totalTimeElements, isok)
CALL AssertError1(isok, myname, "timeOrder not found")

IF (debug) CALL Display(obj%totalTimeElements, &
                        myname//" totalTimeElements: ")

CALL Reallocate(obj%order, obj%totalTimeElements)

isok = SIZE(tempintvec) .EQ. 1
IF (isok) THEN
  obj%order(:) = tempintvec(1)
ELSE
  isok = SIZE(tempintvec) .EQ. obj%totalTimeElements
  CALL AssertError1(isok, myname, "timeOrder should have "// &
                    "totalTimeElements values")
  obj%order(:) = tempintvec(1:obj%totalTimeElements)
END IF

obj%maxOrder = MAXVAL(obj%order)

IF (debug) CALL Display(myName//" baseInterpolationForTime")

CALL GetValue(table=table, key="baseInterpolationForTime", &
              VALUE=astr, default_value=default_baseInterpolation, &
              origin=origin, stat=stat, isfound=isok)

obj%baseInterpolation = UpperCase(astr%slice(1, 4))

IF (debug) CALL Display(myName//" baseTypeForTime")

CALL GetValue(table=table, key="baseTypeForTime", &
              VALUE=astr, default_value=default_baseType, &
              origin=origin, stat=stat, isfound=isok)
obj%baseType = BaseType_ToInteger(astr%chars())

IF (debug) CALL Display(myName//" ipTypeForTime")

CALL GetValue(table=table, key="ipTypeForTime", &
              VALUE=astr, default_value=default_ipType, &
              origin=origin, stat=stat, isfound=isok)
obj%ipType = BaseInterpolation_ToInteger(astr%chars())

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
INTEGER(I4B) :: nrow, ncol, nnt
REAL(DFP) :: tij(1, 2)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START]')

tij(1, 1) = tij(1, 2)
tij(1, 2) = tij(1, 1) + obj%elemLength(timeElemNum)

CALL obj%SetQuad(timeElemNum)
CALL obj%SetElemsd(timeElemNum, tij)

CALL obj%GetMt(ans=obj%mt, nrow=nrow, ncol=ncol)
obj%nnt = nrow

CALL obj%GetMtPlus(ans=obj%mtplus, nrow=nrow, ncol=ncol)

CALL obj%GetCt(ans=obj%ct, nrow=nrow, ncol=ncol)

CALL obj%GetWt(ans=obj%ct, nrow=nrow, ncol=ncol)

CALL obj%GetAt()

CALL obj%GetBt()

CALL obj%GetKt_Tilda(ans=obj%kt_tilda, nrow=nrow, ncol=ncol)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END]')

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-26
! summary:  Set tij

MODULE PROCEDURE obj_SetTij
CHARACTER(*), PARAMETER :: myName = "objj_SetTij()"
INTEGER(I4B) :: order, nrow, ncol
REAL(DFP) :: tij_end(1, 2)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

order = obj%order(timeElemNum)
tij_end(1, 1) = obj%currentTime
tij_end(1, 2) = tij_end(1, 1) + obj%elemLength(timeElemNum)

CALL InterpolationPoint_Line_(order=order, ipType=obj%ipType, &
                              ans=obj%tij, nrow=nrow, ncol=ncol, layout="V", &
                              xij=tij_end)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_SetTij

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuad
CHARACTER(*), PARAMETER :: myName = "obj_SetQuad()"
INTEGER(I4B) :: order, integralOrder

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

order = obj%order(timeElemNum)
integralOrder = 2 * order

CALL QuadPoint_Initiate(obj=obj%quad, elemType=elem%line, &
                        domainName="B", order=integralOrder, &
                        quadratureType=obj%quadType)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_SetQuad

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElemsd
CHARACTER(*), PARAMETER :: myName = "obj_SetElemsd()"
INTEGER(I4B) :: nips, nns, cellOrder(1), order, cellOrient(1), nrow, ncol, &
                ii, jj
REAL(DFP) :: refElemCoord(1, 2), temp(2, MAX_ORDER_TIME + 1), &
             ipxij(1, MAX_ORDER_TIME + 1), tempgrad(2, max_order_time + 1, 1)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

order = obj%order(timeElemNum)
nips = Quad_Size(obj%quad, 2)
! nns = obj%totalDOFTime(timeElemNum)
nns = order + 1
refElemCoord(1, 1) = minus_one
refElemCoord(1, 2) = one
cellOrient(1) = 1

CALL Elemsd_Allocate(obj=obj%elemsd, nsd=1_I4B, xidim=1_I4B, &
                     nns=nns, nips=nips)

CALL LagrangeElemShapeData(obj=obj%linElemsd, &
                           quad=obj%quad, &
                           nsd=obj%elemsd%nsd, &
                           xidim=obj%elemsd%xidim, &
                           elemtype=elem%line, &
                           refelemCoord=refelemCoord, &
                           domainName="B", &
                           order=1_I4B)

SELECT CASE (obj%baseInterpolation)
CASE ("LAGR")

  CALL LagrangeElemShapeData(obj=obj%elemsd, &
                             quad=obj%quad, &
                             nsd=obj%elemsd%nsd, &
                             xidim=obj%elemsd%xidim, &
                             elemtype=elem%line, &
                             refelemCoord=refelemCoord, &
                             domainName="B", &
                             order=order, &
                             ipType=obj%ipType, &
                             basisType=obj%baseType)

  obj%timeShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%timeShapeFuncBndy(1, 1) = 1.0_DFP

  obj%timeShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%timeShapeFuncBndy(2, 2) = 1.0_DFP

  CALL InterpolationPoint_Line_(order=order, &
                                ipType=obj%ipType, &
                                ans=ipxij, &
                                nrow=nrow, ncol=ncol, &
                                layout="V")

  CALL LagrangeGradientEvalAll_Line_(order=order, x=refElemCoord, &
                                     xij=ipxij(1:nrow, 1:ncol), &
                                     ans=tempgrad, &
                                     dim1=nrow, dim2=ncol, dim3=ii)

  obj%timeShapeFuncGradBndy(1:nns, 1) = tempgrad(1, 1:ncol, 1)
  obj%timeShapeFuncGradBndy(1:nns, 2) = tempgrad(2, 1:ncol, 1)

CASE ("HIER", "HEIR")

  cellOrder = order
  CALL HierarchicalElemShapeData(obj=obj%elemsd, &
                                 quad=obj%quad, &
                                 nsd=obj%elemsd%nsd, &
                                 xidim=obj%elemsd%xidim, &
                                 elemtype=elem%line, &
                                 refelemCoord=refelemCoord, &
                                 domainName="B", &
                                 cellOrder=cellOrder, &
                                 cellOrient=cellOrient)

  obj%timeShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%timeShapeFuncBndy(1, 1) = 1.0_DFP

  obj%timeShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%timeShapeFuncBndy(2, 2) = 1.0_DFP

  obj%timeShapeFuncGradBndy(1:nns, 1) = zero
  obj%timeShapeFuncGradBndy(1, 1) = -half
  obj%timeShapeFuncGradBndy(2, 1) = half

  obj%timeShapeFuncGradBndy(1:nns, 2) = obj%timeShapeFuncGradBndy(1:nns, 1)

CASE ("ORTH")

  cellOrder = order
  CALL OrthogonalElemShapeData(obj=obj%elemsd, &
                               quad=obj%quad, &
                               nsd=obj%elemsd%nsd, &
                               xidim=obj%elemsd%xidim, &
                               elemtype=elem%line, &
                               refelemCoord=refelemCoord, &
                               domainName="B", order=order, &
                               basisType=poly%legendre)

  CALL OrthogonalBasis_Line_(order=order, xij=refElemCoord, &
                             refLine="B", basisType=poly%legendre, ans=temp, &
                             nrow=nrow, ncol=ncol)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    obj%timeShapeFuncBndy(jj, ii) = temp(ii, jj)
  END DO

  !TODO: implement gradient
  obj%timeShapeFuncGradBndy = zero

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'no case found for baseInterpolation')

END SELECT

CALL Elemsd_Set(obj=obj%elemsd, val=tij, &
                N=obj%linElemsd%N(1:2, 1:nips), &
                dNdXi=obj%linElemsd%dNdXi(1:2, 1:1, 1:nips))

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_SetElemsd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCt()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

nrow = obj%elemsd%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsd%nips
  scale = obj%elemsd%ws(ips) * obj%elemsd%thickness(ips)
  CALL OuterProd_(a=obj%elemsd%N(1:nrow, ips), &
                  b=obj%elemsd%dNdXi(1:ncol, 1, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_GetCt

!----------------------------------------------------------------------------
!                                                                      GetMt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMt()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

nrow = obj%elemsd%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsd%nips
  scale = obj%elemsd%ws(ips) * obj%elemsd%thickness(ips)
  CALL OuterProd_(a=obj%elemsd%N(1:nrow, ips), &
                  b=obj%elemsd%N(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')
END PROCEDURE obj_GetMt

!----------------------------------------------------------------------------
!                                                                  GetMtPlus
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMtPlus
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMtPlus()"
#endif
INTEGER(I4B) :: ii, jj
REAL(DFP) :: scale

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

nrow = obj%elemsd%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

scale = 1.0_DFP
CALL OuterProd_(a=obj%timeShapeFuncBndy(1:nrow, 1), &
                b=obj%timeShapeFuncBndy(1:ncol, 1), &
                ans=ans, nrow=ii, ncol=jj, anscoeff=0.0_DFP, scale=scale)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')
END PROCEDURE obj_GetMtPlus

!----------------------------------------------------------------------------
!                                                                GetKt_Tilda
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetKt_Tilda
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetKt_Tilda()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

nrow = obj%elemsd%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsd%nips

  scale = obj%elemsd%ws(ips) * obj%elemsd%thickness(ips)

  CALL OuterProd_(a=obj%elemsd%N(1:nrow, ips), &
                  b=obj%bt(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)

END DO

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')
END PROCEDURE obj_GetKt_Tilda

!----------------------------------------------------------------------------
!                                                                    GetWt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetWt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetWt()"
#endif

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

nrow = obj%elemsd%nns
ncol = nrow

obj%wt(1:nrow, 1:ncol) = obj%ct(1:nrow, 1:ncol) + obj%mtplus(1:nrow, 1:ncol)
CALL GetInvMat(obj%wt(1:nrow, 1:ncol))
obj%wmt(1:nrow, 1:ncol) = MATMUL(obj%wt(1:nrow, 1:ncol), obj%mt(1:nrow, 1:ncol))
obj%wmt(1:nrow, 1:ncol) = TRANSPOSE(obj%wmt(1:nrow, 1:ncol))

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')
END PROCEDURE obj_GetWt

!----------------------------------------------------------------------------
!                                                                    GetAt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAt()"
#endif

INTEGER(I4B) :: nrow, ncol, ii, tsize
REAL(DFP) :: temp(MAX_ORDER_TIME), areal

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

nrow = obj%elemsd%nns
ncol = nrow

tsize = obj%elemsd%nips

temp(1:nrow) = MATMUL(obj%wt(1:nrow, 1:ncol), obj%timeShapeFuncBndy(1:ncol, 1))

obj%tat(1:nrow) = 0.0_DFP

obj%at_right = DOT_PRODUCT(obj%timeShapeFuncBndy(1:nrow, 2), temp(1:nrow))

DO ii = 1, tsize
  obj%at(ii) = DOT_PRODUCT(obj%elemsd%N(1:nrow, ii), temp(1:nrow))
  areal = obj%at(ii) * obj%elemsd%ws(ii)
  obj%tat(1:nrow) = obj%tat(1:nrow) + obj%elemsd%N(1:nrow, ii) * areal
END DO

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_GetAt

!----------------------------------------------------------------------------
!                                                                    GetBt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBt()"
#endif

INTEGER(I4B) :: nrow, ncol

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

nrow = obj%elemsd%nns
ncol = obj%elemsd%nips
obj%bt(1:nrow, 1:ncol) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                                obj%elemsd%N(1:nrow, 1:ncol))

obj%bt(1:nrow, 1:ncol) = obj%bt(1:nrow, 1:ncol) * half

obj%bt_right(1:nrow) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                              obj%timeShapeFuncBndy(1:nrow, 2))

obj%bt_right(1:nrow) = obj%bt_right(1:nrow) * half

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_GetBt

!----------------------------------------------------------------------------
!                                                                    GetBt
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetBt_vst
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_GetBt()"
! #endif
! INTEGER(I4B) :: order, integralOrder, ii, jj, kk, &
!                 nipt, nipt_int, nnt, nrow, ncol
! REAL(DFP) :: refCoord(1, 2), ja, scale
! REAL(DFP), ALLOCATABLE :: int_points(:, :)
!
! IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
!
! order = 1_I4B
! ! order = obj%timeOrder(1)
! integralOrder = 2 * order
! refCoord(1, 1) = minus_one
! refCoord(1, 2) = one
!
! CALL QuadPoint_Initiate(obj=obj%intQuad, elemType=elem%line, &
!                         domainName="B", order=integralOrder, &
!                         quadratureType=GaussLegendre)
! CALL LagrangeElemShapeData(obj=obj%linElemsd, &
!                            quad=obj%intQuad, &
!                            nsd=obj%elemsd%nsd, &
!                            xidim=obj%elemsd%xidim, &
!                            elemtype=elem%line, &
!                            refelemCoord=refCoord, &
!                            domainName="B", &
!                            order=1_I4B)
!
! nipt = obj%elemsdfortime%nips
! int_points = obj%intQuad%points
! nipt_int = SIZE(int_points, 2)
! nnt = obj%elemsd%nns
!
! obj%bt = zero
!
! DO ii = 1, nipt
!   refCoord(1, 2) = obj%quad%points(1, ii)
!   CALL Elemsd_Set(obj=obj%linElemsd, val=refCoord, &
!                   N=obj%linElemsd%N, dNdXi=obj%linElemsd%dNdXi)
!   int_points(1, :) = obj%linElemsd%coord(1, :)
!   ja = obj%linElemsd%jacobian(1, 1, 1)
!   CALL QuadPoint_Initiate(obj=obj%intQuad, &
!                           points=int_points)
!   refCoord(1, 2) = one
!   CALL LagrangeElemShapeData(obj=obj%intElemsd, &
!                              quad=obj%intQuad, &
!                              nsd=obj%elemsd%nsd, &
!                              xidim=obj%elemsd%xidim, &
!                              elemtype=elem%line, &
!                              refelemCoord=refCoord, &
!                              domainName="B", &
!                              order=obj%timeOrder(1))
!
!   DO jj = 1, nnt
!     DO kk = 1, nipt_int
!       scale = obj%intElemsd%ws(kk) * half * ja
!       obj%bt(jj, ii) = obj%bt(jj, ii) + &
!                        scale * int_points(2, kk) * &
!                        obj%intElemsd%N(jj, kk)
!     END DO
!   END DO
! END DO
!
! nrow = obj%elemsd%nns
! ncol = obj%elemsd%nips
!
! obj%bt_right(1:nrow) = zero
!
! IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
!
! END PROCEDURE obj_GetBt_vst

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
