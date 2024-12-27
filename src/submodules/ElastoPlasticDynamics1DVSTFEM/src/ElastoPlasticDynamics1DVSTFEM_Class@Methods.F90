SUBMODULE(ElastoPlasticDynamics1DVSTFEM_Class) Methods
USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_stat, toml_array, &
                 toml_len => len

USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

USE Display_Method, ONLY: ToString, Display

USE GlobalData, ONLY: stdout, &
                      CHAR_LF, &
                      DOF_FMT, &
                      NONE, &
                      LIS_GMRES, &
                      CHAR_SLASH, &
                      MATRIX_COLUMN, &
                      GaussLegendreLobatto, &
                      GaussLobatto, &
                      GaussChebyshevLobatto, &
                      GaussUltrasphericalLobatto, &
                      GaussJacobiLobatto, &
                      GaussLegendre

USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToInteger, &
                                    BaseType_ToInteger, &
                                    BaseType_ToChar, &
                                    BaseInterpolation_ToChar

USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_

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

USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate, &
                            CSRMatrix_Add => Add, &
                            CSRMatrix_GetSubMatrix => GetSubMatrix, &
                            CSRMatrix_Display => Display, &
                            CSRMatrix_Size => Size, &
                            CSRMatrix_SetSparsity => SetSparsity, &
                            CSRMatrix_ApplyDBC => ApplyDBC, &
                            CSRMatrix_Set => Set, &
                            CSRMatrix_Matvec => Matvec, &
                            CSRMatrix_LinSolve => CSRMatrix_GMRES, &
                            CSRMatrixLinSolveInitiate

USE DOF_Method, ONLY: DOF_Initiate => Initiate, &
                      DOF_SIZE => Size, &
                      DOF_GetIndex_ => GetIndex_, &
                      DOF_GetNodeLoc => GetNodeLoc

USE RealVector_Method, ONLY: RealVector_Initiate => Initiate, &
                             RealVector_Add => Add, &
                             RealVector_GetValue_ => GetValue_, &
                             RealVector_Set => Set, &
                             RealVector_Display => Display, &
                             RealVector_Scale => SCAL, &
                             RealVector_Size => Size, &
                             RealVector_Norm => Norm2

USE RealMatrix_Method, ONLY: RealMatrix_Initiate => Initiate, &
                             RealMatrix_Display => Display, &
                             RealMatrix_Set => Set, &
                             RealMatrix_Get => Get, &
                             RealMatrix_Add => Add, &
                             RealMatrix_MatMul => MatMul

USE LagrangePolynomialUtility, ONLY: InterpolationPoint_
USE InputUtility
USE ArangeUtility, ONLY: arange

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                              -                     obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

CALL Abstract1DSTDeallocate(obj)

IF (ALLOCATED(obj%stress)) DEALLOCATE (obj%stress)
IF (ALLOCATED(obj%tstrain)) DEALLOCATE (obj%tstrain)
IF (ALLOCATED(obj%pstrain)) DEALLOCATE (obj%pstrain)
IF (ALLOCATED(obj%pparam)) DEALLOCATE (obj%pparam)

IF (ALLOCATED(obj%stress0)) DEALLOCATE (obj%stress0)
IF (ALLOCATED(obj%tstrain0)) DEALLOCATE (obj%tstrain0)
IF (ALLOCATED(obj%pstrain0)) DEALLOCATE (obj%pstrain0)
IF (ALLOCATED(obj%pparam0)) DEALLOCATE (obj%pparam0)

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
! Internal variables
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
INTEGER(I4B) :: origin, stat, tsize
LOGICAL(LGT) :: abool, isok
REAL(DFP), ALLOCATABLE :: temprealvec(:)
LOGICAL(LGT), ALLOCATABLE :: tempboolvec(:)
TYPE(toml_array), POINTER :: array

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL Abstract1DSTImportFromToml(obj, table)

CALL toml_get(table, "updateTanmat", obj%updateTanmat, .FALSE., &
              stat=stat, origin=origin)
CALL toml_get(table, "toleranceForNR", obj%toleranceForNR, &
              1.0D-8, stat=stat, origin=origin)
CALL toml_get(table, "maxIterNumNR", obj%maxIterNumNR, &
              10_I4B, stat=stat, origin=origin)

! INFO: saveQPData
#ifdef DEBUG_VER
CALL Display(myName//" saveQPData")
#endif

array => NULL()
obj%saveQPData = .TRUE.
CALL toml_get(table, "saveQPData", array, origin=origin, stat=stat)
CALL toml_get(array, tempboolvec, origin=origin, stat=stat)

IF (SIZE(tempboolvec) .EQ. 3) THEN
  obj%saveQPData = tempboolvec
ELSEIF (SIZE(tempboolvec) .GT. 0) THEN
  CALL AssertError1(.FALSE., myname, "saveQPData should have 3 values")
END IF
array => NULL()
DEALLOCATE (tempboolvec)

! INFO: plotQPData
#ifdef DEBUG_VER
CALL Display(myName//" plotQPData")
#endif

obj%plotQPData = .TRUE.
CALL toml_get(table, "plotQPData", array, origin=origin, stat=stat)
CALL toml_get(array, tempboolvec, origin=origin, stat=stat)

IF (SIZE(tempboolvec) .EQ. 3) THEN
  obj%plotQPData = tempboolvec
ELSEIF (SIZE(tempboolvec) .GT. 0) THEN
  CALL AssertError1(.FALSE., myname, "plotQPData should have 3 values")
END IF
array => NULL()
DEALLOCATE (tempboolvec)

tsize = obj%totalSpaceElements

ALLOCATE (obj%stress(tsize), obj%tstrain(tsize), &
          obj%pstrain(tsize), obj%pparam(tsize))

ALLOCATE (obj%stress0(tsize), obj%tstrain0(tsize), &
          obj%pstrain0(tsize), obj%pparam0(tsize))

!INFO: plasticModulus
#ifdef DEBUG_VER
CALL Display(myName//" plasticModulus")
#endif

CALL ElementDataImportFromToml(table, "plasticModulus", temprealvec, &
                               obj%totalSpaceElements, isok)
CALL AssertError1(isok, myname, "plasticModulus is not found")

CALL Reallocate(obj%plasticModulus, obj%totalSpaceElements)

abool = SIZE(temprealvec) .EQ. 1
IF (abool) THEN
  obj%plasticModulus = temprealvec(1)
ELSE
  isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
  CALL AssertError1(isok, myname, "plasticModulus should have "// &
                    "totalSpaceElements values")
  obj%plasticModulus(:) = temprealvec(1:obj%totalSpaceElements)
END IF

!INFO: yieldStress
#ifdef DEBUG_VER
CALL Display(myName//" yieldStress")
#endif

CALL ElementDataImportFromToml(table, "yieldStress", temprealvec, &
                               obj%totalSpaceElements, isok)
CALL AssertError1(isok, myname, "yieldStress is not found")

CALL Reallocate(obj%yieldStress, obj%totalSpaceElements)

abool = SIZE(temprealvec) .EQ. 1
IF (abool) THEN
  obj%yieldStress = temprealvec(1)
ELSE
  isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
  CALL AssertError1(isok, myname, "yieldStress should have "// &
                    "totalSpaceElements values")
  obj%yieldStress(:) = temprealvec(1:obj%totalSpaceElements)
END IF

!INFO: extraPlasticParam1
#ifdef DEBUG_VER
CALL Display(myName//" extraPlasticParam1")
#endif
CALL ElementDataImportFromToml(table, "extraPlasticParam1", temprealvec, &
                               obj%totalSpaceElements, isok)

CALL Reallocate(obj%extraPlasticParam1, obj%totalSpaceElements)

IF (.NOT. isok) THEN
  obj%extraPlasticParam1 = 0.0

ELSE

  abool = SIZE(temprealvec) .EQ. 1
  IF (abool) THEN
    obj%extraPlasticParam1 = temprealvec(1)
  ELSE
    isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
    CALL AssertError1(isok, myname, "extraPlasticParam1 should have "// &
                      "totalSpaceElements values")
    obj%extraPlasticParam1(:) = temprealvec(1:obj%totalSpaceElements)
  END IF
END IF

!INFO: extraPlasticParam2
#ifdef DEBUG_VER
CALL Display(myName//" extraPlasticParam2")
#endif
CALL ElementDataImportFromToml(table, "extraPlasticParam2", temprealvec, &
                               obj%totalSpaceElements, isok)

CALL Reallocate(obj%extraPlasticParam2, obj%totalSpaceElements)

IF (.NOT. isok) THEN
  obj%extraPlasticParam2 = 0.0

ELSE

  abool = SIZE(temprealvec) .EQ. 1
  IF (abool) THEN
    obj%extraPlasticParam2 = temprealvec(1)
  ELSE
    isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
    CALL AssertError1(isok, myname, "extraPlasticParam2 should have "// &
                      "totalSpaceElements values")
    obj%extraPlasticParam2(:) = temprealvec(1:obj%totalSpaceElements)
  END IF
END IF

! INFO: currently constant time order is assumed
isok = ANY(obj%timeOrder .EQ. obj%timeOrder(1))
CALL AssertError1(isok, myname, &
                  "Currently the time order should be constant")

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCt()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips
  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%elemsdForTime%dNdXi(1:ncol, 1, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=one, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips
  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%elemsdForTime%N(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=one, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = zero

scale = one
CALL OuterProd_(a=obj%timeShapeFuncBndy(1:nrow, 1), &
                b=obj%timeShapeFuncBndy(1:ncol, 1), &
                ans=ans, nrow=ii, ncol=jj, anscoeff=zero, scale=scale)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMtPlus

!----------------------------------------------------------------------------
!                                                                    GetWt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetWt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetWt()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow

obj%wt = zero
obj%wmt = zero

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow

obj%at = zero
obj%at_right = zero
obj%tat = zero

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetAt

!----------------------------------------------------------------------------
!                                                                    GetBt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBt()"
#endif
INTEGER(I4B) :: order, integralOrder, ii, jj, kk, &
                nipt, nipt_int, nnt, nrow, ncol
REAL(DFP) :: refCoord(1, 2), ja, scale
REAL(DFP), ALLOCATABLE :: int_points(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = 1_I4B
integralOrder = 2 * order
refCoord(1, 1) = minus_one
refCoord(1, 2) = one

CALL QuadPoint_Initiate(obj=obj%intQuadForTime, elemType=elem%line, &
                        domainName="B", order=integralOrder, &
                        quadratureType=GaussLegendre)
CALL LagrangeElemShapeData(obj=obj%linElemsdForTime, &
                           quad=obj%intQuadForTime, &
                           nsd=obj%elemsdForTime%nsd, &
                           xidim=obj%elemsdForTime%xidim, &
                           elemtype=elem%line, &
                           refelemCoord=refCoord, &
                           domainName="B", &
                           order=obj%timeOrder(1))

nipt = obj%elemsdfortime%nips
int_points = obj%intQuadForTime%points
nipt_int = SIZE(int_points, 2)
nnt = obj%elemsdForTime%nns

obj%bt = zero

DO ii = 1, nipt
  refCoord(1, 2) = obj%quadForTime%points(1, ii)
  CALL Elemsd_Set(obj=obj%linElemsdForTime, val=refCoord, &
                  N=obj%linElemsdForTime%N, dNdXi=obj%linElemsdForTime%dNdXi)
  int_points(1, :) = obj%linElemsdForTime%coord(1, :)
  ja = obj%linElemsdForTime%jacobian(1, 1, 1)
  CALL QuadPoint_Initiate(obj=obj%intQuadForTime, &
                          points=int_points)
  refCoord(1, 2) = one
  CALL LagrangeElemShapeData(obj=obj%intElemsdForTime, &
                             quad=obj%intQuadForTime, &
                             nsd=obj%elemsdForTime%nsd, &
                             xidim=obj%elemsdForTime%xidim, &
                             elemtype=elem%line, &
                             refelemCoord=refCoord, &
                             domainName="B", &
                             order=obj%timeOrder(1))

  DO jj = 1, nnt
    DO kk = 1, nipt_int
      scale = obj%intElemsdForTime%ws(kk) * half * ja
      obj%bt(jj, ii) = obj%bt(jj, ii) + &
                       scale * int_points(2, kk) * &
                       obj%intElemsdForTime%N(jj, kk)
    END DO
  END DO
END DO

nrow = obj%elemsdForTime%nns
ncol = obj%elemsdForTime%nips

obj%bt_right(1:nrow) = zero

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetBt

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetKt_Tilda
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetKt_Tilda()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips

  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)

  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%bt(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=one, scale=scale)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetKt_Tilda

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleTanmat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleTanmat()"
INTEGER(I4B) :: ielSpace, nrow, ncol, nns, nnt, tcon, con(256)
REAL(DFP) :: dt, dt_by_2, dts, dts_by_2, dx, dx_by_2, &
             dx2, dx2_by_2, two_by_dx, xij(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dt = obj%timeElemLength(timeElemNum)
dt_by_2 = dt * 0.5_DFP
dts = dt * dt
dts_by_2 = dts * 0.5_DFP

nnt = obj%elemsdForTime%nns

CALL obj%SetQuadForTime(timeElemNum)
CALL obj%SetElemsdForTime(timeElemNum, tij)

xij(1, 1) = obj%spaceDomain(1)

CALL CSRMatrix_Set(obj=obj%tanmat, VALUE=zero)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP
  dx2 = dx * dx
  dx2_by_2 = dx2 * 0.5_DFP
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  obj%ke = zero

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * dx_by_2 &
                           * obj%ms(1:nrow, 1:ncol)

  nns = nrow

  CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
  obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                           two_by_dx * obj%ks(1:nrow, 1:ncol)

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  CALL OTimesTilda(a=obj%ct(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=zero, scale=one)

  CALL OTimesTilda(a=obj%mtplus(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=one)

  CALL OTimesTilda(a=obj%kt_tilda(1:nnt, 1:nnt), &
                   b=obj%ks(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=dts_by_2)

  CALL OTimesTilda(a=obj%mt(1:nnt, 1:nnt), &
                   b=obj%cs(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=dt_by_2)

  CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ke(1:nrow, 1:ncol), &
                     scale=one, storageFMT=DOF_FMT, nodenum=con(1:tcon))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleTanmat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHSF
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHSF()"
INTEGER(I4B) :: con(256), ielSpace, nrow, ncol, nns, nnt, tsize
REAL(DFP) :: dx, dx_by_2, two_by_dx, dt, &
             f1(MAX_ORDER_SPACE + 1), &
             v0(MAX_ORDER_SPACE + 1), &
             xij(1, 2)
INTEGER(I4B), PARAMETER :: conversion(1) = [NONE]
LOGICAL(LGT) :: isTractionLeft, isTractionRight
INTEGER(I4B) :: idofs(MAX_ORDER_TIME + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL RealVector_Set(obj=obj%rhsf, VALUE=zero)

nnt = obj%elemsdForTime%nns

isTractionRight = ASSOCIATED(obj%tractionRight)
isTractionLeft = ASSOCIATED(obj%tractionLeft)

dt = obj%timeElemLength(timeElemNum)

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * half
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  obj%rhse = zero

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * &
                           dx_by_2 * obj%ms(1:nrow, 1:ncol)

  CALL RealVector_GetValue_(obj=obj%v0, nodenum=con(1:nns), &
                            VALUE=v0, tsize=nns)

  f1(1:nns) = MATMUL(obj%ms(1:nrow, 1:ncol), v0(1:ncol))

  CALL OTimesTilda(a=obj%timeShapeFuncBndy(1:nnt, 1), b=f1(1:nns), ans=obj%rhse, &
                   tsize=tsize, anscoeff=zero, scale=one)

  !! Body force
  CALL obj%GetBodyForce(ans=obj%rhse, tsize=tsize, spaceElemNum=ielSpace, &
                        timeElemNum=timeElemNum, anscoeff=one, scale=one)

  CALL RealVector_Add(obj=obj%rhsf, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

  xij(1, 1) = xij(1, 2)

END DO

! Traction right
IF (isTractionRight) THEN
  CALL obj%GetConnectivity(spaceElemNum=obj%totalSpaceElements, ans=con, tsize=nns)
  CALL obj%GetTractionRight(ans=obj%rhse, tsize=tsize, &
                            timeElemNum=timeElemNum, anscoeff=zero, scale=one)

  CALL RealVector_Add(obj=obj%rhsf, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

END IF

! Traction left
IF (isTractionLeft) THEN
  CALL obj%GetConnectivity(spaceElemNum=1, ans=con, tsize=nns)
  CALL obj%GetTractionLeft(ans=obj%rhse, tsize=tsize, &
                           timeElemNum=timeElemNum, anscoeff=zero, scale=one)

  CALL RealVector_Add(obj=obj%rhsf, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

END IF

idofs(1:nnt) = arange(1_I4B, nnt)
obj%currentExternalForceNorm = RealVector_Norm(obj=obj%rhsf, &
                                               dof=obj%dof, &
                                               idof=idofs(1:nnt))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleRHSF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"
#endif
INTEGER(I4B) :: ielSpace, nrow, ncol, nns, nnt, tcon, con(256), tsize
REAL(DFP) :: dt, dt_by_2, dts, dts_by_2, dx, dx_by_2, &
             dx2, dx2_by_2, two_by_dx, xij(1, 2), scale
REAL(DFP) :: sol_v((MAX_ORDER_SPACE + 1) * (MAX_ORDER_TIME + 1))
INTEGER(I4B), PARAMETER :: conversion(1) = [NONE]
INTEGER(I4B) :: idofs(MAX_ORDER_TIME + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dt = obj%timeElemLength(timeElemNum)
dt_by_2 = dt * 0.5_DFP
dts = dt * dt
dts_by_2 = dts * 0.5_DFP

CALL obj%SetElemsdForTime(timeElemNum, tij)

nnt = obj%elemsdForTime%nns
idofs = 0
idofs(1:nnt) = arange(1_I4B, nnt)

xij(1, 1) = obj%spaceDomain(1)

CALL RealVector_Set(obj=obj%rhs, VALUE=zero)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP
  dx2 = dx * dx
  dx2_by_2 = dx2 * 0.5_DFP
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  obj%ke = 0.0_DFP

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * dx_by_2 &
                           * obj%ms(1:nrow, 1:ncol)
  nns = nrow

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  CALL OTimesTilda(a=obj%ct(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=zero, scale=one)

  CALL OTimesTilda(a=obj%mtplus(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=one)

  CALL OTimesTilda(a=obj%mt(1:nnt, 1:nnt), &
                   b=obj%cs(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=dt_by_2)

  CALL RealVector_GetValue_(obj=obj%sol0, dofobj=obj%dof, &
                            idof=idofs(1:nnt), nodenum=con(1:nns), &
                            VALUE=sol_v, tsize=tsize)

  obj%rhse = zero
  !! add the result of matrix vector multiplication to rhs
  obj%rhse(1:tsize) = MATMUL(obj%ke(1:tsize, 1:tsize), sol_v(1:tsize))
  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
                      scale=one, dofobj=obj%dof, nodenum=con(1:nns), &
                      conversion=conversion)

  !! and calculate the internal force from modified stress
  CALL GetInternalForce(obj=obj, spaceElemNum=ielSpace, tsize=tsize)
  scale = dt_by_2
  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
                      scale=scale, dofobj=obj%dof, nodenum=con(1:nns), &
                      conversion=conversion)

  xij(1, 1) = xij(1, 2)

END DO

CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhsf, scale=minus_one)
CALL RealVector_Scale(obj%rhs, minus_one)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE AssembleRHS_predict(obj, timeElemNum, tij)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(inout) :: obj
  INTEGER(I4B), INTENT(IN) :: timeElemNum
  REAL(DFP), INTENT(IN) :: tij(1, 2)
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"
#endif

  INTEGER(I4B) :: con(256), ielSpace, nrow, ncol, nns, nnt, tsize, &
                  nips, nipt, ii

  REAL(DFP) :: dx, dx_by_2, two_by_dx, dt, &
               dt_by_2, minus_dt_by_2, &
               f1(MAX_ORDER_SPACE + 1), &
               f2(MAX_ORDER_SPACE + 1), &
               v0(MAX_ORDER_SPACE + 1), &
               u0(MAX_ORDER_SPACE + 1), &
               xij(1, 2), scale

  INTEGER(I4B), PARAMETER :: conversion(1) = [NONE]

  LOGICAL(LGT) :: isTractionLeft, isTractionRight

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL RealVector_Set(obj=obj%rhs, VALUE=zero)

  nnt = obj%elemsdForTime%nns

  isTractionRight = ASSOCIATED(obj%tractionRight)
  isTractionLeft = ASSOCIATED(obj%tractionLeft)

  nipt = obj%elemsdForTime%nips
  dt = obj%timeElemLength(timeElemNum)
  minus_dt_by_2 = minus_one * dt * 0.5_DFP
  dt_by_2 = minus_one * minus_dt_by_2

  xij(1, 1) = obj%spaceDomain(1)

  DO ielSpace = 1, obj%totalSpaceElements

    dx = obj%spaceElemLength(ielSpace)
    dx_by_2 = dx * 0.5_DFP
    two_by_dx = 2.0_DFP / dx

    xij(1, 2) = xij(1, 1) + dx

    CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

    CALL obj%SetQuadForSpace(ielSpace)
    CALL obj%SetElemsdForSpace(ielSpace, xij)
    nips = obj%elemsdForSpace%nips

    obj%rhse = 0.0_DFP

    CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
    obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * &
                             dx_by_2 * obj%ms(1:nrow, 1:ncol)

    CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
    obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                             two_by_dx * obj%ks(1:nrow, 1:ncol)

    CALL RealVector_GetValue_(obj=obj%v0, nodenum=con(1:nns), VALUE=v0, &
                              tsize=nns)

    CALL RealVector_GetValue_(obj=obj%u0, nodenum=con(1:nns), VALUE=u0, &
                              tsize=nns)

    f1(1:nns) = MATMUL(obj%ms(1:nrow, 1:ncol), v0(1:ncol))

    CALL OTimesTilda(a=obj%timeShapeFuncBndy(1:nnt, 1), b=f1(1:nns), &
                     ans=obj%rhse, tsize=tsize, anscoeff=zero, scale=one)

  !! Body force
    CALL obj%GetBodyForce(ans=obj%rhse, tsize=tsize, spaceElemNum=ielSpace, &
                          timeElemNum=timeElemNum, anscoeff=one, scale=one)

    ! CALL RealVector_Add(obj=)
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

    obj%rhse = zero

    DO ii = 1, nips

      scale = minus_dt_by_2 * obj%elemsdForSpace%ws(ii) * &
              obj%elemsdForSpace%thickness(ii)

      scale = scale * obj%stress0(ielSpace)%val(ii)

      obj%rhse(1:nns) = obj%rhse(1:nns) + &
                        scale * obj%elemsdForSpace%dNdXi(1:nns, 1, ii)

    END DO

    DO ii = 2, nnt
      obj%rhse((ii - 1) * nns + 1:ii * nns) = obj%rhse(1:nns)
    END DO

    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:nnt * nns), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

    xij(1, 1) = xij(1, 2)

  END DO

! Traction right
  IF (isTractionRight) THEN
  CALL obj%GetConnectivity(spaceElemNum=obj%totalSpaceElements, ans=con, tsize=nns)
    CALL obj%GetTractionRight(ans=obj%rhse, tsize=tsize, &
                            timeElemNum=timeElemNum, anscoeff=zero, scale=one)

    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

  END IF

! Traction left
  IF (isTractionLeft) THEN
    CALL obj%GetConnectivity(spaceElemNum=1, ans=con, tsize=nns)
    CALL obj%GetTractionLeft(ans=obj%rhse, tsize=tsize, &
                            timeElemNum=timeElemNum, anscoeff=zero, scale=one)

    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE AssembleRHS_predict

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE GetInternalForce(obj, spaceElemNum, tsize)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: spaceElemNum
  INTEGER(I4B), INTENT(OUT) :: tsize
  INTEGER(I4B) :: nips, nipt, nns, nnt, ii, jj
  REAL(DFP) :: scale1, scale2

  nips = obj%elemsdForSpace%nips
  nipt = obj%elemsdForTime%nips

  nns = obj%elemsdForSpace%nns
  nnt = obj%elemsdForTime%nns

  obj%rhse = zero

  DO jj = 1, nipt

    scale1 = obj%elemsdForTime%ws(jj) * &
             obj%elemsdForTime%thickness(jj)

    DO ii = 1, nips

      scale2 = scale1 * obj%elemsdForSpace%ws(ii) * &
               obj%elemsdForSpace%thickness(ii)

      scale2 = scale2 * obj%stress(spaceElemNum)%Val(ii, jj)

      CALL OTimesTilda(a=obj%elemsdForTime%N(1:nnt, jj), &
                       b=obj%elemsdForSpace%dNdXi(1:nns, 1, ii), &
                       ans=obj%rhse, tsize=tsize, &
                       anscoeff=one, scale=scale2)

    END DO

  END DO

END SUBROUTINE GetInternalForce

!----------------------------------------------------------------------------
!                                                                 set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
#endif

INTEGER(I4B) :: tnodes, nrow, ncol
REAL(DFP) :: tij(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Abstract1DSTSet(obj)

tnodes = obj%totalVertexDOFSpace + obj%totalEdgeDOFSpace
CALL RealVector_Initiate(obj%u_theta, tnodes)

CALL SetQuadratureVariables(obj)

! INFO: currently constant time order is assumed
tij(1, 1) = zero
tij(1, 2) = obj%timeElemLength(1)

CALL obj%InitiateConnectivity()

CALL obj%InitiateFields(timeElemNum=1)
tnodes = RealVector_Size(obj%rhs)
CALL RealVector_Initiate(obj%rhsf, tnodes)
CALL RealVector_Initiate(obj%sol0, tnodes)
CALL RealVector_Set(obj%sol0, VALUE=zero)

CALL obj%SetQuadForTime(1)
CALL obj%SetElemsdForTime(1, tij)

CALL obj%GetMt(ans=obj%mt, nrow=nrow, ncol=ncol)
CALL obj%GetMtPlus(ans=obj%mtplus, nrow=nrow, ncol=ncol)
CALL obj%GetCt(ans=obj%ct, nrow=nrow, ncol=ncol)
CALL obj%GetWt(ans=obj%ct, nrow=nrow, ncol=ncol)
CALL obj%GetAt()
CALL obj%GetBt()
CALL obj%GetKt_Tilda(ans=obj%kt_tilda, nrow=nrow, ncol=ncol)

CALL CSRMatrix_Set(obj=obj%tanmat, VALUE=zero)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetQuadratureVariables(obj)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: iel, nips, tsize
  INTEGER(I4B) :: maxnips, maxnipt

  tsize = 0
  maxnips = 0
  CALL obj%SetQuadForTime(1)
  maxnipt = Quad_size(obj%quadForTime, 2)

  DO iel = 1, obj%totalSpaceElements
    CALL obj%SetQuadForSpace(iel)
    nips = Quad_Size(obj%quadForSpace, 2)
    tsize = tsize + nips

    IF (maxnips .LT. nips) THEN
      maxnips = nips
    END IF

    ! RealVector_
    CALL RealVector_Initiate(obj%stress0(iel), nips)
    CALL RealVector_Initiate(obj%tstrain0(iel), nips)
    CALL RealVector_Initiate(obj%pstrain0(iel), nips)
    CALL RealVector_Initiate(obj%pparam0(iel), nips)

    CALL RealVector_Set(obj%stress0(iel), VALUE=zero)
    CALL RealVector_Set(obj%tstrain0(iel), VALUE=zero)
    CALL RealVector_Set(obj%pstrain0(iel), VALUE=zero)
    CALL RealVector_Set(obj%pparam0(iel), VALUE=zero)

    ! RealMatrix_
    CALL RealMatrix_Initiate(obj%stress(iel), [nips, maxnipt])
    CALL RealMatrix_Initiate(obj%tstrain(iel), [nips, maxnipt])
    CALL RealMatrix_Initiate(obj%pstrain(iel), [nips, maxnipt])
    CALL RealMatrix_Initiate(obj%pparam(iel), [nips, maxnipt])

    obj%stress(iel)%Val = zero
    obj%tstrain(iel)%Val = zero
    obj%pstrain(iel)%Val = zero
    obj%pparam(iel)%Val = zero

  END DO

  obj%maxNIPTime = maxnipt
  obj%maxNIPSpace = maxnips
  obj%totalQuadPointsForSpace = tsize

END SUBROUTINE SetQuadratureVariables

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateQPVariables
CHARACTER(*), PARAMETER :: myName = "obj_UpdateQPVariables()"
INTEGER(I4B) :: iel, nips, nipt, ii, jj, nns, tsize
REAL(DFP) :: xij(1, 2), u0(MAX_ORDER_SPACE + 1), dx, twobydx
REAL(DFP) :: dstrain(2 * MAX_ORDER_SPACE + 1), &
             dstress(2 * MAX_ORDER_SPACE + 1), &
             dt, tanMod
INTEGER(I4B) :: con(MAX_ORDER_SPACE + 1)

xij(1, 1) = obj%spaceDomain(1)
nipt = obj%elemsdForTime%nips
dt = obj%timeElemLength(obj%currentTimeStep)

DO ii = 1, nipt

  CALL Set_dU_theta(obj=obj, iptid=ii, dt=dt)

  DO iel = 1, obj%totalSpaceElements

    CALL obj%GetConnectivity(spaceElemNum=iel, ans=con, tsize=nns)

    dx = obj%spaceElemLength(iel)
    twobydx = 2.0_DFP / dx

    xij(1, 2) = xij(1, 1) + dx

    CALL obj%SetQuadForSpace(iel)
    CALL obj%SetElemsdForSpace(iel, xij)
    nips = obj%elemsdForSpace%nips

    u0 = zero
    dstrain = zero
    dstress = zero

    CALL RealVector_GetValue_(obj=obj%u_theta, nodenum=con(1:nns), &
                              VALUE=u0, tsize=tsize)

    dstrain(1:nips) = twobydx * MATMUL(u0(1:tsize), &
                                       obj%elemsdForSpace%dNdXi(:, 1, :))

    obj%tstrain(iel)%val(:, ii) = obj%tstrain0(iel)%val(:) + &
                                  dstrain(1:nips)

    dstress(1:nips) = obj%elasticModulus(iel) * dstrain(1:nips)

    obj%stress(iel)%val(:, ii) = obj%stress0(iel)%val(:) + &
                                 dstress(1:nips)

    DO jj = 1, nips
      IF (ASSOCIATED(obj%UserReturnMapping)) THEN
        CALL obj%UserReturnMapping(obj=obj, spaceElemNum=iel, &
                                   stress=obj%stress(iel)%val(jj, ii), &
                                   tstrain=obj%tstrain(iel)%val(jj, ii), &
                                   pstrain=obj%pstrain(iel)%val(jj, ii), &
                                   pparam=obj%pparam(iel)%val(jj, ii), &
                                   stress0=obj%stress0(iel)%val(jj), &
                                   tstrain0=obj%tstrain0(iel)%val(jj), &
                                   pstrain0=obj%pstrain0(iel)%val(jj), &
                                   pparam0=obj%pparam0(iel)%val(jj) &
                                   )
      ELSE
        CALL ReturnMapping(obj=obj, spaceElemNum=iel, &
                           stress=obj%stress(iel)%val(jj, ii), &
                           pstrain=obj%pstrain(iel)%val(jj, ii), &
                           pparam=obj%pparam(iel)%val(jj, ii), &
                           pstrain0=obj%pstrain0(iel)%val(jj), &
                           pparam0=obj%pparam0(iel)%val(jj) &
                           )
      END IF
    END DO

    xij(1, 1) = xij(1, 2)

  END DO

END DO

END PROCEDURE obj_UpdateQPVariables

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Set_dU_theta(obj, iptid, dt)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: iptid
  REAL(DFP), INTENT(IN) :: dt
  INTEGER(I4B) :: nnt, ii
  REAL(DFP) :: scale

  ! CALL RealVector_Set(obj=obj%u_theta, VALUE=obj%u0)
  CALL RealVector_Set(obj=obj%u_theta, VALUE=zero)

  nnt = obj%elemsdForTime%nns

  DO ii = 1, nnt
    scale = obj%bt(ii, iptid) * dt
    CALL RealVector_Add(obj1=obj%u_theta, dofobj1=obj%dof, idof1=1_I4B, &
                        obj2=obj%sol0, dofobj2=obj%dof, idof2=ii, scale=scale)
  END DO

END SUBROUTINE Set_dU_theta

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! default return mapping for bilinear elasto-plastic model
SUBROUTINE ReturnMapping(obj, spaceElemNum, stress, pstrain, pparam, pstrain0, pparam0)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: spaceElemNum
  REAL(DFP), INTENT(INOUT) :: stress, pstrain, pparam
  REAL(DFP), INTENT(IN) :: pstrain0, pparam0

  REAL(DFP), PARAMETER :: tolF = 1.0E-8
  REAL(DFP) :: dlam, f0, f1, signS, eMod, pMod, sigY0, &
               tmp, signP

  eMod = obj%elasticModulus(spaceElemNum)
  pMod = obj%plasticModulus(spaceElemNum)
  sigY0 = obj%yieldStress(spaceElemNum)

  pstrain = pstrain0
  pparam = pparam0
  signS = stress / ABS(stress)

  f0 = ABS(stress) - (sigY0 + pMod * pparam)

  IF (f0 .LE. zero) THEN
    !! elastic domain
    RETURN
  ELSE
    f1 = f0
    DO WHILE (f1 .GT. f0 * tolF)
      tmp = eMod + pMod
      dlam = f1 / tmp
      stress = stress - dlam * eMod * signS
      pstrain = pstrain + dlam * signS
      pparam = pparam + ABS(dlam)
      f1 = ABS(stress) - (sigY0 + pMod * pparam)
    END DO
  END IF

END SUBROUTINE ReturnMapping

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE UpdateQPVariables_EndPoint(obj, force)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force
  CHARACTER(*), PARAMETER :: myName = "UpdateQPVariables_EndPoint()"
  INTEGER(I4B) :: ielSpace, nns, nips, nipt, iipt
  REAL(DFP) :: xij(1, 2), u0(MAX_ORDER_SPACE + 1), dx, twobydx
  REAL(DFP) :: vals(2 * MAX_ORDER_SPACE + 1)
  INTEGER(I4B) :: con(MAX_ORDER_SPACE + 1)
  LOGICAL(LGT) :: isLobatto, force0

  force0 = Input(default=.FALSE., option=force)

  isLobatto = ANY([GaussLegendreLobatto, GaussLobatto, &
                   GaussChebyshevLobatto, GaussJacobiLobatto, &
                   GaussUltrasphericalLobatto] &
                  .EQ. obj%quadTypeForTime)

  IF (isLobatto .AND. .NOT. force0) THEN
    CALL Display("!! lobatto quadrature you use in time !!")
    nipt = obj%elemsdForTime%nips
    DO ielSpace = 1, obj%totalSpaceElements

      CALL RealVector_Set(obj%tstrain0(ielSpace), &
                          VALUE=obj%tstrain(ielSpace)%val(:, nipt))
      CALL RealVector_Set(obj%stress0(ielSpace), &
                          VALUE=obj%stress(ielSpace)%val(:, nipt))
      CALL RealVector_Set(obj%pstrain0(ielSpace), &
                          VALUE=obj%pstrain(ielSpace)%val(:, nipt))
      CALL RealVector_Set(obj%pparam0(ielSpace), &
                          VALUE=obj%pparam(ielSpace)%val(:, nipt))

      DO iipt = 1, nipt - 1
        obj%stress(ielSpace)%val(:, iipt) = obj%stress(ielSpace)%val(:, nipt)
       obj%tstrain(ielSpace)%val(:, iipt) = obj%tstrain(ielSpace)%val(:, nipt)
       obj%pstrain(ielSpace)%val(:, iipt) = obj%pstrain(ielSpace)%val(:, nipt)
        obj%pparam(ielSpace)%val(:, iipt) = obj%pparam(ielSpace)%val(:, nipt)
      END DO

    END DO

    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[NOT IMPLEMENTED] lobatto quadrature is needed')

END SUBROUTINE UpdateQPVariables_EndPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Update
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Update()"
#endif

INTEGER(I4B) :: ii, nnt, nipt
REAL(DFP) :: scale, dt
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%converged) THEN

  CALL RealVector_Set(obj%sol, obj%sol0)
  CALL RealVector_Set(obj%sol0, VALUE=zero)

  dt = obj%timeElemLength(obj%currentTimeStep)
  obj%currentTime = obj%currentTime + dt
  obj%currentTimeStep = obj%currentTimeStep + 1

  nnt = obj%elemsdForTime%nns
  nipt = obj%elemsdForTime%nips

  IF (obj%saveErrorNorm(1)) &
    CALL RealVector_Set(obj=obj%um1, VALUE=obj%u0)
  IF (obj%saveErrorNorm(2)) &
    CALL RealVector_Set(obj=obj%vm1, VALUE=obj%v0)
! IF (obj%saveErrorNorm(3)) &
!   CALL RealVector_Set(obj=obj%am1, VALUE=obj%a0)

  CALL RealVector_Set(obj=obj%v0, VALUE=zero)

  CALL RealVector_Scale(obj%u0, one)

  DO ii = 1, nnt
    scale = obj%timeShapeFuncBndy(ii, 2)
    CALL RealVector_Add(obj1=obj%v0, dofobj1=obj%dof, idof1=1_I4B, &
                        obj2=obj%sol, dofobj2=obj%dof, idof2=ii, scale=scale)

    !! WARN: lobatto quadrature is assumed
    scale = obj%bt(ii, nipt) * dt
    CALL RealVector_Add(obj1=obj%u0, dofobj1=obj%dof, idof1=1_I4B, &
                        obj2=obj%sol, dofobj2=obj%dof, idof2=ii, scale=scale)
  END DO

  CALL UpdateQPVariables_EndPoint(obj)

ELSE

  CALL RealVector_Add(obj=obj%sol0, VALUE=obj%sol, &
                      scale=one)

  CALL obj%UpdateQPVariables()

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Update

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteData()"
#endif

REAL(DFP) :: stData(obj%totalQuadPointsForSpace, 4), &
             xij(1, 2), dx, t
INTEGER(I4B) :: ielSpace, nips, ind
CHARACTER(:), ALLOCATABLE :: filename_stress, filename_tstrain, &
                             filename_pstrain
LOGICAL(LGT) :: isok1, isok2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Abstract1DSTWriteData(obj)

isok1 = ANY(obj%saveQPData)
isok2 = ANY(obj%plotQPData)

IF (.NOT. isok1 .AND. .NOT. isok2) RETURN

t = obj%currentTime
xij(1, 1) = obj%spaceDomain(1)

filename_stress = obj%result_dir//CHAR_SLASH//obj%filename//'_stress_'// &
                  tostring(obj%currentTimeStep - 1_I4B)
filename_tstrain = obj%result_dir//CHAR_SLASH//obj%filename//'_tstrain_'// &
                   tostring(obj%currentTimeStep - 1_I4B)
filename_pstrain = obj%result_dir//CHAR_SLASH//obj%filename//'_pstrain_'// &
                   tostring(obj%currentTimeStep - 1_I4B)

ind = 0

DO ielSpace = 1, obj%totalSpaceElements

  dx = obj%spaceElemLength(ielSpace)

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)
  nips = obj%elemsdForSpace%nips

  stData(ind + 1:ind + nips, 1) = obj%elemsdForSpace%coord(1, :)
  stData(ind + 1:ind + nips, 2) = obj%stress0(ielSpace)%val
  stData(ind + 1:ind + nips, 3) = obj%tstrain0(ielSpace)%val
  stData(ind + 1:ind + nips, 4) = obj%pstrain0(ielSpace)%val

  ind = ind + nips

  xij(1, 1) = xij(1, 2)

END DO

IF (obj%saveQPData(1)) &
  CALL ExportToCSVFile(obj=obj%stressfile, filename=filename_stress, &
                       xdata=stData(:, 1), ydata=stData(:, 2), &
                       timeVal=obj%currentTime, &
                       timeStep=obj%currentTimeStep - 1, &
                       tsize=obj%totalQuadPointsForSpace)

IF (obj%plotQPData(1)) &
  CALL plotData(obj%plot, filename_stress, stData(:, 1), stData(:, 2), &
                obj%spaceDomain, 'stress')

IF (obj%saveQPData(2)) &
  CALL ExportToCSVFile(obj=obj%tstrainfile, filename=filename_tstrain, &
                       xdata=stData(:, 1), ydata=stData(:, 3), &
                       timeVal=obj%currentTime, &
                       timeStep=obj%currentTimeStep - 1, &
                       tsize=obj%totalQuadPointsForSpace)

IF (obj%plotQPData(2)) &
  CALL plotData(obj%plot, filename_tstrain, stData(:, 1), stData(:, 3), &
                obj%spaceDomain, 'total strain')

IF (obj%saveQPData(3)) &
  CALL ExportToCSVFile(obj=obj%pstrainfile, filename=filename_pstrain, &
                       xdata=stData(:, 1), ydata=stData(:, 4), &
                       timeVal=obj%currentTime, &
                       timeStep=obj%currentTimeStep - 1, &
                       tsize=obj%totalQuadPointsForSpace)

IF (obj%plotQPData(3)) &
  CALL plotData(obj%plot, filename_pstrain, stData(:, 1), stData(:, 4), &
                obj%spaceDomain, 'plastic strain')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_WriteData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ExportToCSVFile(obj, filename, xdata, ydata,  timeVal, timeStep, tsize)
  TYPE(CSVFile_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: filename
  REAL(DFP), INTENT(IN) :: xdata(:), ydata(:)
  REAL(DFP), INTENT(IN) :: timeVal
  INTEGER(I4B), INTENT(IN) :: timeStep, tsize
  CHARACTER(:), ALLOCATABLE :: aline
  INTEGER(I4B) :: ii

  CALL obj%Initiate(filename=filename//".csv", unit=100, &
                    status="REPLACE", action="WRITE", &
                    comment="#", separator=",")
  CALL obj%OPEN()

  aline = "# time-step = "//tostring(timeStep - 1_I4B)// &
          ", time = "//tostring(timeVal)//" s"
  CALL obj%WRITE(aline)

  aline = "x, stress"
  CALL obj%WRITE(aline)

  DO ii = 1, tsize
    aline = tostring(xdata(ii))//", "//tostring(ydata(ii))
    CALL obj%WRITE(aline)
  END DO
  CALL obj%DEALLOCATE()

END SUBROUTINE ExportToCSVFile

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE plotData(obj, filename, xdata, ydata, xlim, ylabel)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(in) :: filename
  REAL(DFP), INTENT(IN) :: xdata(:), ydata(:)
  REAL(DFP), INTENT(IN) :: xlim(2)
  CHARACTER(*), INTENT(IN) :: ylabel

  REAL(DFP) :: xlim0(2), ylim(2)

  CALL obj%filename(filename//'.plt')
  CALL obj%options('set terminal pngcairo; set output "' &
                   //filename//'.png"')

  xlim0 = xlim
  ylim(1) = MINVAL(yDATA(:))
  ylim(2) = MAXVAL(yDATA(:))
  xlim0(1) = xlim0(1) - 0.1 * (xlim0(2) - xlim0(1))
  xlim0(2) = xlim0(2) + 0.1 * (xlim0(2) - xlim0(1))
  ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
  ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

  CALL obj%xlim(xlim0)
  CALL obj%ylim(ylim)
  CALL obj%xlabel('x')
  CALL obj%ylabel(ylabel)
  CALL obj%plot(x1=xDATA(:), y1=yDATA(:), ls1="with lines")
  CALL obj%reset()

END SUBROUTINE plotData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Run
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Run()"
#endif

INTEGER(I4B) :: ielTime, ii
REAL(DFP) :: x1, tij(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

x1 = obj%spaceDomain(1)
tij(1, 1) = obj%timeDomain(1)

CALL obj%SetInitialVelocity()
CALL obj%SetInitialDisplacement()
! CALL UpdateQPVariables_EndPoint(obj, force=.TRUE.)

! CALL obj%WriteData()

DO ielTime = 1, obj%totalTimeElements

  CALL Display(tij(1, 1), myname//" t1: ")

  tij(1, 2) = tij(1, 1) + obj%timeElemLength(ielTime)
  obj%converged = .FALSE.
  obj%currentNRStep = 0

  ! CALL Abstract1DSTAssembleTanmat(obj=obj, timeElemNum=ielTime, &
  !                                 tij=tij)
  CALL obj%AssembleTanmat(timeElemNum=ielTime, &
                          tij=tij)
  CALL obj%AssembleRHSF(timeElemNum=ielTime)
  CALL AssembleRHS_predict(obj=obj, timeElemNum=ielTime, &
                           tij=tij)
  CALL Abstract1DSTApplyDirichletBC(obj=obj, &
                                    timeElemNum=ielTime, tij=tij)
  CALL obj%Solve()
  CALL obj%Update()

  DO ii = 1, obj%maxIterNumNR
    CALL obj%AssembleRHS(timeElemNum=ielTime, tij=tij)
    CALL ApplyDirichletBC_Residual(obj=obj)
    CALL CheckConvergence(obj)

    IF (obj%converged) THEN
      CALL Display(obj%currentNRStep, " Current NR step :: ")
      CALL Display(obj%currentResidualNorm, &
                   "NR Converged ; final norm :: ")
      CALL obj%Update()
      EXIT
    ELSE
      obj%currentNRStep = ii
      CALL Display(obj%currentNRStep, " Current NR step :: ")
      CALL Display(obj%currentResidualNorm, &
                   "NR Continue; current norm :: ")
      IF (obj%updateTanmat) THEN
        CALL Display("Tangent matrix is updated")
        CALL obj%AssembleTanmat(timeElemNum=ielTime, tij=tij)
        CALL ApplyDirichletBC_Tanmat(obj)
      END IF
      CALL obj%Solve()
      CALL obj%Update()
    END IF
  END DO

  IF (obj%currentNRStep .EQ. obj%maxIterNumNR) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '!! Newton-Raphson iteration did not converge !!')
  END IF

  CALL obj%EvalErrorNorm(timeElemNum=ielTime, tij=tij)

  IF (MOD(ielTime, obj%outputFreq) .EQ. 0_I4B) &
    CALL obj%WriteData()

  tij(1, 1) = tij(1, 2)

END DO

CALL obj%WriteErrorData()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Run

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ApplyDirichletBC_Residual(obj)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj

  LOGICAL(LGT) :: isDirichletLeft, isDirichletRight
  INTEGER(I4B) :: nnt, tsize_dbc_idof, tsize, tsize_dbc_value

  isDirichletLeft = ASSOCIATED(obj%velocityLeft)
  isDirichletRight = ASSOCIATED(obj%velocityRight)
  tsize_dbc_idof = 0
  tsize_dbc_value = 0
  obj%dbc_value = zero

  IF (isDirichletLeft) THEN
    CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof, tsize=tsize, &
                       nodenum=1)

    tsize_dbc_idof = tsize
    nnt = obj%elemsdForTime%nns
    tsize_dbc_value = tsize_dbc_value + nnt
  END IF

  IF (isDirichletRight) THEN
    CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof(tsize_dbc_idof + 1:), &
                       tsize=tsize, &
                       nodenum=obj%totalSpaceNodes)

    tsize_dbc_idof = tsize_dbc_idof + tsize

    nnt = obj%elemsdForTime%nns
    tsize_dbc_value = tsize_dbc_value + nnt
  END IF

  IF (tsize_dbc_value .NE. 0) THEN
    CALL RealVector_Set(obj=obj%rhs, nodenum=obj%dbc_idof(1:tsize_dbc_idof), &
                        VALUE=obj%dbc_value(1:tsize_dbc_value))
  END IF

END SUBROUTINE ApplyDirichletBC_Residual

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ApplyDirichletBC_Tanmat(obj)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(inout) :: obj
  LOGICAL(LGT) :: isDirichletLeft, isDirichletRight
  INTEGER(I4B) :: tsize_dbc_idof, tsize

  isDirichletLeft = ASSOCIATED(obj%velocityLeft)
  isDirichletRight = ASSOCIATED(obj%velocityRight)
  tsize_dbc_idof = 0

  IF (isDirichletLeft) THEN
    CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof, tsize=tsize, &
                       nodenum=1)
    tsize_dbc_idof = tsize
  END IF

  IF (isDirichletRight) THEN
    CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof(tsize_dbc_idof + 1:), &
                       tsize=tsize, &
                       nodenum=obj%totalSpaceNodes)
    tsize_dbc_idof = tsize_dbc_idof + tsize
  END IF

  IF (tsize_dbc_idof .NE. 0) THEN
    CALL CSRMatrix_ApplyDBC(obj=obj%tanmat, &
                            dbcptrs=obj%dbc_idof(1:tsize_dbc_idof))
    CALL RealVector_Set(obj=obj%sol, VALUE=0.0_DFP)
  END IF

END SUBROUTINE ApplyDirichletBC_Tanmat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CheckConvergence(obj)
  CLASS(ElastoPlasticDynamics1DVSTFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: idofs(MAX_ORDER_TIME + 1), nnt

  nnt = obj%elemsdForTime%nns

  idofs(1:nnt) = arange(1_I4B, nnt)
  obj%currentResidualNorm = RealVector_Norm(obj=obj%rhs, &
                                            dof=obj%dof, &
                                            idof=idofs(1:nnt))

  IF (obj%currentExternalForceNorm .EQ. zero) THEN
    IF (obj%currentResidualNorm .LT. obj%toleranceForNR) &
      obj%converged = .TRUE.
  ELSE
    IF (obj%currentResidualNorm / obj%currentExternalForceNorm &
        .LT. obj%toleranceForNR) &
      obj%converged = .TRUE.
  END IF

END SUBROUTINE CheckConvergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
#include "../../include/errors.F90"

END SUBMODULE Methods
