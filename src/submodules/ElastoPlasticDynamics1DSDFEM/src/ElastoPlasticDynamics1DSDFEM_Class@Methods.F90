SUBMODULE(ElastoPlasticDynamics1DSDFEM_Class) Methods
USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_stat, toml_array

USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

USE Display_Method, ONLY: ToString, Display

USE GlobalData, ONLY: stdout, &
                      CHAR_LF, &
                      DOF_FMT, &
                      NONE, &
                      LIS_GMRES, &
                      CHAR_SLASH

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
!                                                   obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Abstract1DSDDeallocate(obj)

IF (ALLOCATED(obj%stress)) DEALLOCATE (obj%stress)
IF (ALLOCATED(obj%tstrain)) DEALLOCATE (obj%tstrain)
IF (ALLOCATED(obj%pstrain)) DEALLOCATE (obj%pstrain)
IF (ALLOCATED(obj%pparam)) DEALLOCATE (obj%pparam)

IF (ALLOCATED(obj%stress0)) DEALLOCATE (obj%stress0)
IF (ALLOCATED(obj%tstrain0)) DEALLOCATE (obj%tstrain0)
IF (ALLOCATED(obj%pstrain0)) DEALLOCATE (obj%pstrain0)
IF (ALLOCATED(obj%pparam0)) DEALLOCATE (obj%pparam0)

IF (ALLOCATED(obj%NRConvergedSteps)) &
  DEALLOCATE (obj%NRConvergedSteps)

IF (ASSOCIATED(obj%UserReturnMapping)) &
  obj%UserReturnMapping => NULL()
IF (ASSOCIATED(obj%UserGetTangentModulus)) &
  obj%UserGetTangentModulus => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                             ImportFromToml
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

CALL Abstract1DSDImportFromToml(obj, table)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                                   obj_Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
#endif

INTEGER(I4B) :: tnodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Abstract1DSDSet(obj)

ALLOCATE (obj%NRConvergedSteps(obj%totalTimeSteps))

CALL SetQuadratureVariables(obj)

CALL obj%InitiateConnectivity()

CALL obj%InitiateFields()

tnodes = RealVector_Size(obj%rhs)
CALL RealVector_Initiate(obj%rhsf, tnodes)
CALL RealVector_Initiate(obj%sol0, tnodes)
CALL RealVector_Set(obj%sol0, VALUE=zero)

CALL CSRMatrix_Set(obj=obj%tanmat, VALUE=zero)

!! default user subroutine
obj%UserReturnMapping => ReturnMapping
obj%UserGetTangentModulus => GetTangentModulus

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetQuadratureVariables(obj)
  CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: iel, nips, tsize
  INTEGER(I4B) :: maxnips

  tsize = 0
  maxnips = 0

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
    CALL RealVector_Initiate(obj%stress(iel), nips)
    CALL RealVector_Initiate(obj%tstrain(iel), nips)
    CALL RealVector_Initiate(obj%pstrain(iel), nips)
    CALL RealVector_Initiate(obj%pparam(iel), nips)

    CALL RealVector_Set(obj%stress0(iel), VALUE=zero)
    CALL RealVector_Set(obj%tstrain0(iel), VALUE=zero)
    CALL RealVector_Set(obj%pstrain0(iel), VALUE=zero)
    CALL RealVector_Set(obj%pparam0(iel), VALUE=zero)
    CALL RealVector_Set(obj%stress(iel), VALUE=zero)
    CALL RealVector_Set(obj%tstrain(iel), VALUE=zero)
    CALL RealVector_Set(obj%pstrain(iel), VALUE=zero)
    CALL RealVector_Set(obj%pparam(iel), VALUE=zero)

  END DO

  obj%maxNIPSpace = maxnips
  obj%totalQuadPointsForSpace = tsize

END SUBROUTINE SetQuadratureVariables

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ReturnMapping(obj, spaceElemNum, stress, stress0, &
                   tstrain, tstrain0, pstrain, pstrain0, pparam, pparam0, ans)
  CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: spaceElemNum
  REAL(DFP), INTENT(INOUT) :: stress, tstrain, pstrain, pparam
  REAL(DFP), OPTIONAL, INTENT(IN) :: stress0, tstrain0, pstrain0, pparam0
  REAL(DFP), OPTIONAL, INTENT(OUT) :: ans

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

SUBROUTINE GetTangentModulus(obj, spaceElemNum, stress, stress0, &
                   tstrain, tstrain0, pstrain, pstrain0, pparam, pparam0, ans)
  CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: spaceElemNum
  REAL(DFP), INTENT(INOUT) :: stress, tstrain, pstrain, pparam
  REAL(DFP), OPTIONAL, INTENT(IN) :: stress0, tstrain0, pstrain0, pparam0
  REAL(DFP), OPTIONAL, INTENT(OUT) :: ans

  REAL(DFP) :: eMod, pMod, sigY0, f0

  eMod = obj%elasticModulus(spaceElemNum)
  pMod = obj%plasticModulus(spaceElemNum)

  IF (pparam .GT. pparam0) THEN
    ans = eMod * pMod / (eMod + pMod)
  ELSE
    ans = eMod
  END IF

END SUBROUTINE GetTangentModulus

!----------------------------------------------------------------------------
!                                                             AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleTanmat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleTanmat()"
INTEGER(I4B) :: ielSpace, nrow, ncol, tcon, con(256)
REAL(DFP) :: dt, dt_by_2, dts, dts_by_2, dx, dx_by_2, &
             dx2, dx2_by_2, two_by_dx, xij(1, 2), scale
LOGICAL(LGT) :: isMass, isStiff, isDamp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dt = obj%timeStepSize(obj%currentTimeStep)
dt_by_2 = dt * 0.5_DFP
dts = dt * dt
dts_by_2 = dts * 0.5_DFP

isMass = .NOT. obj%algoParam%tanmat_zero(1)
isStiff = .NOT. obj%algoParam%tanmat_zero(2)
isDamp = .NOT. obj%algoParam%tanmat_zero(3)

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

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * dx_by_2 &
                           * obj%ms(1:nrow, 1:ncol)

  IF (obj%currentNRStep .EQ. 0) THEN
    CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
    obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                             two_by_dx * obj%ks(1:nrow, 1:ncol)
  END IF

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  IF (isMass) THEN
    scale = obj%algoParam%tanmat(1)
    CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ms(1:nrow, 1:ncol), &
                       scale=scale, storageFMT=DOF_FMT, nodenum=con(1:tcon))
  END IF

  IF (isDamp) THEN
    scale = obj%algoParam%tanmat(2) * dt
    CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%cs(1:nrow, 1:ncol), &
                       scale=scale, storageFMT=DOF_FMT, nodenum=con(1:tcon))
  END IF

  IF (isStiff) THEN
    scale = obj%algoParam%tanmat(3) * dts
    IF (obj%currentNRStep .EQ. 0) THEN
      CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ks(1:nrow, 1:ncol), &
                         scale=scale, storageFMT=DOF_FMT, nodenum=con(1:tcon))
    ELSE
      CALL AddKst(obj, ielSpace, nrow=nrow, ncol=ncol)
      CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ks(1:nrow, 1:ncol), &
                         scale=scale, storageFMT=DOF_FMT, nodenum=con(1:tcon))
    END IF
  END IF

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

SUBROUTINE AddKst(obj, iel, nrow, ncol)
  CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: iel
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: nips, nns, ii, &
                  aint1, aint2
  REAL(DFP) :: scale, jsp

  nips = obj%elemsdForSpace%nips

  nns = obj%elemsdForSpace%nns

  nrow = nns
  ncol = nrow

  jsp = 2.0_DFP / obj%spaceElemLength(iel)

  obj%ks = zero

  DO ii = 1, nips
    CALL obj%UserGetTangentModulus(obj=obj, &
                                   spaceElemNum=iel, &
                                   stress=obj%stress(iel)%val(ii), &
                                   stress0=obj%stress0(iel)%val(ii), &
                                   tstrain=obj%tstrain(iel)%val(ii), &
                                   tstrain0=obj%tstrain0(iel)%val(ii), &
                                   pstrain=obj%pstrain(iel)%val(ii), &
                                   pstrain0=obj%pstrain0(iel)%val(ii), &
                                   pparam=obj%pparam(iel)%val(ii), &
                                   pparam0=obj%pparam0(iel)%val(ii), &
                                   ans=scale)

    scale = scale * jsp * obj%elemsdForSpace%ws(ii) * &
            obj%elemsdForSpace%thickness(ii)
    CALL OuterProd_(a=obj%elemsdForSpace%dNdXi(1:nns, 1, ii), &
                    b=obj%elemsdForSpace%dNdXi(1:nns, 1, ii), &
                    ans=obj%ks, nrow=aint1, ncol=aint2, &
                    anscoeff=one, scale=scale)

  END DO

END SUBROUTINE AddKst

!----------------------------------------------------------------------------
!                                                               AssembleRHSF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHSF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHSF()"
#endif

INTEGER(I4B) :: con(256), ielSpace, nrow, ncol, nns, tsize

REAL(DFP) :: dx, dx_by_2, two_by_dx, dt, dts, &
             v0(MAX_ORDER_SPACE + 1), &
             u0(MAX_ORDER_SPACE + 1), &
             a0(MAX_ORDER_SPACE + 1), &
             tmp(MAX_ORDER_SPACE + 1), &
             xij(1, 2), time

INTEGER(I4B), PARAMETER :: conversion(1) = [NONE]

LOGICAL(LGT) :: isTractionLeft, isTractionRight, &
                isNonZero, isBodyForce

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL RealVector_Set(obj=obj%rhsf, VALUE=zero)

isTractionRight = ASSOCIATED(obj%tractionRight)
isTractionLeft = ASSOCIATED(obj%tractionLeft)
isBodyForce = ASSOCIATED(obj%bodyForce)

dt = obj%timeStepSize(obj%currentTimeStep)
dts = dt * dt
time = obj%currentTime + (1.0_DFP + obj%algoParam%alpha) * dt

isNonZero = .NOT. obj%algoParam%rhs_f1_zero
IF (isNonZero) THEN
  CALL RealVector_Add(obj1=obj%rhsf, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%force1, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=obj%algoParam%rhs_f1 * dts)
END IF

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * &
                           dx_by_2 * obj%ms(1:nrow, 1:ncol)

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  CALL RealVector_GetValue_(obj=obj%v0, nodenum=con(1:nns), VALUE=v0, &
                            tsize=nns)

  CALL RealVector_GetValue_(obj=obj%u0, nodenum=con(1:nns), VALUE=u0, &
                            tsize=nns)

  CALL RealVector_GetValue_(obj=obj%a0, nodenum=con(1:nns), VALUE=a0, &
                            tsize=nns)

  obj%rhse = 0.0_DFP
  tmp = 0.0_DFP
  ! mass contribution
  isNonZero = .NOT. obj%algoParam%rhs_u1_zero(1)
  IF (isNonZero) tmp = tmp + u0 * obj%algoParam%rhs_u1(1)

  isNonZero = .NOT. obj%algoParam%rhs_v1_zero(1)
  IF (isNonZero) tmp = tmp + dt * v0 * obj%algoParam%rhs_v1(1)

  isNonZero = .NOT. obj%algoParam%rhs_a1_zero(1)
  IF (isNonZero) tmp = tmp + dts * a0 * obj%algoParam%rhs_a1(1)

  obj%rhse(1:nns) = obj%rhse(1:nns) + MATMUL(obj%ms(1:nrow, 1:ncol), tmp(1:ncol))

  ! damp contribution
  tmp = 0.0_DFP
  ! mass contribution
  isNonZero = .NOT. obj%algoParam%rhs_u1_zero(2)
  IF (isNonZero) tmp = tmp + u0 * obj%algoParam%rhs_u1(2)

  isNonZero = .NOT. obj%algoParam%rhs_v1_zero(2)
  IF (isNonZero) tmp = tmp + dt * v0 * obj%algoParam%rhs_v1(2)

  isNonZero = .NOT. obj%algoParam%rhs_a1_zero(2)
  IF (isNonZero) tmp = tmp + dts * a0 * obj%algoParam%rhs_a1(2)
  obj%rhse(1:nns) = obj%rhse(1:nns) + &
                    dt * MATMUL(obj%cs(1:nrow, 1:ncol), tmp(1:ncol))

  ! stiff contribution
  ! none

  CALL RealVector_Add(obj=obj%rhsf, VALUE=obj%rhse(1:nns), &
                      scale=one, dofobj=obj%dof, nodenum=con(1:nns), &
                      conversion=conversion)

  !! Body force
  IF (isBodyForce) THEN
    CALL obj%GetBodyForce(ans=obj%rhse, tsize=tsize, spaceElemNum=ielSpace, &
                          time=time, anscoeff=zero, scale=one)

    CALL RealVector_Add(obj=obj%force2, VALUE=obj%rhse(1:tsize), &
                        scale=one, dofobj=obj%dof, nodenum=con(1:nns), &
                        conversion=conversion)
  END IF

  xij(1, 1) = xij(1, 2)

END DO

! Traction right
IF (isTractionRight) THEN
  CALL obj%GetConnectivity(spaceElemNum=obj%totalSpaceElements, ans=con, tsize=nns)
  CALL obj%GetTractionRight(ans=obj%rhse, tsize=tsize, &
                            time=time, anscoeff=zero, &
                            scale=one)

  CALL RealVector_Add(obj=obj%force2, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

END IF

! Traction left
IF (isTractionLeft) THEN
  CALL obj%GetConnectivity(spaceElemNum=1, ans=con, tsize=nns)
  CALL obj%GetTractionLeft(ans=obj%rhse, tsize=tsize, &
                           time=time, anscoeff=zero, &
                           scale=one)

  CALL RealVector_Add(obj=obj%force2, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)
END IF

isNonZero = .NOT. obj%algoParam%rhs_f2_zero
IF (isNonZero) THEN
  CALL RealVector_Add(obj1=obj%rhsf, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%force2, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=obj%algoParam%rhs_f2 * dts)
END IF

obj%currentExternalForceNorm = RealVector_Norm(obj=obj%rhsf)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleRHSF

!----------------------------------------------------------------------------
!                                                                AssembleRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"
#endif

REAL(DFP) :: sol_u(MAX_ORDER_SPACE + 1), xij(1, 2)
INTEGER(I4B), PARAMETER :: conversion(1) = [NONE]

INTEGER(I4B) :: ielSpace, nrow, ncol, nns, con(256), tsize, &
                ii, nips
REAL(DFP) :: dt, dts, dx, dx_by_2, scale, scale1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL RealVector_Set(obj=obj%rhs, VALUE=zero)

dt = obj%timeStepSize(obj%currentTimeStep)
dts = dt * dt

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)
  nips = obj%elemsdForSpace%nips

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * dx_by_2 &
                           * obj%ms(1:nrow, 1:ncol)

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  CALL RealVector_GetValue_(obj=obj%sol0, nodenum=con(1:nns), &
                            VALUE=sol_u, tsize=tsize)

  obj%rhse = zero
  scale = obj%algoParam%rhs_u1(1)
  obj%rhse(1:tsize) = scale * &
                      MATMUL(obj%ms(1:tsize, 1:tsize), sol_u(1:tsize))

  scale = obj%algoParam%rhs_u1(2) * dt
  obj%rhse(1:tsize) = obj%rhse(1:tsize) + scale * &
                      MATMUL(obj%cs(1:tsize, 1:tsize), sol_u(1:tsize))

  scale = obj%algoParam%rhs_f2 * dts
  DO ii = 1, nips

    scale1 = scale * obj%elemsdForSpace%ws(ii) * &
             obj%elemsdForSpace%thickness(ii)
    scale1 = scale1 * obj%stress(ielSpace)%Val(ii)

    obj%rhse(1:tsize) = obj%rhse(1:tsize) + &
                        scale1 * obj%elemsdForSpace%dNdXi(1:tsize, 1, ii)

  END DO

  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
                      scale=one, dofobj=obj%dof, nodenum=con(1:nns), &
                      conversion=conversion)

  xij(1, 1) = xij(1, 2)

END DO

! cal residual
CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhsf, scale=minus_one)
CALL RealVector_Scale(obj%rhs, minus_one)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!                                                                 solve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Solve
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Solve()"
#endif

INTEGER(I4B) :: n, solverName

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

n = CSRMatrix_Size(obj%tanmat, 1)
solverName = LIS_GMRES

CALL CSRMatrixLinSolveInitiate(ipar=obj%ipar, fpar=obj%fpar, W=obj%work, &
                               n=n, solverName=solverName)

CALL CSRMatrix_LinSolve(obj=obj%tanmat, sol=obj%sol%val(1:n), &
               rhs=obj%rhs%val(1:n), ipar=obj%ipar, fpar=obj%fpar, W=obj%work)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Solve

!----------------------------------------------------------------------------
!                                                           UpdateQPVariables
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateQPVariables
CHARACTER(*), PARAMETER :: myName = "obj_UpdateQPVariables()"
INTEGER(I4B) :: iel, nips, jj, nns, tsize
REAL(DFP) :: xij(1, 2), u0(MAX_ORDER_SPACE + 1), dx, twobydx
REAL(DFP) :: dstrain(2 * MAX_ORDER_SPACE + 1), &
             dstress(2 * MAX_ORDER_SPACE + 1), &
             dt
INTEGER(I4B) :: con(MAX_ORDER_SPACE + 1)

xij(1, 1) = obj%spaceDomain(1)
dt = obj%timeStepSize(obj%currentTimeStep)

! NOTE: displacement must be increment here
! not total value so obj%sol is used
CALL RealVector_Set(obj%rhs, VALUE=obj%u0)
CALL RealVector_Scale(obj%rhs, minus_one)
CALL RealVector_Add(obj%rhs, VALUE=obj%sol0, scale=one)

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

  CALL RealVector_GetValue_(obj=obj%rhs, nodenum=con(1:nns), &
                            VALUE=u0, tsize=tsize)

  dstrain(1:nips) = twobydx * MATMUL(u0(1:tsize), &
                                     obj%elemsdForSpace%dNdXi(:, 1, :))

  obj%tstrain(iel)%val(:) = obj%tstrain0(iel)%val(:) + &
                            dstrain(1:nips)

  dstress(1:nips) = obj%elasticModulus(iel) * dstrain(1:nips)

  obj%stress(iel)%val(:) = obj%stress0(iel)%val(:) + &
                           dstress(1:nips)

  DO jj = 1, nips
    CALL obj%UserReturnMapping(obj=obj, spaceElemNum=iel, &
                               stress=obj%stress(iel)%val(jj), &
                               tstrain=obj%tstrain(iel)%val(jj), &
                               pstrain=obj%pstrain(iel)%val(jj), &
                               pparam=obj%pparam(iel)%val(jj), &
                               stress0=obj%stress0(iel)%val(jj), &
                               tstrain0=obj%tstrain0(iel)%val(jj), &
                               pstrain0=obj%pstrain0(iel)%val(jj), &
                               pparam0=obj%pparam0(iel)%val(jj) &
                               )
  END DO

  xij(1, 1) = xij(1, 2)

END DO

END PROCEDURE obj_UpdateQPVariables

!----------------------------------------------------------------------------
!                                                                   Update
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Update
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Update()"
#endif

INTEGER(I4B) :: iel
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%converged) THEN

  CALL RealVector_Set(obj%sol, obj%sol0)

  CALL Abstract1DSDUpdate(obj)

  DO iel = 1, obj%totalSpaceElements

    CALL RealVector_Set(obj%tstrain0(iel), &
                        VALUE=obj%tstrain(iel)%val(:))
    CALL RealVector_Set(obj%stress0(iel), &
                        VALUE=obj%stress(iel)%val(:))
    CALL RealVector_Set(obj%pstrain0(iel), &
                        VALUE=obj%pstrain(iel)%val(:))
    CALL RealVector_Set(obj%pparam0(iel), &
                        VALUE=obj%pparam(iel)%val(:))

  END DO

ELSE

  ! displacement update
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
!                                                                 WriteData
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

CALL Abstract1DSDWriteData(obj)

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
!                                                                    Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Run
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Run()"
#endif

INTEGER(I4B) :: iTime, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetInitialVelocity()
CALL obj%SetInitialDisplacement()
CALL obj%SetInitialAcceleration()

CALL obj%writeData()

DO iTime = 1, obj%totalTimeSteps
  obj%converged = .FALSE.
  obj%currentNRStep = 0
  CALL Display(obj%currentTime, myname//" current time: ")
  CALL obj%AssembleTanmat()
  CALL obj%AssembleRHSF()
  CALL obj%AssembleRHS()
  CALL Abstract1DSDApplyDirichletBC(obj)
  CALL obj%Solve()
  CALL obj%Update()

  DO ii = 1, obj%maxIterNumNR
    CALL obj%AssembleRHS()
    CALL ApplyDirichletBC_Residual(obj=obj)
    CALL CheckConvergence(obj)

    IF (obj%converged) THEN
      CALL Display(obj%currentNRStep, " Current NR step :: ")
      CALL Display(obj%currentResidualNorm, &
                   "NR Converged ; final norm :: ")
      obj%NRConvergedSteps(iTime) = obj%currentNRStep
      CALL obj%Update()
      EXIT
    ELSE
      CALL Display(obj%currentNRStep, " Current NR step :: ")
      CALL Display(obj%currentResidualNorm, &
                   "NR Continue; current norm :: ")
      obj%currentNRStep = ii
      IF (obj%updateTanmat) THEN
        CALL Display("Tangent matrix is updated")
        CALL obj%AssembleTanmat()
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

  IF (MOD(iTime, obj%outputFreq) .EQ. 0_I4B) &
    CALL obj%WriteData()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Run

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ApplyDirichletBC_Residual(obj)
  CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  LOGICAL(LGT) :: isDirichletLeft, isDirichletRight
  INTEGER(I4B) :: tsize, tsize_dbc_value

  isDirichletLeft = ASSOCIATED(obj%displacementLeft)
  isDirichletRight = ASSOCIATED(obj%displacementRight)
  tsize_dbc_value = 0
  obj%dbc_coeff = zero

  IF (isDirichletLeft) THEN
    CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof, tsize=tsize, &
                       nodenum=1)
    tsize_dbc_value = tsize_dbc_value + 1
  END IF

  IF (isDirichletRight) THEN
    CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof(tsize_dbc_value + 1:), &
                       tsize=tsize, &
                       nodenum=obj%totalSpaceNodes)
    tsize_dbc_value = tsize_dbc_value + 1
  END IF

  IF (tsize_dbc_value .NE. 0) THEN
   CALL RealVector_Set(obj=obj%rhs, nodenum=obj%dbc_idof(1:tsize_dbc_value), &
                        VALUE=obj%dbc_coeff(1:tsize_dbc_value))

  END IF

END SUBROUTINE ApplyDirichletBC_Residual

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ApplyDirichletBC_Tanmat(obj)
  CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj
  LOGICAL(LGT) :: isDirichletLeft, isDirichletRight
  INTEGER(I4B) :: tsize, tsize_dbc_value

  isDirichletLeft = ASSOCIATED(obj%displacementLeft)
  isDirichletRight = ASSOCIATED(obj%displacementRight)
  tsize_dbc_value = 0

  IF (isDirichletLeft) THEN
    CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof, tsize=tsize, &
                       nodenum=1)
    tsize_dbc_value = tsize_dbc_value + 1
  END IF

  IF (isDirichletRight) THEN
    CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof(tsize_dbc_value + 1:), &
                       tsize=tsize, &
                       nodenum=obj%totalSpaceNodes)
    tsize_dbc_value = tsize_dbc_value + 1
  END IF

  IF (tsize_dbc_value .NE. 0) THEN
    CALL CSRMatrix_ApplyDBC(obj=obj%tanmat, &
                            dbcptrs=obj%dbc_idof(1:tsize_dbc_value))

    CALL RealVector_Set(obj=obj%sol, VALUE=0.0_DFP)
  END IF

END SUBROUTINE ApplyDirichletBC_Tanmat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CheckConvergence(obj)
  CLASS(ElastoPlasticDynamics1DSDFEM_), INTENT(INOUT) :: obj

  obj%currentResidualNorm = RealVector_Norm(obj=obj%rhs)

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
