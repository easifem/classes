! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

MODULE KernelAssembleSTElastoDynaMatrix_Method
USE GlobalData
USE Field
USE BaseMethod
USE BaseType
USE FieldFactory
USE Domain_Class
USE Mesh_Class
USE FiniteElement_Class
USE ExceptionHandler_Class, ONLY: e
USE AbstractKernelParam, ONLY: KernelProblemType
IMPLICIT NONE

PRIVATE
PUBLIC :: KernelAssembleSTElastoDynaMatrix

CHARACTER(*), PARAMETER :: modName =  &
  & "KernelAssembleSTElastoDynaMatrix_Method"

INTERFACE KernelAssembleSTElastoDynaMatrix
  MODULE PROCEDURE KernelAssembleSTElastoDynaMatrix1
  MODULE PROCEDURE KernelAssembleSTElastoDynaMatrix2
END INTERFACE KernelAssembleSTElastoDynaMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                 KernelAssembleIsoDampMat
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleSTElastoDynaMatrix1(tanmat, massMat, dampMat, &
  & stiffMat, massDensity, youngsModulus, shearModulus, dampCoeff_alpha, &
  & dampCoeff_beta, dom, cellFE, geoCellFE, &
  & spaceElemSD, geoSpaceElemSD, reset, mt, ct, kt)

  CLASS(AbstractMatrixField_), INTENT(INOUT) :: tanmat
  CLASS(MatrixField_), INTENT(INOUT) :: massMat
  CLASS(MatrixField_), INTENT(INOUT) :: dampMat
  CLASS(MatrixField_), INTENT(INOUT) :: stiffMat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: youngsModulus(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: shearModulus(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: dampCoeff_alpha(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: dampCoeff_beta(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: geoCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: geoSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: mt(:, :)
  REAL(DFP), INTENT(IN) :: ct(:, :)
  REAL(DFP), INTENT(IN) :: kt(:, :)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleSTElastoDynaMatrix1()"
  INTEGER(I4B) :: id, tmesh, nsd, nns, iel, nnt, indx6(6), nsd_nns,  &
  & nsd_nns_nnt
  TYPE(ElemShapeData_) :: elemsd, geoElemSD
  TYPE(FEVariable_) :: muVar, youngsVar, alphaVar, betaVar, rhoVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: youngsModulusField,  &
    & shearModulusField, massDensityField, alphaField, betaField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: Mmat, xij, Amat, Bmat, Cmat
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: isok, problem
  REAL(DFP) :: alpha, beta

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) THEN
    CALL tanmat%Set(VALUE=0.0_DFP)
    CALL massMat%Set(VALUE=0.0_DFP)
    CALL dampMat%Set(VALUE=0.0_DFP)
    CALL stiffMat%Set(VALUE=0.0_DFP)
  END IF

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  indx6(1:2) = SHAPE(mt); indx6(3:4) = SHAPE(ct); indx6(5:6) = SHAPE(kt)

  CALL Assert(indx6,  &
    & msg="Shape of mt, ct, and kt should be the same.",  &
    & file=__FILE__,  &
    & line=__LINE__,  &
    & routine=myName)

  nnt = SIZE(mt, 1)

  NULLIFY (meshptr, spaceFE, linSpaceFE, youngsModulusField,  &
    & shearModulusField, refelem, massDensityField, alphaField, betaField)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)

    isok = ASSOCIATED(meshptr)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: Null mesh found at id = '//tostring(id))
      RETURN
    END IF

    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => geoCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    geoElemSD = geoSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    nsd_nns = nsd * nns
    nsd_nns_nnt = nsd_nns * nnt

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Amat, nsd_nns, nsd_nns)
    CALL Reallocate(Bmat, nsd_nns, nsd_nns)
    CALL Reallocate(Cmat, nsd_nns, nsd_nns)
    CALL Reallocate(Mmat, nsd_nns_nnt, nsd_nns_nnt)

    youngsModulusField => youngsModulus(id)%ptr
    shearModulusField => shearModulus(id)%ptr
    massDensityField => massDensity(id)%ptr
    alphaField => dampCoeff_alpha(id)%ptr
    betaField => dampCoeff_beta(id)%ptr

#ifdef DEBUG_VER
    isok = ASSOCIATED(youngsModulusField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: youngsModulus('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(shearModulusField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: shearModulus('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(massDensityField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: massDensity('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(alphaField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: dampCoeff_alpha('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(betaField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: dampCoeff_beta('//tostring(id)//') is NULL.')
      RETURN
    END IF
#endif

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      CALL youngsModulusField%Get(fevar=youngsVar, globalElement=iel)
      CALL shearModulusField%Get(fevar=muVar, globalElement=iel)
      CALL alphaField%Get(fevar=alphaVar, globalElement=iel)
      CALL betaField%Get(fevar=betaVar, globalElement=iel)
      CALL massDensityField%Get(fevar=rhoVar, globalElement=iel)

      alpha = Get(obj=alphaVar, rank=TypeFEVariableScalar,  &
        & vartype=TypeFEVariableConstant)

      beta = Get(obj=betaVar, rank=TypeFEVariableScalar,  &
        & vartype=TypeFEVariableConstant)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=geoElemSD)

      Amat = MassMatrix(test=elemsd, trial=elemsd, opt=nsd,  &
        & rho=rhoVar, rhorank=TypeFEVariableScalar)
      Bmat = StiffnessMatrix(test=elemsd, trial=elemsd, &
        & lambda=youngsVar, mu=muVar, isLambdaYoungsModulus=.TRUE.)
      Cmat = alpha * Amat + beta * Bmat

      CALL SpaceTimeOuterProd(tanmat=Mmat, ms=Amat, mt=mt,  &
        & cs=Cmat, ct=ct, ks=Bmat, kt=kt, nns=nsd_nns, nnt=nnt)

      CALL massMat%Set(globalNode=nptrs, VALUE=Amat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      CALL stiffMat%Set(globalNode=nptrs, VALUE=Bmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      CALL dampMat%Set(globalNode=nptrs, VALUE=Cmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      CALL tanmat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  NULLIFY (meshptr, spaceFE, youngsModulusField, shearModulusField,  &
    & refelem, linSpaceFE, alphaField, betaField, massDensityField)

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(Amat)) DEALLOCATE (Amat)
  IF (ALLOCATED(Bmat)) DEALLOCATE (Bmat)
  IF (ALLOCATED(Cmat)) DEALLOCATE (Cmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (geoElemSD)
  CALL DEALLOCATE (youngsVar)
  CALL DEALLOCATE (muVar)
  CALL DEALLOCATE (rhoVar)
  CALL DEALLOCATE (alphaVar)
  CALL DEALLOCATE (betaVar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleSTElastoDynaMatrix1

!----------------------------------------------------------------------------
!                                           KernelAssembleElastoDynaMatrix2
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleSTElastoDynaMatrix2(tanmat, massMat, dampMat,  &
  & stiffMat, massDensity, Cijkl, dampCoeff_alpha, dampCoeff_beta, dom,  &
  & cellFE, geoCellFE, spaceElemSD, geoSpaceElemSD, reset,  &
  & mt, ct, kt)

  CLASS(AbstractMatrixField_), INTENT(INOUT) :: tanmat
  CLASS(MatrixField_), INTENT(INOUT) :: massMat
  CLASS(MatrixField_), INTENT(INOUT) :: dampMat
  CLASS(MatrixField_), INTENT(INOUT) :: stiffMat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
  CLASS(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: Cijkl(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: dampCoeff_alpha(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: dampCoeff_beta(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: geoCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: geoSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: mt(:, :)
  REAL(DFP), INTENT(IN) :: ct(:, :)
  REAL(DFP), INTENT(IN) :: kt(:, :)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleElastoDynaMatrix2()"
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel, nsd_nns,  &
  & nsd_nns_nnt, indx6(6), nnt
  TYPE(ElemShapeData_) :: elemsd, geoElemSD
  TYPE(FEVariable_) :: CijklVar, alphaVar, betaVar, rhoVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractTensorMeshField_), POINTER :: CijklField
  CLASS(AbstractScalarMeshField_), POINTER :: massDensityField,  &
    & alphaField, betaField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: Mmat, xij, Amat, Bmat, Cmat
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: isok, problem
  REAL(DFP) :: alpha, beta

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) THEN
    CALL tanmat%Set(VALUE=0.0_DFP)
    CALL massMat%Set(VALUE=0.0_DFP)
    CALL dampMat%Set(VALUE=0.0_DFP)
    CALL stiffMat%Set(VALUE=0.0_DFP)
  END IF

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  indx6(1:2) = SHAPE(mt); indx6(3:4) = SHAPE(ct); indx6(5:6) = SHAPE(kt)

  CALL Assert(indx6,  &
    & msg="Shape of mt, ct, and kt should be the same.",  &
    & file=__FILE__,  &
    & line=__LINE__,  &
    & routine=myName)

  nnt = SIZE(mt, 1)

  NULLIFY (meshptr, spaceFE, CijklField, refelem, linSpaceFE,  &
    & massDensityField, alphaField, betaField)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)

    isok = ASSOCIATED(meshptr)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: Null mesh found at id = '//tostring(id))
      RETURN
    END IF

    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    telems = meshptr%GetTotalElements()

    spaceFE => cellFE(id)%ptr
    linSpaceFE => geoCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    geoElemSD = geoSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    nsd_nns = nsd * nns
    nsd_nns_nnt = nsd_nns * nnt

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Amat, nsd_nns, nsd_nns)
    CALL Reallocate(Bmat, nsd_nns, nsd_nns)
    CALL Reallocate(Cmat, nsd_nns, nsd_nns)
    CALL Reallocate(Mmat, nsd_nns_nnt, nsd_nns_nnt)

    CijklField => Cijkl(id)%ptr
    massDensityField => massDensity(id)%ptr
    alphaField => dampCoeff_alpha(id)%ptr
    betaField => dampCoeff_beta(id)%ptr

#ifdef DEBUG_VER
    isok = ASSOCIATED(CijklField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] ::Cijkl('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(massDensityField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: massDensity('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(alphaField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: dampCoeff_alpha('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(betaField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: dampCoeff_beta('//tostring(id)//') is NULL.')
      RETURN
    END IF
#endif

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      CALL CijklField%Get(fevar=CijklVar, globalElement=iel)
      CALL alphaField%Get(fevar=alphaVar, globalElement=iel)
      CALL betaField%Get(fevar=betaVar, globalElement=iel)
      CALL massDensityField%Get(fevar=rhoVar, globalElement=iel)

      alpha = Get(obj=alphaVar, rank=TypeFEVariableScalar,  &
        & vartype=TypeFEVariableConstant)

      beta = Get(obj=betaVar, rank=TypeFEVariableScalar,  &
        & vartype=TypeFEVariableConstant)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=geoElemSD)

      Amat = MassMatrix(test=elemsd, trial=elemsd, opt=tdof,  &
        & rho=rhoVar, rhorank=TypeFEVariableScalar)
      Bmat = StiffnessMatrix(test=elemsd, trial=elemsd, Cijkl=CijklVar)
      Cmat = alpha * Amat + beta * Bmat

      CALL SpaceTimeOuterProd(tanmat=Mmat, ms=Amat, mt=mt,  &
        & cs=Cmat, ct=ct, ks=Bmat, kt=kt, nns=nsd_nns, nnt=nnt)

      CALL massMat%Set(globalNode=nptrs, VALUE=Amat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      CALL stiffMat%Set(globalNode=nptrs, VALUE=Bmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      CALL dampMat%Set(globalNode=nptrs, VALUE=Cmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      CALL tanmat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  IF (ALLOCATED(Amat)) DEALLOCATE (Amat)
  IF (ALLOCATED(Bmat)) DEALLOCATE (Bmat)
  IF (ALLOCATED(Cmat)) DEALLOCATE (Cmat)
  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (geoElemSD)
  CALL DEALLOCATE (CijklVar)
  CALL DEALLOCATE (rhoVar)
  CALL DEALLOCATE (alphaVar)
  CALL DEALLOCATE (betaVar)

  NULLIFY (meshptr, spaceFE, CijklField, refelem, linSpaceFE,  &
    & massDensityField, alphaField, betaField)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleSTElastoDynaMatrix2

!----------------------------------------------------------------------------
!                                                        SpaceTimeOuterProd
!----------------------------------------------------------------------------

SUBROUTINE SpaceTimeOuterProd(tanmat, ms, mt, cs, ct, ks, kt, nns, nnt)
  REAL(DFP), INTENT(INOUT) :: tanmat(:, :)
  REAL(DFP), INTENT(IN) :: ms(:, :)
  REAL(DFP), INTENT(IN) :: mt(:, :)
  REAL(DFP), INTENT(IN) :: cs(:, :)
  REAL(DFP), INTENT(IN) :: ct(:, :)
  REAL(DFP), INTENT(IN) :: ks(:, :)
  REAL(DFP), INTENT(IN) :: kt(:, :)
  INTEGER(I4B), INTENT(IN) :: nns
  INTEGER(I4B), INTENT(IN) :: nnt

  INTEGER(I4B) :: a, b, r1, r2, c1, c2

  DO b = 1, nnt
    c1 = (b - 1) * nns + 1
    c2 = b * nns
    DO a = 1, nnt
      r1 = (a - 1) * nns + 1
      r2 = a * nns
      tanmat(r1:r2, c1:c2) = mt(a, b) * ms + ct(a, b) * cs + kt(a, b) * ks
    END DO
  END DO

END SUBROUTINE SpaceTimeOuterProd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE KernelAssembleSTElastoDynaMatrix_Method
