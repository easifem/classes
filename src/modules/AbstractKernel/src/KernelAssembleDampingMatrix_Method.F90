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

MODULE KernelAssembleDampingMatrix_Method
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
PUBLIC :: KernelAssembleDampingMatrix

CHARACTER(*), PARAMETER :: modName = "KernelAssembleDampingMatrix_Method"

INTERFACE KernelAssembleDampingMatrix
  MODULE PROCEDURE KernelAssembleDampingMatrix1
  MODULE PROCEDURE KernelAssembleDampingMatrix2
END INTERFACE KernelAssembleDampingMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                 KernelAssembleIsoDampMat
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleDampingMatrix1(mat, massDensity, youngsModulus,  &
  & shearModulus, dampCoeff_alpha, dampCoeff_beta, dom, cellFE, linCellFE,  &
  & spaceElemSD, linSpaceElemSD, reset)

  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: youngsModulus(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: shearModulus(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: dampCoeff_alpha(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: dampCoeff_beta(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleDampingMatrix1()"
  INTEGER(I4B) :: id, tmesh, nsd, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  TYPE(FEVariable_) :: muVar, youngsVar, alphaVar, betaVar, rhoVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: youngsModulusField,  &
    & shearModulusField, massDensityField, alphaField, betaField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :), Amat(:, :), Bmat(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: isok, problem
  REAL(DFP) :: alpha, beta

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)
  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  tdof = nsd

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
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)
    CALL Reallocate(Amat, tdof * nns, tdof * nns)
    CALL Reallocate(Bmat, tdof * nns, tdof * nns)

    youngsModulusField => youngsModulus(id)%ptr
    shearModulusField => shearModulus(id)%ptr
    massDensityField => massDensity(id)%ptr
    alphaField => dampCoeff_alpha(id)%ptr
    betaField => dampCoeff_beta(id)%ptr

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
        & geoElemSD=linElemSD)

      Amat = MassMatrix(test=elemsd, trial=elemsd, opt=tdof,  &
        & rho=rhoVar, rhorank=TypeFEVariableScalar)
      Bmat = StiffnessMatrix(test=elemsd, trial=elemsd, &
        & lambda=youngsVar, mu=muVar, isLambdaYoungsModulus=.TRUE.)
      Mmat(:, :) = alpha * Amat + beta * Bmat

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  NULLIFY (meshptr, spaceFE, youngsModulusField, shearModulusField,  &
    & refelem, linSpaceFE, alphaField, betaField, massDensityField)

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(Amat)) DEALLOCATE (Amat)
  IF (ALLOCATED(Bmat)) DEALLOCATE (Bmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)
  CALL DEALLOCATE (youngsVar)
  CALL DEALLOCATE (muVar)
  CALL DEALLOCATE (rhoVar)
  CALL DEALLOCATE (alphaVar)
  CALL DEALLOCATE (betaVar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleDampingMatrix1

!----------------------------------------------------------------------------
!                                              KernelAssembleDampingMatrix2
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleDampingMatrix2(mat, massDensity, Cijkl, &
    & dampCoeff_alpha, dampCoeff_beta, dom, cellFE, linCellFE,  &
    & spaceElemSD, linSpaceElemSD, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
  CLASS(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: Cijkl(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: dampCoeff_alpha(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: dampCoeff_beta(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleDampingMatrix2()"
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  TYPE(FEVariable_) :: CijklVar, alphaVar, betaVar, rhoVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractTensorMeshField_), POINTER :: CijklField
  CLASS(AbstractScalarMeshField_), POINTER :: massDensityField,  &
    & alphaField, betaField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :), Amat(:, :), Bmat(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: isok, problem
  REAL(DFP) :: alpha, beta

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)
  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  tdof = nsd

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
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)
    CALL Reallocate(Amat, tdof * nns, tdof * nns)
    CALL Reallocate(Bmat, tdof * nns, tdof * nns)

    CijklField => Cijkl(id)%ptr
    massDensityField => massDensity(id)%ptr
    alphaField => dampCoeff_alpha(id)%ptr
    betaField => dampCoeff_beta(id)%ptr

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

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,   &
        & geoElemSD=linElemSD)

      Amat = MassMatrix(test=elemsd, trial=elemsd, opt=tdof,  &
        & rho=rhoVar, rhorank=TypeFEVariableScalar)
      Bmat = StiffnessMatrix(test=elemsd, trial=elemsd, Cijkl=CijklVar)
      Mmat(:, :) = alpha * Amat + beta * Bmat

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(Amat)) DEALLOCATE (Amat)
  IF (ALLOCATED(Bmat)) DEALLOCATE (Bmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)
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
END SUBROUTINE KernelAssembleDampingMatrix2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE KernelAssembleDampingMatrix_Method
