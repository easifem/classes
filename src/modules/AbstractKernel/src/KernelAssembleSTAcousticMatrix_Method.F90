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

MODULE KernelAssembleSTAcousticMatrix_Method
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
PUBLIC :: KernelAssembleSTAcousticMatrix

CHARACTER(*), PARAMETER :: modName =  &
  & "KernelAssembleSTAcousticMatrix_Method"

INTERFACE KernelAssembleSTAcousticMatrix
  MODULE PROCEDURE KernelAssembleSTAcousticMatrix1
END INTERFACE KernelAssembleSTAcousticMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                 KernelAssembleIsoDampMat
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleSTAcousticMatrix1(tanmat, massMat, &
  & diffMat, massDensity, scalarCoefficient, dom, cellFE, geoCellFE, &
  & spaceElemSD, geoSpaceElemSD, reset, mt, kt)

  CLASS(AbstractMatrixField_), INTENT(INOUT) :: tanmat
  CLASS(MatrixField_), INTENT(INOUT) :: massMat
  CLASS(MatrixField_), INTENT(INOUT) :: diffMat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
 CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: scalarCoefficient(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: geoCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: geoSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: mt(:, :)
  REAL(DFP), INTENT(IN) :: kt(:, :)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleSTAcousticMatrix1()"
  INTEGER(I4B) :: id, tmesh, nsd, nns, iel, nnt, indx4(4), nns_nnt
  TYPE(ElemShapeData_) :: elemsd, geoElemSD
  TYPE(FEVariable_) :: rhoVar, scaCoefVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: massDensityField,  &
    & scalarCoefficientField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: Mmat, xij, Amat, Bmat
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: isok, problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) THEN
    CALL tanmat%Set(VALUE=0.0_DFP)
    CALL massMat%Set(VALUE=0.0_DFP)
    CALL diffMat%Set(VALUE=0.0_DFP)
  END IF

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  indx4(1:2) = SHAPE(mt); indx4(3:4) = SHAPE(kt)

  CALL Assert(indx4,  &
    & msg="Shape of mt and kt should be the same.",  &
    & file=__FILE__,  &
    & line=__LINE__,  &
    & routine=myName)

  nnt = SIZE(mt, 1)

  NULLIFY (meshptr, spaceFE, linSpaceFE, scalarCoefficientField,  &
    & refelem, massDensityField)

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

    nns_nnt = nns * nnt

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Amat, nns, nns)
    CALL Reallocate(Bmat, nns, nns)
    CALL Reallocate(Mmat, nns_nnt, nns_nnt)

    massDensityField => massDensity(id)%ptr
    scalarCoefficientField => scalarCoefficient(id)%ptr

#ifdef DEBUG_VER
    isok = ASSOCIATED(massDensityField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: massDensity('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(scalarCoefficientField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
       & '[INTERNAL ERROR] :: scalarCoefficient('//tostring(id)//') is NULL.')
      RETURN
    END IF
#endif

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      CALL massDensityField%Get(fevar=rhoVar, globalElement=iel)
      CALL scalarCoefficientField%Get(fevar=scaCoefVar, globalElement=iel)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=geoElemSD)

      Amat = MassMatrix(test=elemsd, trial=elemsd, opt=1_I4B,  &
        & rho=rhoVar, rhorank=TypeFEVariableScalar)
      Bmat = DiffusionMatrix(test=elemsd, trial=elemsd,  &
        & k=scaCoefVar, krank=TypeFEVariableScalar)

      CALL SpaceTimeOuterProd(tanmat=Mmat, ms=Amat, mt=mt,  &
        & ks=Bmat, kt=kt, nns=nns, nnt=nnt)

      CALL massMat%Set(globalNode=nptrs, VALUE=Amat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      CALL diffMat%Set(globalNode=nptrs, VALUE=Bmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      CALL tanmat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  NULLIFY (meshptr, spaceFE, scalarCoefficientField, massDensityField,  &
    & refelem, linSpaceFE)

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(Amat)) DEALLOCATE (Amat)
  IF (ALLOCATED(Bmat)) DEALLOCATE (Bmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (geoElemSD)
  CALL DEALLOCATE (rhoVar)
  CALL DEALLOCATE (scaCoefVar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleSTAcousticMatrix1

!----------------------------------------------------------------------------
!                                                        SpaceTimeOuterProd
!----------------------------------------------------------------------------

SUBROUTINE SpaceTimeOuterProd(tanmat, ms, mt, ks, kt, nns, nnt)
  REAL(DFP), INTENT(INOUT) :: tanmat(:, :)
  REAL(DFP), INTENT(IN) :: ms(:, :)
  REAL(DFP), INTENT(IN) :: mt(:, :)
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
      tanmat(r1:r2, c1:c2) = mt(a, b) * ms + kt(a, b) * ks
    END DO
  END DO

END SUBROUTINE SpaceTimeOuterProd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE KernelAssembleSTAcousticMatrix_Method
