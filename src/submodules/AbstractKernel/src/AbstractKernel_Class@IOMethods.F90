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

SUBMODULE(AbstractKernel_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             KernelDisplay
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: bool1
INTEGER(I4B) :: aint, ii

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInitiated, "Kernel initiated: ", unitNo=unitNo)
IF (.NOT. obj%isInitiated) RETURN
CALL Display(obj%isConstantMatProp, "isConstantMatProp: ", unitNo=unitNo)
CALL Display(obj%isCommonDomain, "isCommonDomain: ", unitNo=unitNo)
CALL Display(obj%isIncompressible, "isIncompressible: ", unitNo=unitNo)
CALL Display(obj%isMaterialInterfaces, "isMaterialInterfaces: ", &
  & unitNo=unitNo)
CALL Display(obj%isIsotropic, "isIsotropic: ", unitNo=unitNo)
CALL Display(obj%isNitsche, "isNitsche: ", unitNo=unitNo)
CALL Display(obj%problemType, "problemType: ", unitNo=unitNo)
CALL Display(obj%tOverlappedMaterials,  &
  & "tOverlappedMaterials: ", unitNo=unitNo)
CALL Display(obj%tSolidMaterials, "tSolidMaterials: ", unitNo=unitNo)
CALL Display(obj%SOLID_MATERIAL_ID, "SOLID_MATERIAL_ID: ", unitNo=unitNo)
CALL Display(obj%algorithm, "algorithm: ", unitNo=unitNo)
CALL Display(obj%name, "name: ", unitno=unitno)
CALL Display(obj%engine, "engine: ", unitno=unitno)
CALL Display(obj%tanmatProp, "tanmatProp: ", unitno=unitno)
CALL Display(obj%outputPath, "outputPath: ", unitno=unitno)
CALL Display(obj%coordinateSystem, "coordinateSystem: ", unitno=unitno)
CALL Display(obj%maxIter, "maxIter: ", unitno=unitno)
CALL Display(obj%timeDependency, "timeDependency: ", unitno=unitno)
CALL Display(obj%nsd, "nsd: ", unitno=unitno)
CALL Display(obj%nnt, "nnt: ", unitno=unitno)
CALL Display(obj%tdof, "tdof: ", unitno=unitno)
IF (ALLOCATED(obj%timeVec)) THEN
  CALL Display(obj%timeVec, "timeVec: ", unitno=unitno)
END IF
CALL Display(obj%normRHS, "normRHS: ", unitno=unitno)
CALL Display(obj%dt, "dt: ", unitno=unitno)
CALL Display(obj%startTime, "startTime: ", unitno=unitno)
CALL Display(obj%endTime, "endTime: ", unitno=unitno)
CALL Display(obj%currentTime, "currentTime: ", unitno=unitno)
CALL Display(obj%currentTimeStep, "currentTimeStep: ", unitno=unitno)
CALL Display(obj%totalTimeStep, "totalTimeStep: ", unitno=unitno)
CALL Display(obj%lengthScale, "lengthScale: ", unitno=unitno)
CALL Display(obj%postProcessOpt, "postProcessOpt: ", unitno=unitno)
CALL Display(obj%gravity, "gravity: ", unitno=unitno)
CALL Display(obj%nitscheAlpha, "nitscheAlpha: ", unitno=unitno)
IF (INT(obj%NitscheType, kind=I4B) .EQ. Nitsche_Sym) THEN
  CALL Display("NitscheType: SYM", unitno=unitno)
ELSE
  CALL Display("NitscheType: UNSYM", unitno=unitno)
END IF
CALL Display(obj%incrementScale, "incrementScale : ", unitNo=unitNo)
CALL Display(obj%rtoleranceForDisplacement, "rtoleranceForDisplacement: ", &
  & unitNo=unitNo)
CALL Display(obj%atoleranceForDisplacement, "atoleranceForDisplacement: ", &
  & unitNo=unitNo)
CALL Display(obj%rtoleranceForVelocity, "rtoleranceForVelocity: ", &
  & unitNo=unitNo)
CALL Display(obj%atoleranceForVelocity, "atoleranceForVelocity: ", &
  & unitNo=unitNo)
CALL Display(obj%rtoleranceForResidual, "rtoleranceForResidual: ", &
  & unitNo=unitNo)
CALL Display(obj%atoleranceForResidual, "atoleranceForResidual: ", &
  & unitNo=unitNo)
CALL Display(obj%displacementError0, "displacementError0: ", unitNo=unitNo)
CALL Display(obj%displacementError, "displacementError: ", unitNo=unitNo)
CALL Display(obj%velocityError0, "velocityError0: ", unitNo=unitNo)
CALL Display(obj%velocityError, "velocityError: ", unitNo=unitNo)
CALL Display(obj%residualError0, "residualError0: ", unitNo=unitNo)
CALL Display(obj%residualError, "residualError: ", unitNo=unitNo)
CALL Display(obj%iterData, "iterData: ", unitno=unitno)
IF (obj%isMaterialInterfaces) THEN
  CALL Display(obj%materialInterfaces, 'materialInterfaces', &
    & unitNo=unitNo)
END IF
bool1 = ALLOCATED(obj%elemToMatId)
CALL Display(bool1, "elemToMatID ALLOCATED: ", unitno=unitno)
bool1 = ALLOCATED(obj%dbcIndx)
CALL Display(bool1, "dbcIndx ALLOCATED: ", unitno=unitno)
bool1 = ASSOCIATED(obj%linsol)
CALL Display(bool1, "linsol ASSOCIATED: ", unitno=unitno)
bool1 = ASSOCIATED(obj%tanmat)
CALL Display(bool1, "tanmat ASSOCIATED: ", unitno=unitno)
IF (bool1) THEN
  CALL obj%tanmat%Display("tanmat: ", unitNo=unitNo)
END IF

CALL Display(obj%domainFile, "domainFile: ", unitNo=unitNo)
bool1 = ASSOCIATED(obj%dom)
CALL Display(bool1, "dom ASSOCIATED: ", unitNo=unitNo)
bool1 = ALLOCATED(obj%domains)
CALL Display(bool1, "domains ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%matIfaceConnectData)
CALL Display(bool1, "matIfaceConnectData ALLOCATED: ", unitNo=unitNo)

CALL Display(obj%baseContinuityForSpace, "baseContinuityForSpace: ", &
  & unitNo=unitNo)

CALL Display(obj%baseInterpolationForSpace, "baseInterpolationForSpace: ", &
  & unitNo=unitNo)

CALL Display(obj%quadratureTypeForSpace, "quadratureTypeForSpace: ", &
  & unitNo=unitNo)

CALL Display(obj%quadTypeForSpace, "quadTypeForSpace: ", &
  & unitNo=unitNo)

CALL Display(obj%ipTypeForSpace, "ipTypeForSpace: ", &
  & unitNo=unitNo)

CALL Display(obj%basisTypeForSpace, "basisTypeForSpace: ", &
  & unitNo=unitNo)

CALL Display(obj%alphaForSpace, "alphaForSpace: ", &
  & unitNo=unitNo)

CALL Display(obj%betaForSpace, "betaForSpace: ", &
  & unitNo=unitNo)

CALL Display(obj%lambdaForSpace, "lambdaForSpace: ", &
  & unitNo=unitNo)

bool1 = ALLOCATED(obj%quadratureForSpace)
CALL Display(bool1, "quadratureForSpace ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%quadratureForSpace_facet)
CALL Display(bool1, "quadratureForSpace_facet ALLOCATED: ", unitNo=unitNo)

CALL Display(obj%baseContinuityForTime, "baseContinuityForTime: ", &
  & unitNo=unitNo)

CALL Display(obj%baseInterpolationForTime, "baseInterpolationForTime: ", &
  & unitNo=unitNo)

CALL Display(obj%quadratureTypeForTime, "quadratureTypeForTime: ",  &
  & unitNo=unitNo)

CALL Display(obj%quadTypeForTime, "quadTypeForTime: ", unitNo=unitNo)

CALL Display(obj%ipTypeForTime, "ipTypeForTime: ", unitNo=unitNo)

CALL Display(obj%basisTypeForTime, "basisTypeForTime: ", unitNo=unitNo)

CALL Display(obj%alphaForTime, "alphaForTime: ", unitNo=unitNo)

CALL Display(obj%betaForTime, "betaForTime: ", unitNo=unitNo)

CALL Display(obj%lambdaForTime, "lambdaForTime: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%cellFE)
CALL Display(bool1, "cellFE ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%linCellFE)
CALL Display(bool1, "linCellFE ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%facetFE)
CALL Display(bool1, "facetFE ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%linFacetFE)
CALL Display(bool1, "linFacetFE ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%edgeFE)
CALL Display(bool1, "edgeFE ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%linEdgeFE)
CALL Display(bool1, "linEdgeFE ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%spaceElemSD)
CALL Display(bool1, "spaceElemSD ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%linSpaceElemSD)
CALL Display(bool1, "linSpaceElemSD ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%spaceElemSD_facet)
CALL Display(bool1, "spaceElemSD_facet ALLOCATED: ", unitNo=unitNo)

bool1 = ALLOCATED(obj%linSpaceElemSD_facet)
CALL Display(bool1, "linSpaceElemSD_facet ALLOCATED: ", unitNo=unitNo)

! dbc
bool1 = ALLOCATED(obj%dbc)
CALL Display(bool1, "dbc ALLOCATED: ", unitNo=unitNo)
IF (bool1) THEN
  CALL DirichletBCDisplay(obj%dbc, "dbc: ", unitNo=unitNo)
END IF

! nbc
bool1 = ALLOCATED(obj%nbc)
CALL Display(bool1, "nbc ALLOCATED: ", unitNo=unitNo)
IF (bool1) THEN
  CALL NeumannBCDisplay(obj%nbc, "nbc: ", unitNo=unitNo)
END IF

! nbcPointSource
bool1 = ALLOCATED(obj%nbcPointSource)
CALL Display(bool1, "nbcPointSource ALLOCATED: ", unitNo=unitNo)
IF (bool1) THEN
  CALL NeumannBCDisplay(obj%nbcPointSource, "nbcPointSource: ",  &
    & unitNo=unitNo)
END IF

! wbc
bool1 = ALLOCATED(obj%wdbc)
CALL Display(bool1, "wdbc ALLOCATED: ", unitNo=unitNo)
IF (bool1) THEN
  CALL NitscheBCDisplay(obj%wdbc, "wdbc: ", unitNo=unitNo)
END IF

! nitscheFacetToCell
bool1 = ALLOCATED(obj%nitscheFacetToCell)
CALL Display(bool1, "nitscheFacetToCell ALLOCATED: ", unitNo=unitNo)
IF (bool1) THEN
  aint = SIZE(obj%nitscheFacetToCell)
  CALL Display("nitscheFacetToCell["//tostring(aint)//"]:: ALLOCATED", &
    & unitno)
END IF

bool1 = ALLOCATED(obj%solidMaterial)
IF (bool1) THEN
  CALL Display("solidMaterial: ALLOCATED, SIZE[" &
    & //TOSTRING(SIZE(obj%solidMaterial))//']', &
    & unitNo=unitNo)
  DO ii = 1, SIZE(obj%solidMaterial)
    IF (ASSOCIATED(obj%solidMaterial(ii)%ptr)) THEN
      CALL obj%solidMaterial(ii)%ptr%Display( &
        & "solidMaterial("//TOSTRING(ii)//") : ", &
        & unitNo=unitNo)
    ELSE
      CALL Display("solidMaterial("//TOSTRING(ii)// &
        & ") : NOT ASSOCIATED", unitNo=unitNo)
    END IF
  END DO
END IF

! solidMaterialToMesh
bool1 = ALLOCATED(obj%solidMaterialToMesh)
CALL Display(bool1, "solidMaterialToMesh ALLOCATED: ", unitNo=unitNo)
IF (bool1) THEN
  CALL Display("solidMaterialToMesh: SIZE[" &
    & //TOSTRING(SIZE(obj%solidMaterialToMesh))//']', &
    & unitNo=unitNo)
  DO ii = 1, SIZE(obj%solidMaterialToMesh)
    CALL obj%solidMaterialToMesh(ii)%Display( &
      & "solidMaterialToMesh("//TOSTRING(ii)//") : ", &
      & unitNo=unitNo)
  END DO
END IF

! stiffnessMat
bool1 = ASSOCIATED(obj%stiffnessMat)
CALL Display(bool1, "stiffnessMat ASSOCIATED: ", unitNo=unitNo)

! massMat
bool1 = ASSOCIATED(obj%massMat)
CALL Display(bool1, "massMat ASSOCIATED: ", unitNo=unitNo)

! dampingMat
bool1 = ASSOCIATED(obj%dampingMat)
CALL Display(bool1, "dampingMat ASSOCIATED: ", unitNo=unitNo)

! displacement
bool1 = ASSOCIATED(obj%displacement)
CALL Display(bool1, "displacement ASSOCIATED: ", unitNo=unitNo)

! velocity
bool1 = ASSOCIATED(obj%velocity)
CALL Display(bool1, "velocity ASSOCIATED: ", unitNo=unitNo)

! acceleration
bool1 = ASSOCIATED(obj%acceleration)
CALL Display(bool1, "acceleration ASSOCIATED: ", unitNo=unitNo)

! shearModulus
bool1 = ALLOCATED(obj%shearModulus)
CALL Display(bool1, "obj%shearModulus ALLOCATED: ", unitNo=unitNo)

! youngsModulus
bool1 = ALLOCATED(obj%youngsModulus)
CALL Display(bool1, "obj%youngsModulus ALLOCATED: ", unitNo=unitNo)

! dampCoeff_alpha
bool1 = ALLOCATED(obj%dampCoeff_alpha)
CALL Display(bool1, "obj%dampCoeff_alpha ALLOCATED: ", unitNo=unitNo)

! dampCoeff_beta
bool1 = ALLOCATED(obj%dampCoeff_beta)
CALL Display(bool1, "obj%dampCoeff_beta ALLOCATED: ", unitNo=unitNo)

! Cijkl
bool1 = ALLOCATED(obj%Cijkl)
CALL Display(bool1, "obj%Cijkl ALLOCATED: ", unitNo=unitNo)

! stress
bool1 = ALLOCATED(obj%stress)
CALL Display(bool1, "obj%stress ALLOCATED: ", unitNo=unitNo)

! strain
bool1 = ALLOCATED(obj%strain)
CALL Display(bool1, "obj%strain ALLOCATED: ", unitNo=unitNo)

! bodySourceFunc
bool1 = ASSOCIATED(obj%bodySourceFunc)
CALL Display(bool1, "obj%bodySourceFunc ASSOCIATED: ", unitNo=unitNo)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                               WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_hdf5
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_hdf5()"
TYPE(String) :: dsetname
LOGICAL(LGT) :: isok, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

problem = .NOT. obj%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
   & "[INTERNAL ERROR] :: AbstractKernel_::obj is not initiated")
  RETURN
END IF

isok = hdf5%isOpen()
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[INTERNAL ERROR] :: HDF5 file is not opened")
  RETURN
END IF

isok = hdf5%isWrite()
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[INTERNAL ERROR] :: HDF5 file does not have Write permission")
  RETURN
END IF

isok = ASSOCIATED(obj%displacement)
IF (isok) THEN
  dsetname = TRIM(group)//"/displacement"
  CALL obj%displacement%Export(hdf5=hdf5, group=dsetname%chars())
END IF

isok = ASSOCIATED(obj%velocity)
IF (isok) THEN
  dsetname = TRIM(group)//"/velocity"
  CALL obj%velocity%export(hdf5=hdf5, group=dsetname%chars())
END IF

isok = ASSOCIATED(obj%acceleration)
IF (isok) THEN
  dsetname = TRIM(group)//"/acceleration"
  CALL obj%acceleration%export(hdf5=hdf5, group=dsetname%chars())
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_WriteData_hdf5

!----------------------------------------------------------------------------
!                                                               WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk()"
LOGICAL(LGT) :: isok, problem
TYPE(VTKFile_) :: avtk
TYPE(String) :: filename

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

problem = .NOT. obj%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
   & "[INTERNAL ERROR] :: AbstractKernel_::obj is not initiated")
  RETURN
END IF

isok = ASSOCIATED(obj%displacement)
IF (isok) THEN
  filename = obj%outputPath//obj%GetPrefix()//"_displacement_"  &
    & //tostring(obj%currentTimeStep)//".vtu"
  CALL avtk%InitiateVTKFile(filename=filename%chars())
  CALL obj%displacement%WriteData(vtk=avtk)
  CALL avtk%DEALLOCATE()
END IF

isok = ASSOCIATED(obj%velocity)
IF (isok) THEN
  filename = obj%outputPath//obj%GetPrefix()//"_velocity_"  &
    & //tostring(obj%currentTimeStep)//".vtu"
  CALL avtk%InitiateVTKFile(filename=filename%chars())
  CALL obj%velocity%WriteData(vtk=avtk)
  CALL avtk%DEALLOCATE()
END IF

isok = ASSOCIATED(obj%acceleration)
IF (isok) THEN
  filename = obj%outputPath//obj%GetPrefix()//"_acceleration_"  &
    & //tostring(obj%currentTimeStep)//".vtu"
  CALL avtk%InitiateVTKFile(filename=filename%chars())
  CALL obj%acceleration%WriteData(vtk=avtk)
  CALL avtk%DEALLOCATE()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_WriteData_vtk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
