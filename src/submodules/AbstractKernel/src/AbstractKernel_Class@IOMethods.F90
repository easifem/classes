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

! name
CALL Display(obj%name, "name: ", unitno=unitno)

! isCommonDomain
CALL Display(obj%isCommonDomain, "isCommonDomain: ", unitNo=unitNo)

! engine
CALL Display(obj%engine, "engine: ", unitno=unitno)

! coordinateSystem
CALL Display(obj%coordinateSystem, "coordinateSystem: ", unitno=unitno)

! maxIter
CALL Display(obj%maxIter, "maxIter: ", unitno=unitno)

! timeDependency
CALL Display(obj%timeDependency, "timeDependency: ", unitno=unitno)

! nsd
CALL Display(obj%nsd, "nsd: ", unitno=unitno)

! nnt
CALL Display(obj%nnt, "nnt: ", unitno=unitno)

! tdof
CALL Display(obj%tdof, "tdof: ", unitno=unitno)

! normRHS
CALL Display(obj%normRHS, "normRHS: ", unitno=unitno)

! dt
CALL Display(obj%dt, "dt: ", unitno=unitno)

! startTime
CALL Display(obj%startTime, "startTime: ", unitno=unitno)

! endTime
CALL Display(obj%endTime, "endTime: ", unitno=unitno)

! currentTime
CALL Display(obj%currentTime, "currentTime: ", unitno=unitno)

! lengthScale
CALL Display(obj%lengthScale, "lengthScale: ", unitno=unitno)

! currentTimeStep
CALL Display(obj%currentTimeStep, "currentTimeStep: ", unitno=unitno)

! totalTimeStep
CALL Display(obj%totalTimeStep, "totalTimeStep: ", unitno=unitno)

! postProcessOpt
CALL Display(obj%postProcessOpt, "postProcessOpt: ", unitno=unitno)

! gravity
CALL Display(obj%gravity, "gravity: ", unitno=unitno)

! iterData
CALL Display(obj%iterData, "iterData: ", unitno=unitno)

! isConstantMatProp
CALL Display(obj%isConstantMatProp, "isConstantMatProp: ", unitNo=unitNo)

! isIsotropic
CALL Display(obj%isIsotropic, "isIsotropic: ", unitNo=unitNo)

! isIncompressible
CALL Display(obj%isIncompressible, "isIncompressible: ", unitNo=unitNo)

! isMaterialInterfaces

CALL Display(obj%isMaterialInterfaces, "isMaterialInterfaces: ", &
  & unitNo=unitNo)
IF (obj%isMaterialInterfaces) THEN
  CALL Display(obj%materialInterfaces, 'materialInterfaces', &
    & unitNo=unitNo)
END IF

! matIfaceConnectData
bool1 = ALLOCATED(obj%matIfaceConnectData)
CALL Display(bool1, "matIfaceConnectData ALLOCATED: ", unitNo=unitNo)

! tMaterials
CALL Display(obj%tMaterials, "tMaterials: ", unitNo=unitNo)

! incrementScale
CALL Display(obj%incrementScale, "incrementScale : ", unitNo=unitNo)

! rtoleranceForDisplacement
CALL Display(obj%rtoleranceForDisplacement, "rtoleranceForDisplacement: ", &
  & unitNo=unitNo)

! atoleranceForDisplacement
CALL Display(obj%atoleranceForDisplacement, "atoleranceForDisplacement: ", &
  & unitNo=unitNo)

! rtoleranceForVelocity
CALL Display(obj%rtoleranceForVelocity, "rtoleranceForVelocity: ", &
  & unitNo=unitNo)

! atoleranceForVelocity
CALL Display(obj%atoleranceForVelocity, "atoleranceForVelocity: ", &
  & unitNo=unitNo)

! rtoleranceForResidual
CALL Display(obj%rtoleranceForResidual, "rtoleranceForResidual: ", &
  & unitNo=unitNo)

! atoleranceForResidual
CALL Display(obj%atoleranceForResidual, "atoleranceForResidual: ", &
  & unitNo=unitNo)

! displacementError0
CALL Display(obj%displacementError0, "displacementError0: ", unitNo=unitNo)

! displacementError
CALL Display(obj%displacementError, "displacementError: ", unitNo=unitNo)

! velocityError0
CALL Display(obj%velocityError0, "velocityError0: ", unitNo=unitNo)

! velocityError
CALL Display(obj%velocityError, "velocityError: ", unitNo=unitNo)

! residualError0
CALL Display(obj%residualError0, "residualError0: ", unitNo=unitNo)

! residualError
CALL Display(obj%residualError, "residualError: ", unitNo=unitNo)

! elemToMatId
bool1 = ALLOCATED(obj%elemToMatId)
CALL Display(bool1, "elemToMatID ALLOCATED: ", unitno=unitno)

! linsol
bool1 = ASSOCIATED(obj%linsol)
CALL Display(bool1, "linsol ASSOCIATED: ", unitno=unitno)

! tanmat
bool1 = ASSOCIATED(obj%tanmat)
CALL Display(bool1, "tanmat ASSOCIATED: ", unitno=unitno)

IF (bool1) THEN
  CALL obj%tanmat%Display("tanmat: ", unitNo=unitNo)
END IF

! baseContinuityForSpace
CALL Display(obj%baseContinuityForSpace, "baseContinuityForSpace: ", &
  & unitNo=unitNo)

! baseInterpolationForSpace
CALL Display(obj%baseInterpolationForSpace, "baseInterpolationForSpace: ", &
  & unitNo=unitNo)

! quadratureTypeForSpace
CALL Display(obj%quadratureTypeForSpace, "quadratureTypeForSpace: ", &
  & unitNo=unitNo)

! baseContinuityForTime
CALL Display(obj%baseContinuityForTime, "baseContinuityForTime: ", &
  & unitNo=unitNo)

! baseInterpolationForTime
CALL Display(obj%baseInterpolationForTime, "baseInterpolationForTime: ", &
  & unitNo=unitNo)

! quadratureTypeForTime
CALL Display(obj%quadratureTypeForTime, "quadratureTypeForTime: ", &
  & unitNo=unitNo)

! domainFile
CALL Display(obj%domainFile, "domainFile: ", unitNo=unitNo)

! dom
bool1 = ASSOCIATED(obj%dom)
CALL Display(bool1, "dom ASSOCIATED: ", unitNo=unitNo)

! domains
bool1 = ALLOCATED(obj%domains)
CALL Display(bool1, "domains ALLOCATED: ", unitNo=unitNo)

! quadratureForSpace
bool1 = ALLOCATED(obj%quadratureForSpace)
CALL Display(bool1, "quadratureForSpace ALLOCATED: ", unitNo=unitNo)

! cellFE
bool1 = ALLOCATED(obj%cellFE)
CALL Display(bool1, "cellFE ALLOCATED: ", unitNo=unitNo)

! facetFE
bool1 = ALLOCATED(obj%facetFE)
CALL Display(bool1, "facetFE ALLOCATED: ", unitNo=unitNo)

! edgeFE
bool1 = ALLOCATED(obj%edgeFE)
CALL Display(bool1, "edgeFE ALLOCATED: ", unitNo=unitNo)

! isNitsche
CALL Display(obj%isNitsche, "isNitsche: ", unitNo=unitNo)
CALL Display(obj%nitscheAlpha, "nitscheAlpha: ", unitno=unitno)
IF (INT(obj%NitscheType, kind=I4B) .EQ. Nitsche_Sym) THEN
  CALL Display("NitscheType: SYM", unitno=unitno)
ELSE
  CALL Display("NitscheType: UNSYM", unitno=unitno)
END IF

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
ELSE
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

! dispBC
bool1 = ASSOCIATED(obj%dispBC)
CALL Display(bool1, "dispBC ASSOCIATED: ", unitNo=unitNo)

! velBC
bool1 = ASSOCIATED(obj%velBC)
CALL Display(bool1, "velBC ASSOCIATED: ", unitNo=unitNo)

! accBC
bool1 = ASSOCIATED(obj%accBC)
CALL Display(bool1, "accBC ASSOCIATED: ", unitNo=unitNo)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                               WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_hdf5
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_hdf5"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: kernel which you are using cannot writeData hdf5 format')
END PROCEDURE obj_WriteData_hdf5

!----------------------------------------------------------------------------
!                                                               WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: kernel which you are using cannot writeData vtk format')
END PROCEDURE obj_WriteData_vtk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
