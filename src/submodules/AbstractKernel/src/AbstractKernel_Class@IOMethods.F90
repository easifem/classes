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