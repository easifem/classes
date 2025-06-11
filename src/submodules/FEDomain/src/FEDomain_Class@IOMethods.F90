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

SUBMODULE(FEDomain_Class) IOMethods
USE Display_Method
USE FEMesh_Class, ONLY: FEMesh_, FEMesh_Pointer
USE AbstractDomain_Class, ONLY: AbstractDomainDisplay, &
                                AbstractDomainDisplayDomainInfo, &
                                AbstractDomainImport

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL AbstractDomainDisplay(obj=obj, msg=msg, unitno=unitno)

IF (.NOT. obj%IsInit()) RETURN

abool = ASSOCIATED(obj%meshVolume)
CALL Display(abool, "meshVolume ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL BlankLines(nol=1, unitno=unitno)
  CALL obj%meshVolume%DisplayMeshInfo("Volume Mesh Info:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END IF

abool = ASSOCIATED(obj%meshSurface)
CALL Display(abool, "meshSurface ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL BlankLines(nol=1, unitno=unitno)
  CALL obj%meshSurface%DisplayMeshInfo("Surface Mesh Info:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END IF

abool = ASSOCIATED(obj%meshCurve)
CALL Display(abool, "meshCurve ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL BlankLines(nol=1, unitno=unitno)
  CALL obj%meshCurve%DisplayMeshInfo("Curve Mesh Info:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END IF

abool = ASSOCIATED(obj%meshPoint)
CALL Display(abool, "meshPoint ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL BlankLines(nol=1, unitno=unitno)
  CALL obj%meshPoint%DisplayMeshInfo("Point Mesh Info:", unitno=unitno)
  CALL BlankLines(nol=1, unitno=unitno)
END IF

abool = ASSOCIATED(obj%mesh)
CALL Display(abool, "mesh ASSOCIATED: ", unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                          DisplaDomainInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayDomainInfo
LOGICAL(LGT) :: abool

CALL AbstractDomainDisplayDomainInfo(obj=obj, unitno=unitno, msg=msg)

abool = ASSOCIATED(obj%mesh)
CALL Display(abool, "mesh ASSOCIATED: ", unitno=unitno)
IF (abool) THEN
  CALL obj%mesh%DisplayMeshInfo("Mesh Info:", unitno=unitno)
END IF

END PROCEDURE obj_DisplayDomainInfo

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
INTEGER(I4B) :: nsd, aintval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractDomainImport(obj=obj, hdf5=hdf5, group=group)

nsd = obj%GetNSD()

IF (nsd .EQ. 3_I4B) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Importing meshVolume')
#endif

  obj%meshVolume => FEMesh_Pointer()
  CALL obj%meshVolume%Initiate(hdf5=hdf5, group=group, dim=3_I4B)
  aintval = obj%meshVolume%GetTotalElements()
  CALL obj%SetTotalElements(indx=3, VALUE=aintval)
END IF

IF (nsd .GT. 1_I4B) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Importing meshSurface')
#endif

  obj%meshSurface => FEMesh_Pointer()
  CALL obj%meshSurface%Initiate(hdf5=hdf5, group=group, dim=2_I4B)
  aintval = obj%meshSurface%GetTotalElements()
  CALL obj%SetTotalElements(indx=2, VALUE=aintval)

END IF

IF (nsd .GE. 1_I4B) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Importing meshCurve')
#endif

  obj%meshCurve => FEMesh_Pointer()
  CALL obj%meshCurve%Initiate(hdf5=hdf5, group=group, dim=1_I4B)
  aintval = obj%meshCurve%GetTotalElements()
  CALL obj%SetTotalElements(indx=1, VALUE=aintval)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Importing meshPoint')
#endif

obj%meshPoint => FEMesh_Pointer()
CALL obj%meshPoint%Initiate(hdf5=hdf5, group=group, dim=0_I4B)
aintval = obj%meshPoint%GetTotalElements()
CALL obj%SetTotalElements(indx=0, VALUE=aintval)

! Setting mesh pointer based on nsd
SELECT CASE (nsd)
CASE (0)
  obj%mesh => obj%meshPoint
CASE (1)
  obj%mesh => obj%meshCurve
CASE (2)
  obj%mesh => obj%meshSurface
CASE (3)
  obj%mesh => obj%meshVolume
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
