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
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method
USE StringUtility
USE ReallocateUtility
USE tomlf, ONLY: toml_serialize, toml_get => get_value
USE TomlUtility
USE HDF5File_Method
USE FEMesh_Class, ONLY: FEMesh_, FEMesh_Pointer
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL Display(obj%isInitiated, "FEDomain_::obj Initiated: ", unitno=unitno)
IF (.NOT. obj%isInitiated) RETURN

CALL Display("engine: "//obj%engine, unitno=unitno)
CALL Display("majorVersion: "//tostring(obj%majorVersion), unitno=unitno)
CALL Display("minorVersion: "//tostring(obj%minorVersion), unitno=unitno)
CALL Display("version: "//tostring(obj%version), unitno=unitno)
CALL Display("nsd: "//tostring(obj%nsd), unitno=unitno)
CALL Display("maxNptrs: "//tostring(obj%maxNptrs), unitno=unitno)
CALL Display("minNptrs: "//tostring(obj%minNptrs), unitno=unitno)
CALL Display("tNodes: "//tostring(obj%tNodes), unitno=unitno)
CALL Display(obj%isNodeNumberSparse, "isNodeNumberSparse: ", unitno=unitno)
CALL Display("maxElemNum: "//tostring(obj%maxElemNum), unitno=unitno)
CALL Display("minElemNum: "//tostring(obj%minElemNum), unitno=unitno)
CALL Display(obj%isElemNumberSparse, "isElemNumberSparse: ", unitno=unitno)
CALL Display("tEntitiesForNodes: "//tostring(obj%tEntitiesForNodes), &
  & unitno=unitno)
CALL Display("tEntitiesForElements: "//tostring(obj%tEntitiesForElements), &
  & unitno=unitno)
CALL Display("tEntitiesForElements: "//tostring(obj%tEntitiesForElements), &
  & unitno=unitno)
CALL Display("tElements: "//tostring(obj%tElements), &
  & unitno=unitno)
CALL Display("tEntities: "//tostring(obj%tEntities), &
  & unitno=unitno)

abool = ALLOCATED(obj%nodeCoord)
CALL Display(abool, "nodeCoord Allocated: ", unitno=unitno)

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

CALL Display(obj%meshMap%isInitiated, "meshMap Initiated: ", unitno=unitno)

END PROCEDURE obj_Display

END SUBMODULE IOMethods
