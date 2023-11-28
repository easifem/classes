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

SUBMODULE(PorousMaterial_Class) IOMethods
USE MaterialFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Porous_Display
! CHARACTER(*), PARAMETER :: myName = "Porous_Display"
CALL AbstractMaterialDisplay(obj=obj, msg=msg, unitNo=unitNo)
IF (ASSOCIATED(obj%stressStrainModel)) THEN
  CALL obj%stressStrainModel%Display(msg="stressStrainModel :", &
  & unitNo=unitNo)
END IF
END PROCEDURE Porous_Display

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE Porous_Import
CHARACTER(*), PARAMETER :: myName = "Porous_Import"
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Import()')
#endif

CALL AbstractMaterialImport(obj=obj, hdf5=hdf5, group=group)

IF (hdf5%pathExists(TRIM(group)//"/stressStrainModel")) THEN
  dsetname = TRIM(group)//"/stressStrainModel/name"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'The dataset ./stressStrainModel/name should be present')
  END IF
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
  obj%stressStrainModel => PoroMechanicsModelFactory( &
    & TRIM(strval%chars()))
  dsetname = TRIM(group)//"/stressStrainModel"
  CALL obj%stressStrainModel%IMPORT(hdf5=hdf5, &
    & group=dsetname%chars())
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Import()')
#endif
END PROCEDURE Porous_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE Porous_Export
CHARACTER(*), PARAMETER :: myName = "Porous_Export"
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Export()')
#endif

CALL AbstractMaterialExport(obj=obj, hdf5=hdf5, group=group)
IF (ASSOCIATED(obj%stressStrainModel)) THEN
  dsetname = TRIM(group)//"/stressStrainModel"
  CALL obj%stressStrainModel%export(hdf5=hdf5, group=dsetname%chars())
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Export()')
#endif
END PROCEDURE Porous_Export

END SUBMODULE IOMethods
