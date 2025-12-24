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

SUBMODULE(FluidMaterial_Class) HDFMethods
USE Display_Method, ONLY: ToString
USE AbstractMaterial_Class, ONLY: AbstractMaterialImport, &
                                  AbstractMaterialExport
USE MaterialFactory, ONLY: FluidMechanicsModelFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

LOGICAL(LGT) :: isok
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractMaterialImport(obj=obj, hdf5=hdf5, group=group)

! stressStrainModel
isok = hdf5%PathExists(TRIM(group)//"/stressStrainModel")
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

dsetname = TRIM(group)//"/stressStrainModel/name"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'dataset '//dsetname//' should be present.')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
obj%stressStrainModel => FluidMechanicsModelFactory(strval%chars())
dsetname = TRIM(group)//"/stressStrainModel"
CALL obj%stressStrainModel%IMPORT(hdf5=hdf5, group=dsetname%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export"
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Export()")
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

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                            Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
