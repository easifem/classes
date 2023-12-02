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

SUBMODULE(SolidMaterial_Class) IOMethods
USE MaterialFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
! CHARACTER(*), PARAMETER :: myName = "obj_Display"
! main
CALL AbstractMaterialDisplay(obj=obj, msg=msg, unitNo=unitNo)
IF (ASSOCIATED(obj%stressStrainModel)) THEN
  CALL obj%stressStrainModel%Display(msg="# stressStrainModel :", &
  & unitNo=unitNo)
END IF
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import"
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Import()')
#endif

CALL AbstractMaterialImport(obj=obj, hdf5=hdf5, group=group)

! stressStrainModel
IF (hdf5%pathExists(TRIM(group)//"/stressStrainModel")) THEN
  dsetname = TRIM(group)//"/stressStrainModel/name"

  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[CONIFG ERROR] :: dataset ./stressStrainModel/name '//  &
      & 'should be present.')
    RETURN
  END IF

  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
  obj%stressStrainModel => SolidMechanicsModelFactory( &
    & TRIM(strval%chars()))
  dsetname = TRIM(group)//"/stressStrainModel"
  CALL obj%stressStrainModel%IMPORT(hdf5=hdf5, &
    & group=dsetname%chars())
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Import()')
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
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL AbstractMaterialImportFromToml(obj=obj, table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL AbstractMaterialImportFromToml(obj=obj, array=array)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml3
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml3()"
! TYPE(toml_table), ALLOCATABLE :: table
! TYPE(toml_table), POINTER :: node
! INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL AbstractMaterialImportFromToml(obj=obj, tomlName=tomlName,  &
  & afile=afile, filename=filename, printToml=printToml)

! ! Get the entire file in table
! IF (PRESENT(afile)) THEN
!   CALL GetValue(table=table, afile=afile)
! ELSEIF (PRESENT(filename)) THEN
!   CALL GetValue(table=table, filename=filename)
! ELSE
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[ARGUMENT ERROR] :: either filename or afile should be present!')
!   RETURN
! END IF
!
! ! get tomlName from the table
! node => NULL()
! array => NULL()
! CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE.,  &
!   & stat=stat)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_ImportFromToml3

END SUBMODULE IOMethods
