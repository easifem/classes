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

SUBMODULE(ScalarField_Class) IOMethods
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldImport
USE Display_Method, ONLY: ToString
USE TomlUtility, ONLY: GetValue
USE FPL, ONLY: FPL_INIT, FPL_FINALIZE

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: dsetname
LOGICAL(LGT) :: bools(3)
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldImport(obj=obj, hdf5=hdf5, group=group, &
                             fedof=fedof, fedofs=fedofs)

dsetname = TRIM(group)//"/tSize"
bools(1) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/dof"
bools(2) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/realVec"
bools(3) = hdf5%pathExists(dsetname%chars())

IF (.NOT. ALL(bools)) THEN

  CALL param%initiate()
  CALL SetScalarFieldParam(param=param, name=obj%name%chars(), &
                           engine=obj%engine%chars(), fieldType=obj%fieldType)
  obj%isInitiated = .FALSE.
  CALL obj%Initiate(param=param, fedof=fedof)
  CALL param%DEALLOCATE()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                               ExportToVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"

INTEGER(I4B) :: tsize, tnodes
REAL(DFP), ALLOCATABLE :: VALUE(:)
TYPE(String) :: name
CHARACTER(1), ALLOCATABLE :: dofnames(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalPhysicalVars()
ALLOCATE (dofnames(tsize))
CALL obj%GetPhysicalNames(dofnames)

tsize = obj%fedof%GetTotalDOF()
tnodes = obj%fedof%GetTotalVertexDOF()

ALLOCATE (VALUE(tsize))
CALL obj%Get(VALUE=VALUE, tsize=tsize)

! name = obj%name%chars()//"_"//dofnames(1)
name = obj%name%Join(array=dofnames, sep="_")

CALL vtk%WriteDataArray(name=name, x=VALUE(1:tnodes), numberOfComponents=1)

name = ''
DEALLOCATE (dofnames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

TYPE(ParameterList_) :: param
CHARACTER(:), ALLOCATABLE :: key
TYPE(String) :: name, engine, fedofName, fieldTypeChar
INTEGER(I4B) :: fieldType, origin, stat
LOGICAL(LGT) :: isfound
CHARACTER(*), PARAMETER :: default_engine = "NATIVE_SERIAL"
CHARACTER(*), PARAMETER :: default_fieldTypeChar = "Normal"
CHARACTER(*), PARAMETER :: default_name = myprefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

key = "name"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//key//" ...")
#endif
CALL GetValue(table=table, key=key, VALUE=name, &
              default_value=default_name, origin=origin, &
              stat=stat, isFound=isfound)
CALL AssertError1(isfound, myName, &
                  key//" not found in the toml file")

key = "engine"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//key//" ...")
#endif
CALL GetValue(table=table, key=key, VALUE=engine, &
              default_value=default_engine, origin=origin, &
              stat=stat, isFound=isfound)
CALL AssertError1(isfound, myName, &
                  key//" not found in the toml file")

key = "fieldType"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//key//" ...")
#endif
CALL GetValue(table=table, key=key, VALUE=fieldTypeChar, &
              default_value=default_fieldTypeChar, origin=origin, &
              stat=stat, isFound=isfound)
CALL AssertError1(isfound, myName, &
                  key//" not found in the toml file")
! BUG:
! Convert fieldTypeChar to fieldType

CALL FPL_Init
CALL param%Initiate()

CALL SetScalarFieldParam(param=param, name=name%chars(), &
                         engine=obj%engine%chars(), fieldType=fieldType)

! CALL obj%Initiate(param=param, fedof=fedof)

CALL param%DEALLOCATE()
CALL FPL_Finalize

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
