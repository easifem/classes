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

SUBMODULE(AbstractField_Class) TomlMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize, toml_array, &
                 toml_len => len
USE StringUtility, ONLY: UpperCase
USE DirichletBC_Class, ONLY: DirichletBCImportFromToml
USE NeumannBC_Class, ONLY: NeumannBCImportFromToml

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

TYPE(String) :: name
TYPE(String) :: engine
INTEGER(I4B) :: fieldType
INTEGER(I4B), ALLOCATABLE :: spaceCompo(:)
LOGICAL(LGT) :: isSpaceCompo
LOGICAL(LGT) :: isSpaceCompoScalar
INTEGER(I4B), ALLOCATABLE :: timeCompo(:)
LOGICAL(LGT) :: isTimeCompo
LOGICAL(LGT) :: isTimeCompoScalar
INTEGER(I4B) :: tPhysicalVarNames
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
LOGICAL(LGT) :: isPhysicalVarNames
LOGICAL(LGT) :: isPhysicalVarNamesScalar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldReadOptsFromToml( &
  table=table, name=name, engine=engine, fieldType=fieldType, &
  spaceCompo=spaceCompo, isSpaceCompo=isSpaceCompo, &
  isSpaceCompoScalar=isSpaceCompoScalar, timeCompo=timeCompo, &
  isTimeCompo=isTimeCompo, isTimeCompoScalar=isTimeCompoScalar, &
  physicalVarNames=physicalVarNames, tPhysicalVarNames=tPhysicalVarNames, &
  isPhysicalVarNames=isPhysicalVarNames, &
  isPhysicalVarNamesScalar=isPhysicalVarNamesScalar)

CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, dom=dom)
CALL AbstractFieldReadGeoFEDOFFromToml(table=table, fedof=geofedof, dom=dom)
CALL AbstractFieldReadTimeFEDOFFromToml(table=table, timefedof=timefedof, &
                                        timeOpt=timeOpt)

CALL obj%Initiate( &
  name=name%chars(), engine=engine%chars(), fieldType=fieldType, &
  spaceCompo=spaceCompo, isSpaceCompo=isSpaceCompo, &
  isSpaceCompoScalar=isSpaceCompoScalar, timeCompo=timeCompo, &
  isTimeCompo=isTimeCompo, isTimeCompoScalar=isTimeCompoScalar, &
  physicalVarNames=physicalVarNames, tPhysicalVarNames=tPhysicalVarNames, &
  isPhysicalVarNames=isPhysicalVarNames, &
  isPhysicalVarNamesScalar=isPhysicalVarNamesScalar, &
  fedof=fedof, geofedof=geofedof, timefedof=timefedof)

CALL AbstractFieldReadUserFunctionFromToml(obj=obj, table=table)
CALL AbstractFieldReadDBCFromToml(obj=obj, table=table)
CALL AbstractFieldReadNBCFromToml(obj=obj, table=table)
CALL AbstractFieldReadPointNBCFromToml(obj=obj, table=table)

name = ""
engine = ""
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(physicalVarNames)) DEALLOCATE (physicalVarNames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
LOGICAL(LGT) :: isok
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

#ifdef DEBUG_VER
isok = ALLOCATED(table)
CALL AssertError1(isok, myName, "table is not allocated from GetValue")
#endif

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")
#endif

CALL obj%ImportFromToml(table=node, fedof=fedof, geofedof=geofedof, &
                        timefedof=timefedof, dom=dom, timeOpt=timeOpt)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), myname//" Domain toml config: "// &
               CHAR_LF, unitno=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml3()"
#endif

LOGICAL(LGT) :: isok
! INTEGER(I4B) :: comm, local_n, global_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL SetAbstractFieldParamFromToml(param=param, table=table, prefix=prefix)

CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, dom=dom)
CALL AbstractFieldReadGeoFEDOFFromToml(table=table, fedof=geofedof, dom=dom)

isok = PRESENT(timefedof)
IF (isok) CALL AbstractFieldReadTimeFEDOFFromToml( &
  table=table, timefedof=timefedof, timeOpt=timeOpt)

! CALL obj%Initiate(param=param, fedof=fedof, geofedof=geofedof, &
!                   timefedof=timefedof)

CALL AbstractFieldReadUserFunctionFromToml(obj=obj, table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml3

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml4()"
LOGICAL(LGT) :: isok
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

#ifdef DEBUG_VER
isok = ALLOCATED(table)
CALL AssertError1(isok, myName, "table is not allocated from GetValue")
#endif

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")
#endif

CALL obj%ImportFromToml(table=node, fedof=fedof, geofedof=geofedof, &
                        timefedof=timefedof, dom=dom, timeOpt=timeOpt)

#ifdef DEBUG_VER
isok = PRESENT(printToml)
IF (isok) THEN
  CALL Display(toml_serialize(node), myname//" Domain toml config: "// &
               CHAR_LF, unitno=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml4

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml5()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL SetAbstractFieldParamFromToml(param=param, table=table, prefix=prefix)
CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, dom=dom)
CALL AbstractFieldReadGeoFEDOFFromToml(table=table, fedof=geofedof, dom=dom)

isok = PRESENT(timefedof)
IF (isok) &
  CALL AbstractFieldReadTimeFEDOFFromToml(table=table, timefedof=timefedof, &
                                          timeOpt=timeOpt)

! CALL obj%Initiate(param=param, fedof=fedof, geofedof=geofedof, &
!                   timefedof=timefedof)

CALL AbstractFieldReadUserFunctionFromToml(obj=obj, table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml5

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml6()"
#endif
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

#ifdef DEBUG_VER
isok = ALLOCATED(table)
CALL AssertError1(isok, myName, "table is not allocated from GetValue")
#endif

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")
#endif

CALL obj%ImportFromToml(table=node, fedof=fedof, geofedof=geofedof, &
                        timefedof=timefedof, dom=dom, timeOpt=timeOpt)

#ifdef DEBUG_VER
isok = PRESENT(printToml)
IF (isok) THEN
  CALL Display(toml_serialize(node), myname//" Domain toml config: "// &
               CHAR_LF, unitno=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml6

!----------------------------------------------------------------------------
!                                                           ReadNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadNameFromToml(table, name)
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(OUT) :: name

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadNameFromToml()"
#endif

  CHARACTER(*), PARAMETER :: default_name = "field"
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading name ...')
#endif

  CALL GetValue(table=table, key="name", VALUE=name, &
                default_value=default_name, origin=origin, &
                stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL AssertError1(isFound, myName, &
                    "name not found in the toml file")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadNameFromToml

!----------------------------------------------------------------------------
!                                                           ReadNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadEngineFromToml(table, engine)
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(OUT) :: engine

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadEngineFromToml()"
#endif

  CHARACTER(*), PARAMETER :: default_engine = TypeEngineName%native_serial
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading engine ...')
#endif

  CALL GetValue( &
    table=table, key="engine", VALUE=engine, default_value=default_engine, &
    origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL AssertError1(isFound, myName, &
                    "engine not found in the toml file")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadEngineFromToml

!----------------------------------------------------------------------------
!                                                       ReadFieldTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadFieldTypeFromToml(table, fieldType)
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(OUT) :: fieldType

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadFieldTypeFromToml()"
#endif

  CHARACTER(*), PARAMETER :: default_fieldTypeChar = TypeField%normal_char
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound
  TYPE(String) :: fieldTypeChar

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading fieldType ...')
#endif
  CALL GetValue(table=table, key="fieldType", VALUE=fieldTypeChar, &
                default_value=default_fieldTypeChar, origin=origin, &
                stat=stat, isFound=isfound)

#ifdef DEBUG_VER
  CALL AssertError1(isFound, myName, &
                    "fieldType not found in the toml file")
#endif

  fieldType = TypeField%ToNumber(fieldTypeChar%chars())

  fieldTypeChar = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadFieldTypeFromToml

!----------------------------------------------------------------------------
!                                                     ReadSpaceCompoFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadSpaceCompoFromToml(table, spaceCompo, isSpaceCompo, &
                                  isSpaceCompoScalar)
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: spaceCompo(:)
  LOGICAL(LGT), INTENT(OUT) :: isSpaceCompo
  LOGICAL(LGT), INTENT(OUT) :: isSpaceCompoScalar

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadSpaceCompoFromToml()"
#endif
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading spaceCompo ...')
#endif

  CALL GetValue(table=table, key="spaceCompo", VALUE=spaceCompo, &
                origin=origin, stat=stat, isFound=isSpaceCompo, &
                isScalar=isSpaceCompoScalar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadSpaceCompoFromToml

!----------------------------------------------------------------------------
!                                                     ReadTimeCompoFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadTimeCompoFromToml(table, timeCompo, isTimeCompo, &
                                 isTimeCompoScalar)
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: timeCompo(:)
  LOGICAL(LGT), INTENT(OUT) :: isTimeCompo
  LOGICAL(LGT), INTENT(OUT) :: isTimeCompoScalar

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadTimeCompoFromToml()"
#endif
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading timeCompo ...')
#endif

  CALL GetValue(table=table, key="timeCompo", VALUE=timeCompo, &
                origin=origin, stat=stat, isFound=isTimeCompo, &
                isScalar=isTimeCompoScalar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadTimeCompoFromToml

!----------------------------------------------------------------------------
!                                              ReadPhysicalVarNamesFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadPhysicalVarNamesFromToml( &
  table, physicalVarNames, isPhysicalVarNames, isPhysicalVarNamesScalar, &
  tPhysicalVarNames)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(1), ALLOCATABLE, INTENT(INOUT) :: physicalVarNames(:)
  LOGICAL(LGT), INTENT(OUT) :: isPhysicalVarNames
  LOGICAL(LGT), INTENT(OUT) :: isPhysicalVarNamesScalar
  INTEGER(I4B), INTENT(OUT) :: tPhysicalVarNames

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadPhysicalVarNamesFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, ii
  TYPE(String), ALLOCATABLE :: tempstrs(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading physicalVarNames ...')
#endif

  CALL GetValue( &
    table=table, key="physicalVarNames", VALUE=tempstrs, origin=origin, &
    stat=stat, isFound=isPhysicalVarNames, isScalar=isPhysicalVarNamesScalar)

  tPhysicalVarNames = 0
  IF (isPhysicalVarNames) tPhysicalVarNames = SIZE(tempstrs)
  ALLOCATE (physicalVarNames(tPhysicalVarNames))
  DO ii = 1, tPhysicalVarNames
    physicalVarNames(ii) = tempstrs(ii)%slice(1, 1)
    tempstrs(ii) = ""
  END DO

  IF (ALLOCATED(tempstrs)) DEALLOCATE (tempstrs)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadPhysicalVarNamesFromToml

!----------------------------------------------------------------------------
!                                               AbstractFieldReadOptsFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadOptsFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadOptsFromToml()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ReadNameFromToml(table=table, name=name)
CALL ReadEngineFromToml(table=table, engine=engine)
CALL ReadFieldTypeFromToml(table=table, fieldType=fieldType)

CALL ReadSpaceCompoFromToml( &
  table=table, spaceCompo=spaceCompo, isSpaceCompo=isSpaceCompo, &
  isSpaceCompoScalar=isSpaceCompoScalar)

CALL ReadTimeCompoFromToml( &
  table=table, timeCompo=timeCompo, isTimeCompo=isTimeCompo, &
  isTimeCompoScalar=isTimeCompoScalar)

CALL ReadPhysicalVarNamesFromToml( &
  table=table, physicalVarNames=physicalVarNames, &
  isPhysicalVarNames=isPhysicalVarNames, &
  isPhysicalVarNamesScalar=isPhysicalVarNamesScalar, &
  tPhysicalVarNames=tPhysicalVarNames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractFieldReadOptsFromToml

!----------------------------------------------------------------------------
!                                             AbstractFieldReadFEDOFFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadFEDOFFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadFEDOFFromToml1()"
#endif

CHARACTER(*), PARAMETER :: default_fedofname = "fedof"
CHARACTER(:), ALLOCATABLE :: key
TYPE(String) :: fedofName
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isfedof, isok, isFound
TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isfedof = fedof%IsInitiated()

IF (isfedof) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'fedof is already initiated, nothing to do')
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

! If fedof is NOT initiated then we will call
! fedof%ImportFromToml. The following code is for that.

#ifdef DEBUG_VER
! First lets check if mesh if given or not
isok = PRESENT(dom)
CALL AssertError1(isok, myName, &
                  "Mesh is not given to initiate fedof")
#endif

! Getting fedofName which is a subtable name in
! toml file that contains data for fedof
key = "fedofName"
CALL GetValue(table=table, key=key, VALUE=fedofName, &
              default_value=default_fedofName, origin=origin, &
              stat=stat, isFound=isFound)

! Get the node from toml table [subtable]
CALL toml_get(table, fedofName%chars(), node, &
              origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  fedofName//" node not found")
#endif

! Now we can init fedof from toml
CALL fedof%ImportFromToml(table=node, dom=dom)

node => NULL(); key = ""; fedofName = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractFieldReadFEDOFFromToml1

!----------------------------------------------------------------------------
!                                               AbstractFieldReadFEDOfromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadFEDOFFromToml2
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadFEDOFFromToml2()"
CHARACTER(:), ALLOCATABLE :: key
TYPE(String), ALLOCATABLE :: physicalVarNames(:)
INTEGER(I4B) :: origin, stat, tsize, ii, tPhysicalVarNames
LOGICAL(LGT) :: isok, isfedofalloc, isPhysicalVarNames, &
                isPhysicalVarNamesScalar, isfedof
TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

key = "physicalVarNames"
!========================
#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading '//key//' ...')
#endif
CALL GetValue(table=table, key=key, VALUE=physicalVarNames, &
              origin=origin, stat=stat, &
              isFound=isPhysicalVarNames, isScalar=isPhysicalVarNamesScalar)
tPhysicalVarNames = 0
IF (isPhysicalVarNames) tPhysicalVarNames = SIZE(physicalVarNames)

isfedofalloc = ALLOCATED(fedof)
IF (isfedofalloc) THEN
  tsize = SIZE(fedof)

#ifdef DEBUG_VER
  isok = tsize .EQ. tPhysicalVarNames
  CALL AssertError1(isok, myName, &
                    "fedof size does not match physicalVarNames size")

  DO ii = 1, tPhysicalVarNames
    isok = ASSOCIATED(fedof(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      "fedof("//ToString(ii)//")%ptr is not associated")
  END DO
#endif

ELSE
  tsize = tPhysicalVarNames
  ALLOCATE (fedof(tsize))

  DO ii = 1, tPhysicalVarNames
    ALLOCATE (FEDOF_ :: fedof(ii)%ptr)
  END DO
END IF

DO ii = 1, tPhysicalVarNames
  isfedof = fedof(ii)%ptr%IsInitiated()
  IF (isfedof) CYCLE

  ! Get the node (subtable) from toml table for each physical variable
  CALL toml_get(table, physicalVarNames(ii)%chars(), node, &
                origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
                    physicalVarNames(ii)//" node not found")
#endif

  ! Now we can init fedof from toml
  CALL AbstractFieldReadFEDOFFromToml(table=node, &
                                      fedof=fedof(ii)%ptr, dom=dom)

END DO

key = ""
DO ii = 1, tPhysicalVarNames
  physicalVarNames(ii) = ""
END DO
DEALLOCATE (physicalVarNames)
node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractFieldReadFEDOFFromToml2

!----------------------------------------------------------------------------
!                                              AbstractFieldReadFEDOFFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadFEDOFFromToml3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadFEDOFFromToml3()"
#endif

CHARACTER(:), ALLOCATABLE :: key
TYPE(String), ALLOCATABLE :: physicalVarNames(:)
INTEGER(I4B) :: origin, stat, tsize, ii, tPhysicalVarNames, tmesh
LOGICAL(LGT) :: isok, isfedofalloc, isPhysicalVarNames, &
                isPhysicalVarNamesScalar, isfedof
TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

key = "physicalVarNames"
!========================
#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading '//key//' ...')
#endif

CALL GetValue(table=table, key=key, VALUE=physicalVarNames, &
              origin=origin, stat=stat, &
              isFound=isPhysicalVarNames, isScalar=isPhysicalVarNamesScalar)
tPhysicalVarNames = 0
IF (isPhysicalVarNames) tPhysicalVarNames = SIZE(physicalVarNames)

isfedofalloc = ALLOCATED(fedof)
IF (isfedofalloc) THEN
  tsize = SIZE(fedof)

#ifdef DEBUG_VER
  isok = tsize .EQ. tPhysicalVarNames
  CALL AssertError1(isok, myName, &
                    "fedof size does not match physicalVarNames size")

  DO ii = 1, tsize
    isok = ASSOCIATED(fedof(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      "fedof("//ToString(ii)//")%ptr is not associated")
  END DO
#endif

ELSE
  tsize = tPhysicalVarNames
  ALLOCATE (fedof(tsize))

  DO ii = 1, tsize
    ALLOCATE (FEDOF_ :: fedof(ii)%ptr)
  END DO
END IF

#ifdef DEBUG_VER
tmesh = SIZE(dom)
isok = tmesh .EQ. tsize
CALL AssertError1(isok, myName, &
                  "mesh size does not match physicalVarNames size")

DO ii = 1, tsize
  isok = ASSOCIATED(dom(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    "mesh("//ToString(ii)//")%ptr is not associated")
END DO
#endif

DO ii = 1, tPhysicalVarNames
  isfedof = fedof(ii)%ptr%IsInitiated()
  IF (isfedof) CYCLE

  ! Get the node (subtable) from toml table for each physical variable
  CALL toml_get(table, physicalVarNames(ii)%chars(), node, &
                origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
                    physicalVarNames(ii)//" node not found")
#endif

  ! Now we can init fedof from toml
  CALL AbstractFieldReadFEDOFFromToml(table=node, &
                                      fedof=fedof(ii)%ptr, dom=dom(ii)%ptr)

END DO

key = ""
DO ii = 1, tPhysicalVarNames
  physicalVarNames(ii) = ""
END DO
DEALLOCATE (physicalVarNames)

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractFieldReadFEDOFFromToml3

!----------------------------------------------------------------------------
!                                           AbstractFieldReadGeoFEDOFFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadGeoFEDOFFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadGeoFEDOFFromToml1()"
#endif

CHARACTER(*), PARAMETER :: default_fedofname = "geofedof"
CHARACTER(:), ALLOCATABLE :: key
TYPE(String) :: fedofName
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isfedof, isok, isFound
TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isfedof = fedof%IsInitiated()

IF (isfedof) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'fedof is already initiated, nothing to do')
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

! If fedof is NOT initiated then we will call
! fedof%ImportFromToml. The following code is for that.

#ifdef DEBUG_VER
! First lets check if mesh if given or not
isok = PRESENT(dom)
CALL AssertError1(isok, myName, &
                  "Mesh is not given to initiate fedof")
#endif

! Getting geofedofName which is a subtable name in
! toml file that contains data for fedof
key = "geofedofName"
CALL GetValue(table=table, key=key, VALUE=fedofName, &
              default_value=default_fedofName, origin=origin, &
              stat=stat, isFound=isFound)

! Get the node from toml table [subtable]
CALL toml_get(table, fedofName%chars(), node, &
              origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  fedofName//" node not found")
#endif

! Now we can init fedof from toml
CALL fedof%ImportFromToml(table=node, dom=dom)

node => NULL(); key = ""; fedofName = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE AbstractFieldReadGeoFEDOFFromToml1

!----------------------------------------------------------------------------
!                                               AbstractFieldReadFEDOfromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadGeoFEDOFFromToml2
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadGeoFEDOFFromToml2()"
CHARACTER(:), ALLOCATABLE :: key
TYPE(String), ALLOCATABLE :: physicalVarNames(:)
INTEGER(I4B) :: origin, stat, tsize, ii, tPhysicalVarNames
LOGICAL(LGT) :: isok, isfedofalloc, isPhysicalVarNames, &
                isPhysicalVarNamesScalar, isfedof
TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

key = "physicalVarNames"
!========================
#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading '//key//' ...')
#endif
CALL GetValue(table=table, key=key, VALUE=physicalVarNames, &
              origin=origin, stat=stat, &
              isFound=isPhysicalVarNames, isScalar=isPhysicalVarNamesScalar)
tPhysicalVarNames = 0
IF (isPhysicalVarNames) tPhysicalVarNames = SIZE(physicalVarNames)

isfedofalloc = ALLOCATED(fedof)
IF (isfedofalloc) THEN
  tsize = SIZE(fedof)

#ifdef DEBUG_VER
  isok = tsize .EQ. tPhysicalVarNames
  CALL AssertError1(isok, myName, &
                    "fedof size does not match physicalVarNames size")

  DO ii = 1, tPhysicalVarNames
    isok = ASSOCIATED(fedof(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      "fedof("//ToString(ii)//")%ptr is not associated")
  END DO
#endif

ELSE
  tsize = tPhysicalVarNames
  ALLOCATE (fedof(tsize))

  DO ii = 1, tPhysicalVarNames
    ALLOCATE (FEDOF_ :: fedof(ii)%ptr)
  END DO
END IF

DO ii = 1, tPhysicalVarNames
  isfedof = fedof(ii)%ptr%IsInitiated()
  IF (isfedof) CYCLE

  ! Get the node (subtable) from toml table for each physical variable
  CALL toml_get(table, physicalVarNames(ii)%chars(), node, &
                origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
                    physicalVarNames(ii)//" node not found")
#endif

  ! Now we can init fedof from toml
  CALL AbstractFieldReadGeoFEDOFFromToml( &
    table=node, fedof=fedof(ii)%ptr, dom=dom)

END DO

key = ""
DO ii = 1, tPhysicalVarNames
  physicalVarNames(ii) = ""
END DO
DEALLOCATE (physicalVarNames)
node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractFieldReadGeoFEDOFFromToml2

!----------------------------------------------------------------------------
!                                         AbstractFieldReadGeoFEDOFFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadGeoFEDOFFromToml3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadGeoFEDOFFromToml3()"
#endif

CHARACTER(:), ALLOCATABLE :: key
TYPE(String), ALLOCATABLE :: physicalVarNames(:)
INTEGER(I4B) :: origin, stat, tsize, ii, tPhysicalVarNames, tmesh
LOGICAL(LGT) :: isok, isfedofalloc, isPhysicalVarNames, &
                isPhysicalVarNamesScalar, isfedof
TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

key = "physicalVarNames"
!========================
#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading '//key//' ...')
#endif

CALL GetValue(table=table, key=key, VALUE=physicalVarNames, &
              origin=origin, stat=stat, &
              isFound=isPhysicalVarNames, isScalar=isPhysicalVarNamesScalar)
tPhysicalVarNames = 0
IF (isPhysicalVarNames) tPhysicalVarNames = SIZE(physicalVarNames)

isfedofalloc = ALLOCATED(fedof)
IF (isfedofalloc) THEN
  tsize = SIZE(fedof)

#ifdef DEBUG_VER
  isok = tsize .EQ. tPhysicalVarNames
  CALL AssertError1(isok, myName, &
                    "fedof size does not match physicalVarNames size")

  DO ii = 1, tsize
    isok = ASSOCIATED(fedof(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      "fedof("//ToString(ii)//")%ptr is not associated")
  END DO
#endif

ELSE
  tsize = tPhysicalVarNames
  ALLOCATE (fedof(tsize))

  DO ii = 1, tsize
    ALLOCATE (FEDOF_ :: fedof(ii)%ptr)
  END DO
END IF

#ifdef DEBUG_VER
tmesh = SIZE(dom)
isok = tmesh .EQ. tsize
CALL AssertError1(isok, myName, &
                  "mesh size does not match physicalVarNames size")

DO ii = 1, tsize
  isok = ASSOCIATED(dom(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    "mesh("//ToString(ii)//")%ptr is not associated")
END DO
#endif

DO ii = 1, tPhysicalVarNames
  isfedof = fedof(ii)%ptr%IsInitiated()
  IF (isfedof) CYCLE

  ! Get the node (subtable) from toml table for each physical variable
  CALL toml_get(table, physicalVarNames(ii)%chars(), node, &
                origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
                    physicalVarNames(ii)//" node not found")
#endif

  ! Now we can init fedof from toml
  CALL AbstractFieldReadGeoFEDOFFromToml(table=node, &
                                         fedof=fedof(ii)%ptr, dom=dom(ii)%ptr)

END DO

key = ""
DO ii = 1, tPhysicalVarNames
  physicalVarNames(ii) = ""
END DO
DEALLOCATE (physicalVarNames)

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractFieldReadGeoFEDOFFromToml3

!----------------------------------------------------------------------------
!                                         AbstractFieldReadTimeFEDOFFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadTimeFEDOFFromToml1
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadTimeFEDOFFromToml1()"
CHARACTER(*), PARAMETER :: DEFAULT_FEDOFNAME = "timefedof"
CHARACTER(*), PARAMETER :: DEFAULT_FEDOFNAME_KEY = "timefedofName"
CHARACTER(:), ALLOCATABLE :: key
TYPE(String) :: fedofName
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isfedof, isok, isFound
TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(timefedof)
IF (.NOT. isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'timefedof is not present, nothing to do')

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN

END IF

isfedof = timefedof%IsInitiated()

IF (isfedof) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'fedof is already initiated, nothing to do')
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

! If timefedof is NOT initiated then we will call
! timefedof%ImportFromToml. The following code is for that.

! First lets check if timeOpt is given or not
#ifdef DEBUG_VER
isok = PRESENT(timeOpt)
CALL AssertError1(isok, myName, &
                  "timeOpt is not given to initiate timefedof")
#endif

! Getting fedofName which is a subtable name in
! toml file that contains data for fedof
key = DEFAULT_FEDOFNAME_KEY
CALL GetValue(table=table, key=key, VALUE=fedofName, &
              default_value=DEFAULT_FEDOFNAME, origin=origin, &
              stat=stat, isFound=isFound)

! Get the node from toml table [subtable]
CALL toml_get(table, fedofName%chars(), node, &
              origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  fedofName//" node not found")
#endif

! Now we can init fedof from toml
CALL timefedof%ImportFromToml(table=node, timeOpt=timeOpt)

node => NULL(); key = ""; fedofName = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractFieldReadTimeFEDOFFromToml1

!----------------------------------------------------------------------------
!                                         AbstractFieldReadTimeFEDOFFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadTimeFEDOFFromToml2
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadTimeFEDOFFromToml2()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE AbstractFieldReadTimeFEDOFFromToml2

!----------------------------------------------------------------------------
!                                      AbstractFieldReadUserFunctionFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadUserFunctionFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadUserFunctionFromToml()"
#endif

TYPE(toml_table), POINTER :: node => NULL()
INTEGER(I4B) :: stat, origin
TYPE(String) :: astr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%exact => NULL()

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading exact...')
#endif

astr = "exact"
CALL toml_get(table, astr%chars(), node, origin=origin, &
              requested=.FALSE., stat=stat)
isok = ASSOCIATED(node)
IF (.NOT. isok) RETURN

ALLOCATE (obj%exact)
CALL obj%exact%ImportFromToml(table=node)

! errorNorm
CALL GetValue(table=node, key="errorNorm", VALUE=obj%saveErrorNorm, &
              default_value=.FALSE., stat=stat, origin=origin, isfound=isok)

! normType
CALL GetValue(table=node, key="normType", VALUE=astr, &
              default_value="L2SP", stat=stat, origin=origin, isfound=isok)

obj%errorType = UpperCase(astr%slice(1, 4))

! plotWithResult
CALL GetValue(table=node, key="plotWithResult", VALUE=obj%plotWithResult, &
              default_value=.FALSE., stat=stat, origin=origin, isfound=isok)

! plotErrorNorm
CALL GetValue(table=node, key="plotErrorNorm", VALUE=obj%plotErrorNorm, &
              default_value=.FALSE., stat=stat, origin=origin, isfound=isok)

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE AbstractFieldReadUserFunctionFromToml

!----------------------------------------------------------------------------
!                                                AbstractFieldReadDBCFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadDBCFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadDBCFromToml()"
#endif

LOGICAL(LGT) :: isok
CLASS(AbstractDomain_), POINTER :: dom

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dom => NULL()
isok = ASSOCIATED(obj%fedof)
IF (isok) THEN
  dom => obj%fedof%GetDomainPointer()
END IF

#ifdef DEBUG_VER
isok = ALLOCATED(obj%fedofs)
IF (isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
 '[WIP ERROR] :: Currently this routine cannot be used with multiple domains')
END IF
#endif

CALL DirichletBCImportFromToml(obj=obj%dbc, tomlName="dirichletBC", &
                               table=table, dom=dom)
dom => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE AbstractFieldReadDBCFromToml

!----------------------------------------------------------------------------
!                                                AbstractFieldReadNBCFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadNBCFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadNBCFromToml()"
#endif

LOGICAL(LGT) :: isok
CLASS(AbstractDomain_), POINTER :: dom

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dom => NULL()
isok = ASSOCIATED(obj%fedof)
IF (isok) THEN
  dom => obj%fedof%GetDomainPointer()
END IF

#ifdef DEBUG_VER
isok = ALLOCATED(obj%fedofs)
IF (isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
 '[WIP ERROR] :: Currently this routine cannot be used with multiple domains')
END IF
#endif

CALL NeumannBCImportFromToml(obj=obj%nbc, tomlName="neumannBC", &
                             table=table, dom=dom)
dom => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE AbstractFieldReadNBCFromToml

!----------------------------------------------------------------------------
!                                           AbstractFieldReadPointNBCFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldReadPointNBCFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadPointNBCFromToml()"
#endif

LOGICAL(LGT) :: isok
CLASS(AbstractDomain_), POINTER :: dom

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dom => NULL()
isok = ASSOCIATED(obj%fedof)
IF (isok) THEN
  dom => obj%fedof%GetDomainPointer()
END IF

#ifdef DEBUG_VER
isok = ALLOCATED(obj%fedofs)
IF (isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
 '[WIP ERROR] :: Currently this routine cannot be used with multiple domains')
END IF
#endif

CALL NeumannBCImportFromToml(obj=obj%nbc_point, tomlName="pointNeumannBC", &
                             table=table, dom=dom)
dom => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE AbstractFieldReadPointNBCFromToml

!----------------------------------------------------------------------------
!                                               SetAbstractFieldParamFromToml
!----------------------------------------------------------------------------

! MODULE PROCEDURE SetAbstractFieldParamFromToml
! CHARACTER(*), PARAMETER :: myName = "SetAbstractFieldParamFromToml()"
! CHARACTER(*), PARAMETER :: default_engine = TypeEngineName%native_serial
! CHARACTER(*), PARAMETER :: default_fieldTypeChar = TypeField%normal_char
!
! CHARACTER(:), ALLOCATABLE :: key
! CHARACTER(1), ALLOCATABLE :: physicalVarNamesChar(:)
! TYPE(String) :: name, engine, fieldTypeChar
! TYPE(String), ALLOCATABLE :: physicalVarNames(:)
! INTEGER(I4B) :: fieldType, origin, stat, tPhysicalVarNames, ii
! INTEGER(I4B), ALLOCATABLE :: spaceCompo(:), timeCompo(:)
! LOGICAL(LGT) :: isfound, isSpaceCompo, isTimeCompo, isSpaceCompoScalar, &
!                isTimeCompoScalar, isPhysicalVarNames, isPhysicalVarNamesScalar
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! key = "name"
! !============
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         'Reading '//key//" ...")
! #endif
! CALL GetValue(table=table, key=key, VALUE=name, &
!               default_value=prefix, origin=origin, &
!               stat=stat, isFound=isfound)
!
! #ifdef DEBUG_VER
! CALL AssertError1(isfound, myName, &
!                   key//" not found in the toml file")
! #endif
!
! key = "engine"
! !=============
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         'Reading '//key//" ...")
! #endif
! CALL GetValue(table=table, key=key, VALUE=engine, &
!               default_value=default_engine, origin=origin, &
!               stat=stat, isFound=isfound)
! #ifdef DEBUG_VER
! CALL AssertError1(isfound, myName, &
!                   key//" not found in the toml file")
! #endif
!
! key = "fieldType"
! !================
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         'Reading '//key//" ...")
! #endif
! CALL GetValue(table=table, key=key, VALUE=fieldTypeChar, &
!               default_value=default_fieldTypeChar, origin=origin, &
!               stat=stat, isFound=isfound)
! #ifdef DEBUG_VER
! CALL AssertError1(isfound, myName, &
!                   key//" not found in the toml file")
! #endif
!
! fieldType = TypeField%ToNumber(fieldTypeChar%chars())
!
! key = "spaceCompo"
! !================
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         'Reading '//key//" ...")
! #endif
! CALL GetValue(table=table, key=key, VALUE=spaceCompo, &
!               origin=origin, stat=stat, isFound=isSpaceCompo, &
!               isScalar=isSpaceCompoScalar)
!
! key = "timeCompo"
! !================
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         'Reading '//key//" ...")
! #endif
! CALL GetValue(table=table, key=key, VALUE=timeCompo, &
!               origin=origin, stat=stat, isFound=isTimeCompo, &
!               isScalar=isTimeCompoScalar)
!
! key = "physicalVarNames"
! !========================
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         'Reading '//key//' ...')
! #endif
! CALL GetValue(table=table, key=key, VALUE=physicalVarNames, &
!               origin=origin, stat=stat, isFound=isPhysicalVarNames, &
!               isScalar=isPhysicalVarNamesScalar)
! tPhysicalVarNames = 0
! IF (isPhysicalVarNames) tPhysicalVarNames = SIZE(physicalVarNames)
! ALLOCATE (physicalVarNamesChar(tPhysicalVarNames))
! DO ii = 1, tPhysicalVarNames
!   physicalVarNamesChar(ii) = physicalVarNames(ii)%slice(1, 1)
! END DO
!
! CALL SetAbstractFieldParam(param=param, name=name%chars(), &
!                            engine=engine%chars(), fieldType=fieldType, &
!                            prefix=prefix, comm=comm, local_n=local_n, &
!                            global_n=global_n, spaceCompo=spaceCompo, &
!                            isSpaceCompo=isSpaceCompo, &
!                            isSpaceCompoScalar=isSpaceCompoScalar, &
!                            timeCompo=timeCompo, isTimeCompo=isTimeCompo, &
!                            isTimeCompoScalar=isTimeCompoScalar, &
!                            physicalVarNames=physicalVarNamesChar, &
!                            tPhysicalVarNames=tPhysicalVarNames, &
!                            isPhysicalVarNames=isPhysicalVarNames)
!
! name = ""; engine = ""; fieldTypeChar = ""; key = ""
! DEALLOCATE (physicalVarNamesChar)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif
!
! END PROCEDURE SetAbstractFieldParamFromToml

!----------------------------------------------------------------------------
!                                                                    Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
