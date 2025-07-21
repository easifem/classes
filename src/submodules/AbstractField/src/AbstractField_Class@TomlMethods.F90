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
                 toml_serialize
USE StringUtility, ONLY: UpperCase

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

TYPE(ParameterList_) :: param
CHARACTER(:), ALLOCATABLE :: prefix
! INTEGER(I4B) :: comm, local_n, global_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL param%Initiate()

prefix = obj%GetPrefix()
CALL SetAbstractFieldParamFromToml(param=param, table=table, prefix=prefix)

CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, mesh=mesh)

CALL AbstractFieldReadTimeFEDOFFromToml(table=table, timefedof=timefedof, &
                                        timeOpt=timeOpt)

CALL obj%Initiate(param=param, fedof=fedof, timefedof=timefedof)

CALL AbstractFieldReadUserFunctionFromToml(obj=obj, table=table)

CALL param%DEALLOCATE()

prefix = ""

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

CALL obj%ImportFromToml(table=node, fedof=fedof, timefedof=timefedof, &
                        mesh=mesh, timeOpt=timeOpt)

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

TYPE(ParameterList_) :: param
CHARACTER(:), ALLOCATABLE :: prefix
LOGICAL(LGT) :: isok
! INTEGER(I4B) :: comm, local_n, global_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL param%Initiate()

prefix = obj%GetPrefix()
CALL SetAbstractFieldParamFromToml(param=param, table=table, prefix=prefix)

CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, mesh=mesh)

isok = PRESENT(timefedof)
IF (isok) CALL AbstractFieldReadTimeFEDOFFromToml(table=table, &
                                         timefedof=timefedof, timeOpt=timeOpt)

CALL obj%Initiate(param=param, fedof=fedof, timefedof=timefedof)

CALL AbstractFieldReadUserFunctionFromToml(obj=obj, table=table)

CALL param%DEALLOCATE()

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml3

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml4
! internal variables
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml4()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

isok = ALLOCATED(table)
CALL AssertError1(isok, myName, "table is not allocated from GetValue")

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")

CALL obj%ImportFromToml(table=node, fedof=fedof, timefedof=timefedof, &
                        mesh=mesh, timeOpt=timeOpt)

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

END PROCEDURE obj_ImportFromToml4

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml5()"
#endif

TYPE(ParameterList_) :: param
CHARACTER(:), ALLOCATABLE :: prefix
LOGICAL(LGT) :: isok
! INTEGER(I4B) :: comm, local_n, global_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL param%Initiate()

prefix = obj%GetPrefix()
CALL SetAbstractFieldParamFromToml(param=param, table=table, prefix=prefix)

CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, mesh=mesh)

isok = PRESENT(timefedof)
IF (isok) CALL AbstractFieldReadTimeFEDOFFromToml(table=table, &
                                         timefedof=timefedof, timeOpt=timeOpt)

CALL obj%Initiate(param=param, fedof=fedof, timefedof=timefedof)

CALL AbstractFieldReadUserFunctionFromToml(obj=obj, table=table)

CALL param%DEALLOCATE()

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml5

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml6
! internal variables
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml6()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

isok = ALLOCATED(table)
CALL AssertError1(isok, myName, "table is not allocated from GetValue")

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")

CALL obj%ImportFromToml(table=node, fedof=fedof, timefedof=timefedof, &
                        mesh=mesh, timeOpt=timeOpt)

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

END PROCEDURE obj_ImportFromToml6

!----------------------------------------------------------------------------
!                                               SetAbstractFieldParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractFieldParamFromToml
CHARACTER(*), PARAMETER :: myName = "SetAbstractFieldParamFromToml()"
CHARACTER(*), PARAMETER :: default_engine = TypeEngineName%native_serial
CHARACTER(*), PARAMETER :: default_fieldTypeChar = TypeField%normal_char

CHARACTER(:), ALLOCATABLE :: key
CHARACTER(1), ALLOCATABLE :: physicalVarNamesChar(:)
TYPE(String) :: name, engine, fieldTypeChar
TYPE(String), ALLOCATABLE :: physicalVarNames(:)
INTEGER(I4B) :: fieldType, origin, stat, tPhysicalVarNames, ii
INTEGER(I4B), ALLOCATABLE :: spaceCompo(:), timeCompo(:)
LOGICAL(LGT) :: isfound, isSpaceCompo, isTimeCompo, isSpaceCompoScalar, &
               isTimeCompoScalar, isPhysicalVarNames, isPhysicalVarNamesScalar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

key = "name"
!============
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//key//" ...")
#endif
CALL GetValue(table=table, key=key, VALUE=name, &
              default_value=prefix, origin=origin, &
              stat=stat, isFound=isfound)
CALL AssertError1(isfound, myName, &
                  key//" not found in the toml file")

key = "engine"
!=============
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
!================
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//key//" ...")
#endif
CALL GetValue(table=table, key=key, VALUE=fieldTypeChar, &
              default_value=default_fieldTypeChar, origin=origin, &
              stat=stat, isFound=isfound)
CALL AssertError1(isfound, myName, &
                  key//" not found in the toml file")
fieldType = TypeField%ToNumber(fieldTypeChar%chars())

key = "spaceCompo"
!================
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//key//" ...")
#endif
CALL GetValue(table=table, key=key, VALUE=spaceCompo, &
              origin=origin, stat=stat, isFound=isSpaceCompo, &
              isScalar=isSpaceCompoScalar)

key = "timeCompo"
!================
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//key//" ...")
#endif
CALL GetValue(table=table, key=key, VALUE=timeCompo, &
              origin=origin, stat=stat, isFound=isTimeCompo, &
              isScalar=isTimeCompoScalar)

key = "physicalVarNames"
!========================
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//key//' ...')
#endif
CALL GetValue(table=table, key=key, VALUE=physicalVarNames, &
              origin=origin, stat=stat, &
              isFound=isPhysicalVarNames, &
              isScalar=isPhysicalVarNamesScalar)
tPhysicalVarNames = 0
IF (isPhysicalVarNames) tPhysicalVarNames = SIZE(physicalVarNames)
ALLOCATE (physicalVarNamesChar(tPhysicalVarNames))
DO ii = 1, tPhysicalVarNames
  physicalVarNamesChar(ii) = physicalVarNames(ii)%slice(1, 1)
END DO

CALL SetAbstractFieldParam(param=param, name=name%chars(), &
                           engine=engine%chars(), &
                           fieldType=fieldType, &
                           prefix=prefix, &
                           comm=comm, local_n=local_n, global_n=global_n, &
                           spaceCompo=spaceCompo, &
                           isSpaceCompo=isSpaceCompo, &
                           isSpaceCompoScalar=isSpaceCompoScalar, &
                           timecompo=timecompo, &
                           isTimeCompo=isTimeCompo, &
                           isTimeCompoScalar=isTimeCompoScalar, &
                           physicalVarNames=physicalVarNamesChar, &
                           tPhysicalVarNames=tPhysicalVarNames, &
                           isPhysicalVarNames=isPhysicalVarNames)

name = ""; engine = ""; fieldTypeChar = ""; key = ""
DEALLOCATE (physicalVarNamesChar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetAbstractFieldParamFromToml

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
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
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
isok = PRESENT(mesh)
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
CALL fedof%ImportFromToml(table=node, mesh=mesh)

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
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
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
                                      fedof=fedof(ii)%ptr, mesh=mesh)

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
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
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
tmesh = SIZE(mesh)
isok = tmesh .EQ. tsize
CALL AssertError1(isok, myName, &
                  "mesh size does not match physicalVarNames size")

DO ii = 1, tsize
  isok = ASSOCIATED(mesh(ii)%ptr)
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
                                      fedof=fedof(ii)%ptr, mesh=mesh(ii)%ptr)

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
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'timefedof is not present, nothing to do')

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN

END IF

isfedof = timefedof%IsInitiated()

IF (isfedof) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
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
! internal variables
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadUserFunctionFromToml()"

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
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
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
              default_value="L2SP", stat=stat, &
              origin=origin, isfound=isok)

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
!                                                                    Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
