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

SUBMODULE(AbstractField_Class) IOMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE TomlUtility, ONLY: GetValue
USE Display_Method, ONLY: Display, ToString
USE FPL, ONLY: FPL_INIT, FPL_FINALIZE
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

CALL Display(msg, unitNo=unitNo)

CALL Display(obj%isInitiated, msg="isInitiated: ", unitNo=unitNo)
IF (.NOT. obj%isInitiated) RETURN

CALL Display(obj%name%chars(), msg="name : ", unitNo=unitNo)

IF (obj%fieldType .EQ. TypeField%normal) THEN
  CALL Display("fieldType : CONSTANT", unitNo=unitNo)
ELSE
  CALL Display("fieldType : NORMAL", unitNo=unitNo)
END IF

CALL Display(obj%engine%chars(), msg='engine : ', unitNo=unitNo)
CALL Display(obj%comm, msg='comm: ', unitNo=unitNo)
CALL Display(obj%myRank, msg='myRank: ', unitNo=unitNo)
CALL Display(obj%numProcs, msg='numProcs: ', unitNo=unitNo)
CALL Display(obj%global_n, msg='global_n: ', unitNo=unitNo)
CALL Display(obj%local_n, msg='local_n: ', unitNo=unitNo)
CALL Display(obj%is, msg='is: ', unitNo=unitNo)
CALL Display(obj%ie, msg='ie: ', unitNo=unitNo)
CALL Display(obj%lis_ptr, msg='lis_ptr: ', unitNo=unitNo)

isok = ASSOCIATED(obj%fedof)
CALL Display(isok, "fedof ASSOCIATED: ", unitNo=unitNo)

isok = ASSOCIATED(obj%timefedof)
CALL Display(isok, "timefedof ASSOCIATED: ", unitNo=unitNo)

isok = ALLOCATED(obj%fedofs)
CALL Display(isok, "fedofs ALLOCATED: ", unitNo=unitNo)
tsize = 0
IF (isok) THEN
  tsize = SIZE(obj%fedofs)
  CALL Display("fedofs : ALLOCATED ["//ToString(tsize)//"]", unitNo=unitNo)
END IF

! tsize is zero if not allocated, so it is safe to use
DO ii = 1, tsize
  isok = ASSOCIATED(obj%fedofs(ii)%ptr)
  CALL Display(isok, "fedofs("//ToString(ii)//")%ptr ASSOCIATED: ", &
               unitNo=unitNo)
END DO

isok = ALLOCATED(obj%timefedofs)
CALL Display(isok, "timefedofs ALLOCATED: ", unitNo=unitNo)
tsize = 0
IF (isok) THEN
  tsize = SIZE(obj%timefedofs)
 CALL Display("timefedofs : ALLOCATED ["//ToString(tsize)//"]", unitNo=unitNo)
END IF

! tsize is zero if not allocated, so it is safe to use
DO ii = 1, tsize
  isok = ASSOCIATED(obj%timefedofs(ii)%ptr)
  CALL Display(isok, "timefedofs("//ToString(ii)//")%ptr ASSOCIATED: ", &
               unitNo=unitNo)
END DO

isok = ASSOCIATED(obj%exact)
CALL Display(isok, "Userfunction exact is associated: ", unitNo=unitNo)
IF (isok) THEN
  CALL obj%exact%Display(msg="exact: ", unitNo=unitNo)
END IF

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_hdf5
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_hdf5()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[IMPLEMENTATION ERROR] :: This method should be implemented '// &
                  'by children of AbstractField_')
END PROCEDURE obj_WriteData_hdf5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[IMPLEMENTATION ERROR] :: This method should be implemented by'// &
                  ' children of AbstractField_')
END PROCEDURE obj_WriteData_vtk

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
TYPE(String) :: dname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
              '[INTERNAL ERROR] :: Instnace of MatrixField_ is not initiated')
END IF

! Check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

! Check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: HDF5 file does not have write permission')
END IF

! fieldType
dname = TRIM(group)//"/fieldType"
CALL hdf5%WRITE(dsetname=dname%chars(), &
                vals=STRING(TypeField%ToString(obj%fieldType)))

! name
dname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%name)

! engine
dname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%engine)

! comm
dname = TRIM(group)//"/comm"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%comm)

! myRank
dname = TRIM(group)//"/myRank"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%myRank)

! numProcs
dname = TRIM(group)//"/numProcs"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%numProcs)

! local_n
dname = TRIM(group)//"/local_n"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%local_n)

! global_n
dname = TRIM(group)//"/global_n"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%global_n)

! is
dname = TRIM(group)//"/is"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%is)

! ie
dname = TRIM(group)//"/ie"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%ie)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                             obj_Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: strval, dsetname
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main program
IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    '[INTERNAL ERROR] :: The instance of AbstractField_ is already initiated')
  RETURN
END IF

! Check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
  RETURN
END IF

! Check
IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                '[INTERNAL ERROR] :: HDF5 file does not have read permission')
  RETURN
END IF

! fieldType
dsetname = TRIM(group)//"/fieldType"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
  obj%fieldType = TypeField%ToNumber(strval%chars())
ELSE
  obj%fieldType = TypeField%normal
END IF

! name
dsetname = TRIM(group)//"/name"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: The dataset name should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)
END IF

! engine
dsetname = TRIM(group)//"/engine"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%engine = "NATIVE_SERIAL"
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%engine)
END IF

! comm
dsetname = TRIM(group)//"/comm"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%comm = 0
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%comm)
END IF

! myRank
dsetname = TRIM(group)//"/myRank"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%myRank = 0
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%myRank)
END IF

! numProcs
dsetname = TRIM(group)//"/numProcs"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%numProcs = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%numProcs)
END IF

! global_n
dsetname = TRIM(group)//"/global_n"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%global_n = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%global_n)
END IF

! local_n
dsetname = TRIM(group)//"/local_n"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%local_n = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%local_n)
END IF

! is
dsetname = TRIM(group)//"/is"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%is = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%is)
END IF

! ie
dsetname = TRIM(group)//"/ie"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  obj%ie = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%ie)
END IF

IF (ASSOCIATED(obj%fedof)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: obj%fedof is associated, deallocate first')
  RETURN
END IF

IF (ALLOCATED(obj%fedofs)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: obj%fedofs is allocated, deallocate first')
  RETURN
END IF

IF (PRESENT(fedof)) THEN
  obj%fedof => fedof

ELSE IF (PRESENT(fedofs)) THEN

  tsize = SIZE(fedofs)

  ALLOCATE (obj%fedofs(tsize))

  DO ii = 1, tsize
    obj%fedofs(ii)%ptr => fedofs(ii)%ptr
  END DO

ELSE

  CALL e%RaiseError(modName//'::'//myName//" - "// &
    "[INTERNAL ERROR] :: For non-rectangle matrix dom should be present, "// &
                    "for rectangle matrix matrix fedofs should be present")
  RETURN

END IF

obj%isInitiated = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

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

CALL FPL_Init
CALL param%Initiate()

prefix = obj%GetPrefix()
CALL SetAbstractFieldParamFromToml(param=param, table=table, prefix=prefix)

CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, mesh=mesh)

CALL AbstractFieldReadTimeFEDOFFromToml(table=table, timefedof=timefedof, &
                                        timeOpt=timeOpt)

CALL obj%Initiate(param=param, fedof=fedof, timefedof=timefedof)

CALL param%DEALLOCATE()
CALL FPL_Finalize

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
! internal variables
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
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

CALL FPL_Init
CALL param%Initiate()

prefix = obj%GetPrefix()
CALL SetAbstractFieldParamFromToml(param=param, table=table, prefix=prefix)

CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, mesh=mesh)

isok = PRESENT(timefedof)
IF (isok) CALL AbstractFieldReadTimeFEDOFFromToml(table=table, &
                                         timefedof=timefedof, timeOpt=timeOpt)

CALL obj%Initiate(param=param, fedof=fedof, timefedof=timefedof)

CALL param%DEALLOCATE()
CALL FPL_Finalize

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

CALL FPL_Init
CALL param%Initiate()

prefix = obj%GetPrefix()
CALL SetAbstractFieldParamFromToml(param=param, table=table, prefix=prefix)

CALL AbstractFieldReadFEDOFFromToml(table=table, fedof=fedof, mesh=mesh)

isok = PRESENT(timefedof)
IF (isok) CALL AbstractFieldReadTimeFEDOFFromToml(table=table, &
                                         timefedof=timefedof, timeOpt=timeOpt)

CALL obj%Initiate(param=param, fedof=fedof, timefedof=timefedof)

CALL param%DEALLOCATE()
CALL FPL_Finalize

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
    origin=origin, stat=stat, isFound=isTimeCompo, isScalar=isTimeCompoScalar)

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
                           isPhysicalVarNames=isPhysicalVarNames &
                           )

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
CHARACTER(*), PARAMETER :: myName = "AbstractFieldReadFEDOFFromToml1()"
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

! First lets check if mesh if given or not
isok = PRESENT(mesh)
CALL AssertError1(isok, myName, &
                  "Mesh is not given to initiate fedof")

! Getting fedofName which is a subtable name in
! toml file that contains data for fedof
key = "fedofName"
CALL GetValue(table=table, key=key, VALUE=fedofName, &
              default_value=default_fedofName, origin=origin, &
              stat=stat, isFound=isFound)

! Get the node from toml table [subtable]
CALL toml_get(table, fedofName%chars(), node, &
              origin=origin, requested=.FALSE., stat=stat)

isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  fedofName//" node not found")

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
!                                         AbstractFieldReadTimeFEDOFFromToml
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
!                                                                    Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
