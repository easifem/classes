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

SUBMODULE(AbstractBC_Class) IOMethods
USE BaseMethod
USE TomlUtility
USE tomlf, ONLY:  &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_array,  &
  & toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import"
TYPE(String) :: dsetname, strval
REAL(DFP) :: real0
REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "[START] Import()")
#endif

CALL obj%DEALLOCATE()

IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: AbstractBC_::obj is already initiated'//  &
  & ', deallocate first')
  RETURN
END IF
obj%isInitiated = .TRUE.
obj%dom => dom

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file is not opened')
  RETURN
END IF

IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file does not have read permission')
  RETURN
END IF

dsetname = TRIM(group)//"/name"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: The dataset name should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)
END IF

dsetname = TRIM(group)//"/idof"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: The dataset idof should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%idof)
END IF

dsetname = TRIM(group)//"/nodalValueType"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: The dataset nodalValueType should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
END IF

SELECT CASE (strval%chars())
CASE ("CONSTANT")
  obj%nodalValueType = Constant
CASE ("SPACE")
  obj%nodalValueType = Space
CASE ("TIME")
  obj%nodalValueType = Time
CASE ("SPACETIME")
  obj%nodalValueType = SpaceTime
END SELECT

dsetname = TRIM(group)//"/isUserFunction"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The dataset isUserFunction should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isUserFunction)
END IF

IF (obj%isUserFunction) THEN
  ALLOCATE (obj%func)
  dsetname = TRIM(group)//"/userFunction"
  CALL obj%func%IMPORT(hdf5=hdf5, group=dsetname%chars())
END IF

dsetname = TRIM(group)//"/useExternal"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The dataset useExternal should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%useExternal)
END IF

dsetname = TRIM(group)//"/boundary"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The dataset Boundary, which is a group, should be present')
ELSE
  CALL obj%boundary%IMPORT(hdf5=hdf5, group=dsetname%chars())
END IF

IF (.NOT. obj%isUserFunction) THEN
  dsetname = TRIM(group)//"/nodalValue"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The dataset nodalValue should be present')
  END IF
  SELECT CASE (obj%nodalValueType)
  CASE (Constant)
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=real0)
    CALL Reallocate(obj%nodalValue, 1, 1)
    obj%nodalValue = real0
  CASE (Space, Time)
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=real1)
    CALL Reallocate(obj%nodalValue, SIZE(real1), 1)
    obj%nodalValue(:, 1) = real1
  CASE (SpaceTime)
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=real2)
    obj%nodalValue = real2
  END SELECT
END IF

IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Import()')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export"
TYPE(String) :: dsetname, strval
REAL(DFP) :: real0
REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Export()')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: The object is not initiated, initiate it first!')
  RETURN
END IF

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file is not opened')
  RETURN
END IF

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: HDF5 file does not have write permission')
  RETURN
END IF

! WRITE name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

! WRITE idof
dsetname = TRIM(group)//"/idof"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%idof)

! WRITE nodalValueType
dsetname = TRIM(group)//"/nodalValueType"
SELECT CASE (obj%nodalValueType)
CASE (Constant)
  strval = "CONSTANT"
CASE (Space)
  strval = "SPACE"
CASE (Time)
  strval = "TIME"
CASE (SpaceTime)
  strval = "SPACETIME"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

! WRITE isUserFunction
dsetname = TRIM(group)//"/isUserFunction"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isUserFunction)

IF (ASSOCIATED(obj%func)) THEN
  dsetname = TRIM(group)//"/userFunction"
  CALL obj%func%Export(hdf5=hdf5, group=dsetname%chars())
END IF

! WRITE useExternal
dsetname = TRIM(group)//"/useExternal"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%useExternal)

! WRITE Boundary
dsetname = TRIM(group)//"/boundary"
CALL obj%boundary%export(hdf5=hdf5, group=dsetname%chars())

! Read nodalValue
IF (.NOT. obj%isUserFunction) THEN

  dsetname = TRIM(group)//"/nodalValue"
  IF (.NOT. ALLOCATED(obj%nodalValue)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR] :: nodalValue is not allocated, '//  &
      & 'it seems nodalValue is not set')
    RETURN
  END IF

  SELECT CASE (obj%nodalValueType)
  CASE (Constant)
    real0 = obj%nodalValue(1, 1)
    CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=real0)
  CASE (Space, Time)
    real1 = obj%nodalValue(:, 1)
    CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=real1)
  CASE (SpaceTime)
    real2 = obj%nodalValue
    CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=real2)
  END SELECT

END IF

IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
TYPE(String) :: strval
REAL(DFP) :: real0
REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)

!> check
IF (.NOT. obj%isInitiated) THEN
  CALL Display("AbstractBC_::obj is not initiated, nothing to display", &
    & unitNo=unitNo)
  RETURN
END IF

CALL Display("name : "//TRIM(obj%name%chars()), unitNo=unitNo)
CALL Display(obj%idof, "idof : ", unitNo=unitNo)

SELECT CASE (obj%nodalValueType)
CASE (Constant)
  strval = "CONSTANT"
CASE (Space)
  strval = "SPACE"
CASE (Time)
  strval = "TIME"
CASE (SpaceTime)
  strval = "SPACETIME"
END SELECT
CALL Display("nodalValueType : "//TRIM(strval%chars()), unitNo=unitNo)

CALL Display(obj%isUserFunction, "isUserFunction : ", unitNo=unitNo)
CALL Display(obj%useExternal, "useExternal : ", unitNo=unitNo)
CALL obj%Boundary%Display(msg="Boundary : ", unitNo=unitNo)

IF (.NOT. obj%isUserFunction) THEN
  IF (.NOT. ALLOCATED(obj%nodalValue)) THEN
    CALL Display("nodalValue : NOT ALLOCATED", unitNo=unitNo)
  ELSE
    SELECT CASE (obj%nodalValueType)
    CASE (Constant)
      real0 = obj%nodalValue(1, 1)
      CALL Display(real0, "nodalValue : ", unitNo=unitNo)
    CASE (Space, Time)
      real1 = obj%nodalValue(:, 1)
      CALL Display(real1, "nodalValue : ", unitNo=unitNo, orient="col")
    CASE (SpaceTime)
      real2 = obj%nodalValue(:, :)
      CALL Display(real2, "nodalValue : ", unitNo=unitNo)
    END SELECT
  END IF
END IF

IF (obj%isUserFunction) THEN
  IF (ASSOCIATED(obj%func)) THEN
    CALL obj%func%Display("userFunction", unitNo=unitNo)
  ELSE
    CALL Display("userFunction :: NOT ASSOCIATEDS", unitNo=unitNo)
  END IF
END IF

IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportParamFromToml
CHARACTER(*), PARAMETER :: myName = "obj_ImportParamFromToml()"
INTEGER(I4B) :: origin, stat, nodalValueType, idof
LOGICAL(LGT) :: isNormal, isTangent, isUserFunction, useExternal
TYPE(String) :: nodalValueType_string, name, astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL toml_get(table, "isUserFunction", isUserFunction,  &
  & default_isUserFunction, origin=origin, stat=stat)

CALL toml_get(table, "isTangent", isTangent,  &
  & default_isTangent, origin=origin, stat=stat)

CALL toml_get(table, "isNormal", isNormal,  &
  & default_isNormal, origin=origin, stat=stat)

CALL toml_get(table, "useExternal", useExternal,  &
  & default_useExternal, origin=origin, stat=stat)

CALL toml_get(table, "nodalValueType", nodalValueType_string%raw,  &
  & default_nodalValueType_char, origin=origin, stat=stat)

CALL toml_get(table, "idof", idof, default_idof, origin=origin, stat=stat)

CALL toml_get(table, "name", name%raw,  &
  & obj%GetPrefix(), origin=origin, stat=stat)

astr = nodalValueType_string%Upper()
SELECT CASE (astr%chars())
CASE ("CONSTANT")
  nodalValueType = Constant
CASE ("TIME")
  nodalValueType = Time
CASE ("SPACE")
  nodalValueType = Space
CASE ("SPACETIME")
  nodalValueType = SpaceTime
CASE DEFAULT
  nodalValueType = default_nodalValueType
END SELECT
astr = ""

CALL SetAbstractBCParam( &
  & param=param,  &
  & prefix=obj%GetPrefix(),  &
  & name=name%chars(),  &
  & idof=idof,  &
  & nodalValueType=nodalValueType,  &
  & isUserFunction=isUserFunction,  &
  & isNormal=isNormal,  &
  & isTangent=isTangent,  &
  & useExternal=useExternal &
  & )

name = ""
nodalValueType_string = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
TYPE(ParameterList_) :: param
TYPE(toml_table), POINTER :: node
TYPE(MeshSelection_) :: boundary
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: bool1, isFound
REAL(DFP), ALLOCATABLE :: value_r1(:), value_r2(:, :)
REAL(DFP) :: constantValue

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif
CALL param%Initiate()

CALL obj%ImportParamFromToml(param=param, table=table)

node => NULL()
CALL toml_get(table, "boundary", node, origin=origin, requested=.FALSE.,  &
  & stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: following error occured while reading '//  &
    & 'the toml file :: cannot find [boundary] table in config.')
END IF

CALL boundary%ImportFromToml(table=node, dom=dom)
CALL obj%Initiate(param=param, boundary=boundary, dom=dom)

IF (obj%isUserFunction) THEN
  node => NULL()
  CALL toml_get(table, "function", node, origin=origin, requested=.FALSE.,  &
    & stat=stat)

  IF (.NOT. ASSOCIATED(node)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: following error occured while reading '//  &
      & 'the toml file :: cannot find [function] table in config.')
    RETURN
  END IF

  ALLOCATE (obj%func)
  CALL obj%func%ImportFromToml(table=node)
  RETURN
END IF

SELECT CASE (obj%nodalValueType)
CASE (Constant)
  CALL toml_get(table, "value", constantValue, origin=origin, stat=stat)
  bool1 = stat .NE. toml_stat%success
  IF (bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: nodalValueType is Constant. '//  &
      & 'obj%isUserFunction is False. So, '// &
      & 'value should be a present (scalar real value).')
    RETURN
  END IF
  CALL obj%Set(constantnodalValue=constantValue)

CASE (Space, Time)
  CALL GetValue(table=table, key="value", VALUE=value_r1,  &
    & isFound=isFound, origin=origin, stat=stat)

  bool1 = (.NOT. isFound) .OR. (stat .NE. toml_stat%success)

  IF (bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: nodalValueType is Space or Time.'//  &
      & 'obj%isUserFunction is False. So, '// &
      & 'value should be a vector of real numbers.'//CHAR_LF// &
      & 'You can specify a vector by directly giving the vector values.'// &
      & 'Otherwise, specify filename which contains the vector'//  &
      & 'values.')
    RETURN
  END IF

  IF (obj%nodalValueType .EQ. Space) THEN
    CALL obj%Set(spacenodalValue=value_r1)
  ELSE
    CALL obj%Set(timenodalValue=value_r1)
  END IF

CASE (SpaceTime)
  CALL GetValue(table=table, key="value", VALUE=value_r2,  &
    & isFound=isFound, origin=origin, stat=stat)

  bool1 = (.NOT. isFound) .OR. (stat .NE. toml_stat%success)

  IF (bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: nodalValueType is Space or Time.'//  &
      & 'obj%isUserFunction is False. So, '// &
      & 'value should be a vector of real numbers.'//CHAR_LF// &
      & 'You can specify a vector by directly giving the vector values.'// &
      & 'Otherwise, specify filename which contains the vector'//  &
      & 'values.')
    RETURN
  END IF

  CALL obj%Set(spaceTimenodalValue=value_r2)

END SELECT

CALL param%DEALLOCATE()
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                '[CONFIG ERROR] :: following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
END IF

CALL obj%ImportFromToml(table=node, dom=dom)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
