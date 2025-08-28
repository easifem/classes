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

SUBMODULE(AbstractBC_Class) TomlMethods
USE GlobalData, ONLY: CHAR_LF, stdout
USE Display_Method, ONLY: Display, ToString

USE FieldOpt_Class, ONLY: TypeFieldOpt
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt

USE TomlUtility, ONLY: GetValue

USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_stat
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportParamFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportParamFromToml()"
#endif

INTEGER(I4B) :: origin, stat, nodalValueType, idof
LOGICAL(LGT) :: isNormal, isTangent, isUserFunction, isUseExternal
TYPE(String) :: nodalValueType_string, name, astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, key="isUserFunction", VALUE=isUserFunction, &
              default_value=default_isUserFunction, origin=origin, stat=stat)

CALL GetValue(table=table, key="isTangent", VALUE=isTangent, &
              default_value=default_isTangent, origin=origin, stat=stat)

CALL GetValue(table=table, key="isNormal", VALUE=isNormal, &
              default_value=default_isNormal, origin=origin, stat=stat)

CALL GetValue(table=table, key="isUseExternal", VALUE=isUseExternal, &
              default_value=default_useExternal, origin=origin, stat=stat)

CALL GetValue(table=table, key="nodalValueType", &
              VALUE=nodalValueType_string, &
              default_value=default_nodalValueType_char, &
              origin=origin, stat=stat)

nodalValueType = TypeFieldOpt%ToNumber(nodalValueType_string%chars())

CALL GetValue(table=table, key="idof", VALUE=idof, &
              default_value=default_idof, origin=origin, stat=stat)

CALL GetValue(table=table, key="name", VALUE=name, &
              default_value=obj%GetPrefix(), origin=origin, stat=stat)

CALL SetAbstractBCParam(param=param, &
                        prefix=obj%GetPrefix(), &
                        name=name%chars(), &
                        idof=idof, &
                        nodalValueType=nodalValueType, &
                        isUserFunction=isUserFunction, &
                        isNormal=isNormal, &
                        isTangent=isTangent, &
                        isUseExternal=isUseExternal)

name = ''
nodalValueType_string = ''
astr = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                           ReadNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadNameFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadNameFromToml()"
#endif
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading name ...')
#endif

  CALL GetValue(table=table, key="name", VALUE=obj%name, &
                default_value=obj%GetPrefix(), origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadNameFromToml

!----------------------------------------------------------------------------
!                                                           ReadIdofFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIdofFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIdofFromToml()"
#endif
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading idof ...')
#endif

  CALL GetValue(table=table, key="idof", VALUE=obj%idof, &
                default_value=default_idof, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadIdofFromToml

!----------------------------------------------------------------------------
!                                                  ReadNodalValueTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadNodalValueTypeFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadNodalValueTypeFromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading nodalValueType ...')
#endif

  CALL GetValue(table=table, key="nodalValueType", VALUE=astr, &
                default_value=default_nodalValueType_char, &
                origin=origin, stat=stat)

  obj%nodalValueType = TypeFieldOpt%ToNumber(astr%chars())
  astr = ''

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadNodalValueTypeFromToml

!----------------------------------------------------------------------------
!                                                        ReadIsNormalFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsNormalFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsNormalFromToml()"
#endif
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isNormal ...')
#endif

  CALL GetValue(table=table, key="isNormal", VALUE=obj%isNormal, &
                default_value=default_isNormal, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ReadIsNormalFromToml

!----------------------------------------------------------------------------
!                                                       ReadIsTangentFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsTangentFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsTangentFromToml()"
#endif
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isTangent ...')
#endif

  CALL GetValue(table=table, key="isTangent", VALUE=obj%isTangent, &
                default_value=default_isTangent, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadIsTangentFromToml

!----------------------------------------------------------------------------
!                                                   ReadIsUseExternalFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsUseExternalFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsUseExternalFromToml()"
#endif
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isUseExternal ...')
#endif

  CALL GetValue(table=table, key="isUseExternal", VALUE=obj%isUseExternal, &
                default_value=default_useExternal, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadIsUseExternalFromToml

!----------------------------------------------------------------------------
!                                                  ReadIsUserFunctionFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsUserFunctionFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsUserFunctionFromToml()"
#endif
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isUserFunction ...')
#endif

  CALL GetValue(table=table, key="isUserFunction", VALUE=obj%isUserFunction, &
               default_value=default_isUserFunction, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadIsUserFunctionFromToml

!----------------------------------------------------------------------------
!                                                        ReadBoundaryFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadBoundaryFromToml(obj, table, dom)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadBoundaryFromToml()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: origin, stat
  TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading boundary ...')
#endif

  node => NULL()
  CALL toml_get(table, "boundary", node, origin=origin, requested=.FALSE., &
                stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, 'following error occured while reading &
                   &the toml file :: cannot find [boundary] table in config.')
#endif

  CALL obj%boundary%ImportFromToml(table=node, dom=dom)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadBoundaryFromToml

!----------------------------------------------------------------------------
!                                                   ReadUserFunctionFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadUserFunctionFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadUserFunctionFromToml()"
  LOGICAL(LGT) :: isok
#endif
  INTEGER(I4B) :: origin, stat
  TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (.NOT. obj%isUserFunction) RETURN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading function ...')
#endif

  node => NULL()
  CALL toml_get(table, "function", node, origin=origin, requested=.FALSE., &
                stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, 'following error occured while reading &
                   &the toml file :: cannot find [function] table in config.')
#endif

  ALLOCATE (obj%func)
  CALL obj%func%ImportFromToml(table=node)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadUserFunctionFromToml

!----------------------------------------------------------------------------
!                                              ReadConstantNodalValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadConstantNodalValueFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadConstantNodalValueFromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  REAL(DFP) :: areal
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading constant nodal value ...')
#endif

  IF (obj%isUserFunction) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  isok = obj%nodalValueType .EQ. fevaropt%constant

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  CALL GetValue(table=table, key="value", VALUE=areal, &
                origin=origin, stat=stat, default_value=0.0_DFP, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
    'value not found in toml file. It should be present if &
     &nodalValueType is Constant.')
#endif

  CALL obj%Set(constantnodalValue=areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadConstantNodalValueFromToml

!----------------------------------------------------------------------------
!                                              ReadConstantNodalValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadSpaceNodalValueFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadSpaceNodalValueFromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  REAL(DFP), ALLOCATABLE :: areal(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading space nodal value ...')
#endif

  IF (obj%isUserFunction) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  isok = obj%nodalValueType .EQ. fevaropt%space

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  CALL GetValue(table=table, key="value", VALUE=areal, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, 'nodalValueType is Space.&
       &obj%isUserFunction is False. So, value should be a vector of real &
       &numbers. You can specify a vector by directly giving the vector &
       &values. Otherwise, specify filename which contains vector values.')
#endif

  CALL obj%Set(spaceNodalValue=areal)

  IF (ALLOCATED(areal)) DEALLOCATE (areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadSpaceNodalValueFromToml

!----------------------------------------------------------------------------
!                                                 ReadTimeNodalValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadTimeNodalValueFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadTimeNodalValueFromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  REAL(DFP), ALLOCATABLE :: areal(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading time nodal value ...')
#endif

  IF (obj%isUserFunction) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  isok = obj%nodalValueType .EQ. fevaropt%time

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  CALL GetValue(table=table, key="value", VALUE=areal, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, 'nodalValueType is Time.&
       &obj%isUserFunction is False. So, value should be a vector of real &
       &numbers. You can specify a vector by directly giving the vector &
       &values. Otherwise, specify filename which contains vector values.')
#endif

  CALL obj%Set(timeNodalValue=areal)

  IF (ALLOCATED(areal)) DEALLOCATE (areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadTimeNodalValueFromToml

!----------------------------------------------------------------------------
!                                             ReadSpaceTimeNodalValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadSpaceTimeNodalValueFromToml(obj, table)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadSpaceTimeNodalValueFromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  REAL(DFP), ALLOCATABLE :: areal(:, :)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading spaceTime nodal value ...')
#endif

  IF (obj%isUserFunction) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  isok = obj%nodalValueType .EQ. fevaropt%spaceTime

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  CALL GetValue(table=table, key="value", VALUE=areal, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, 'nodalValueType is spaceTime.&
       &obj%isUserFunction is False. So, value should be a vector of real &
       &numbers. You can specify a vector by directly giving the vector &
       &values. Otherwise, specify filename which contains vector values.')
#endif

  CALL obj%Set(spaceTimeNodalValue=areal)

  IF (ALLOCATED(areal)) DEALLOCATE (areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadSpaceTimeNodalValueFromToml

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%dom => dom
CALL ReadNameFromToml(obj=obj, table=table)
CALL ReadIdofFromToml(obj=obj, table=table)
CALL ReadNodalValueTypeFromToml(obj=obj, table=table)
CALL ReadIsNormalFromToml(obj=obj, table=table)
CALL ReadIsTangentFromToml(obj=obj, table=table)
CALL ReadIsUseExternalFromToml(obj=obj, table=table)
CALL ReadIsUserFunctionFromToml(obj=obj, table=table)
CALL ReadBoundaryFromToml(obj=obj, table=table, dom=dom)
CALL ReadUserFunctionFromToml(obj=obj, table=table)
! if .not. obj%isUserFunction, then we read nodalValueType
CALL ReadConstantNodalValueFromToml(obj=obj, table=table)
CALL ReadSpaceNodalValueFromToml(obj=obj, table=table)
CALL ReadTimeNodalValueFromToml(obj=obj, table=table)
CALL ReadSpaceTimeNodalValueFromToml(obj=obj, table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node, dom=dom)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
