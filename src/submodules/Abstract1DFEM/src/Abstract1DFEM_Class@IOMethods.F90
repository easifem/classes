! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(Abstract1DFEM_Class) IOMethods

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

USE GlobalData, ONLY: stdout, &
                      CHAR_LF

USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToInteger, &
                                    BaseType_ToInteger, &
                                    BaseType_ToChar, &
                                    BaseInterpolation_ToChar

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(obj%totalSpaceNodes, "totalSpaceNodes: ", unitno=unitno)
CALL Display(obj%totalSpaceElements, "totalSpaceElements: ", unitno=unitno)

CALL Display(obj%baseContinuityForSpace, "baseContinuityForSpace: ", &
             unitno=unitno)

CALL Display(obj%baseInterpolationForSpace, "baseInterpolationForSpace: ", &
             unitno=unitno)

astr = BaseType_ToChar(obj%baseTypeForSpace)
CALL Display(astr, "baseTypeForSpace: ", unitno=unitno)

astr = BaseInterpolation_TOChar(obj%ipTypeForSpace)
CALL Display(astr, "ipTypeForSpace: ", unitno=unitno)

astr = BaseInterpolation_ToChar(obj%quadTypeForSpace)
CALL Display(astr, "quadTypeForSpace: ", unitno=unitno)

CALL Display(obj%maxSpaceOrder, "maxSpaceOrder: ", unitno=unitno)
CALL Display(obj%spaceDomain, "spaceDomain: ", unitno=unitno)

isok = ALLOCATED(obj%spaceOrder)
CALL Display(isok, "spaceOrder: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%spaceOrder, "spaceOrder: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%totalDOFSpace)
CALL Display(isok, "totalDOFSpace: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%totalDOFSpace, "totalDOFSpace: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%spaceElemLength)
CALL Display(isok, "spaceElemLength: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%spaceElemLength, "spaceElemLength: ", unitno=unitno)
END IF

CALL Display(obj%result_dir%chars(), "result_dir: ", unitno=unitno)
CALL Display(obj%filename%chars(), "filename: ", unitno=unitno)

isok = ASSOCIATED(obj%bodyForce)
CALL Display(isok, "bodyForce ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%tractionRight)
CALL Display(isok, "tractionRight ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%tractionLeft)
CALL Display(isok, "tractionLeft ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%displacementLeft)
CALL Display(isok, "displacementLeft ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%displacementRight)
CALL Display(isok, "displacementRight ASSOCIATED: ", unitno=unitno)

CALL Display("Output Controll")
CALL Display(obj%saveData, "saveData: ", unitno=unitno)
CALL Display(obj%plotData, "plotData: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
! Internal variables
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
INTEGER(I4B) :: origin, stat, tsize
LOGICAL(LGT) :: isok, abool
TYPE(String) :: astr
INTEGER(I4B), ALLOCATABLE :: tempintvec(:)
REAL(DFP), ALLOCATABLE :: temprealvec(:)
TYPE(toml_table), POINTER :: node
TYPE(toml_array), POINTER :: array
LOGICAL(LGT), ALLOCATABLE :: tempboolvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
CALL Display(myName//" result_dir")
#endif

CALL GetValue(table=table, key="result_dir", VALUE=obj%result_dir, &
     default_value=default_result_dir, origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
CALL Display(myName//" filename")
#endif

CALL GetValue(table=table, key="filename", VALUE=obj%filename, &
       origin=origin, stat=stat, isfound=isok, default_value=default_filename)

CALL GetValue(table=table, key="verbosity", VALUE=obj%verbosity, &
      origin=origin, stat=stat, isfound=isok, default_value=default_verbosity)

#ifdef DEBUG_VER
CALL Display(myName//" totalSpaceNodes")
#endif

CALL GetValue(table=table, key="totalSpaceNodes", VALUE=obj%totalSpaceNodes, &
              default_value=0_I4B, origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
CALL Display(myName//" totalSpaceElements")
#endif

CALL GetValue(table=table, key="totalSpaceElements", &
              VALUE=obj%totalSpaceElements, &
              default_value=0_I4B, origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
CALL Display(myName//" baseInterpolationForSpace")
#endif

CALL GetValue(table=table, key="baseInterpolationForSpace", &
              VALUE=astr, default_value=default_baseInterpolationForSpace, &
              origin=origin, stat=stat, isfound=isok)
obj%baseInterpolationForSpace = UpperCase(astr%slice(1, 4))

#ifdef DEBUG_VER
CALL Display(myName//" baseTypeForSpace")
#endif

CALL GetValue(table=table, key="baseTypeForSpace", &
              VALUE=astr, default_value=default_baseTypeForSpace, &
              origin=origin, stat=stat, isfound=isok)
obj%baseTypeForSpace = BaseType_ToInteger(astr%chars())

#ifdef DEBUG_VER
CALL Display(myName//" ipTypeForSpace")
#endif

CALL GetValue(table=table, key="ipTypeForSpace", &
              VALUE=astr, default_value=default_ipTypeForSpace, &
              origin=origin, stat=stat, isfound=isok)
obj%ipTypeForSpace = BaseInterpolation_ToInteger(astr%chars())

#ifdef DEBUG_VER
CALL Display(myName//" spaceDomain")
#endif

CALL GetValue_(table=table, key="spaceDomain", tsize=tsize, &
               VALUE=obj%spaceDomain, origin=origin, stat=stat, isfound=isok)
isok = tsize .EQ. 2
CALL AssertError1(isok, myname, "spaceDomain should have 2 values")

#ifdef DEBUG_VER
CALL Display(myName//" spaceOrder")
#endif

!INFO: spaceOrder
CALL GetValue(table=table, key="spaceOrder", VALUE=tempintvec, &
              origin=origin, stat=stat, isfound=isok)
CALL AssertError1(isok, myname, "spaceOrder not found")

#ifdef DEBUG_VER
CALL Display(obj%totalSpaceElements, "totalSpaceElements: ")
#endif

CALL Reallocate(obj%spaceOrder, obj%totalSpaceElements)

abool = SIZE(tempintvec) .EQ. 1
IF (abool) THEN
  obj%spaceOrder(:) = tempintvec(1)
ELSE
  isok = SIZE(tempintvec) .EQ. obj%totalSpaceElements
  CALL AssertError1(isok, myname, "spaceOrder should have "// &
                    "totalSpaceElements values")
  obj%spaceOrder(:) = tempintvec(1:obj%totalSpaceElements)
END IF

obj%maxSpaceOrder = MAXVAL(obj%spaceOrder)

!INFO: spaceElemLength
#ifdef DEBUG_VER
CALL Display(myName//" spaceElemLength")
#endif

CALL GetValue(table=table, key="spaceElemLength", VALUE=temprealvec, &
              origin=origin, stat=stat, isfound=isok)
CALL AssertError1(isok, myname, "spaceElemLength not found")

CALL Reallocate(obj%spaceElemLength, obj%totalSpaceElements)

abool = SIZE(temprealvec) .EQ. 1
IF (abool) THEN
  obj%spaceElemLength = temprealvec(1)
ELSE
  isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
  CALL AssertError1(isok, myname, "spaceElemLength should have "// &
                    "totalSpaceElements values")
  obj%spaceElemLength(:) = temprealvec(1:obj%totalSpaceElements)
END IF

! INFO: saveData
#ifdef DEBUG_VER
CALL Display(myName//" saveData")
#endif

obj%saveData = .TRUE.
CALL toml_get(table, "saveData", array, origin=origin, stat=stat)
CALL toml_get(array, tempboolvec, origin=origin, stat=stat)

abool = SIZE(tempboolvec) .EQ. 1
IF (abool) THEN
  obj%saveData = tempboolvec(1)
ELSE
  isok = SIZE(tempboolvec) .EQ. 4
  CALL AssertError1(isok, myname, "saveData should have 4 values")
  obj%saveData = tempboolvec
END IF
array => NULL()
DEALLOCATE (tempboolvec)

! INFO: plotData
#ifdef DEBUG_VER
CALL Display(myName//" plotData")
#endif

obj%plotData = .TRUE.
CALL toml_get(table, "plotData", array, origin=origin, stat=stat)
CALL toml_get(array, tempboolvec, origin=origin, stat=stat)

abool = SIZE(tempboolvec) .EQ. 1
IF (abool) THEN
  obj%plotData = tempboolvec(1)
ELSE
  isok = SIZE(tempboolvec) .EQ. 3
  CALL AssertError1(isok, myname, "plotData should have 3 values")
  obj%plotData = tempboolvec
END IF
DEALLOCATE (tempboolvec)
array => NULL()

!INFO: quadTypeForSpace
#ifdef DEBUG_VER
CALL Display(myName//" quadTypeForSpace")
#endif

CALL GetValue(table=table, key="quadTypeForSpace", VALUE=astr, &
              default_value=default_quadTypeForSpace, origin=origin, &
              stat=stat, isfound=isok)
obj%quadTypeForSpace = BaseInterpolation_ToInteger(astr%chars())

!INFO: bodyForce
astr = "bodyForce"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%bodyForce)
  CALL obj%bodyForce%ImportFromToml(table=node)
END IF
node => NULL()

!INFO: tractionRight
astr = "tractionRight"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%tractionRight)
  CALL obj%tractionRight%ImportFromToml(table=node)
END IF
node => NULL()

!INFO: tractionLeft
astr = "tractionLeft"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%tractionLeft)
  CALL obj%tractionLeft%ImportFromToml(table=node)
END IF
node => NULL()

!INFO: displacementRight
astr = "displacementRight"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%displacementRight)
  CALL obj%displacementRight%ImportFromToml(table=node)
END IF
node => NULL()

!INFO: displacementLeft
astr = "displacementLeft"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%displacementLeft)
  CALL obj%displacementLeft%ImportFromToml(table=node)
END IF
node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                           ImportFromToml
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
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

isok = ALLOCATED(table)
CALL AssertError1(isok, myname, "table is not allocated from GetValue")

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: following error occured while reading '// &
               'the toml file :: cannot find '//tomlName//" table in config.")
END IF

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), myname//" Domain toml config: "// &
               CHAR_LF, unitno=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
