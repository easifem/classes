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

SUBMODULE(ScalarWave2DFEM_Class) IOMethods

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

USE GlobalData, ONLY: stdout, &
                      CHAR_LF, &
                      DOF_FMT, &
                      NONE, &
                      LIS_GMRES, &
                      CHAR_SLASH

USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToInteger, &
                                    BaseType_ToInteger, &
                                    BaseType_ToChar, &
                                    BaseInterpolation_ToChar

USE ReallocateUtility, ONLY: Reallocate

USE BaseType, ONLY: elem => TypeElemNameOpt, &
                    TypeFEVariableMatrix, TypeFEVariableConstant

USE DirichletBC_Class, ONLY: DirichletBCImportFromToml
USE NeumannBC_Class, ONLY: NeumannBCImportFromToml
USE SolidMaterial_Class, ONLY: SolidMaterialImportFromToml
USE FEVariable_Method, ONLY: NodalVariable
USE AbstractLinSolver_Class, ONLY: AbstractLinSolverImportFromToml

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

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

CALL Display(obj%totalSpaceElements, "totalSpaceElements: ", unitno=unitno)
CALL Display(obj%totalTimeSteps, "totalTimeSteps: ", unitno=unitno)

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
CALL Display(obj%timeRange, "timeRange: ", unitno=unitno)

CALL Display(obj%spaceOrder, "spaceOrder: ", unitno=unitno)

isok = ALLOCATED(obj%timeStepSize)
CALL Display(isok, "timeStepSize: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%timeStepSize, "timeStepSize: ", unitno=unitno)
END IF

CALL Display(obj%result_dir%chars(), "result_dir: ", unitno=unitno)
CALL Display(obj%filename%chars(), "filename: ", unitno=unitno)

isok = ASSOCIATED(obj%bodyForce)
CALL Display(isok, "bodyForce ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%initialDisp)
CALL Display(isok, "initialDisp ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%initialVel)
CALL Display(isok, "initialVel ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%initialAcc)
CALL Display(isok, "initialAcc ASSOCIATED: ", unitno=unitno)

isok = ALLOCATED(obj%nbc)
CALL Display(isok, "NeumannBC Allocated: ", unitno=unitno)

isok = ALLOCATED(obj%nbc_point)
CALL Display(isok, "pointSource Allocated: ", unitno=unitno)

CALL obj%algoParam%Display("SDAlgoParam: ", unitno=unitno)

CALL obj%solidMaterial(1)%ptr%Display("solidMaterial: ", unitno=unitno)

CALL Display("Output Controll")
CALL Display(obj%saveData, "saveData: ", unitno=unitno)
CALL Display(obj%outputFreq, "outputFreq: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START]')

CALL ImportEssentialsFromToml(obj, table)

CALL obj%InitiateDomains()

CALL ImportParametersFromToml(obj, table)

CALL ImportFunctionsFromToml(obj, table)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportEssentialsFromToml(obj, table)
  CLASS(ScalarWave2DFEM_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(inout) :: table
  CHARACTER(*), PARAMETER :: myName = "ImportEssentialsFromToml"
  INTEGER(I4B) :: origin, stat, tsize
  LOGICAL(LGT) :: isok, abool
  TYPE(String) :: astr
  REAL(DFP), ALLOCATABLE :: temprealvec(:)
  TYPE(toml_array), POINTER :: array
  TYPE(toml_table), POINTER :: node
  LOGICAL(LGT), ALLOCATABLE :: tempboolvec(:)

  IF (debug) CALL Display(myName//" result_dir")

  CALL GetValue(table=table, key="result_dir", VALUE=obj%result_dir, &
     default_value=default_result_dir, origin=origin, stat=stat, isfound=isok)

  IF (debug) CALL Display(myName//" filename")

  CALL GetValue(table=table, key="filename", VALUE=obj%filename, &
       origin=origin, stat=stat, isfound=isok, default_value=default_filename)

  CALL GetValue(table=table, key="verbosity", VALUE=obj%verbosity, &
      origin=origin, stat=stat, isfound=isok, default_value=default_verbosity)

  IF (debug) CALL Display(myName//" meshfilename")

  CALL GetValue(table=table, key="meshfilename", VALUE=obj%meshfilename, &
                origin=origin, stat=stat, isfound=isok, default_value="")
  CALL AssertError1(isok, myname, "meshfilename not found")

  IF (debug) CALL Display(myName//" totalTimeSteps")

  CALL GetValue(table=table, key="totalTimeSteps", &
                VALUE=obj%totalTimeSteps, &
                default_value=0_I4B, origin=origin, stat=stat, isfound=isok)

  IF (debug) CALL Display(myName//" baseInterpolationForSpace")

  CALL GetValue(table=table, key="baseInterpolationForSpace", &
                VALUE=astr, default_value=default_baseInterpolationForSpace, &
                origin=origin, stat=stat, isfound=isok)
  obj%baseInterpolationForSpace = UpperCase(astr%slice(1, 4))

  CALL obj%algoParam%ImportFromToml(table=table)

  IF (debug) CALL Display(myName//" baseTypeForSpace")

  CALL GetValue(table=table, key="baseTypeForSpace", &
                VALUE=astr, default_value=default_baseTypeForSpace, &
                origin=origin, stat=stat, isfound=isok)
  obj%baseTypeForSpace = BaseType_ToInteger(astr%chars())

  IF (debug) CALL Display(myName//" ipTypeForSpace")

  CALL GetValue(table=table, key="ipTypeForSpace", &
                VALUE=astr, default_value=default_ipTypeForSpace, &
                origin=origin, stat=stat, isfound=isok)
  obj%ipTypeForSpace = BaseInterpolation_ToInteger(astr%chars())

  IF (debug) CALL Display(myName//" quadTypeForSpace")

  CALL GetValue(table=table, key="quadTypeForSpace", VALUE=astr, &
                default_value=default_quadTypeForSpace, origin=origin, &
                stat=stat, isfound=isok)
  obj%quadTypeForSpace = BaseInterpolation_ToInteger(astr%chars())

  IF (debug) CALL Display(myName//" timeRange")

  CALL GetValue_(table=table, key="timeRange", tsize=tsize, &
                 VALUE=obj%timeRange, origin=origin, stat=stat, isfound=isok)
  isok = tsize .EQ. 2
  CALL AssertError1(isok, myname, "timeRange should have 2 values")

  IF (debug) CALL Display(myName//" timeStepSize")

  CALL GetValue(table=table, key="timeStepSize", VALUE=temprealvec, &
                origin=origin, stat=stat, isfound=isok)
  CALL AssertError1(isok, myname, "timeStepSize not found")

  CALL Reallocate(obj%timeStepSize, obj%totalTimeSteps)

  abool = SIZE(temprealvec) .EQ. 1
  IF (abool) THEN
    obj%timeStepSize = temprealvec(1)
  ELSE
    isok = SIZE(temprealvec) .EQ. obj%totalTimeSteps
    CALL AssertError1(isok, myname, "timeStepSize should have "// &
                      "totalTimeElements values")
    obj%timeStepSize(:) = temprealvec(1:obj%totalTimeSteps)
  END IF

  IF (debug) CALL Display(myName//" saveData")

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

  IF (debug) CALL Display(myName//" outputFreq")

  CALL GetValue(table=table, key="outputFreq", VALUE=obj%outputFreq, &
                default_value=1_I4B, origin=origin, stat=stat)

  IF (debug) CALL Display(myName//" Linsolver")
  CALL toml_get(table, "linsolver", node, origin=origin, &
                requested=.FALSE., stat=stat)
  isok = ASSOCIATED(node)
  CALL asserterror1(isok, myname, "linsolver is not found")
  CALL AbstractLinSolverImportFromToml(obj%linsolver, node)

  node => NULL()

END SUBROUTINE ImportEssentialsFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportParametersFromToml(obj, table)
  CLASS(ScalarWave2DFEM_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(inout) :: table
  CHARACTER(*), PARAMETER :: myName = "ImportParametersFromToml"
  INTEGER(I4B) :: origin, stat, tsize
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  TYPE(toml_array), POINTER :: array
  TYPE(UserFunction_), POINTER :: func => NULL()

  ! TODO: need to improve this method
  ! for example I want to set the differenct values for
  ! specific domains
  IF (debug) CALL Display(myName//" Solid material")
  node => NULL()
  CALL toml_get(table, "solidMaterial", array, origin=origin, &
                requested=.FALSE., stat=stat)
  isok = ASSOCIATED(array)
  CALL asserterror1(isok, myname, "solidMaterial not found")
  tsize = toml_len(array)
  isok = tsize .EQ. 1
  CALL asserterror1(isok, myname, &
                    "currently only homogeneous material is supported")
  ALLOCATE (obj%solidMaterial(tsize))
  IF (debug) CALL Display(tsize, "number of solidMaterial :: ")
  CALL SolidMaterialImportFromToml(obj=obj%solidMaterial, &
                                   tomlName="solidMaterial", table=table)
  array => NULL()

  ! func => obj%solidMaterial(1)%ptr%GetMaterialPointer("massDensity")
  ! isok = ASSOCIATED(func)
  ! CALL AssertError1(isok, myname, "massDensity not found")
  ! CALL func%GetFEVariable(obj%density)
  ! func => NULL()

  func => obj%solidMaterial(1)%ptr%GetMaterialPointer("squareWaveSpeed")
  isok = ASSOCIATED(func)
  CALL AssertError1(isok, myname, "squareWaveSpeed not found")
  CALL func%GetFEVariable(obj%squareWaveSpeed)
  func => NULL()

END SUBROUTINE ImportParametersFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportFunctionsFromToml(obj, table)
  CLASS(ScalarWave2DFEM_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(inout) :: table
  CHARACTER(*), PARAMETER :: myName = "ImportFunctionsFromToml"
  INTEGER(I4B) :: origin, stat, tsize, ii, nnum
  LOGICAL(LGT) :: isok
  TYPE(String) :: astr
  TYPE(toml_table), POINTER :: node
  TYPE(toml_array), POINTER :: array

  astr = "bodyForce"
  IF (debug) CALL Display(myName//astr%chars())
  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
                stat=stat)
  isok = ASSOCIATED(node)
  IF (isok) THEN
    ALLOCATE (obj%bodyForce)
    CALL obj%bodyForce%ImportFromToml(table=node)
  END IF
  node => NULL()

  astr = "initialAcc"
  IF (debug) CALL Display(myName//astr%chars())
  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
                stat=stat)
  isok = ASSOCIATED(node)
  IF (isok) THEN
    ALLOCATE (obj%initialAcc)
    CALL obj%initialAcc%ImportFromToml(table=node)
  END IF
  node => NULL()

  astr = "initialVel"
  IF (debug) CALL Display(myName//astr%chars())
  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
                stat=stat)
  isok = ASSOCIATED(node)
  IF (isok) THEN
    ALLOCATE (obj%initialVel)
    CALL obj%initialVel%ImportFromToml(table=node)
  END IF
  node => NULL()

  astr = "initialDisp"
  IF (debug) CALL Display(myName//astr%chars())
  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
                stat=stat)
  isok = ASSOCIATED(node)
  IF (isok) THEN
    ALLOCATE (obj%initialDisp)
    CALL obj%initialDisp%ImportFromToml(table=node)
  END IF
  node => NULL()

  astr = "dirichletBC"
  IF (debug) CALL Display(myName//astr%chars())
  node => NULL()
 CALL toml_get(table, astr%chars(), array, origin=origin, requested=.FALSE., &
                stat=stat)
  isok = ASSOCIATED(array)
  IF (isok) THEN
    tsize = toml_len(array)
    ALLOCATE (obj%dbc(tsize))
    IF (debug) CALL Display(tsize, "number of DirichletBC :: ")
    CALL DirichletBCImportFromToml(obj=obj%dbc, tomlName="dirichletBC", &
                                   table=table, dom=obj%dom)
    array => NULL()
  ELSE
    IF (debug) CALL Display("No dirichletBC")
  END IF

  astr = "neumannBC"
  IF (debug) CALL Display(myName//astr%chars())
  node => NULL()
 CALL toml_get(table, astr%chars(), array, origin=origin, requested=.FALSE., &
                stat=stat)
  isok = ASSOCIATED(array)
  IF (isok) THEN
    tsize = toml_len(array)
    ALLOCATE (obj%nbc(tsize))
    IF (debug) CALL Display(tsize, "number of NeumannBC :: ")
    CALL NeumannBCImportFromToml(obj=obj%nbc, tomlName="neumannBC", &
                                 table=table, dom=obj%dom)
    array => NULL()
  ELSE
    IF (debug) CALL Display("No neumannBC")
  END IF

  astr = "pointSource"
  IF (debug) CALL Display(myName//astr%chars())
  node => NULL()
 CALL toml_get(table, astr%chars(), array, origin=origin, requested=.FALSE., &
                stat=stat)
  isok = ASSOCIATED(array)
  IF (isok) THEN
    tsize = toml_len(array)
    ALLOCATE (obj%nbc_point(tsize))
    IF (debug) CALL Display(tsize, "number of pointSource :: ")
    CALL NeumannBCImportFromToml(obj=obj%nbc_point, tomlName="pointSource", &
                                 table=table, dom=obj%dom)
    nnum = 0
    DO ii = 1, tsize
      nnum = obj%nbc_point(ii)%ptr%GetTotalNodeNum(obj%fedof)
      IF (nnum .GT. obj%maxNodeNum_pointSource) &
        obj%maxNodeNum_pointSource = nnum
    END DO
    array => NULL()
  ELSE
    IF (debug) CALL Display("No pointSource")
  END IF

END SUBROUTINE ImportFunctionsFromToml

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
