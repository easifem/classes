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

SUBMODULE(Abstract1DUVSTFEM_Class) IOMethods

USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

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

USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_

USE ReallocateUtility, ONLY: Reallocate

USE ProductUtility, ONLY: OuterProd_, OTimesTilda

USE BaseType, ONLY: elem => TypeElemNameOpt

USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate, &
                                  Quad_Size => Size, &
                                  Quad_Display => Display

USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                OrthogonalElemShapeData

USE SwapUtility, ONLY: SWAP

USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate, &
                            CSRMatrix_Add => Add, &
                            CSRMatrix_GetSubMatrix => GetSubMatrix, &
                            CSRMatrix_Display => Display, &
                            CSRMatrix_Size => Size, &
                            CSRMatrix_SetSparsity => SetSparsity, &
                            CSRMatrix_ApplyDBC => ApplyDBC, &
                            CSRMatrix_Set => Set, &
                            CSRMatrix_Matvec => Matvec, &
                            CSRMatrix_LinSolve => CSRMatrix_GMRES, &
                            CSRMatrixLinSolveInitiate

USE DOF_Method, ONLY: DOF_Initiate => Initiate, &
                      DOF_SIZE => Size, &
                      DOF_GetIndex_ => GetIndex_, &
                      DOF_GetNodeLoc => GetNodeLoc

USE RealVector_Method, ONLY: RealVector_Initiate => Initiate, &
                             RealVector_Add => Add, &
                             RealVector_GetValue_ => GetValue_, &
                             RealVector_Set => Set, &
                             RealVector_Display => Display, &
                             RealVector_Scale => SCAL

USE LagrangePolynomialUtility, ONLY: InterpolationPoint_
USE InputUtility

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

CALL Display(obj%totalSpaceNodes, "totalSpaceNodes: ", unitno=unitno)
CALL Display(obj%totalSpaceElements, "totalSpaceElements: ", unitno=unitno)
CALL Display(obj%totalTimeNodes, "totalTimeNodes: ", unitno=unitno)

CALL Display(obj%baseContinuityForSpace, "baseContinuityForSpace: ", &
             unitno=unitno)

CALL Display(obj%baseContinuityForTime, "baseContinuityForTime: ", &
             unitno=unitno)

CALL Display(obj%baseInterpolationForSpace, "baseInterpolationForSpace: ", &
             unitno=unitno)

CALL Display(obj%baseInterpolationForTime, "baseInterpolationForTime: ", &
             unitno=unitno)

astr = BaseType_ToChar(obj%baseTypeForSpace)
CALL Display(astr, "baseTypeForSpace: ", unitno=unitno)

astr = BaseInterpolation_TOChar(obj%ipTypeForSpace)
CALL Display(astr, "ipTypeForSpace: ", unitno=unitno)

astr = BaseType_ToChar(obj%baseTypeForTime)
CALL Display(astr, "baseTypeForTime: ", unitno=unitno)

astr = BaseInterpolation_ToChar(obj%ipTypeForTime)
CALL Display(astr, "ipTypeForTime: ", unitno=unitno)

astr = BaseInterpolation_ToChar(obj%quadTypeForSpace)
CALL Display(astr, "quadTypeForSpace: ", unitno=unitno)

astr = BaseInterpolation_ToChar(obj%quadTypeForTime)
CALL Display(astr, "quadTypeForTime: ", unitno=unitno)

CALL Display(obj%maxSpaceOrder, "maxSpaceOrder: ", unitno=unitno)
CALL Display(obj%maxTimeOrder, "maxTimeOrder: ", unitno=unitno)
CALL Display(obj%spaceDomain, "spaceDomain: ", unitno=unitno)
CALL Display(obj%timeDomain, "timeDomain: ", unitno=unitno)

isok = ALLOCATED(obj%spaceOrder)
CALL Display(isok, "spaceOrder: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%spaceOrder, "spaceOrder: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%timeOrder)
CALL Display(isok, "timeOrder: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%timeOrder, "timeOrder: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%totalDOFSpace)
CALL Display(isok, "totalDOFSpace: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%totalDOFSpace, "totalDOFSpace: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%totalDOFTime)
CALL Display(isok, "totalDOFTime: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%totalDOFTime, "totalDOFTime: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%spaceElemLength)
CALL Display(isok, "spaceElemLength: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%spaceElemLength, "spaceElemLength: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%timeElemLength)
CALL Display(isok, "timeElemLength: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%timeElemLength, "timeElemLength: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%elasticModulus)
CALL Display(isok, "elasticModulus: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%elasticModulus, "elasticModulus: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%density)
CALL Display(isok, "density: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%density, "density: ", unitno=unitno)
END IF

CALL Display(obj%result_dir%chars(), "result_dir: ", unitno=unitno)
CALL Display(obj%filename%chars(), "filename: ", unitno=unitno)

isok = ASSOCIATED(obj%bodyForce)
CALL Display(isok, "bodyForce ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%tractionRight)
CALL Display(isok, "tractionRight ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%tractionLeft)
CALL Display(isok, "tractionLeft ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%velocityLeft)
CALL Display(isok, "velocityLeft ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%velocityRight)
CALL Display(isok, "velocityRight ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%initialDisp)
CALL Display(isok, "initialDisp ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%initialVel)
CALL Display(isok, "initialVel ASSOCIATED: ", unitno=unitno)

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
CALL Display(myName//" totalTimeNodes")
#endif

CALL GetValue(table=table, key="totalTimeNodes", VALUE=obj%totalTimeNodes, &
              default_value=0_I4B, origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
CALL Display(myName//" totalTimeElements")
#endif

CALL GetValue(table=table, key="totalTimeElements", &
              VALUE=obj%totalTimeElements, &
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
CALL Display(myName//" baseInterpolationForTime")
#endif

CALL GetValue(table=table, key="baseInterpolationForTime", &
              VALUE=astr, default_value=default_baseInterpolationForTime, &
              origin=origin, stat=stat, isfound=isok)
obj%baseInterpolationForTime = UpperCase(astr%slice(1, 4))

#ifdef DEBUG_VER
CALL Display(myName//" baseTypeForTime")
#endif

CALL GetValue(table=table, key="baseTypeForTime", &
              VALUE=astr, default_value=default_baseTypeForTime, &
              origin=origin, stat=stat, isfound=isok)
obj%baseTypeForTime = BaseType_ToInteger(astr%chars())

#ifdef DEBUG_VER
CALL Display(myName//" ipTypeForTime")
#endif

CALL GetValue(table=table, key="ipTypeForTime", &
              VALUE=astr, default_value=default_ipTypeForTime, &
              origin=origin, stat=stat, isfound=isok)
obj%ipTypeForTime = BaseInterpolation_ToInteger(astr%chars())

#ifdef DEBUG_VER
CALL Display(myName//" spaceDomain")
#endif

CALL GetValue_(table=table, key="spaceDomain", tsize=tsize, &
               VALUE=obj%spaceDomain, origin=origin, stat=stat, isfound=isok)
isok = tsize .EQ. 2
CALL AssertError1(isok, myname, "spaceDomain should have 2 values")

#ifdef DEBUG_VER
CALL Display(myName//" timeDomain")
#endif

CALL GetValue_(table=table, key="timeDomain", tsize=tsize, &
               VALUE=obj%timeDomain, origin=origin, stat=stat, isfound=isok)
isok = tsize .EQ. 2
CALL AssertError1(isok, myname, "timeDomain should have 2 values")

#ifdef DEBUG_VER
CALL Display(myName//" spaceOrder")
#endif

!INFO: spaceOrder
CALL ElementDataImportFromToml_int(table, "spaceOrder", tempintvec, &
                                   obj%totalSpaceElements, isok)
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

!INFO: timeOrder
#ifdef DEBUG_VER
CALL Display(myName//" timeOrder")
#endif

CALL ElementDataImportFromToml_int(table, "timeOrder", tempintvec, &
                                   obj%totalTimeElements, isok)
CALL AssertError1(isok, myname, "timeOrder not found")

#ifdef DEBUG_VER
CALL Display(obj%totalTimeElements, myname//" totalTimeElements: ")
#endif

CALL Reallocate(obj%timeOrder, obj%totalTimeElements)

abool = SIZE(tempintvec) .EQ. 1
IF (abool) THEN
  obj%timeOrder(:) = tempintvec(1)
ELSE
  isok = SIZE(tempintvec) .EQ. obj%totalTimeElements
  CALL AssertError1(isok, myname, "timeOrder should have "// &
                    "totalTimeElements values")
  obj%timeOrder(:) = tempintvec(1:obj%totalTimeElements)
END IF

obj%maxTimeOrder = MAXVAL(obj%timeOrder)

!INFO: spaceElemLength
#ifdef DEBUG_VER
CALL Display(myName//" spaceElemLength")
#endif

CALL ElementDataImportFromToml(table, "spaceElemLength", temprealvec, &
                               obj%totalSpaceElements, isok)
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

! !INFO: timeElemLength
#ifdef DEBUG_VER
CALL Display(myName//" timeElemLength")
#endif

CALL ElementDataImportFromToml(table, "timeElemLength", temprealvec, &
                               obj%totalTimeElements, isok)
CALL AssertError1(isok, myname, "timeElemLength not found")

CALL Reallocate(obj%timeElemLength, obj%totalTimeElements)

abool = SIZE(temprealvec) .EQ. 1
IF (abool) THEN
  obj%timeElemLength = temprealvec(1)
ELSE
  isok = SIZE(temprealvec) .EQ. obj%totalTimeElements
  CALL AssertError1(isok, myname, "timeElemLength should have "// &
                    "totalTimeElements values")
  obj%timeElemLength(:) = temprealvec(1:obj%totalTimeElements)
END IF

! INFO: saveData
#ifdef DEBUG_VER
CALL Display(myName//" saveData")
#endif

obj%saveData = .TRUE.
CALL toml_get(table, "saveData", array, origin=origin, stat=stat)
CALL toml_get(array, tempboolvec, origin=origin, stat=stat)

IF (SIZE(tempboolvec) .EQ. 4) THEN
  obj%saveData = tempboolvec
ELSEIF (SIZE(tempboolvec) .GT. 0) THEN
  CALL AssertError1(.FALSE., myname, "saveData should have 4 values")
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

IF (SIZE(tempboolvec) .EQ. 3) THEN
  obj%plotData = tempboolvec
ELSEIF (SIZE(tempboolvec) .GT. 0) THEN
  CALL AssertError1(.FALSE., myname, "plotData should have 3 values")
END IF
array => NULL()
DEALLOCATE (tempboolvec)

! INFO: outputFreq
#ifdef DEBUG_VER
CALL Display(myName//" outputFreq")
#endif

CALL GetValue(table=table, key="outputFreq", VALUE=obj%outputFreq, &
              default_value=1_I4B, origin=origin, stat=stat)

!INFO: elasticModulus
#ifdef DEBUG_VER
CALL Display(myName//" elasticModulus")
#endif

CALL ElementDataImportFromToml(table, "elasticModulus", temprealvec, &
                               obj%totalSpaceElements, isok)
CALL AssertError1(isok, myname, "elasticModulus not found")

CALL Reallocate(obj%elasticModulus, obj%totalSpaceElements)

abool = SIZE(temprealvec) .EQ. 1
IF (abool) THEN
  obj%elasticModulus = temprealvec(1)
ELSE
  isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
  CALL AssertError1(isok, myname, "elasticModulus should have "// &
                    "totalSpaceElements values")
  obj%elasticModulus(:) = temprealvec(1:obj%totalSpaceElements)
END IF

!INFO: density
#ifdef DEBUG_VER
CALL Display(myName//" density")
#endif
CALL ElementDataImportFromToml(table, "density", temprealvec, &
                               obj%totalSpaceElements, isok)
CALL AssertError1(isok, myname, "density not found")

CALL Reallocate(obj%density, obj%totalSpaceElements)

abool = SIZE(temprealvec) .EQ. 1
IF (abool) THEN
  obj%density = temprealvec(1)
ELSE
  isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
  CALL AssertError1(isok, myname, "density should have "// &
                    "totalSpaceElements values")
  obj%density(:) = temprealvec(1:obj%totalSpaceElements)
END IF

!INFO: rayleighAlpha
#ifdef DEBUG_VER
CALL Display(myName//" rayleighAlpha")
#endif
CALL ElementDataImportFromToml(table, "rayleighAlpha", temprealvec, &
                               obj%totalSpaceElements, isok)

CALL Reallocate(obj%rayleighAlpha, obj%totalSpaceElements)

IF (.NOT. isok) THEN
  obj%rayleighAlpha = 0.0

ELSE

  abool = SIZE(temprealvec) .EQ. 1
  IF (abool) THEN
    obj%rayleighAlpha = temprealvec(1)
  ELSE
    isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
    CALL AssertError1(isok, myname, "rayleighAlpha should have "// &
                      "totalSpaceElements values")
    obj%rayleighAlpha(:) = temprealvec(1:obj%totalSpaceElements)
  END IF
END IF

!INFO: rayleighBeta
#ifdef DEBUG_VER
CALL Display(myName//" rayleighBeta")
#endif
CALL ElementDataImportFromToml(table, "rayleighBeta", temprealvec, &
                               obj%totalSpaceElements, isok)

CALL Reallocate(obj%rayleighBeta, obj%totalSpaceElements)

IF (.NOT. isok) THEN
  obj%rayleighBeta = 0.0

ELSE

  abool = SIZE(temprealvec) .EQ. 1
  IF (abool) THEN
    obj%rayleighBeta = temprealvec(1)
  ELSE
    isok = SIZE(temprealvec) .EQ. obj%totalSpaceElements
    CALL AssertError1(isok, myname, "rayleighBeta should have "// &
                      "totalSpaceElements values")
    obj%rayleighBeta(:) = temprealvec(1:obj%totalSpaceElements)
  END IF
END IF

!INFO: quadTypeForSpace
#ifdef DEBUG_VER
CALL Display(myName//" quadTypeForSpace")
#endif

CALL GetValue(table=table, key="quadTypeForSpace", VALUE=astr, &
              default_value=default_quadTypeForSpace, origin=origin, &
              stat=stat, isfound=isok)
obj%quadTypeForSpace = BaseInterpolation_ToInteger(astr%chars())

!INFO: quadTypeForTime
#ifdef DEBUG_VER
CALL Display(myName//" quadTypeForTime")
#endif

CALL GetValue(table=table, key="quadTypeForTime", VALUE=astr, &
              default_value=default_quadTypeForTime, origin=origin, &
              stat=stat, isfound=isok)
obj%quadTypeForTime = BaseInterpolation_ToInteger(astr%chars())

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

!INFO: velocityRight
astr = "velocityRight"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%velocityRight)
  CALL obj%velocityRight%ImportFromToml(table=node)
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

!INFO: velocityLeft
astr = "velocityLeft"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%velocityLeft)
  CALL obj%velocityLeft%ImportFromToml(table=node)
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

!INFO: initialVel
astr = "initialVel"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%initialVel)
  CALL obj%initialVel%ImportFromToml(table=node)
END IF
node => NULL()

!INFO: initialDisp
astr = "initialDisp"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%initialDisp)
  CALL obj%initialDisp%ImportFromToml(table=node)
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
