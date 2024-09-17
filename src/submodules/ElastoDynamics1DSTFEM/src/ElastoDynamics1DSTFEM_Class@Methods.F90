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

SUBMODULE(ElastoDynamics1DSTFEM_Class) Methods
USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_stat

USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

USE Display_Method, ONLY: ToString, Display

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

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                              -                     obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                   obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isConnectivity = .FALSE.
obj%baseContinuityForSpace = "H1"
obj%baseContinuityForTime = "H1"
obj%baseInterpolationForSpace = "LAGR"
obj%baseInterpolationForTime = "LAGR"
obj%verbosity = 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

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

!INFO: timeOrder
#ifdef DEBUG_VER
CALL Display(myName//" timeOrder")
#endif

CALL GetValue(table=table, key="timeOrder", VALUE=tempintvec, &
              origin=origin, stat=stat, isfound=isok)
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

! !INFO: timeElemLength
#ifdef DEBUG_VER
CALL Display(myName//" timeElemLength")
#endif

CALL GetValue(table=table, key="timeElemLength", VALUE=temprealvec, &
              origin=origin, stat=stat, isfound=isok)
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

!INFO: elasticModulus
#ifdef DEBUG_VER
CALL Display(myName//" elasticModulus")
#endif

CALL GetValue(table=table, key="elasticModulus", VALUE=temprealvec, &
              origin=origin, stat=stat, isfound=isok)
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
CALL GetValue(table=table, key="density", VALUE=temprealvec, &
              origin=origin, stat=stat, isfound=isok)
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
CALL GetValue(table=table, key="rayleighAlpha", VALUE=temprealvec, &
              origin=origin, stat=stat, isfound=isok)

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
CALL GetValue(table=table, key="rayleighBeta", VALUE=temprealvec, &
              origin=origin, stat=stat, isfound=isok)

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
!                                                                   obj_Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
#endif

INTEGER(I4B) :: tnodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%currentTimeStep = 1
obj%currentTime = obj%timeDomain(1)

CALL SetTotalDOFSpace(obj=obj)
CALL SetTotalDOFTime(obj=obj)

tnodes = obj%totalVertexDOFSpace + obj%totalEdgeDOFSpace
CALL RealVector_Initiate(obj%u0, tnodes)
CALL RealVector_Initiate(obj%a0, tnodes)
CALL RealVector_Initiate(obj%v0, tnodes)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                             InitiateTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFields
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFields()"
#endif

INTEGER(I4B), PARAMETER :: storageFMT = DOF_FMT
CHARACTER(LEN=1), PARAMETER :: names(1) = ["u"]
INTEGER(I4B) :: tnodes(1), spaceCompo(1), timeCompo(1), nrow, ncol, ii, &
                con(256), tcon, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

spaceCompo(1) = 1
timeCompo(1) = obj%totalDOFTime(timeElemNum)
tnodes(1) = obj%totalVertexDOFSpace + obj%totalEdgeDOFSpace

CALL DOF_Initiate(obj=obj%dof, tNodes=tnodes, names=names, &
            spacecompo=spacecompo, timecompo=timecompo, storageFMT=storageFMT)

nrow = DOF_SIZE(obj%dof)
ncol = nrow

CALL CSRMatrix_Initiate(obj=obj%tanmat, ncol=ncol, nrow=nrow, &
                        idof=obj%dof, jdof=obj%dof)

DO ii = 1, obj%totalSpaceElements
  CALL obj%GetConnectivity(spaceElemNum=ii, ans=con, tsize=tcon)
  DO jj = 1, tcon
    CALL CSRMatrix_SetSparsity(obj=obj%tanmat, row=con(jj), col=con(1:tcon))
  END DO
END DO

CALL CSRMatrix_SetSparsity(obj=obj%tanmat)

CALL RealVector_Initiate(obj%sol, nrow)
CALL RealVector_Initiate(obj%rhs, nrow)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateFields

!----------------------------------------------------------------------------
!                                                      InitiateConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateConnectivity()"
#endif

INTEGER(I4B) :: ii, jj, icount, iedgedof, tnodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isConnectivity) RETURN

obj%isConnectivity = .TRUE.

tnodes = 0
DO ii = 1, obj%totalSpaceElements
  tnodes = tnodes + obj%totalDOFSpace(ii)
END DO

CALL Reallocate(obj%conIA, obj%totalSpaceElements + 1)
CALL Reallocate(obj%conJA, tnodes)
obj%conIA(1) = 1

icount = 1
iedgedof = obj%totalVertexDOFSpace

DO ii = 1, obj%totalSpaceElements
  obj%conJA(icount) = ii
  icount = icount + 1
  obj%conJA(icount) = ii + 1
  icount = icount + 1
  obj%conIA(ii + 1) = obj%conIA(ii) + obj%totalDOFSpace(ii)

  DO jj = 1, obj%totalDOFSpace(ii) - 2
    iedgedof = iedgedof + 1
    obj%conJA(icount) = iedgedof
    icount = icount + 1
  END DO
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateConnectivity

!----------------------------------------------------------------------------
!                                                           SetTotalDOFSpace
!----------------------------------------------------------------------------

SUBROUTINE SetTotalDOFSpace(obj)
  CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetTotalDOFSpace()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Reallocate(obj%totalDOFSpace, obj%totalSpaceElements)

  obj%totalVertexDOFSpace = obj%totalSpaceNodes
  obj%totalEdgeDOFSpace = 0

  DO iel = 1, obj%totalSpaceElements
    obj%totalDOFSpace(iel) = obj%spaceOrder(iel) + 1

    IF (obj%totalDOFSpace(iel) .GE. 2) THEN
      obj%totalEdgeDOFSpace = obj%totalEdgeDOFSpace &
                              + obj%totalDOFSpace(iel) - 2
    END IF
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetTotalDOFSpace

!----------------------------------------------------------------------------
!                                                            SetTotalDOFTime
!----------------------------------------------------------------------------

SUBROUTINE SetTotalDOFTime(obj)
  CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetTotalDOFTime()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Reallocate(obj%totalDOFTime, obj%totalTimeElements)

  obj%totalVertexDOFTime = obj%totalTimeNodes
  obj%totalEdgeDOFTime = 0

  DO iel = 1, obj%totalTimeElements
    obj%totalDOFTime(iel) = obj%timeOrder(iel) + 1

    IF (obj%totalDOFTime(iel) .GE. 2) THEN
      obj%totalEdgeDOFTime = obj%totalEdgeDOFTime &
                             + obj%totalDOFTime(iel) - 2
    END IF
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetTotalDOFTime

!----------------------------------------------------------------------------
!                                                                 obj_GetCt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCt()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips
  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%elemsdForTime%dNdXi(1:ncol, 1, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetCt

!----------------------------------------------------------------------------
!                                                                      GetMt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMt()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips
  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%elemsdForTime%N(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMt

!----------------------------------------------------------------------------
!                                                                  GetMtPlus
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMtPlus
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMtPlus()"
#endif
INTEGER(I4B) :: ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

scale = 1.0_DFP
CALL OuterProd_(a=obj%timeShapeFuncBndy(1:nrow, 1), &
                b=obj%timeShapeFuncBndy(1:ncol, 1), &
                ans=ans, nrow=ii, ncol=jj, anscoeff=0.0_DFP, scale=scale)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMtPlus

!----------------------------------------------------------------------------
!                                                                GetKt_Tilda
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetKt_Tilda
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetKt_Tilda()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips

  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)

  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%bt(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetKt_Tilda

!----------------------------------------------------------------------------
!                                                                    GetWt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetWt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetWt()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow

obj%wt(1:nrow, 1:ncol) = obj%ct(1:nrow, 1:ncol) + obj%mtplus(1:nrow, 1:ncol)
CALL GetInvMat(obj%wt(1:nrow, 1:ncol))
obj%wmt(1:nrow, 1:ncol) = MATMUL(obj%wt(1:nrow, 1:ncol), obj%mt(1:nrow, 1:ncol))
obj%wmt(1:nrow, 1:ncol) = TRANSPOSE(obj%wmt(1:nrow, 1:ncol))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetWt

!----------------------------------------------------------------------------
!                                                                    GetAt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAt()"
#endif

INTEGER(I4B) :: nrow, ncol, ii, tsize
REAL(DFP) :: temp(MAX_ORDER_TIME), areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow

tsize = obj%elemsdForTime%nips

temp(1:nrow) = MATMUL(obj%wt(1:nrow, 1:ncol), obj%timeShapeFuncBndy(1:ncol, 1))

obj%tat(1:nrow) = 0.0_DFP

obj%at_right = DOT_PRODUCT(obj%timeShapeFuncBndy(1:nrow, 2), temp(1:nrow))

DO ii = 1, tsize
  obj%at(ii) = DOT_PRODUCT(obj%elemsdForTime%N(1:nrow, ii), temp(1:nrow))
  areal = obj%at(ii) * obj%elemsdForTime%ws(ii)
  obj%tat(1:nrow) = obj%tat(1:nrow) + obj%elemsdForTime%N(1:nrow, ii) * areal
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetAt

!----------------------------------------------------------------------------
!                                                                    GetBt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBt()"
#endif

INTEGER(I4B) :: nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = obj%elemsdForTime%nips
obj%bt(1:nrow, 1:ncol) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                                obj%elemsdForTime%N(1:nrow, 1:ncol))

obj%bt(1:nrow, 1:ncol) = obj%bt(1:nrow, 1:ncol) * half

obj%bt_right(1:nrow) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                              obj%timeShapeFuncBndy(1:nrow, 2))

obj%bt_right(1:nrow) = obj%bt_right(1:nrow) * half

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetBt

!----------------------------------------------------------------------------
!                                                                     GetMs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMs()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForSpace%nips
  scale = obj%elemsdForSpace%ws(ips) * obj%elemsdForSpace%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForSpace%N(1:nrow, ips), &
                  b=obj%elemsdForSpace%N(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMs

!----------------------------------------------------------------------------
!                                                                    GetKs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetKs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetKs()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForSpace%nips
  scale = obj%elemsdForSpace%ws(ips) * obj%elemsdForSpace%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForSpace%dNdXi(1:nrow, 1, ips), &
                  b=obj%elemsdForSpace%dNdXi(1:ncol, 1, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetKs

!----------------------------------------------------------------------------
!                                                                     GetCs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCs()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = alpha * obj%ks(1:nrow, 1:ncol) &
                      + beta * obj%ms(1:nrow, 1:ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetCs

!----------------------------------------------------------------------------
!                                                            SetQuadForSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadForSpace
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadForSpace()"
#endif

INTEGER(I4B) :: order, integralOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = obj%spaceOrder(spaceElemNum)
integralOrder = 2 * order

CALL QuadPoint_Initiate(obj=obj%quadForSpace, elemType=elem%line, &
                        domainName="B", order=integralOrder, &
                        quadratureType=obj%quadTypeForSpace)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetQuadForSpace

!----------------------------------------------------------------------------
!                                                             SetQuadForTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadForTime
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadForTime()"
#endif

INTEGER(I4B) :: order, integralOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = obj%timeOrder(timeElemNum)
integralOrder = 2 * order

CALL QuadPoint_Initiate(obj=obj%quadForTime, elemType=elem%line, &
                        domainName="B", order=integralOrder, &
                        quadratureType=obj%quadTypeForTime)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetQuadForTime

!----------------------------------------------------------------------------
!                                                          SetElemsdForSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElemsdForSpace
CHARACTER(*), PARAMETER :: myName = "obj_SetElemsdForSpace()"
INTEGER(I4B) :: nips, nns, cellOrder(1), order, cellOrient(1)
REAL(DFP) :: refElemCoord(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = obj%spaceOrder(spaceElemNum)
nips = Quad_Size(obj%quadForSpace, 2)
nns = obj%totalDOFSpace(spaceElemNum)
refElemCoord(1, 1) = -1.0_DFP
refElemCoord(1, 2) = 1.0_DFP
cellOrient = 1

CALL Elemsd_Allocate(obj=obj%elemsdForSpace, nsd=1_I4B, xidim=1_I4B, &
                     nns=nns, nips=nips)

CALL LagrangeElemShapeData(obj=obj%linElemsdForSpace, &
                           quad=obj%quadForSpace, &
                           nsd=obj%elemsdForSpace%nsd, &
                           xidim=obj%elemsdForSpace%xidim, &
                           elemtype=elem%line, &
                           refelemCoord=refelemCoord, &
                           domainName="B", &
                           order=1_I4B)

SELECT CASE (obj%baseInterpolationForSpace)
CASE ("LAGR")

  CALL LagrangeElemShapeData(obj=obj%elemsdForSpace, &
                             quad=obj%quadForSpace, &
                             nsd=obj%elemsdForSpace%nsd, &
                             xidim=obj%elemsdForSpace%xidim, &
                             elemtype=elem%line, &
                             refelemCoord=refelemCoord, &
                             domainName="B", &
                             order=order, &
                             ipType=obj%ipTypeForSpace, &
                             basisType=obj%baseTypeForSpace)

  obj%spaceShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%spaceShapeFuncBndy(1, 1) = 1.0_DFP

  obj%spaceShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%spaceShapeFuncBndy(2, 2) = 1.0_DFP

CASE ("HIER", "HEIR")

  cellOrder = order
  CALL HierarchicalElemShapeData(obj=obj%elemsdForSpace, &
                                 quad=obj%quadForSpace, &
                                 nsd=obj%elemsdForSpace%nsd, &
                                 xidim=obj%elemsdForSpace%xidim, &
                                 elemtype=elem%line, &
                                 refelemCoord=refelemCoord, &
                                 domainName="B", &
                                 cellOrder=cellOrder, &
                                 cellOrient=cellOrient)

  obj%spaceShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%spaceShapeFuncBndy(1, 1) = 1.0_DFP

  obj%spaceShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%spaceShapeFuncBndy(2, 2) = 1.0_DFP

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'no case found for baseInterpolationForSpace')

END SELECT

CALL Elemsd_Set(obj=obj%elemsdForSpace, val=xij, &
                N=obj%linElemsdForSpace%N(1:2, 1:nips), &
                dNdXi=obj%linElemsdForSpace%dNdXi(1:2, 1:1, 1:nips))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetElemsdForSpace

!----------------------------------------------------------------------------
!                                                           SetElemsdForTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElemsdForTime
CHARACTER(*), PARAMETER :: myName = "obj_SetElemsdForTime()"
INTEGER(I4B) :: nips, nns, cellOrder(1), order, cellOrient(1), nrow, ncol, &
                ii, jj
REAL(DFP) :: refElemCoord(1, 2), temp(2, MAX_ORDER_TIME + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = obj%timeOrder(timeElemNum)
nips = Quad_Size(obj%quadForTime, 2)
nns = obj%totalDOFTime(timeElemNum)
refElemCoord(1, 1) = -1.0_DFP
refElemCoord(1, 2) = 1.0_DFP
cellOrient(1) = 1

CALL Elemsd_Allocate(obj=obj%elemsdForTime, nsd=1_I4B, xidim=1_I4B, &
                     nns=nns, nips=nips)

CALL LagrangeElemShapeData(obj=obj%linElemsdForTime, &
                           quad=obj%quadForTime, &
                           nsd=obj%elemsdForTime%nsd, &
                           xidim=obj%elemsdForTime%xidim, &
                           elemtype=elem%line, &
                           refelemCoord=refelemCoord, &
                           domainName="B", &
                           order=1_I4B)

SELECT CASE (obj%baseInterpolationForTime)
CASE ("LAGR")

  CALL LagrangeElemShapeData(obj=obj%elemsdForTime, &
                             quad=obj%quadForTime, &
                             nsd=obj%elemsdForTime%nsd, &
                             xidim=obj%elemsdForTime%xidim, &
                             elemtype=elem%line, &
                             refelemCoord=refelemCoord, &
                             domainName="B", &
                             order=order, &
                             ipType=obj%ipTypeForTime, &
                             basisType=obj%baseTypeForTime)

  obj%timeShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%timeShapeFuncBndy(1, 1) = 1.0_DFP

  obj%timeShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%timeShapeFuncBndy(2, 2) = 1.0_DFP

CASE ("HIER", "HEIR")

  cellOrder = order
  CALL HierarchicalElemShapeData(obj=obj%elemsdForTime, &
                                 quad=obj%quadForTime, &
                                 nsd=obj%elemsdForTime%nsd, &
                                 xidim=obj%elemsdForTime%xidim, &
                                 elemtype=elem%line, &
                                 refelemCoord=refelemCoord, &
                                 domainName="B", &
                                 cellOrder=cellOrder, &
                                 cellOrient=cellOrient)

  obj%timeShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%timeShapeFuncBndy(1, 1) = 1.0_DFP

  obj%timeShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%timeShapeFuncBndy(2, 2) = 1.0_DFP

CASE ("ORTH")

  cellOrder = order
  CALL OrthogonalElemShapeData(obj=obj%elemsdForTime, &
                               quad=obj%quadForTime, &
                               nsd=obj%elemsdForTime%nsd, &
                               xidim=obj%elemsdForTime%xidim, &
                               elemtype=elem%line, &
                               refelemCoord=refelemCoord, &
                               domainName="B", order=order, &
                               basisType=poly%legendre)

  CALL OrthogonalBasis_Line_(order=order, xij=refElemCoord, &
                             refLine="B", basisType=poly%legendre, ans=temp, &
                             nrow=nrow, ncol=ncol)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    obj%timeShapeFuncBndy(jj, ii) = temp(ii, jj)
  END DO

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'no case found for baseInterpolationForTime')

END SELECT

CALL Elemsd_Set(obj=obj%elemsdForTime, val=tij, &
                N=obj%linElemsdForTime%N(1:2, 1:nips), &
                dNdXi=obj%linElemsdForTime%dNdXi(1:2, 1:1, 1:nips))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetElemsdForTime

!----------------------------------------------------------------------------
!                                                           GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

DO ii = obj%conIA(spaceElemNum), obj%conIA(spaceElemNum + 1) - 1
  tsize = tsize + 1
  ans(tsize) = obj%conJA(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                             AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleTanmat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleTanmat()"
INTEGER(I4B) :: ielSpace, nrow, ncol, nns, nnt, tcon, con(256)
REAL(DFP) :: dt, dt_by_2, dts, dts_by_2, dx, dx_by_2, &
             dx2, dx2_by_2, two_by_dx, xij(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%InitiateConnectivity()

CALL obj%InitiateFields(timeElemNum=timeElemNum)

dt = obj%timeElemLength(timeElemNum)
dt_by_2 = dt * 0.5_DFP
dts = dt * dt
dts_by_2 = dts * 0.5_DFP

CALL obj%SetQuadForTime(timeElemNum)
CALL obj%SetElemsdForTime(timeElemNum, tij)

CALL obj%GetMt(ans=obj%mt, nrow=nrow, ncol=ncol)
nnt = nrow

CALL obj%GetMtPlus(ans=obj%mtplus, nrow=nrow, ncol=ncol)

CALL obj%GetCt(ans=obj%ct, nrow=nrow, ncol=ncol)

CALL obj%GetWt(ans=obj%ct, nrow=nrow, ncol=ncol)

CALL obj%GetAt()

CALL obj%GetBt()

CALL obj%GetKt_Tilda(ans=obj%kt_tilda, nrow=nrow, ncol=ncol)

xij(1, 1) = obj%spaceDomain(1)

CALL CSRMatrix_Set(obj=obj%tanmat, VALUE=zero)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP
  dx2 = dx * dx
  dx2_by_2 = dx2 * 0.5_DFP
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  obj%ke = 0.0_DFP

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * dx_by_2 &
                           * obj%ms(1:nrow, 1:ncol)

  nns = nrow

  CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
  obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                           two_by_dx * obj%ks(1:nrow, 1:ncol)

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  CALL OTimesTilda(a=obj%ct(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=zero, scale=one)

  CALL OTimesTilda(a=obj%mtplus(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=one)

  CALL OTimesTilda(a=obj%kt_tilda(1:nnt, 1:nnt), &
                   b=obj%ks(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=dts_by_2)

  CALL OTimesTilda(a=obj%mt(1:nnt, 1:nnt), &
                   b=obj%cs(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=dt_by_2)

  CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ke(1:nrow, 1:ncol), &
                     scale=one, storageFMT=DOF_FMT, nodenum=con(1:tcon))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleTanmat

!----------------------------------------------------------------------------
!                                                                AssembleRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"
#endif

INTEGER(I4B) :: con(256), ielSpace, nrow, ncol, nns, nnt, tsize

REAL(DFP) :: dx, dx_by_2, two_by_dx, dt, minus_dt_by_2, &
             f1(MAX_ORDER_SPACE + 1), &
             f2(MAX_ORDER_SPACE + 1), &
             v0(MAX_ORDER_SPACE + 1), &
             u0(MAX_ORDER_SPACE + 1), &
             xij(1, 2)

INTEGER(I4B), PARAMETER :: conversion(1) = [NONE]

LOGICAL(LGT) :: isTractionLeft, isTractionRight

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL RealVector_Set(obj=obj%rhs, VALUE=zero)

nnt = obj%elemsdForTime%nns

isTractionRight = ASSOCIATED(obj%tractionRight)
isTractionLeft = ASSOCIATED(obj%tractionLeft)

dt = obj%timeElemLength(timeElemNum)
minus_dt_by_2 = minus_one * dt * 0.5_DFP

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  obj%rhse = 0.0_DFP

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * &
                           dx_by_2 * obj%ms(1:nrow, 1:ncol)

  CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
  obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                           two_by_dx * obj%ks(1:nrow, 1:ncol)

  CALL RealVector_GetValue_(obj=obj%v0, nodenum=con(1:nns), VALUE=v0, &
                            tsize=nns)

  CALL RealVector_GetValue_(obj=obj%u0, nodenum=con(1:nns), VALUE=u0, &
                            tsize=nns)

  f1(1:nns) = MATMUL(obj%ms(1:nrow, 1:ncol), v0(1:ncol))
  f2(1:nns) = MATMUL(obj%ks(1:nrow, 1:ncol), u0(1:ncol))

  CALL OTimesTilda(a=obj%timeShapeFuncBndy(1:nnt, 1), b=f1(1:nns), ans=obj%rhse, &
                   tsize=tsize, anscoeff=zero, scale=one)

  CALL OTimesTilda(a=obj%tat(1:nnt), b=f2(1:nns), ans=obj%rhse, &
                   tsize=tsize, anscoeff=one, scale=minus_dt_by_2)

  !! Body force
  CALL obj%GetBodyForce(ans=obj%rhse, tsize=tsize, spaceElemNum=ielSpace, &
                        timeElemNum=timeElemNum, anscoeff=one, scale=one)

  ! CALL RealVector_Add(obj=)
  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

  xij(1, 1) = xij(1, 2)

END DO

! Traction right
IF (isTractionRight) THEN
  CALL obj%GetTractionRight(ans=obj%rhse, tsize=tsize, &
                            timeElemNum=timeElemNum, anscoeff=zero, scale=one)

  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

END IF

! Traction left
IF (isTractionLeft) THEN
  CALL obj%GetTractionLeft(ans=obj%rhse, tsize=tsize, &
                           timeElemNum=timeElemNum, anscoeff=zero, scale=one)

  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!                                                              GetBodyForce
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBodyForce
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBodyForce()"
#endif

INTEGER(I4B) :: nns, nnt, nips, nipt, ii, a, ipt, ips
REAL(DFP) :: r(10), args(2), t, js_jt
LOGICAL(LGT) :: isBodyForce

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isBodyForce = ASSOCIATED(obj%bodyForce)
IF (.NOT. isBodyForce) RETURN

nns = obj%elemsdForSpace%nns
nnt = obj%elemsdForTime%nns
nips = obj%elemsdForSpace%nips
nipt = obj%elemsdForTime%nips
tsize = nns * nnt
ans(1:tsize) = ans(1:tsize) * anscoeff

js_jt = obj%spaceElemLength(spaceElemNum) * obj%timeElemLength(timeElemNum) &
        * 0.25_DFP * obj%density(spaceElemNum)

DO ipt = 1, nipt

  t = obj%elemsdForTime%coord(1, ipt)
  args(2) = t

  r(1) = obj%elemsdForTime%ws(ipt) * js_jt

  DO ips = 1, nips

    args(1) = obj%elemsdForSpace%coord(1, ips)

    CALL obj%bodyForce%Get(val=r(2), args=args)

    r(3) = obj%elemsdForSpace%ws(ips)

    r(4) = r(1) * r(2) * r(3)

    DO a = 1, nnt

      r(5) = obj%elemsdForTime%N(a, ipt)

      r(6) = r(4) * r(5)

      DO ii = 1, nns

        r(7) = obj%elemsdForSpace%N(ii, ips)

        r(8) = scale * r(6) * r(7)

        ans(ii + (a - 1) * nns) = ans(ii + (a - 1) * nns) + r(8)

      END DO

    END DO

  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetBodyForce

!----------------------------------------------------------------------------
!                                                            GetTractionLeft
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTractionLeft
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTractionLeft()"
#endif

INTEGER(I4B) :: nns, nnt, nipt, ii, a, ipt
REAL(DFP) :: r(10), args(1), dt_by_2
LOGICAL(LGT) :: isTractionLeft

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isTractionLeft = ASSOCIATED(obj%tractionLeft)
IF (.NOT. isTractionLeft) RETURN

nns = obj%elemsdForSpace%nns
nnt = obj%elemsdForTime%nns
nipt = obj%elemsdForTime%nips
tsize = nns * nnt
ans(1:tsize) = ans(1:tsize) * anscoeff

dt_by_2 = obj%timeElemLength(timeElemNum) * 0.5_DFP

DO ipt = 1, nipt

  args(1) = obj%elemsdForTime%coord(1, ipt)
  r(1) = obj%elemsdForTime%ws(ipt) * dt_by_2
  CALL obj%tractionLeft%Get(val=r(2), args=args)
  r(3) = r(1) * r(2)

  DO a = 1, nnt

    r(4) = obj%elemsdForTime%N(a, ipt)
    r(5) = r(3) * r(4)

    DO ii = 1, nns

      r(6) = obj%spaceShapeFuncBndy(ii, 1)
      r(7) = scale * r(5) * r(6)

      ans(ii + (a - 1) * nns) = ans(ii + (a - 1) * nns) + r(7)

    END DO

  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTractionLeft

!----------------------------------------------------------------------------
!                                                          GetTractionRight
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTractionRight
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTractionRight()"
#endif

INTEGER(I4B) :: nns, nnt, nipt, ii, a, ipt
REAL(DFP) :: r(10), args(1), dt_by_2
LOGICAL(LGT) :: isTractionRight

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isTractionRight = ASSOCIATED(obj%tractionRight)
IF (.NOT. isTractionRight) RETURN

nns = obj%elemsdForSpace%nns
nnt = obj%elemsdForTime%nns
nipt = obj%elemsdForTime%nips
tsize = nns * nnt
ans(1:tsize) = ans(1:tsize) * anscoeff

dt_by_2 = obj%timeElemLength(timeElemNum) * 0.5_DFP

DO ipt = 1, nipt

  args(1) = obj%elemsdForTime%coord(1, ipt)
  r(1) = obj%elemsdForTime%ws(ipt) * dt_by_2
  CALL obj%tractionRight%Get(val=r(2), args=args)
  r(3) = r(1) * r(2)

  DO a = 1, nnt

    r(4) = obj%elemsdForTime%N(a, ipt)
    r(5) = r(3) * r(4)

    DO ii = 1, nns

      r(6) = obj%spaceShapeFuncBndy(ii, 2)
      r(7) = scale * r(5) * r(6)

      ans(ii + (a - 1) * nns) = ans(ii + (a - 1) * nns) + r(7)

    END DO

  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTractionRight

!----------------------------------------------------------------------------
!                                                          ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC()"
#endif

LOGICAL(LGT) :: isDirichletLeft, isDirichletRight
INTEGER(I4B) :: ii, nnt, nipt, jj, tsize_dbc_idof, tsize, tsize_dbc_value

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isDirichletLeft = ASSOCIATED(obj%velocityLeft)
isDirichletRight = ASSOCIATED(obj%velocityRight)
tsize_dbc_idof = 0
tsize_dbc_value = 0

IF (isDirichletLeft) THEN
  CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof, tsize=tsize, &
                     nodenum=1)

  tsize_dbc_idof = tsize

  nnt = obj%elemsdForTime%nns
  nipt = obj%elemsdForTime%nips

  DO ii = 1, nipt
    ! obj%dbc_coeff(ii) = obj%velocityLeft(obj%elemsdForTime%coord(1, ii))
    CALL obj%velocityLeft%Get(val=obj%dbc_coeff(ii), &
                              args=obj%elemsdForTime%coord(1, ii:ii))
  END DO

  obj%dbc_rhs(1:nnt) = 0.0
  DO ii = 1, nipt
    DO jj = 1, nnt
      obj%dbc_rhs(jj) = obj%dbc_rhs(jj) + obj%elemsdForTime%N(jj, ii) &
                        * obj%elemsdForTime%ws(ii) * obj%dbc_coeff(ii)
    END DO
  END DO

  CALL obj%GetMt(ans=obj%mt, nrow=ii, ncol=jj)

  CALL SymLinSolve(X=obj%dbc_value(1:nnt), A=obj%mt(1:nnt, 1:nnt), &
                   B=obj%dbc_rhs(1:nnt))

  tsize_dbc_value = tsize_dbc_value + nnt
END IF

IF (isDirichletRight) THEN
  CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof(tsize_dbc_idof + 1:), &
                     tsize=tsize, &
                     nodenum=obj%totalSpaceNodes)

  tsize_dbc_idof = tsize_dbc_idof + tsize

  nnt = obj%elemsdForTime%nns
  nipt = obj%elemsdForTime%nips

  DO ii = 1, nipt
    CALL obj%velocityRight%Get(val=obj%dbc_coeff(ii), &
                               args=obj%elemsdForTime%coord(1, ii:ii))
  END DO

  obj%dbc_rhs(1:nnt) = 0.0
  DO ii = 1, nipt
    DO jj = 1, nnt
      obj%dbc_rhs(jj) = obj%dbc_rhs(jj) + obj%elemsdForTime%N(jj, ii) &
                        * obj%elemsdForTime%ws(ii) * obj%dbc_coeff(ii)
    END DO
  END DO

  CALL obj%GetMt(ans=obj%mt, nrow=ii, ncol=jj)

  CALL SymLinSolve( &
    X=obj%dbc_value(tsize_dbc_value + 1:tsize_dbc_value + nnt), &
    A=obj%mt(1:nnt, 1:nnt), B=obj%dbc_rhs(1:nnt))

  tsize_dbc_value = tsize_dbc_value + nnt
END IF

IF (tsize_dbc_value .NE. 0) THEN
  CALL CSRMatrix_GetSubMatrix(obj=obj%tanmat, &
                              cols=obj%dbc_idof(1:tsize_dbc_idof), &
                              submat=obj%submat, subIndices=obj%subIndices)

  CALL CSRMatrix_ApplyDBC(obj=obj%tanmat, &
                          dbcptrs=obj%dbc_idof(1:tsize_dbc_idof))

  CALL RealVector_Set(obj=obj%sol, VALUE=0.0_DFP)
  CALL RealVector_Set(obj=obj%sol, nodenum=obj%dbc_idof(1:tsize_dbc_idof), &
                      VALUE=obj%dbc_value(1:tsize_dbc_value))

  CALL CSRMatrix_Matvec(obj=obj%submat, x=obj%sol, y=obj%rhs, &
                        scale=minus_one, addContribution=.TRUE.)

  CALL RealVector_Set(obj=obj%rhs, nodenum=obj%dbc_idof(1:tsize_dbc_idof), &
                      VALUE=obj%dbc_value(1:tsize_dbc_value))

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ApplyDirichletBC

!----------------------------------------------------------------------------
!                                                         SetInitialVelocity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInitialVelocity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialVelocity()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, nns, nips, jj, ielSpace, con(256), tcon
REAL(DFP) :: xij(1, 2), val(MAX_ORDER_SPACE + 1), dx, &
             coeff(MAX_ORDER_SPACE * 2 + 2), rhs(MAX_ORDER_SPACE + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%initialVel)
IF (.NOT. isok) THEN
  CALL RealVector_Set(obj=obj%v0, VALUE=zero)
  RETURN
END IF

CALL obj%InitiateConnectivity()

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)
  CALL obj%GetMs(ans=obj%ms, nrow=ii, ncol=jj)

  nns = obj%elemsdForSpace%nns
  nips = obj%elemsdForSpace%nips

  DO ii = 1, nips
    CALL obj%initialVel%Get(val=coeff(ii), &
                            args=obj%elemsdForSpace%coord(1, ii:ii))
  END DO

  rhs(1:nns) = 0.0

  DO jj = 1, nns
    DO ii = 1, nips
      rhs(jj) = rhs(jj) + obj%elemsdForSpace%N(jj, ii) &
                * obj%elemsdForSpace%ws(ii) * coeff(ii)
    END DO
  END DO

  CALL SymLinSolve(X=val(1:nns), A=obj%ms(1:nns, 1:nns), &
                   B=rhs(1:nns))

  CALL RealVector_Set(obj=obj%v0, nodenum=con(1:nns), VALUE=val(1:nns))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetInitialVelocity

!----------------------------------------------------------------------------
!                                                         SetInitialVelocity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInitialDisplacement
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialDisplacement()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, nns, nips, jj, ielSpace, con(256), tcon
REAL(DFP) :: xij(1, 2), val(MAX_ORDER_SPACE + 1), dx, &
             coeff(MAX_ORDER_SPACE * 2 + 2), rhs(MAX_ORDER_SPACE + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%initialDisp)
IF (.NOT. isok) THEN
  CALL RealVector_Set(obj=obj%u0, VALUE=zero)
  RETURN
END IF

CALL obj%InitiateConnectivity()

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)
  CALL obj%GetMs(ans=obj%ms, nrow=ii, ncol=jj)

  nns = obj%elemsdForSpace%nns
  nips = obj%elemsdForSpace%nips

  DO ii = 1, nips
    CALL obj%initialDisp%Get(val=coeff(ii), &
                             args=obj%elemsdForSpace%coord(1, ii:ii))
  END DO

  rhs(1:nns) = 0.0

  DO jj = 1, nns
    DO ii = 1, nips
      rhs(jj) = rhs(jj) + obj%elemsdForSpace%N(jj, ii) &
                * obj%elemsdForSpace%ws(ii) * coeff(ii)
    END DO
  END DO

  CALL SymLinSolve(X=val(1:nns), A=obj%ms(1:nns, 1:nns), &
                   B=rhs(1:nns))

  CALL RealVector_Set(obj=obj%u0, nodenum=con(1:nns), VALUE=val(1:nns))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetInitialDisplacement

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Solve
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Solve()"
#endif

INTEGER(I4B) :: n, solverName

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

n = CSRMatrix_Size(obj%tanmat, 1)
solverName = LIS_GMRES

CALL CSRMatrixLinSolveInitiate(ipar=obj%ipar, fpar=obj%fpar, W=obj%work, &
                               n=n, solverName=solverName)

CALL CSRMatrix_LinSolve(obj=obj%tanmat, sol=obj%sol%val(1:n), &
               rhs=obj%rhs%val(1:n), ipar=obj%ipar, fpar=obj%fpar, W=obj%work)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Solve

!----------------------------------------------------------------------------
!                                                                   Update
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Update
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Update()"
#endif

INTEGER(I4B) :: ii, nnt
REAL(DFP) :: scale, dt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dt = obj%timeElemLength(obj%currentTimeStep)
obj%currentTime = obj%currentTime + dt
obj%currentTimeStep = obj%currentTimeStep + 1

nnt = obj%elemsdForTime%nns

CALL RealVector_Set(obj=obj%v0, VALUE=zero)

CALL RealVector_Scale(obj%u0, obj%at_right)

DO ii = 1, nnt
  scale = obj%timeShapeFuncBndy(ii, 2)
  CALL RealVector_Add(obj1=obj%v0, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%sol, dofobj2=obj%dof, idof2=ii, scale=scale)

  scale = obj%bt_right(ii) * dt
  CALL RealVector_Add(obj1=obj%u0, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%sol, dofobj2=obj%dof, idof2=ii, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Update

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteData()"
#endif

REAL(DFP) :: t, dx, xij(1, 2), u0(MAX_ORDER_SPACE + 1), &
             v0(MAX_ORDER_SPACE + 1), xlim(2), ylim(2)

REAL(DFP), ALLOCATABLE :: DATA(:, :)

INTEGER(I4B) :: ielSpace, con(MAX_ORDER_SPACE + 1), nns, nips, &
                totalNodes, ii

CHARACTER(:), ALLOCATABLE :: filename_disp, filename_vel, filename_acc, &
                             aline, filename_data

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

t = obj%currentTime
xij(1, 1) = obj%spaceDomain(1)

filename_disp = obj%result_dir//CHAR_SLASH//obj%filename//'_disp_'// &
                tostring(obj%currentTimeStep)

filename_vel = obj%result_dir//CHAR_SLASH//obj%filename//'_vel_'// &
               tostring(obj%currentTimeStep)

filename_acc = obj%result_dir//CHAR_SLASH//obj%filename//'_acc_'// &
               tostring(obj%currentTimeStep)

filename_data = obj%result_dir//CHAR_SLASH//obj%filename//'_data_'// &
                tostring(obj%currentTimeStep)

totalNodes = obj%totalVertexDOFSpace

ALLOCATE (DATA(totalNodes, 3))

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

  dx = obj%spaceElemLength(ielSpace)

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  CALL RealVector_GetValue_(obj=obj%u0, nodenum=con(1:nns), VALUE=u0, &
                            tsize=nns)

  CALL RealVector_GetValue_(obj=obj%v0, nodenum=con(1:nns), VALUE=v0, &
                            tsize=nns)

  nips = obj%elemsdForSpace%nips

  DATA(con(1), 1) = xij(1, 1)
  DATA(con(2), 1) = xij(1, 2)

  DATA(con(1), 2) = DOT_PRODUCT(u0(1:nns), obj%spaceShapeFuncBndy(1:nns, 1))
  DATA(con(2), 2) = DOT_PRODUCT(u0(1:nns), obj%spaceShapeFuncBndy(1:nns, 2))

  DATA(con(1), 3) = DOT_PRODUCT(v0(1:nns), obj%spaceShapeFuncBndy(1:nns, 1))
  DATA(con(2), 3) = DOT_PRODUCT(v0(1:nns), obj%spaceShapeFuncBndy(1:nns, 2))

  xij(1, 1) = xij(1, 2)

END DO

! csv file

! disp
#ifdef DEBUG_VER
CALL Display("Writing data to file: "//filename_disp//".csv")
#endif
CALL obj%dispfile%Initiate(filename=filename_disp//".csv", unit=100, &
                 status="REPLACE", action="WRITE", comment="#", separator=",")
CALL obj%dispfile%OPEN()

aline = "# time-step = "//tostring(obj%currentTimeStep)// &
        ", time = "//tostring(obj%currentTime)//" s"
CALL obj%dispfile%WRITE(aline)

aline = "x, disp"
CALL obj%dispfile%WRITE(aline)

DO ii = 1, totalNodes
  aline = tostring(DATA(ii, 1))//", "//tostring(DATA(ii, 2))
  CALL obj%dispfile%WRITE(aline)
END DO

! vel
#ifdef DEBUG_VER
CALL Display("Writing data to file: "//filename_vel//".csv")
#endif
CALL obj%velfile%Initiate(filename=filename_vel//".csv", &
                 status="REPLACE", action="WRITE", comment="#", separator=",")
CALL obj%velfile%OPEN()

aline = "# time-step = "//tostring(obj%currentTimeStep)// &
        ", time = "//tostring(obj%currentTime)//" s"
CALL obj%velfile%WRITE(aline)

aline = "x, vel"
CALL obj%velfile%WRITE(aline)

DO ii = 1, totalNodes
  aline = tostring(DATA(ii, 1))//", "//tostring(DATA(ii, 3))
  CALL obj%velfile%WRITE(aline)
END DO

! acc
#ifdef DEBUG_VER
CALL Display("Writing data to file: "//filename_acc//".csv")
#endif
CALL obj%accfile%Initiate(filename=filename_acc//".csv", &
                 status="REPLACE", action="WRITE", comment="#", separator=",")
CALL obj%accfile%OPEN()

aline = "# time-step = "//tostring(obj%currentTimeStep)// &
        ", time = "//tostring(obj%currentTime)//" s"
CALL obj%accfile%WRITE(aline)
aline = "x, acc"
CALL obj%accfile%WRITE(aline)
!TODO: Write acceleration data

! write all data
#ifdef DEBUG_VER
CALL Display("Writing data to file: "//filename_data//".csv")
#endif
CALL obj%datafile%Initiate(filename=filename_data//".csv", &
                 status="REPLACE", action="WRITE", comment="#", separator=",")
CALL obj%datafile%OPEN()

aline = "# time-step = "//tostring(obj%currentTimeStep)// &
        ", time = "//tostring(obj%currentTime)//" s"
CALL obj%datafile%WRITE(aline)

aline = "x, disp, vel"
CALL obj%datafile%WRITE(aline)
CALL obj%datafile%WRITE(val=DATA(1:totalNodes, 1:3), orient="ROW")

#ifdef DEBUG_VER
CALL Display("Done writing files csvfiles")
#endif

CALL obj%dispfile%DEALLOCATE()
CALL obj%velfile%DEALLOCATE()
CALL obj%accfile%DEALLOCATE()
CALL obj%datafile%DEALLOCATE()

! plotting

CALL obj%plot%filename(filename_disp//'.plt')
CALL obj%plot%options('set terminal pngcairo; set output "'//filename_disp//'.png"')
xlim = obj%spaceDomain
ylim(1) = MINVAL(DATA(1:totalNodes, 2))
ylim(2) = MAXVAL(DATA(1:totalNodes, 2))
xlim(1) = xlim(1) - 0.1 * (xlim(2) - xlim(1))
xlim(2) = xlim(2) + 0.1 * (xlim(2) - xlim(1))
ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

CALL obj%plot%xlim(xlim)
CALL obj%plot%ylim(ylim)
CALL obj%plot%xlabel('x')
CALL obj%plot%ylabel('u')
CALL obj%plot%plot(x1=DATA(1:totalNodes, 1), y1=DATA(1:totalNodes, 2))
CALL obj%plot%reset()

CALL obj%plot%filename(filename_vel//'.plt')
CALL obj%plot%options('set terminal pngcairo; set output "'//filename_vel//'.png"')
xlim = obj%spaceDomain
ylim(1) = MINVAL(DATA(1:totalNodes, 3))
ylim(2) = MAXVAL(DATA(1:totalNodes, 3))
xlim(1) = xlim(1) - 0.1 * (xlim(2) - xlim(1))
xlim(2) = xlim(2) + 0.1 * (xlim(2) - xlim(1))
ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

CALL obj%plot%xlim(xlim)
CALL obj%plot%ylim(ylim)
CALL obj%plot%xlabel('x')
CALL obj%plot%ylabel('v')
CALL obj%plot%plot(x1=DATA(1:totalNodes, 1), y1=DATA(1:totalNodes, 3))
CALL obj%plot%reset()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_WriteData

!----------------------------------------------------------------------------
!                                                                    Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Run
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Run()"
#endif

INTEGER(I4B) :: ielTime
REAL(DFP) :: x1, tij(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

x1 = obj%spaceDomain(1)
tij(1, 1) = obj%timeDomain(1)

CALL obj%SetInitialVelocity()
CALL obj%SetInitialDisplacement()

DO ielTime = 1, obj%totalTimeElements
  CALL Display(tij(1, 1), myname//" t1: ")
  tij(1, 2) = tij(1, 1) + obj%timeElemLength(ielTime)
  CALL obj%AssembleTanmat(timeElemNum=ielTime, tij=tij)
  CALL obj%AssembleRHS(timeElemNum=ielTime, tij=tij)
  CALL obj%ApplyDirichletBC(timeElemNum=ielTime, tij=tij)
  CALL obj%Solve()
  CALL obj%Update()
  CALL obj%WriteData()
  tij(1, 1) = tij(1, 2)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Run

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
