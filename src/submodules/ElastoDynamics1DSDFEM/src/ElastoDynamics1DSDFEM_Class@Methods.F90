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

SUBMODULE(ElastoDynamics1DSDFEM_Class) Methods
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

USE LagrangePolynomialUtility, ONLY: InterpolationPoint_

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
obj%baseInterpolationForSpace = "LAGR"
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
CALL Display(obj%spaceDomain, "spaceDomain: ", unitno=unitno)
CALL Display(obj%timeRange, "timeRange: ", unitno=unitno)

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

isok = ALLOCATED(obj%timeStepSize)
CALL Display(isok, "timeStepSize: ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  CALL Display(obj%timeStepSize, "timeStepSize: ", unitno=unitno)
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

isok = ASSOCIATED(obj%displacementLeft)
CALL Display(isok, "displacementLeft ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%displacementRight)
CALL Display(isok, "displacementRight ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%initialDisp)
CALL Display(isok, "initialDisp ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%initialVel)
CALL Display(isok, "initialVel ASSOCIATED: ", unitno=unitno)

isok = ASSOCIATED(obj%initialAcc)
CALL Display(isok, "initialAcc ASSOCIATED: ", unitno=unitno)

CALL obj%algoParam%Display("SDAlgoParam: ", unitno=unitno)

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
CALL Display(myName//" totalTimeSteps")
#endif

CALL GetValue(table=table, key="totalTimeSteps", &
              VALUE=obj%totalTimeSteps, &
              default_value=0_I4B, origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
CALL Display(myName//" baseInterpolationForSpace")
#endif

CALL GetValue(table=table, key="baseInterpolationForSpace", &
              VALUE=astr, default_value=default_baseInterpolationForSpace, &
              origin=origin, stat=stat, isfound=isok)
obj%baseInterpolationForSpace = UpperCase(astr%slice(1, 4))

CALL obj%algoParam%ImportFromToml(table=table)

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
CALL Display(myName//" timeRange")
#endif

CALL GetValue_(table=table, key="timeRange", tsize=tsize, &
               VALUE=obj%timeRange, origin=origin, stat=stat, isfound=isok)
isok = tsize .EQ. 2
CALL AssertError1(isok, myname, "timeRange should have 2 values")

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

! !INFO: timeStepSize
#ifdef DEBUG_VER
CALL Display(myName//" timeStepSize")
#endif

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

!INFO: initialAcc
astr = "initialAcc"
#ifdef DEBUG_VER
CALL Display(myName//astr%chars())
#endif
node => NULL()
CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
              stat=stat)
isok = ASSOCIATED(node)
IF (isok) THEN
  ALLOCATE (obj%initialAcc)
  CALL obj%initialAcc%ImportFromToml(table=node)
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
obj%currentTime = obj%timeRange(1)

CALL SetTotalDOFSpace(obj=obj)

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
timeCompo(1) = 1
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
CALL RealVector_Initiate(obj%force1, nrow)
CALL RealVector_Initiate(obj%force2, nrow)
CALL RealVector_Initiate(obj%tmp1, nrow)

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
  CLASS(ElastoDynamics1DSDFEM_), INTENT(INOUT) :: obj

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
INTEGER(I4B) :: ielSpace, nrow, ncol, tcon, con(256)
REAL(DFP) :: dt, dt_by_2, dts, dts_by_2, dx, dx_by_2, &
             dx2, dx2_by_2, two_by_dx, xij(1, 2), scale
LOGICAL(LGT) :: isMass, isStiff, isDamp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%InitiateConnectivity()

CALL obj%InitiateFields()

dt = obj%timeStepSize(obj%currentTimeStep)
dt_by_2 = dt * 0.5_DFP
dts = dt * dt
dts_by_2 = dts * 0.5_DFP

isMass = .NOT. obj%algoParam%tanmat_zero(1)
isStiff = .NOT. obj%algoParam%tanmat_zero(2)
isDamp = .NOT. obj%algoParam%tanmat_zero(3)

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

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * dx_by_2 &
                           * obj%ms(1:nrow, 1:ncol)

  CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
  obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                           two_by_dx * obj%ks(1:nrow, 1:ncol)

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  IF (isMass) THEN
    scale = obj%algoParam%tanmat(1)
    CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ms(1:nrow, 1:ncol), &
                       scale=scale, storageFMT=DOF_FMT, nodenum=con(1:tcon))
  END IF

  IF (isDamp) THEN
    scale = obj%algoParam%tanmat(2) * dt
    CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%cs(1:nrow, 1:ncol), &
                       scale=scale, storageFMT=DOF_FMT, nodenum=con(1:tcon))
  END IF

  IF (isStiff) THEN
    scale = obj%algoParam%tanmat(3) * dts
    CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ks(1:nrow, 1:ncol), &
                       scale=scale, storageFMT=DOF_FMT, nodenum=con(1:tcon))
  END IF

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

INTEGER(I4B) :: con(256), ielSpace, nrow, ncol, nns, tsize

REAL(DFP) :: dx, dx_by_2, two_by_dx, dt, dts, &
             v0(MAX_ORDER_SPACE + 1), &
             u0(MAX_ORDER_SPACE + 1), &
             a0(MAX_ORDER_SPACE + 1), &
             tmp(MAX_ORDER_SPACE + 1), &
             xij(1, 2), time

INTEGER(I4B), PARAMETER :: conversion(1) = [NONE]

LOGICAL(LGT) :: isTractionLeft, isTractionRight, &
                isNonZero, isBodyForce

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL RealVector_Set(obj=obj%rhs, VALUE=zero)

isTractionRight = ASSOCIATED(obj%tractionRight)
isTractionLeft = ASSOCIATED(obj%tractionLeft)
isBodyForce = ASSOCIATED(obj%bodyForce)

dt = obj%timeStepSize(obj%currentTimeStep)
dts = dt * dt
time = obj%currentTime + (1.0_DFP + obj%algoParam%alpha) * dt

isNonZero = .NOT. obj%algoParam%rhs_f1_zero
IF (isNonZero) THEN
  CALL RealVector_Add(obj1=obj%rhs, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%force1, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=obj%algoParam%rhs_f1 * dts)
END IF

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * &
                           dx_by_2 * obj%ms(1:nrow, 1:ncol)

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
  obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                           two_by_dx * obj%ks(1:nrow, 1:ncol)

  CALL RealVector_GetValue_(obj=obj%v0, nodenum=con(1:nns), VALUE=v0, &
                            tsize=nns)

  CALL RealVector_GetValue_(obj=obj%u0, nodenum=con(1:nns), VALUE=u0, &
                            tsize=nns)

  CALL RealVector_GetValue_(obj=obj%a0, nodenum=con(1:nns), VALUE=a0, &
                            tsize=nns)

  obj%rhse = 0.0_DFP
  tmp = 0.0_DFP
  ! mass contribution
  isNonZero = .NOT. obj%algoParam%rhs_u1_zero(1)
  IF (isNonZero) tmp = tmp + u0 * obj%algoParam%rhs_u1(1)

  isNonZero = .NOT. obj%algoParam%rhs_v1_zero(1)
  IF (isNonZero) tmp = tmp + dt * v0 * obj%algoParam%rhs_v1(1)

  isNonZero = .NOT. obj%algoParam%rhs_a1_zero(1)
  IF (isNonZero) tmp = tmp + dts * a0 * obj%algoParam%rhs_a1(1)

  obj%rhse(1:nns) = obj%rhse(1:nns) + MATMUL(obj%ms(1:nrow, 1:ncol), tmp(1:ncol))

  ! damp contribution
  tmp = 0.0_DFP
  ! mass contribution
  isNonZero = .NOT. obj%algoParam%rhs_u1_zero(2)
  IF (isNonZero) tmp = tmp + u0 * obj%algoParam%rhs_u1(2)

  isNonZero = .NOT. obj%algoParam%rhs_v1_zero(2)
  IF (isNonZero) tmp = tmp + dt * v0 * obj%algoParam%rhs_v1(2)

  isNonZero = .NOT. obj%algoParam%rhs_a1_zero(2)
  IF (isNonZero) tmp = tmp + dts * a0 * obj%algoParam%rhs_a1(2)
  obj%rhse(1:nns) = obj%rhse(1:nns) + MATMUL(obj%cs(1:nrow, 1:ncol), tmp(1:ncol))

  ! stiff contribution
  tmp = 0.0_DFP
  ! mass contribution
  isNonZero = .NOT. obj%algoParam%rhs_u1_zero(3)
  IF (isNonZero) tmp = tmp + u0 * obj%algoParam%rhs_u1(3)

  isNonZero = .NOT. obj%algoParam%rhs_v1_zero(3)
  IF (isNonZero) tmp = tmp + dt * v0 * obj%algoParam%rhs_v1(3)

  isNonZero = .NOT. obj%algoParam%rhs_a1_zero(3)
  IF (isNonZero) tmp = tmp + dts * a0 * obj%algoParam%rhs_a1(3)
  obj%rhse(1:nns) = obj%rhse(1:nns) + MATMUL(obj%ks(1:nrow, 1:ncol), tmp(1:ncol))

  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:nns), &
                      scale=one, dofobj=obj%dof, nodenum=con(1:nns), &
                      conversion=conversion)

  !! Body force
  IF (isBodyForce) THEN
    CALL obj%GetBodyForce(ans=obj%rhse, tsize=tsize, spaceElemNum=ielSpace, &
                          time=time, anscoeff=zero, scale=one)

    CALL RealVector_Add(obj=obj%force2, VALUE=obj%rhse(1:tsize), &
                        scale=one, dofobj=obj%dof, nodenum=con(1:nns), &
                        conversion=conversion)
  END IF

  xij(1, 1) = xij(1, 2)

END DO

! Traction right
IF (isTractionRight) THEN
  CALL obj%GetTractionRight(ans=obj%rhse, tsize=tsize, &
                            time=time, anscoeff=zero, &
                            scale=one)

  CALL RealVector_Add(obj=obj%force2, VALUE=obj%rhse(1:tsize), &
       scale=one, dofobj=obj%dof, nodenum=con(1:tsize), conversion=conversion)

END IF

! Traction left
IF (isTractionLeft) THEN
  CALL obj%GetTractionLeft(ans=obj%rhse, tsize=tsize, &
                           time=time, anscoeff=zero, &
                           scale=one)

  CALL RealVector_Add(obj=obj%force2, VALUE=obj%rhse(1:tsize), &
       scale=one, dofobj=obj%dof, nodenum=con(1:tsize), conversion=conversion)
END IF

isNonZero = .NOT. obj%algoParam%rhs_f2_zero
IF (isNonZero) THEN
  CALL RealVector_Add(obj1=obj%rhs, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%force2, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=obj%algoParam%rhs_f2 * dts)
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

INTEGER(I4B) :: nns, nips, ii, ips
REAL(DFP) :: r(10), args(2), js
LOGICAL(LGT) :: isBodyForce

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isBodyForce = ASSOCIATED(obj%bodyForce)
IF (.NOT. isBodyForce) RETURN

nns = obj%elemsdForSpace%nns
nips = obj%elemsdForSpace%nips
tsize = nns
ans(1:tsize) = ans(1:tsize) * anscoeff

js = obj%spaceElemLength(spaceElemNum) * 0.50_DFP

args(2) = time

DO ips = 1, nips

  args(1) = obj%elemsdForSpace%coord(1, ips)

  CALL obj%bodyForce%Get(val=r(1), args=args)

  r(2) = obj%elemsdForSpace%ws(ips) * js

  r(3) = r(1) * r(2)

  DO ii = 1, nns

    r(4) = obj%elemsdForSpace%N(ii, ips)

    r(5) = scale * r(3) * r(4)

    ans(ii) = ans(ii) + r(5)

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

INTEGER(I4B) :: nns, ii
REAL(DFP) :: r(10), args(1)
LOGICAL(LGT) :: isTractionLeft

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isTractionLeft = ASSOCIATED(obj%tractionLeft)
IF (.NOT. isTractionLeft) RETURN

nns = obj%elemsdForSpace%nns
tsize = nns
ans(1:tsize) = ans(1:tsize) * anscoeff

args(1) = time

CALL obj%tractionLeft%Get(val=r(1), args=args)

DO ii = 1, nns

  r(2) = obj%spaceShapeFuncBndy(ii, 1)
  r(3) = scale * r(2) * r(1)

  ans(ii) = ans(ii) + r(1)

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

INTEGER(I4B) :: nns, ii
REAL(DFP) :: r(10), args(1)
LOGICAL(LGT) :: isTractionRight

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isTractionRight = ASSOCIATED(obj%tractionRight)
IF (.NOT. isTractionRight) RETURN

nns = obj%elemsdForSpace%nns
tsize = nns
ans(1:tsize) = ans(1:tsize) * anscoeff

args(1) = time

CALL obj%tractionRight%Get(val=r(1), args=args)

DO ii = 1, nns

  r(2) = obj%spaceShapeFuncBndy(ii, 1)
  r(3) = scale * r(2) * r(1)

  ans(ii) = ans(ii) + r(1)

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
INTEGER(I4B) :: tsize, tsize_dbc_value
REAL(DFP) :: args(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isDirichletLeft = ASSOCIATED(obj%displacementLeft)
isDirichletRight = ASSOCIATED(obj%displacementRight)
tsize_dbc_value = 0

args = obj%currentTime + obj%timeStepSize(obj%currentTimeStep)

IF (isDirichletLeft) THEN
  CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof, tsize=tsize, &
                     nodenum=1)
  CALL obj%displacementLeft%Get(val=obj%dbc_coeff(1), args=args)
  tsize_dbc_value = tsize_dbc_value + 1
END IF

IF (isDirichletRight) THEN
  CALL DOF_GetIndex_(obj=obj%dof, ans=obj%dbc_idof(tsize_dbc_value + 1:), &
                     tsize=tsize, &
                     nodenum=obj%totalSpaceNodes)
  CALL obj%displacementRight%Get(val=obj%dbc_coeff(2), args=args)
  tsize_dbc_value = tsize_dbc_value + 1
END IF

IF (tsize_dbc_value .NE. 0) THEN
  CALL CSRMatrix_GetSubMatrix(obj=obj%tanmat, &
                              cols=obj%dbc_idof(1:tsize_dbc_value), &
                              submat=obj%submat, subIndices=obj%subIndices)

  CALL CSRMatrix_ApplyDBC(obj=obj%tanmat, &
                          dbcptrs=obj%dbc_idof(1:tsize_dbc_value))

  CALL RealVector_Set(obj=obj%sol, VALUE=0.0_DFP)
  CALL RealVector_Set(obj=obj%sol, nodenum=obj%dbc_idof(1:tsize_dbc_value), &
                      VALUE=obj%dbc_coeff(1:tsize_dbc_value))

  CALL CSRMatrix_Matvec(obj=obj%submat, x=obj%sol, y=obj%rhs, &
                        scale=minus_one, addContribution=.TRUE.)

  CALL RealVector_Set(obj=obj%rhs, nodenum=obj%dbc_idof(1:tsize_dbc_value), &
                      VALUE=obj%dbc_coeff(1:tsize_dbc_value))

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ApplyDirichletBC

!----------------------------------------------------------------------------
!                                                      setInitialAcceleration
!----------------------------------------------------------------------------
MODULE PROCEDURE obj_SetInitialAcceleration
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialAcceleration()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, nns, nips, jj, ielSpace, con(256), tcon
REAL(DFP) :: xij(1, 2), val(MAX_ORDER_SPACE + 1), dx, &
             coeff(MAX_ORDER_SPACE * 2 + 2), rhs(MAX_ORDER_SPACE + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%initialAcc)
IF (.NOT. isok) THEN
  CALL RealVector_Set(obj=obj%a0, VALUE=zero)
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
    CALL obj%initialAcc%Get(val=coeff(ii), &
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

  CALL RealVector_Set(obj=obj%a0, nodenum=con(1:nns), VALUE=val(1:nns))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetInitialAcceleration

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

REAL(DFP) :: scale, dt, dts
LOGICAL(LGT) :: isNonZero

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dt = obj%timeStepSize(obj%currentTimeStep)
dts = dt * dt
obj%currentTime = obj%currentTime + dt
obj%currentTimeStep = obj%currentTimeStep + 1

! CALL RealVector_Set(obj=obj%v0, VALUE=zero)
CALL RealVector_Set(obj=obj%force1, VALUE=obj%force2)

! displacement
CALL RealVector_Set(obj=obj%rhs, VALUE=zero)
scale = obj%algoParam%dis(1)
isNonZero = .NOT. obj%algoParam%dis_zero(1)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%rhs, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%u0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%dis(2) * dt
isNonZero = .NOT. obj%algoParam%dis_zero(2)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%rhs, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%v0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%dis(3) * dts
isNonZero = .NOT. obj%algoParam%dis_zero(3)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%rhs, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%a0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%dis(4)
isNonZero = .NOT. obj%algoParam%dis_zero(4)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%rhs, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%sol, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)
!velocity
CALL RealVector_Set(obj=obj%force2, VALUE=zero)
scale = obj%algoParam%vel(1) / dt
isNonZero = .NOT. obj%algoParam%vel_zero(1)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%force2, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%u0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%vel(2)
isNonZero = .NOT. obj%algoParam%vel_zero(2)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%force2, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%v0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%vel(3) * dt
isNonZero = .NOT. obj%algoParam%vel_zero(3)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%force2, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%a0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%vel(4) / dt
isNonZero = .NOT. obj%algoParam%vel_zero(4)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%force2, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%sol, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)
! accerlation
CALL RealVector_Set(obj=obj%tmp1, VALUE=zero)
scale = obj%algoParam%acc(1) / dts
isNonZero = .NOT. obj%algoParam%acc_zero(1)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%tmp1, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%u0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%acc(2) / dt
isNonZero = .NOT. obj%algoParam%acc_zero(2)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%tmp1, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%v0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%acc(3)
isNonZero = .NOT. obj%algoParam%acc_zero(3)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%tmp1, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%a0, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

scale = obj%algoParam%acc(4) / dts
isNonZero = .NOT. obj%algoParam%acc_zero(4)
IF (isNonZero) &
  CALL RealVector_Add(obj1=obj%tmp1, dofobj1=obj%dof, idof1=1_I4B, &
                      obj2=obj%sol, dofobj2=obj%dof, idof2=1_I4B, &
                      scale=scale)

!update
CALL RealVector_Set(obj=obj%u0, VALUE=obj%rhs)
CALL RealVector_Set(obj=obj%v0, VALUE=obj%force2)
CALL RealVector_Set(obj=obj%a0, VALUE=obj%tmp1)

CALL RealVector_Set(obj=obj%rhs, VALUE=zero)
CALL RealVector_Set(obj=obj%force2, VALUE=zero)

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
             v0(MAX_ORDER_SPACE + 1), &
             a0(MAX_ORDER_SPACE + 1), xlim(2), ylim(2)

REAL(DFP), ALLOCATABLE :: DATA(:, :), ips(:, :)

INTEGER(I4B) :: ielSpace, con(MAX_ORDER_SPACE + 1), nns, nips, &
                totalNodes, ii, jj, n, inds(2)

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

SELECT CASE (obj%baseInterpolationForSpace)
CASE ("LAGR")
  totalNodes = obj%totalVertexDOFSpace + obj%totalEdgeDOFSpace
  ALLOCATE (ips(1, obj%maxSpaceOrder + 1))
CASE DEFAULT
  totalNodes = obj%totalVertexDOFSpace
END SELECT

ALLOCATE (DATA(totalNodes, 4))

n = 1

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

  CALL RealVector_GetValue_(obj=obj%a0, nodenum=con(1:nns), VALUE=a0, &
                            tsize=nns)

  SELECT CASE (obj%baseInterpolationForSpace)
  CASE ("LAGR")
    CALL InterpolationPoint_(order=obj%spaceOrder(ielSpace), &
                             elemType=elem%line, &
                             ipType=obj%ipTypeForSpace, &
                             xij=xij, layout="VEFC", &
                             ans=ips, nrow=inds(1), ncol=inds(2))

    DATA(n, 1) = ips(1, 1)
    DATA(n, 2) = u0(1)
    DATA(n, 3) = v0(1)
    DATA(n, 4) = a0(1)

    DO jj = 1, nns - 2
      DATA(n + jj, 1) = ips(1, jj + 2)
      DATA(n + jj, 2) = u0(jj + 2)
      DATA(n + jj, 3) = v0(jj + 2)
      DATA(n + jj, 4) = a0(jj + 2)
    END DO

    n = n + inds(2) - 1
    DATA(n, 1) = ips(1, 2)
    DATA(n, 2) = u0(2)
    DATA(n, 3) = v0(2)
    DATA(n, 4) = a0(2)

  CASE DEFAULT
    DATA(con(1), 1) = xij(1, 1)
    DATA(con(2), 1) = xij(1, 2)

    DATA(con(1), 2) = DOT_PRODUCT(u0(1:nns), obj%spaceShapeFuncBndy(1:nns, 1))
    DATA(con(2), 2) = DOT_PRODUCT(u0(1:nns), obj%spaceShapeFuncBndy(1:nns, 2))

    DATA(con(1), 3) = DOT_PRODUCT(v0(1:nns), obj%spaceShapeFuncBndy(1:nns, 1))
    DATA(con(2), 3) = DOT_PRODUCT(v0(1:nns), obj%spaceShapeFuncBndy(1:nns, 2))

    DATA(con(1), 4) = DOT_PRODUCT(a0(1:nns), obj%spaceShapeFuncBndy(1:nns, 1))
    DATA(con(2), 4) = DOT_PRODUCT(a0(1:nns), obj%spaceShapeFuncBndy(1:nns, 2))
  END SELECT

  xij(1, 1) = xij(1, 2)

END DO

! csv file

! disp
#ifdef DEBUG_VER
CALL Display("Writing data to file: "//filename_disp//".csv")
#endif
CALL obj%dispfile%Initiate(filename=filename_disp//".csv", unit=100, &
                           status="REPLACE", action="WRITE", &
                           comment="#", separator=",")
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

DO ii = 1, totalNodes
  aline = tostring(DATA(ii, 1))//", "//tostring(DATA(ii, 4))
  CALL obj%accfile%WRITE(aline)
END DO

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

CALL obj%plot%filename(filename_acc//'.plt')
CALL obj%plot%options('set terminal pngcairo; set output "'//filename_acc//'.png"')
xlim = obj%spaceDomain
ylim(1) = MINVAL(DATA(1:totalNodes, 4))
ylim(2) = MAXVAL(DATA(1:totalNodes, 4))
xlim(1) = xlim(1) - 0.1 * (xlim(2) - xlim(1))
xlim(2) = xlim(2) + 0.1 * (xlim(2) - xlim(1))
ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

CALL obj%plot%xlim(xlim)
CALL obj%plot%ylim(ylim)
CALL obj%plot%xlabel('x')
CALL obj%plot%ylabel('a')
CALL obj%plot%plot(x1=DATA(1:totalNodes, 1), y1=DATA(1:totalNodes, 4))
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

INTEGER(I4B) :: iTime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetInitialVelocity()
CALL obj%SetInitialDisplacement()

DO iTime = 1, obj%totalTimeSteps
  CALL Display(obj%currentTime, myname//" current time: ")
  CALL obj%AssembleTanmat()
  CALL obj%AssembleRHS()
  CALL obj%ApplyDirichletBC()
  CALL obj%Solve()
  CALL obj%Update()
  CALL obj%WriteData()
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
