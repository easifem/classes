! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(FEFactoryUtility) Methods
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BasisOpt_Class, ONLY: TypeBasisOpt
USE Display_Method, ONLY: ToString
USE HexahedronH1DGHierarchicalFE_Class, ONLY: HexahedronH1DGHierarchicalFE_
USE HexahedronH1DGLagrangeFE_Class, ONLY: HexahedronH1DGLagrangeFE_
USE HexahedronH1HierarchicalFE_Class, ONLY: HexahedronH1HierarchicalFE_
USE HexahedronH1LagrangeFE_Class, ONLY: HexahedronH1LagrangeFE_
USE InputUtility, ONLY: Input
USE LineH1DGHierarchicalFE_Class, ONLY: LineH1DGHierarchicalFE_
USE LineH1DGLagrangeFE_Class, ONLY: LineH1DGLagrangeFE_
USE LineH1HierarchicalFE_Class, ONLY: LineH1HierarchicalFE_
USE LineH1LagrangeFE_Class, ONLY: LineH1LagrangeFE_
USE OneDimBasisOpt_Class, ONLY: TypeOneDimBasisOpt
USE OneDimBasisOpt_Class, ONLY: TypeOneDimBasisOpt
USE OneDimHierarchicalFE_Class, ONLY: OneDimHierarchicalFE_
USE OneDimHierarchicalFE_Class, ONLY: OneDimHierarchicalFE_
USE OneDimLagrangeFE_Class, ONLY: OneDimLagrangeFE_
USE OneDimLagrangeFE_Class, ONLY: OneDimLagrangeFE_
USE OneDimOrthogonalFE_Class, ONLY: OneDimOrthogonalFE_
USE OneDimOrthogonalFE_Class, ONLY: OneDimOrthogonalFE_
USE PrismH1DGHierarchicalFE_Class, ONLY: PrismH1DGHierarchicalFE_
USE PrismH1DGLagrangeFE_Class, ONLY: PrismH1DGLagrangeFE_
USE PrismH1HierarchicalFE_Class, ONLY: PrismH1HierarchicalFE_
USE PrismH1LagrangeFE_Class, ONLY: PrismH1LagrangeFE_
USE PyramidH1DGHierarchicalFE_Class, ONLY: PyramidH1DGHierarchicalFE_
USE PyramidH1DGLagrangeFE_Class, ONLY: PyramidH1DGLagrangeFE_
USE PyramidH1HierarchicalFE_Class, ONLY: PyramidH1HierarchicalFE_
USE PyramidH1LagrangeFE_Class, ONLY: PyramidH1LagrangeFE_
USE QuadrangleH1DGHierarchicalFE_Class, ONLY: QuadrangleH1DGHierarchicalFE_
USE QuadrangleH1DGLagrangeFE_Class, ONLY: QuadrangleH1DGLagrangeFE_
USE QuadrangleH1HierarchicalFE_Class, ONLY: QuadrangleH1HierarchicalFE_
USE QuadrangleH1LagrangeFE_Class, ONLY: QuadrangleH1LagrangeFE_
USE String_Class, ONLY: String
USE StringUtility, ONLY: UpperCase
USE TetrahedronH1DGHierarchicalFE_Class, ONLY: TetrahedronH1DGHierarchicalFE_
USE TetrahedronH1DGLagrangeFE_Class, ONLY: TetrahedronH1DGLagrangeFE_
USE TetrahedronH1HierarchicalFE_Class, ONLY: TetrahedronH1HierarchicalFE_
USE TetrahedronH1LagrangeFE_Class, ONLY: TetrahedronH1LagrangeFE_
USE TomlUtility, ONLY: GetValue
USE TriangleH1DGHierarchicalFE_Class, ONLY: TriangleH1DGHierarchicalFE_
USE TriangleH1DGLagrangeFE_Class, ONLY: TriangleH1DGLagrangeFE_
USE TriangleH1HierarchicalFE_Class, ONLY: TriangleH1HierarchicalFE_
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "FEFactoryUtility@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                FEFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE FEFactory1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "FEFactory1()"
#endif
LOGICAL(LGT) :: isBrokenSpace0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isBrokenSpace0 = Input(default=math%no, option=isBrokenSpace)

SELECT CASE (elemType)
CASE (TypeElemNameOpt%line)
  ans => FEFactoryLine(baseContinuity=baseContinuity, &
                       baseInterpolation=baseInterpolation, &
                       isBrokenSpace=isBrokenSpace0)

CASE (TypeElemNameOpt%triangle)
  ans => FEFactoryTriangle(baseContinuity=baseContinuity, &
                           baseInterpolation=baseInterpolation, &
                           isBrokenSpace=isBrokenSpace0)

CASE (TypeElemNameOpt%quadrangle)
  ans => FEFactoryQuadrangle(baseContinuity=baseContinuity, &
                             baseInterpolation=baseInterpolation, &
                             isBrokenSpace=isBrokenSpace0)

CASE (TypeElemNameOpt%tetrahedron)
  ans => FEFactoryTetrahedron(baseContinuity=baseContinuity, &
                              baseInterpolation=baseInterpolation, &
                              isBrokenSpace=isBrokenSpace0)

CASE (TypeElemNameOpt%hexahedron)
  ans => FEFactoryHexahedron(baseContinuity=baseContinuity, &
                             baseInterpolation=baseInterpolation, &
                             isBrokenSpace=isBrokenSpace0)

CASE (TypeElemNameOpt%prism)
  ans => FEFactoryPrism(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        isBrokenSpace=isBrokenSpace0)

CASE (TypeElemNameOpt%pyramid)
  ans => FEFactoryPyramid(baseContinuity=baseContinuity, &
                          baseInterpolation=baseInterpolation, &
                          isBrokenSpace=isBrokenSpace0)
CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for elemType")
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE FEFactory1

!----------------------------------------------------------------------------
!                                                            FEFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE FEFactory2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "FEFactory2()"
#endif

TYPE(String) :: baseInterpolation, baseContinuity
CHARACTER(4) :: baseInterpolation0
CHARACTER(2) :: baseContinuity0
LOGICAL(LGT) :: isFound, isBrokenSpace
INTEGER(I4B) :: STAT, origin

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Read baseInterpolation and baseContinuity from table
baseInterpolation0 = TypeBasisOpt%GetBaseInterpolation()
baseContinuity0 = TypeBasisOpt%GetBaseContinuity()

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading baseInterpolation...')
#endif

CALL GetValue(table=table, key="baseInterpolation", &
              VALUE=baseInterpolation, &
              default_value=baseInterpolation0, &
              origin=origin, STAT=STAT, isFound=isFound)

#ifdef DEBUG_VER
IF (.NOT. isFound) THEN
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'baseInterpolation not found in toml table, ' &
                    //"using default value="//baseInterpolation0)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading baseContinuity...')
#endif

CALL GetValue(table=table, key="baseContinuity", &
              VALUE=baseContinuity, &
              default_value=baseContinuity0, &
              origin=origin, STAT=STAT, isFound=isFound)

#ifdef DEBUG_VER
IF (.NOT. isFound) THEN
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'baseContinuity not found in toml, ' &
                    //"using default value="//baseContinuity0)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading isBrokenSpace...')
#endif

CALL GetValue(table=table, key="brokenSpace", &
              VALUE=isBrokenSpace, &
              default_value=math%no, &
              origin=origin, STAT=STAT, isFound=isFound)

#ifdef DEBUG_VER
IF (.NOT. isFound) THEN
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'brokenSpace not found in toml, ' &
                    //"using brokenSpace=false")
END IF
#endif

ans => FEFactory(elemType=elemType, &
                 baseContinuity=baseContinuity%Chars(), &
                 baseInterpolation=baseInterpolation%Chars(), &
                 isBrokenSpace=isBrokenSpace)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE FEFactory2

!----------------------------------------------------------------------------
!                                                              FEFactoryLine
!----------------------------------------------------------------------------

FUNCTION FEFactoryLine(baseContinuity, baseInterpolation, isBrokenSpace) &
  RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  CLASS(AbstractFE_), POINTER :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactoryLine()"
#endif

  CHARACTER(6) :: acase

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  acase = baseContinuity(1:2)//baseInterpolation(1:4)
  acase = UpperCase(acase)

  SELECT CASE (acase)
  CASE ("H1LAGR")
    IF (isBrokenSpace) THEN
      ALLOCATE (LineH1DGLagrangeFE_ :: ans)
    ELSE
      ALLOCATE (LineH1LagrangeFE_ :: ans)
    END IF

  CASE ("H1HIER", "H1HEIR")
    IF (isBrokenSpace) THEN
      ALLOCATE (LineH1DGHierarchicalFE_ :: ans)
    ELSE
      ALLOCATE (LineH1HierarchicalFE_ :: ans)
    END IF

  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "No case found for acase="//acase)
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION FEFactoryLine

!----------------------------------------------------------------------------
!                                                          FEFactoryTriangle
!----------------------------------------------------------------------------

FUNCTION FEFactoryTriangle(baseContinuity, baseInterpolation, isBrokenSpace) &
  RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  CLASS(AbstractFE_), POINTER :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactoryTriangle()"
#endif
  CHARACTER(6) :: acase

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  acase = baseContinuity(1:2)//baseInterpolation(1:4)
  acase = UpperCase(acase)

  SELECT CASE (acase)
  CASE ("H1LAGR")
    IF (isBrokenSpace) THEN
      ALLOCATE (TriangleH1DGLagrangeFE_ :: ans)
    ELSE
      ALLOCATE (TriangleH1LagrangeFE_ :: ans)
    END IF

  CASE ("H1HIER", "H1HEIR")
    IF (isBrokenSpace) THEN
      ALLOCATE (TriangleH1DGHierarchicalFE_ :: ans)
    ELSE
      ALLOCATE (TriangleH1HierarchicalFE_ :: ans)
    END IF

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(math%no, myName, &
                      "No case found for acase="//acase)
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION FEFactoryTriangle

!----------------------------------------------------------------------------
!                                                         FEFactoryQuadrangle
!----------------------------------------------------------------------------

FUNCTION FEFactoryQuadrangle(baseContinuity, baseInterpolation, &
                             isBrokenSpace) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  CLASS(AbstractFE_), POINTER :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactoryQuadrangle()"
#endif

  CHARACTER(6) :: acase

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  acase = baseContinuity(1:2)//baseInterpolation(1:4)
  acase = UpperCase(acase)

  SELECT CASE (acase)
  CASE ("H1LAGR")
    IF (isBrokenSpace) THEN
      ALLOCATE (QuadrangleH1DGLagrangeFE_ :: ans)
    ELSE
      ALLOCATE (QuadrangleH1LagrangeFE_ :: ans)
    END IF

  CASE ("H1HIER", "H1HEIR")
    IF (isBrokenSpace) THEN
      ALLOCATE (QuadrangleH1DGHierarchicalFE_ :: ans)
    ELSE
      ALLOCATE (QuadrangleH1HierarchicalFE_ :: ans)
    END IF

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(math%no, myName, &
                      "No case found for acase="//acase)
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION FEFactoryQuadrangle

!----------------------------------------------------------------------------
!                                                        FEFactoryTetrahedron
!----------------------------------------------------------------------------

FUNCTION FEFactoryTetrahedron(baseContinuity, baseInterpolation, &
                              isBrokenSpace) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  CLASS(AbstractFE_), POINTER :: ans

  ! define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactoryTetrahedron()"
#endif
  CHARACTER(6) :: acase

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  acase = baseContinuity(1:2)//baseInterpolation(1:4)
  acase = UpperCase(acase)

  SELECT CASE (acase)
  CASE ("H1LAGR")
    IF (isBrokenSpace) THEN
      ALLOCATE (TetrahedronH1DGLagrangeFE_ :: ans)
    ELSE
      ALLOCATE (TetrahedronH1LagrangeFE_ :: ans)
    END IF

  CASE ("H1HIER", "H1HEIR")
    IF (isBrokenSpace) THEN
      ALLOCATE (TetrahedronH1DGHierarchicalFE_ :: ans)
    ELSE
      ALLOCATE (TetrahedronH1HierarchicalFE_ :: ans)
    END IF

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(math%no, myName, &
                      "No case found for acase="//acase)
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION FEFactoryTetrahedron

!----------------------------------------------------------------------------
!                                                         FEFactoryHexahedron
!----------------------------------------------------------------------------

FUNCTION FEFactoryHexahedron(baseContinuity, baseInterpolation, &
                             isBrokenSpace) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  CLASS(AbstractFE_), POINTER :: ans

  ! define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactoryHexahedron()"
#endif
  CHARACTER(6) :: acase

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  acase = baseContinuity(1:2)//baseInterpolation(1:4)
  acase = UpperCase(acase)

  SELECT CASE (acase)
  CASE ("H1LAGR")
    IF (isBrokenSpace) THEN
      ALLOCATE (HexahedronH1DGLagrangeFE_ :: ans)
    ELSE
      ALLOCATE (HexahedronH1LagrangeFE_ :: ans)
    END IF

  CASE ("H1HIER", "H1HEIR")
    IF (isBrokenSpace) THEN
      ALLOCATE (HexahedronH1DGHierarchicalFE_ :: ans)
    ELSE
      ALLOCATE (HexahedronH1HierarchicalFE_ :: ans)
    END IF

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(math%no, myName, &
                      "No case found for acase="//acase)
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION FEFactoryHexahedron

!----------------------------------------------------------------------------
!                                                         FEFactoryPrism
!----------------------------------------------------------------------------

FUNCTION FEFactoryPrism(baseContinuity, baseInterpolation, isBrokenSpace) &
  RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  CLASS(AbstractFE_), POINTER :: ans

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactoryPrism()"
#endif
  CHARACTER(6) :: acase

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  acase = baseContinuity(1:2)//baseInterpolation(1:4)
  acase = UpperCase(acase)

  SELECT CASE (acase)
  CASE ("H1LAGR")
    IF (isBrokenSpace) THEN
      ALLOCATE (PrismH1DGLagrangeFE_ :: ans)
    ELSE
      ALLOCATE (PrismH1LagrangeFE_ :: ans)
    END IF

  CASE ("H1HIER", "H1HEIR")
    IF (isBrokenSpace) THEN
      ALLOCATE (PrismH1DGHierarchicalFE_ :: ans)
    ELSE
      ALLOCATE (PrismH1HierarchicalFE_ :: ans)
    END IF

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(math%no, myName, &
                      "No case found for acase="//acase)
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION FEFactoryPrism

!----------------------------------------------------------------------------
!                                                         FEFactoryPyramid
!----------------------------------------------------------------------------

FUNCTION FEFactoryPyramid(baseContinuity, baseInterpolation, isBrokenSpace) &
  RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  LOGICAL(LGT), INTENT(IN) :: isBrokenSpace
  CLASS(AbstractFE_), POINTER :: ans

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactoryPyramid()"
#endif
  CHARACTER(6) :: acase

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  acase = baseContinuity(1:2)//baseInterpolation(1:4)
  acase = UpperCase(acase)

  SELECT CASE (acase)
  CASE ("H1LAGR")
    IF (isBrokenSpace) THEN
      ALLOCATE (PyramidH1DGLagrangeFE_ :: ans)
    ELSE
      ALLOCATE (PyramidH1LagrangeFE_ :: ans)
    END IF

  CASE ("H1HIER", "H1HEIR")
    IF (isBrokenSpace) THEN
      ALLOCATE (PyramidH1DGHierarchicalFE_ :: ans)
    ELSE
      ALLOCATE (PyramidH1HierarchicalFE_ :: ans)
    END IF

  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "No case found for acase="//acase)
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION FEFactoryPyramid

!----------------------------------------------------------------------------
!                                                              FEFactoryLine
!----------------------------------------------------------------------------

MODULE PROCEDURE OneDimFEFactory1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "FEFactoryLine()"
#endif
CHARACTER(4) :: acase

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

acase = UpperCase(baseInterpolation(1:4))

SELECT CASE (acase)
CASE ("LAGR")
  ALLOCATE (OneDimLagrangeFE_ :: ans)

CASE ("HIER", "HEIR")
  ALLOCATE (OneDimHierarchicalFE_ :: ans)

CASE ("ORTH")
  ALLOCATE (OneDimOrthogonalFE_ :: ans)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for acase="//acase)
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE OneDimFEFactory1

!----------------------------------------------------------------------------
!                                                            OneDimFEFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE OneDimFEFactory2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "OneDimFEFactory2()"
#endif

TYPE(String) :: baseInterpolation, baseContinuity
CHARACTER(4) :: baseInterpolation0, baseContinuity0
LOGICAL(LGT) :: isFound
INTEGER(I4B) :: STAT, origin

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading baseInterpolation...')
#endif

!Read baseInterpolation and baseContinuity from table
baseInterpolation0 = TypeOneDimBasisOpt%GetBaseInterpolation()
baseContinuity0 = TypeOneDimBasisOpt%GetBaseContinuity()

CALL GetValue( &
  table=table, key="baseInterpolation", VALUE=baseInterpolation, &
  default_value=baseInterpolation0, origin=origin, STAT=STAT, &
  isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading baseContinuity...')
#endif

CALL GetValue( &
  table=table, key="baseContinuity", VALUE=baseContinuity, &
  default_value=baseContinuity0, origin=origin, STAT=STAT, isFound=isFound)

ans => OneDimFEFactory1(baseContinuity=baseContinuity%chars(), &
                        baseInterpolation=baseInterpolation%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE OneDimFEFactory2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
