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
USE Display_Method, ONLY: ToString
USE String_Class, ONLY: String
USE StringUtility, ONLY: UpperCase
USE BasisOpt_Class, ONLY: TypeBasisOpt
USE TomlUtility, ONLY: GetValue
USE BaseType, ONLY: TypeElemNameOpt

USE LineH1LagrangeFE_Class, ONLY: LineH1LagrangeFE_
USE LineH1HierarchicalFE_Class, ONLY: LineH1HierarchicalFE_
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
USE TriangleH1HierarchicalFE_Class, ONLY: TriangleH1HierarchicalFE_
USE QuadrangleH1LagrangeFE_Class, ONLY: QuadrangleH1LagrangeFE_
USE QuadrangleH1HierarchicalFE_Class, ONLY: QuadrangleH1HierarchicalFE_
USE TetrahedronH1LagrangeFE_Class, ONLY: TetrahedronH1LagrangeFE_
USE TetrahedronH1HierarchicalFE_Class, ONLY: TetrahedronH1HierarchicalFE_
USE HexahedronH1LagrangeFE_Class, ONLY: HexahedronH1LagrangeFE_
USE HexahedronH1HierarchicalFE_Class, ONLY: HexahedronH1HierarchicalFE_
USE PrismH1LagrangeFE_Class, ONLY: PrismH1LagrangeFE_
USE PrismH1HierarchicalFE_Class, ONLY: PrismH1HierarchicalFE_
USE PyramidH1LagrangeFE_Class, ONLY: PyramidH1LagrangeFE_
USE PyramidH1HierarchicalFE_Class, ONLY: PyramidH1HierarchicalFE_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                           FEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Internal routine for creating pointer form baseContinuity
! and baseInterpolation

MODULE PROCEDURE FEFactory1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "FEFactory1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (elemType)
CASE (TypeElemNameOpt%line)
  ans => FEFactoryLine(baseContinuity=baseContinuity, &
                       baseInterpolation=baseInterpolation)

CASE (TypeElemNameOpt%triangle)
  ans => FEFactoryTriangle(baseContinuity=baseContinuity, &
                           baseInterpolation=baseInterpolation)

CASE (TypeElemNameOpt%quadrangle)
  ans => FEFactoryQuadrangle(baseContinuity=baseContinuity, &
                             baseInterpolation=baseInterpolation)

CASE (TypeElemNameOpt%tetrahedron)
  ans => FEFactoryTetrahedron(baseContinuity=baseContinuity, &
                              baseInterpolation=baseInterpolation)

CASE (TypeElemNameOpt%hexahedron)
  ans => FEFactoryHexahedron(baseContinuity=baseContinuity, &
                             baseInterpolation=baseInterpolation)

CASE (TypeElemNameOpt%prism)
  ans => FEFactoryPrism(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation)

CASE (TypeElemNameOpt%pyramid)
  ans => FEFactoryPyramid(baseContinuity=baseContinuity, &
                          baseInterpolation=baseInterpolation)
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
LOGICAL(LGT) :: isFound
INTEGER(I4B) :: stat, origin

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Read baseInterpolation and baseContinuity from table
baseInterpolation0 = TypeBasisOpt%GetBaseInterpolation()
baseContinuity0 = TypeBasisOpt%GetBaseContinuity()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseInterpolation...')
#endif

CALL GetValue(table=table, key="baseInterpolation", &
              VALUE=baseInterpolation, &
              default_value=baseInterpolation0, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
IF (.NOT. isFound) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                   'baseInterpolation not found in toml, using default value')
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseContinuity...')
#endif

CALL GetValue(table=table, key="baseContinuity", &
              VALUE=baseContinuity, &
              default_value=baseContinuity0, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
IF (.NOT. isFound) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                      'baseContinuity not found in toml, using default value')
END IF
#endif

ans => FEFactory(elemType=elemType, &
                 baseContinuity=baseContinuity%chars(), &
                 baseInterpolation=baseInterpolation%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE FEFactory2

!----------------------------------------------------------------------------
!                                                              FEFactoryLine
!----------------------------------------------------------------------------

FUNCTION FEFactoryLine(baseContinuity, baseInterpolation) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
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
    ALLOCATE (LineH1LagrangeFE_ :: ans)

  CASE ("H1HIER", "H1HEIR")
    ALLOCATE (LineH1HierarchicalFE_ :: ans)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
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

FUNCTION FEFactoryTriangle(baseContinuity, baseInterpolation) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
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
    ALLOCATE (TriangleH1LagrangeFE_ :: ans)

  CASE ("H1HIER", "H1HEIR")
    ALLOCATE (TriangleH1HierarchicalFE_ :: ans)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
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

FUNCTION FEFactoryQuadrangle(baseContinuity, baseInterpolation) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
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
    ALLOCATE (QuadrangleH1LagrangeFE_ :: ans)

  CASE ("H1HIER", "H1HEIR")
    ALLOCATE (QuadrangleH1HierarchicalFE_ :: ans)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
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

FUNCTION FEFactoryTetrahedron(baseContinuity, baseInterpolation) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  CLASS(AbstractFE_), POINTER :: ans

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
    ALLOCATE (TetrahedronH1LagrangeFE_ :: ans)

  CASE ("H1HIER", "H1HEIR")
    ALLOCATE (TetrahedronH1HierarchicalFE_ :: ans)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
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

FUNCTION FEFactoryHexahedron(baseContinuity, baseInterpolation) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  CLASS(AbstractFE_), POINTER :: ans

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
    ALLOCATE (HexahedronH1LagrangeFE_ :: ans)

  CASE ("H1HIER", "H1HEIR")
    ALLOCATE (HexahedronH1HierarchicalFE_ :: ans)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
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

FUNCTION FEFactoryPrism(baseContinuity, baseInterpolation) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  CLASS(AbstractFE_), POINTER :: ans

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
    ALLOCATE (PrismH1LagrangeFE_ :: ans)

  CASE ("H1HIER", "H1HEIR")
    ALLOCATE (PrismH1HierarchicalFE_ :: ans)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
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

FUNCTION FEFactoryPyramid(baseContinuity, baseInterpolation) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  CLASS(AbstractFE_), POINTER :: ans

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
    ALLOCATE (PyramidH1LagrangeFE_ :: ans)

  CASE ("H1HIER", "H1HEIR")
    ALLOCATE (PyramidH1HierarchicalFE_ :: ans)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
                      "No case found for acase="//acase)
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION FEFactoryPyramid

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
