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

SUBMODULE(OneDimFEFactoryUtility) Methods
USE String_Class, ONLY: String
USE LagrangeOneDimFE_Class, ONLY: LagrangeOneDimFE_
USE HierarchicalOneDimFE_Class, ONLY: HierarchicalOneDimFE_
USE OrthogonalOneDimFE_Class, ONLY: OrthogonalOneDimFE_
USE StringUtility, ONLY: UpperCase
USE TomlUtility, ONLY: GetValue
USE OneDimBasisOpt_Class, ONLY: TypeOneDimBasisOpt
USE Display_Method, ONLY: ToString

CONTAINS

!----------------------------------------------------------------------------
!                                                    InternalOneDimFEFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE InternalOneDimFEFactory
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "InternalOneDimFEFactory()"
#endif

CHARACTER(LEN=4) :: baseInterpolation0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

baseInterpolation0 = UpperCase(baseInterpolation(1:4))

ans => NULL()

SELECT CASE (baseInterpolation0)
CASE ("LAGR")
  ALLOCATE (LagrangeOneDimFE_ :: ans)
CASE ("HIER", "HEIR")
  ALLOCATE (HierarchicalOneDimFE_ :: ans)
CASE ("ORTHO")
  ALLOCATE (OrthogonalOneDimFE_ :: ans)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                   "No case found for baseInterpolation="//baseInterpolation0)
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE InternalOneDimFEFactory

!----------------------------------------------------------------------------
!                                                            OneDimFEFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE OneDimFEFactory1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "OneDimFEFactory1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => InternalOneDimFEFactory(baseContinuity=baseContinuity, &
                               baseInterpolation=baseInterpolation)

CALL ans%Initiate(baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, &
                  order=order, feType=feType, ipType=ipType, &
                  basisType=basisType, alpha=alpha, &
                  beta=beta, lambda=lambda, &
                  quadratureType=quadratureType, &
                  quadratureOrder=quadratureOrder, &
                  quadratureNips=quadratureNips, &
                  quadratureAlpha=quadratureAlpha, &
                  quadratureBeta=quadratureBeta, &
                  quadratureLambda=quadratureLambda)

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
INTEGER(I4B) :: stat, origin

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseInterpolation...')
#endif

!Read baseInterpolation and baseContinuity from table
baseInterpolation0 = TypeOneDimBasisOpt%GetBaseInterpolation()
baseContinuity0 = TypeOneDimBasisOpt%GetBaseContinuity()

CALL GetValue( &
  table=table, key="baseInterpolation", VALUE=baseInterpolation, &
  default_value=baseInterpolation0, origin=origin, stat=stat, &
  isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseContinuity...')
#endif

CALL GetValue( &
  table=table, key="baseContinuity", VALUE=baseContinuity, &
  default_value=baseContinuity0, origin=origin, stat=stat, isFound=isFound)

ans => InternalOneDimFEFactory(baseContinuity=baseContinuity%chars(), &
                               baseInterpolation=baseInterpolation%chars())

CALL ans%ImportFromToml(table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE OneDimFEFactory2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
