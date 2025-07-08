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

SUBMODULE(TimeFEDOF_Class) ConstructorMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize

USE Display_Method, ONLY: ToString, Display

USE ReallocateUtility, ONLY: Reallocate

USE InputUtility, ONLY: Input

USE Display_Method, ONLY: Display, ToString

USE IntVector_Method, ONLY: IntegerCopy => Copy

USE StringUtility, ONLY: UpperCase

USE ReallocateUtility, ONLY: Reallocate

USE FPL_Method, ONLY: Set, GetValue, CheckEssentialParam

USE BaseType, ONLY: TypeInterpolationOpt, &
                    TypePolynomialOpt

USE LagrangeOneDimFE_Class, ONLY: LagrangeOneDimFEPointer

USE HierarchicalOneDimFE_Class, ONLY: HierarchicalOneDimFEPointer

USE OrthogonalOneDimFE_Class, ONLY: OrthogonalOneDimFEPointer

USE AbstractOneDimFE_Class, ONLY: SetAbstractOneDimFEParam

USE OneDimFEFactory_Method, ONLY: OneDimFEFactory

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CALL CheckEssentialParam(obj=param, keys=essentialParam, prefix=myprefix, &
                         myName=myName, modName=modName)
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                        SetTimeFEDOFParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTimeFEDOFParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetTimeFEDOFParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! SetTimeOptParam
CALL SetAbstractOneDimFEParam(param=param, prefix=myprefix, &
                              baseContinuity=baseContinuity, &
                              baseInterpolation=baseInterpolation, &
                              order=order, feType=feType, &
                              ipType=ipType, basisType=basisType, &
                              alpha=alpha, beta=beta, lambda=lambda, &
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

END PROCEDURE SetTimeFEDOFParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
INTEGER(I4B) :: tcells, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%opt => timeOpt

obj%fe => OneDimFEFactory(baseContinuity=baseContinuity, &
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

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%fe => OneDimFEFactory(param=param)
obj%isInit = .TRUE.
obj%opt => timeOpt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%opt => NULL()

IF (ASSOCIATED(obj%fe)) THEN
  CALL obj%fe%DEALLOCATE()
END IF
obj%fe => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = obj2%isInit
obj%opt => obj2%opt
obj%fe => obj2%fe

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                               IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInitiated: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

isok = ASSOCIATED(obj%opt)
CALL Display(isok, "opt ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%opt%Display(msg="opt: ", unitno=unitno)
END IF

isok = ASSOCIATED(obj%fe)
CALL Display(isok, "fe ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%fe%Display(msg="fe: ", unitno=unitno)
END IF

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%opt => timeOpt
obj%isInit = .TRUE.
obj%fe => OneDimFEFactory(table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
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

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")

CALL obj%ImportFromToml(table=node, timeOpt=timeOpt)

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
!                                                               GetCaseName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCaseName
ans = obj%fe%GetCaseName()
END PROCEDURE obj_GetCaseName

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetCellOrder()
ans = ans + 1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTotalDOF

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                           GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeOptPointer
ans => obj%opt
END PROCEDURE obj_GetTimeOptPointer

!----------------------------------------------------------------------------
!                                                       GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseInterpolation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseInterpolation()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myName, 'obj%fe is not associated')
#endif

ans = obj%fe%GetBaseInterpolation()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                               GetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCellOrder()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myName, 'obj%fe is not associated')
#endif

ans = obj%fe%GetOrder()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetCellOrder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetQuadraturePoints1()'
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myName, 'obj%fe is not associated')
#endif

CALL obj%fe%GetQuadraturePoints(quad=quad, order=order, nips=nips, &
         quadratureType=quadratureType, alpha=alpha, beta=beta, lambda=lambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetLocalElemShapeData()'
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myName, 'obj%fe is not associated')
#endif

CALL obj%fe%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'obj_GetGlobalElemShapeData()'
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myname, 'obj%fe is not associated')
#endif

CALL obj%fe%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, geoElemsd=geoElemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
