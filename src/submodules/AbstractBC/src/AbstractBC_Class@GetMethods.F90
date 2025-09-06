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

SUBMODULE(AbstractBC_Class) GetMethods
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: CHAR_LF
USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                                 GetMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshID
ans = obj%boundary%GetMeshID(dim=dim)
END PROCEDURE obj_GetMeshID

!----------------------------------------------------------------------------
!                                                           GetMeshIDPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshIDPointer
CALL obj%boundary%GetMeshIDPointer(dim=dim, ans=ans, tsize=tsize)
END PROCEDURE obj_GetMeshIDPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFNo
ans = obj%idof
END PROCEDURE obj_GetDOFNo

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

CHARACTER(6) :: casename

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

casename = fedof%GetCaseName()

SELECT CASE (casename)

CASE ("H1LAGR")

  CALL obj%GetH1Lagrange(fedof=fedof, nodenum=nodenum, &
                         nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                         times=times)

CASE ("H1HIER", "H1HEIR")

  CALL obj%GetH1Hierarchical(fedof=fedof, nodenum=nodenum, &
                             nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                             times=times)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    "No case found for fedof casename="//casename)
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

CHARACTER(6) :: casename

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

casename = fedof%GetCaseName()

SELECT CASE (casename)

CASE ("H1LAGR")
  CALL obj%GetH1Lagrange(fedof=fedof, nodenum=nodenum, tsize=tsize)

CASE ("H1HIER", "H1HEIR")
  CALL obj%GetH1Hierarchical(fedof=fedof, nodenum=nodenum, tsize=tsize)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myname, &
                    "No case found for fedof casename="//casename)
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                           GetTotalNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodenum
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodenum()"
#endif

CHARACTER(6) :: casename

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

casename = fedof%GetCaseName()

SELECT CASE (casename)

CASE ("H1LAGR")
  ans = obj%GetTotalNodeNumH1Lagrange(fedof=fedof)

CASE ("H1HIER", "H1HEIR")
  ans = obj%GetTotalNodeNumH1Hierarchical(fedof=fedof)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myname, "No case found for fedof casename")
  RETURN
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetTotalNodenum

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isuseFunction
ans = obj%isUserFunction
END PROCEDURE obj_isuseFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
CALL obj%boundary%GetParam(isSelectionByBox=isSelectionByBox, &
                           isSelectionByMeshID=isSelectionByMeshID, &
                           isSelectionByElemNum=isSelectionByElemNum, &
                           isSelectionByNodeNum=isSelectionByNodeNum)

IF (PRESENT(idof)) idof = obj%idof
IF (PRESENT(isTangent)) isTangent = obj%isTangent
IF (PRESENT(isNormal)) isNormal = obj%isNormal
IF (PRESENT(useFunction)) useFunction = obj%isUserFunction
IF (PRESENT(isUserFunction)) isUserFunction = obj%isUserFunction
IF (PRESENT(nodalValueType)) nodalValueType = obj%nodalValueType
IF (PRESENT(isInitiated)) isInitiated = obj%isInit
IF (PRESENT(isUseExternal)) isUseExternal = obj%isUseExternal
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
