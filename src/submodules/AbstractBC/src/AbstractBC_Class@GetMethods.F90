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

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetMeshID
ans = obj%boundary%GetMeshID(dim=dim)
END PROCEDURE bc_GetMeshID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetDOFNo
ans = obj%idof
END PROCEDURE bc_GetDOFNo

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Get
CHARACTER(*), PARAMETER :: myName = "bc_Get()"
INTEGER(I4B) :: ii, tsize, tnodes, tTimes
LOGICAL(LGT) :: isNodalValuePresent, isNOTOK, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL AssertError1(obj%isInitiated, myName, &
                  'AbstractBC_ object is not initiated, initiate it first.')

isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')

tnodes = obj%boundary%GetTotalNodeNum(dom=obj%dom)
CALL Reallocate(nodenum, tnodes)
CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=tnodes)
! tnodes = SIZE(nodeNum)

tTimes = 1
IF (PRESENT(times)) tTimes = SIZE(times)

isNodalValuePresent = PRESENT(nodalValue)

IF (isNodalValuePresent .AND. obj%isUserFunction) THEN
  CALL obj%GetFromUserFunction(nodeNum=nodeNum, nodalValue=nodalValue, &
                               times=times)
  RETURN
END IF

isNOTOK = isNodalValuePresent .AND. (.NOT. ALLOCATED(obj%nodalValue))

IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: AbstractBC_::obj%nodalValue is not allocated!')
  RETURN
END IF

IF (.NOT. isNodalValuePresent) RETURN

! get nodal values
SELECT CASE (obj%nodalValueType)

! Constant
CASE (CONSTANT)
  CALL Reallocate(nodalValue, tnodes, tTimes)
  nodalValue = obj%nodalValue(1, 1)

! Space
CASE (SPACE)
  isNOTOK = SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)
  IF (isNOTOK) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
    RETURN
  END IF

  nodalValue = obj%nodalValue

! Time
CASE (TIME)
  tsize = SIZE(obj%nodalValue, 1)
  IF (PRESENT(times)) THEN
    IF (tsize .NE. SIZE(times)) THEN
      CALL e%RaiseError(modName//'::'//myName//" - "// &
           '[INTERNAL ERROR] :: SIZE( obj%nodalValue, 2 ) .NE. SIZE( times )')
    END IF
  END IF

  CALL Reallocate(nodalValue, SIZE(nodeNum), tsize)

  DO ii = 1, tsize
    nodalValue(:, ii) = obj%nodalValue(ii, 1)
  END DO

! SpaceTime
CASE (SpaceTime)
  IF (SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
  END IF

  IF (PRESENT(times)) THEN
    IF (SIZE(obj%nodalValue, 2) .NE. SIZE(times)) THEN
      CALL e%RaiseError(modName//'::'//myName//" - "// &
           '[INTERNAL ERROR] :: SIZE( obj%nodalValue, 2 ) .NE. SIZE( times )')
    END IF
  END IF

  nodalValue = obj%nodalValue
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE bc_Get

!----------------------------------------------------------------------------
!                                                           bc_GetFEVar
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetFEVar
CHARACTER(*), PARAMETER :: myName = "bc_GetFEVar()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development.')

! If useFunction is true  then
!    if constant
!    if time
!    if space
!    if space-time

! If useFunction is not true then
END PROCEDURE bc_GetFEVar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_isuseFunction
ans = obj%isUserFunction
END PROCEDURE bc_isuseFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetQuery
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
IF (PRESENT(isInitiated)) isInitiated = obj%isInitiated
IF (PRESENT(useExternal)) useExternal = obj%useExternal
END PROCEDURE bc_GetQuery

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetPrefix
CHARACTER(*), PARAMETER :: myName = "bc_GetPrefix()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE bc_GetPrefix

!----------------------------------------------------------------------------
!                                                       GetFromUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetFromUserFunction
CHARACTER(*), PARAMETER :: myName = "bc_GetFromUserFunction()"
INTEGER(I4B) :: ii, kk, retType, tnodes, nsd, tTimes, argType, &
                tsize
REAL(DFP) :: xij(4, 1), ans
LOGICAL(LGT) :: problem

! get pointer to nodecoord

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

problem = .NOT. ASSOCIATED(obj%func)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[INTERNAL ERROR] :: When nodalValueType is "// &
                    CHAR_LF//"Space and useFunction is specified, "// &
                    CHAR_LF//"then SpaceFunction is needed, "// &
                    CHAR_LF//"but it is not associated")
  RETURN
END IF

retType = obj%func%GetReturnType()
problem = retType .NE. Scalar
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[INTERNAL ERROR] :: Return type of user function should be '// &
                    'scalar.')
  RETURN
END IF

problem = (obj%nodalValueType .EQ. Time) .AND. (.NOT. PRESENT(times))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[INTERNAL ERROR] :: When `nodalValueType` is Time "// &
                    " and `IsUserFunction` is TRUE, "// &
                    " then `times` is needed in the passing argument,"// &
                    " but it is not present")
  RETURN
END IF

problem = (obj%nodalValueType .EQ. SpaceTime) .AND. (.NOT. PRESENT(times))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 "[INTERNAL ERROR] :: When `nodalValueType` is SpaceTime "// &
                    " and `IsUserFunction` is TRUE, "// &
                    " then `times` is needed in the passing argument,"// &
                    " but it is not present")
  RETURN
END IF

argType = obj%func%GetArgType()
problem = argType .NE. obj%nodalValueType
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: argType='//tostring(argType)// &
                    ' in user function is not same '// &
                    'as nodalValueType '//tostring(obj%nodalValueType)// &
                    ' in AbstractBC_')
  RETURN
END IF

tnodes = SIZE(nodeNum)
nsd = obj%dom%GetNSD()

SELECT CASE (obj%nodalValueType)

! Constant
CASE (Constant)
  CALL Reallocate(nodalValue, tnodes, 1)

  CALL obj%dom%GetNodeCoord(nodeCoord=xij(:, 1), tsize=tsize, &
                            globalNode=nodeNum(1), islocal=.FALSE.)

  CALL obj%func%Get(val=ans)

  nodalValue(:, 1) = ans

! Space
CASE (Space)
  CALL Reallocate(nodalValue, tnodes, 1)

  DO ii = 1, tnodes
    CALL obj%dom%GetNodeCoord(nodeCoord=xij(:, 1), tsize=tsize, &
                              globalNode=nodeNum(ii), islocal=.FALSE.)

    CALL obj%func%Get(val=ans, args=xij(1:3, 1))

    nodalValue(ii, 1) = ans
  END DO

! Time
CASE (Time)

  tTimes = SIZE(times)

  CALL Reallocate(nodalValue, tnodes, tTimes)

  DO ii = 1, tTimes
    CALL obj%func%Get(val=ans, args=times(ii:ii))

    nodalValue(:, ii) = ans
  END DO

! SpaceTime
CASE (SpaceTime)
  tTimes = SIZE(times)

  CALL Reallocate(nodalValue, tnodes, tTimes)

  DO kk = 1, tTimes
    xij(nsd + 1, 1) = times(kk)

    DO ii = 1, tnodes
      CALL obj%dom%GetNodeCoord(nodeCoord=xij(:, 1), tsize=tsize, &
                                globalNode=nodeNum(ii), islocal=.FALSE.)

      CALL obj%func%Get(val=ans, args=xij(1:4, 1))

      nodalValue(ii, kk) = ans
    END DO
  END DO

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE bc_GetFromUserFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
