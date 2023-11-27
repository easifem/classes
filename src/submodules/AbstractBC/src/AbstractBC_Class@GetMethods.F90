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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetMeshID
ans = obj%boundary%getMeshID(dim=dim)
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
INTEGER(I4B) :: ii, tsize, tNodes, tTimes
LOGICAL(LGT) :: isNodalValuePresent, isNOTOK

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Get()')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: AbstractBC_ object is not initiated'//  &
  & ', initiate it first.')
  RETURN
END IF

! get node numbers
IF (.NOT. ASSOCIATED(obj%dom)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[INTERNAL ERROR] :: AbstractBC_::obj%dom is not associated!')
  RETURN
END IF

nodeNum = obj%boundary%GetNodeNum(domain=obj%dom)
tNodes = SIZE(nodeNum)

tTimes = 1
IF (PRESENT(times)) tTimes = SIZE(times)

isNodalValuePresent = PRESENT(nodalValue)

IF (isNodalValuePresent .AND. obj%useFunction) THEN
  CALL obj%GetFromFunction(nodeNum=nodeNum, nodalValue=nodalValue,  &
    & times=times)
  RETURN
END IF

IF (isNodalValuePresent .AND. obj%isUserFunction) THEN
  CALL obj%GetFromUserFunction(nodeNum=nodeNum, nodalValue=nodalValue,  &
    & times=times)
  RETURN
END IF

isNOTOK = isNodalValuePresent .AND. (.NOT. ALLOCATED(obj%nodalValue))

IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: AbstractBC_::obj%nodalValue is not allocated!')
  RETURN
END IF

IF (.NOT. isNodalValuePresent) RETURN

! get nodal values
SELECT CASE (obj%nodalValueType)

! Constant
CASE (CONSTANT)
  CALL Reallocate(nodalValue, tNodes, tTimes)
  nodalValue = obj%nodalValue(1, 1)

! Space
CASE (SPACE)
  isNOTOK = SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)
  IF (isNOTOK) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
     & '[INTERNAL ERROR] :: SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
    RETURN
  END IF

  nodalValue = obj%nodalValue

! Time
CASE (TIME)
  tsize = SIZE(obj%nodalValue, 1)
  IF (PRESENT(times)) THEN
    IF (tsize .NE. SIZE(times)) THEN
      CALL e%RaiseError(modName//'::'//myName//" - "// &
       & '[INTERNAL ERROR] :: SIZE( obj%nodalValue, 2 ) .NE. SIZE( times )')
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
     & '[INTERNAL ERROR] :: SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
  END IF

  IF (PRESENT(times)) THEN
    IF (SIZE(obj%nodalValue, 2) .NE. SIZE(times)) THEN
      CALL e%RaiseError(modName//'::'//myName//" - "// &
        & '[INTERNAL ERROR] :: SIZE( obj%nodalValue, 2 ) .NE. SIZE( times )')
    END IF
  END IF

  nodalValue = obj%nodalValue
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Get()')
#endif
END PROCEDURE bc_Get

!----------------------------------------------------------------------------
!                                                           bc_GetFEVar
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetFEVar
CHARACTER(*), PARAMETER :: myName = "bc_GetFEVar()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development.')

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

MODULE PROCEDURE bc_GetFromFunction
CHARACTER(*), PARAMETER :: myName = "bc_GetFromFunction"
INTEGER(I4B) :: ii, kk
REAL(DFP), POINTER :: xij(:, :)

! get pointer to nodecoord

xij => obj%dom%getNodeCoordPointer()

SELECT CASE (obj%nodalValueType)

! Space
CASE (Space)

  IF (.NOT. ASSOCIATED(obj%SpaceFunction)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "When nodalValueType is &
    & Space and useFunction is specified, &
    & then SpaceFunction is needed, &
    & but it is not associated")
  END IF

  CALL Reallocate(nodalValue, SIZE(nodeNum), 1)

  DO ii = 1, SIZE(nodeNum)
    nodalValue(ii, 1) = obj%SpaceFunction( &
      & x=xij(:, obj%dom%getLocalNodeNumber(globalNode=nodeNum(ii))))
  END DO

! Time
CASE (Time)

  IF (.NOT. ASSOCIATED(obj%TimeFunction)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "When nodalValueType is Time &
    & and useFunction is specified, &
    & then TimeFunction is needed, &
    & but it is not associated")
  END IF

  IF (.NOT. PRESENT(times)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "When `nodalValueType` is Time &
    & and `useFunction` is TRUE, &
    & then `times` is needed in the passing argument, &
    & but it is not present")
  END IF

  CALL Reallocate(nodalValue, SIZE(nodeNum), SIZE(times))

  DO ii = 1, SIZE(times)
    nodalValue(:, ii) = obj%TimeFunction(t=times(ii))
  END DO

! SpaceTime
CASE (SpaceTime)

  IF (.NOT. ASSOCIATED(obj%SpaceTimeFunction)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & "When `nodalValueType` is `SpaceTime` and &
      & `useFunction` is specified, &
      & then `SpaceTimeFunction` is needed, &
      & but it is not associated")
  END IF

  IF (.NOT. PRESENT(times)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & "When `nodalValueType` is `SpaceTime` &
      & and `useFunction` is True, &
      & then `times` is needed as argument, &
      & but it is not present")
  END IF

  CALL Reallocate(nodalValue, SIZE(nodeNum), SIZE(times))

  DO kk = 1, SIZE(times)
    DO ii = 1, SIZE(nodeNum)
      nodalValue(ii, kk) = &
        & obj%SpaceTimeFunction( &
        & x=xij(:, obj%dom%getLocalNodeNumber(globalNode=nodeNum(ii))), &
        & t=times(kk))
    END DO
  END DO

END SELECT

NULLIFY (xij)

END PROCEDURE bc_GetFromFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_isuseFunction
ans = obj%useFunction
END PROCEDURE bc_isuseFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetQuery
CALL obj%boundary%GetQuery(&
& isSelectionByBox=isSelectionByBox, &
& isSelectionByMeshID=isSelectionByMeshID, &
& isSelectionByElemNum=isSelectionByElemNum, &
& isSelectionByNodeNum=isSelectionByNodeNum)

IF (PRESENT(idof)) idof = obj%idof
IF (PRESENT(isTangent)) isTangent = obj%isTangent
IF (PRESENT(isNormal)) isNormal = obj%isNormal
IF (PRESENT(useFunction)) useFunction = obj%useFunction
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
  & '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE bc_GetPrefix

!----------------------------------------------------------------------------
!                                                       GetFromUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetFromUserFunction
CHARACTER(*), PARAMETER :: myName = "bc_GetFromUserFunction()"
INTEGER(I4B) :: ii, kk, retType, tNodes, nsd, tTimes
REAL(DFP) :: xij(4, 1), ans
LOGICAL(LGT) :: problem

! get pointer to nodecoord

IF (.NOT. ASSOCIATED(obj%func)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[INTERNAL ERROR] :: When nodalValueType is "//  &
  & CHAR_LF//"Space and useFunction is specified, "//  &
  & CHAR_LF//"then SpaceFunction is needed, "//  &
  & CHAR_LF//"but it is not associated")
  RETURN
END IF

retType = obj%func%GetReturnType()

IF (retType .NE. Scalar) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Return type of user function should be '//  &
    & 'scalar.')
  RETURN
END IF

problem = (obj%nodalValueType .EQ. Time) .AND. (.NOT. PRESENT(times))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[INTERNAL ERROR] :: When `nodalValueType` is Time "//  &
    & " and `IsUserFunction` is TRUE, "//  &
    & " then `times` is needed in the passing argument,"//  &
    & " but it is not present")
  RETURN
END IF

problem = (obj%nodalValueType .EQ. SpaceTime) .AND. (.NOT. PRESENT(times))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[INTERNAL ERROR] :: When `nodalValueType` is SpaceTime "//  &
    & " and `IsUserFunction` is TRUE, "//  &
    & " then `times` is needed in the passing argument,"//  &
    & " but it is not present")
  RETURN
END IF

tNodes = SIZE(nodeNum)
nsd = obj%dom%GetNSD()

SELECT CASE (obj%nodalValueType)

! Constant
CASE (Constant)
  CALL Reallocate(nodalValue, tNodes, 1)

  CALL obj%dom%GetNodeCoord(nodeCoord=xij(1:nsd, 1:1),  &
    & globalNode=nodeNum(1:1))

  CALL obj%func%Get(val=ans, args=xij(1:nsd, 1))

  nodalValue(:, 1) = ans

! Space
CASE (Space)
  CALL Reallocate(nodalValue, tNodes, 1)

  DO ii = 1, tNodes
    CALL obj%dom%GetNodeCoord(nodeCoord=xij(1:nsd, 1:1),  &
      & globalNode=nodeNum(ii:ii))

    CALL obj%func%Get(val=ans, args=xij(1:nsd, 1))

    nodalValue(ii, 1) = ans
  END DO

! Time
CASE (Time)

  tTimes = SIZE(times)

  CALL Reallocate(nodalValue, tNodes, tTimes)

  DO ii = 1, tTimes
    CALL obj%func%Get(val=ans, args=times(ii:ii))

    nodalValue(:, ii) = ans
  END DO

! SpaceTime
CASE (SpaceTime)
  tTimes = SIZE(times)

  CALL Reallocate(nodalValue, tNodes, tTimes)

  DO kk = 1, tTimes
    xij(nsd + 1, 1) = times(kk)

    DO ii = 1, tNodes
      CALL obj%dom%GetNodeCoord(nodeCoord=xij(1:nsd, 1:1),  &
        & globalNode=nodeNum(ii:ii))

      CALL obj%func%Get(val=ans, args=xij(1:nsd + 1, 1))

      nodalValue(ii, kk) = ans
    END DO
  END DO

END SELECT

END PROCEDURE bc_GetFromUserFunction

END SUBMODULE GetMethods
