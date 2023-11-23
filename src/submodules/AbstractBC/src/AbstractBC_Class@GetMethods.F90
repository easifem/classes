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
INTEGER(I4B) :: ii, tsize
CHARACTER(*), PARAMETER :: myName = "bc_Get"

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'AbstractBC_ object is not initiated, initiate it first.')
END IF

! get node numbers

IF (.NOT. ASSOCIATED(obj%dom)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'AbstractBC_::obj%dom is not associated!')
ELSE
  nodeNum = obj%boundary%getNodeNum(domain=obj%dom)
END IF

! get nodal values

IF (PRESENT(nodalValue)) THEN

  ! Use function case

  IF (obj%useFunction) THEN
    CALL obj%GetFromFunction(nodeNum=nodeNum, &
      & nodalValue=nodalValue, times=times)
    RETURN
  END IF

  IF (.NOT. ALLOCATED(obj%nodalValue)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'nodalValue is not allocated!')
  END IF

  ! Getting nodal values

  SELECT CASE (obj%nodalValueType)

    ! Constant

  CASE (CONSTANT)

    IF (PRESENT(times)) THEN
      CALL Reallocate(nodalValue, SIZE(nodeNum), SIZE(times))
    ELSE
      CALL Reallocate(nodalValue, SIZE(nodeNum), 1)
    END IF

    nodalValue = obj%nodalValue(1, 1)

    ! Space

  CASE (SPACE)

    IF (SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
       & 'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
    END IF

    nodalValue = obj%nodalValue

    ! Time

  CASE (TIME)

    tsize = SIZE(obj%nodalValue, 1)
    IF (PRESENT(times)) THEN
      IF (tsize .NE. SIZE(times)) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
         & 'SIZE( obj%nodalValue, 2 ) .NE. SIZE( times )')
      END IF
    END IF

    CALL Reallocate(nodalValue, SIZE(nodeNum), tsize)

    DO ii = 1, tsize
      nodalValue(:, ii) = obj%nodalValue(ii, 1)
    END DO

    ! SpaceTime

  CASE (SpaceTime)

    IF (SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
       & 'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
    END IF

    IF (PRESENT(times)) THEN
      IF (SIZE(obj%nodalValue, 2) .NE. SIZE(times)) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & 'SIZE( obj%nodalValue, 2 ) .NE. SIZE( times )')
      END IF
    END IF

    nodalValue = obj%nodalValue

  END SELECT
END IF

END PROCEDURE bc_Get

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

  ! space function

CASE (Space)

  IF (.NOT. ASSOCIATED(obj%SpaceFunction)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
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
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & "When nodalValueType is Time &
    & and useFunction is specified, &
    & then TimeFunction is needed, &
    & but it is not associated")
  END IF

  IF (.NOT. PRESENT(times)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
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
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "When `nodalValueType` is `SpaceTime` and &
      & `useFunction` is specified, &
      & then `SpaceTimeFunction` is needed, &
      & but it is not associated")
  END IF

  IF (.NOT. PRESENT(times)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
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

END SUBMODULE GetMethods
