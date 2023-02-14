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
!

SUBMODULE(AbstractBC_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "bc_CheckEssentialParam"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by children of AbstractBC_')
END PROCEDURE bc_CheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Initiate
CHARACTER(*), PARAMETER :: myName = "bc_Initiate"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by children of AbstractBC_')
END PROCEDURE bc_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Import
CHARACTER(*), PARAMETER :: myName = "bc_Import"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by children of AbstractBC_')
END PROCEDURE bc_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Export
CHARACTER(*), PARAMETER :: myName = "bc_Export"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by children of AbstractBC_')
END PROCEDURE bc_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Display
CHARACTER(*), PARAMETER :: myName = "bc_Display"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by children of AbstractBC_')
END PROCEDURE bc_Display

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Deallocate
obj%isInitiated = .FALSE.
obj%name = ''
obj%idof = 0
obj%nodalValueType = -1
obj%useFunction = .FALSE.
IF (ALLOCATED(obj%nodalValue)) DEALLOCATE (obj%nodalValue)
IF (ASSOCIATED(obj%SpaceTimeFunction)) obj%SpaceTimeFunction => NULL()
IF (ASSOCIATED(obj%SpaceFunction)) obj%SpaceFunction => NULL()
IF (ASSOCIATED(obj%TimeFunction)) obj%TimeFunction => NULL()
CALL obj%boundary%DEALLOCATE()
IF (ASSOCIATED(obj%dom)) obj%dom => NULL()
END PROCEDURE bc_Deallocate

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
!
! get node numbers
!

IF (.NOT. ASSOCIATED(obj%dom)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'AbstractBC_::obj%dom is not associated!')
ELSE
  nodeNum = obj%Boundary%getNodeNum(domain=obj%dom)
END IF
!
! get nodal values
!
IF (PRESENT(nodalValue)) THEN
  !
  ! Use function case
  !
  IF (obj%UseFunction) THEN
    CALL GetUserFunction(obj=obj, nodeNum=nodeNum, &
      & nodalValue=nodalValue, times=times)
    RETURN
  END IF

  IF (.NOT. ALLOCATED(obj%nodalValue)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'nodalValue is not allocated!')
  END IF
  !
  ! Getting nodal values
  !
  SELECT CASE (obj%nodalValueType)
    !
    ! Constant
    !
  CASE (CONSTANT)

    IF (PRESENT(times)) THEN
      CALL Reallocate(nodalValue, SIZE(nodeNum), SIZE(times))
    ELSE
      CALL Reallocate(nodalValue, SIZE(nodeNum), 1)
    END IF

    nodalValue = obj%nodalValue(1, 1)
    !
    ! Space
    !
  CASE (SPACE)

    IF (SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
       & 'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
    END IF

    nodalValue = obj%nodalValue
    !
    ! Time
    !
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
    !
    ! SpaceTime
    !
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

SUBROUTINE GetUserFunction(obj, nodeNum, nodalValue, times)
  CLASS(AbstractBC_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: nodeNum(:)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodalValue(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !
  ! Define internal variable
  !
  CHARACTER(*), PARAMETER :: myName = "GetUserFunction"
  INTEGER(I4B) :: ii, kk
  REAL(DFP), POINTER :: xij(:, :)
  !
  ! get pointer to nodecoord
  !
  xij => obj%dom%getNodeCoordPointer()
  !
  SELECT CASE (obj%nodalValueType)
    !
    ! space function
    !
  CASE (Space)

    IF (.NOT. ASSOCIATED(obj%SpaceFunction)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'When nodalValueType is &
      & Space and UseFunction is specified, &
      & then SpaceFunction is needed, &
      & but it is not associated!')
    END IF

    CALL Reallocate(nodalValue, SIZE(nodeNum), 1)

    DO ii = 1, SIZE(nodeNum)
      nodalValue(ii, 1) = obj%SpaceFunction( &
        & x=xij(:, obj%dom%getLocalNodeNumber(globalNode=nodeNum(ii))))
    END DO
    !
    ! Time
    !
  CASE (Time)

    IF (.NOT. ASSOCIATED(obj%TimeFunction)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'When nodalValueType is Time &
      & and UseFunction is specified, &
      & then TimeFunction is needed, &
      & but it is not associated!')
    END IF

    IF (.NOT. PRESENT(times)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'When `nodalValueType` is Time &
      & and `UseFunction` is TRUE, &
      & then `times` is needed in the passing argument, &
      & but it is not present!')
    END IF

    CALL Reallocate(nodalValue, SIZE(nodeNum), SIZE(times))

    DO ii = 1, SIZE(times)
      nodalValue(:, ii) = obj%TimeFunction(t=times(ii))
    END DO
    !
    ! SpaceTime
    !
  CASE (SpaceTime)

    IF (.NOT. ASSOCIATED(obj%SpaceTimeFunction)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'When `nodalValueType` is `SpaceTime` and &
        & `UseFunction` is specified, &
        & then `SpaceTimeFunction` is needed, &
        & but it is not associated!')
    END IF

    IF (.NOT. PRESENT(times)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'When `nodalValueType` is `SpaceTime` &
        & and `UseFunction` is True, &
        & then `times` is needed as argument, &
        & but it is not present!')
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

END SUBROUTINE GetUserFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_isUseFunction
ans = obj%useFunction
END PROCEDURE bc_isUseFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
