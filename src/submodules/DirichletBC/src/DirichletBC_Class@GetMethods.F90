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

SUBMODULE(DirichletBC_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 dbc_set
!----------------------------------------------------------------------------

MODULE PROCEDURE dbc_Get
CHARACTER(LEN=*), PARAMETER :: myName = "dbc_Get"
TYPE(IntVector_) :: aIntVec
INTEGER(I4B), ALLOCATABLE :: meshID(:)
INTEGER(I4B) :: nsd, ii, jj, kk
REAL(DFP), POINTER :: xij(:, :)

!> check
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'DiricheltBC object is not initiated, initiate it first.')
END IF
!> use function case
IF (obj%UseFunction) THEN
  !> SelectionByNodeNum
  IF (obj%Boundary%isSelectionByNodeNum) THEN
    CALL APPEND(aIntVec, obj%Boundary%getNodeNum())
  END IF
  !> isSelectionByMeshID
  IF (obj%Boundary%isSelectionByMeshID) THEN
    nsd = obj%dom%getNSD()
    DO ii = 0, nsd - 1
      IF (obj%Boundary%isMeshIDAllocated(dim=ii)) THEN
        meshID = obj%Boundary%getMeshID(dim=ii)
        CALL APPEND(aIntVec, obj%dom%getNptrs(xidim=ii, meshID=meshID))
      END IF
    END DO
    IF (ALLOCATED(meshID)) DEALLOCATE (meshID)
  END IF
  !> check
  IF (.NOT. isAllocated(aIntVec)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'It seems Dirichlet node number are empty')
  ELSE
    NodeNum = aIntVec
    CALL Deallocate(aIntVec)
  END IF
  !> SpaceFunction
  xij = obj%dom%getNodeCoordPointer()
  SELECT CASE (obj%nodalValueType)
  CASE (Space)
    IF (.NOT. ASSOCIATED(obj%SpaceFunction)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'When NodalValueType is Space and UseFunction is specified, &
        & then SpaceFunction is needed, but it is not associated!')
    ELSE
      CALL Reallocate(NodalValue, SIZE(NodeNum), 1)
      DO ii = 1, SIZE(NodeNum)
        jj = obj%dom%getLocalNodeNumber(globalNode=NodeNum(ii))
        NodalValue(ii, 1) = obj%SpaceFunction(x=xij(:, jj))
      END DO
    END IF
  CASE (Time)
    IF (.NOT. ASSOCIATED(obj%TimeFunction)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'When NodalValueType is Time and UseFunction is specified, &
        & then TimeFunction is needed, but it is not associated!')
    ELSE
      IF (.NOT. PRESENT(times)) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'When NodalValueType is Time and UseFunction is specified, &
        & then times is needed as argument, but it is not present!')
      ELSE
        CALL Reallocate(NodalValue, SIZE(NodeNum), SIZE(times))
        DO ii = 1, SIZE(times)
          NodalValue(:, ii) = obj%TimeFunction(t=times(ii))
        END DO
      END IF
    END IF
  CASE (SpaceTime)
    IF (.NOT. ASSOCIATED(obj%SpaceTimeFunction)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'When NodalValueType is SpaceTime and UseFunction is specified, &
        & then SpaceTimeFunction is needed, but it is not associated!')
    ELSE
      IF (.NOT. PRESENT(times)) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'When NodalValueType is SpaceTime and UseFunction is specified, &
        & then times is needed as argument, but it is not present!')
      ELSE
        CALL Reallocate(NodalValue, SIZE(NodeNum), SIZE(times))
        DO kk = 1, SIZE(times)
          DO ii = 1, SIZE(NodeNum)
            jj = obj%dom%getLocalNodeNumber(globalNode=NodeNum(ii))
            NodalValue(ii, kk) = obj%SpaceTimeFunction(x=xij(:, jj),&
              & t=times(kk))
          END DO
        END DO
      END IF
    END IF
  END SELECT
  NULLIFY (xij)
ELSE
  !> check
  IF (.NOT. ALLOCATED(obj%NodalValue)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'NodalValue is not allocated!')
  END IF
  !> SelectionByNodeNum
  IF (obj%Boundary%isSelectionByNodeNum) THEN
    NodeNum = obj%Boundary%getNodeNum()
    SELECT CASE (obj%NodalValueType)
    CASE (Constant)
      IF (PRESENT(times)) THEN
        CALL Reallocate(NodalValue, SIZE(NodeNum), SIZE(times))
      ELSE
        CALL Reallocate(NodalValue, SIZE(NodeNum), 1)
      END IF
      NodalValue = obj%NodalValue(1, 1)
    CASE (Space)
      IF (SIZE(obj%NodalValue, 1) .NE. SIZE(NodeNum)) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & 'SIZE( obj%NodalValue, 1 ) .NE. SIZE( NodeNum )')
      ELSE
        NodalValue = obj%NodalValue
      END IF
    CASE (Time)
      IF (PRESENT(times)) THEN
        IF (SIZE(obj%NodalValue, 1) .NE. SIZE(times)) THEN
          CALL e%raiseError(modName//'::'//myName//" - "// &
            & 'SIZE( obj%NodalValue, 1 ) .NE. SIZE( times )')
        END IF
      END IF
      CALL Reallocate(NodalValue, SIZE(NodeNum), &
      & SIZE(obj%NodalValue, 1))
      DO ii = 1, SIZE(obj%NodalValue, 1)
        NodalValue(:, ii) = obj%NodalValue(ii, 1)
      END DO
    CASE (SpaceTime)
      IF (SIZE(obj%NodalValue, 1) .NE. SIZE(NodeNum)) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & 'SIZE( obj%NodalValue, 1 ) .NE. SIZE( NodeNum )')
      END IF
      IF (PRESENT(times)) THEN
        IF (SIZE(obj%NodalValue, 2) .NE. SIZE(times)) THEN
          CALL e%raiseError(modName//'::'//myName//" - "// &
            & 'SIZE( obj%NodalValue, 2 ) .NE. SIZE( times )')
        END IF
      END IF
      NodalValue = obj%NodalValue
    END SELECT
  ELSE IF (obj%Boundary%isSelectionByMeshID) THEN
    nsd = obj%dom%getNSD()
    DO ii = 0, nsd - 1
      IF (obj%Boundary%isMeshIDAllocated(dim=ii)) THEN
        meshID = obj%Boundary%getMeshID(dim=ii)
        CALL APPEND(aIntVec, obj%dom%getNptrs(xidim=ii, meshID=meshID))
      END IF
    END DO
    IF (ALLOCATED(meshID)) DEALLOCATE (meshID)
    !> check
    IF (.NOT. isAllocated(aIntVec)) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'It seems Dirichlet node number are empty')
    ELSE
      NodeNum = aIntVec
      CALL Deallocate(aIntVec)
    END IF
    IF (obj%nodalValueType .EQ. Constant) THEN
      IF (PRESENT(times)) THEN
        CALL Reallocate(NodalValue, SIZE(NodeNum), SIZE(times))
        NodalValue = obj%NodalValue(1, 1)
      ELSE
        CALL Reallocate(NodalValue, SIZE(NodeNum), 1)
        NodalValue = obj%NodalValue(1, 1)
      END IF
    ELSE
    END IF
  END IF
END IF

END PROCEDURE dbc_Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE dbc_getDOFNo
ans = obj%idof
END PROCEDURE dbc_getDOFNo

END SUBMODULE GetMethods
