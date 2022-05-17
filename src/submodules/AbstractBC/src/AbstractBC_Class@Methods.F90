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

MODULE PROCEDURE bc_addSurrogate
  CALL e%addSurrogate(userObj)
END PROCEDURE bc_addSurrogate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Deallocate
  obj%isInitiated = .FALSE.
  obj%name = ''
  CALL obj%boundary%Deallocate()
  obj%dom => NULL()
END PROCEDURE bc_Deallocate

!----------------------------------------------------------------------------
!                                                                 getMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_getMeshID
  ans = obj%boundary%getMeshID( dim=dim )
END PROCEDURE bc_getMeshID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_getDOFNo
  ans = obj%idof
END PROCEDURE bc_getDOFNo

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Get
  INTEGER(I4B) :: ii
  CHARACTER(LEN=*), PARAMETER :: myName = "bc_Get"
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
  IF (.NOT. obj%isInitiated) &
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'DiricheltBC object is not initiated, initiate it first.')
#endif
  !!
  !! get node numbers
  !!
  nodeNum = obj%Boundary%getNodeNum(domain=obj%dom)
  !!
  !! get nodal values
  !!
  IF( PRESENT( nodalValue ) ) THEN
    !!
    !! Use function case
    !!
    IF (obj%UseFunction) THEN
      CALL GetUserFunction(obj=obj, nodeNum=nodeNum, &
        & nodalValue=nodalValue, times=times )
      RETURN
    END IF
    !!
    !! check
    !!
#ifdef DEBUG_VER
    IF (.NOT. ALLOCATED(obj%nodalValue)) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'nodalValue is not allocated!')
#endif
    !!
    !! Getting nodal values
    !!
    SELECT CASE (obj%nodalValueType)
    !!
    !! Constant
    !!
    CASE (CONSTANT)
      !!
      IF (PRESENT(times)) THEN
        CALL Reallocate(nodalValue, SIZE(nodeNum), SIZE(times))
      ELSE
        CALL Reallocate(nodalValue, SIZE(nodeNum), 1)
      END IF
      !!
      nodalValue = obj%nodalValue(1, 1)
    !!
    !! Space
    !!
    CASE (SPACE)
      !!
#ifdef DEBUG_VER
      IF (SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)) &
        & CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
#endif
      !!
      nodalValue = obj%nodalValue
      !!
      !! Time
      !!
    CASE (TIME)
      !!
#ifdef DEBUG_VER
      IF (PRESENT(times)) THEN
        IF (SIZE(obj%nodalValue, 1) .NE. SIZE(times)) &
          & CALL e%raiseError(modName//'::'//myName//" - "// &
          & 'SIZE( obj%nodalValue, 2 ) .NE. SIZE( times )')
      END IF
#endif
      !!
      CALL Reallocate( nodalValue, SIZE(nodeNum), SIZE(obj%nodalValue, 1) )
      !!
      DO ii = 1, SIZE(obj%nodalValue, 1)
        nodalValue(:, ii) = obj%nodalValue(ii, 1)
      END DO
    !!
    !! SpaceTime
    !!
    CASE (SpaceTime)
      !!
#ifdef DEBUG_VER
      IF (SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)) &
        & CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
      !!
      IF (PRESENT(times)) THEN
        IF (SIZE(obj%nodalValue, 2) .NE. SIZE(times)) &
          & CALL e%raiseError(modName//'::'//myName//" - "// &
          & 'SIZE( obj%nodalValue, 2 ) .NE. SIZE( times )')
      END IF
#endif
      !!
      nodalValue = obj%nodalValue
      !!
    END SELECT
  END IF
  !!
  !!
END PROCEDURE bc_Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE GetUserFunction(obj, nodeNum, nodalValue, times )
  !! Intent of dummy variable
  CLASS( AbstractBC_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN) :: nodeNum( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: nodalValue( :, : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: times( : )
  !!
  !! Define internal variable
  !!
  CHARACTER(LEN=*), PARAMETER :: myName = "GetUserFunction"
  INTEGER(I4B) :: ii, kk
  REAL(DFP), POINTER :: xij(:, :)
  !!
  !! get pointer to nodecoord
  !!
  xij => obj%dom%getNodeCoordPointer()
  !!
  SELECT CASE (obj%nodalValueType)
  !!
  !! space function
  !!
  CASE (Space)
    !!
#ifdef DEBUG_VER
    !!
    !! check
    !!
    IF (.NOT. ASSOCIATED(obj%SpaceFunction)) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'When nodalValueType is &
      & Space and UseFunction is specified, &
      & then SpaceFunction is needed, &
      & but it is not associated!')
#endif
    !!
    CALL Reallocate(nodalValue, SIZE(nodeNum), 1)
    !!
    DO ii = 1, SIZE(nodeNum)
      nodalValue(ii, 1) = obj%SpaceFunction( &
        & x=xij(:, obj%dom%getLocalNodeNumber(globalNode=nodeNum(ii) )))
    END DO
  !!
  !! Time
  !!
  CASE (Time)
    !!
#ifdef DEBUG_VER
    !!
    !! check
    !!
    IF (.NOT. ASSOCIATED(obj%TimeFunction)) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'When nodalValueType is Time &
      & and UseFunction is specified, &
      & then TimeFunction is needed, &
      & but it is not associated!')
    !!
    IF (.NOT. PRESENT(times)) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'When `nodalValueType` is Time &
      & and `UseFunction` is TRUE, &
      & then `times` is needed in the passing argument, &
      & but it is not present!')
#endif
    !!
    CALL Reallocate(nodalValue, SIZE(nodeNum), SIZE(times))
    !!
    DO ii = 1, SIZE(times)
      nodalValue(:, ii) = obj%TimeFunction(t=times(ii))
    END DO
  !!
  !! SpaceTime
  !!
  CASE (SpaceTime)
    !!
#ifdef DEBUG_VER
    !!
    !! Check
    !!
    IF (.NOT. ASSOCIATED(obj%SpaceTimeFunction)) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'When `nodalValueType` is `SpaceTime` and &
      & `UseFunction` is specified, &
      & then `SpaceTimeFunction` is needed, &
      & but it is not associated!')
    !!
    IF (.NOT. PRESENT(times)) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'When `nodalValueType` is `SpaceTime` &
      & and `UseFunction` is True, &
      & then `times` is needed as argument, &
      & but it is not present!')
#endif
    !!
    CALL Reallocate(nodalValue, SIZE(nodeNum), SIZE(times))
    !!
    DO kk = 1, SIZE(times)
      DO ii = 1, SIZE(nodeNum)
        nodalValue(ii, kk) = &
          & obj%SpaceTimeFunction( &
          & x=xij(:, obj%dom%getLocalNodeNumber(globalNode=nodeNum(ii))), &
          & t=times(kk))
      END DO
    END DO
    !!
  END SELECT
  !!
  NULLIFY (xij)
  !!
END SUBROUTINE GetUserFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods