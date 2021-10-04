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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains matrix vector method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) PreconditionMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           setPrecondition
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setPrecondition
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_setPrecondition"
  INTEGER( I4B ) :: ierr

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'MatrixField_ is not initiated')
  IF( PRESENT( param ) ) THEN
    IF( param%isPresent( key="MatrixField/preconditionName") ) THEN
      ierr = param%get( key="MatrixField/preconditionName", value=obj%Pmat%PmatName )
    ELSE
      CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'MatrixField/preconditionName should be present in param')
    END IF
    !
    SELECT CASE( obj%Pmat%PmatName )
    CASE( PRECOND_ILUT )
      IF( param%isPresent(key="MatrixField/droptol") ) THEN
        ierr = param%get( key="MatrixField/droptol", value=obj%Pmat%droptol )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/droptol should be present in param')
      END IF
      !
      IF( param%isPresent(key="MatrixField/lfil") ) THEN
        ierr = param%get( key="MatrixField/lfil", value=obj%Pmat%lfil )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/lfil should be present in param')
      END IF
    CASE( PRECOND_ILUTP )
      IF( param%isPresent(key="MatrixField/droptol") ) THEN
        ierr = param%get( key="MatrixField/droptol", value=obj%Pmat%droptol )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/droptol should be present in param')
      END IF
      !
      IF( param%isPresent(key="MatrixField/lfil") ) THEN
        ierr = param%get( key="MatrixField/lfil", value=obj%Pmat%lfil )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/lfil should be present in param')
      END IF
      !
      IF( param%isPresent(key="MatrixField/permtol") ) THEN
        ierr = param%get( key="MatrixField/permtol", value=obj%Pmat%permtol )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/permtol should be present in param')
      END IF
      !
      IF( param%isPresent(key="MatrixField/mbloc") ) THEN
        ierr = param%get( key="MatrixField/mbloc", value=obj%Pmat%mbloc )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/mbloc should be present in param')
      END IF
    CASE( PRECOND_ILUD )
      IF( param%isPresent(key="MatrixField/droptol") ) THEN
        ierr = param%get( key="MatrixField/droptol", value=obj%Pmat%droptol )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/droptol should be present in param')
      END IF
      !
      IF( param%isPresent(key="MatrixField/alpha") ) THEN
        ierr = param%get( key="MatrixField/alpha", value=obj%Pmat%alpha )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/alpha should be present in param')
      END IF
    CASE( PRECOND_ILUDP )
      IF( param%isPresent(key="MatrixField/droptol") ) THEN
        ierr = param%get( key="MatrixField/droptol", value=obj%Pmat%droptol )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/droptol should be present in param')
      END IF
      !
      IF( param%isPresent(key="MatrixField/alpha") ) THEN
        ierr = param%get( key="MatrixField/alpha", value=obj%Pmat%alpha )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/alpha should be present in param')
      END IF
      !
      IF( param%isPresent(key="MatrixField/permtol") ) THEN
        ierr = param%get( key="MatrixField/permtol", value=obj%Pmat%permtol )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/permtol should be present in param')
      END IF
      !
      IF( param%isPresent(key="MatrixField/mbloc") ) THEN
        ierr = param%get( key="MatrixField/mbloc", value=obj%Pmat%mbloc )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/mbloc should be present in param')
      END IF
    CASE( PRECOND_ILUK )
      IF( param%isPresent(key="MatrixField/lfil") ) THEN
        ierr = param%get( key="MatrixField/lfil", value=obj%Pmat%lfil )
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'MatrixField/lfil should be present in param')
      END IF
    END SELECT
  ELSE
    SELECT CASE( obj%Pmat%PmatName )
    CASE( PRECOND_ILUT )
      CALL mField_getILUT( obj )
    CASE( PRECOND_ILUTP )
      CALL mField_getILUTP( obj )
    CASE( PRECOND_ILUD )
      CALL mField_getILUD( obj )
    CASE( PRECOND_ILUDP )
      CALL mField_getILUDP( obj )
    CASE( PRECOND_ILUK )
      CALL mField_getILUK( obj )
    END SELECT
  END IF
END PROCEDURE mField_setPrecondition

!----------------------------------------------------------------------------
!                                                           getPrecondition
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getPrecondition
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_getPrecondition"
  CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine has not been implemented so far')
END PROCEDURE mField_getPrecondition

!----------------------------------------------------------------------------
!                                                                      ILUT
!----------------------------------------------------------------------------

SUBROUTINE mField_getILUT( obj )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  ! internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName = "mField_getILUT"
  INTEGER( I4B ) :: ierr
  !
  obj%Pmat%PmatName = PRECOND_ILUT
  obj%isPmatInitiated = .TRUE.
  CALL getILUT(obj=obj%mat, lfil=obj%Pmat%lfil, droptol=obj%Pmat%droptol, &
    & ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU )
END SUBROUTINE mField_getILUT

!----------------------------------------------------------------------------
!                                                                      ILUTP
!----------------------------------------------------------------------------

SUBROUTINE mField_getILUTP( obj )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  ! internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName = "mField_getILUTP"
  INTEGER( I4B ) :: ierr
  !
  obj%Pmat%PmatName = PRECOND_ILUTP
  obj%isPmatInitiated = .TRUE.
  CALL getILUTP( &
    & obj=obj%mat, lfil=obj%Pmat%lfil, droptol=obj%Pmat%droptol, &
    & permtol=obj%Pmat%permtol, mbloc=obj%Pmat%mbloc, &
    & IPERM=obj%Pmat%IPERM, &
    & ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU )
END SUBROUTINE mField_getILUTP

!----------------------------------------------------------------------------
!                                                                      ILUD
!----------------------------------------------------------------------------

SUBROUTINE mField_getILUD( obj )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  ! internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName = "mField_getILUD"
  INTEGER( I4B ) :: ierr
  !
  obj%Pmat%PmatName = PRECOND_ILUD
  obj%isPmatInitiated = .TRUE.
  CALL getILUD( &
    & obj=obj%mat, alpha=obj%Pmat%alpha, droptol=obj%Pmat%droptol, &
    & ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU )
END SUBROUTINE mField_getILUD

!----------------------------------------------------------------------------
!                                                                     ILUTDP
!----------------------------------------------------------------------------

SUBROUTINE mField_getILUDP( obj )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  ! internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName = "mField_getILUDP"
  INTEGER( I4B ) :: ierr
  !
  obj%Pmat%PmatName = PRECOND_ILUDP
  obj%isPmatInitiated = .TRUE.
  CALL getILUDP( &
    & obj=obj%mat, alpha=obj%Pmat%alpha, droptol=obj%Pmat%droptol, &
    & permtol=obj%Pmat%permtol, mbloc=obj%Pmat%mbloc, &
    & IPERM=obj%Pmat%IPERM, &
    & ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU )
END SUBROUTINE mField_getILUDP

!----------------------------------------------------------------------------
!                                                                     ILUK
!----------------------------------------------------------------------------

SUBROUTINE mField_getILUK( obj )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  ! internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName = "mField_getILUK"
  INTEGER( I4B ) :: ierr
  !
  obj%Pmat%PmatName = PRECOND_ILUK
  obj%isPmatInitiated = .TRUE.
  CALL getILUK( &
    & obj=obj%mat, lfil=obj%Pmat%lfil, LEVS=obj%Pmat%LEVS, &
    & ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU )
END SUBROUTINE mField_getILUK

!----------------------------------------------------------------------------
!                                                      reversePermutation
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_reversePermutation
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_reversePermutation"
  CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This subroutine has not been implemented yet')
END PROCEDURE mField_reversePermutation

END SUBMODULE PreconditionMethods