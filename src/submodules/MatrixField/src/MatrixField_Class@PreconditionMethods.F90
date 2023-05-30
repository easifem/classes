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
CHARACTER(*), PARAMETER :: myName = "mField_setPrecondition"
INTEGER(I4B) :: ierr

! check
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField_ is not initiated')
END IF

IF (obj%engine%chars() .NE. "NATIVE_SERIAL") THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is avaiable for NATIVE_SERIAL')
END IF

IF (PRESENT(param)) THEN

  obj%Pmat%nrow = SIZE(obj%mat, 1)
  obj%Pmat%ncol = SIZE(obj%mat, 2)

  IF (param%isPresent(key="Precond/name")) THEN
    ierr = param%get(key="Precond/name", &
      & VALUE=obj%Pmat%PmatName)
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/name should be present in param')
  END IF

  SELECT CASE (obj%Pmat%PmatName)

    ! ILUT
    ! droptol, lfil
  CASE (PRECOND_ILUT)

    IF (param%isPresent(key="Precond/droptol")) THEN
      ierr = param%get(key="Precond/droptol", VALUE=obj%Pmat%droptol)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'Precond/droptol should be present in param')
    END IF

    IF (param%isPresent(key="Precond/lfil")) THEN
      ierr = param%get(key="Precond/lfil", VALUE=obj%Pmat%lfil)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/lfil should be present in param')
    END IF

    RETURN

    ! ILUTP
    ! droptol, lfil, permtol, mbloc
  CASE (PRECOND_ILUTP)

    IF (param%isPresent(key="Precond/droptol")) THEN
      ierr = param%get(key="Precond/droptol", VALUE=obj%Pmat%droptol)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'Precond/droptol should be present in param')
    END IF

    IF (param%isPresent(key="Precond/lfil")) THEN
      ierr = param%get(key="Precond/lfil", VALUE=obj%Pmat%lfil)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/lfil should be present in param')
    END IF

    IF (param%isPresent(key="Precond/permtol")) THEN
      ierr = param%get(key="Precond/permtol", VALUE=obj%Pmat%permtol)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/permtol should be present in param')
    END IF

    IF (param%isPresent(key="Precond/mbloc")) THEN
      ierr = param%get(key="Precond/mbloc", VALUE=obj%Pmat%mbloc)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/mbloc should be present in param')
    END IF

    RETURN

    ! ILUD
    ! droptol, alpha
  CASE (PRECOND_ILUD)
    IF (param%isPresent(key="Precond/droptol")) THEN
      ierr = param%get(key="Precond/droptol", VALUE=obj%Pmat%droptol)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/droptol should be present in param')
    END IF

    IF (param%isPresent(key="Precond/alpha")) THEN
      ierr = param%get(key="Precond/alpha", VALUE=obj%Pmat%alpha)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/alpha should be present in param')
    END IF

    RETURN

    ! ILUDP
    ! droptol, alpha, permtol, mbloc
  CASE (PRECOND_ILUDP)
    IF (param%isPresent(key="Precond/droptol")) THEN
      ierr = param%get(key="Precond/droptol", VALUE=obj%Pmat%droptol)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/droptol should be present in param')
    END IF

    IF (param%isPresent(key="Precond/alpha")) THEN
      ierr = param%get(key="Precond/alpha", VALUE=obj%Pmat%alpha)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/alpha should be present in param')
    END IF

    IF (param%isPresent(key="Precond/permtol")) THEN
      ierr = param%get(key="Precond/permtol", VALUE=obj%Pmat%permtol)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/permtol should be present in param')
    END IF

    IF (param%isPresent(key="Precond/mbloc")) THEN
      ierr = param%get(key="Precond/mbloc", VALUE=obj%Pmat%mbloc)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/mbloc should be present in param')
    END IF

    RETURN

    ! ILUK
    ! lfil
  CASE (PRECOND_ILUK)

    IF (param%isPresent(key="Precond/lfil")) THEN
      ierr = param%get(key="Precond/lfil", VALUE=obj%Pmat%lfil)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Precond/lfil should be present in param')
    END IF
    RETURN
  END SELECT
END IF

SELECT CASE (obj%Pmat%PmatName)

CASE (PRECOND_ILUT)
  ! CALL mField_getILUT( obj )
  obj%isPmatInitiated = .TRUE.
  CALL getILUT( &
    & obj=obj%mat, &
    & lfil=obj%Pmat%lfil, &
    & droptol=obj%Pmat%droptol, &
    & ALU=obj%Pmat%A, &
    & JLU=obj%Pmat%JA, &
    & JU=obj%Pmat%JU)

CASE (PRECOND_ILUTP)
  ! CALL mField_getILUTP( obj )
  obj%isPmatInitiated = .TRUE.
  CALL getILUTP( &
    & obj=obj%mat, &
    & lfil=obj%Pmat%lfil, &
    & droptol=obj%Pmat%droptol, &
    & permtol=obj%Pmat%permtol, &
    & mbloc=obj%Pmat%mbloc, &
    & IPERM=obj%Pmat%IPERM, &
    & ALU=obj%Pmat%A, &
    & JLU=obj%Pmat%JA, &
    & JU=obj%Pmat%JU)

CASE (PRECOND_ILUD)
  ! CALL mField_getILUD( obj )
  obj%isPmatInitiated = .TRUE.
  CALL getILUD( &
    & obj=obj%mat, &
    & alpha=obj%Pmat%alpha, &
    & droptol=obj%Pmat%droptol, &
    & ALU=obj%Pmat%A, &
    & JLU=obj%Pmat%JA, &
    & JU=obj%Pmat%JU)

CASE (PRECOND_ILUDP)
  ! CALL mField_getILUDP( obj )
  obj%isPmatInitiated = .TRUE.
  CALL getILUDP( &
    & obj=obj%mat, &
    & alpha=obj%Pmat%alpha, &
    & droptol=obj%Pmat%droptol, &
    & permtol=obj%Pmat%permtol, &
    & mbloc=obj%Pmat%mbloc, &
    & IPERM=obj%Pmat%IPERM, &
    & ALU=obj%Pmat%A, &
    & JLU=obj%Pmat%JA, &
    & JU=obj%Pmat%JU)

CASE (PRECOND_ILUK)
  ! CALL mField_getILUK( obj )
  obj%isPmatInitiated = .TRUE.
  CALL getILUK( &
    & obj=obj%mat, &
    & lfil=obj%Pmat%lfil, &
    & LEVS=obj%Pmat%LEVS, &
    & ALU=obj%Pmat%A, &
    & JLU=obj%Pmat%JA, &
    & JU=obj%Pmat%JU)

END SELECT

END PROCEDURE mField_setPrecondition

!----------------------------------------------------------------------------
!                                                      reversePermutation
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_reversePermutation
CHARACTER(*), PARAMETER :: myName = "mField_reversePermutation"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine has not been implemented yet')
END PROCEDURE mField_reversePermutation

!----------------------------------------------------------------------------
!                                                           getPrecondition
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getPrecondition
CHARACTER(*), PARAMETER :: myName = "mField_getPrecondition"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine has not been implemented so far')
END PROCEDURE mField_getPrecondition

!----------------------------------------------------------------------------
!                                                         ApplyDBCtoPrecond
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-03
! summary: Thsi algo does not work, I am working onit

SUBROUTINE ApplyDBCtoPrecond(obj, dbcPtrs)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: dbcPtrs(:)

  INTEGER(I4B) :: i, ii
  LOGICAL(LGT), ALLOCATABLE :: mask(:)

  ASSOCIATE (JA => obj%Pmat%JA, A => obj%Pmat%A, nrow => obj%Pmat%nrow)

    ALLOCATE (mask(nrow))
    mask = .FALSE.
    mask(dbcPtrs) = .TRUE.

    DO CONCURRENT(i=1:SIZE(dbcPtrs))
      ii = dbcPtrs(i)
      A(JA(ii):JA(ii + 1) - 1) = 0.0_DFP
    END DO

    DO CONCURRENT(i=1:nrow)
      DO ii = JA(i), JA(i + 1) - 1
        IF (mask(JA(ii))) THEN
          A(ii) = 0.0_DFP
        END IF
      END DO
    END DO

    A(dbcPtrs) = 1.0_DFP

    DEALLOCATE (mask)

  END ASSOCIATE

END SUBROUTINE ApplyDBCtoPrecond

END SUBMODULE PreconditionMethods
