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
USE GlobalData, ONLY: PRECOND_ILUT, PRECOND_ILUTP, PRECOND_ILUD, &
                      PRECOND_ILUDP, PRECOND_ILUK

USE CSRMatrix_Method, ONLY: GetILUT, GetILUTP, GetILUD, &
                            GetILUDP, GetILUK, CSRMatrix_Size => Size
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           SetPrecondition
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_SetPrecondition
! CHARACTER(*), PARAMETER :: myName = "obj_SetPrecondition()"
! LOGICAL(LGT) :: isok
!
! INTEGER(I4B) :: ierr
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! #ifdef DEBUG_VER
! isok = obj%IsInitiated()
! CALL AssertError1(isok, myName, &
!                   'MatrixField_ is not initiated')
! #endif
!
! #ifdef DEBUG_VER
! isok = obj%engine%chars() .EQ. "NATIVE_SERIAL"
! CALL AssertError1(isok, myName, &
!                   'This routine is avaiable for NATIVE_SERIAL')
! #endif
!
! IF (PRESENT(param)) THEN
!
!   obj%Pmat%nrow = CSRMatrix_SIZE(obj%mat, 1)
!   obj%Pmat%ncol = CSRMatrix_SIZE(obj%mat, 2)
!
! #ifdef DEBUG_VER
!   isok = param%isPresent(key="Precond/name")
!   CALL AssertError1(isok, myName, &
!                     'Precond/name should be present in param')
! #endif
!
!   ierr = param%Get(key="Precond/name", VALUE=obj%Pmat%PmatName)
!
!   SELECT CASE (obj%Pmat%PmatName)
!
!     ! ILUT
!     ! droptol, lfil
!   CASE (PRECOND_ILUT)
!
! #ifdef DEBUG_VER
!     isok = param%isPresent(key="Precond/droptol")
!     CALL AssertError1(isok, myName, &
!                       'Precond/droptol should be present in param')
! #endif
!
!     ierr = param%Get(key="Precond/droptol", VALUE=obj%Pmat%droptol)
!
! #ifdef DEBUG_VER
!     isok = param%isPresent(key="Precond/lfil")
!     CALL AssertError1(isok, myName, &
!                       'Precond/lfil should be present in param')
! #endif
!
!     ierr = param%Get(key="Precond/lfil", VALUE=obj%Pmat%lfil)
!
!     RETURN
!
!     ! ILUTP
!     ! droptol, lfil, permtol, mbloc
!   CASE (PRECOND_ILUTP)
!
! #ifdef DEBUG_VER
!     isok = param%isPresent(key="Precond/droptol")
!     CALL AssertError1(isok, myName, &
!                       'Precond/droptol should be present in param')
! #endif
!
!     ierr = param%Get(key="Precond/droptol", VALUE=obj%Pmat%droptol)
!
! #ifdef DEBUG_VER
!     isok = param%isPresent(key="Precond/lfil")
!     CALL AssertError1(isok, myName, &
!                       'Precond/lfil should be present in param')
! #endif
!
!     ierr = param%Get(key="Precond/lfil", VALUE=obj%Pmat%lfil)
!
!     IF (param%isPresent(key="Precond/permtol")) THEN
!       ierr = param%Get(key="Precond/permtol", VALUE=obj%Pmat%permtol)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/permtol should be present in param')
!     END IF
!
!     IF (param%isPresent(key="Precond/mbloc")) THEN
!       ierr = param%Get(key="Precond/mbloc", VALUE=obj%Pmat%mbloc)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/mbloc should be present in param')
!     END IF
!
!     RETURN
!
!     ! ILUD
!     ! droptol, alpha
!   CASE (PRECOND_ILUD)
!     IF (param%isPresent(key="Precond/droptol")) THEN
!       ierr = param%Get(key="Precond/droptol", VALUE=obj%Pmat%droptol)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/droptol should be present in param')
!     END IF
!
!     IF (param%isPresent(key="Precond/alpha")) THEN
!       ierr = param%Get(key="Precond/alpha", VALUE=obj%Pmat%alpha)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/alpha should be present in param')
!     END IF
!
!     RETURN
!
!     ! ILUDP
!     ! droptol, alpha, permtol, mbloc
!   CASE (PRECOND_ILUDP)
!     IF (param%isPresent(key="Precond/droptol")) THEN
!       ierr = param%Get(key="Precond/droptol", VALUE=obj%Pmat%droptol)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/droptol should be present in param')
!     END IF
!
!     IF (param%isPresent(key="Precond/alpha")) THEN
!       ierr = param%Get(key="Precond/alpha", VALUE=obj%Pmat%alpha)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/alpha should be present in param')
!     END IF
!
!     IF (param%isPresent(key="Precond/permtol")) THEN
!       ierr = param%Get(key="Precond/permtol", VALUE=obj%Pmat%permtol)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/permtol should be present in param')
!     END IF
!
!     IF (param%isPresent(key="Precond/mbloc")) THEN
!       ierr = param%Get(key="Precond/mbloc", VALUE=obj%Pmat%mbloc)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/mbloc should be present in param')
!     END IF
!
!     RETURN
!
!     ! ILUK
!     ! lfil
!   CASE (PRECOND_ILUK)
!
!     IF (param%isPresent(key="Precond/lfil")) THEN
!       ierr = param%Get(key="Precond/lfil", VALUE=obj%Pmat%lfil)
!     ELSE
!       CALL e%RaiseError(modName//'::'//myName//" - "// &
!                         'Precond/lfil should be present in param')
!     END IF
!     RETURN
!
!   CASE DEFAULT
!     CALL e%RaiseError(modName//'::'//myName//' - '// &
!                       '[INTERNAL ERROR] :: No case found for Pmatname')
!     RETURN
!   END SELECT
!
! END IF
!
! SELECT CASE (obj%Pmat%PmatName)
!
! CASE (PRECOND_ILUT)
!   ! CALL obj_GetILUT( obj )
!   obj%isPmatInitiated = .TRUE.
!   CALL GetILUT(obj=obj%mat, lfil=obj%Pmat%lfil, droptol=obj%Pmat%droptol, &
!                ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU)
!
! CASE (PRECOND_ILUTP)
!   ! CALL obj_GetILUTP( obj )
!   obj%isPmatInitiated = .TRUE.
!   CALL GetILUTP(obj=obj%mat, lfil=obj%Pmat%lfil, droptol=obj%Pmat%droptol, &
!        permtol=obj%Pmat%permtol, mbloc=obj%Pmat%mbloc, IPERM=obj%Pmat%IPERM, &
!                 ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU)
!
! CASE (PRECOND_ILUD)
!   ! CALL obj_GetILUD( obj )
!   obj%isPmatInitiated = .TRUE.
!   CALL GetILUD(obj=obj%mat, alpha=obj%Pmat%alpha, droptol=obj%Pmat%droptol, &
!                ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU)
!
! CASE (PRECOND_ILUDP)
!   ! CALL obj_GetILUDP( obj )
!   obj%isPmatInitiated = .TRUE.
!   CALL GetILUDP(obj=obj%mat, alpha=obj%Pmat%alpha, droptol=obj%Pmat%droptol, &
!        permtol=obj%Pmat%permtol, mbloc=obj%Pmat%mbloc, IPERM=obj%Pmat%IPERM, &
!                 ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU)
!
! CASE (PRECOND_ILUK)
!   ! CALL obj_GetILUK( obj )
!   obj%isPmatInitiated = .TRUE.
!   CALL GetILUK(obj=obj%mat, lfil=obj%Pmat%lfil, LEVS=obj%Pmat%LEVS, &
!                ALU=obj%Pmat%A, JLU=obj%Pmat%JA, JU=obj%Pmat%JU)
!
! CASE DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTERNAL ERROR] :: No case found for Pmatname')
!
! END SELECT
!
! END PROCEDURE obj_SetPrecondition

!----------------------------------------------------------------------------
!                                                      reversePermutation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_reversePermutation
CHARACTER(*), PARAMETER :: myName = "obj_reversePermutation"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_reversePermutation

!----------------------------------------------------------------------------
!                                                           GetPrecondition
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrecondition
CHARACTER(*), PARAMETER :: myName = "obj_GetPrecondition"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetPrecondition

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

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE PreconditionMethods
