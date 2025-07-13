! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE BasisOptUtility
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: LGT, I4B, DFP

IMPLICIT NONE

PUBLIC :: SetIntegerType, SetRealType

CHARACTER(*), PARAMETER :: modName = "BasisOptUtility"

INTERFACE SetIntegerType
  MODULE PROCEDURE :: SetIntegerType1, SetIntegerType2
END INTERFACE SetIntegerType

INTERFACE SetRealType
  MODULE PROCEDURE :: SetRealType1
END INTERFACE SetRealType

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetIntegerType1(a, n, b)
  INTEGER(I4B), INTENT(INOUT) :: a(:)
  INTEGER(I4B), INTENT(IN) :: n
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: b(:)

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetIntegerType()"
#endif
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = PRESENT(b)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  tsize = SIZE(b)

  isok = tsize .EQ. 1
  IF (isok) THEN
    DO ii = 1, n
      a(ii) = b(1)
    END DO

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  DO ii = 1, n
    a(ii) = b(ii)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetIntegerType1

!----------------------------------------------------------------------------
!                                                          SetIntegerType2
!----------------------------------------------------------------------------

SUBROUTINE SetIntegerType2(a, nrow, ncol, b)
  INTEGER(I4B), INTENT(INOUT) :: a(:, :)
  INTEGER(I4B), INTENT(IN) :: nrow, ncol
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: b(:, :)

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetIntegerType()"
#endif
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, nrow0, ncol0, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = PRESENT(b)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  tsize = SIZE(b)

  isok = tsize .EQ. 1
  IF (isok) THEN
    a(1:nrow, 1:ncol) = b(1, 1)

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  nrow0 = MIN(nrow, SIZE(b, 1))
  ncol0 = MIN(ncol, SIZE(b, 2))
  DO jj = 1, ncol0
    DO ii = 1, nrow0
      a(ii, jj) = b(ii, jj)
    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetIntegerType2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetRealType1(a, n, b)
  REAL(DFP), INTENT(INOUT) :: a(:)
  INTEGER(I4B), INTENT(IN) :: n
  REAL(DFP), OPTIONAL, INTENT(IN) :: b(:)

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetRealType()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = PRESENT(b)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  tsize = SIZE(b)

  isok = tsize .EQ. 1
  IF (isok) THEN
    DO ii = 1, n
      a(ii) = b(1)
    END DO
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  DO ii = 1, n
    a(ii) = b(ii)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetRealType1

!----------------------------------------------------------------------------
!                                                          SetIntegerType2
!----------------------------------------------------------------------------

SUBROUTINE SetRealType2(a, nrow, ncol, b)
  REAL(DFP), INTENT(INOUT) :: a(:, :)
  INTEGER(I4B), INTENT(IN) :: nrow, ncol
  REAL(DFP), OPTIONAL, INTENT(IN) :: b(:, :)

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetIntegerType()"
#endif
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, nrow0, ncol0, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = PRESENT(b)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  tsize = SIZE(b)

  isok = tsize .EQ. 1
  IF (isok) THEN
    a(1:nrow, 1:ncol) = b(1, 1)

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  nrow0 = MIN(nrow, SIZE(b, 1))
  ncol0 = MIN(ncol, SIZE(b, 2))
  DO jj = 1, ncol0
    DO ii = 1, nrow0
      a(ii, jj) = b(ii, jj)
    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetRealType2

END MODULE BasisOptUtility
