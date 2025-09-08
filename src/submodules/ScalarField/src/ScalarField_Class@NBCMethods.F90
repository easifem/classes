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

SUBMODULE(ScalarField_Class) NBCMethods
USE Display_Method, ONLY: ToString, Display
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ApplyPointNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyNeumannBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyNeumannBC1()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: istimes, isElemToEdge, isElemToFace
INTEGER(I4B) :: tsize, tElemToFace, indx, localCellNumber, localFaceNumber

istimes = PRESENT(times)

#ifdef DEBUG_VER
tsize = 0
IF (istimes) THEN
  tsize = SIZE(times)
  isok = tsize .EQ. 1
  CALL AssertError1(isok, myName, &
                   'SIZE(times) is '//ToString(tsize)//', but it should be 1')
END IF
#endif

!! istimes os .false.
isElemToEdge = nbc%IsElemToEdgeInitiated()
isElemToFace = nbc%IsElemToFaceInitiated()

! Check
isok = isElemToEdge .OR. isElemToFace
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
          'isElemToEdge and isElemToFace are both .false. So, nothing to do.')

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

tElemToFace = nbc%GetTotalElemToFace()

#ifdef DEBUG_VER
CALL Display("localCellNumber | localFaceNumber")
#endif

DO indx = 1, tElemToFace
  CALL nbc%GetElemToFace(indx=indx, localCellNumber=localCellNumber, &
                         localFaceNumber=localFaceNumber)




END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyNeumannBC1

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE NBCMethods
