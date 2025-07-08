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

SUBMODULE(AbstractField_Class) GetMethods
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
!USE AbstractNodeField_Class, ONLY: AbstractNodeField_
!USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_

CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

IF (PRESENT(isInitiated)) isInitiated = obj%isInitiated
IF (PRESENT(fieldType)) fieldType = obj%fieldType
IF (PRESENT(name)) name = obj%name%chars()
IF (PRESENT(engine)) engine = obj%engine%chars()
IF (PRESENT(comm)) comm = obj%comm
IF (PRESENT(myRank)) myRank = obj%myRank
IF (PRESENT(numProcs)) numProcs = obj%numProcs
IF (PRESENT(global_n)) global_n = obj%global_n
IF (PRESENT(local_n)) local_n = obj%local_n
IF (PRESENT(is)) is = obj%is
IF (PRESENT(ie)) ie = obj%ie
IF (PRESENT(lis_ptr)) lis_ptr = obj%lis_ptr

IF (PRESENT(fedof)) fedof => obj%fedof

IF (PRESENT(fedofs)) THEN

  isok = ALLOCATED(obj%fedofs)
  IF (.NOT. isok) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
           '[INTERNAL ERROR] :: AbstractField_::obj%fedofs is not allocated ')
    RETURN
  END IF

  isok = SIZE(obj%fedofs) .EQ. SIZE(fedofs)

  IF (.NOT. isok) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: AbstractField_::obj%fedofs size mismatch')
    RETURN
  END IF

  DO ii = 1, SIZE(fedofs)
    fedofs(ii)%ptr => obj%fedofs(ii)%ptr
  END DO

END IF

!SELECT TYPE (obj)
!CLASS IS (AbstractNodeField_)
!  IF (PRESENT(tSize)) tSize = obj%tSize
!  IF (PRESENT(realVec)) realVec = obj%realVec
!  IF (PRESENT(dof)) dof = obj%dof
!CLASS IS (AbstractMatrixField_)
!  IF (PRESENT(isPMatInitiated)) isPMatInitiated = obj%isPMatInitiated
!END SELECT
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                       GetTotalPhysicalVars
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalPhysicalVars
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalPhysicalVars()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE obj_GetTotalPhysicalVars

!----------------------------------------------------------------------------
!                                                     obj_GetPhysicalNames
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPhysicalNames
CHARACTER(*), PARAMETER :: myName = "obj_GetNames()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  " child classes.")
END PROCEDURE obj_GetPhysicalNames

!----------------------------------------------------------------------------
!                                                           GetSpaceCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpaceCompo
CHARACTER(*), PARAMETER :: myName = "obj_GetSpaceCompo()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  " child classes.")
END PROCEDURE obj_GetSpaceCompo

!----------------------------------------------------------------------------
!                                                           GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeCompo
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeCompo"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  " child classes.")
END PROCEDURE obj_GetTimeCompo

!----------------------------------------------------------------------------
!                                                           GetStorageFMT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStorageFMT
CHARACTER(*), PARAMETER :: myName = "obj_GetStorageFMT"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  " child classes.")
END PROCEDURE obj_GetStorageFMT

!----------------------------------------------------------------------------
!                                                               GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  ' child classes')
END PROCEDURE obj_GetTotalDOF

!----------------------------------------------------------------------------
!                                                          GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexDOF
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE obj_GetTotalVertexDOF

!----------------------------------------------------------------------------
!                                                          GetTotalEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEdgeDOF
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEdgeDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE obj_GetTotalEdgeDOF

!----------------------------------------------------------------------------
!                                                          GetTotalFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFaceDOF
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalFaceDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE obj_GetTotalFaceDOF

!----------------------------------------------------------------------------
!                                                          GetTotalCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalCellDOF
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalCellDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE obj_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                                                 isConstant
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isConstant
IF (obj%fieldType .EQ. TypeField%constant) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_isConstant

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
ans = ""
CALL e%RaiseError(modName//'::'//myName//' - '// &
'[IMPLEMENTATION ERROR] :: This method should be implemented by child class.')
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                           GetFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEDOFPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEDOFPointer1()"
#endif

#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize
#endif

LOGICAL(LGT) :: indxPresent, fedofsAllocated

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

indxPresent = PRESENT(indx)
fedofsAllocated = ALLOCATED(obj%fedofs)

IF (indxPresent .AND. fedofsAllocated) THEN

#ifdef DEBUG_VER
  tsize = SIZE(obj%fedofs)
  isok = indx .LE. tsize

  CALL AssertError1(isok, myName, &
                    "indx should be less than or equal to size of fedofs")
#endif

  ans => obj%fedofs(indx)%ptr

ELSE

  ans => obj%fedof

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetFEDOFPointer1

!----------------------------------------------------------------------------
!                                                          GetFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEDOFPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEDOFPointer2()"
#endif

INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%fedofs)

IF (isok) THEN
  tsize = SIZE(obj%fedofs)
ELSE
  tsize = 0
END IF

ALLOCATE (ans(tsize))

DO ii = 1, tsize
  ans(ii)%ptr => obj%fedofs(ii)%ptr
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetFEDOFPointer2

!----------------------------------------------------------------------------
!                                                        GetTimeFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeFEDOFPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeFEDOFPointer1()"
#endif

#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize
#endif

LOGICAL(LGT) :: indxPresent, fedofsAllocated

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

indxPresent = PRESENT(indx)
fedofsAllocated = ALLOCATED(obj%timefedofs)

IF (indxPresent .AND. fedofsAllocated) THEN

#ifdef DEBUG_VER
  tsize = SIZE(obj%timefedofs)
  isok = indx .LE. tsize

  CALL AssertError1(isok, myName, &
                    "indx should be less than or equal to size of timefedofs")
#endif

  ans => obj%timefedofs(indx)%ptr

ELSE

  ans => obj%timefedof

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTimeFEDOFPointer1

!----------------------------------------------------------------------------
!                                                        GetTimeFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeFEDOFPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeFEDOFPointer2()"
#endif

INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%timefedofs)

IF (isok) THEN
  tsize = SIZE(obj%timefedofs)
ELSE
  tsize = 0
END IF

ALLOCATE (ans(tsize))

DO ii = 1, tsize
  ans(ii)%ptr => obj%timefedofs(ii)%ptr
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTimeFEDOFPointer2

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
