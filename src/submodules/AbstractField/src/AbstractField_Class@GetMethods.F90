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
USE FPL_Method
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
IF (PRESENT(domain)) domain => obj%domain

IF (PRESENT(domains)) THEN
  IF (.NOT. ALLOCATED(obj%domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'AbstractField_::obj%domains is not allocated ')
  END IF

  IF (SIZE(obj%domains) .NE. SIZE(domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'AbstractField_::obj%domains size is not same as size of domains')
  END IF

  DO ii = 1, SIZE(domains)
    domains(ii)%ptr => obj%domains(ii)%ptr
  END DO
END IF

IF (PRESENT(fedof)) fedof = obj%fedof

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

MODULE PROCEDURE aField_GetTotalPhysicalVars
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalPhysicalVars()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE aField_GetTotalPhysicalVars

!----------------------------------------------------------------------------
!                                                     aField_GetPhysicalNames
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetPhysicalNames
CHARACTER(*), PARAMETER :: myName = "aField_GetNames()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  " child classes.")
END PROCEDURE aField_GetPhysicalNames

!----------------------------------------------------------------------------
!                                                           GetSpaceCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetSpaceCompo
CHARACTER(*), PARAMETER :: myName = "aField_GetSpaceCompo()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  " child classes.")
END PROCEDURE aField_GetSpaceCompo

!----------------------------------------------------------------------------
!                                                           GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTimeCompo
CHARACTER(*), PARAMETER :: myName = "aField_GetTimeCompo"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  " child classes.")
END PROCEDURE aField_GetTimeCompo

!----------------------------------------------------------------------------
!                                                           GetStorageFMT
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetStorageFMT
CHARACTER(*), PARAMETER :: myName = "aField_GetStorageFMT"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  " child classes.")
END PROCEDURE aField_GetStorageFMT

!----------------------------------------------------------------------------
!                                                               GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  ' child classes')
END PROCEDURE aField_GetTotalDOF

!----------------------------------------------------------------------------
!                                                          GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalVertexDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalVertexDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE aField_GetTotalVertexDOF

!----------------------------------------------------------------------------
!                                                          GetTotalEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalEdgeDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalEdgeDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE aField_GetTotalEdgeDOF

!----------------------------------------------------------------------------
!                                                          GetTotalFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalFaceDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalFaceDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE aField_GetTotalFaceDOF

!----------------------------------------------------------------------------
!                                                          GetTotalCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalCellDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalCellDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE aField_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                                                 isConstant
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_isConstant
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE aField_isConstant

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetPrefix
CHARACTER(*), PARAMETER :: myName = "aField_GetPrefix()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
           '[WIP ERROR] :: This method should be implemented by child class.')
ans = ""
END PROCEDURE aField_GetPrefix

END SUBMODULE GetMethods
