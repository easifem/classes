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

SUBMODULE(AbstractField_Class) SetMethods
USE GlobalData, ONLY: CHAR_LF
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
!USE AbstractNodeField_Class, ONLY: AbstractNodeField_
!USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_

CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
INTEGER(I4B) :: ii

IF (PRESENT(isInitiated)) obj%isInitiated = isInitiated
IF (PRESENT(fieldType)) obj%fieldType = fieldType
IF (PRESENT(name)) obj%name = TRIM(name)
IF (PRESENT(engine)) obj%engine = TRIM(engine)
IF (PRESENT(comm)) obj%comm = comm
IF (PRESENT(myRank)) obj%myRank = myRank
IF (PRESENT(numProcs)) obj%numProcs = numProcs
IF (PRESENT(global_n)) obj%global_n = global_n
IF (PRESENT(local_n)) obj%local_n = local_n
IF (PRESENT(is)) obj%is = is
IF (PRESENT(ie)) obj%ie = ie
IF (PRESENT(lis_ptr)) obj%lis_ptr = lis_ptr

IF (PRESENT(domain)) obj%domain => domain
IF (PRESENT(fedof)) obj%fedof = fedof

IF (PRESENT(domains)) THEN
  IF (.NOT. ALLOCATED(obj%domains)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[CONFIG ERROR] :: AbstractField_::Obj%domains is not allocated ')
  END IF

  IF (SIZE(obj%domains) .NE. SIZE(domains)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: AbstractField_::Obj%domains '// &
                      CHAR_LF//'size is not same as size of domains')
  END IF

  DO ii = 1, SIZE(domains)
    obj%domains(ii)%ptr => domains(ii)%ptr
  END DO
END IF

IF (PRESENT(fedofs)) THEN
  IF (.NOT. ALLOCATED(obj%fedofs)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[CONFIG ERROR] :: AbstractField_::Obj%fedofs is not allocated ')
  END IF

  IF (SIZE(obj%fedofs) .NE. SIZE(fedofs)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: AbstractField_::Obj%fedofs '// &
                      CHAR_LF//'size is not same as size of fedofs')
  END IF

  DO ii = 1, SIZE(fedofs)
    obj%fedofs(ii)%ptr => fedofs(ii)%ptr
  END DO
END IF

!
!SELECT TYPE (obj)
!CLASS IS (AbstractNodeField_)
!  IF (PRESENT(tSize)) obj%tSize = tSize
!  IF (PRESENT(realVec)) obj%realVec = realVec
!  IF (PRESENT(dof)) obj%dof = dof
!CLASS IS (AbstractMatrixField_)
!  IF (PRESENT(isPMatInitiated)) obj%isPMatInitiated = isPMatInitiated
!END SELECT
END PROCEDURE obj_SetParam

END SUBMODULE SetMethods
