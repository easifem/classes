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

SUBMODULE(AbstractNodeField_Class) Methods
USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_getPointer
ans => getPointer(obj%realVec)
END PROCEDURE anf_getPointer

!----------------------------------------------------------------------------
!                                                                    Size
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_size
ans = obj%tSize
END PROCEDURE anf_size

!----------------------------------------------------------------------------
!                                                                 Initiate2
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_initiate2
CHARACTER(*), PARAMETER :: myName = "anf_initiate2"
INTEGER(I4B) :: ii, tsize
!
IF (.NOT. obj2%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Obj2 is not initiated!')
!
IF (.NOT. obj%isInitiated) THEN
  !
  obj%isInitiated = .TRUE.
  obj%fieldType = obj2%fieldType
  obj%domain => obj2%domain
  obj%name = obj2%name
  obj%engine = obj2%engine
  !
  IF (ALLOCATED(obj2%domains)) THEN
    tsize = SIZE(obj2%domains)
    ALLOCATE (obj%domains(tsize))
    DO ii = 1, tsize
      obj%domains(ii)%ptr => obj2%domains(ii)%ptr
    END DO
  END IF
  !
  SELECT TYPE (obj2)
  CLASS IS (AbstractNodeField_)
    obj%tSize = obj2%tSize
    obj%realVec = obj2%realVec
    obj%dof = obj2%dof
  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Obj2 is not a child of AbstractNodeField_!')
  END SELECT
  !
ELSE
  !
  SELECT TYPE (obj2)
  CLASS IS (AbstractNodeField_)
    obj%realVec = obj2%realVec
  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Obj2 is not a child of AbstractNodeField_!')
  END SELECT
  !
END IF

END PROCEDURE anf_initiate2

!----------------------------------------------------------------------------
!                                                            anf_Initiate3
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_initiate3
CHARACTER(*), PARAMETER :: myName = "anf_Initiate3"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Initiate3 should be implemented by the child of AbstractNodeField_')
END PROCEDURE anf_initiate3

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Deallocate
CALL AbstractFieldDeallocate(obj)
obj%tSize = 0
CALL DEALLOCATE (obj%realVec)
CALL DEALLOCATE (obj%dof)
END PROCEDURE anf_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Norm2
ans = NORM2(obj=obj%realvec)
END PROCEDURE anf_Norm2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
