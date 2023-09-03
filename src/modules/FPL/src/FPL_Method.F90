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
! summary: This module contains extra method for Fortran Parameter Lists

MODULE FPL_Method
USE GlobalData
USE BaSetype
USE BaseMethod
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
PRIVATE
CHARACTER(*), PARAMETER :: modName = "FPL_Method"
!! TYPE(ExceptionHandler_) :: e
PUBLIC :: Set
PUBLIC :: getValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Set
  MODULE PROCEDURE fpl_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE getValue
  MODULE PROCEDURE fpl_getValue1
  MODULE PROCEDURE fpl_getValue2
END INTERFACE getValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                 fpl_Set1
!----------------------------------------------------------------------------

SUBROUTINE fpl_Set1(obj, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: key
  TYPE(DOF_), INTENT(IN) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  ierr = obj%Set(key=TRIM(key)//"/map", VALUE=VALUE%map)
  ierr = obj%Set(key=TRIM(key)//"/valmap", VALUE=VALUE%valmap)
  ierr = obj%Set(key=TRIM(key)//"/storageFMT", VALUE=VALUE%storageFMT)
END SUBROUTINE fpl_Set1

!----------------------------------------------------------------------------
!                                                                 fpl_get
!----------------------------------------------------------------------------

SUBROUTINE fpl_getValue1(obj, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: key
  TYPE(DOF_), INTENT(INOUT) :: VALUE
  ! Internal variable
  INTEGER(I4B) :: ierr
  INTEGER(I4B), ALLOCATABLE :: s(:)
  ierr = obj%getShape(key=TRIM(key)//"/map", shape=s)
  CALL Reallocate(VALUE%map, s(1), s(2))
  ierr = obj%getShape(key=TRIM(key)//"/valmap", shape=s)
  CALL Reallocate(VALUE%valmap, s(1))
  ierr = obj%get(key=TRIM(key)//"/map", VALUE=VALUE%map)
  ierr = obj%get(key=TRIM(key)//"/valmap", VALUE=VALUE%valmap)
  ierr = obj%get(key=TRIM(key)//"/storageFMT", VALUE=VALUE%storageFMT)
  DEALLOCATE (s)
END SUBROUTINE fpl_getValue1

!----------------------------------------------------------------------------
!                                                                  getValue
!----------------------------------------------------------------------------

SUBROUTINE fpl_getValue2(obj, key, VALUE)
  ! Define dummy variables
  TYPE(ParameterList_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: key
  TYPE(String), INTENT(INOUT) :: VALUE
  ! Internal variable
  CHARACTER(:), ALLOCATABLE :: char_var
  INTEGER(I4B) :: ierr
  CHARACTER(*), PARAMETER :: myName = "fpl_getValue2()"

  IF (obj%isPresent(key=key)) THEN
    ALLOCATE (CHARACTER( &
      & obj%DataSizeInBytes(key=key)) :: char_var)
    ierr = obj%get(key=key, VALUE=char_var)
    IF (ALLOCATED(char_var)) THEN
      VALUE = char_var
      DEALLOCATE (char_var)
    ELSE
      VALUE = ""
    END IF
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & key//' not found in obj')
  END IF
END SUBROUTINE fpl_getValue2

END MODULE FPL_Method
