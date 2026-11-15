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

SUBMODULE(ScalarFieldLis_Class) GetMethods
USE BaseType, ONLY: math => TypeMathOpt
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#include "lisf.h"

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "ScalarFieldLis_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSingle()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSingle

!----------------------------------------------------------------------------
!                                                                GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple1()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(indx)

CALL lis_vector_get_values_from_index(obj%lis_ptr, tsize, indx, VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMultiple1

!----------------------------------------------------------------------------
!                                                                GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple2()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = (iend - istart) / stride + 1
CALL lis_vector_get_values_from_range(obj%lis_ptr, istart, stride, tsize, &
                                      VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMultiple2

!----------------------------------------------------------------------------
!                                                                GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple3()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = (iend - istart) / stride + 1

CALL lis_vector_get_values_from_range2( &
  obj%lis_ptr, istart, stride, tsize, VALUE, istart_value, stride_value, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMultiple3

!----------------------------------------------------------------------------
!                                                               GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple4()"
#endif
INTEGER(I4B) :: ierr, jj, tsize
LOGICAL(LGT) :: isAdd, isScale
REAL(DFP) :: areal
REAL(DFP), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isAdd = Input(option=addContribution, default=math%no)
isScale = PRESENT(scale)
areal = Input(option=scale, default=math%one)
tsize = (iend - istart) / stride + 1
jj = istart_value + (tsize - 1) * stride_value

IF (.NOT. isAdd) THEN

  ! plain copy: LIS writes in the range of VALUE
  CALL lis_vector_get_values_from_range2( &
    obj%lis_ptr, istart, stride, tsize, VALUE, istart_value, stride_value, &
    ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  IF (isScale) THEN
    VALUE(istart_value:jj:stride_value) = &
      areal * VALUE(istart_value:jj:stride_value)
  END IF

ELSE

  ! add: copy the values first
  ! ALLOCATE (temp(tsize))
  CALL Reallocate(temp, tsize)
  CALL lis_vector_get_values_from_range2( &
    obj%lis_ptr, istart, stride, tsize, temp, 1, 1, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  VALUE(istart_value:jj:stride_value) = &
    VALUE(istart_value:jj:stride_value) + areal * temp(1:tsize)
  IF (ALLOCATED(temp)) DEALLOCATE (temp)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMultiple4

!----------------------------------------------------------------------------
!                                                               GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => NULL()

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[INTERNAL ERROR] :: This method is not available for ScalarFieldLis_')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = obj%local_n
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
