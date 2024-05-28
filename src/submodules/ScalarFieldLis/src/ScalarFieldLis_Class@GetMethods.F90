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
USE AbstractField_Class, ONLY: TypeField
USE BaseType, ONLY: TypeFEVariableScalar, TypeFEVariableSpace

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                             GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
INTEGER(I4B) :: ierr

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE, ierr)
ELSE
  CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE, ierr)
END IF

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_GetSingle

!----------------------------------------------------------------------------
!                                                               GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetPointer()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
       '[INTERNAL ERROR] :: This method is not available for ScalarFieldLis_')
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
INTEGER(I4B) :: localnode
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: problem
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"

CALL lis_vector_is_null(obj%lis_ptr, ierr)
problem = .NOT. obj%isInitiated .OR. (ierr .EQ. LIS_TRUE)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
  RETURN
END IF

#endif

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE, ierr)
#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif
  RETURN
END IF

localnode = obj%fedof%mesh%GetLocalNodeNumber(globalNode=globalNode, &
                                              islocal=islocal)

#ifdef DEBUG_VER

tsize = obj%SIZE()
problem = localnode .EQ. 0_I4B .OR. localNode .GT. tsize
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
      '[INTERNAL ERROR] :: localnode is either 0 or greater than size of '// &
                    " scalarFieldLis_::obj")
  RETURN
END IF

#endif

CALL lis_vector_get_value(obj%lis_ptr, localnode, VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

#endif

tsize = obj%SIZE()

IF (obj%fieldType .EQ. TypeField%constant) THEN

  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE(1), ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  DO CONCURRENT(ii=2:tsize); VALUE(ii) = VALUE(1); END DO

  RETURN
END IF

CALL lis_vector_gather(obj%lis_ptr, VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
INTEGER(I4B) :: localnode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

#endif

tsize = SIZE(globalNode)

IF (obj%fieldType .EQ. TypeField%constant) THEN

  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE(1), ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  DO CONCURRENT(ii=2:tsize); VALUE(ii) = VALUE(1); END DO

  RETURN

END IF

localnode = obj%fedof%mesh%GetLocalNodeNumber(globalNode)

DO ii = 1, tsize
  CALL lis_vector_get_value(obj%lis_ptr, localnode(ii), VALUE(ii), ierr)
END DO

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
INTEGER(I4B) :: size1, size2

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
           '[INTERNAL ERROR] :: Either ScalarField_::obj is not initiated'// &
                    " or, obj%lis_ptr is not available")
  RETURN
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] ScalarFieldLis_::value is not initiated')
  RETURN
END IF

size1 = obj%SIZE()
size2 = VALUE%SIZE()

IF (size1 .NE. size2) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'Size of obj and value are not same')
END IF

#endif

realvec => NULL()

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()
  CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
  realvec => NULL()

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

TYPE IS (ScalarFieldLis_)

  CALL lis_vector_copy(obj%lis_ptr, VALUE%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for type value')
  RETURN
END SELECT

END PROCEDURE obj_Get6

END SUBMODULE GetMethods
