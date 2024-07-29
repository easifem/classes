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

SUBMODULE(AbstractNodeField_Class) GetMethods
#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

USE RealVector_Method, ONLY: RealVector_GetPointer => GetPointer, &
                             RealVector_Get => Get, &
                             GetValue_

USE BaseType, ONLY: IntVector_

USE IntVector_Method, ONLY: IntVector_Get => Get, &
                            IntVector_DEALLOCATE => DEALLOCATE, &
                            ASSIGNMENT(=)

USE Display_Method, ONLY: ToString

USE AbstractField_Class, ONLY: FIELD_TYPE_CONSTANT

USE DOF_Method, ONLY: GetNodeLoc

USE InputUtility, ONLY: Input

USE ArangeUtility, ONLY: Arange

USE AppendUtility, ONLY: Append

USE AbstractMesh_Class, ONLY: AbstractMesh_

#ifdef USE_LIS
#include "lisf.h"
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CHARACTER(*), PARAMETER :: myName = "obj_GetFEVariable()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                         GetPhysicalNames
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPhysicalNames
INTEGER(I4B) :: tnames
INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPhysicalNames()"
LOGICAL(LGT) :: problem
#endif

#ifdef DEBUG_VER
problem = .NOT. ALLOCATED(obj%dof_names_char)
IF (problem) THEN

  CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_names_char not'// &
                    ' not allocated.')
  RETURN
END IF
#endif

tnames = SIZE(obj%dof_names_char)

#ifdef DEBUG_VER
aint = SIZE(ans)
problem = tnames .NE. aint
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: The size of names (' &
                    //ToString(aint)// &
                    ') is not same as total physical variables = ' &
                    //ToString(tnames))
  RETURN
END IF
#endif

DO aint = 1, tnames
  ans(aint) = obj%dof_names_char(aint)
END DO

END PROCEDURE obj_GetPhysicalNames

!----------------------------------------------------------------------------
!                                                       GetTotalPhysicalVars
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalPhysicalVars
INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalPhysicalVars()"
INTEGER(I4B) :: tnames
LOGICAL(LGT) :: problem
#endif

ans = 0

#ifdef DEBUG_VER
problem = .NOT. ALLOCATED(obj%dof_names_char)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_names_char not'// &
                    ' not allocated.')
  RETURN
END IF
#endif

aint = obj%dof_tPhysicalVars

#ifdef DEBUG_VER
tnames = SIZE(obj%dof_names_char)
problem = tnames .NE. aint
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: The size of names ('//ToString(tnames)// &
               ') is not same as total physical variables = '//ToString(aint))
  RETURN
END IF
#endif

ans = aint

END PROCEDURE obj_GetTotalPhysicalVars

!----------------------------------------------------------------------------
!                                                           GetSpaceCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpaceCompo
INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalPhysicalVars()"
INTEGER(I4B) :: tnames
LOGICAL(LGT) :: problem
#endif

#ifdef DEBUG_VER
problem = .NOT. ALLOCATED(obj%dof_spaceCompo)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: AbstractNodeField_::obj%spaceCompo not'// &
                    ' not allocated.')
  RETURN
END IF
#endif

aint = tPhysicalVars

#ifdef DEBUG_VER
tnames = SIZE(obj%dof_spaceCompo)

problem = tnames .NE. aint

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[INTERNAL ERROR] :: The size of spaceCompo ('//ToString(tnames)// &
               ') is not same as total physical variables = '//ToString(aint))
  RETURN
END IF
#endif

ans(1:aint) = obj%dof_spaceCompo(1:aint)
END PROCEDURE obj_GetSpaceCompo

!----------------------------------------------------------------------------
!                                                           GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeCompo
INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeCompo()"
INTEGER(I4B) :: tnames
LOGICAL(LGT) :: problem
#endif

#ifdef DEBUG_VER
problem = .NOT. ALLOCATED(obj%dof_timeCompo)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
       '[INTERNAL ERROR] :: AbstractNodeField_::obj%timeCompo not allocated.')
  RETURN
END IF
#endif

aint = tPhysicalVars

#ifdef DEBUG_VER
tnames = SIZE(obj%dof_timeCompo)
problem = tnames .NE. aint

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: The size of timeCompo (' &
                    //ToString(tnames)// &
                    ') is not same as total physical variables = ' &
                    //ToString(aint))
  RETURN
END IF
#endif

ans(1:aint) = obj%dof_timeCompo(1:aint)

END PROCEDURE obj_GetTimeCompo

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
ans => RealVector_GetPointer(obj%realVec)
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                     Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_size
CHARACTER(*), PARAMETER :: myName = "obj_size()"

ans = obj%tSize
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
END PROCEDURE obj_size

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
#ifdef USE_LIS
INTEGER(I4B) :: ierr
#endif

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  VALUE = RealVector_Get(obj=obj%realVec, nodenum=indx, dataType=1.0_DFP)
  RETURN
END IF

#ifdef USE_LIS

CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

RETURN

#endif

END PROCEDURE obj_GetSingle

!----------------------------------------------------------------------------
!                                                                 GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple1
#ifdef USE_LIS
INTEGER(I4B) :: ierr
#endif

! NATIVE_SERIAL
IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  CALL GetValue_(obj=obj%realVec, nodenum=indx, VALUE=VALUE, tsize=tsize)
  RETURN
END IF
! end of NATIVE_SERIAL

! USE_LIS
#ifdef USE_LIS
tsize = SIZE(indx)

CALL lis_vector_get_values_from_index(obj%lis_ptr, tsize, indx, VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

RETURN

#endif
!end USE_LIS

END PROCEDURE obj_GetMultiple1

!----------------------------------------------------------------------------
!                                                                 GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple2
#ifdef USE_LIS
INTEGER(I4B) :: ierr
#endif

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  CALL GetValue_(obj=obj%realVec, istart=istart, iend=iend, stride=stride, &
                 VALUE=VALUE, tsize=tsize)
  RETURN
END IF

! USE_LIS

#ifdef USE_LIS
tsize = (iend - istart) / stride + 1
CALL lis_vector_get_values_from_range(obj%lis_ptr, istart, stride, tsize, &
                                      VALUE, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

RETURN

#endif
! end of USE_LIS

END PROCEDURE obj_GetMultiple2

!----------------------------------------------------------------------------
!                                                                 GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple3
#ifdef USE_LIS
INTEGER(I4B) :: ierr
#endif

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  CALL GetValue_(obj=obj%realVec, istart=istart, iend=iend, stride=stride, &
                 VALUE=VALUE, tsize=tsize, istart_value=istart_value, &
                 iend_value=iend_value, stride_value=stride_value)
  RETURN
END IF

! USE_LIS
#ifdef USE_LIS

tsize = (iend - istart) / stride + 1

CALL lis_vector_get_values_from_range2(obj%lis_ptr, istart, stride, tsize, &
                                      VALUE, istart_value, stride_value, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

RETURN

#endif
! end of USE_LIS

END PROCEDURE obj_GetMultiple3

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc1
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc1()"
INTEGER(I4B), ALLOCATABLE :: spaceCompo0(:), timeCompo0(:), localNode(:)
INTEGER(I4B) :: ivar0, tsize, itime, ttime
TYPE(IntVector_), ALLOCATABLE :: int_vec_list(:)
CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(spaceCompo)) THEN
  tsize = SIZE(spaceCompo)
  ALLOCATE (spaceCompo0(tsize))
  spaceCompo0(1:tsize) = spaceCompo(1:tsize)
ELSE
  ALLOCATE (spaceCompo0(1))
  spaceCompo0(1) = 1
END IF

IF (PRESENT(timeCompo)) THEN
  tsize = SIZE(timeCompo)
  ALLOCATE (timeCompo0(tsize))
  timeCompo0(1:tsize) = timeCompo(1:tsize)
ELSE
  ALLOCATE (timeCompo0(1))
  timeCompo0(1) = 1
END IF
ttime = SIZE(timeCompo0)

IF (PRESENT(ivar)) THEN
  ivar0 = ivar
ELSE
  ivar0 = 1
END IF

ALLOCATE (int_vec_list(ttime))
tsize = SIZE(globalNode)
ALLOCATE (localNode(tsize))

IF (ASSOCIATED(obj%fedof)) THEN

  mesh => obj%fedof%GetMeshPointer()
  localNode = mesh%GetLocalNodeNumber(globalNode=globalNode)

ELSEIF (ALLOCATED(obj%fedofs)) THEN

  tsize = SIZE(obj%fedofs)

  IF (ivar0 .GT. tsize) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: ivar is greater than size of '// &
                      ' AbstractNodeField_::obj%fedofs.')
    RETURN
  END IF

  mesh => obj%fedofs(ivar0)%ptr%GetMeshPointer()
  localNode = mesh%GetLocalNodeNumber(globalNode)

ELSE

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: AbstractNodeField_::obj%fedof'// &
                    ' obj%fedofs are not allocated.')
  RETURN

END IF

DO itime = 1, ttime
  int_vec_list(itime) = GetNodeLoc(obj=obj%dof, nodenum=localNode, &
              ivar=ivar0, spaceCompo=spaceCompo0, timeCompo=timeCompo0(itime))
END DO

ans = IntVector_Get(obj=int_vec_list, dataType=1_I4B)

IF (ALLOCATED(int_vec_list)) THEN
  DO itime = 1, ttime
    CALL IntVector_DEALLOCATE(int_vec_list(itime))
  END DO
  DEALLOCATE (int_vec_list)
END IF

IF (ALLOCATED(spaceCompo0)) DEALLOCATE (spaceCompo0)
IF (ALLOCATED(timeCompo0)) DEALLOCATE (timeCompo0)
IF (ALLOCATED(localNode)) DEALLOCATE (localNode)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_GetNodeLoc1

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc2
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc2()"
INTEGER(I4B), ALLOCATABLE :: globalNode(:), timeCompo(:)
INTEGER(I4B) :: tPhysicalVars, spaceCompo(1), ivar0
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tPhysicalVars = obj%GetTotalPhysicalVars()

ALLOCATE (timeCompo(tPhysicalVars))

timeCompo = obj%GetTimeCompo(tPhysicalVars)

spaceCompo(1) = dbc%GetDOFNo()

ivar0 = Input(default=1_I4B, option=ivar)

tsize = dbc%GetTotalNodeNum(obj%fedof)
ALLOCATE (globalNode(tsize))

CALL dbc%Get(nodeNum=globalNode, tsize=tsize, fedof=obj%fedof)
ans = obj%GetNodeLoc(globalNode=globalNode, ivar=ivar, &
                     spaceCompo=spaceCompo, &
                     timeCompo=Arange(1_I4B, timeCompo(ivar0)))

IF (ALLOCATED(globalNode)) DEALLOCATE (globalNode)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeLoc2

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc3
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc3()"
INTEGER(I4B) :: ii, tsize
INTEGER(I4B), ALLOCATABLE :: nptrs(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(dbc)

DO ii = 1, tsize
  nptrs = obj%GetNodeLoc(dbc=dbc(ii)%ptr, ivar=ivar)
  CALL Append(ans, nptrs)
END DO

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeLoc3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
