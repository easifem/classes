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
USE Display_Method, ONLY: Display, ToString
USE RealVector_Method, ONLY: RealVector_GetPointer => GetPointer, &
                             RealVector_Get => Get, &
                             GetValue_
USE BaseType, ONLY: IntVector_
USE IntVector_Method, ONLY: IntVector_Get => Get, &
                            IntVector_DEALLOCATE => DEALLOCATE, &
                            ASSIGNMENT(=)
USE DOF_Method, ONLY: GetNodeLoc, GetNodeLoc_
USE InputUtility, ONLY: Input
USE ArangeUtility, ONLY: Arange
USE AppendUtility, ONLY: Append
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ReallocateUtility, ONLY: Reallocate

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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_size()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%tSize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
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
!                                                             GetTotalNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeLoc1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodeLoc1()"
#endif

INTEGER(I4B) :: ttime, tspace, tnode
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tspace = 1
isok = PRESENT(spaceCompo)
IF (isok) tspace = SIZE(spaceCompo)

ttime = 1
isok = PRESENT(timeCompo)
IF (isok) ttime = SIZE(timeCompo)

tnode = SIZE(globalNode)

ans = ttime * tspace * tnode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetTotalNodeLoc1

!----------------------------------------------------------------------------
!                                                           GetTotalNodeLoc2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeLoc2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc2()"
#endif

INTEGER(I4B), ALLOCATABLE :: timeCompo(:)
INTEGER(I4B) :: tPhysicalVars, ivar0
INTEGER(I4B) :: tsize, tspace, ttime, tnode
LOGICAL(LGT) :: isfedof, isfedofs, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ivar0 = Input(default=1_I4B, option=ivar)
tspace = 1

tPhysicalVars = obj%GetTotalPhysicalVars()
ALLOCATE (timeCompo(tPhysicalVars))
timeCompo = obj%GetTimeCompo(tPhysicalVars)
ttime = timeCompo(ivar0)

isfedof = ASSOCIATED(obj%fedof)
isfedofs = ALLOCATED(obj%fedofs)

#ifdef DEBUG_VER
isok = isfedof .OR. isfedofs
CALL AssertError1(isok, myName, &
                  "Neither fedof is associated nor fedofs is allocated.")

isok = .NOT. (isfedof .AND. isfedofs)
CALL AssertError1(isok, myName, &
                  "Both fedof and fedofs are allocated/associated.")
#endif

#ifdef DEBUG_VER
IF (isfedofs) THEN
  tsize = SIZE(obj%fedofs)
  isok = ivar0 .LE. tsize
  CALL AssertError1(isok, myName, &
                    'ivar='//ToString(ivar0)//' is greater than size of &
                    &obj%fedofs='//ToString(tsize))

  isok = ASSOCIATED(obj%fedofs(ivar0)%ptr)
  CALL AssertError1(isok, myName, &
                   'obj%fedofs('//ToString(ivar0)//')%ptr is not associated.')
END IF
#endif

IF (isfedofs) tnode = dbc%GetTotalNodeNum(obj%fedofs(ivar0)%ptr)
IF (isfedof) tnode = dbc%GetTotalNodeNum(obj%fedof)

ans = tnode * tspace * ttime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodeLoc2

!----------------------------------------------------------------------------
!                                                           GetTotalNodeLoc3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeLoc3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodeLoc3()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(dbc)

ans = 0

DO ii = 1, tsize
  ans = ans + obj%GetTotalNodeLoc(dbc=dbc(ii)%ptr, ivar=ivar)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodeLoc3

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc1()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalNodeLoc(globalNode=globalNode, ivar=ivar, &
                            spaceCompo=spaceCompo, timeCompo=timeCompo)

CALL Reallocate(ans, tsize)

CALL obj%GetNodeLoc_(globalNode=globalNode, ivar=ivar, &
                     spaceCompo=spaceCompo, timeCompo=timeCompo, &
                     ans=ans, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_GetNodeLoc1

!----------------------------------------------------------------------------
!                                                                 GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc_1()"
#endif

INTEGER(I4B) :: spaceCompo0(256), timeCompo0(256)
INTEGER(I4B) :: ivar0, ttime, tspace, ii, jj
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Check errors here
ivar0 = Input(default=1_I4B, option=ivar)

#ifdef DEBUG_VER
isok = PRESENT(islocal)
IF (isok) THEN
  CALL AssertError1(islocal, myName, &
                    "islocal should be .true.")
END IF
#endif

isok = PRESENT(spaceCompo)
IF (isok) THEN
  tspace = SIZE(spaceCompo)
  spaceCompo0(1:tspace) = spaceCompo(1:tspace)
ELSE
  tspace = 1
  spaceCompo0(1:tspace) = 1
END IF

isok = PRESENT(timeCompo)
IF (isok) THEN
  ttime = SIZE(timeCompo)
  timeCompo0(1:ttime) = timeCompo(1:ttime)
ELSE
  ttime = 1
  timeCompo0(1:ttime) = 1
END IF

tsize = 0
DO ii = 1, ttime
  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, &
                   ivar=ivar0, spaceCompo=spaceCompo0(1:tspace), &
                   timeCompo=timeCompo0(ii), &
                   ans=ans(tsize + 1:), tsize=jj)
  tsize = tsize + jj
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetNodeLoc_1

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc2()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalNodeLoc(dbc=dbc, ivar=ivar)
CALL Reallocate(ans, tsize)
CALL obj%GetNodeLoc_(ans=ans, tsize=tsize, dbc=dbc, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeLoc2

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc_2()"
INTEGER(I4B) :: tfedofs
#endif

INTEGER(I4B), ALLOCATABLE :: globalNode(:)
INTEGER(I4B) :: tPhysicalVars, spaceCompo(1), ivar0, tnode, timeCompo(256), &
                iNodeOnNode, iNodeOnEdge, iNodeOnFace
LOGICAL(LGT) :: isok, isfedof, isfedofs
CLASS(FEDOF_), POINTER :: fedof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ivar0 = Input(default=1_I4B, option=ivar)

tPhysicalVars = obj%GetTotalPhysicalVars()
timeCompo(1:tPhysicalVars) = obj%GetTimeCompo(tPhysicalVars)
spaceCompo(1) = dbc%GetDOFNo()

isfedof = ASSOCIATED(obj%fedof)
isfedofs = ALLOCATED(obj%fedofs)

#ifdef DEBUG_VER
isok = isfedof .OR. isfedofs
CALL AssertError1(isok, myName, &
                  "Neither fedof is associated nor fedofs is allocated.")
isok = .NOT. (isfedof .AND. isfedofs)
CALL AssertError1(isok, myName, &
                  "Both fedof and fedofs are allocated/associated.")
#endif

#ifdef DEBUG_VER
IF (isfedofs) THEN
  tfedofs = SIZE(obj%fedofs)
  isok = ivar0 .LE. tfedofs
  CALL AssertError1(isok, myName, &
                    'ivar='//ToString(ivar0)//' is greater than size of &
                    &obj%fedofs='//ToString(tfedofs))

  isok = ASSOCIATED(obj%fedofs(ivar0)%ptr)
  CALL AssertError1(isok, myName, &
                   'obj%fedofs('//ToString(ivar0)//')%ptr is not associated.')

END IF
#endif

IF (isfedofs) fedof => obj%fedofs(ivar0)%ptr
IF (isfedof) fedof => obj%fedof

tnode = dbc%GetTotalNodeNum(fedof)
ALLOCATE (globalNode(tnode))
CALL dbc%Get(nodeNum=globalNode, tsize=tnode, fedof=fedof, &
    iNodeOnNode=iNodeOnNode, iNodeOnEdge=iNodeOnEdge, iNodeOnFace=iNodeOnFace)

CALL obj%GetNodeLoc_(globalNode=globalNode, ans=ans, tsize=tsize, &
                     ivar=ivar0, spaceCompo=spaceCompo, &
                     timeCompo=Arange(1_I4B, timeCompo(ivar0)))

IF (ALLOCATED(globalNode)) DEALLOCATE (globalNode)
fedof => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeLoc_2

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc3()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalNodeLoc(dbc=dbc, ivar=ivar)
CALL Reallocate(ans, tsize)
CALL obj%GetNodeLoc_(ans=ans, tsize=tsize, dbc=dbc, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeLoc3

!----------------------------------------------------------------------------
!                                                                GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc_3()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, tdbc, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tdbc = SIZE(dbc)

#ifdef DEBUG_VER
DO ii = 1, tdbc
  isok = ASSOCIATED(dbc(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'dbc('//ToString(ii)//')%ptr is not associated.')
END DO
#endif

tsize = 0
DO ii = 1, tdbc
  CALL obj%GetNodeLoc_(dbc=dbc(ii)%ptr, ivar=ivar, ans=ans(tsize + 1:), &
                       tsize=jj)
  tsize = tsize + jj
END DO
!! Here we are calling GetNodeLoc_2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeLoc_3

!----------------------------------------------------------------------------
!                                                    GetTotalDirichletBCIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDirichletBCIndex
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDirichletBCIndex()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%dbc)
ans = 0
IF (isok) ans = obj%GetTotalNodeLoc(dbc=obj%dbc, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDirichletBCIndex

!----------------------------------------------------------------------------
!                                                         GetDirichletBCIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCIndex
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCIndex()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalDirichletBCIndex(ivar=ivar)
CALL Reallocate(ans, tsize)
CALL obj%GetDirichletBCIndex_(ans=ans, tsize=tsize, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetDirichletBCIndex

!----------------------------------------------------------------------------
!                                                       GetDirichletBCIndex_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCIndex_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCIndex_()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%dbc)
tsize = 0
IF (isok) CALL obj%GetNodeLoc_(dbc=obj%dbc, ivar=ivar, ans=ans, tsize=tsize)
!! Here we are calling GetNodeLoc_3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetDirichletBCIndex_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
