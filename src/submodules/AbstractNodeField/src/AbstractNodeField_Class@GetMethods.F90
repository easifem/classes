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
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AppendUtility, ONLY: Append
USE ArangeUtility, ONLY: Arange
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: math => TypeMathOpt
USE DOF_Method, ONLY: GetNodeLoc_
USE Display_Method, ONLY: ToString
USE InputUtility, ONLY: Input
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: IntVector_DEALLOCATE => DEALLOCATE
USE IntVector_Method, ONLY: IntVector_Get => Get
USE RealVector_Method, ONLY: GetValue_
USE RealVector_Method, ONLY: RealVector_Get => Get
USE RealVector_Method, ONLY: RealVector_GetPointer => GetPointer
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             GetStorageFMT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStorageFMT
ans = MYSTORAGEFORMAT
END PROCEDURE obj_GetStorageFMT

!----------------------------------------------------------------------------
!                                                       GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEVariable()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[WIP ERROR] :: This routine should be implemented by child class.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                         GetPhysicalNames
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPhysicalNames
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPhysicalNames()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tnames
INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dof_names_char)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_names_char is not allocated.')
#endif

tnames = SIZE(obj%dof_names_char)

#ifdef DEBUG_VER
aint = SIZE(ans)
isok = tnames .EQ. aint
CALL AssertError1(isok, myName, &
                  'The size of ans ('//ToString(aint)// &
                  ') is not same as total physical variables = '// &
                  ToString(tnames))
#endif

DO aint = 1, tnames
  ans(aint) = obj%dof_names_char(aint)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPhysicalNames

!----------------------------------------------------------------------------
!                                                       GetTotalPhysicalVars
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalPhysicalVars
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalPhysicalVars()"
INTEGER(I4B) :: tnames
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dof_names_char)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_names_char is not allocated.')
#endif

aint = obj%dof_tPhysicalVars

#ifdef DEBUG_VER
tnames = SIZE(obj%dof_names_char)
isok = tnames .EQ. aint
CALL AssertError1( &
  isok, myName, 'The size of names ('//ToString(tnames)// &
  ') is not same as total physical variables = '//ToString(aint))
#endif

ans = aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalPhysicalVars

!----------------------------------------------------------------------------
!                                                           GetSpaceCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpaceCompo
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalPhysicalVars()"
INTEGER(I4B) :: tnames
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dof_spaceCompo)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%spaceCompo not allocated.')
#endif

aint = tPhysicalVars

#ifdef DEBUG_VER
tnames = SIZE(obj%dof_spaceCompo)
isok = tnames .EQ. aint
CALL AssertError1( &
  isok, myName, 'The size of spaceCompo ('//ToString(tnames)// &
  ') is not same as total physical variables = '//ToString(aint))
#endif

ans(1:aint) = obj%dof_spaceCompo(1:aint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSpaceCompo

!----------------------------------------------------------------------------
!                                                           GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeCompo
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeCompo()"
INTEGER(I4B) :: tnames
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dof_timeCompo)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%timeCompo not allocated.')
#endif

aint = tPhysicalVars

#ifdef DEBUG_VER
tnames = SIZE(obj%dof_timeCompo)
isok = tnames .EQ. aint
CALL AssertError1( &
  isok, myName, 'The size of timeCompo ('//ToString(tnames)// &
  ') is not same as total physical variables = '//ToString(aint))
#endif

ans(1:aint) = obj%dof_timeCompo(1:aint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTimeCompo

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => RealVector_GetPointer(obj%realVec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                     Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Size()"
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
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSingle()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

VALUE = RealVector_Get(obj=obj%realVec, nodenum=indx, dataType=math%one)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSingle

!----------------------------------------------------------------------------
!                                                                 GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue_(obj=obj%realVec, nodenum=indx, VALUE=VALUE, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMultiple1

!----------------------------------------------------------------------------
!                                                                 GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue_(obj=obj%realVec, istart=istart, iend=iend, stride=stride, &
               VALUE=VALUE, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMultiple2

!----------------------------------------------------------------------------
!                                                                 GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue_( &
  obj=obj%realVec, istart=istart, iend=iend, stride=stride, VALUE=VALUE, &
  tsize=tsize, istart_value=istart_value, iend_value=iend_value, &
  stride_value=stride_value)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMultiple3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
