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

SUBMODULE(AbstractField_Class) ConstructorMethods
USE GlobalData, ONLY: TypeIntI4B
USE Display_Method, ONLY: ToString
USE InputUtility, ONLY: Input
USE FPL_Method, ONLY: CheckEssentialParam, &
                      FPL_Set => Set, &
                      FPL_GetValue => GetValue
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldCheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "AbstractFieldCheckEssentialParam()"
TYPE(String) :: astr
TYPE(String), ALLOCATABLE :: essentialParam(:)
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

astr = "/name/engine/fieldType/comm/local_n/global_n"
CALL astr%Split(essentialParam, sep="/")
CALL CheckEssentialParam(obj=param, keys=essentialParam, prefix=prefix, &
                         myName=myName, modName=modName)

astr = ""
isok = ALLOCATED(essentialParam)
IF (.NOT. isok) RETURN

DO ii = 1, SIZE(essentialParam)
  essentialParam(ii) = ""
END DO
DEALLOCATE (essentialParam)
END PROCEDURE AbstractFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                       SetScalarFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractFieldParam
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr
CHARACTER(*), PARAMETER :: myName = "SetAbstractFieldParam()"
LOGICAL(LGT) :: isSublist, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

sublist => NULL()

! Create a new sublist
isSublist = param%IsSubList(prefix)

IF (isSublist) THEN
  ierr = param%GetSubList(key=prefix, sublist=sublist)
  isok = ierr .EQ. 0_I4B
  CALL AssertError1(isok, myName, &
                    'Error occured in getting sublist(1)')
END IF

IF (.NOT. isSublist) THEN
  sublist => param%NewSubList(key=prefix)
END IF

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'Error occured in getting sublist(2)')

CALL FPL_Set(obj=sublist, datatype="Char", prefix=prefix, key="name", &
             VALUE=name)

CALL FPL_Set(obj=sublist, datatype="Char", prefix=prefix, key="engine", &
             VALUE=engine)

CALL FPL_Set(obj=sublist, datatype=TypeIntI4B, prefix=prefix, key="fieldType", &
             VALUE=input(option=fieldType, default=TypeField%normal))

CALL FPL_Set(obj=sublist, datatype=TypeIntI4B, prefix=prefix, key="comm", &
             VALUE=input(option=comm, default=0_I4B))

CALL FPL_Set(obj=sublist, datatype=TypeIntI4B, prefix=prefix, key="local_n", &
             VALUE=input(option=local_n, default=0_I4B))

CALL FPL_Set(obj=sublist, datatype=TypeIntI4B, prefix=prefix, key="global_n", &
             VALUE=input(option=global_n, default=0_I4B))

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetAbstractFieldParam

!----------------------------------------------------------------------------
!                                               AbstractFieldInitiate_Help1
!----------------------------------------------------------------------------

SUBROUTINE AbstractFieldInitiate_Help1(obj, param, prefix)
  CLASS(AbstractField_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix

  obj%isInitiated = .TRUE.
  CALL FPL_GetValue(obj=param, prefix=prefix, key="fieldType", &
                    VALUE=obj%fieldType)
  CALL FPL_GetValue(obj=param, prefix=prefix, key="name", &
                    VALUE=obj%name)
  CALL FPL_GetValue(obj=param, prefix=prefix, key="engine", VALUE=obj%engine)
  CALL FPL_GetValue(obj=param, prefix=prefix, key="comm", VALUE=obj%comm)
  CALL FPL_GetValue(obj=param, prefix=prefix, key="global_n", &
                    VALUE=obj%global_n)
  CALL FPL_GetValue(obj=param, prefix=prefix, key="local_n", &
                    VALUE=obj%local_n)
END SUBROUTINE AbstractFieldInitiate_Help1

!----------------------------------------------------------------------------
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr
CHARACTER(:), ALLOCATABLE :: prefix
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

prefix = obj%GetPrefix()

! main
sublist => NULL()
ierr = param%GetSubList(key=prefix, sublist=sublist)
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  'Error occured in getting sublist(1)')

! NOTE: We should not call deallocate in abstract classes.
! This is because, in concrete classes we may set some
! parameters before calling this method.
! All those parameters will be gone if we call deallocate
! here.
! CALL obj%DEALLOCATE()

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'Error occured in getting sublist(2)')

CALL AbstractFieldInitiate_Help1(obj, sublist, prefix)

obj%fedof => fedof
obj%timefedof => timefedof

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

IF (.NOT. obj2%isInitiated .OR. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: Either obj is already initiated or '// &
                    ' obj2 is not initiated.')
  RETURN
END IF

obj%isInitiated = obj2%isInitiated
obj%fieldType = obj2%fieldType
obj%name = obj2%name
obj%engine = obj2%engine
obj%comm = obj2%comm
obj%myRank = obj2%myRank
obj%numProcs = obj2%numProcs
obj%global_n = obj2%global_n
obj%local_n = obj2%local_n
obj%is = obj2%is
obj%ie = obj2%ie
obj%lis_ptr = obj2%lis_ptr

obj%fedof => obj2%fedof

IF (ALLOCATED(obj2%fedofs)) THEN

  tsize = SIZE(obj2%fedofs)
  ALLOCATE (obj%fedofs(tsize))

  DO ii = 1, tsize
    obj%fedofs(ii)%ptr => obj2%fedofs(ii)%ptr
  END DO

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr, ii, tsize
LOGICAL(LGT) :: isOK
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

prefix = obj%GetPrefix()

! main
sublist => NULL()
ierr = param%GetSubList(key=prefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
  RETURN
END IF

! NOTE: We should not call deallocate in abstract classes.
! This is because, in concrete classes we may set some
! parameters before calling this method.
! All those parameters will be gone if we call deallocate
! here.
! CALL obj%DEALLOCATE()

isok = ASSOCIATED(sublist)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
  RETURN
END IF

CALL AbstractFieldInitiate_Help1(obj, sublist, prefix)

tsize = SIZE(fedof)
ALLOCATE (obj%fedofs(tsize))
DO ii = 1, tsize
  isOK = ASSOCIATED(fedof(ii)%ptr)
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
           '[INTERNAL ERROR] :: fedof('//ToString(ii)//') is not ASSOCIATED.')
    RETURN
  END IF
  obj%fedofs(ii)%ptr => fedof(ii)%ptr
END DO

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ii
obj%name = ""
obj%engine = ""
obj%isInitiated = .FALSE.
obj%fieldType = TypeField%normal
obj%comm = 0
obj%myRank = 0
obj%numProcs = 1
obj%global_n = 0
obj%local_n = 0
obj%is = 0
obj%ie = 0
obj%lis_ptr = 0

IF (ASSOCIATED(obj%fedof)) obj%fedof => NULL()

IF (ALLOCATED(obj%fedofs)) THEN

  DO ii = 1, SIZE(obj%fedofs)

    IF (ASSOCIATED(obj%fedofs(ii)%ptr)) obj%fedofs(ii)%ptr => NULL()

  END DO

  DEALLOCATE (obj%fedofs)

END IF

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
