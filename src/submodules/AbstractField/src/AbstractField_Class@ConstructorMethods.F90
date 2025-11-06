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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetAbstractFieldParam()"
#endif

TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr, tempint, ii
LOGICAL(LGT) :: isSublist, isok, isSpace, isTime, acase

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

sublist => NULL()

! Create a new sublist
isSublist = param%IsSubList(prefix)

IF (isSublist) THEN
  ierr = param%GetSubList(key=prefix, sublist=sublist)

#ifdef DEBUG_VER
  isok = ierr .EQ. 0_I4B
  CALL AssertError1(isok, myName, &
                    'Error occured in getting sublist(1)')
#endif

END IF

IF (.NOT. isSublist) THEN
  sublist => param%NewSubList(key=prefix)
END IF

#ifdef DEBUG_VER
isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'Error occured in getting sublist(2)')
#endif

CALL FPL_Set(obj=sublist, datatype="Char", prefix=prefix, key="name", &
             VALUE=name)

CALL FPL_Set(obj=sublist, datatype="Char", prefix=prefix, key="engine", &
             VALUE=engine)

tempint = Input(option=fieldType, default=TypeField%normal)
CALL FPL_Set(obj=sublist, datatype=TypeField%normal, prefix=prefix, &
             key="fieldType", VALUE=tempint)

tempint = Input(option=comm, default=0_I4B)
CALL FPL_Set(obj=sublist, datatype=tempint, prefix=prefix, key="comm", &
             VALUE=tempint)

tempint = Input(option=local_n, default=0_I4B)
CALL FPL_Set(obj=sublist, datatype=tempint, prefix=prefix, key="local_n", &
             VALUE=tempint)

tempint = Input(option=global_n, default=0_I4B)
CALL FPL_Set(obj=sublist, datatype=tempint, prefix=prefix, key="global_n", &
             VALUE=tempint)

isSpace = PRESENT(isSpaceCompo) .AND. PRESENT(spaceCompo)
CALL FPL_Set(obj=sublist, datatype=isSpace, prefix=prefix, &
             key="isSpaceCompo", VALUE=isSpace)

isTime = PRESENT(isTimeCompo) .AND. PRESENT(timeCompo)
CALL FPL_Set(obj=sublist, datatype=isTime, prefix=prefix, key="isTimeCompo", &
             VALUE=isTime)

! check isSpaceCompoScalar
isok = isSpace .AND. PRESENT(isSpaceCompoScalar)
acase = .FALSE.; IF (isok) acase = isSpaceCompoScalar
CALL FPL_Set(obj=sublist, datatype=acase, prefix=prefix, &
             key="isSpaceCompoScalar", VALUE=acase)
IF (acase) THEN
  CALL FPL_Set(obj=sublist, datatype=spaceCompo(1), prefix=prefix, &
               key="spaceCompo", VALUE=spaceCompo(1))
END IF
acase = (.NOT. acase) .AND. isSpace
IF (acase) THEN
  CALL FPL_Set(obj=sublist, datatype=spaceCompo, prefix=prefix, &
               key="spaceCompo", VALUE=spaceCompo)
END IF

! check isTimeCompoScalar
isok = isTime .AND. PRESENT(isTimeCompoScalar)
acase = .FALSE.
IF (isok) acase = isTimeCompoScalar
CALL FPL_Set(obj=sublist, datatype=acase, prefix=prefix, &
             key="isTimeCompoScalar", VALUE=acase)
IF (acase) THEN
  CALL FPL_Set(obj=sublist, datatype=timeCompo(1), prefix=prefix, &
               key="timeCompo", VALUE=timeCompo(1))
END IF
acase = (.NOT. acase) .AND. isTime
IF (acase) THEN
  CALL FPL_Set(obj=sublist, datatype=timeCompo, prefix=prefix, &
               key="timeCompo", VALUE=timeCompo)
END IF

! physical variable names are handled here
tempint = Input(option=tPhysicalVarNames, default=0_I4B)
CALL FPL_Set(obj=sublist, datatype=tempint, prefix=prefix, &
             key="tPhysicalVarNames", VALUE=tempint)

isok = PRESENT(physicalVarNames) .AND. PRESENT(isPhysicalVarNames)
acase = .FALSE.; IF (isok) acase = isPhysicalVarNames

IF (acase) THEN
  DO ii = 1, tempint
    CALL FPL_Set(obj=sublist, datatype="char", prefix=prefix, &
              key="physicalVarName"//ToString(ii), VALUE=physicalVarNames(ii))
  END DO
END IF

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetAbstractFieldParam

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

#ifdef DEBUG_VER
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  'Error occured in getting sublist(1)')
#endif

! note: We should not call deallocate in abstract classes.
! This is because, in concrete classes we may set some
! parameters before calling this method.
! All those parameters will be gone if we call deallocate
! here.
! CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'Error occured in getting sublist(2)')
#endif

obj%isInit = .TRUE.
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

obj%fedof => fedof
obj%geofedof => geofedof
IF (PRESENT(timefedof)) obj%timefedof => timefedof

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
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = obj2%IsInitiated()
CALL AssertError1(isok, myName, &
                  'obj2 is not initiated.')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'obj is already initiated.')
#endif

obj%isInit = obj2%isInit
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
obj%geofedof => obj2%geofedof

isok = ALLOCATED(obj2%fedofs)
IF (isok) THEN
  tsize = SIZE(obj2%fedofs)
  ALLOCATE (obj%fedofs(tsize))
  DO ii = 1, tsize
    obj%fedofs(ii)%ptr => obj2%fedofs(ii)%ptr
  END DO
END IF

obj%timefedof => obj2%timefedof
isok = ALLOCATED(obj2%timefedofs)
IF (isok) THEN
  tsize = SIZE(obj2%timefedofs)
  ALLOCATE (obj%timefedofs(tsize))
  DO ii = 1, tsize
    obj%timefedofs(ii)%ptr => obj2%timefedofs(ii)%ptr
  END DO
END IF

obj%exact => obj2%exact
obj%saveErrorNorm = obj2%saveErrorNorm
obj%errorType = obj2%errorType
obj%plotWithResult = obj2%plotWithResult
obj%plotErrorNorm = obj2%plotErrorNorm

isok = ALLOCATED(obj2%dbc)
IF (isok) THEN
  tsize = SIZE(obj2%dbc)
  ALLOCATE (obj%dbc(tsize))
  DO ii = 1, tsize
    obj%dbc(ii)%ptr => obj2%dbc(ii)%ptr
  END DO
END IF

isok = ALLOCATED(obj2%nbc)
IF (isok) THEN
  tsize = SIZE(obj2%nbc)
  ALLOCATE (obj%nbc(tsize))
  DO ii = 1, tsize
    obj%nbc(ii)%ptr => obj2%nbc(ii)%ptr
  END DO
END IF

isok = ALLOCATED(obj2%nbc_point)
IF (isok) THEN
  tsize = SIZE(obj2%nbc_point)
  ALLOCATE (obj%nbc_point(tsize))
  DO ii = 1, tsize
    obj%nbc_point(ii)%ptr => obj2%nbc_point(ii)%ptr
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
#endif

TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr, ii, tsize
CHARACTER(:), ALLOCATABLE :: prefix
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

! main
prefix = obj%GetPrefix()

sublist => NULL()
ierr = param%GetSubList(key=prefix, sublist=sublist)

#ifdef DEBUG_VER
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  'Error occured in getting sublist(1)')
#endif

! note: We should not call deallocate in abstract classes.
! This is because, in concrete classes we may set some
! parameters before calling this method.
! All those parameters will be gone if we call deallocate
! here.
! CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'Error occured in getting sublist(2)')
#endif

obj%isInit = .TRUE.
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

tsize = SIZE(fedof)
ALLOCATE (obj%fedofs(tsize))
DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = ASSOCIATED(fedof(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'fedof('//ToString(ii)//') is not ASSOCIATED.')
#endif
  obj%fedofs(ii)%ptr => fedof(ii)%ptr
END DO

tsize = SIZE(geofedof)
ALLOCATE (obj%geofedofs(tsize))
DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = ASSOCIATED(geofedof(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'geofedof('//ToString(ii)//') is not ASSOCIATED.')
#endif
  obj%geofedofs(ii)%ptr => geofedof(ii)%ptr
END DO

! If timefedof is not preseent then exit
isok = PRESENT(timefedof)
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  prefix = ""
  sublist => NULL()
  RETURN
END IF

tsize = SIZE(timefedof)
ALLOCATE (obj%timefedofs(tsize))
DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = ASSOCIATED(timefedof(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'timefedof('//ToString(ii)//') is not ASSOCIATED.')
#endif
  obj%timefedofs(ii)%ptr => timefedof(ii)%ptr
END DO

prefix = ""
sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

! main

! note: We should not call deallocate in abstract classes.
! This is because, in concrete classes we may set some
! parameters before calling this method.
! All those parameters will be gone if we call deallocate
! here.
! CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%name = name
obj%engine = engine
obj%fieldType = Input(option=fieldType, default=TypeField%normal)
obj%comm = Input(option=comm, default=0_I4B)
obj%local_n = Input(option=local_n, default=0_I4B)
obj%global_n = Input(option=global_n, default=0_I4B)

obj%fedof => fedof
obj%geofedof => geofedof
IF (PRESENT(timefedof)) obj%timefedof => timefedof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate5()"
#endif

INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

! main

! note: We should not call deallocate in abstract classes.
! This is because, in concrete classes we may set some
! parameters before calling this method.
! All those parameters will be gone if we call deallocate
! here.
! CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%name = name
obj%engine = engine
obj%fieldType = Input(option=fieldType, default=TypeField%normal)
obj%comm = Input(option=comm, default=0_I4B)
obj%local_n = Input(option=local_n, default=0_I4B)
obj%global_n = Input(option=global_n, default=0_I4B)

tsize = SIZE(fedof)
ALLOCATE (obj%fedofs(tsize))
DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = ASSOCIATED(fedof(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'fedof('//ToString(ii)//') is not ASSOCIATED.')
#endif
  obj%fedofs(ii)%ptr => fedof(ii)%ptr
END DO

tsize = SIZE(geofedof)
ALLOCATE (obj%geofedofs(tsize))
DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = ASSOCIATED(geofedof(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'geofedof('//ToString(ii)//') is not ASSOCIATED.')
#endif
  obj%geofedofs(ii)%ptr => geofedof(ii)%ptr
END DO

! If timefedof is not preseent then exit
isok = PRESENT(timefedof)
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

tsize = SIZE(timefedof)
ALLOCATE (obj%timefedofs(tsize))
DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = ASSOCIATED(timefedof(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'timefedof('//ToString(ii)//') is not ASSOCIATED.')
#endif
  obj%timefedofs(ii)%ptr => timefedof(ii)%ptr
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%fieldType = TypeField%normal
obj%name = ""
obj%engine = ""
obj%comm = 0
obj%myRank = 0
obj%numProcs = 1
obj%global_n = 0
obj%local_n = 0
obj%is = 0
obj%ie = 0
obj%lis_ptr = 0
obj%fedof => NULL()
obj%geofedof => NULL()

isok = ALLOCATED(obj%fedofs)
IF (isok) THEN
  tsize = SIZE(obj%fedofs)
  DO ii = 1, tsize
    obj%fedofs(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%fedofs)
END IF

isok = ALLOCATED(obj%geofedofs)
IF (isok) THEN
  tsize = SIZE(obj%geofedofs)
  DO ii = 1, tsize
    obj%geofedofs(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%geofedofs)
END IF

obj%timefedof => NULL()

isok = ALLOCATED(obj%timefedofs)
IF (isok) THEN
  tsize = SIZE(obj%timefedofs)
  DO ii = 1, tsize
    obj%timefedofs(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%timefedofs)
END IF

obj%exact => NULL()
obj%saveErrorNorm = .FALSE.
obj%errorType = "NONE"
obj%plotWithResult = .FALSE.
obj%plotErrorNorm = .FALSE.

isok = ALLOCATED(obj%dbc)
IF (isok) THEN
  tsize = SIZE(obj%dbc)
  DO ii = 1, tsize
    obj%dbc(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%dbc)
END IF

isok = ALLOCATED(obj%nbc)
IF (isok) THEN
  tsize = SIZE(obj%nbc)
  DO ii = 1, tsize
    obj%nbc(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%nbc)
END IF

isok = ALLOCATED(obj%nbc_point)
IF (isok) THEN
  tsize = SIZE(obj%nbc_point)
  DO ii = 1, tsize
    obj%nbc_point(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%nbc_point)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
