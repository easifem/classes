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

SUBMODULE(AbstractNodeField_Class) ConstructorMethods
USE AbstractField_Class, ONLY: AbstractFieldInitiate
USE AbstractField_Class, ONLY: AbstractFieldDeallocate
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: DOF_Deallocate => DEALLOCATE
USE DOF_Method, ONLY: DOF_Initiate => Initiate
USE ReallocateUtility, ONLY: Reallocate
USE RealVector_Method, ONLY: RealVector_Deallocate => DEALLOCATE
USE RealVector_Method, ONLY: RealVector_Initiate => Initiate
USE RealVector_Method, ONLY: RealVector_Size => Size
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "AbstractNodeField_Class@ConstructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                               AbstractNodeFieldCheckError
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractNodeFieldCheckError
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractNodeFieldCheckError()"
INTEGER(I4B) :: tvar
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%dof_tPhysicalVars .NE. 0_I4B
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_tPhysicalVars is 0')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dof_spaceCompo)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_spaceCompo is NOT ALLOCATED')
#endif

#ifdef DEBUG_VER
tvar = SIZE(obj%dof_spaceCompo)
isok = tvar .EQ. obj%dof_tPhysicalVars
CALL AssertError1(isok, myName, &
                  'size of dof_spaceCompo ('//ToString(tvar)// &
                  ') is not same as dof_tPhysicalVars ('// &
                  ToString(obj%dof_tPhysicalVars)//')')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dof_timeCompo)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_timeCompo is NOT ALLOCATED')
#endif

#ifdef DEBUG_VER
tvar = SIZE(obj%dof_timeCompo)
isok = tvar .EQ. obj%dof_tPhysicalVars
CALL AssertError1(isok, myName, &
                  'size of dof_timeCompo ('//ToString(tvar)// &
                  ') is not same as dof_tPhysicalVars ('// &
                  ToString(obj%dof_tPhysicalVars)//')')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dof_tNodes)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_tNodes is NOT ALLOCATED')
#endif

#ifdef DEBUG_VER
tvar = SIZE(obj%dof_tNodes)
isok = tvar .EQ. obj%dof_tPhysicalVars
CALL AssertError1(isok, myName, &
                  'size of dof_tNodes ('//ToString(tvar)// &
                  ') is not same as dof_tPhysicalVars ('// &
                  ToString(obj%dof_tPhysicalVars)//')')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%dof_names_char)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_names_char is NOT ALLOCATED')
#endif

#ifdef DEBUG_VER
tvar = SIZE(obj%dof_names_char)
isok = tvar .EQ. obj%dof_tPhysicalVars
CALL AssertError1(isok, myName, &
                  'size of dof_names_char ('//ToString(tvar)// &
                  ') is not same as dof_tPhysicalVars ('// &
                  ToString(obj%dof_tPhysicalVars)//')')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE AbstractNodeFieldCheckError

!----------------------------------------------------------------------------
!                                                                 Initiate2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Deallocate in the caller
! CALL obj%DEALLOCATE()
CALL AbstractFieldInitiate( &
  obj=obj, obj2=obj2, copyFull=copyFull, copyStructure=copyStructure, &
  usePointer=usePointer)

SELECT TYPE (obj2); CLASS IS (AbstractNodeField_)
  obj%dof = obj2%dof

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

  isok = ALLOCATED(obj2%geofedofs)
  IF (isok) THEN
    tsize = SIZE(obj2%geofedofs)
    ALLOCATE (obj%geofedofs(tsize))
    DO ii = 1, tsize
      obj%geofedofs(ii)%ptr => obj2%geofedofs(ii)%ptr
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

  isok = ALLOCATED(obj2%nodalValue)
  IF (isok) THEN
    obj%nodalValue = obj2%nodalValue
  END IF

  isok = ALLOCATED(obj2%nodeNum)
  IF (isok) THEN
    obj%nodeNum = obj2%nodeNum
  END IF

  obj%dof_tPhysicalVars = obj2%dof_tPhysicalVars
  obj%dof_storageFMT = obj2%dof_storageFMT

  isok = ALLOCATED(obj2%dof_spaceCompo)
  IF (isok) obj%dof_spaceCompo = obj2%dof_spaceCompo

  isok = ALLOCATED(obj2%dof_timeCompo)
  IF (isok) obj%dof_timeCompo = obj2%dof_timeCompo

  isok = ALLOCATED(obj2%dof_tNodes)
  IF (isok) obj%dof_tNodes = obj2%dof_tNodes

  isok = ALLOCATED(obj2%dof_names_char)
  IF (isok) obj%dof_names_char = obj2%dof_names_char

  obj%tSize = obj2%tSize

  obj%realVec = obj2%realVec
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

INTEGER(I4B) :: ii, tsize1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

! Important: we should not call this method here
! CALL obj%DEALLOCATE()
CALL AbstractFieldInitiate(obj=obj, name=name, engine=engine, &
                           fieldType=fieldType, comm=comm, &
                           local_n=local_n, global_n=global_n)

obj%fedof => fedof
obj%geofedof => geofedof
IF (PRESENT(timefedof)) obj%timefedof => timefedof

#ifdef DEBUG_VER
tsize1 = SIZE(spaceCompo)
CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                  "a=size(spaceCompo), b=tPhysicalVarNames")
#endif

#ifdef DEBUG_VER
tsize1 = SIZE(timeCompo)
CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                  "a=size(timeCompo), b=tPhysicalVarNames")
#endif

#ifdef DEBUG_VER
tsize1 = SIZE(physicalVarNames)
CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                  "a=size(physicalVarNames), b=tPhysicalVarNames")
#endif

#ifdef DEBUG_VER
tsize1 = SIZE(tNodes)
CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                  "a=size(tNodes), b=tPhysicalVarNames")
#endif

obj%dof_tPhysicalVars = tPhysicalVarNames
obj%dof_storageFMT = storageFMT
obj%tSize = tSize

CALL Reallocate(obj%dof_spaceCompo, tPhysicalVarNames)
CALL Reallocate(obj%dof_timeCompo, tPhysicalVarNames)
CALL Reallocate(obj%dof_tNodes, tPhysicalVarNames)
ALLOCATE (obj%dof_names_char(tPhysicalVarNames))

DO ii = 1, tPhysicalVarNames
  obj%dof_spaceCompo(ii) = spaceCompo(ii)
  obj%dof_timeCompo(ii) = timeCompo(ii)
  obj%dof_tNodes(ii) = tNodes(ii)
  obj%dof_names_char(ii) (1:1) = physicalVarNames(ii) (1:1)
END DO

CALL DOF_Initiate(obj=obj%dof, tNodes=obj%dof_tNodes, &
                  names=obj%dof_names_char, spaceCompo=obj%dof_spaceCompo, &
                  timeCompo=obj%dof_timeCompo, storageFMT=obj%dof_storageFMT)

CALL RealVector_Initiate(obj=obj%realVec, dofobj=obj%dof)

#ifdef DEBUG_VER
tsize1 = RealVector_SIZE(obj%realVec)
CALL AssertError2(tsize1, tSize, myName, &
                  "a=RealVector_Size(obj%realVec), b=tSize")
#endif

IF (obj%local_n .EQ. 0) CALL obj%SetLocal_n(local_n=obj%tSize)
IF (obj%global_n .EQ. 0) CALL obj%SetGlobal_n(global_n=obj%tSize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, tsize1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

! Important: we should not call this method here
! CALL obj%DEALLOCATE()
CALL AbstractFieldInitiate(obj=obj, name=name, engine=engine, &
                           fieldType=fieldType, comm=comm, &
                           local_n=local_n, global_n=global_n)

#ifdef DEBUG_VER
tsize1 = SIZE(spaceCompo)
CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                  "a=size(spaceCompo), b=tPhysicalVarNames")
#endif

#ifdef DEBUG_VER
tsize1 = SIZE(timeCompo)
CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                  "a=size(timeCompo), b=tPhysicalVarNames")
#endif

#ifdef DEBUG_VER
tsize1 = SIZE(physicalVarNames)
CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                  "a=size(physicalVarNames), b=tPhysicalVarNames")
#endif

#ifdef DEBUG_VER
tsize1 = SIZE(tNodes)
CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                  "a=size(tNodes), b=tPhysicalVarNames")
#endif

obj%dof_tPhysicalVars = tPhysicalVarNames
obj%dof_storageFMT = storageFMT
obj%tSize = tSize

CALL Reallocate(obj%dof_spaceCompo, tPhysicalVarNames)
CALL Reallocate(obj%dof_timeCompo, tPhysicalVarNames)
CALL Reallocate(obj%dof_tNodes, tPhysicalVarNames)
ALLOCATE (obj%dof_names_char(tPhysicalVarNames))

DO ii = 1, tPhysicalVarNames
  obj%dof_spaceCompo(ii) = spaceCompo(ii)
  obj%dof_timeCompo(ii) = timeCompo(ii)
  obj%dof_tNodes(ii) = tNodes(ii)
  obj%dof_names_char(ii) (1:1) = physicalVarNames(ii) (1:1)
END DO

CALL DOF_Initiate(obj=obj%dof, tNodes=obj%dof_tNodes, &
                  names=obj%dof_names_char, spaceCompo=obj%dof_spaceCompo, &
                  timeCompo=obj%dof_timeCompo, storageFMT=obj%dof_storageFMT)

CALL RealVector_Initiate(obj=obj%realVec, dofobj=obj%dof)

#ifdef DEBUG_VER
tsize1 = RealVector_SIZE(obj%realVec)
CALL AssertError2(tsize1, tSize, myName, &
                  "a=RealVector_Size(obj%realVec), b=tSize")
#endif

IF (obj%local_n .EQ. 0) CALL obj%SetLocal_n(local_n=obj%tSize)
IF (obj%global_n .EQ. 0) CALL obj%SetGlobal_n(global_n=obj%tSize)

! handle fedof
tsize1 = SIZE(fedof)
IF (tsize1 .EQ. math%one_i) THEN

#ifdef DEBUG_VER
  isok = ASSOCIATED(fedof(1)%ptr)
  CALL AssertError1(isok, myName, &
                    "fedof(1)%ptr is not associated.")
#endif

  ALLOCATE (obj%fedofs(tPhysicalVarNames))
  DO ii = 1, tPhysicalVarNames
    obj%fedofs(ii)%ptr => fedof(1)%ptr
  END DO

ELSE

#ifdef DEBUG_VER
  CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                    "a=size(fedof), b=tPhysicalVarNames")
#endif

  ALLOCATE (obj%fedofs(tPhysicalVarNames))
  DO ii = 1, tPhysicalVarNames

#ifdef DEBUG_VER
    isok = ASSOCIATED(fedof(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      "fedof(ii)%ptr is not associated, for ii = "// &
                      ToString(ii))
#endif

    obj%fedofs(ii)%ptr => fedof(ii)%ptr
  END DO

END IF

! handle geofedof
tsize1 = SIZE(geofedof)
IF (tsize1 .EQ. math%one_i) THEN

#ifdef DEBUG_VER
  isok = ASSOCIATED(geofedof(1)%ptr)
  CALL AssertError1(isok, myName, &
                    "geofedof(1)%ptr is not associated.")
#endif

  ALLOCATE (obj%geofedofs(tPhysicalVarNames))
  DO ii = 1, tPhysicalVarNames
    obj%geofedofs(ii)%ptr => geofedof(1)%ptr
  END DO

ELSE

#ifdef DEBUG_VER
  CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                    "a=size(geofedof), b=tPhysicalVarNames")
#endif

  ALLOCATE (obj%geofedofs(tPhysicalVarNames))
  DO ii = 1, tPhysicalVarNames

#ifdef DEBUG_VER
    isok = ASSOCIATED(geofedof(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      "geofedof(ii)%ptr is not associated, for ii = "// &
                      ToString(ii))
#endif

    obj%geofedofs(ii)%ptr => geofedof(ii)%ptr
  END DO

END IF

! we are done here if timefedof is not present.
IF (.NOT. PRESENT(timefedof)) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! following code is executed when timefedof is present.

! handle timefedof
tsize1 = SIZE(timefedof)
IF (tsize1 .EQ. math%one_i) THEN

#ifdef DEBUG_VER
  isok = ASSOCIATED(timefedof(1)%ptr)
  CALL AssertError1(isok, myName, &
                    "timefedof(1)%ptr is not associated.")
#endif

  ALLOCATE (obj%timefedofs(tPhysicalVarNames))
  DO ii = 1, tPhysicalVarNames
    obj%timefedofs(ii)%ptr => timefedof(1)%ptr
  END DO

ELSE

#ifdef DEBUG_VER
  CALL AssertError2(tsize1, tPhysicalVarNames, myName, &
                    "a=size(timefedof), b=tPhysicalVarNames")
#endif

  ALLOCATE (obj%timefedofs(tPhysicalVarNames))
  DO ii = 1, tPhysicalVarNames

#ifdef DEBUG_VER
    isok = ASSOCIATED(timefedof(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      "timefedof(ii)%ptr is not associated, for ii = "// &
                      ToString(ii))
#endif

    obj%timefedofs(ii)%ptr => timefedof(ii)%ptr
  END DO

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                  Deallocate
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

CALL AbstractFieldDeallocate(obj)

obj%isMaxTotalNodeNumForBCSet = math%no
obj%maxTotalNodeNumForBC = math%zero_i
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

isok = ALLOCATED(obj%nodalValue)
IF (isok) DEALLOCATE (obj%nodalValue)

isok = ALLOCATED(obj%nodeNum)
IF (isok) DEALLOCATE (obj%nodeNum)

CALL DOF_Deallocate(obj%dof)

obj%dof_tPhysicalVars = 0
obj%dof_storageFMT = MYSTORAGEFORMAT
IF (ALLOCATED(obj%dof_spaceCompo)) DEALLOCATE (obj%dof_spaceCompo)
IF (ALLOCATED(obj%dof_timeCompo)) DEALLOCATE (obj%dof_timeCompo)
IF (ALLOCATED(obj%dof_tNodes)) DEALLOCATE (obj%dof_tNodes)
IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
obj%tSize = 0
CALL RealVector_Deallocate(obj%realVec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
