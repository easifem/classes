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
USE Display_Method, ONLY: ToString

USE RealVector_Method, ONLY: RealVector_Deallocate => DEALLOCATE, &
                             RealVector_Initiate => Initiate, &
                             RealVector_Size => Size

USE DOF_Method, ONLY: DOF_Deallocate => DEALLOCATE, &
                      DOF_Initiate => Initiate

USE AbstractField_Class, ONLY: AbstractFieldInitiate, &
                               AbstractFieldDeallocate

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                               AbstractNodeFieldCheckError
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractNodeFieldCheckError
CHARACTER(*), PARAMETER :: myName = "AbstractNodeFieldCheckError()"
INTEGER(I4B) :: ivar, tvar
LOGICAL(LGT) :: problem, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = obj%dof_tPhysicalVars .NE. 0_I4B
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_tPhysicalVars is 0')

isok = ALLOCATED(obj%dof_spaceCompo)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_spaceCompo is NOT ALLOCATED')

tvar = SIZE(obj%dof_spaceCompo)
isok = tvar .EQ. obj%dof_tPhysicalVars
CALL AssertError1(isok, myName, &
                  'size of dof_spaceCompo ('//ToString(tvar)// &
                  ') is not same as dof_tPhysicalVars ('// &
                  ToString(obj%dof_tPhysicalVars)//')')

isok = ALLOCATED(obj%dof_timeCompo)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_timeCompo is NOT ALLOCATED')

tvar = SIZE(obj%dof_timeCompo)
isok = tvar .EQ. obj%dof_tPhysicalVars
CALL AssertError1(isok, myName, &
                  'size of dof_timeCompo ('//ToString(tvar)// &
                  ') is not same as dof_tPhysicalVars ('// &
                  ToString(obj%dof_tPhysicalVars)//')')

isok = ALLOCATED(obj%dof_tNodes)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_tNodes is NOT ALLOCATED')

tvar = SIZE(obj%dof_tNodes)
isok = tvar .EQ. obj%dof_tPhysicalVars
CALL AssertError1(isok, myName, &
                  'size of dof_tNodes ('//ToString(tvar)// &
                  ') is not same as dof_tPhysicalVars ('// &
                  ToString(obj%dof_tPhysicalVars)//')')

isok = ALLOCATED(obj%dof_names_char)
CALL AssertError1(isok, myName, &
                  'AbstractNodeField_::obj%dof_names_char is NOT ALLOCATED')

tvar = SIZE(obj%dof_names_char)
isok = tvar .EQ. obj%dof_tPhysicalVars
CALL AssertError1(isok, myName, &
                  'size of dof_names_char ('//ToString(tvar)// &
                  ') is not same as dof_tPhysicalVars ('// &
                  ToString(obj%dof_tPhysicalVars)//')')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractNodeFieldCheckError

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

CHARACTER(:), ALLOCATABLE :: prefix
INTEGER(I4B) :: local_n, global_n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

prefix = obj%GetPrefix()

CALL AbstractFieldInitiate(obj=obj, param=param, fedof=fedof, &
                           timefedof=timefedof)

CALL AbstractNodeFieldCheckError(obj)

!info: So when a child calls this routine then dof_** are already set
! That is also the reason why we are not calling deallocate in the begining

CALL DOF_Initiate(obj=obj%dof, tNodes=obj%dof_tNodes, &
                  names=obj%dof_names_char, spaceCompo=obj%dof_spaceCompo, &
                  timeCompo=obj%dof_timeCompo, storageFMT=obj%dof_storageFMT)

CALL RealVector_Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%tSize = RealVector_SIZE(obj%realVec)

CALL obj%GetParam(local_n=local_n, global_n=global_n)
IF (local_n .EQ. 0) CALL obj%SetParam(local_n=obj%tSize)
IF (global_n .EQ. 0) CALL obj%SetParam(global_n=obj%tSize)

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
CALL AbstractFieldInitiate(obj=obj, obj2=obj2, copyFull=copyFull, &
                           copyStructure=copyStructure, usePointer=usePointer)

SELECT TYPE (obj2); CLASS IS (AbstractNodeField_)
  obj%dof_tPhysicalVars = obj2%dof_tPhysicalVars
  obj%dof_storageFMT = obj2%dof_storageFMT

  IF (ALLOCATED(obj2%dof_spaceCompo)) obj%dof_spaceCompo = obj2%dof_spaceCompo
  IF (ALLOCATED(obj2%dof_timeCompo)) obj%dof_timeCompo = obj2%dof_timeCompo
  IF (ALLOCATED(obj2%dof_tNodes)) obj%dof_tNodes = obj2%dof_tNodes
  IF (ALLOCATED(obj2%dof_names_char)) obj%dof_names_char = obj2%dof_names_char
  obj%tSize = obj2%tSize
  obj%realVec = obj2%realVec
  obj%dof = obj2%dof
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                            obj_Initiate3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
#endif

INTEGER(I4B) :: local_n, global_n
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

prefix = obj%GetPrefix()

CALL AbstractFieldInitiate(obj=obj, param=param, fedof=fedof, &
                           timefedof=timefedof)

CALL AbstractNodeFieldCheckError(obj)

CALL DOF_Initiate(obj=obj%dof, tNodes=obj%dof_tNodes, &
                  names=obj%dof_names_char, spaceCompo=obj%dof_spaceCompo, &
                  timeCompo=obj%dof_timeCompo, storageFMT=obj%dof_storageFMT)

CALL RealVector_Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%tSize = RealVector_SIZE(obj%realVec)

CALL obj%GetParam(local_n=local_n, global_n=global_n)

IF (local_n .EQ. 0) CALL obj%SetParam(local_n=obj%tSize)
IF (global_n .EQ. 0) CALL obj%SetParam(global_n=obj%tSize)

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, jj, tsize1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()

CALL AbstractFieldInitiate(obj=obj, name=name, engine=engine, &
                           storageFMT=storageFMT, fieldType=fieldType, &
                           comm=comm, local_n=local_n, global_n=global_n, &
                           fedof=fedof, timefedof=timefedof)

#ifdef DEBUG_VER
isok = PRESENT(tPhysicalVarNames)
CALL AssertError1(isok, myName, 'tPhysicalVarNames is not present')

isok = PRESENT(storageFMT)
CALL AssertError1(isok, myName, 'storageFMT is not present')

isok = PRESENT(spaceCompo)
CALL AssertError1(isok, myName, 'spaceCompo is not present')

isok = PRESENT(isSpaceCompo)
CALL AssertError1(isok, myName, 'isSpaceCompo is not present')

isok = PRESENT(isSpaceCompoScalar)
CALL AssertError1(isok, myName, 'isSpaceCompoScalar is not present')

tsize1 = SIZE(spaceCompo)
isok = tsize1 .EQ. tPhysicalVarNames
CALL AssertError1(isok, myName, &
                  'size of spaceCompo ('//ToString(tsize1)// &
                  ') is not same as tPhysicalVarNames ('// &
                  ToString(tPhysicalVarNames)//')')

isok = PRESENT(timeCompo)
CALL AssertError1(isok, myName, 'timeCompo is not present')

isok = PRESENT(isTimeCompo)
CALL AssertError1(isok, myName, 'isTimeCompo is not present')

isok = PRESENT(isTimeCompoScalar)
CALL AssertError1(isok, myName, 'isTimeCompoScalar is not present')

tsize1 = SIZE(timeCompo)
isok = tsize1 .EQ. tPhysicalVarNames
CALL AssertError1(isok, myName, &
                  'size of timeCompo ('//ToString(tsize1)// &
                  ') is not same as tPhysicalVarNames ('// &
                  ToString(tPhysicalVarNames)//')')

isok = PRESENT(physicalVarNames)
CALL AssertError1(isok, myName, 'physicalVarNames is not present')

isok = PRESENT(isPhysicalVarNames)
CALL AssertError1(isok, myName, 'isPhysicalVarNames is not present')

isok = PRESENT(isPhysicalVarNamesScalar)
CALL AssertError1(isok, myName, 'isPhysicalVarNamesScalar is not present')

tsize1 = SIZE(physicalVarNames)
isok = tsize1 .EQ. tPhysicalVarNames
CALL AssertError1(isok, myName, &
                  'size of physicalVarNames ('//ToString(tsize1)// &
                  ') is not same as tPhysicalVarNames ('// &
                  ToString(tPhysicalVarNames)//')')

isok = PRESENT(tNodes)
CALL AssertError1(isok, myName, 'tNodes is not present')

isok = PRESENT(isTNodes)
CALL AssertError1(isok, myName, 'isTNodes is not present')

isok = PRESENT(isTNodesScalar)
CALL AssertError1(isok, myName, 'isTNodesScalar is not present')

tsize1 = SIZE(tNodes)
isok = tsize1 .EQ. tPhysicalVarNames
CALL AssertError1(isok, myName, &
                  'size of tNodes('//ToString(tsize1)// &
                  ') is not same as tPhysicalVarNames ('// &
                  ToString(tPhysicalVarNames)//')')

isok = PRESENT(tSize)
CALL AssertError1(isok, myName, 'tSize is not present')

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
isok = tsize1 .EQ. tSize
CALL AssertError1(isok, myName, &
                  'size of realVec ('//ToString(tsize1)// &
                  ') is not same as tSize ('//ToString(tSize)//')')
#endif

CALL obj%GetParam(local_n=ii, global_n=jj)
IF (ii .EQ. 0) CALL obj%SetParam(local_n=obj%tSize)
IF (jj .EQ. 0) CALL obj%SetParam(global_n=obj%tSize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate5()"
#endif

INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

CALL AbstractFieldInitiate(obj=obj, name=name, engine=engine, &
                           fieldType=fieldType, comm=comm, local_n=local_n, &
                           global_n=global_n, spaceCompo=spaceCompo, &
                           isSpaceCompo=isSpaceCompo, &
                           isSpaceCompoScalar=isSpaceCompoScalar, &
                           timeCompo=timeCompo, isTimeCompo=isTimeCompo, &
                           isTimeCompoScalar=isTimeCompoScalar, &
                           tPhysicalVarNames=tPhysicalVarNames, &
                           physicalVarNames=physicalVarNames, &
                           isPhysicalVarNames=isPhysicalVarNames, &
                           fedof=fedof, timefedof=timefedof)

CALL DOF_Initiate(obj=obj%dof, &
                  tNodes=obj%dof_tNodes, &
                  names=obj%dof_names_char, &
                  spaceCompo=obj%dof_spaceCompo, &
                  timeCompo=obj%dof_timeCompo, &
                  storageFMT=obj%dof_storageFMT)

CALL RealVector_Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%tSize = RealVector_SIZE(obj%realVec)

CALL obj%GetParam(local_n=ii, global_n=jj)

IF (ii .EQ. 0) CALL obj%SetParam(local_n=obj%tSize)
IF (jj .EQ. 0) CALL obj%SetParam(global_n=obj%tSize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldDeallocate(obj)
obj%dof_tPhysicalVars = 0
obj%dof_storageFMT = NODES_FMT
IF (ALLOCATED(obj%dof_spaceCompo)) DEALLOCATE (obj%dof_spaceCompo)
IF (ALLOCATED(obj%dof_timeCompo)) DEALLOCATE (obj%dof_timeCompo)
IF (ALLOCATED(obj%dof_tNodes)) DEALLOCATE (obj%dof_tNodes)
IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
obj%tSize = 0
CALL RealVector_Deallocate(obj%realVec)
CALL DOF_Deallocate(obj%dof)

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
