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

SUBMODULE(BlockNodeField_Class) ConstructorMethods
USE Display_Method, ONLY: ToString
USE FPL_Method, ONLY: Set, GetValue
USE String_Class, ONLY: String
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldSetParam, &
                                   AbstractNodeFieldInitiate, &
                                   AbstractNodeFieldDeallocate
USE AbstractField_Class, ONLY: AbstractFieldCheckEssentialParam, &
                               SetAbstractFieldParam
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
USE ArangeUtility, ONLY: Arange
USE DOF_Method, ONLY: OPERATOR(.tDOF.)

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                         setBlockNodeField
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBlockNodeFieldParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetBlockNodeFieldParam()"
INTEGER(I4B) :: intvec(3)
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ierr, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(physicalVarNames)

#ifdef DEBUG_VER
intvec(1) = SIZE(physicalVarNames)
intvec(2) = SIZE(spaceCompo)
intvec(3) = SIZE(timeCompo)

isok = ALL(intvec .EQ. tsize)
CALL AssertError1(isok, myName, &
            'size of physicalVarNames, spaceCompo, timeCompo should be same.')
#endif

CALL SetAbstractFieldParam(param=param, &
                           prefix=myprefix, &
                           name=name, &
                           engine=engine, &
                           fieldType=fieldType, &
                           comm=comm, &
                           local_n=local_n, &
                           global_n=global_n, &
                           spaceCompo=spaceCompo, &
                           isSpaceCompo=.TRUE., &
                           timeCompo=timeCompo, &
                           isTimeCompo=.TRUE., &
                           physicalVarNames=physicalVarNames, &
                           tPhysicalVarNames=tsize, &
                           isPhysicalVarNames=.TRUE.)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetBlockNodeFieldParam

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
INTEGER(I4B) :: ii, n
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)

isok = param%IsPresent(key=myprefix//"/spaceCompo")
CALL AssertError1(isok, myName, &
                  myprefix//'/spaceCompo should be present in param')

n = 0
isok = param%IsPresent(key=myprefix//"/tPhysicalVarNames")
CALL AssertError1(isok, myName, &
                  myprefix//'/tPhysicalVarNames should be present in param')
ii = param%Get(key=myprefix//'/tPhysicalVarNames', VALUE=n)

DO ii = 1, n
  isok = param%IsPresent(key=myprefix//"/physicalVarName"// &
                         ToString(ii))
  CALL AssertError1(isok, myName, &
                    myprefix//'/physicalVarName'//ToString(ii)// &
                    ' should be present in param')
END DO

! We are not checking for timeCompo
! because this information can also come from timefedof
! we do not check it in initate method also
! because the default value is 1_I4B

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
TYPE(FEDOFPointer_), ALLOCATABLE :: fedofs(:)
TYPE(TimeFEDOFPointer_), ALLOCATABLE :: timefedofs(:)
INTEGER(I4B) :: tPhysicalVarNames, ii, ierr
TYPE(ParameterList_), POINTER :: sublist
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main
sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(1)')
isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(2)')

CALL GetValue(obj=sublist, prefix=myprefix, key="tPhysicalVarNames", &
              VALUE=tPhysicalVarNames)

ALLOCATE (fedofs(tPhysicalVarNames))
DO ii = 1, tPhysicalVarNames
  fedofs(ii)%ptr => fedof
END DO

ALLOCATE (timefedofs(tPhysicalVarNames))
isok = PRESENT(timefedof)
IF (isok) THEN
  DO ii = 1, tPhysicalVarNames
    timefedofs(ii)%ptr => timefedof
  END DO
  CALL obj%Initiate(param=param, fedof=fedofs, timefedof=timefedofs)
ELSE
  CALL obj%Initiate(param=param, fedof=fedofs)
END IF

DO ii = 1, tPhysicalVarNames
  fedofs(ii)%ptr => NULL()
  timefedofs(ii)%ptr => NULL()
END DO
DEALLOCATE (fedofs)
DEALLOCATE (timefedofs)

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
TYPE(String) :: astr
INTEGER(I4B) :: tPhysicalVarNames, ii, ierr, storageFMT, tSize
INTEGER(I4B), ALLOCATABLE :: timeCompo(:), spaceCompo(:), tNodes(:)
TYPE(ParameterList_), POINTER :: sublist
LOGICAL(LGT) :: isok, istimefedof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

sublist => NULL()
ierr = param%GetSubList(key=myprefix, sublist=sublist)

#ifdef DEBUG_VER
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(1)')
isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  'some error occured in getting sublist(2)')
#endif

CALL obj%CheckEssentialParam(sublist)

! tPhysicalVarNames
CALL GetValue(obj=sublist, prefix=myprefix, key='tPhysicalVarNames', &
              VALUE=tPhysicalVarNames)

! Check
#ifdef DEBUG_VER
ii = SIZE(fedof)
isok = ii .EQ. tPhysicalVarNames
CALL AssertError1(isok, myName, &
                  'Size of fedof('//ToString(ii)// &
                  ') not equal to total number of physical variables ('// &
                  ToString(tPhysicalVarNames)//')')
#endif

! Check
#ifdef DEBUG_VER
DO ii = 1, tPhysicalVarNames
  isok = ASSOCIATED(fedof(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'fedof('//ToString(ii)//')%ptr is not associated!')
END DO
#endif

! allocate
CALL Reallocate(tNodes, tPhysicalVarNames)
CALL Reallocate(timeCompo, tPhysicalVarNames)
CALL Reallocate(spaceCompo, tPhysicalVarNames)
ALLOCATE (physicalVarNames(tPhysicalVarNames))

! physicalVarName
DO ii = 1, tPhysicalVarNames
  astr = ""
  CALL GetValue(obj=sublist, prefix=myprefix, &
                key="physicalVarName"//ToString(ii), VALUE=astr)
  physicalVarNames(ii) (1:1) = astr%slice(1, 1)
END DO

! spaceCompo
CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo", &
              VALUE=spaceCompo)

! timeCompo
timeCompo = 1_I4B
istimefedof = PRESENT(timefedof)
IF (istimefedof) THEN
  DO ii = 1, tPhysicalVarNames
    timeCompo(ii) = timefedof(ii)%ptr%GetTotalDOF()
  END DO

ELSE
  isok = sublist%IsPresent(key=myprefix//"/timeCompo")
  IF (isok) CALL GetValue(obj=sublist, prefix=myprefix, &
                          key="timeCompo", VALUE=timeCompo)
END IF

storageFMT = mystorageformat

tSize = 0
DO ii = 1, tPhysicalVarNames
  tNodes(ii) = fedof(ii)%ptr%GetTotalDOF()
  tSize = tSize + tNodes(ii) * timeCompo(ii) * spaceCompo(ii)
END DO

CALL AbstractNodeFieldSetParam(obj=obj, &
                               dof_tPhysicalVars=tPhysicalVarNames, &
                               dof_storageFMT=storageFMT, &
                               dof_spaceCompo=spaceCompo, &
                               dof_timeCompo=timeCompo, &
                               dof_tNodes=tNodes, &
                               dof_names_char=physicalVarNames, &
                               tSize=tSize)

CALL AbstractNodeFieldInitiate(obj=obj, param=param, fedof=fedof, &
                               timefedof=timefedof)

tsize = .tdof.obj%dof
CALL Reallocate(obj%idofs, tsize)
obj%idofs = Arange(1_I4B, tsize)

astr = ""
IF (ALLOCATED(physicalVarNames)) DEALLOCATE (physicalVarNames)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(tNodes)) DEALLOCATE (tNodes)
sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                           Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
