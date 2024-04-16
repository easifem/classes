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
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         setBlockNodeField
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBlockNodeFieldParam
CHARACTER(*), PARAMETER :: myName = "SetBlockNodeFieldParam()"
INTEGER(I4B) :: ierr, ii, intvec(3)
LOGICAL(LGT) :: isNOTOK
TYPE(ParameterList_), POINTER :: sublist

intvec = [SIZE(physicalVarNames), SIZE(spaceCompo), SIZE(timeCompo)]
isNOTOK = ANY(intvec .NE. SIZE(physicalVarNames))

IF (isNOTOK) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: Size of physicalVarNames, spaceCompo, '//  &
  & 'and timeCompo should be same.')
  RETURN
END IF

CALL SetAbstractFieldParam( &
  & param=param, &
  & prefix=myprefix, &
  & name=name, &
  & engine=engine, &
  & fieldType=fieldType, &
  & comm=comm, &
  & local_n=local_n, &
  & global_n=global_n)

sublist => NULL()
ierr = param%GetSubList(key=myprefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in Getting sublist(1)')
  RETURN
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in Getting sublist(2)')
END IF

CALL Set(obj=sublist, datatype=1_I4B, prefix=myprefix, &
  & key="tPhysicalVarNames", VALUE=intvec(1))

DO ii = 1, SIZE(physicalVarNames)
  CALL Set(obj=sublist, datatype="char", prefix=myprefix,  &
    & key="physicalVarName"//tostring(ii), VALUE=physicalVarNames(ii))
END DO

CALL Set(obj=sublist, datatype=[1_I4B], prefix=myprefix, key="spaceCompo",  &
  & VALUE=spaceCompo)

CALL Set(obj=sublist, datatype=[1_I4B], prefix=myprefix, key="timeCompo",  &
  & VALUE=timeCompo)

sublist => NULL()
END PROCEDURE SetBlockNodeFieldParam

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam"
INTEGER(I4B) :: ii, n

CALL AbstractFieldCheckEssentialParam(obj=obj, param=param, prefix=myprefix)

IF (.NOT. param%IsPresent(key=myprefix//"/spaceCompo")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & myprefix//'/spaceCompo should be present in param')
  RETURN
END IF

IF (.NOT. param%IsPresent(key=myprefix//"/timeCompo")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & myprefix//'/timeCompo should be present in param')
  RETURN
END IF

n = 0
IF (.NOT. param%IsPresent(key=myprefix//"/tPhysicalVarNames")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & myprefix//'/tPhysicalVarNames should be present in param')
  RETURN
ELSE
  ii = param%Get(key=myprefix//'/tPhysicalVarNames', VALUE=n)
END IF

DO ii = 1, n
  IF (.NOT. param%IsPresent(key=myprefix//"/physicalVarName" &
    & //TOSTRING(ii))) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & myprefix//'/physicalVarName'//TOSTRING(ii) &
    & //' should be present in param')
  END IF
END DO

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
TYPE(AbstractDomainPointer_), ALLOCATABLE :: domains(:)
INTEGER(I4B) :: tPhysicalVarNames, ii, ierr
TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Initiate()')
#endif

! main
sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
  RETURN
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
  RETURN
END IF

CALL GetValue(obj=sublist, prefix=myprefix, key="tPhysicalVarNames",  &
  & VALUE=tPhysicalVarNames)

ALLOCATE (domains(tPhysicalVarNames))
DO ii = 1, tPhysicalVarNames
  domains(ii)%ptr => dom
END DO

CALL obj%Initiate(param=param, dom=domains)

DO ii = 1, tPhysicalVarNames
  domains(ii)%ptr => NULL()
END DO
DEALLOCATE (domains)

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Initiate()')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3"
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
TYPE(String) :: astr
INTEGER(I4B) :: tPhysicalVarNames, ii, ierr, storageFMT, tSize
INTEGER(I4B), ALLOCATABLE :: timeCompo(:), spaceCompo(:), tNodes(:)
TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Initiate()')
#endif

IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: BlockNodeField_::obj is already initiated.')
  RETURN
END IF

! main
sublist => NULL()

ierr = param%GetSubList(key=myprefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
  RETURN
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
  RETURN
END IF

CALL obj%CheckEssentialParam(sublist)
CALL obj%DEALLOCATE()

! tPhysicalVarNames
CALL GetValue(obj=sublist, prefix=myprefix, key='tPhysicalVarNames',  &
  & VALUE=tPhysicalVarNames)

! Check
ii = SIZE(dom)
IF (ii .NE. tPhysicalVarNames) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERRPR] :: Size of dom ('//tostring(ii)//  &
    & ') not equal to total number of physical variables ('//  &
    & tostring(tPhysicalVarNames)//')')
  RETURN
END IF

! Check
DO ii = 1, tPhysicalVarNames
  IF (.NOT. ASSOCIATED(dom(ii)%ptr)) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR] :: dom( '//TOSTRING(ii)//  &
      & ')%ptr is NOT ASSOCIATED!')
    RETURN
  END IF
END DO

! allocate
CALL Reallocate(tNodes, tPhysicalVarNames)
CALL Reallocate(timeCompo, tPhysicalVarNames)
CALL Reallocate(spaceCompo, tPhysicalVarNames)
ALLOCATE (physicalVarNames(tPhysicalVarNames))

! physicalVarName
DO ii = 1, tPhysicalVarNames
  astr = ""
  CALL GetValue(obj=sublist, prefix=myprefix,  &
    & key="physicalVarName"//tostring(ii), VALUE=astr)
  physicalVarNames(ii) (1:1) = astr%slice(1, 1)
END DO

! spaceCompo
CALL GetValue(obj=sublist, prefix=myprefix, key="spaceCompo",  &
  & VALUE=spaceCompo)

! timeCompo
timeCompo = 1_I4B
CALL GetValue(obj=sublist, prefix=myprefix, key="timeCompo",  &
  & VALUE=timeCompo)

storageFMT = DOF_FMT

tSize = 0
DO ii = 1, tPhysicalVarNames
  tNodes(ii) = dom(ii)%ptr%GetTotalNodes()
  tSize = tSize + tNodes(ii) * timeCompo(ii) * spaceCompo(ii)
END DO

CALL AbstractNodeFieldSetParam( &
  & obj=obj,  &
  & dof_tPhysicalVars=tPhysicalVarNames,  &
  & dof_storageFMT=storageFMT,  &
  & dof_spaceCompo=spaceCompo,  &
  & dof_timeCompo=timeCompo,  &
  & dof_tNodes=tNodes,  &
  & dof_names_char=physicalVarNames,  &
  & tSize=tSize)

CALL AbstractNodeFieldInitiate(obj=obj, param=param, dom=dom)

astr = ""
IF (ALLOCATED(physicalVarNames)) DEALLOCATE (physicalVarNames)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(tNodes)) DEALLOCATE (tNodes)
sublist => NULL()
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

END SUBMODULE ConstructorMethods
