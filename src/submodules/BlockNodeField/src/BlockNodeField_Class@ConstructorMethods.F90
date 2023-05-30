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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         setBlockNodeField
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBlockNodeFieldParam
CHARACTER(*), PARAMETER :: myName = "SetBlockNodeFieldParam"
INTEGER(I4B) :: ierr0, ii
IF (ANY([SIZE(physicalVarNames), SIZE(spaceCompo), SIZE(timeCompo)]  &
  & .NE. SIZE(physicalVarNames))) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of physicalVarNames, spaceCompo, and timeCompo should be &
  & same')
END IF
ierr0 = param%set(key=myprefix//"/name", VALUE=TRIM(name))
ierr0 = param%set(key=myprefix//"/engine", VALUE=TRIM(engine))
ii = SIZE(physicalVarNames)
ierr0 = param%set(key=myprefix//"/tPhysicalVarNames", VALUE=ii)
DO ii = 1, SIZE(physicalVarNames)
  ierr0 = param%set(key=myprefix//"/physicalVarName"//TOSTRING(ii),  &
    & VALUE=physicalVarNames(ii))
END DO
ierr0 = param%set(key=myprefix//"/spaceCompo",  &
  &  VALUE=spaceCompo)
ierr0 = param%set(key=myprefix//"/timeCompo",  &
  & VALUE=timeCompo)
ierr0 = param%set(key=myprefix//"/fieldType", VALUE=INPUT( &
  & option=fieldType, default=FIELD_TYPE_NORMAL))
IF (PRESENT(comm)) THEN
  ierr0 = param%set(key=myprefix//"/comm", VALUE=comm)
ELSE
  ierr0 = param%set(key=myprefix//"/comm", VALUE=0_I4B)
END IF
IF (PRESENT(local_n)) THEN
  ierr0 = param%set(key=myprefix//"/local_n", VALUE=local_n)
ELSE
  ierr0 = param%set(key=myprefix//"/local_n", VALUE=0_I4B)
END IF
IF (PRESENT(global_n)) THEN
  ierr0 = param%set(key=myprefix//"/global_n", VALUE=global_n)
ELSE
  ierr0 = param%set(key=myprefix//"/global_n", VALUE=0_I4B)
END IF
END PROCEDURE SetBlockNodeFieldParam

!----------------------------------------------------------------------------
!                                                       checkEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "bnField_checkEssentialParam"
INTEGER(I4B) :: ii, n

IF (.NOT. param%isPresent(key=myprefix//"/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/name should be present in param')
END IF
IF (.NOT. param%isPresent(key=myprefix//"/engine")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/engine should be present in param')
END IF
IF (.NOT. param%isPresent(key=myprefix//"/tPhysicalVarNames")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/tPhysicalVarNames should be present in param')
ELSE
  ii = param%get(key=myprefix//'/tPhysicalVarNames', VALUE=n)
END IF
IF (.NOT. param%isPresent(key=myprefix//"/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/spaceCompo should be present in param')
END IF
IF (.NOT. param%isPresent(key=myprefix//"/timeCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/timeCompo should be present in param')
END IF
IF (.NOT. param%isPresent(key=myprefix//"/fieldType")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/fieldType should be present in param')
END IF
DO ii = 1, n
  IF (.NOT. param%isPresent(key=myprefix//"/physicalVarName" &
    & //TOSTRING(ii))) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & myprefix//'/physicalVarName' &
    & //TOSTRING(ii) &
    & //' should be present in param')
  END IF
END DO
END PROCEDURE bnField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Initiate1
CHARACTER(*), PARAMETER :: myName = "bnField_Initiate1"
TYPE(DomainPointer_), ALLOCATABLE :: domains(:)
INTEGER(I4B) :: tPhysicalVarNames, ii
ii = param%get(key=myprefix//"/tPhysicalVarNames", &
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
END PROCEDURE bnField_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Initiate3
CHARACTER(*), PARAMETER :: myName = "bnField_Initiate3"
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
CHARACTER(:), ALLOCATABLE :: char_var
INTEGER(I4B) :: tVar, ii, ierror, storageFMT
INTEGER(I4B), ALLOCATABLE :: timeCompo(:), spaceCompo(:), tNodes(:)

IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is already initiated')
END IF

CALL obj%checkEssentialParam(param)

! engine
obj%engine = "NATIVE_SERIAL"

! engine
ALLOCATE (CHARACTER( &
  & param%DataSizeInBytes(key=myprefix//"/engine")) :: char_var)
ierror = param%get(key=myprefix//"/engine", VALUE=char_var)
obj%engine = char_var
DEALLOCATE (char_var)

! name
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key=myprefix//"/name")) :: char_var)
ierror = param%get(key=myprefix//"/name", VALUE=char_var)
obj%name = char_var
DEALLOCATE (char_var)

! fieldType
ierror = param%get(key=myprefix//"/fieldType",  &
  & VALUE=obj%fieldType)

! tPhysicalVarNames
ierror = param%get(key=myprefix//'/tPhysicalVarNames', VALUE=tVar)

! check
IF (SIZE(dom) .NE. tVar) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of dom not equal to the total number of physical variables')

! check
DO ii = 1, tVar
  IF (.NOT. ASSOCIATED(dom(ii)%ptr)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'dom( '//TOSTRING(ii)//')%ptr is NOT ASSOCIATED!')
  END IF
END DO

! allocate
CALL Reallocate(tNodes, tVar)
CALL Reallocate(timeCompo, tVar)
CALL Reallocate(spaceCompo, tVar)
ALLOCATE (physicalVarNames(tVar))

! physicalVarName
DO ii = 1, tVar
  ALLOCATE (CHARACTER(param%DataSizeInBytes( &
    & key=myprefix//"/physicalVarName"//TOSTRING(ii))) :: char_var)
  ierror = param%get(key=myprefix//"/physicalVarName" &
    & //TOSTRING(ii), VALUE=char_var)
  physicalVarNames(ii) (1:1) = char_var(1:1); DEALLOCATE (char_var)
END DO

! spaceCompo
IF (param%isPresent(key=myprefix//"/spaceCompo")) THEN
  ierror = param%get(key=myprefix//"/spaceCompo", VALUE=spaceCompo)
END IF

! timeCompo
IF (param%isPresent(key=myprefix//"/timeCompo")) THEN
  ierror = param%get(key=myprefix//"/timeCompo", VALUE=timeCompo)
ELSE
  timeCompo = 1
END IF

! storage format
storageFMT = FMT_DOF

! domains, tNodes
ALLOCATE (obj%domains(tvar))
obj%tSize = 0

DO ii = 1, tVar
  obj%domains(ii)%ptr => dom(ii)%ptr
  tNodes(ii) = obj%domains(ii)%ptr%getTotalNodes()
  obj%tSize = obj%tSize + tNodes(ii) * timeCompo(ii) * spaceCompo(ii)
END DO

IF (obj%local_n .EQ. 0) THEN
  obj%local_n = obj%tSize
END IF
IF (obj%global_n .EQ. 0) THEN
  obj%global_n = obj%tSize
END IF

! tNodes for constant field
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  tNodes(:) = 1
END IF

! DOF_
CALL Initiate(obj=obj%dof, tNodes=tNodes, names=physicalVarNames, &
  & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT)

! realVec
CALL Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%isInitiated = .TRUE.
DEALLOCATE (tNodes, spaceCompo, timeCompo, physicalVarNames)
END PROCEDURE bnField_Initiate3

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Final
CALL obj%DEALLOCATE()
END PROCEDURE bnField_Final

END SUBMODULE ConstructorMethods
