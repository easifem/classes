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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[BlockMatrixField_]]

SUBMODULE(BlockMatrixField_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    setBlockMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setBlockMatrixFieldParam
INTEGER(I4B) :: ierr0, ii
CHARACTER(*), PARAMETER :: myName = "setBlockMatrixFieldParam"
! main
!
! check
!
#ifdef DEBUG_VER
IF (ANY([SIZE(physicalVarNames), SIZE(spaceCompo), SIZE(timeCompo)]  &
  & .NE. SIZE(physicalVarNames))) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of physicalVarNames, spaceCompo, and timeCompo should be same')
END IF
#endif
!
ierr0 = param%set(key="BlockMatrixField/name", VALUE=TRIM(name))
ierr0 = param%set(key="BlockMatrixField/matrixProp", &
  & VALUE=TRIM(matrixProp))
ii = SIZE(physicalVarNames)
ierr0 = param%set(key="BlockMatrixField/tPhysicalVarNames", VALUE=ii)
DO ii = 1, SIZE(physicalVarNames)
  ierr0 = param%set(key="BlockMatrixField/physicalVarName"//TOSTRING(ii), &
    & VALUE=physicalVarNames(ii))
END DO
ierr0 = param%set(key="BlockMatrixField/spaceCompo",  &
  &  VALUE=spaceCompo)
ierr0 = param%set(key="BlockMatrixField/timeCompo",  &
  & VALUE=timeCompo)
ierr0 = param%set(key="BlockMatrixField/fieldType", VALUE=INPUT( &
  & option=fieldType, default=FIELD_TYPE_NORMAL))
END PROCEDURE setBlockMatrixFieldParam

!----------------------------------------------------------------------------
!                                           setBlockMatrixFieldPrecondParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setBlockMatrixFieldPrecondParam
CALL setBlockMatrixFieldPrecondParam( &
  & param=param, &
  & name=name, &
  & lfil=lfil, &
  & mbloc=mbloc, &
  & droptol=droptol, &
  & permtol=permtol, &
  & alpha=alpha)
END PROCEDURE setBlockMatrixFieldPrecondParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bmField_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "bmField_checkEssentialParam"
INTEGER(I4B) :: ii, n
!
IF (.NOT. param%isPresent(key="BlockMatrixField/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockMatrixField/name should be present in param')
END IF
!
IF (.NOT. param%isPresent(key="BlockMatrixField/matrixProp")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockMatrixField/matrixProp should be present in param')
END IF
!
IF (.NOT. param%isPresent(key="BlockMatrixField/tPhysicalVarNames")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockMatrixField/tPhysicalVarNames should be present in param')
ELSE
  ii = param%get(key='BlockMatrixField/tPhysicalVarNames', VALUE=n)
END IF
!
IF (.NOT. param%isPresent(key="BlockMatrixField/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockMatrixField/spaceCompo should be present in param')
END IF
!
IF (.NOT. param%isPresent(key="BlockMatrixField/timeCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockMatrixField/timeCompo should be present in param')
END IF
!
IF (.NOT. param%isPresent(key="BlockMatrixField/fieldType")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockMatrixField/fieldType should be present in param')
END IF
!
DO ii = 1, n
  IF (.NOT. param%isPresent(key="BlockMatrixField/physicalVarName" &
    & //TOSTRING(ii))) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'BlockMatrixField/physicalVarName' &
    & //TOSTRING(ii) &
    & //' should be present in param')
  END IF
END DO
END PROCEDURE bmField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate1
CHARACTER(*), PARAMETER :: myName = "mField_Initiate1"
TYPE(DomainPointer_), ALLOCATABLE :: domains(:)
INTEGER(I4B) :: tPhysicalVarNames, ii
!
ii = param%get(key="BlockMatrixField/tPhysicalVarNames", &
  & VALUE=tPhysicalVarNames)
!
ALLOCATE (domains(tPhysicalVarNames))
!
DO ii = 1, tPhysicalVarNames
  domains(ii)%ptr => dom
END DO
!
CALL obj%Initiate(param=param, dom=domains)
!
DO ii = 1, tPhysicalVarNames
  domains(ii)%ptr => NULL()
END DO
!
IF (ALLOCATED(domains)) DEALLOCATE (domains)
!
END PROCEDURE mField_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate2
CHARACTER(*), PARAMETER :: myName = "mField_Initiate2"
CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[:WIP] This routine is under construction!')
! SELECT TYPE (obj2)
! CLASS IS (BlockMatrixField_)
! END SELECT
END PROCEDURE mField_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate3
CHARACTER(*), PARAMETER :: myName = "mField_Initiate3"
INTEGER(I4B) :: ierror, nrow, ncol, storageFMT, tVar, ii
INTEGER(I4B), ALLOCATABLE :: tNodes(:), timeCompo(:), spaceCompo(:)
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
CHARACTER(:), ALLOCATABLE :: char_var
CHARACTER(:), ALLOCATABLE :: matProp
TYPE(DOF_) :: dofobj
!
! check
!
IF (obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The instance of BlockMatrixField is already initiated')
!
CALL obj%checkEssentialParam(param)
!
! engine
!
obj%engine = "NATIVE_SERIAL"
!
! name
!
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key="BlockMatrixField/name")) :: char_var)
ierror = param%get(key="BlockMatrixField/name", VALUE=char_var)
obj%name = char_var; DEALLOCATE (char_var)
!
! fieldType
!
ierror = param%get(key="BlockMatrixField/fieldType",  &
  & VALUE=obj%fieldType)
!
! tPhysicalVarNames
!
ierror = param%get(key='BlockMatrixField/tPhysicalVarNames', VALUE=tVar)
!
! domain
!
IF (SIZE(dom) .NE. tVar) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of dom not equal to the total number of physical variables')
DO ii = 1, tVar
  IF (.NOT. ASSOCIATED(dom(ii)%ptr)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'dom( '//TOSTRING(ii)//')%ptr is NOT ASSOCIATED!')
  END IF
END DO
!
! allocate
!
ALLOCATE (tNodes(tVar), timeCompo(tVar), spaceCompo(tVar), &
  & physicalVarNames(tVar))
!
! physicalVarName
!
DO ii = 1, tVar
  ALLOCATE (CHARACTER(param%DataSizeInBytes( &
    & key="BlockMatrixField/physicalVarName"//TOSTRING(ii))) :: char_var)
  ierror = param%get(key="BlockMatrixField/physicalVarName" &
    & //TOSTRING(ii), VALUE=char_var)
  physicalVarNames(ii) (1:1) = char_var(1:1); DEALLOCATE (char_var)
END DO
!
! spaceCompo
!
ierror = param%get(key="BlockMatrixField/spaceCompo", VALUE=spaceCompo)
!
! timeCompo
!
ierror = param%get(key="BlockMatrixField/timeCompo", VALUE=timeCompo)
!
! storage format
!
storageFMT = FMT_DOF
!
! domains
!
ALLOCATE (obj%domains(tvar))
DO ii = 1, tVar
  obj%domains(ii)%ptr => dom(ii)%ptr
  tNodes(ii) = obj%domains(ii)%ptr%getTotalNodes()
END DO
!
! make [[DOF_]]
!
CALL Initiate(obj=dofobj, tNodes=tNodes, names=physicalVarNames, &
  & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT)
!
! matrixProp
!
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key="BlockMatrixField/matrixProp")) :: matProp)
ierror = param%get(key="BlockMatrixField/matrixProp", VALUE=matProp)
!
! CSRMatrix/Initiate
!
nrow = .tNodes.dofobj
ncol = nrow
CALL Initiate(obj=obj%mat, nrow=nrow, ncol=ncol, idof=dofobj, &
  & jdof=dofobj, matrixProp=matProp)
DEALLOCATE (matProp)
obj%isInitiated = .TRUE.
obj%isPmatInitiated = .FALSE.
!
! setting the sparsity
!
CALL DomainSetSparsity(mat=obj%mat, domains=obj%domains)
CALL DEALLOCATE (dofobj)
IF (ALLOCATED(tNodes)) DEALLOCATE (tNodes)
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(physicalVarNames)) DEALLOCATE (physicalVarNames)
END PROCEDURE mField_Initiate3

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Deallocate
CALL AbstractMatrixFieldDeallocate(obj)
END PROCEDURE mField_Deallocate

!----------------------------------------------------------------------------
!                                                                Final
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Final
CALL obj%DEALLOCATE()
END PROCEDURE mField_Final

END SUBMODULE ConstructorMethods
