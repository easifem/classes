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
USE Domain_Class, ONLY: DomainSetSparsity
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    SetBlockMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBlockMatrixFieldParam
INTEGER(I4B) :: ierr0, ii
CHARACTER(*), PARAMETER :: myName = "SetBlockMatrixFieldParam"

IF (ANY([SIZE(physicalVarNames), SIZE(spaceCompo), SIZE(timeCompo)]  &
  & .NE. SIZE(physicalVarNames))) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of physicalVarNames, spaceCompo, and timeCompo should be same')
END IF

ierr0 = param%Set(key=myPrefix//"/name", VALUE=TRIM(name))
ierr0 = param%Set(key=myPrefix//"/engine", VALUE=TRIM(engine))

ierr0 = param%Set(key=myPrefix//"/matrixProp", &
  & VALUE=TRIM(matrixProp))

ii = SIZE(physicalVarNames)

ierr0 = param%Set(key=myPrefix//"/tPhysicalVarNames", VALUE=ii)

DO ii = 1, SIZE(physicalVarNames)
  ierr0 = param%Set(key=myPrefix//"/physicalVarName"//TOSTRING(ii), &
    & VALUE=physicalVarNames(ii))
END DO

ierr0 = param%Set(key=myPrefix//"/spaceCompo",  &
  &  VALUE=spaceCompo)

ierr0 = param%Set(key=myPrefix//"/timeCompo",  &
  & VALUE=timeCompo)

ierr0 = param%Set(key=myPrefix//"/fieldType", VALUE=INPUT( &
  & option=fieldType, default=FIELD_TYPE_NORMAL))

ierr0 = param%Set(key=myPrefix//"/comm", VALUE=INPUT( &
& option=comm, default=0_I4B))

ierr0 = param%Set(key=myPrefix//"/global_n", VALUE=INPUT( &
& option=global_n, default=0_I4B))

ierr0 = param%Set(key=myPrefix//"/local_n", VALUE=INPUT( &
& option=local_n, default=0_I4B))

END PROCEDURE SetBlockMatrixFieldParam

!----------------------------------------------------------------------------
!                                           SetBlockMatrixFieldPrecondParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBlockMatrixFieldPrecondParam
CALL SetMatrixFieldPrecondParam( &
  & param=param, &
  & name=name, &
  & engine=engine, &
  & lfil=lfil, &
  & mbloc=mbloc, &
  & droptol=droptol, &
  & permtol=permtol, &
  & alpha=alpha, &
  & comm=comm, local_n=local_n, global_n=global_n)
END PROCEDURE SetBlockMatrixFieldPrecondParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_checkEssentialParam"
INTEGER(I4B) :: ii, n

IF (.NOT. param%isPresent(key=myPrefix//"/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/name should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/engine")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/engine should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/matrixProp")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/matrixProp should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/tPhysicalVarNames")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/tPhysicalVarNames should be present in param')
ELSE
  ii = param%get(key=myPrefix//'/tPhysicalVarNames', VALUE=n)
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/spaceCompo should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/timeCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/timeCompo should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/fieldType")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/fieldType should be present in param')
END IF

DO ii = 1, n
  IF (.NOT. param%isPresent(key=myPrefix//"/physicalVarName" &
    & //TOSTRING(ii))) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & myPrefix//'/physicalVarName' &
    & //TOSTRING(ii) &
    & //' should be present in param')
  END IF
END DO

IF (.NOT. param%isPresent(key=myPrefix//"/comm")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/comm should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/global_n")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/global_n should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/local_n")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/local_n should be present in param')
END IF

END PROCEDURE obj_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1"
TYPE(AbstractDomainPointer_), ALLOCATABLE :: domains(:)
INTEGER(I4B) :: tPhysicalVarNames, ii

ii = param%get(key="BlockMatrixField/tPhysicalVarNames", &
  & VALUE=tPhysicalVarNames)

ALLOCATE (domains(tPhysicalVarNames))

DO ii = 1, tPhysicalVarNames
  domains(ii)%ptr => dom
END DO

CALL obj%Initiate(param=param, dom=domains)

DO ii = 1, tPhysicalVarNames
  domains(ii)%ptr => NULL()
END DO

IF (ALLOCATED(domains)) DEALLOCATE (domains)

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3"
INTEGER(I4B) :: ierror, nrow, ncol, storageFMT, tVar, ii, nnz
INTEGER(I4B), ALLOCATABLE :: tNodes(:), timeCompo(:), spaceCompo(:)
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
CHARACTER(:), ALLOCATABLE :: char_var
CHARACTER(:), ALLOCATABLE :: matProp
TYPE(DOF_) :: dofobj

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[START] Initiate()')

IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
   & 'The instance of BlockMatrixField is already initiated')
END IF

CALL obj%checkEssentialParam(param)

! engine
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key=myPrefix//"/engine")) :: char_var)
ierror = param%get(key=myPrefix//"/engine", VALUE=char_var)
obj%engine = char_var
DEALLOCATE (char_var)

! name
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key=myprefix//"/name")) :: char_var)
ierror = param%get(key=myprefix//"/name", VALUE=char_var)
obj%name = char_var; DEALLOCATE (char_var)

! fieldType
ierror = param%get(key=myprefix//"/fieldType",  &
  & VALUE=obj%fieldType)

! tPhysicalVarNames
ierror = param%get(key=myprefix//'/tPhysicalVarNames', VALUE=tVar)

! domain
IF (SIZE(dom) .NE. tVar) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of dom not equal to the total number of physical variables')
DO ii = 1, tVar
  IF (.NOT. ASSOCIATED(dom(ii)%ptr)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'dom( '//TOSTRING(ii)//')%ptr is NOT ASSOCIATED!')
  END IF
END DO

! allocate
ALLOCATE (tNodes(tVar), timeCompo(tVar), spaceCompo(tVar), &
  & physicalVarNames(tVar))

! physicalVarName
DO ii = 1, tVar
  ALLOCATE (CHARACTER(param%DataSizeInBytes( &
    & key=myprefix//"/physicalVarName"//TOSTRING(ii))) :: char_var)
  ierror = param%get(key=myprefix//"/physicalVarName" &
    & //TOSTRING(ii), VALUE=char_var)
  physicalVarNames(ii) (1:1) = char_var(1:1); DEALLOCATE (char_var)
END DO

! spaceCompo
ierror = param%get(key=myprefix//"/spaceCompo", VALUE=spaceCompo)

! timeCompo
ierror = param%get(key=myprefix//"/timeCompo", VALUE=timeCompo)

! storage format
storageFMT = FMT_DOF

! domains
ALLOCATE (obj%domains(tvar))
DO ii = 1, tVar
  obj%domains(ii)%ptr => dom(ii)%ptr
  tNodes(ii) = obj%domains(ii)%ptr%getTotalNodes()
END DO

! make [[DOF_]]
CALL Display("Calling Initiating dofobj")
CALL Initiate( &
  & obj=dofobj, &
  & tNodes=tNodes,&
  & names=physicalVarNames, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo, &
  & storageFMT=storageFMT)

! matrixProp
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key=myprefix//"/matrixProp")) :: matProp)
ierror = param%get(key=myprefix//"/matrixProp", VALUE=matProp)

! CSRMatrix/Initiate
CALL Display("Initiating CSRMatrix_")
nrow = .tNodes.dofobj
ncol = nrow
CALL Initiate(obj=obj%mat, nrow=nrow, ncol=ncol, idof=dofobj, &
  & jdof=dofobj, matrixProp=matProp)
DEALLOCATE (matProp)
obj%isInitiated = .TRUE.
obj%isPmatInitiated = .FALSE.

! Setting the sparsity
CALL Display("Calling DomainSetSparsity()")
CALL DomainSetSparsity(mat=obj%mat, domains=obj%domains)

! comm
ierror = param%get(key=myprefix//"/comm", VALUE=obj%comm)
ierror = param%get(key=myprefix//"/global_n", VALUE=obj%global_n)
ierror = param%get(key=myprefix//"/local_n", VALUE=obj%local_n)

IF (obj%local_n .EQ. 0) THEN
  obj%local_n = nrow
END IF
IF (obj%global_n .EQ. 0) THEN
  obj%global_n = nrow
END IF

!cleanup

CALL DEALLOCATE (dofobj)
IF (ALLOCATED(tNodes)) DEALLOCATE (tNodes)
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(physicalVarNames)) DEALLOCATE (physicalVarNames)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[END] Initiate()')

END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

END SUBMODULE ConstructorMethods
