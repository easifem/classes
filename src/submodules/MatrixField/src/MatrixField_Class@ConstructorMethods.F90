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
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       setMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setMatrixFieldParam
INTEGER(I4B) :: ierr0
ierr0 = param%set(key=myPrefix//"/name", VALUE=TRIM(name))
ierr0 = param%set(key=myPrefix//"/matrixProp", VALUE=TRIM(matrixProp))
ierr0 = param%set(key=myPrefix//"/engine", VALUE=TRIM(engine))
ierr0 = param%set(key=myPrefix//"/spaceCompo",  &
  &  VALUE=INPUT(option=spaceCompo, default=1))
ierr0 = param%set(key=myPrefix//"/timeCompo",  &
  & VALUE=INPUT(option=timeCompo, default=1))
ierr0 = param%set(key=myPrefix//"/fieldType", VALUE=INPUT(  &
  & option=fieldType, default=FIELD_TYPE_NORMAL))
ierr0 = param%set(key=myPrefix//"/comm", VALUE=INPUT(  &
  & option=comm, default=0_I4B))
ierr0 = param%set(key=myPrefix//"/local_n", VALUE=INPUT(  &
  & option=local_n, default=0_I4B))
ierr0 = param%set(key=myPrefix//"/global_n", VALUE=INPUT(  &
  & option=global_n, default=0_I4B))
END PROCEDURE setMatrixFieldParam

!----------------------------------------------------------------------------
!                                                 setMatrixFieldPrecondParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setMatrixFieldPrecondParam
INTEGER(I4B) :: ierr0
CHARACTER(*), PARAMETER :: myName = "setMatrixFieldPrecondParam"
!
ierr0 = param%set(key="Precond/name", VALUE=name)
ierr0 = param%set(key="Precond/engine", VALUE=TRIM(engine))
ierr0 = param%set(key="Precond/comm", VALUE=INPUT(  &
  & option=comm, default=0_I4B))
ierr0 = param%set(key="Precond/local_n", VALUE=INPUT(  &
  & option=local_n, default=0_I4B))
ierr0 = param%set(key="Precond/global_n", VALUE=INPUT(  &
  & option=global_n, default=0_I4B))

SELECT CASE (name)
CASE (PRECOND_ILUT)
  IF (.NOT. PRESENT(droptol) .OR. &
    & .NOT. PRESENT(lfil)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'for PRECOND_ILUT droptol and lfil should be present!!!')
  ELSE
    ierr0 = param%set(key="Precond/droptol", VALUE=droptol)
    ierr0 = param%set(key="Precond/lfil", VALUE=lfil)
  END IF
CASE (PRECOND_ILUTP)
  IF (.NOT. PRESENT(droptol) .OR. &
    & .NOT. PRESENT(lfil) .OR. &
    & .NOT. PRESENT(permtol) .OR. &
    & .NOT. PRESENT(mbloc)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
     & 'for PRECOND_ILUTP droptol, lfil, permtol, mbloc should be present!!!')
  ELSE
    ierr0 = param%set(key="Precond/droptol", VALUE=droptol)
    ierr0 = param%set(key="Precond/lfil", VALUE=lfil)
    ierr0 = param%set(key="Precond/permtol", VALUE=permtol)
    ierr0 = param%set(key="Precond/mbloc", VALUE=mbloc)
  END IF
CASE (PRECOND_ILUD)
  IF (.NOT. PRESENT(droptol) .OR. &
    & .NOT. PRESENT(alpha)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
     & 'for PRECOND_ILUTP droptol and alpha should be present!!!')
  ELSE
    ierr0 = param%set(key="Precond/droptol", VALUE=droptol)
    ierr0 = param%set(key="Precond/alpha", VALUE=alpha)
  END IF
CASE (PRECOND_ILUDP)
  IF (.NOT. PRESENT(droptol) .OR. &
    & .NOT. PRESENT(alpha) .OR. &
    & .NOT. PRESENT(permtol) .OR. &
    & .NOT. PRESENT(mbloc)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILUTP droptol, alpha, permtol, mbloc should be present!!!')
  ELSE
    ierr0 = param%set(key="Precond/droptol", VALUE=droptol)
    ierr0 = param%set(key="Precond/alpha", VALUE=alpha)
    ierr0 = param%set(key="Precond/permtol", VALUE=permtol)
    ierr0 = param%set(key="Precond/mbloc", VALUE=mbloc)
  END IF
CASE (PRECOND_ILUK)
  IF (.NOT. PRESENT(lfil)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'for PRECOND_ILUK lfil should be present!!!')
  ELSE
    ierr0 = param%set(key="Precond/lfil", VALUE=lfil)
  END IF
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for given precondition name')
END SELECT

END PROCEDURE setMatrixFieldPrecondParam

!----------------------------------------------------------------------------
!                                               SetRectangleMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetRectangleMatrixFieldParam
INTEGER(I4B) :: ierr0, ii
CHARACTER(*), PARAMETER :: myName = "SetRectangleMatrixFieldParam"
!
ierr0 = param%set(key=myPrefix//"/name", VALUE=TRIM(name))
!
ierr0 = param%set(key=myPrefix//"/matrixProp", &
  & VALUE=TRIM(matrixProp))
!
ierr0 = param%set(key=myPrefix//"/engine", &
  & VALUE=TRIM(engine))
!
ii = SIZE(physicalVarNames)
ierr0 = param%set(key=myPrefix//"/tPhysicalVarNames", VALUE=ii)
!
DO ii = 1, SIZE(physicalVarNames)
  ierr0 = param%set(key=myPrefix//"/physicalVarName"//TOSTRING(ii), &
    & VALUE=physicalVarNames(ii))
END DO
!
ierr0 = param%set(key=myPrefix//"/spaceCompo",  &
  &  VALUE=spaceCompo)
!
ierr0 = param%set(key=myPrefix//"/timeCompo",  &
  & VALUE=timeCompo)
!
ierr0 = param%set(key=myPrefix//"/fieldType", VALUE=INPUT( &
  & option=fieldType, default=FIELD_TYPE_NORMAL))

ierr0 = param%set(key=myPrefix//"/comm", VALUE=INPUT(  &
  & option=comm, default=0_I4B))
ierr0 = param%set(key=myPrefix//"/local_n", VALUE=INPUT(  &
  & option=local_n, default=0_I4B))
ierr0 = param%set(key=myPrefix//"/global_n", VALUE=INPUT(  &
  & option=global_n, default=0_I4B))
!
END PROCEDURE SetRectangleMatrixFieldParam

!----------------------------------------------------------------------------
!                                             MatrixFieldCheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixFieldCheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "MatrixFieldCheckEssentialParam"

IF (.NOT. param%isPresent(key=myPrefix//"/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/name should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/matrixProp")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/matrixProp should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/engine")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/engine should be present in param')
END IF

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

END PROCEDURE MatrixFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                    RectangleMatrixFieldCheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE RectangleMatrixFieldCheckEssentialParam
CHARACTER(*), PARAMETER :: myName = &
  &  "RectangleMatrixFieldCheckEssentialParam"
INTEGER(I4B) :: ii, n

IF (.NOT. param%isPresent(key=myPrefix//"/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/name should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/matrixProp")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/matrixProp should be present in param')
END IF

IF (.NOT. param%isPresent(key=myPrefix//"/engine")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & myPrefix//'/engine should be present in param')
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

END PROCEDURE RectangleMatrixFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate1
CHARACTER(*), PARAMETER :: myName = "mField_Initiate1"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: nrow, ncol, nnz
INTEGER(I4B) :: storageFMT
INTEGER(I4B) :: tNodes(1)
INTEGER(I4B) :: timeCompo(1)
INTEGER(I4B) :: spaceCompo(1)
CHARACTER(:), ALLOCATABLE :: char_var
CHARACTER(1) :: names_char(1)
TYPE(DOF_) :: dofobj

! main program
IF (obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Matrix field object is already initiated')
CALL obj%checkEssentialParam(param)

! engine
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key=myPrefix//"/engine")) :: char_var)
ierr = param%get(key=myPrefix//"/engine", VALUE=char_var)
obj%engine = char_var
DEALLOCATE (char_var)

! name
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key=myPrefix//"/name")) :: char_var)
ierr = param%get(key=myPrefix//"/name", VALUE=char_var)
obj%name = char_var
names_char(1) (1:1) = char_var(1:1)
DEALLOCATE (char_var)

! fieldType
IF (param%isPresent(key=myPrefix//"/fieldType")) THEN
  ierr = param%get(key=myPrefix//"/fieldType", VALUE=obj%fieldType)
ELSE
  obj%fieldType = FIELD_TYPE_NORMAL
END IF

! spaceCompo
ierr = param%get(key=myPrefix//"/spaceCompo", VALUE=spaceCompo(1))

! timeCompo
ierr = param%get(key=myPrefix//"/timeCompo", VALUE=timeCompo(1))

! storage format
storageFMT = FMT_NODES
tNodes = dom%getTotalNodes()

! make [[DOF_]]
CALL Initiate(obj=dofobj, tNodes=tNodes, names=names_char, &
  & spaceCompo=spaceCompo, timeCompo=timeCompo, &
  & storageFMT=storageFMT)

! matrixProp
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key=myPrefix//"/matrixProp")) :: char_var)
ierr = param%get(key=myPrefix//"/matrixProp", VALUE=char_var)
nrow = tNodes(1) * spaceCompo(1) * timeCompo(1)
ncol = nrow
obj%domain => dom

CALL Initiate( &
  & obj=obj%mat, &
  & nrow=nrow, &
  & ncol=ncol, &
  & idof=dofobj, &
  & jdof=dofobj, &
  & matrixProp=char_var)

DEALLOCATE (char_var)
obj%isInitiated = .TRUE.
obj%isPmatInitiated = .FALSE.
obj%isRectangle = .FALSE.

! setting the sparsity
CALL obj%domain%setSparsity(mat=obj%mat)

! comm
ierr = param%get(key=myprefix//"/comm", VALUE=obj%comm)
ierr = param%get(key=myprefix//"/global_n", VALUE=obj%global_n)
ierr = param%get(key=myprefix//"/local_n", VALUE=obj%local_n)

IF (obj%local_n .EQ. 0) THEN
  obj%local_n = nrow
END IF
IF (obj%global_n .EQ. 0) THEN
  obj%global_n = nrow
END IF

CALL DEALLOCATE (dofobj)
END PROCEDURE mField_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate2
CHARACTER(*), PARAMETER :: myName = "mField_Initiate2"

SELECT TYPE (obj2)
CLASS IS (MatrixField_)

  IF (.NOT. obj2%isInitiated .OR. obj%isInitiated) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either obj is already initiated or obj2 is not initiated!')
  END IF

  CALL AbstractFieldInitiate2( &
    & obj=obj, &
    & obj2=obj2, &
    & copyFull=copyFull, &
    & copyStructure=copyStructure, &
    & usePointer=usePointer)

  obj%mat = obj2%mat
  obj%isPmatInitiated = .FALSE.
  obj%isRectangle = obj2%isRectangle

CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'obj2 should an instance of MatrixField_ or its child')
END SELECT
END PROCEDURE mField_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate3
CHARACTER(*), PARAMETER :: myName = "mField_Initiate3"
INTEGER(I4B), PARAMETER :: tVar = 2
INTEGER(I4B) :: ierror
INTEGER(I4B) :: nrow, ncol, nnz
INTEGER(I4B) :: storageFMT
INTEGER(I4B) :: tNodes(tVar)
INTEGER(I4B) :: timeCompo(tVar)
INTEGER(I4B) :: spaceCompo(tVar)
INTEGER(I4B) :: ii
CHARACTER(1) :: physicalVarNames(tVar)
CHARACTER(:), ALLOCATABLE :: matrixProp
CHARACTER(:), ALLOCATABLE :: char_var
TYPE(DOF_) :: idofobj, jdofobj

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[START] Initiate()')

IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField_::obj is already initiated')
END IF

CALL RectangleMatrixFieldCheckEssentialParam(obj, param)

! matrixProp
ALLOCATE (CHARACTER(param%DataSizeInBytes(  &
  & key=myprefix//"/matrixProp")) :: matrixProp)
ierror = param%get(key=myprefix//"/matrixProp", VALUE=matrixProp)
!
IF (TRIM(matrixProp) .NE. "RECTANGLE") THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'matrixProp should be RECTANGLE')
END IF

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
obj%name = char_var
DEALLOCATE (char_var)

! fieldType
ierror = param%get(key=myprefix//"/fieldType", VALUE=obj%fieldType)

! check domain
IF (SIZE(dom) .NE. tVar) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of dom should be equal to 2, that is two domains.')
END IF

DO ii = 1, tVar
  IF (.NOT. ASSOCIATED(dom(ii)%ptr)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'dom( '//TOSTRING(ii)//')%ptr is NOT ASSOCIATED!')
  END IF
END DO

! physicalVarName
DO ii = 1, tVar
  ALLOCATE (CHARACTER(param%DataSizeInBytes( &
    & key=myPrefix//"/physicalVarName"//TOSTRING(ii))) :: char_var)

  ierror = param%get(key=myPrefix//"/physicalVarName" &
    & //TOSTRING(ii), VALUE=char_var)

  physicalVarNames(ii) (1:1) = char_var(1:1)

  DEALLOCATE (char_var)
END DO

! spaceCompo
IF (param%isPresent(key=myPrefix//"/spaceCompo")) THEN
  ierror = param%get(key=myPrefix//"/spaceCompo", VALUE=spaceCompo)
END IF

! timeCompo
IF (param%isPresent(key=myPrefix//"/timeCompo")) THEN
  ierror = param%get(key=myPrefix//"/timeCompo", VALUE=timeCompo)
END IF

! storage format
storageFMT = FMT_NODES

! domains
ALLOCATE (obj%domains(tvar))
DO ii = 1, tVar
  obj%domains(ii)%ptr => dom(ii)%ptr
  tNodes(ii) = obj%domains(ii)%ptr%getTotalNodes()
END DO

! make [[DOF_]]
CALL Initiate( &
  & obj=idofobj, &
  & tNodes=tNodes(1:1), &
  & names=physicalVarNames(1:1), &
  & spaceCompo=spaceCompo(1:1), &
  & timeCompo=timeCompo(1:1), &
  & storageFMT=storageFMT)

CALL Initiate( &
  & obj=jdofobj, &
  & tNodes=tNodes(2:2), &
  & names=physicalVarNames(2:2), &
  & spaceCompo=spaceCompo(2:2), &
  & timeCompo=timeCompo(2:2), &
  & storageFMT=storageFMT)

! CSRMatrix/Initiate
nrow = .tNodes.idofobj
ncol = .tNodes.jdofobj

CALL Initiate( &
  & obj=obj%mat, &
  & nrow=nrow, &
  & ncol=ncol, &
  & idof=idofobj, &
  & jdof=jdofobj, &
  & matrixProp=matrixProp)

DEALLOCATE (matrixProp)

obj%isInitiated = .TRUE.
obj%isPmatInitiated = .FALSE.
obj%isRectangle = .TRUE.

! setting the sparsity
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

CALL DEALLOCATE (idofobj)
CALL DEALLOCATE (jdofobj)
END PROCEDURE mField_Initiate3

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Deallocate
INTEGER(I4B) :: ierr
CALL AbstractMatrixFieldDeallocate(obj)
CALL DEALLOCATE (obj%mat)
CALL Pmat_Deallocate(obj%Pmat)
obj%isRectangle = .FALSE.
END PROCEDURE mField_Deallocate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Pmat_Deallocate
obj%PmatName = 0
IF (ALLOCATED(obj%A)) DEALLOCATE (obj%A)
IF (ALLOCATED(obj%JA)) DEALLOCATE (obj%JA)
IF (ALLOCATED(obj%IA)) DEALLOCATE (obj%IA)
IF (ALLOCATED(obj%JU)) DEALLOCATE (obj%JU)
IF (ALLOCATED(obj%IPERM)) DEALLOCATE (obj%IPERM)
IF (ALLOCATED(obj%LEVS)) DEALLOCATE (obj%LEVS)
obj%nnz = 0
obj%ncol = 0
obj%nrow = 0
obj%isInitiated = .FALSE.
obj%lfil = 0
obj%mbloc = 0
obj%alpha = 0
obj%droptol = 0
obj%permtol = 0
END PROCEDURE Pmat_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Final
CALL obj%DEALLOCATE()
END PROCEDURE mField_Final

END SUBMODULE ConstructorMethods
