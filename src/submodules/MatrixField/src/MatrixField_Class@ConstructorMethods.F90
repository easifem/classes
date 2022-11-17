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
!                                                    setMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setMatrixFieldParam
INTEGER(I4B) :: ierr
ierr = param%set(key="MatrixField/name", value=TRIM(name))
ierr = param%set(key="MatrixField/matrixProp", value=TRIM(matrixProp))
ierr = param%set(key="MatrixField/spaceCompo",  &
  &  value=INPUT(option=spaceCompo, default=1))
ierr = param%set(key="MatrixField/timeCompo",  &
  & value=INPUT(option=timeCompo, default=1))
ierr = param%set(key="MatrixField/fieldType", value=INPUT(  &
  & option=fieldType, default=FIELD_TYPE_NORMAL))
END PROCEDURE setMatrixFieldParam

!----------------------------------------------------------------------------
!                                               SetRectangleMatrixFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetRectangleMatrixFieldParam
INTEGER(I4B) :: ierr0, ii
CHARACTER(LEN=*), PARAMETER :: myName = "SetRectangleMatrixFieldParam"
!!
ierr0 = param%set(key="MatrixField/name", value=TRIM(name))
!!
ierr0 = param%set(key="MatrixField/matrixProp", &
  & value=TRIM(matrixProp))
!!
ii = SIZE(physicalVarNames)
ierr0 = param%set(key="MatrixField/tPhysicalVarNames", value=ii)
!!
DO ii = 1, SIZE(physicalVarNames)
  ierr0 = param%set(key="MatrixField/physicalVarName"//TOSTRING(ii), &
    & value=physicalVarNames(ii))
END DO
!!
ierr0 = param%set(key="MatrixField/spaceCompo",  &
  &  value=spaceCompo)
!!
ierr0 = param%set(key="MatrixField/timeCompo",  &
  & value=timeCompo)
!!
ierr0 = param%set(key="MatrixField/fieldType", value=INPUT( &
  & option=fieldType, default=FIELD_TYPE_NORMAL))
!!
END PROCEDURE SetRectangleMatrixFieldParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_addSurrogate
CALL e%addSurrogate(UserObj)
END PROCEDURE mField_addSurrogate

!----------------------------------------------------------------------------
!                                             MatrixFieldCheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixFieldCheckEssentialParam
CHARACTER(LEN=*), PARAMETER :: myName = "MatrixFieldCheckEssentialParam"
IF (.NOT. param%isPresent(key="MatrixField/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField/name should be present in param')
END IF
IF (.NOT. param%isPresent(key="MatrixField/matrixProp")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField/matrixProp should be present in param')
END IF
END PROCEDURE MatrixFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                    RectangleMatrixFieldCheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE RectangleMatrixFieldCheckEssentialParam
CHARACTER(LEN=*), PARAMETER :: myName = &
  &  "RectangleMatrixFieldCheckEssentialParam"
INTEGER(I4B) :: ii, n
!!
IF (.NOT. param%isPresent(key="MatrixField/name")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField/name should be present in param')
END IF
!!
IF (.NOT. param%isPresent(key="MatrixField/matrixProp")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField/matrixProp should be present in param')
END IF
!!
IF (.NOT. param%isPresent(key="MatrixField/tPhysicalVarNames")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField/tPhysicalVarNames should be present in param')
ELSE
  ii = param%get(key='MatrixField/tPhysicalVarNames', value=n)
END IF
!!
IF (.NOT. param%isPresent(key="MatrixField/spaceCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField/spaceCompo should be present in param')
END IF
!!
IF (.NOT. param%isPresent(key="MatrixField/timeCompo")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField/timeCompo should be present in param')
END IF
!!
IF (.NOT. param%isPresent(key="MatrixField/fieldType")) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MatrixField/fieldType should be present in param')
END IF
!!
DO ii = 1, n
  IF (.NOT. param%isPresent(key="MatrixField/physicalVarName" &
    & //TOSTRING(ii))) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'MatrixField/physicalVarName' &
    & //TOSTRING(ii) &
    & //' should be present in param')
  END IF
END DO
!!
END PROCEDURE RectangleMatrixFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate1
CHARACTER(LEN=*), PARAMETER :: myName = "mField_Initiate1"
INTEGER(I4B) :: ierror, nrow, ncol, storageFMT, tNodes(1), &
  & timeCompo(1), spaceCompo(1)
CHARACTER(LEN=:), ALLOCATABLE :: char_var
CHARACTER(LEN=1) :: names_char(1)
TYPE(DOF_) :: dofobj
!!
!! main program
!!
IF (obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Matrix field object is already initiated')
CALL obj%checkEssentialParam(param)
!!
!! engine
!!
obj%engine = "NATIVE_SERIAL"
!!
!! name
!!
ALLOCATE (CHARACTER(LEN=param%DataSizeInBytes(  &
  & key="MatrixField/name")) :: char_var)
ierror = param%get(key="MatrixField/name", value=char_var)
obj%name = char_var
names_char(1) (1:1) = char_var(1:1)
DEALLOCATE (char_var)
!!
!! fieldType
!!
IF (param%isPresent(key="MatrixField/fieldType")) THEN
  ierror = param%get(key="MatrixField/fieldType", value=obj%fieldType)
ELSE
  obj%fieldType = FIELD_TYPE_NORMAL
END IF
!!
!! spaceCompo
!!
ierror = param%get(key="MatrixField/spaceCompo", value=spaceCompo(1))
!!
!! timeCompo
!!
ierror = param%get(key="MatrixField/timeCompo", value=timeCompo(1))
!!
!! storage format
!!
storageFMT = FMT_NODES
tNodes = dom%getTotalNodes()
!!
!! make [[DOF_]]
!!
CALL initiate(obj=dofobj, tNodes=tNodes, names=names_char, &
  & spaceCompo=spaceCompo, timeCompo=timeCompo, storageFMT=storageFMT)
!!
!! matrixProp
!!
ALLOCATE (CHARACTER(LEN=param%DataSizeInBytes(  &
  & key="MatrixField/matrixProp")) :: char_var)
ierror = param%get(key="MatrixField/matrixProp", value=char_var)
nrow = tNodes(1) * spaceCompo(1) * timeCompo(1)
ncol = nrow
obj%domain => dom
!!
CALL initiate(obj=obj%mat, nrow=nrow, ncol=ncol, idof=dofobj, &
  & jdof=dofobj, matrixProp=char_var)
!!
DEALLOCATE (char_var)
obj%isInitiated = .TRUE.
obj%isPmatInitiated = .FALSE.
!!
!! setting the sparsity
!!
CALL obj%domain%setSparsity(mat=obj%mat)
CALL Deallocate (dofobj)
END PROCEDURE mField_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate2
CHARACTER(LEN=*), PARAMETER :: myName = "mField_Initiate2"
SELECT TYPE (obj2)
CLASS IS (MatrixField_)
  IF (.NOT. obj2%isInitiated .OR. obj%isInitiated) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either obj is already initiated or obj2 is not initiated!')
  obj%isInitiated = .TRUE.
  obj%name = obj2%name
  obj%fieldType = obj2%fieldType
  obj%mat = obj2%mat
  obj%isPmatInitiated = .FALSE.
  obj%engine = obj2%engine
END SELECT
END PROCEDURE mField_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Initiate3
CHARACTER(LEN=*), PARAMETER :: myName = "mField_Initiate3"
INTEGER(I4B), PARAMETER :: tVar = 2
INTEGER(I4B) :: ierror, nrow, ncol, storageFMT, tNodes(tVar), &
  & timeCompo(tVar), spaceCompo(tVar), ii
CHARACTER(LEN=1) :: physicalVarNames(2)
CHARACTER(LEN=:), ALLOCATABLE :: matrixProp
CHARACTER(LEN=:), ALLOCATABLE :: char_var
TYPE(DOF_) :: idofobj, jdofobj
!!
!! check
!!
IF (obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Matrix field object is already initiated')
!!
CALL RectangleMatrixFieldCheckEssentialParam(obj, param)
!!
!! matrixProp
!!
ALLOCATE (CHARACTER(LEN=param%DataSizeInBytes(  &
  & key="MatrixField/matrixProp")) :: matrixProp)
ierror = param%get(key="MatrixField/matrixProp", value=matrixProp)
!!
IF (TRIM(matrixProp) .NE. "RECTANGLE") THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'matrixProp should be RECTANGLE')
END IF
!!
!! engine
!!
obj%engine = "NATIVE_SERIAL"
!!
!! name
!!
ALLOCATE (CHARACTER(LEN=param%DataSizeInBytes(  &
  & key="MatrixField/name")) :: char_var)
ierror = param%get(key="MatrixField/name", value=char_var)
obj%name = char_var
DEALLOCATE (char_var)
!!
!! fieldType
!!
ierror = param%get(key="MatrixField/fieldType", value=obj%fieldType)
!!
!! check domain
!!
IF (SIZE(dom) .NE. tVar) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of dom should be equal to 2, that is two domains.')
!!
DO ii = 1, tVar
  IF (.NOT. ASSOCIATED(dom(ii)%ptr)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'dom( '//TOSTRING(ii)//')%ptr is NOT ASSOCIATED!')
  END IF
END DO
!!
!! physicalVarName
!!
DO ii = 1, tVar
  !!
  ALLOCATE (CHARACTER(LEN=param%DataSizeInBytes( &
    & key="MatrixField/physicalVarName"//TOSTRING(ii))) :: char_var)
  !!
  ierror = param%get(key="MatrixField/physicalVarName" &
    & //TOSTRING(ii), value=char_var)
  !!
  physicalVarNames(ii) (1:1) = char_var(1:1)
  !!
  DEALLOCATE (char_var)
  !!
END DO
!!
!! spaceCompo
!!
IF (param%isPresent(key="MatrixField/spaceCompo")) THEN
  ierror = param%get(key="MatrixField/spaceCompo", value=spaceCompo)
END IF
!!
!! timeCompo
!!
IF (param%isPresent(key="MatrixField/timeCompo")) THEN
  ierror = param%get(key="MatrixField/timeCompo", value=timeCompo)
END IF
!!
!! storage format
!!
storageFMT = FMT_NODES
!!
!! domains
!!
ALLOCATE (obj%domains(tvar))
DO ii = 1, tVar
  obj%domains(ii)%ptr => dom(ii)%ptr
  tNodes(ii) = obj%domains(ii)%ptr%getTotalNodes()
END DO
!!
!! make [[DOF_]]
!!
CALL Initiate( &
  & obj=idofobj, &
  & tNodes=tNodes(1:1), &
  & names=physicalVarNames(1:1), &
  & spaceCompo=spaceCompo(1:1), &
  & timeCompo=timeCompo(1:1), &
  & storageFMT=storageFMT)
!!
CALL Initiate( &
  & obj=jdofobj, &
  & tNodes=tNodes(2:2), &
  & names=physicalVarNames(2:2), &
  & spaceCompo=spaceCompo(2:2), &
  & timeCompo=timeCompo(2:2), &
  & storageFMT=storageFMT)
!!
!! CSRMatrix/Initiate
!!
nrow = .tNodes.idofobj
ncol = .tNodes.jdofobj
!!
CALL Initiate( &
  & obj=obj%mat, &
  & nrow=nrow, &
  & ncol=ncol, &
  & idof=idofobj, &
  & jdof=jdofobj, &
  & matrixProp=matrixProp)
!!
DEALLOCATE (matrixProp)
!!
obj%isInitiated = .TRUE.
obj%isPmatInitiated = .FALSE.
!!
!! setting the sparsity
!!
CALL DomainSetSparsity(mat=obj%mat, domains=obj%domains)
CALL Deallocate (idofobj)
CALL Deallocate (jdofobj)
END PROCEDURE mField_Initiate3

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Deallocate
CALL Deallocate (obj%mat)
CALL Deallocate (obj%Pmat)
CALL AbstractMatrixFieldDeallocate(obj)
END PROCEDURE mField_Deallocate

!----------------------------------------------------------------------------
!                                                            Deallocate
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
END SUBMODULE ConstructorMethods
