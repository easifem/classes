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
!

SUBMODULE(AbstractKernel_Class) InitiateFieldsMethods
USE BaseMethod, ONLY: Reallocate
USE KernelUtility, ONLY: KernelInitiateTangentMatrix,  &
  & KernelInitiateSpaceMatrix
USE FieldFactory, ONLY: MatrixFieldFactory, AbstractMatrixFieldFactory,  &
  & InitiateVectorFields, InitiateScalarFields, InitiateMatrixFields
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                      InitiateTangentMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateTangentMatrix
CHARACTER(*), PARAMETER :: myName = "obj_InitiateTangentMatrix()"
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

isok = ASSOCIATED(obj%linsol)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: AbstractElasticity_::obj%linsol is not'//  &
    & " associated. It means configuration problem.")
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: AbstractElasticity_::obj%dom is not'//  &
    & " associated. It means configuration problem.")
  RETURN
END IF

isok = ASSOCIATED(obj%tanmat)
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'Allocating AbstractKernel_::obj%tanmat as follows...'//  &
    & CHAR_LF//'  Calling ')
  obj%tanmat => AbstractMatrixFieldFactory(engine=obj%engine%chars(),  &
    & name="MATRIX")
END IF

CALL KernelInitiateTangentMatrix(mat=obj%tanmat,  &
  & linsol=obj%linsol, dom=obj%dom, nsd=obj%nsd, nnt=obj%nnt,  &
  & engine=obj%engine%chars(), name="tanmat",  &
  & matrixProp=obj%tanmatProp%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateTangentMatrix

!----------------------------------------------------------------------------
!                                               InitiateScalarFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateScalarFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateScalarFields()"
LOGICAL(LGT) :: problem, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

problem = ALLOCATED(obj%scalarFields)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%scalarFields'// &
    & ' already allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom is not associated.')
  RETURN
END IF

problem = obj%nsd .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%nsd is zero.')
  RETURN
END IF

!INFO: Initiate method from FieldFactory
CALL InitiateScalarFields(obj=obj%scalarFields, names=names,  &
  & fieldType=typeField%normal, engine=obj%engine%chars(),  &
  & dom=obj%dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_InitiateScalarFields

!----------------------------------------------------------------------------
!                                                       InitiateVectorFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateVectorFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateVectorFields()"
LOGICAL(LGT) :: problem, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

problem = ALLOCATED(obj%vectorFields)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%vectorFields '// &
    & ' already allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom is not associated.')
  RETURN
END IF

problem = obj%nsd .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%nsd is zero.')
  RETURN
END IF

!INFO: Initiate method from FieldFactory
CALL InitiateVectorFields(obj=obj%vectorFields, names=names,  &
  & spaceCompo=obj%nsd, fieldType=typeField%normal,  &
  & engine=obj%engine%chars(), dom=obj%dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_InitiateVectorFields

!----------------------------------------------------------------------------
!                                                      InitiateMatrixFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMatrixFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMatrixFields()"
LOGICAL(LGT) :: problem, isok
INTEGER(I4B) :: tnames, ii
INTEGER(I4B), ALLOCATABLE :: fieldType(:)
TYPE(String), ALLOCATABLE :: engine(:)
TYPE(DomainPointer_), ALLOCATABLE :: dom(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

problem = ALLOCATED(obj%matrixFields)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%matrixFields '// &
    & ' already allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom is not associated.')
  RETURN
END IF

problem = obj%nsd .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%nsd is zero.')
  RETURN
END IF

tnames = SIZE(names)
ALLOCATE (obj%matrixFields(tnames))
CALL Reallocate(fieldType, tnames)
CALL Reallocate(engine, tnames)
ALLOCATE (dom(tnames))
fieldType = typeField%normal

DO ii = 1, tnames
  engine(ii) = obj%engine
  dom(ii)%ptr => obj%dom
END DO

!INFO: Initiate method from FieldFactory
CALL InitiateMatrixFields(obj=obj%matrixFields,  &
  & names=names,  &
  & matrixProps=matrixProp,  &
  & spaceCompo=spaceCompo,  &
  & timeCompo=timeCompo,  &
  & fieldType=fieldType,  &
  & engine=engine,  &
  & dom=dom)

DO ii = 1, tnames
  dom(ii)%ptr => NULL()
  engine(ii) = ""
END DO
DEALLOCATE (dom)
DEALLOCATE (fieldType)
DEALLOCATE (engine)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateMatrixFields

!----------------------------------------------------------------------------
!                                                            InitiateFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFields"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%InitiateTangentMatrix()
CALL obj%InitiateMaterialProperties()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateFields

END SUBMODULE InitiateFieldsMethods
