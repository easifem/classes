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

USE CPUTime_Class, ONLY: CPUTime_

USE AbstractKernelParam, ONLY: TypeKernelProblemOpt

USE AbstractField_Class, ONLY: TypeField

USE KernelMatrixField_Method, ONLY: KernelInitiateTangentMatrix

USE AbstractKernelParam, ONLY: TypeKernelProblemOpt

USE FieldFactory, ONLY: AbstractMatrixFieldFactory, &
                        InitiateScalarFields, &
                        InitiateSTScalarFields, &
                        InitiateVectorFields, &
                        InitiateSTVectorFields, &
                        InitiateMatrixFields

USE ScalarField_Class, ONLY: ScalarFieldSafeAllocate
USE STScalarField_Class, ONLY: STScalarFieldSafeAllocate
USE VectorField_Class, ONLY: VectorFieldSafeAllocate
USE STVectorField_Class, ONLY: STVectorFieldSafeAllocate
USE MatrixField_Class, ONLY: MatrixFieldSafeAllocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                      InitiateTangentMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateTangentMatrix
CHARACTER(*), PARAMETER :: myName = "obj_InitiateTangentMatrix()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: nsd0
TYPE(CPUTime_) :: TypeCPUTime
CHARACTER(*), PARAMETER :: tanmatName = "tanmat"

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%tanmat)
IF (.NOT. isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                    'Allocating AbstractKernel_::obj%tanmat as follows...'// &
                 '  Calling AbstractMatrixFieldFactory('//obj%opt%engine//')')
#endif

  obj%tanmat => AbstractMatrixFieldFactory(engine=obj%opt%engine%chars(), &
                                           name=obj%opt%tanmatName%chars())
END IF

SELECT CASE (obj%opt%problemType)
CASE (TypeKernelProblemOpt%scalar)
  nsd0 = 1_I4B

CASE (TypeKernelProblemOpt%vector)
  nsd0 = obj%opt%nsd

CASE (TypeKernelProblemOpt%multiPhysics)
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: no case found for KernelProblemType')

END SELECT

CALL KernelInitiateTangentMatrix(mat=obj%tanmat, &
                                 linsol=obj%linsol, &
                                 fedof=obj%fedof, &
                                 nsd=nsd0, &
                                 nnt=obj%opt%nnt, &
                                 engine=obj%opt%engine%chars(), &
                                 name=tanmatName, &
                                 matrixProp=obj%opt%tanmatProp%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF

END PROCEDURE obj_InitiateTangentMatrix

!----------------------------------------------------------------------------
!                                                       InitiateScalarFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateScalarFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateScalarFields()"
LOGICAL(LGT) :: problem, isok
INTEGER(I4B) :: tsize
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER

isok = ALLOCATED(obj%fields%scalarFields)
IF (isok) THEN
  tsize = SIZE(obj%fields%scalarFields)
  problem = tsize .LT. SIZE(names)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[INTERNAL ERROR] :: AbstractKernel_::obj%fields%scalarFields'// &
                      ' already allocated and size is not enough.')
    RETURN
  END IF
END IF

problem = obj%opt%nsd .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%nsd is zero.')
  RETURN
END IF

#endif

tsize = SIZE(names)
obj%fields%tScalarFields = tsize
CALL ScalarFieldSafeAllocate(obj=obj%fields%scalarFields, newsize=tsize)
! ScalarFieldSafeAllocate is defined in ScalarField_Class

! InitiateScalarFields is defined in FieldFactory
CALL InitiateScalarFields(obj=obj%fields%scalarFields, names=names, &
                  fieldType=TypeField%normal, engine=obj%opt%engine%chars(), &
                          fedof=obj%fedof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateScalarFields

!----------------------------------------------------------------------------
!                                                    InitiateSTScalarFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateSTScalarFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateSTScalarFields()"
LOGICAL(LGT) :: problem, isok
INTEGER(I4B) :: tsize
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER

isok = ALLOCATED(obj%fields%stScalarFields)

IF (isok) THEN
  tsize = SIZE(obj%fields%stScalarFields)
  problem = tsize .LT. SIZE(names)

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[INTERNAL ERROR] :: AbstractKernel_::obj%fields%stScalarFields'// &
                      ' already allocated but its size is not enough')
    RETURN
  END IF

END IF

problem = (obj%opt%nsd .EQ. 0_I4B) .OR. (obj%opt%nnt .EQ. 0_I4B)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
   '[INTERNAL ERROR] :: AbstractKernel_::obj%opt%nsd or obj%opt%nnt is zero.')
  RETURN
END IF

#endif

tsize = SIZE(names)
obj%fields%tSTScalarFields = tsize
CALL STScalarFieldSafeAllocate(obj=obj%fields%stScalarFields, newsize=tsize)

! Initiate method from FieldFactory
CALL InitiateSTScalarFields(obj=obj%fields%stScalarFields, names=names, &
                  fieldType=TypeField%normal, engine=obj%opt%engine%chars(), &
                            fedof=obj%fedof, timeCompo=obj%opt%nnt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateSTScalarFields

!----------------------------------------------------------------------------
!                                                       InitiateVectorFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateVectorFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateVectorFields()"
LOGICAL(LGT) :: problem, isok
INTEGER(I4B) :: tsize
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER
isok = ALLOCATED(obj%fields%vectorFields)

IF (isok) THEN
  tsize = SIZE(obj%fields%vectorFields)
  problem = tsize .LT. SIZE(names)

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[INTERNAL ERROR] :: AbstractKernel_::obj%fields%vectorFields'// &
                      ' already allocated but its size is not enough')
    RETURN
  END IF

END IF

problem = obj%opt%nsd .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[INTERNAL ERROR] :: AbstractKernel_::obj%opt%nsd is zero.')
  RETURN
END IF

#endif

tsize = SIZE(names)
obj%fields%tVectorFields = tsize
CALL VectorFieldSafeAllocate(obj=obj%fields%vectorFields, newsize=tsize)

CALL InitiateVectorFields(obj=obj%fields%vectorFields, names=names, &
                         spaceCompo=obj%opt%nsd, fieldType=TypeField%normal, &
                          engine=obj%opt%engine%chars(), &
                          fedof=obj%fedof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateVectorFields

!----------------------------------------------------------------------------
!                                                     InitiateSTVectorFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateSTVectorFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateSTVectorFields()"
LOGICAL(LGT) :: problem, isok
INTEGER(I4B) :: tsize
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER

isok = ALLOCATED(obj%fields%stVectorFields)
IF (isok) THEN
  tsize = SIZE(obj%fields%stVectorFields)
  problem = tsize .LT. SIZE(names)

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[INTERNAL ERROR] :: AbstractKernel_::obj%fields%stVectorFields'// &
                      ' already allocated but its size is not enough')
    RETURN
  END IF

END IF

problem = (obj%opt%nsd .EQ. 0_I4B) .OR. (obj%opt%nnt .EQ. 0_I4B)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
           '[INTERNAL ERROR] :: AbstractKernel_::obj%nsd or obj%nnt is zero.')
  RETURN
END IF

#endif

tsize = SIZE(names)
obj%fields%tSTVectorFields = tsize
CALL STVectorFieldSafeAllocate(obj=obj%fields%stVectorFields, newsize=tsize)

CALL InitiateSTVectorFields(obj=obj%fields%stVectorFields, &
            names=names, spaceCompo=obj%opt%nsd, fieldType=TypeField%normal, &
        engine=obj%opt%engine%chars(), fedof=obj%fedof, timeCompo=obj%opt%nnt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateSTVectorFields

!----------------------------------------------------------------------------
!                                                      InitiateMatrixFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMatrixFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMatrixFields()"
LOGICAL(LGT) :: problem, isok
INTEGER(I4B) :: ii, tsize
INTEGER(I4B), ALLOCATABLE :: fieldType(:)
TYPE(String), ALLOCATABLE :: engine(:)
TYPE(FEDOFPointer_), ALLOCATABLE :: fedof(:)
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER

isok = ALLOCATED(obj%fields%matrixFields)

IF (isok) THEN
  tsize = SIZE(obj%fields%matrixFields)
  problem = tsize .LT. SIZE(names)

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[INTERNAL ERROR] :: AbstractKernel_::obj%fields%matrixFields'// &
                      ' already allocated but its size is not enough')
    RETURN
  END IF
END IF

problem = obj%opt%nsd .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: AbstractKernel_::obj%nsd is zero.')
  RETURN
END IF

#endif

tsize = SIZE(names)
obj%fields%tMatrixFields = tsize
CALL MatrixFieldSafeAllocate(obj=obj%fields%matrixFields, newsize=tsize)

ALLOCATE (fieldType(tsize), engine(tsize), fedof(tsize))
DO ii = 1, tsize
  fieldType(ii) = TypeField%normal
END DO

DO ii = 1, tsize
  engine(ii) = obj%opt%engine
  fedof(ii)%ptr => obj%fedof
END DO

!INFO: Initiate method from FieldFactory
CALL InitiateMatrixFields(obj=obj%fields%matrixFields, names=names, &
         matrixProps=matrixProp, spaceCompo=spaceCompo, timeCompo=timeCompo, &
                          fieldType=fieldType, engine=engine, fedof=fedof)

DO ii = 1, tsize
  fedof(ii)%ptr => NULL()
  engine(ii) = ""
END DO

DEALLOCATE (fedof, fieldType, engine)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateMatrixFields

!----------------------------------------------------------------------------
!                                                            InitiateFields
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFields
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFields()"
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%opt%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

CALL obj%InitiateTangentMatrix()
CALL obj%InitiateMaterialProperties()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

IF (obj%opt%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
                              currentTime=obj%opt%currentTime, &
                              currentTimeStep=obj%opt%currentTimeStep, &
                              methodName=myName))
END IF
END PROCEDURE obj_InitiateFields

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE InitiateFieldsMethods
