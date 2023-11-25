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

SUBMODULE(AbstractField_Class) ConstructorMethods
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldCheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "AbstractFieldCheckEssentialParam()"
TYPE(String) :: astr
TYPE(String), ALLOCATABLE :: essentialParam(:)
INTEGER(I4B) :: ii

astr = "/name/engine/fieldType/comm/local_n/global_n"
CALL astr%Split(essentialParam, sep="/")
CALL CheckEssentialParam( &
  & obj=param,  &
  & keys=essentialParam,  &
  & prefix=prefix,  &
  & myName=myName,  &
  & modName=modName)
! INFO: CheckEssentialParam param is defined in easifemClasses FPL_Method

IF (ALLOCATED(essentialParam)) THEN
  DO ii = 1, SIZE(essentialParam)
    essentialParam(ii) = ""
  END DO
  DEALLOCATE (essentialParam)
END IF
astr = ""
END PROCEDURE AbstractFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                       SetScalarFieldParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractFieldParam
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr
CHARACTER(*), PARAMETER :: myName = "SetAbstractFieldParam()"
LOGICAL(LGT) :: isSublist

sublist => NULL()

! Create a new sublist
isSublist = param%isSubList(prefix)

IF (isSublist) THEN
  ierr = param%GetSubList(key=prefix, sublist=sublist)
  IF (ierr .NE. 0) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
  END IF
ELSE
  sublist => param%NewSubList(key=prefix)
END IF

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
END IF

CALL Set(obj=sublist, datatype="Char", prefix=prefix, key="name", VALUE=name)

CALL Set(obj=sublist, datatype="Char", prefix=prefix, key="engine",  &
  & VALUE=engine)

CALL Set(obj=sublist, datatype=TypeIntI4B, prefix=prefix, key="fieldType", &
  & VALUE=input(option=fieldType, default=FIELD_TYPE_NORMAL))

CALL Set(obj=sublist, datatype=TypeIntI4B, prefix=prefix, key="comm", &
  & VALUE=input(option=fieldType, default=0_I4B))

CALL Set(obj=sublist, datatype=TypeIntI4B, prefix=prefix, key="local_n", &
  & VALUE=input(option=local_n, default=0_I4B))

CALL Set(obj=sublist, datatype=TypeIntI4B, prefix=prefix, key="global_n", &
  & VALUE=input(option=global_n, default=0_I4B))

sublist => NULL()
END PROCEDURE SetAbstractFieldParam

!----------------------------------------------------------------------------
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldInitiate
CHARACTER(*), PARAMETER :: myName = "AbstractFieldInitiate()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr

! main
sublist => NULL()
ierr = param%GetSubList(key=prefix, sublist=sublist)
IF (ierr .NE. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(1)')
END IF

! NOTE: We should not call deallocate in abstract classes.
! This is because, in concrete classes we may set some
! parameters before calling this method.
! All those parameters will be gone if we call deallocate
! here.
! CALL obj%DEALLOCATE()

IF (.NOT. ASSOCIATED(sublist)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured in getting sublist(2)')
END IF

obj%isInitiated = .TRUE.
CALL GetValue(obj=sublist, prefix=prefix, key="fieldType", VALUE=obj%fieldType)
CALL GetValue(obj=sublist, prefix=prefix, key="name", VALUE=obj%name)
CALL GetValue(obj=sublist, prefix=prefix, key="engine", VALUE=obj%engine)
CALL GetValue(obj=sublist, prefix=prefix, key="comm", VALUE=obj%comm)
CALL GetValue(obj=sublist, prefix=prefix, key="global_n", VALUE=obj%global_n)
CALL GetValue(obj=sublist, prefix=prefix, key="local_n", VALUE=obj%local_n)
obj%domain => dom
sublist => NULL()
END PROCEDURE AbstractFieldInitiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Initiate2
CHARACTER(*), PARAMETER :: myName = "aField_Initiate2"
INTEGER(I4B) :: ii, tsize

IF (.NOT. obj2%isInitiated .OR. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either obj is already initiated or obj2 is not initiated!')
END IF
obj%isInitiated = obj2%isInitiated
obj%fieldType = obj2%fieldType
obj%name = obj2%name
obj%engine = obj2%engine
obj%comm = obj2%comm
obj%myRank = obj2%myRank
obj%numProcs = obj2%numProcs
obj%global_n = obj2%global_n
obj%local_n = obj2%local_n
obj%is = obj2%is
obj%ie = obj2%ie
obj%lis_ptr = obj2%lis_ptr
obj%domain => obj2%domain
IF (ALLOCATED(obj2%domains)) THEN
  tsize = SIZE(obj2%domains)
  ALLOCATE (obj%domains(tsize))
  DO ii = 1, tsize
    obj%domains(ii)%ptr => obj2%domains(ii)%ptr
  END DO
END IF
END PROCEDURE aField_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Deallocate
INTEGER(I4B) :: ii
obj%name = ""
obj%engine = ""
obj%isInitiated = .FALSE.
obj%fieldType = FIELD_TYPE_NORMAL
obj%comm = 0
obj%myRank = 0
obj%numProcs = 1
obj%global_n = 0
obj%local_n = 0
obj%is = 0
obj%ie = 0
obj%lis_ptr = 0
obj%domain => NULL()
IF (ALLOCATED(obj%domains)) THEN
  DO ii = 1, SIZE(obj%domains)
    obj%domains(ii)%ptr => NULL()
  END DO
  DEALLOCATE (obj%domains)
END IF
END PROCEDURE aField_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FIELD_TYPE_NUMBER
SELECT CASE (TRIM(name))
CASE ("NORMAL")
  ans = FIELD_TYPE_NORMAL
CASE ("CONSTANT")
  ans = FIELD_TYPE_CONSTANT
CASE ("CONSTANT_SPACE")
  ans = FIELD_TYPE_CONSTANT_SPACE
CASE ("CONSTANT_TIME")
  ans = FIELD_TYPE_CONSTANT_TIME
END SELECT
END PROCEDURE FIELD_TYPE_NUMBER

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FIELD_TYPE_NAME
!
SELECT CASE (id)
CASE (FIELD_TYPE_NORMAL)
  ans = "NORMAL"
CASE (FIELD_TYPE_CONSTANT)
  ans = "CONSTANT"
CASE (FIELD_TYPE_CONSTANT_SPACE)
  ans = "CONSTANT_SPACE"
CASE (FIELD_TYPE_CONSTANT_TIME)
  ans = "CONSTANT_TIME"
END SELECT
END PROCEDURE FIELD_TYPE_NAME

END SUBMODULE ConstructorMethods
