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

SUBMODULE(AbstractField_Class) Methods
USE BaseMethod
USE FPL_Method, ONLY: GetValue
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Display
INTEGER(I4B) :: ii

CALL Display("#"//TRIM(msg), unitNo=unitNo)

IF (obj%isInitiated) THEN
  CALL Display("# isInitiated : TRUE", unitNo=unitNo)
ELSE
  CALL Display("# isInitiated : FALSE, Nothing to Display!", unitNo=unitNo)
  RETURN
END IF

CALL Display(obj%name//'', msg="# name : ", unitNo=unitNo)

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL Display("# fieldType : CONSTANT", unitNo=unitNo)
ELSE
  CALL Display("# fieldType : NORMAL", unitNo=unitNo)
END IF

CALL Display(obj%engine, msg='# engine : ', unitNo=unitNo)
CALL Display(obj%comm, msg='# comm: ', unitNo=unitNo)
CALL Display(obj%myRank, msg='# myRank: ', unitNo=unitNo)
CALL Display(obj%numProcs, msg='# numProcs: ', unitNo=unitNo)
CALL Display(obj%global_n, msg='# global_n: ', unitNo=unitNo)
CALL Display(obj%local_n, msg='# local_n: ', unitNo=unitNo)
CALL Display(obj%is, msg='# is: ', unitNo=unitNo)
CALL Display(obj%ie, msg='# ie: ', unitNo=unitNo)
CALL Display(obj%lis_ptr, msg='# lis_ptr: ', unitNo=unitNo)

IF (ASSOCIATED(obj%domain)) THEN
  CALL Display("# domain : ASSOCIATED", unitNo=unitNo)
ELSE
  CALL Display("# domain : NOT ASSOCIATED", unitNo=unitNo)
END IF
!
IF (ALLOCATED(obj%domains)) THEN
  CALL Display("# domains : ALLOCATED [" &
    & //TOSTRING(SIZE(obj%domains)) &
    & //"]", unitNo=unitNo)
  DO ii = 1, SIZE(obj%domains)
    IF (ASSOCIATED(obj%domains(ii)%ptr)) THEN
      CALL Display("# domains("//TOSTRING(ii) &
        & //")%ptr : ASSOCIATED", unitNo=unitNo)
    ELSE
      CALL Display("# domains("//TOSTRING(ii)  &
        & //")%ptr : NOT ASSOCIATED", unitNo=unitNo)
    END IF
  END DO
ELSE
  CALL Display("# domains : NOT ALLOCATED", unitNo=unitNo)
END IF

END PROCEDURE aField_Display

!----------------------------------------------------------------------------
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractFieldInitiate
CHARACTER(*), PARAMETER :: myName = "AbstractFieldInitiate()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This routine is under development')
CALL obj%DEALLOCATE()
CALL obj%CheckEssentialParam(param)
obj%isInitiated = .TRUE.
CALL GetValue(obj=param, prefix=prefix, key="fieldType", VALUE=obj%fieldType)
CALL GetValue(obj=param, prefix=prefix, key="name", VALUE=obj%name)
CALL GetValue(obj=param, prefix=prefix, key="engine", VALUE=obj%engine)
CALL GetValue(obj=param, prefix=prefix, key="comm", VALUE=obj%comm)
CALL GetValue(obj=param, prefix=prefix, key="global_n", VALUE=obj%global_n)
CALL GetValue(obj=param, prefix=prefix, key="local_n", VALUE=obj%local_n)
obj%domain => dom
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_WriteData_hdf5
CHARACTER(*), PARAMETER :: myName = "aField_WriteData_hdf5"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method should be implemented by children of AbstractField_')
END PROCEDURE aField_WriteData_hdf5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_WriteData_vtk
CHARACTER(*), PARAMETER :: myName = "aField_WriteData_vtk"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method should be implemented by children of AbstractField_')
END PROCEDURE aField_WriteData_vtk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetParam
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_

CHARACTER(*), PARAMETER :: myName = "GetParam"
INTEGER(I4B) :: ii

IF (PRESENT(isInitiated)) isInitiated = obj%isInitiated
IF (PRESENT(fieldType)) fieldType = obj%fieldType
IF (PRESENT(name)) name = obj%name%chars()
IF (PRESENT(engine)) engine = obj%engine%chars()
IF (PRESENT(comm)) comm = obj%comm
IF (PRESENT(myRank)) myRank = obj%myRank
IF (PRESENT(numProcs)) numProcs = obj%numProcs
IF (PRESENT(global_n)) global_n = obj%global_n
IF (PRESENT(local_n)) local_n = obj%local_n
IF (PRESENT(is)) is = obj%is
IF (PRESENT(ie)) ie = obj%ie
IF (PRESENT(lis_ptr)) lis_ptr = obj%lis_ptr
IF (PRESENT(domain)) domain => obj%domain

IF (PRESENT(domains)) THEN
  IF (.NOT. ALLOCATED(obj%domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'AbstractField_::obj%domains is not allocated ')
  END IF

  IF (SIZE(obj%domains) .NE. SIZE(domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'AbstractField_::obj%domains size is not same as size of domains')
  END IF

  DO ii = 1, SIZE(domains)
    domains(ii)%ptr => obj%domains(ii)%ptr
  END DO
END IF

SELECT TYPE (obj)
CLASS IS (AbstractNodeField_)
  IF (PRESENT(tSize)) tSize = obj%tSize
  IF (PRESENT(realVec)) realVec = obj%realVec
  IF (PRESENT(dof)) dof = obj%dof
CLASS IS (AbstractMatrixField_)
  IF (PRESENT(isPMatInitiated)) isPMatInitiated = obj%isPMatInitiated
END SELECT

END PROCEDURE GetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetParam
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_

CHARACTER(*), PARAMETER :: myName = "SetParam"
INTEGER(I4B) :: ii

IF (PRESENT(isInitiated)) obj%isInitiated = isInitiated
IF (PRESENT(fieldType)) obj%fieldType = fieldType
IF (PRESENT(name)) obj%name = TRIM(name)
IF (PRESENT(engine)) obj%engine = TRIM(engine)
IF (PRESENT(comm)) obj%comm = comm
IF (PRESENT(myRank)) obj%myRank = myRank
IF (PRESENT(numProcs)) obj%numProcs = numProcs
IF (PRESENT(global_n)) obj%global_n = global_n
IF (PRESENT(local_n)) obj%local_n = local_n
IF (PRESENT(is)) obj%is = is
IF (PRESENT(ie)) obj%ie = ie
IF (PRESENT(lis_ptr)) obj%lis_ptr = lis_ptr
IF (PRESENT(domain)) obj%domain => domain
IF (PRESENT(domains)) THEN
  IF (.NOT. ALLOCATED(obj%domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'AbstractField_::Obj%domains is not allocated ')
  END IF

  IF (SIZE(obj%domains) .NE. SIZE(domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'AbstractField_::Obj%domains size is not same as size of domains')
  END IF

  DO ii = 1, SIZE(domains)
    obj%domains(ii)%ptr => domains(ii)%ptr
  END DO
END IF

SELECT TYPE (obj)
CLASS IS (AbstractNodeField_)
  IF (PRESENT(tSize)) obj%tSize = tSize
  IF (PRESENT(realVec)) obj%realVec = realVec
  IF (PRESENT(dof)) obj%dof = dof
CLASS IS (AbstractMatrixField_)
  IF (PRESENT(isPMatInitiated)) obj%isPMatInitiated = isPMatInitiated
END SELECT
END PROCEDURE SetParam

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Export
CHARACTER(*), PARAMETER :: myName = "aField_Export"
TYPE(String) :: dname, matprop

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Instnace of MatrixField_ is not initiated')
END IF

! Check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

! Check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have write permission')
END IF

! fieldType
dname = TRIM(group)//"/fieldType"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=STRING(FIELD_TYPE_NAME(obj%fieldType)))

! name
dname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%name)

! engine
dname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%engine)

! comm
dname = TRIM(group)//"/comm"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%comm)

! myRank
dname = TRIM(group)//"/myRank"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%myRank)

! numProcs
dname = TRIM(group)//"/numProcs"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%numProcs)

! local_n
dname = TRIM(group)//"/local_n"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%local_n)

! global_n
dname = TRIM(group)//"/global_n"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%global_n)

! is
dname = TRIM(group)//"/is"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%is)

! ie
dname = TRIM(group)//"/ie"
CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%ie)

END PROCEDURE aField_Export

!----------------------------------------------------------------------------
!                                                             aField_Import
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Import
CHARACTER(*), PARAMETER :: myName = "aField_Import"
TYPE(String) :: strval, dsetname, matrixProp

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START] Import()')

! main program
IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The instance of AbstractField_ is already initiated')
END IF

! Check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

! Check
IF (.NOT. hdf5%isRead()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
END IF

! fieldType
dsetname = TRIM(group)//"/fieldType"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
  obj%fieldType = FIELD_TYPE_NUMBER(strval%chars())
ELSE
  obj%fieldType = FIELD_TYPE_NORMAL
END IF

! name
dsetname = TRIM(group)//"/name"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset name should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)
END IF

! engine
dsetname = TRIM(group)//"/engine"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  obj%engine = "NATIVE_SERIAL"
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%engine)
END IF

! comm
dsetname = TRIM(group)//"/comm"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  obj%comm = 0
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%comm)
END IF

! myRank
dsetname = TRIM(group)//"/myRank"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  obj%myRank = 0
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%myRank)
END IF

! numProcs
dsetname = TRIM(group)//"/numProcs"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  obj%numProcs = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%numProcs)
END IF

! global_n
dsetname = TRIM(group)//"/global_n"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  obj%global_n = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%global_n)
END IF

! local_n
dsetname = TRIM(group)//"/local_n"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  obj%local_n = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%local_n)
END IF

! is
dsetname = TRIM(group)//"/is"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  obj%is = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%is)
END IF

! ie
dsetname = TRIM(group)//"/ie"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  obj%ie = 1
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%ie)
END IF

IF (ASSOCIATED(obj%domain)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'obj%domain is associated, deallocate first')
END IF

IF (ALLOCATED(obj%domains)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'obj%domains is allocated, deallocate first')
END IF

IF (PRESENT(dom)) THEN
  obj%domain => dom
ELSE IF (PRESENT(domains)) THEN
  ALLOCATE (obj%domains(2))
  obj%domains(1)%ptr => domains(1)%ptr
  obj%domains(2)%ptr => domains(2)%ptr
ELSE
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & "For non-rectangle matrix dom should be present, "// &
    & "for rectangle matrix matrix domains should be present")
END IF

obj%isInitiated = .TRUE.

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[END] Import()')

END PROCEDURE aField_Import

!----------------------------------------------------------------------------
!                                                       GetTotalPhysicalVars
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalPhysicalVars
  CHARACTER(*), PARAMETER :: myName="aField_GetTotalPhysicalVars()"
  CALL e%RaiseError(modName //'::'//myName// ' - '// &
    & '[IMPLEMENTATION ERROR] :: This routine should be implemented by ' //&
    & 'child classes')
END PROCEDURE aField_GetTotalPhysicalVars

!----------------------------------------------------------------------------
!                                                     aField_GetPhysicalNames
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetPhysicalNames
CHARACTER(*), PARAMETER :: myName = "aField_GetNames()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
  & " child classes.")
END PROCEDURE aField_GetPhysicalNames

!----------------------------------------------------------------------------
!                                                           GetSpaceCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetSpaceCompo
CHARACTER(*), PARAMETER :: myName = "aField_GetSpaceCompo()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
  & " child classes.")
END PROCEDURE aField_GetSpaceCompo

!----------------------------------------------------------------------------
!                                                           GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTimeCompo
CHARACTER(*), PARAMETER :: myName = "aField_GetTimeCompo"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
  & " child classes.")
END PROCEDURE aField_GetTimeCompo

!----------------------------------------------------------------------------
!                                                           GetStorageFMT
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetStorageFMT
CHARACTER(*), PARAMETER :: myName = "aField_GetStorageFMT"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
  & " child classes.")
END PROCEDURE aField_GetStorageFMT

!----------------------------------------------------------------------------
!                                                               GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by ' //  &
  & ' child classes')
END PROCEDURE aField_GetTotalDOF

!----------------------------------------------------------------------------
!                                                          GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalVertexDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalVertexDOF()"
CALL e%RaiseError(modName //'::'//myName// ' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by ' //&
  & 'child classes')
END PROCEDURE aField_GetTotalVertexDOF

!----------------------------------------------------------------------------
!                                                          GetTotalEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalEdgeDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalEdgeDOF()"
CALL e%RaiseError(modName //'::'//myName// ' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by ' //&
  & 'child classes')
END PROCEDURE aField_GetTotalEdgeDOF

!----------------------------------------------------------------------------
!                                                          GetTotalFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalFaceDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalFaceDOF()"
CALL e%RaiseError(modName //'::'//myName// ' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by ' //&
  & 'child classes')
END PROCEDURE aField_GetTotalFaceDOF

!----------------------------------------------------------------------------
!                                                          GetTotalCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_GetTotalCellDOF
CHARACTER(*), PARAMETER :: myName = "aField_GetTotalCellDOF()"
CALL e%RaiseError(modName //'::'//myName// ' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by ' //&
  & 'child classes')
END PROCEDURE aField_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                                                 isConstant
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_isConstant
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE aField_isConstant

END SUBMODULE Methods
