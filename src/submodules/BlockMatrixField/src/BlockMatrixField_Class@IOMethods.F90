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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(BlockMatrixField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Import
CHARACTER(*), PARAMETER :: myName = "mField_Import"
TYPE(String) :: strval, dsetname, name, matrixProp, engine
INTEGER(I4B), ALLOCATABLE :: timeCompo(:), spaceCompo(:)
INTEGER(I4B) :: fieldType, ii, tvar
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
TYPE(ParameterList_) :: param

! main program

! print info
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "[START] Import()")

IF (obj%IsInitiated) &
  & CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The instance of BloclMatrixField_ is already initiated')

! check
IF (.NOT. hdf5%IsOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

! check
IF (.NOT. hdf5%IsRead()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
END IF

! fieldType
dsetname = TRIM(group)//"/fieldType"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
  SELECT CASE (TRIM(strval%Chars()))
  CASE ("NORMAL")
    fieldType = FIELD_TYPE_NORMAL
  CASE ("CONSTANT")
    fieldType = FIELD_TYPE_CONSTANT
  CASE ("CONSTANT_SPACE")
    fieldType = FIELD_TYPE_CONSTANT_SPACE
  CASE ("CONSTANT_TIME")
    fieldType = FIELD_TYPE_CONSTANT_TIME
  END SELECT
ELSE
  fieldType = FIELD_TYPE_NORMAL
END IF

! name
dsetname = TRIM(group)//"/name"
IF (.NOT. hdf5%PathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset name should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=name)
END IF

! engine
dsetname = TRIM(group)//"/engine"
IF (.NOT. hdf5%PathExists(dsetname%chars())) THEN
  engine = "NATIVE_SERIAL"
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=engine)
END IF

! matrixProp
dsetname = TRIM(group)//"/matrixProp"
IF (.NOT. hdf5%PathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset matrixProp should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=matrixProp)
END IF

! tPhysicalVarNames
dsetname = TRIM(group)//"/tPhysicalVarNames"
IF (.NOT. hdf5%PathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset '//dsetname%chars()//' should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar)
END IF

! spaceCompo
dsetname = TRIM(group)//"/spaceCompo"
IF (.NOT. hdf5%PathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset spaceCompo should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=spaceCompo)
END IF

! timeCompo
dsetname = TRIM(group)//"/timeCompo"
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=timeCompo)
ELSE
  timeCompo = 1
END IF

! mat
dsetname = TRIM(group)//"/mat"
IF (hdf5%PathExists(dsetname%chars())) THEN
  obj%engine = engine
  obj%name = name
  obj%fieldType = fieldType
  ALLOCATE (obj%domains(tvar))
  IF (PRESENT(dom)) THEN
    obj%domain => dom
    DO ii = 1, tVar
      obj%domains(ii)%ptr => dom
    END DO
  ELSE IF (PRESENT(domains)) THEN
    obj%domain => NULL()
    DO ii = 1, tVar
      obj%domains(ii)%ptr => domains(ii)%ptr
    END DO
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Either dom or domains should be present')
  END IF
  CALL ImportCSRMatrix(obj=obj%mat, hdf5=hdf5, group=dsetname%chars())
  obj%isInitiated = .TRUE.
  obj%isPmatInitiated = .FALSE.

ELSE
  ! physicalVarNames
  ALLOCATE (physicalVarNames(tvar))
  DO ii = 1, tvar
    dsetname = TRIM(group)//"/physicalVarName"//TOSTRING(ii)
    IF (.NOT. hdf5%PathExists(dsetname%chars())) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'The dataset '//dsetname%Chars()//' should be present')
    ELSE
      CALL e%RaiseInformation(modName//"::"//myName//" - "// &
        & "Importing "//dsetname%chars())
      CALL hdf5%READ(dsetname=dsetname%chars(), &
        & vals=strval)
      physicalVarNames(ii) = strval%chars()
    END IF
  END DO

  !> initiate
  CALL param%Initiate()
  CALL SetBlockMatrixFieldParam(param=param, &
    & name=name%chars(), &
    & engine=engine%chars(), &
    & physicalVarNames=physicalVarNames, &
    & matrixProp=TRIM(matrixProp%Chars()), &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo, &
    & fieldType=fieldType)
  IF (PRESENT(dom)) THEN
    CALL obj%Initiate(param=param, dom=dom)
  ELSE IF (PRESENT(domains)) THEN
    CALL obj%Initiate(param=param, dom=domains)
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Either dom or domains should be present')
  END IF
  CALL param%DEALLOCATE()
END IF

! pmat
dsetname = TRIM(group)//"/pmat"
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "Importing "//dsetname%chars())
IF (hdf5%PathExists(dsetname%chars())) THEN
  CALL obj%ImportPmat(hdf5=hdf5, group=dsetname%chars(), &
    & dom=dom, domains=domains)
ELSE
  CALL e%RaiseDebug(modName//"::"//myName//" - "// &
    & "This routine needs further attention")
END IF

! cleanup
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(physicalVarNames)) DEALLOCATE (physicalVarNames)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END] Import()')
END PROCEDURE mField_Import

END SUBMODULE IOMethods
