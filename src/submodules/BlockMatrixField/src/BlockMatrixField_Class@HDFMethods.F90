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

SUBMODULE(BlockMatrixField_Class) HDFMethods
USE Display_Method, ONLY: ToString
USE String_Class, ONLY: String
USE HDF5File_Method, ONLY: ImportCSRMatrix
USE MatrixFieldUtility, ONLY: Export_Header, &
                              Import_Header, &
                              Import_CheckError, &
                              Import_PhysicalVar
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

TYPE(String) :: strval, dsetname, name, matrixProp, engine
INTEGER(I4B), ALLOCATABLE :: timeCompo(:), spaceCompo(:)
INTEGER(I4B) :: fieldType, ii, tvar
CHARACTER(1), ALLOCATABLE :: physicalVarNames(:)
TYPE(ParameterList_) :: param
LOGICAL(LGT) :: isok, ismat, isRectangle

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! From MatrixFieldUtility
CALL Import_CheckError(obj=obj, hdf5=hdf5, group=group, &
                       myName=myName, modName=modName)

#ifdef DEBUG_VER
isok = PRESENT(fedof) .OR. PRESENT(fedofs)
CALL AssertError1(isok, myName, "Either fedof or fedofs should be present")
#endif

! From MatrixFieldUtility
CALL Import_Header( &
  obj=obj, hdf5=hdf5, group=group, modName=modName, myName=myName, &
  fieldType=fieldType, name=name, engine=engine, matrixProp=matrixProp, &
  isRectangle=isRectangle)

! tPhysicalVarNames
dsetname = TRIM(group)//"/tPhysicalVarNames"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, 'dataset '//dsetname//' should be present')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=tvar)

! spaceCompo
dsetname = TRIM(group)//"/spaceCompo"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, 'dataset '//dsetname//' should be present')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=spaceCompo)

! timeCompo
dsetname = TRIM(group)//"/timeCompo"
#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
timeCompo = 1
IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), vals=timeCompo)
#endif

dsetname = TRIM(group)//"/mat"
ismat = hdf5%PathExists(dsetname%chars())

IF (ismat) THEN
  obj%engine = engine
  obj%name = name
  obj%fieldType = fieldType

  ALLOCATE (obj%fedofs(tvar))
  IF (PRESENT(fedof)) THEN
    obj%fedof => fedof
    DO ii = 1, tVar
      obj%fedofs(ii)%ptr => fedof
    END DO
  END IF

  IF (PRESENT(fedofs)) THEN
    DO ii = 1, tVar
      obj%fedofs(ii)%ptr => fedofs(ii)%ptr
    END DO
  END IF

  CALL ImportCSRMatrix(obj=obj%mat, hdf5=hdf5, group=dsetname%chars())
  obj%isInit = .TRUE.
  obj%isPmatInitiated = .FALSE.

END IF

IF (.NOT. ismat) THEN

  ! physicalVarNames
  ALLOCATE (physicalVarNames(tvar))

  DO ii = 1, tvar
    dsetname = TRIM(group)//"/physicalVarName"//TOSTRING(ii)

#ifdef DEBUG_VER
    isok = hdf5%PathExists(dsetname%chars())
    CALL AssertError1(isok, myName, 'dataset '//dsetname// &
                      ' should be present')
#endif

    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    physicalVarNames(ii) = strval%chars()

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
#endif

  ! CALL param%Initiate()
  !
  ! CALL SetBlockMatrixFieldParam( &
  !   param=param, name=name%chars(), engine=engine%chars(), &
  !   physicalVarNames=physicalVarNames, matrixProp=matrixProp%Chars(), &
  !   spaceCompo=spaceCompo, timeCompo=timeCompo, fieldType=fieldType)
  !
  ! IF (PRESENT(fedof)) &
  !   CALL obj%Initiate(param=param, fedof=fedof, geofedof=geofedof)
  !
  ! IF (PRESENT(fedofs)) &
  !   CALL obj%Initiate(param=param, fedof=fedofs, geofedof=geofedofs)
  !
  ! CALL param%DEALLOCATE()

END IF

! pmat
dsetname = TRIM(group)//"/pmat"
isok = hdf5%PathExists(dsetname%chars())
IF (isok) &
  CALL obj%ImportPmat(hdf5=hdf5, group=dsetname%chars(), fedof=fedof, &
                      fedofs=fedofs)

! cleanup
IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
IF (ALLOCATED(physicalVarNames)) DEALLOCATE (physicalVarNames)

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] Import()')
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
