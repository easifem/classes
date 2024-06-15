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

SUBMODULE(AbstractMeshField_Class) IOMethods
USE GlobalData, ONLY: Constant, Space, Time, SpaceTime, &
                      Scalar, Vector, Matrix, Nodal, Quadrature

USE Display_Method, ONLY: Display

USE AbstractField_Class, ONLY: FIELD_TYPE_NAME

USE SafeSizeUtility, ONLY: SafeSize

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: bool1

CALL Display(obj%isInit, 'Object INITIATED: ', unitno=unitno)

IF (.NOT. obj%isInit) RETURN

CALL Display('name: '//obj%name%chars(), unitno=unitno)
CALL Display('prefix: '//obj%GetPrefix(), unitno=unitno)

CALL Display('fieldType: '//FIELD_TYPE_NAME(obj%fieldType), unitno=unitno)

CALL Display('engine: '//obj%engine%chars(), unitno=unitno)

CALL Display(obj%tSize, 'tSize: ', unitno=unitno)

IF (obj%defineOn .EQ. Nodal) THEN
  CALL Display('defineOn: Nodal', unitno=unitno)
ELSE
  CALL Display('defineOn: Quadrature', unitno=unitno)
END IF

SELECT CASE (obj%rank)
CASE (Scalar)
  CALL Display('rank: Scalar', unitno=unitno)
CASE (Vector)
  CALL Display('rank: Vector', unitno=unitno)
CASE (Matrix)
  CALL Display('rank: Matrix', unitno=unitno)
CASE DEFAULT
  CALL Display('rank: Unknown', unitno=unitno)
END SELECT

SELECT CASE (obj%varType)
CASE (Constant)
  CALL Display('varType: Constant', unitno=unitno)
CASE (Space)
  CALL Display('varType: Space', unitno=unitno)
CASE (Time)
  CALL Display('varType: Time', unitno=unitno)
CASE (SpaceTime)
  CALL Display('varType: SpaceTime', unitno=unitno)
CASE DEFAULT
  CALL Display('varType: Unknown', unitno=unitno)
END SELECT

bool1 = ALLOCATED(obj%val)
CALL Display(bool1, 'val ALLOCATED: ', unitno=unitno)
CALL Display(SafeSize(obj%val), "Size of val:", unitno=unitno)

bool1 = ALLOCATED(obj%indxVal)
CALL Display(bool1, 'indxVal ALLOCATED: ', unitno=unitno)
CALL Display(SafeSize(obj%indxVal), "Size of indxVal:", unitno=unitno)

bool1 = ASSOCIATED(obj%mesh)
CALL Display(bool1, 'mesh ASSOCIATED: ', unitno=unitno)

! CALL Display(obj%SHAPE(), 'shape: ', unitno=unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
CALL e%RaiseError(modName//'::'//myName//" - "// &
                  'This routine is under development!!')
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
TYPE(String) :: strval, dsetname

IF (.NOT. obj%isInit) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'MeshField object is not initiated initiated')
END IF

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'HDF5 file does not have write permission')
END IF

dsetname = TRIM(group)//"/fieldType"
strval = FIELD_TYPE_NAME(obj%fieldType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

dsetname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)

dsetname = TRIM(group)//"/tSize"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tSize)

dsetname = TRIM(group)//"/defineOn"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%defineOn)

dsetname = TRIM(group)//"/rank"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%rank)

dsetname = TRIM(group)//"/varType"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%varType)

IF (ALLOCATED(obj%val)) THEN
  dsetname = TRIM(group)//"/val"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%val)
END IF

IF (ALLOCATED(obj%indxVal)) THEN
  dsetname = TRIM(group)//"/indxVal"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%indxVal)
END IF

IF (ALLOCATED(obj%ss)) THEN
  dsetname = TRIM(group)//"/shape"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%ss)
END IF

IF (ALLOCATED(obj%indxShape)) THEN
  dsetname = TRIM(group)//"/indxShape"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%indxShape)
END IF

CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        "[INTERNAL ERROR] :: Exporting AbstractMeshField_")

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportInVTK
CHARACTER(*), PARAMETER :: myName = "obj_ExportInVTK()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[INTERNAL ERROR] :: This routine is under development.')
END PROCEDURE obj_ExportInVTK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
