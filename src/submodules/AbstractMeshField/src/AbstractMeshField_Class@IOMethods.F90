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

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: bool1

CALL Display(obj%isInitiated, 'Object INITIATED: ', unitNo=unitNo)

IF (.NOT. obj%isInitiated) RETURN

CALL Display('name: '//obj%name%chars(), unitNo=unitNo)
CALL Display('prefix: '//obj%GetPrefix(), unitNo=unitNo)

CALL Display('fieldType: '//FIELD_TYPE_NAME(obj%fieldType), unitNo=unitNo)

CALL Display('engine: '//obj%engine%chars(), unitNo=unitNo)

CALL Display(obj%tSize, 'tSize: ', unitNo=unitNo)

IF (obj%defineOn .EQ. Nodal) THEN
  CALL Display('defineOn: Nodal', unitNo=unitNo)
ELSE
  CALL Display('defineOn: Quadrature', unitNo=unitNo)
END IF

SELECT CASE (obj%rank)
CASE (Scalar)
  CALL Display('rank: Scalar', unitNo=unitNo)
CASE (Vector)
  CALL Display('rank: Vector', unitNo=unitNo)
CASE (Matrix)
  CALL Display('rank: Matrix', unitNo=unitNo)
END SELECT

SELECT CASE (obj%varType)
CASE (Constant)
  CALL Display('varType: Constant', unitNo=unitNo)
CASE (Space)
  CALL Display('varType: Space', unitNo=unitNo)
CASE (Time)
  CALL Display('varType: Time', unitNo=unitNo)
CASE (SpaceTime)
  CALL Display('varType: SpaceTime', unitNo=unitNo)
END SELECT

CALL Display(obj%SHAPE(), 'shape: ', unitNo=unitNo)

bool1 = ALLOCATED(obj%val)
CALL Display(bool1, 'val ALLOCATED: ', unitNo=unitNo)
! IF (bool1) THEN
!   CALL Display(obj%val, 'val: ', unitNo=unitNo)
! END IF

bool1 = ASSOCIATED(obj%mesh)
CALL Display(bool1, 'mesh ASSOCIATED: ', unitNo=unitNo)
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

IF (.NOT. obj%isInitiated) THEN
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

dsetname = TRIM(group)//"/shape"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%s)

IF (ALLOCATED(obj%val)) THEN
  dsetname = TRIM(group)//"/val"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%val)
END IF

CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        "[INTERNAL ERROR] :: Exporting AbstractMeshField_")

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportInVTK
CHARACTER(*), PARAMETER :: myName = "obj_ExportInVTK"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[INTERNAL ERROR] :: This routine is under development.')
END PROCEDURE obj_ExportInVTK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
