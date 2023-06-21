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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Display
IF (.NOT. obj%isInitiated) THEN
  CALL Display('Object is not initiated', unitNo=unitNo)
  RETURN
END IF
!
! name
!
CALL Display('# name: '//obj%name%chars(), unitNo=unitNo)
!
! fieldType
!
CALL Display('# fieldType: '//FIELD_TYPE_NAME(obj%fieldType), &
  & unitNo=unitNo)
!
! engine
!
CALL Display('# engine: '//obj%engine%chars(), &
  & unitNo=unitNo)
!
! tSize
!
CALL Display(obj%tSize, '# tSize: ', unitNo=unitNo)
!
! defineOn
!
IF (obj%defineOn .EQ. Nodal) THEN
  CALL Display('# defineOn: Nodal', unitNo=unitNo)
ELSE
  CALL Display('# defineOn: Quadrature', unitNo=unitNo)
END IF
!
! rank
!
SELECT CASE (obj%rank)
CASE (Scalar)
  CALL Display('# rank: Scalar', unitNo=unitNo)
CASE (Vector)
  CALL Display('# rank: Vector', unitNo=unitNo)
CASE (Matrix)
  CALL Display('# rank: Matrix', unitNo=unitNo)
END SELECT
!
! varType
!
SELECT CASE (obj%varType)
CASE (Constant)
  CALL Display('# varType: Constant', unitNo=unitNo)
CASE (Space)
  CALL Display('# varType: Space', unitNo=unitNo)
CASE (Time)
  CALL Display('# varType: Time', unitNo=unitNo)
CASE (SpaceTime)
  CALL Display('# varType: SpaceTime', unitNo=unitNo)
END SELECT
!
! shape
!
CALL Display(obj%s, '# shape: ', unitNo=unitNo)
!
! val
!
IF (ALLOCATED(obj%val)) THEN
  CALL Display('# val: ALLOCATED', unitNo=unitNo)
ELSE
  CALL Display('# val: NOT ALLOCATED', unitNo=unitNo)
END IF
!
! mesh
!
IF (ASSOCIATED(obj%mesh)) THEN
  CALL Display('# mesh: ASSOCIATED', unitNo=unitNo)
ELSE
  CALL Display('# mesh: NOT ASSOCIATED', unitNo=unitNo)
END IF
END PROCEDURE aField_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Import
CHARACTER(*), PARAMETER :: myName = "aField_Import"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine is under development!!')
END PROCEDURE aField_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Export
CHARACTER(*), PARAMETER :: myName = "aField_Export"
TYPE(String) :: strval, dsetname
!
! main program
!
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'MeshField object is not initiated initiated')
!
! info
!
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "Exporting AbstractMeshField_")
!
! check
!
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF
!
! check
!
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have write permission')
END IF
!
! fieldType
!
dsetname = TRIM(group)//"/fieldType"
strval = FIELD_TYPE_NAME(obj%fieldType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
!
! name
!
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)
!
! engine
!
dsetname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)
!
! tSize
!
dsetname = TRIM(group)//"/tSize"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tSize)
!
! defineOn
!
dsetname = TRIM(group)//"/defineOn"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%defineOn)
!
! rank
!
dsetname = TRIM(group)//"/rank"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%rank)
!
! varType
!
dsetname = TRIM(group)//"/varType"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%varType)
!
! shape
!
dsetname = TRIM(group)//"/shape"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%s)
!
! val
!
IF (ALLOCATED(obj%val)) THEN
  dsetname = TRIM(group)//"/val"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%val)
END IF
!
! info
!
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "Exporting AbstractMeshField_")
!
END PROCEDURE aField_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_ExportInVTK
CHARACTER(*), PARAMETER :: myName = "aField_ExportInVTK"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine is under development.')
END PROCEDURE aField_ExportInVTK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
