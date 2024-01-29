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

SUBMODULE(DynamicMesh_Class) IOMethods
USE HDF5File_Method, ONLY: HDF5ReadScalar, HDF5ReadVector, HDF5ReadMatrix
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:)
CHARACTER(:), ALLOCATABLE :: dsetname
INTEGER(I4B) :: ii, xidim, elemType
CLASS(ElemData_), POINTER :: value_ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL obj%DEALLOCATE()

dsetname = TRIM(group)
CALL AbstractMeshImport(obj=obj, hdf5=hdf5, group=group)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=xidim, group=dsetname,  &
  & fieldname="xidim", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=elemType, group=dsetname,  &
  & fieldname="elemType", myname=myname, modname=modname, check=.TRUE.)

! INFO:
! obj%tElements is read in AbstractMeshImport
IF (obj%tElements .EQ. 0) RETURN

CALL HDF5ReadVector(hdf5=hdf5, VALUE=elemNumber, group=dsetname,  &
  & fieldname="elemNumber", myname=myname, modname=modname, check=.TRUE.)

CALL HDF5ReadMatrix(hdf5=hdf5, VALUE=connectivity, group=dsetname,  &
  & fieldname="connectivity", myname=myname, modname=modname, check=.TRUE.)

ASSOCIATE (elementDataList => obj%elementDataList)
  CALL elementDataList%Initiate()

  DO ii = 1, obj%tElements
    ! elementData(ii)%globalElemNum = elemNumber(ii)
    ! elementData(ii)%localElemNum = ii
    ! elementData(ii)%globalNodes = connectivity(:, ii)

    value_ptr => ElemData_Pointer()
    CALL ElemDataSet(obj=value_ptr, globalElemNum=elemNumber(ii),  &
      & localElemNum=ii, globalNodes=connectivity(:, ii))
    CALL elementDataList%Add(value_ptr)
  END DO

END ASSOCIATE

IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL AbstractMeshDisplay(obj=obj, msg=msg, unitno=unitno)
CALL obj%elementDataList%Display("elementDataList: ", unitno=unitno)
END PROCEDURE obj_Display

END SUBMODULE IOMethods
