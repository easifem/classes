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
USE GlobalData, ONLY: Point1
USE HDF5File_Method, ONLY: HDF5ReadScalar, HDF5ReadVector, HDF5ReadMatrix
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
CHARACTER(:), ALLOCATABLE :: dsetname
INTEGER(I4B) :: ii, xidim, elemType, jj, tsize1, tsize2
LOGICAL(LGT) :: isok
CLASS(ElemData_), POINTER :: elemdata_ptr
CLASS(NodeData_), POINTER :: nodedata_ptr
INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:),  &
  & internalNptrs(:)

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

CALL HDF5ReadVector(hdf5=hdf5, VALUE=internalNptrs, group=dsetname,  &
  & fieldname="intNodeNumber", myname=myname, modname=modname, check=.TRUE.)

isok = (elemType .EQ. Point1) .OR. (elemType .EQ. 0)

ASSOCIATE (elementDataList => obj%elementDataList,  &
  & nodeDataBinaryTree => obj%nodeDataBinaryTree)

  CALL elementDataList%Initiate()
  CALL nodeDataBinaryTree%Initiate()

  DO ii = 1, obj%tElements
    ! elementData(ii)%globalElemNum = elemNumber(ii)
    ! elementData(ii)%localElemNum = ii
    ! elementData(ii)%globalNodes = connectivity(:, ii)

    elemdata_ptr => ElemData_Pointer()
    CALL ElemDataSet(obj=elemdata_ptr, globalElemNum=elemNumber(ii),  &
      & localElemNum=ii, globalNodes=connectivity(:, ii))
    CALL elementDataList%Add(elemdata_ptr)

    DO jj = 1, SIZE(connectivity, 1)

      nodedata_ptr => NodeData_Pointer()
      CALL NodeDataSet(obj=nodedata_ptr, globalNodeNum=connectivity(jj, ii))

      tsize1 = nodeDataBinaryTree%SIZE()
      CALL nodeDataBinaryTree%Insert(nodedata_ptr)
      tsize2 = nodeDataBinaryTree%SIZE()

      IF (tsize1 .EQ. tsize2) THEN
        CALL NodeData_Deallocate(nodedata_ptr)
        DEALLOCATE (nodedata_ptr)
      END IF

    END DO

  END DO

  CALL nodeDataBinaryTree%SetID()
  obj%tNodes = nodeDataBinaryTree%SIZE()

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
CALL obj%nodeDataBinaryTree%Display("nodeDataBinaryTree: ", unitno=unitno)
END PROCEDURE obj_Display

END SUBMODULE IOMethods
