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
USE ReallocateUtility
USE Display_Method
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
TYPE(ElemData_), POINTER :: elemdata_ptr
TYPE(NodeData_), POINTER :: nodedata_ptr
TYPE(NodeData_) :: nodedata
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

IF (isok) RETURN

CALL obj%elementDataList%Initiate()
CALL obj%nodeDataBinaryTree%Initiate()
CALL obj%nodeDataList%Initiate()
CALL obj%elementDataBinaryTree%Initiate()

DO ii = 1, obj%tElements

  elemdata_ptr => ElemData_Pointer()
  CALL ElemDataSet(obj=elemdata_ptr, globalElemNum=elemNumber(ii),  &
    & localElemNum=ii, globalNodes=connectivity(:, ii))
  CALL obj%elementDataList%Add(elemdata_ptr)
  CALL obj%elementDataBinaryTree%Insert(elemdata_ptr)

  DO jj = 1, SIZE(connectivity, 1)

    nodedata_ptr => NodeData_Pointer()
    CALL NodeDataSet(obj=nodedata_ptr, globalNodeNum=connectivity(jj, ii),  &
      & nodeType=TypeNode%boundary)
    ! TypeNode is defined in NodeData_Class
    ! The above step is unusual, but we know the position of
    ! internal nptrs, so later we will set the
    ! those nodes as INTERNAL_NODE, in this way we can
    ! identify the boundary nodes

    tsize1 = obj%nodeDataBinaryTree%SIZE()
    CALL obj%nodeDataBinaryTree%Insert(nodedata_ptr)
    tsize2 = obj%nodeDataBinaryTree%SIZE()

    IF (tsize1 .EQ. tsize2) THEN
      CALL NodeData_Deallocate(nodedata_ptr)
      ! NOTE:
        !! We have not added nodedata_ptr,
        !! We know this because tsize1 == tsize2so
        !! so we should remove this memory
        !! this step is necessary for avoiding the memory leak
      DEALLOCATE (nodedata_ptr)
    ELSE
      CALL obj%nodeDataList%Add(nodedata_ptr)
    END IF

  END DO

END DO

CALL obj%nodeDataBinaryTree%SetID()
  !! This method will set the local node number in the binarytree

CALL obj%elementDataBinaryTree%SetID()
  !! This method will set the local element number in the binarytree

obj%tNodes = obj%nodeDataBinaryTree%SIZE()
  !! This method returns the total number of nodes in mesh

nodedata_ptr => obj%nodeDataBinaryTree%GetMinPointer()
obj%minNptrs = nodedata_ptr%globalNodeNum

nodedata_ptr => obj%nodeDataBinaryTree%GetMaxPointer()
obj%maxNptrs = nodedata_ptr%globalNodeNum

nodedata_ptr => NULL()
CALL Reallocate(obj%local_Nptrs, obj%maxNptrs)

DO CONCURRENT(ii=1:obj%tElements)
  obj%local_Nptrs(connectivity(:, ii)) = connectivity(:, ii)
END DO

! TODO: Parallel
DO ii = 1, obj%maxNptrs
  IF (obj%local_Nptrs(ii) .NE. 0) THEN
    nodedata%globalNodeNum = ii
    nodedata_ptr => obj%nodeDataBinaryTree%GetValuePointer(VALUE=nodedata)
    obj%local_Nptrs(ii) = nodedata_ptr%localNodeNum
  END IF
END DO

! TODO: Parallel
DO ii = 1, SIZE(internalNptrs)
  ! jj = internalNptrs(ii)
  ! CALL NodeDataSet(obj=nodedata, globalNodeNum=jj)
  nodedata%globalNodeNum = internalNptrs(ii)
  nodedata_ptr => obj%nodeDataBinaryTree%GetValuePointer(VALUE=nodedata)
  nodedata_ptr%nodeType = TypeNode%internal
  ! CALL NodeDataSet(obj=nodedata_ptr, nodeType=TypeNode%internal)
END DO

nodedata_ptr => NULL()

IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(internalNptrs)) DEALLOCATE (internalNptrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL AbstractMeshDisplay(obj=obj, msg=msg, unitno=unitno)
CALL obj%elementDataList%Display("elementDataList: ", unitno=unitno)
CALL obj%elementDataBinaryTree%Display("elementDataBinaryTree: ",  &
  & unitno=unitno)

CALL obj%nodeDataBinaryTree%Display("nodeDataBinaryTree: ", unitno=unitno)
CALL obj%nodeDataList%Display("nodeDataList: ", unitno=unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                           DisplayNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayNodeData
CALL obj%nodeDataList%Display("nodeDataList: ", unitno=unitno)
END PROCEDURE obj_DisplayNodeData

!----------------------------------------------------------------------------
!                                                        DisplayElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayElementData
CALL obj%elementDataList%Display("elementDataList: ", unitno=unitno)
END PROCEDURE obj_DisplayElementData

END SUBMODULE IOMethods
