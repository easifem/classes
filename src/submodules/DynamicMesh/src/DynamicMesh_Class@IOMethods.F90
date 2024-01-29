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
USE HDF5File_Method, ONLY: HDF5ReadScalar, HDF5ReadVector
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:),  &
  & internalNptrs(:)
CHARACTER(:), ALLOCATABLE :: dsetname
INTEGER(I4B) :: ii, dummy, jj, xidim, elemType
LOGICAL(LGT) :: isok, abool
CLASS(ElemData_), POINTER :: value_ptr
TYPE(ElemDataListIterator_) :: iter

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

ASSOCIATE (elementDataList => obj%elementDataList)
  CALL elementDataList%Initiate()

  DO ii = 1, obj%tElements
    ! obj%elementData(ii)%globalElemNum = elemNumber(ii)
    ! obj%elementData(ii)%localElemNum = ii
    value_ptr => ElemData_Pointer()
    CALL ElemDataSet(obj=value_ptr, globalElemNum=elemNumber(ii),  &
      & localElemNum=ii)
    CALL elementDataList%Add(value_ptr)
  END DO

END ASSOCIATE

! isok = hdf5%pathExists(TRIM(dsetname)//"/connectivity")
! IF (.NOT. isok) THEN
!   CALL e%RaiseError(modName//'::'//myName//" - "// &
!     & '[INTERNAL ERROR]:: '//dsetname//'/connectivity path does not exists')
!   RETURN
! END IF

! CALL hdf5%READ(TRIM(dsetname)//"/connectivity", connectivity)
! isok = (obj%elemType .EQ. Point1) .OR. (obj%elemType .EQ. 0)
! IF (isok) THEN
!   obj%tNodes = 1
!   IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
!   ALLOCATE (obj%nodeData(obj%tNodes))
!   obj%nodeData(1)%globalNodeNum = 1
!   obj%nodeData(1)%localNodeNum = 1
!   obj%nodeData(1)%nodeType = BOUNDARY_NODE
! ELSE
!   obj%maxNptrs = MAXVAL(connectivity)
!   obj%minNptrs = MINVAL(connectivity)
!   CALL Reallocate(obj%local_Nptrs, obj%maxNptrs)
!
!   DO CONCURRENT(ii=1:obj%tElements)
!     obj%elementData(ii)%globalNodes = connectivity(:, ii)
!     obj%local_Nptrs(connectivity(:, ii)) = connectivity(:, ii)
!   END DO
!
!   obj%tNodes = COUNT(obj%local_Nptrs .NE. 0)
!   IF (ALLOCATED(obj%nodeData)) DEALLOCATE (obj%nodeData)
!   ALLOCATE (obj%nodeData(obj%tNodes))
!   dummy = 0
!
!   DO ii = 1, obj%maxNptrs
!     IF (obj%local_Nptrs(ii) .NE. 0) THEN
!       dummy = dummy + 1
!       obj%nodeData(dummy)%globalNodeNum = obj%local_Nptrs(ii)
!       obj%nodeData(dummy)%localNodeNum = dummy
!       obj%nodeData(dummy)%nodeType = BOUNDARY_NODE
!       ! The above step is unusual, but we know the position of
!       ! internal nptrs, so later we will set the
!       ! those nodes as INTERNAL_NODE, in this way we can
!       ! identify the boundary nodes
!       obj%local_Nptrs(ii) = dummy
!     END IF
!   END DO
!
! END IF
!
! !> reading internalNptrs, nodeData%globalNodeNumber,
! !> nodeData%localNodeNumber, nodeData%nodeType
! !> mark INTERNAL_NODE
!
! isok = hdf5%pathExists(TRIM(dsetname)//"/intNodeNumber")
! IF (.NOT. isok) THEN
!   CALL e%RaiseError(modName//'::'//myName//" - "// &
!     & '[INTERNAL ERROR]:: '//TRIM(dsetname)// &
!     & '/intNodeNumber path does not exists')
!   RETURN
! END IF
!
! CALL hdf5%READ(TRIM(dsetname)//"/intNodeNumber", internalNptrs)
! abool = obj%elemType .EQ. Point1 .OR. obj%elemType .EQ. 0
! IF (abool) THEN
!   obj%nodeData(1)%globalNodeNum = internalNptrs(1)
!   obj%nodeData(1)%nodeType = INTERNAL_NODE
! ELSE
!   DO ii = 1, SIZE(internalNptrs)
!     jj = obj%GetLocalNodeNumber(internalNptrs(ii))
!     obj%nodeData(jj)%globalNodeNum = internalNptrs(ii)
!     obj%nodeData(jj)%nodeType = INTERNAL_NODE
!   END DO
! END IF
!
! isok = obj%tElements .GT. 0
! abool = obj%xidim .GT. 0
! IF (isok) THEN
!   obj%refelem => ReferenceElement_Pointer(xidim=obj%xidim, &
!     & nsd=obj%nsd, elemType=obj%elemType, ipType=Equidistance)
!
!   IF (abool) obj%facetElements = FacetElements(obj%refelem)
!
! END IF
!
! IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
! IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
! IF (ALLOCATED(internalNptrs)) DEALLOCATE (internalNptrs)
!
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
