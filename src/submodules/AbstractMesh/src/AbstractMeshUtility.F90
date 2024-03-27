! This program is a part of EASIFEM library
! Copyright (C) (Since 2000)  Vikas Sharma, Ph.D
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

MODULE AbstractMeshUtility
USE GlobalData
USE Display_Method
USE ReallocateUtility
USE HDF5File_Class
USE HDF5File_Method
USE AbstractMesh_Class
USE CPUTime_Class
USE ExceptionHandler_Class, ONLY: e
USE NodeData_Class
USE ElemData_Class
USE ReferenceElement_Method
USE AssertUtility
! USE FacetData_Class, ONLY: InternalFacetData_, BoundaryFacetData_
! USE AbstractMesh_Class, ONLY: AbstractMesh_, AbstractMeshDeallocate, &
!   & AbstractMeshDisplay, AbstractMeshGetQuery, AbstractMeshImport

IMPLICIT NONE
PRIVATE

PUBLIC :: MeshImportFromGroup
PUBLIC :: MeshImportFromDim
PUBLIC :: InitiateElementToElements3D
PUBLIC :: InitiateElementToElements2D
PUBLIC :: InitiateElementToElements1D

CHARACTER(*), PARAMETER :: modName = "AbstractMeshUtility"

CONTAINS

!----------------------------------------------------------------------------
!                                               InitiateElementToElements3D
!----------------------------------------------------------------------------

SUBROUTINE InitiateElementToElements3D(elementData, tFaceInMesh, showTime)
  TYPE(ElemData_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tFaceInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements3D()"
  LOGICAL(LGT) :: problem, isok1, isok2
  INTEGER(I4B) :: telems, iel, aint, bint, tfaces, ii, jj, &
    & temp1(3 * REFELEM_MAX_FACES), cint
  INTEGER(I4B), ALLOCATABLE :: face2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)
  TYPE(CPUTime_) :: TypeCPUTime

  IF (showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

#ifdef DEBUG_VER
  problem = tFaceInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Total number of faces are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(face2elem, 4, tFaceInMesh)
  CALL Reallocate(amask, tFaceInMesh)
  amask = .FALSE.

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

#ifdef DEBUG_VER
    problem = .NOT. ALLOCATED(elementData(iel)%globalFaces)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: local element number = '//tostring(iel)//  &
        & " does not have globalFaces data allocated.")
      RETURN
    END IF
#endif

    tfaces = SIZE(elementData(iel)%globalFaces)
    DO ii = 1, tfaces
      aint = ABS(elementData(iel)%globalFaces(ii))
      IF (amask(aint)) THEN
        face2elem(2, aint) = iel
        face2elem(4, aint) = ii
        amask(aint) = .FALSE.
      ELSE
        face2elem(1, aint) = iel
        face2elem(3, aint) = ii
        amask(aint) = .TRUE.
      END IF
    END DO

  END DO

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

    tfaces = SIZE(elementData(iel)%globalFaces)
    jj = 0; temp1 = 0
    DO ii = 1, tfaces
      aint = ABS(elementData(iel)%globalFaces(ii))
      bint = face2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%globalElemNum
        temp1(2 + (jj - 1) * 3) = face2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = face2elem(3, aint)

      ELSE
        cint = face2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) =  &
            & elementData(cint)%globalElemNum
          temp1(2 + (jj - 1) * 3) = face2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = face2elem(4, aint)
        END IF
      END IF
    END DO

    aint = jj * 3
    CALL Reallocate(elementData(iel)%globalElements, aint)
    elementData(iel)%globalElements = temp1(1:aint)

  END DO

  IF (ALLOCATED(face2elem)) DEALLOCATE (face2elem)
  IF (ALLOCATED(amask)) DEALLOCATE (amask)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

END SUBROUTINE InitiateElementToElements3D

!----------------------------------------------------------------------------
!                                                InitiateElementToElements2D
!----------------------------------------------------------------------------

SUBROUTINE InitiateElementToElements2D(elementData, tEdgeInMesh, showTime)
  TYPE(ElemData_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tEdgeInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "InitiateElementToElements2D()"
  LOGICAL(LGT) :: problem, isok1, isok2
  INTEGER(I4B) :: telems, iel, aint, bint, tedges, ii, jj, temp1(3 * 4), &
    & cint
  INTEGER(I4B), ALLOCATABLE :: edge2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)
  TYPE(CPUTime_) :: TypeCPUTime

  IF (showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

#ifdef DEBUG_VER
  problem = tEdgeInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Total number of edges are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(edge2elem, 4, tEdgeInMesh)
  CALL Reallocate(amask, tEdgeInMesh)
  amask = .FALSE.

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

#ifdef DEBUG_VER
    problem = .NOT. ALLOCATED(elementData(iel)%globalEdges)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: local element number = '//tostring(iel)//  &
        & " does not have globalEdges data allocated.")
      RETURN
    END IF
#endif

    tedges = SIZE(elementData(iel)%globalEdges)
    DO ii = 1, tedges
      aint = ABS(elementData(iel)%globalEdges(ii))
      IF (amask(aint)) THEN
        edge2elem(2, aint) = iel
        edge2elem(4, aint) = ii
        amask(aint) = .FALSE.
      ELSE
        edge2elem(1, aint) = iel
        edge2elem(3, aint) = ii
        amask(aint) = .TRUE.
      END IF
    END DO

  END DO

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

    tedges = SIZE(elementData(iel)%globalEdges)
    jj = 0
    temp1 = 0
    DO ii = 1, tedges
      aint = ABS(elementData(iel)%globalEdges(ii))
      bint = edge2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%globalElemNum
        temp1(2 + (jj - 1) * 3) = edge2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = edge2elem(3, aint)

      ELSE
        cint = edge2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) = elementData(cint)%globalElemNum
          temp1(2 + (jj - 1) * 3) = edge2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = edge2elem(4, aint)
        END IF
      END IF
    END DO

    aint = jj * 3
    CALL Reallocate(elementData(iel)%globalElements, aint)
    elementData(iel)%globalElements = temp1(1:aint)

  END DO

  IF (ALLOCATED(amask)) DEALLOCATE (amask)
  IF (ALLOCATED(edge2elem)) DEALLOCATE (edge2elem)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

END SUBROUTINE InitiateElementToElements2D

!----------------------------------------------------------------------------
!                                               InitiateElementToElements1D
!----------------------------------------------------------------------------

SUBROUTINE InitiateElementToElements1D(elementData, tNodesInMesh,  &
  & showTime, local_nptrs)
  TYPE(ElemData_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tNodesInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime
  INTEGER(I4B), INTENT(IN) :: local_nptrs(:)

!   ! internal variables
  CHARACTER(*), PARAMETER :: myName = "InitiateElementToElements1D()"
  LOGICAL(LGT) :: problem, isok1, isok2
  INTEGER(I4B) :: telems, iel, aint, bint, tNodes, ii, jj, temp1(3 * 2), &
    & cint
  INTEGER(I4B), ALLOCATABLE :: node2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)
  TYPE(CPUTime_) :: TypeCPUTime

  IF (showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

#ifdef DEBUG_VER
  problem = tNodesInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Total number of nodes are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(node2elem, 4, tNodesInMesh)
  CALL Reallocate(amask, tNodesInMesh)
  amask = .FALSE.

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

#ifdef DEBUG_VER
    problem = .NOT. ALLOCATED(elementData(iel)%globalNodes)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: local element number = '//tostring(iel)//  &
        & " does not have globalNodes data allocated.")
      RETURN
    END IF
#endif

    DO ii = 1, 2
      aint = elementData(iel)%globalNodes(ii)
      aint = local_nptrs(aint)
      IF (amask(aint)) THEN
        node2elem(2, aint) = iel
        node2elem(4, aint) = ii
        amask(aint) = .FALSE.
      ELSE
        node2elem(1, aint) = iel
        node2elem(3, aint) = ii
        amask(aint) = .TRUE.
      END IF
    END DO

  END DO

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

    tNodes = SIZE(elementData(iel)%globalNodes)
    jj = 0
    temp1 = 0
    DO ii = 1, 2
      aint = elementData(iel)%globalNodes(ii)
      aint = local_nptrs(aint)
      bint = node2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%globalElemNum
        temp1(2 + (jj - 1) * 3) = node2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = node2elem(3, aint)

      ELSE
        cint = node2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) = elementData(cint)%globalElemNum
          temp1(2 + (jj - 1) * 3) = node2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = node2elem(4, aint)
        END IF
      END IF
    END DO

    aint = jj * 3
    CALL Reallocate(elementData(iel)%globalElements, aint)
    elementData(iel)%globalElements = temp1(1:aint)

  END DO

  IF (ALLOCATED(amask)) DEALLOCATE (amask)
  IF (ALLOCATED(node2elem)) DEALLOCATE (node2elem)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

END SUBROUTINE InitiateElementToElements1D

!----------------------------------------------------------------------------
!                                                     MeshImportCheckError
!----------------------------------------------------------------------------

SUBROUTINE MeshImportCheckError(hdf5, group)
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

  CHARACTER(*), PARAMETER :: myName = "MeshImportCheckError()"
  CHARACTER(:), ALLOCATABLE :: dsetname
  LOGICAL(LGT) :: isok

  dsetname = TRIM(group)

  isok = hdf5%isOpen()
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: HDF5 file is not opened')
    RETURN
  END IF

  isok = hdf5%isRead()
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: HDF5 file does not have read permission')
    RETURN
  END IF

  isok = hdf5%isGroup(dsetname)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//dsetname//  &
  & ' is not a group; it should be a group which contains the meshEntity')
    RETURN
  END IF

  isok = hdf5%pathExists(dsetname)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR]:: '//dsetname//' path does not exists')
    RETURN
  END IF

  dsetname = ""

END SUBROUTINE MeshImportCheckError

!----------------------------------------------------------------------------
!                                                         MeshImportScalar
!----------------------------------------------------------------------------

SUBROUTINE MeshImportScalar(obj, hdf5, group)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

  CHARACTER(*), PARAMETER :: myName = "MeshImportScalar()"
  CHARACTER(:), ALLOCATABLE :: dsetname

  dsetname = TRIM(group)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%uid, group=dsetname,  &
    & fieldname="uid", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%nsd, group=dsetname,  &
    & fieldname="nsd", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%xidim, group=dsetname,  &
    & fieldname="xidim", myname=myname, modname=modname, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%tElements, group=dsetname,  &
    & fieldname="tElements", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minX, group=dsetname,  &
    & fieldname="minX", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minY, group=dsetname,  &
    & fieldname="minY", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%minZ, group=dsetname,  &
    & fieldname="minZ", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxX, group=dsetname,  &
    & fieldname="maxX", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxY, group=dsetname,  &
    & fieldname="maxY", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxZ, group=dsetname,  &
    & fieldname="maxZ", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%x, group=dsetname,  &
    & fieldname="x", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%y, group=dsetname,  &
    & fieldname="y", myname=myname, modName=modName, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%z, group=dsetname,  &
    & fieldname="z", myname=myname, modName=modName, check=.TRUE.)

END SUBROUTINE MeshImportScalar

!----------------------------------------------------------------------------
!                                                          MeshImportVector
!----------------------------------------------------------------------------

SUBROUTINE MeshImportVector(obj, hdf5, group, connectivity, elemNumber,  &
  & internalNptrs)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: connectivity(:, :)
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: elemNumber(:)
  INTEGER(I4B), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: internalNptrs(:)

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "MeshImportVector()"
  CHARACTER(:), ALLOCATABLE :: dsetname
  LOGICAL(LGT) :: isok

  dsetname = TRIM(group)

  CALL HDF5ReadVector(hdf5=hdf5, VALUE=obj%boundingEntity, group=dsetname,  &
             & fieldname="boundingEntity", myname=myname, modName=modName,  &
                                                             & check=.FALSE.)

  ! If boundingEntity is not initiated then we initiate it with size=0
  ! Bounding entity will not be initiated for point type
  IF (.NOT. ALLOCATED(obj%boundingEntity)) THEN
    CALL Reallocate(obj%boundingEntity, 0)
  END IF

  CALL HDF5ReadVector(hdf5=hdf5, VALUE=elemNumber, group=dsetname,  &
   & fieldname="elemNumber", myname=myname, modName=modName, check=.TRUE.)

  isok = .FALSE.
  IF (ALLOCATED(elemNumber)) THEN
    isok = SIZE(elemNumber) .NE. 0
  END IF

  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: either elemNumber not ALLOCATED '//  &
      & 'or size of elemNumber is zero')
    RETURN
  END IF

  CALL HDF5ReadMatrix(hdf5=hdf5, VALUE=connectivity, group=dsetname,  &
 & fieldname="connectivity", myname=myname, modName=modName, check=.TRUE.)

  obj%maxNNE = SIZE(connectivity, 1)

  IF (PRESENT(internalNptrs)) THEN
    CALL HDF5ReadVector(hdf5=hdf5, VALUE=internalNptrs, group=dsetname,  &
& fieldname="intNodeNumber", myname=myname, modName=modName, check=.TRUE.)
  END IF

  obj%maxElemNum = MAXVAL(elemNumber)
  obj%minElemNum = MINVAL(elemNumber)
  obj%maxNptrs = MAXVAL(connectivity)
  obj%minNptrs = MINVAL(connectivity)

END SUBROUTINE MeshImportVector

!----------------------------------------------------------------------------
!                                                   MeshImportElementData
!----------------------------------------------------------------------------

SUBROUTINE MeshImportElementData(obj, hdf5, group, connectivity, elemNumber)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  INTEGER(I4B), INTENT(IN) :: connectivity(:, :)
  INTEGER(I4B), INTENT(IN) :: elemNumber(:)

  ! Internal variables
  INTEGER(I4B) :: ii, elemType, meshID
  CHARACTER(:), ALLOCATABLE :: dsetname
  CHARACTER(*), PARAMETER :: myName = "MeshImportElementData()"

  dsetname = TRIM(group)

  ALLOCATE (obj%elementData(obj%tElements))

  CALL Reallocate(obj%local_elemNumber, obj%maxElemNum)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=elemType, group=dsetname,  &
    & fieldname="elemType", myname=myname, modname=modname, check=.TRUE.)

  CALL HDF5ReadScalar(hdf5=hdf5, VALUE=meshID, group=dsetname,  &
    & fieldname="uid", myname=myname, modname=modname, check=.TRUE.)

  DO CONCURRENT(ii=1:obj%tElements)
    obj%local_elemNumber(elemNumber(ii)) = ii
    CALL ElemDataSet(obj=obj%elementData(ii), globalElemNum=elemNumber(ii),  &
         & localElemNum=ii, globalNodes=connectivity(:, ii), name=elemType,  &
                                             & isActive=.TRUE., meshID=meshID)
  END DO

END SUBROUTINE MeshImportElementData

!----------------------------------------------------------------------------
!                                                   MeshImportElementData
!----------------------------------------------------------------------------

SUBROUTINE MeshImportElementDataFromDim(obj, elemType, meshID,  &
  & connectivity, elemNumber, istart, iend, nne)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: meshID
  INTEGER(I4B), INTENT(IN) :: connectivity(:, :)
  INTEGER(I4B), INTENT(IN) :: elemNumber(:)
  INTEGER(I4B), INTENT(IN) :: istart
  INTEGER(I4B), INTENT(IN) :: iend
  INTEGER(I4B), INTENT(IN) :: nne

  ! Internal variables
  INTEGER(I4B) :: ii
  ! CHARACTER(*), PARAMETER :: myName = "MeshImportElementDataFromDim()"

  DO ii = istart, iend
    obj%local_elemNumber(elemNumber(ii)) = ii
    CALL ElemDataSet( &
      & obj=obj%elementData(ii),  &
      & globalElemNum=elemNumber(ii),  &
      & localElemNum=ii,  &
      & globalNodes=connectivity(1:nne, ii),  &
      & name=elemType,  &
      & isActive=.TRUE.,  &
      & meshID=meshID)
  END DO

END SUBROUTINE MeshImportElementDataFromDim

!----------------------------------------------------------------------------
!                                                   MeshImportNodeData
!----------------------------------------------------------------------------

SUBROUTINE MeshImportNodeData(obj, connectivity, internalNptrs)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: connectivity(:, :)
  INTEGER(I4B), INTENT(IN) :: internalNptrs(:)

  ! Internal veriables
  INTEGER(I4B) :: ii, jj, aint, dummy
  LOGICAL(LGT), ALLOCATABLE :: mask(:)

  aint = SIZE(connectivity, 1)
  CALL Reallocate(obj%local_nptrs, obj%maxNptrs)

  DO CONCURRENT(ii=1:obj%tElements)
    DO jj = 1, aint
      obj%local_nptrs(connectivity(jj, ii)) = connectivity(jj, ii)
    END DO
  END DO

  obj%tNodes = COUNT(obj%local_nptrs .NE. 0)
  ALLOCATE (obj%nodeData(obj%tNodes))
  CALL Reallocate(mask, obj%maxNptrs)
  mask = .FALSE.
  mask(internalNptrs) = .TRUE.

  dummy = 0
  DO ii = 1, obj%maxNptrs
    IF (obj%local_nptrs(ii) .NE. 0) THEN
      dummy = dummy + 1
      obj%nodeData(dummy)%globalNodeNum = obj%local_Nptrs(ii)
      obj%nodeData(dummy)%localNodeNum = dummy

      IF (mask(ii)) THEN
        obj%nodeData(dummy)%nodeType = INTERNAL_NODE
      ELSE
        obj%nodeData(dummy)%nodeType = BOUNDARY_NODE
      END IF

      obj%local_nptrs(ii) = dummy
    END IF
  END DO

  IF (ALLOCATED(mask)) DEALLOCATE (mask)

END SUBROUTINE MeshImportNodeData

!----------------------------------------------------------------------------
!                                                        MeshImportNodeData
!----------------------------------------------------------------------------

SUBROUTINE MeshImportNodeDataFromDim(obj, connectivity)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: connectivity(:, :)

  ! Internal veriables
  INTEGER(I4B) :: ii, jj, aint, dummy

  aint = SIZE(connectivity, 1)
  CALL Reallocate(obj%local_nptrs, obj%maxNptrs)

  DO ii = 1, obj%tElements
    DO jj = 1, aint
      dummy = connectivity(jj, ii)
      IF (dummy .NE. 0) THEN
        obj%local_nptrs(dummy) = dummy
      END IF
    END DO
  END DO

  obj%tNodes = COUNT(obj%local_nptrs .NE. 0)
  ALLOCATE (obj%nodeData(obj%tNodes))

  dummy = 0
  DO ii = 1, obj%maxNptrs
    IF (obj%local_nptrs(ii) .EQ. 0) CYCLE

    dummy = dummy + 1
    obj%nodeData(dummy)%globalNodeNum = obj%local_nptrs(ii)
    obj%nodeData(dummy)%localNodeNum = dummy
    obj%nodeData(dummy)%nodeType = INTERNAL_NODE
    obj%local_nptrs(ii) = dummy
  END DO

END SUBROUTINE MeshImportNodeDataFromDim

!----------------------------------------------------------------------------
!                                                   MeshImportFromGroup
!----------------------------------------------------------------------------

SUBROUTINE MeshImportFromGroup(obj, hdf5, group)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "MeshImportFromGroup()"
  INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:),  &
    & internalNptrs(:)
  TYPE(CPUTime_) :: TypeCPUTime

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  CALL obj%DEALLOCATE()

  CALL MeshImportCheckError(hdf5, group)

  IF (obj%showTime) THEN
    CALL Display("Showing Time States of Importing Mesh", unitno=stdout)
    CALL EqualLine(unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportScalar(obj, hdf5, group)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time in importing scalar data [MeshImportScalar] : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportVector(obj, hdf5, group, connectivity, elemNumber,  &
    & internalNptrs)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time in importing vector data [MeshImportVector] : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportElementData(obj, hdf5, group, connectivity, elemNumber)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
     & " : time in importing element data [MeshImportElementData] : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportNodeData(obj, connectivity, internalNptrs)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time in importing node data [MeshImportNodeData] : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
  IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
  IF (ALLOCATED(internalNptrs)) DEALLOCATE (internalNptrs)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE MeshImportFromGroup

!----------------------------------------------------------------------------
!                                                       MeshImportFromDim
!----------------------------------------------------------------------------

SUBROUTINE MeshImportFromDim(obj, hdf5, group, dim, entities, tEntities)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  CLASS(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B), INTENT(IN) :: entities(:)
  INTEGER(I4B), INTENT(IN) :: tEntities

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "MeshImportFromDim()"
  INTEGER(I4B), ALLOCATABLE :: connectivity(:, :), elemNumber(:),  &
    & temp_int_2d(:, :), temp_int_1d(:), boundingEntity(:)
  INTEGER(I4B) :: ii, jj, kk, tElements(tEntities), &
 & nsd(tEntities), uid(tEntities), elemType(tEntities), nne(tEntities),  &
    & aint, bint, maxBoundingEntities, xidim(tEntities)
  ! TYPE(CPUTime_) :: TypeCPUTime
  CHARACTER(:), ALLOCATABLE :: dsetname, prefix
  REAL(DFP), DIMENSION(3, tEntities) :: xyz, min_xyz, max_xyz
  TYPE(CPUTime_) :: TypeCPUTime

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  CALL obj%DEALLOCATE()

  IF (obj%showTime) THEN
    CALL Display("Showing Time States of Importing Mesh", unitno=stdout)
    CALL EqualLine(unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  SELECT CASE (dim)
  CASE (0_I4B)
    prefix = group//"/pointEntities_"
    maxBoundingEntities = 0_I4B
  CASE (1_I4B)
    prefix = group//"/curveEntities_"
  CASE (2_I4B)
    prefix = group//"/surfaceEntities_"
  CASE (3_I4B)
    prefix = group//"/volumeEntities_"
  END SELECT

  DO ii = 1, tEntities
    dsetname = prefix//tostring(entities(ii))
    CALL MeshImportCheckError(hdf5, dsetname)
    CALL MeshImportScalar(obj, hdf5, dsetname)
    CALL HDF5ReadScalar(hdf5=hdf5, VALUE=elemType(ii), group=dsetname,  &
     & fieldname="elemType", myname=myname, modname=modname, check=.TRUE.)

    nne(ii) = GetTotalNodes(elemType(ii))

    uid(ii) = obj%uid
    nsd(ii) = obj%nsd
    xidim(ii) = obj%xidim
    tElements(ii) = obj%tElements
    min_xyz(1, ii) = obj%minX
    min_xyz(2, ii) = obj%minY
    min_xyz(3, ii) = obj%minZ
    max_xyz(1, ii) = obj%maxX
    max_xyz(2, ii) = obj%maxY
    max_xyz(3, ii) = obj%maxZ
    xyz(1, ii) = obj%x
    xyz(2, ii) = obj%y
    xyz(3, ii) = obj%z

    aint = GetElementIndex(elemType(ii))
    obj%tElements_topology_wise(aint) = obj%tElements_topology_wise(aint) +  &
                                                               & tElements(ii)

  END DO

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time in importing scalar data [MeshImportScalar] : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

#ifdef DEBUG_VER
  CALL Assert(nn=nsd,  &
    & msg="[INTERNAL ERROR] :: nsd of all entities is not the same.",  &
    & file=__FILE__,  &
    & line=__LINE__,  &
    & routine=myName)

  CALL Assert(nn=xidim,  &
    & msg="[INTERNAL ERROR] :: xidim of all entities is not the same.",  &
    & file=__FILE__,  &
    & line=__LINE__,  &
    & routine=myName)
#endif

  DO ii = 1, SIZE(obj%tElements_topology_wise)
    aint = obj%tElements_topology_wise(ii)
    IF (aint .GT. 0) THEN
      obj%tElemTopologies = obj%tElemTopologies + 1
      obj%elemTopologies(obj%tElemTopologies) =  &
        & ReferenceElementInfo%elemTopologyName(ii)
    END IF
  END DO

  obj%maxNNE = MAXVAL(nne)
  obj%tElements = SUM(tElements)

  CALL Reallocate(connectivity, obj%maxNNE, obj%tElements)
  CALL Reallocate(elemNumber, obj%tElements)

  IF (dim .GT. 0) THEN
    CALL HDF5GetEntities(hdf5=hdf5, group=group, dim=dim - 1,  &
      & tEntities=maxBoundingEntities, myName=myName, modName=modName)
  ELSE
    maxBoundingEntities = 0
  END IF

  CALL Reallocate(boundingEntity, maxBoundingEntities)

  aint = 0
  bint = 0
  DO ii = 1, tEntities
    dsetname = prefix//tostring(entities(ii))
    CALL MeshImportVector(obj, hdf5, dsetname, temp_int_2d, temp_int_1d)
    aint = bint + 1
    bint = bint + tElements(ii)
    connectivity(1:nne(ii), aint:bint) =  &
      & temp_int_2d(1:nne(ii), 1:tElements(ii))
    elemNumber(aint:bint) = temp_int_1d(1:tElements(ii))

    DO jj = 1, SIZE(obj%boundingEntity)
      kk = ABS(obj%boundingEntity(jj))
      boundingEntity(kk) = boundingEntity(kk) + obj%boundingEntity(jj)
    END DO
  END DO

  aint = 0
  DO ii = 1, SIZE(boundingEntity)
    IF (boundingEntity(ii) .NE. 0) aint = aint + 1
  END DO

  CALL Reallocate(obj%boundingEntity, aint)

  aint = 0
  DO ii = 1, SIZE(boundingEntity)
    IF (boundingEntity(ii) .NE. 0) THEN
      aint = aint + 1
      obj%boundingEntity(aint) = boundingEntity(ii)
    END IF
  END DO

  obj%maxElemNum = MAXVAL(elemNumber)
  obj%minElemNum = MINVAL(elemNumber)
  obj%maxNptrs = MAXVAL(connectivity)
  obj%minNptrs = MINVAL(connectivity)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time in importing vector data [MeshImportVector] : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  ALLOCATE (obj%elementData(obj%tElements))
  CALL Reallocate(obj%local_elemNumber, obj%maxElemNum)

  bint = 0
  DO ii = 1, tEntities
    aint = bint + 1
    bint = bint + tElements(ii)
    CALL MeshImportElementDataFromDim( &
      & obj=obj, &
      & elemType=elemType(ii), &
      & meshID=entities(ii), &
      & connectivity=connectivity,  &
      & elemNumber=elemNumber,  &
      & istart=aint,  &
      & iend=bint,  &
      & nne=nne(ii))
  END DO

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
     & " : time in importing element data [MeshImportElementData] : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  CALL MeshImportNodeDataFromDim(obj, connectivity)

  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time in importing node data [MeshImportNodeData] : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
    CALL TypeCPUTime%SetStartTime()
  END IF

  IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
  IF (ALLOCATED(elemNumber)) DEALLOCATE (elemNumber)
  IF (ALLOCATED(temp_int_2d)) DEALLOCATE (temp_int_2d)
  IF (ALLOCATED(temp_int_1d)) DEALLOCATE (temp_int_1d)
  IF (ALLOCATED(boundingEntity)) DEALLOCATE (boundingEntity)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE MeshImportFromDim

END MODULE AbstractMeshUtility
