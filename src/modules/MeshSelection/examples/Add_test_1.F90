!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing Add, Set and allocation queries of MeshSelection_

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BaseType, ONLY: math => TypeMathOpt
USE BoundingBox_Method, ONLY: BoundingBox_Initiate => Initiate
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MeshSelection_Class, ONLY: MeshSelection_
USE MeshSelection_Class, ONLY: MeshSelectionDeallocate
USE MeshSelection_Class, ONLY: MeshSelectionDisplay
USE MeshSelection_Class, ONLY: MeshSelectionReallocate
USE MeshSelection_Class, ONLY: MeshSelectionSet
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(MeshSelection_) :: obj

CALL e%SetQuietMode(math%yes)

! Test 1: Add appends to the list of dim, Set sorts and removes duplicates
BLOCK
  CALL obj%Initiate(isSelectionByMeshID=math%yes)
  CALL obj%Add(dim=0, meshID=[5, 3])
  CALL obj%Add(dim=0, meshID=[3, 1])
  CALL OK(ALL(obj%GetMeshID(dim=0) .EQ. [5, 3, 3, 1]), "Add: appends")

  CALL obj%Set()
  CALL OK(ALL(obj%GetMeshID(dim=0) .EQ. [1, 3, 5]), "Set: unique, sorted")

  CALL obj%Add(dim=1, meshID=[2])
  CALL obj%Add(dim=2, meshID=[3])
  CALL obj%Add(dim=3, meshID=[4])
  CALL OK(ALL(obj%GetMeshID(dim=1) .EQ. [2]), "Add: dim=1")
  CALL OK(ALL(obj%GetMeshID(dim=2) .EQ. [3]), "Add: dim=2")
  CALL OK(ALL(obj%GetMeshID(dim=3) .EQ. [4]), "Add: dim=3")
END BLOCK

! Test 2: GetMeshID and IsMeshIDAllocated for lists that do not exist
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[1])
  CALL OK(SIZE(obj%GetMeshID(dim=1)) .EQ. 0, "GetMeshID: empty list")
  CALL OK(SIZE(obj%GetMeshID(dim=4)) .EQ. 0, "GetMeshID: unknown dim")
  CALL OK(obj%IsMeshIDAllocated(dim=2), "IsMeshIDAllocated: dim=2")
  CALL OK(.NOT. obj%IsMeshIDAllocated(dim=1), "IsMeshIDAllocated: dim=1")
  CALL OK(.NOT. obj%IsMeshIDAllocated(dim=4), "IsMeshIDAllocated: dim=4")
END BLOCK

! Test 3: Add sets the selection flag, also when Initiate did not set it
BLOCK
  CALL obj%Initiate()
  CALL OK(.NOT. obj%IsSelectionByMeshID(), "Add: meshID flag before")
  CALL obj%Add(dim=2, meshID=[1])
  CALL OK(obj%IsSelectionByMeshID(), "Add: meshID flag after")
  CALL obj%Add(dim=2, elemNum=[1])
  CALL OK(obj%IsSelectionByElemNum(), "Add: elemNum flag")
  CALL OK(obj%IsElemNumAllocated(dim=2), "IsElemNumAllocated: dim=2")
  CALL OK(.NOT. obj%IsElemNumAllocated(dim=3), "IsElemNumAllocated: dim=3")
END BLOCK

! Test 4: Add without dim is ignored for meshID, elemNum and box
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(meshID=[1])
  CALL obj%Add(elemNum=[1])
  CALL OK(.NOT. obj%IsSelectionByMeshID(), "Add: meshID needs dim")
  CALL OK(.NOT. obj%IsSelectionByElemNum(), "Add: elemNum needs dim")
END BLOCK

! Test 5: nodeNum with and without dim, Set collects them in nodeNum
BLOCK
  INTEGER(I4B) :: ans(10), tsize

  CALL obj%Initiate()
  CALL obj%Add(dim=0, nodeNum=[3, 1])
  CALL obj%Add(dim=2, nodeNum=[1, 2])
  CALL OK(obj%IsSelectionByNodeNum(), "Add: nodeNum flag")
  CALL OK(.NOT. obj%IsNodeNumAllocated(), "Add: dim lists stay separate")

  CALL obj%Add(nodeNum=[7, 3])
  CALL obj%Set()
  CALL OK(obj%IsNodeNumAllocated(), "Set: nodeNum allocated")

  CALL obj%GetNodeNum(ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 4, "Set: size of nodeNum")
  CALL OK(ALL(ans(1:tsize) .EQ. [1, 2, 3, 7]), "Set: nodeNum")

  CALL obj%Set()
  CALL obj%GetNodeNum(ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 4, "Set: second call changes nothing")
END BLOCK

! Test 6: selection by box
BLOCK
  TYPE(BoundingBox_) :: box(1)

  CALL BoundingBox_Initiate(box(1), nsd=2_I4B, &
                            lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, &
                                 0.0_DFP, 0.0_DFP])
  CALL obj%Initiate()
  CALL obj%Add(dim=2, box=box)
  CALL OK(obj%IsSelectionByBox(), "Add: box flag")
END BLOCK

! Test 7: vector of instances
BLOCK
  TYPE(MeshSelection_), ALLOCATABLE :: vec(:)

  CALL MeshSelectionReallocate(vec, 2_I4B)
  CALL OK(SIZE(vec) .EQ. 2, "MeshSelectionReallocate")

  CALL vec(1)%Initiate()
  CALL vec(2)%Initiate()
  CALL vec(1)%Add(dim=1, meshID=[2, 2, 1])
  CALL vec(2)%Add(dim=1, meshID=[4, 4])
  CALL MeshSelectionSet(vec)
  CALL OK(ALL(vec(1)%GetMeshID(dim=1) .EQ. [1, 2]), "MeshSelectionSet: 1")
  CALL OK(ALL(vec(2)%GetMeshID(dim=1) .EQ. [4]), "MeshSelectionSet: 2")

  CALL MeshSelectionDeallocate(vec)
  CALL OK(.NOT. ALLOCATED(vec), "MeshSelectionDeallocate")
END BLOCK

! Test 8: Display writes to the given unit
BLOCK
  INTEGER(I4B) :: unitNo, nline, ios
  CHARACTER(128) :: aline, iomsg

  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[1])

  OPEN (NEWUNIT=unitNo, STATUS="SCRATCH", ACTION="READWRITE")
  CALL MeshSelectionDisplay(obj, "selection", unitNo=unitNo)
  REWIND (unitNo)
  nline = 0
  readLoop: DO
    READ (unitNo, "(A)", IOSTAT=ios, IOMSG=iomsg) aline
    IF (ios .NE. 0) EXIT readLoop
    nline = nline + 1
  END DO readLoop
  CLOSE (unitNo)
  CALL OK(nline .GT. 0, "Display: writes to unitNo")
END BLOCK

CALL obj%DEALLOCATE()
END PROGRAM main
