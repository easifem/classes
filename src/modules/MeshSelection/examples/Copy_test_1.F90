!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing assignment (obj_Copy) of MeshSelection_
!
! The box lists are private and have no getter, so IsBoxAllocated reads
! their state from the output of Display.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: BoundingBox_Initiate => Initiate
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MeshSelection_Class, ONLY: MeshSelection_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(MeshSelection_) :: obj, obj2

CALL e%SetQuietMode(math%yes)

CALL obj%Initiate()
CALL obj%Add(dim=2, meshID=[2, 1])
CALL obj%Add(dim=1, elemNum=[7, 8])
CALL obj%Add(nodeNum=[4, 3])
CALL obj%Set()

! Test 1: assignment copies the flags and the data
BLOCK
  INTEGER(I4B) :: ans(4), tsize

  obj2 = obj
  CALL OK(obj2%IsInitiated(), "Copy: IsInitiated")
  CALL OK(obj2%IsSelectionByMeshID(), "Copy: meshID flag")
  CALL OK(obj2%IsSelectionByElemNum(), "Copy: elemNum flag")
  CALL OK(obj2%IsSelectionByNodeNum(), "Copy: nodeNum flag")
  CALL OK(.NOT. obj2%IsSelectionByBox(), "Copy: box flag")
  CALL OK(ALL(obj2%GetMeshID(dim=2) .EQ. [1, 2]), "Copy: meshID")

  CALL obj2%GetElemNum(dim=1, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "Copy: size of elemNum")
  CALL OK(ALL(ans(1:tsize) .EQ. [7, 8]), "Copy: elemNum")

  CALL obj2%GetNodeNum(ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "Copy: size of nodeNum")
  CALL OK(ALL(ans(1:tsize) .EQ. [3, 4]), "Copy: nodeNum")
END BLOCK

! Test 2: the copy does not share data with the source
BLOCK
  CALL obj%Add(dim=2, meshID=[9])
  CALL OK(SIZE(obj%GetMeshID(dim=2)) .EQ. 3, "Copy: source grows")
  CALL OK(SIZE(obj2%GetMeshID(dim=2)) .EQ. 2, "Copy: copy is unchanged")
END BLOCK

! Test 3: the data of the destination is replaced by the source
BLOCK
  TYPE(MeshSelection_) :: other

  CALL other%Initiate()
  CALL other%Add(dim=3, meshID=[5])
  obj2 = other
  CALL OK(.NOT. obj2%IsSelectionByElemNum(), "Copy: replaces elemNum flag")
  CALL OK(ALL(obj2%GetMeshID(dim=3) .EQ. [5]), "Copy: replaces meshID")
  CALL OK(SIZE(obj2%GetMeshID(dim=2)) .EQ. 0, "Copy: drops old meshID")
  CALL OK(.NOT. obj2%IsElemNumAllocated(dim=1), "Copy: drops old elemNum")
  CALL OK(.NOT. obj2%IsNodeNumAllocated(), "Copy: drops old nodeNum")
  CALL other%DEALLOCATE()
END BLOCK

! Test 4: a source without boxes drops the boxes of the destination
BLOCK
  CHARACTER(*), PARAMETER :: names(4) = &
                             [CHARACTER(10) :: "pointBox", "curveBox", &
                             "surfaceBox", "volumeBox"]
  TYPE(BoundingBox_) :: box(1)
  TYPE(MeshSelection_) :: dest, other
  INTEGER(I4B) :: ii

  CALL BoundingBox_Initiate(box(1), nsd=2_I4B, &
                            lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, &
                                 0.0_DFP, 0.0_DFP])
  CALL dest%Initiate()
  DO ii = 0, 3
    CALL dest%Add(dim=ii, box=box)
  END DO
  DO ii = 1, SIZE(names)
    CALL OK(IsBoxAllocated(dest, TRIM(names(ii))), &
            "Copy: before "//TRIM(names(ii)))
  END DO

  CALL other%Initiate()
  CALL other%Add(dim=3, meshID=[5])
  dest = other
  CALL OK(.NOT. dest%IsSelectionByBox(), "Copy: drops box flag")
  DO ii = 1, SIZE(names)
    CALL OK(.NOT. IsBoxAllocated(dest, TRIM(names(ii))), &
            "Copy: drops old "//TRIM(names(ii)))
  END DO

  CALL other%DEALLOCATE()
  CALL dest%DEALLOCATE()
END BLOCK

CALL obj%DEALLOCATE()
CALL obj2%DEALLOCATE()

CONTAINS

!> Returns the value of the line "<name> ALLOCATED :" of Display
FUNCTION IsBoxAllocated(sel, name) RESULT(ans)
  TYPE(MeshSelection_), INTENT(IN) :: sel
  CHARACTER(*), INTENT(IN) :: name
  LOGICAL(LGT) :: ans

  CHARACTER(*), PARAMETER :: suffix = " ALLOCATED :"
  CHARACTER(256) :: line, msg
  INTEGER(I4B) :: unitNo, ios, pos

  ans = math%no
  OPEN (NEWUNIT=unitNo, STATUS="SCRATCH", ACTION="READWRITE")
  CALL sel%Display("", unitNo=unitNo)
  REWIND (unitNo)
  lines: DO
    READ (unitNo, "(A)", IOSTAT=ios, IOMSG=msg) line
    IF (ios .NE. 0) EXIT lines
    pos = INDEX(line, name//suffix)
    IF (pos .EQ. 0) CYCLE lines
    READ (line(pos + LEN(name//suffix):), *) ans
    EXIT lines
  END DO lines
  CLOSE (unitNo)
END FUNCTION IsBoxAllocated

END PROGRAM main
