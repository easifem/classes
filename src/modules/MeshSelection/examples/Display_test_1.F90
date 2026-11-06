!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing Display of MeshSelection_
!
! Display writes the state of the 16 lists twice. All of these lines go
! to unitNo.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE MeshSelection_Class, ONLY: MeshSelection_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(MeshSelection_) :: obj

CALL e%SetQuietMode(math%yes)

! Test 1: an object which is not initiated writes only msg and its state
BLOCK
  CALL OK(CountLines(obj, " ALLOCATED :") .EQ. 0, "not initiated: lines")
END BLOCK

! Test 2: all state lines go to unitNo
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[2, 1])
  CALL OK(CountLines(obj, " ALLOCATED :") .EQ. 32, "unitNo: state lines")
  CALL OK(CountLines(obj, "surfaceMeshID ALLOCATED :") .EQ. 2, &
          "unitNo: meshID state")
  CALL OK(CountLines(obj, "SurfaceMeshID : ") .GE. 1, "unitNo: meshID data")
END BLOCK

CALL obj%DEALLOCATE()

CONTAINS

!> Returns the number of lines of Display which contain text
FUNCTION CountLines(sel, text) RESULT(ans)
  TYPE(MeshSelection_), INTENT(IN) :: sel
  CHARACTER(*), INTENT(IN) :: text
  INTEGER(I4B) :: ans

  CHARACTER(256) :: line, msg
  INTEGER(I4B) :: unitNo, ios

  ans = 0
  OPEN (NEWUNIT=unitNo, STATUS="SCRATCH", ACTION="READWRITE")
  CALL sel%Display("selection", unitNo=unitNo)
  REWIND (unitNo)
  lines: DO
    READ (unitNo, "(A)", IOSTAT=ios, IOMSG=msg) line
    IF (ios .NE. 0) EXIT lines
    IF (INDEX(line, text) .GT. 0) ans = ans + 1
  END DO lines
  CLOSE (unitNo)
END FUNCTION CountLines

END PROGRAM main
