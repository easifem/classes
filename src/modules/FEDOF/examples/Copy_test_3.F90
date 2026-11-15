!> author: Vikas Sharma, Ph. D.
! date: 2026-11-02
! summary: Testing Copy of a FEDOF of a broken space
!
! The copy of a FEDOF of a broken space must be a broken space too: an
! element has no vertex DOFs and the same number of DOFs as in the
! original.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
TYPE(FEDOF_) :: obj, obj2
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                  isBrokenSpace=math%yes, order=2, dom=dom)
CALL obj2%Copy(obj)

! Test 1: same total number of DOFs
BLOCK
  CALL OK(obj2%GetTotalDOF() .EQ. obj%GetTotalDOF(), "GetTotalDOF")
END BLOCK

! Test 2: the copy is a broken space, elements have no vertex DOFs
BLOCK
  INTEGER(I4B) :: found
  found = obj2%GetTotalDOF(globalElement=1, opt="Vertex", &
                           islocal=math%yes)
  CALL OK(found .EQ. 0, "copy: no vertex DOFs")
  CALL OK(obj2%GetTotalDOF(globalElement=1, islocal=math%yes) .EQ. &
          obj%GetTotalDOF(globalElement=1, islocal=math%yes), &
          "copy: same number of DOFs of an element")
END BLOCK

CALL obj2%DEALLOCATE()
CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
