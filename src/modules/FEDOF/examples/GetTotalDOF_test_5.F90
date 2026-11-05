!> author: Vikas Sharma, Ph. D.
! date: 2026-11-02
! summary: Testing GetTotalDOF and GetTotalVertexDOF in broken spaces
!
! In a broken space (isBrokenSpace = true) every cell owns the full basis
! of its element: (p+1)(p+2)/2 DOFs for a triangle of order p. No DOF is
! shared, so the total number of DOFs is the number of cells times this
! size, and vertices, edges and faces own no DOFs.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: order

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

DO order = 1, 3
  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                    isBrokenSpace=math%yes, order=order, dom=dom)

  ! Test 1: total number of DOFs is tCells times the cell basis size
  BLOCK
    INTEGER(I4B) :: want
    want = obj%GetTotalCells() * (order + 1) * (order + 2) / 2
    CALL OK(obj%GetTotalDOF() .EQ. want, &
            "GetTotalDOF, order="//ToString(order))
  END BLOCK

  ! Test 2: no vertex DOFs in a broken space
  BLOCK
    CALL OK(obj%GetTotalVertexDOF() .EQ. 0, &
            "GetTotalVertexDOF is zero, order="//ToString(order))
  END BLOCK

  ! Test 3: DOFs of an element, all of them belong to the cell
  BLOCK
    INTEGER(I4B) :: want
    want = (order + 1) * (order + 2) / 2
    CALL OK(obj%GetTotalDOF(globalElement=1, islocal=math%yes) &
            .EQ. want, "GetTotalDOF of element, order="//ToString(order))
    CALL OK(obj%GetTotalDOF(globalElement=1, opt="Cell", &
                            islocal=math%yes) .EQ. want, &
            "GetTotalDOF opt=Cell, order="//ToString(order))
    CALL OK(obj%GetTotalDOF(globalElement=1, opt="Vertex", &
                            islocal=math%yes) .EQ. 0, &
            "GetTotalDOF opt=Vertex is zero, order="//ToString(order))
    CALL OK(obj%GetTotalDOF(globalElement=1, opt="Face", &
                            islocal=math%yes) .EQ. 0, &
            "GetTotalDOF opt=Face is zero, order="//ToString(order))
  END BLOCK
END DO

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
