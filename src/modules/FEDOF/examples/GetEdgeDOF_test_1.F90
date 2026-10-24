!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the degrees of freedom on an edge
!
! GetTotalEdgeDOF and GetEdgeDOF return the number and the numbers of
! the degrees of freedom on an edge, given by its global edge number or by
! an element and its local edge number. For H1 hierarchical basis of order
! p an edge has p - 1 degrees of freedom. For order 2 the total number of
! degrees of freedom is 12 nodes + 20 edges + 11 faces + 2 cells = 45.
!
! The mesh small_hexa8_mesh.h5 has two Hexahedron8 elements which share
! one face: 12 nodes, 20 edges, 11 faces and 2 cells.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/small_hexa8_mesh.h5"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: ans(10), tsize, iedge, order

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, "")

  DO order = 2, 3
    CALL obj%Initiate(baseContinuity="H1", &
                      baseInterpolation="Hierarchical", order=order, &
                      dom=dom)
    IF (order .EQ. 2) CALL OK(obj%GetTotalDOF() .EQ. 45_I4B, &
                              "GetTotalDOF for order 2")

    DO iedge = 1, 12
      CALL OK(obj%GetTotalEdgeDOF(globalElement=1, localEdgeNumber=iedge, &
                                  islocal=math%yes) .EQ. order - 1, &
              "GetTotalEdgeDOF")
    END DO

    CALL obj%GetEdgeDOF(globalElement=1, localEdgeNumber=1, ans=ans, &
                        tsize=tsize, islocal=math%yes)
    CALL OK(tsize .EQ. order - 1, "GetEdgeDOF: number of entries")
    CALL OK(ALL(ans(1:tsize) .GT. 12), &
            "GetEdgeDOF: edge DOF follow the vertex DOF")
  END DO

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
