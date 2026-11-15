!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the face and edge orders of an element
!
! GetFaceOrder returns the order of each face of an element in
! ans(1:3, 1:ncol); the three rows hold the same order. GetEdgeOrder
! returns the order of each edge. With H1 hierarchical basis of homogeneous
! order 2 on a hexahedral mesh an element has 6 faces and 12 edges, all of
! order 2.
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
  INTEGER(I4B) :: faceOrder(3, 10), edgeOrder(20), nrow, ncol, tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, "")

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    order=2, dom=dom)

  CALL obj%GetFaceOrder(ans=faceOrder, nrow=nrow, ncol=ncol, &
                        globalElement=1, islocal=math%yes)
  CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 6_I4B, "GetFaceOrder: shape")
  CALL OK(ALL(faceOrder(1:nrow, 1:ncol) .EQ. 2), "GetFaceOrder: order")

  CALL obj%GetEdgeOrder(ans=edgeOrder, tsize=tsize, globalElement=1, &
                        islocal=math%yes)
  CALL OK(tsize .EQ. 12_I4B, "GetEdgeOrder: number of edges")
  CALL OK(ALL(edgeOrder(1:tsize) .EQ. 2), "GetEdgeOrder: order")

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
