!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the total number of vertex nodes of a quadratic mesh
!
! small_tri6_mesh.h5 has 14 Triangle6 elements with 37 nodes, of
! which 12 are vertex nodes. Each element has 6 nodes and 3 vertex
! nodes, and the order of the elements is 2.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/small_tri6_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: iel

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%GetTotalNodes() .EQ. 37_I4B, "GetTotalNodes")
  CALL OK(obj%GetTotalVertexNodes() .EQ. 12_I4B, "GetTotalVertexNodes")
  iel = obj%GetGlobalElemNumber(1_I4B)
  CALL OK(obj%GetNNE(iel) .EQ. 6_I4B, "GetNNE")
  CALL OK(obj%GetTotalVertexNodes(globalElement=iel, islocal=.FALSE.) &
    .EQ. 3_I4B, "vertex nodes of an element")
  CALL OK(obj%GetOrder(globalElement=iel) .EQ. 2_I4B, "GetOrder")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
