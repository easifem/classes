!> author: Vikas Sharma, Ph. D.
! summary: GetNodeToElements_test_3

PROGRAM main
USE easifemBase
USE easifemClasses

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
  CHARACTER(*), PARAMETER :: filename="../../Mesh/examples/meshdata/small_mesh.h5"

CALL e%SetQuietMode(.TRUE.)
CALL meshfile%Initiate(FileName=filename, MODE="READ")
!Open the mesh file
CALL meshfile%OPEN()
CALL obj%SetShowTime(.TRUE.)
!Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")
! Initiate dynamic data structure
CALL obj%InitiateNodeToElements()
!Display the content of mesh.
CALL obj%DisplayMeshInfo(filename)
! Display the mesh data
BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  INTEGER(I4B), PARAMETER :: globalNode = 10
  INTEGER(I4B) :: tsize
  CALL Reallocate(ans, 100)
  CALL obj%GetNodeToElements_(ans=ans, tsize=tsize, globalNode=globalNode)
  ans(1:tsize) = sort(ans(1:tsize))
  exact_ans = [14, 15, 22, 23, 24, 25]
  CALL OK(ALL(exact_ans .EQ. ans(1:tsize)), "GetNodeToElements: ")
END BLOCK
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
