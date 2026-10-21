!> author: Vikas Sharma, Ph. D.
! summary: GetNodeToElements_test_1

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
  CALL meshfile%Open()
  CALL obj%SetShowTime(.true.)
  !Initiate an instance of `Mesh_`
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")
  ! Initiate dynamic data structure
  CALL obj%InitiateNodeToElements()
  !Display the content of mesh.
  CALL obj%DisplayMeshInfo(filename)
  ! Display the mesh data
  block
    INTEGER(I4B), ALlOCATABLE :: ans(:), exact_ans(:)
    INTEGER(I4B), PARAMETER :: globalNode = 10
    ans = obj%GetNodeToElements(globalNode)
    ans = sort(ans)
    exact_ans = [14, 15, 22, 23, 24, 25]
    CALL OK(ALL(exact_ans .eq. ans), "GetNodeToElements: ")
  end block
  block
    INTEGER(I4B), ALlOCATABLE :: ans(:), exact_ans(:)
    INTEGER(I4B), PARAMETER :: globalNode = 5
    ans = obj%GetNodeToElements(globalNode)
    ans = sort(ans)
    exact_ans = [15, 17, 24]
    CALL OK(ALL(exact_ans .eq. ans), "GetNodeToElements: ")
  end block
  block
    INTEGER(I4B), ALlOCATABLE :: ans(:), exact_ans(:)
    INTEGER(I4B), PARAMETER :: globalNode = 5
    ans = obj%GetNodeToElements(globalNode, islocal=.true.)
    ans = sort(ans)
    exact_ans = [15, 17, 24]
    CALL OK(ALL(exact_ans .eq. ans), "GetNodeToElements: ")
  end block
  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
