!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to get the boundary data in the mesh.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

  ! Initiate and open the mesh file which is in [[HDF5File_]] format. Then, create an instance of mesh.

  CALL meshfile%Initiate(FileName="./mesh.h5", MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

  ! Now we get the boundary element data.
  ! Element number 515 is a boundary element, and one of its face concides
  ! with the mesh boundary.

  CALL Display(obj%getConnectivity(515), "cellNptrs=")
  nptrs = obj%GetBoundaryElementData(globalElement=515)
  CALL Display(nptrs, "Boundary element data for 515=")

  ! Now we get the boundary element data.
  ! Element number 316 is a boundary element, and one of its face concides with the mesh boundary.

  CALL Display(obj%getConnectivity(316), "cellNptrs=")
  nptrs = obj%GetBoundaryElementData(globalElement=316)
  CALL Display(nptrs, "Boundary element data for 316=")

  CALL obj%Deallocate()
  CALL meshfile%Close()
  CALL meshfile%Deallocate()
END PROGRAM main
