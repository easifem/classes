!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to get the bounding box of the mesh.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  TYPE(BoundingBox_) :: box

  ! Initiate and open the mesh file which is in [[HDF5File_]] format.
  ! Then initiate an instance of the [[Mesh_]].
  CALL meshfile%Initiate(FileName="./mesh.h5", MODE="READ")
  CALL meshfile%Open()

  ! Getting the bounding box of the mesh of 2d elements.
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")
  box = obj%GetBoundingBox()
  CALL Display(box, "")
  CALL obj%Deallocate()

  ! Getting the bounding box of the mesh of 1d elements.

  CALL obj%Initiate(hdf5=meshfile, group="/curveEntities_1")
  box = obj%GetBoundingBox()
  CALL Display(box, "")
  CALL obj%Deallocate()

  CALL meshfile%Close()
  CALL meshfile%Deallocate()
END PROGRAM main
