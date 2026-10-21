!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to get the element to element connectivity
!          data in the mesh.
!
! Now we get the full information about the element connected to the given
! global element numbers. In this case we set `onlyElements=.FALSE.`, or you
! can ignore this argument as it is optional, with default value of `.FALSE.`.
!
! We get the following output.
!
! - The first column is the global element number of neighbour.
! - The second column is the local facet number of the parent (iel=23)
! - The third column is the local facet number of neighbour
!
! If you only want the global element numbers of the neighbours, then you can
! set onlyElements=.TRUE.
!
! Now we get the full information about the element connected to the given
! global element numbers.
!
! Results:
!
!```txt
! Element connected to iel=23
! ---------------------------
!         22   1   2
!         26   2   3
!         25   3   1
!```
!
! Results:
!
!```txt
! Element connected to iel=23
! ---------------------------
!             22
!             26
!             25
!```

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
INTEGER(I4B), ALLOCATABLE :: elem2elem(:, :)
  CHARACTER(*), PARAMETER :: filename="../../Mesh/examples/meshdata/small_mesh.h5"
! Initiate and open the mesh file which is in [[HDF5File_]] format. Then, create an instance of mesh.
CALL meshfile%Initiate(filename, "READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

elem2elem = obj%GetElementToElements(globalElement=23, onlyElements=.FALSE.)
CALL Display(elem2elem, "Element connected to iel=23")

elem2elem = obj%GetElementToElements(globalElement=23, onlyElements=.TRUE.)
CALL Display(elem2elem, "Element connected to iel=23")

CALL obj%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
