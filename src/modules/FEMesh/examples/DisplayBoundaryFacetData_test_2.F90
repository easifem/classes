!> author: Vikas Sharma, Ph. D.
! date: 2025-06-02
! summary:  This example shows how to use `DisplayBoundaryFacetData`
! method to display the boundary facet data.
! In this example we do create an instance of Domain, therefore,
! we build the information about `DOMAIN_BOUNDARY_ELEMENT` and `BOUNDARY_ELEMENT`.
!
! This example shows how to use `DisplayBoundaryFacetData` method to display
! the boundary facet data.
!
! In this example we do create an instance of Domain, therefore, we build the
! information about `DOMAIN_BOUNDARY_ELEMENT` and `BOUNDARY_ELEMENT`.

PROGRAM main
  use FEMesh_Class
  use FEDomain_Class
  use HDF5File_Class
  use GlobalData

  IMPLICIT NONE

  CLASS(AbstractMesh_), POINTER :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: domainFile
  INTEGER(I4B) :: iel, ii
  CHARACTER(LEN=*), PARAMETER :: filename="./meshdata/small_tri3_mesh.h5"

  CALL domainFile%Initiate(FileName=filename, MODE="READ")
  CALL domainFile%Open()
  CALL dom%Initiate(hdf5=domainFile, group="")

  obj => dom%GetMeshPointer(dim=dom%getNSD(), entityNum=1)

  CALL obj%DisplayBoundaryFacetData("BoundaryFacetData = ")

  CALL obj%Deallocate()
  CALL domainFile%Deallocate()
END PROGRAM main
