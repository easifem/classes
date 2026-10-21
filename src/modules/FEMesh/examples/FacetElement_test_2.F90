!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to obtain the `ReferenceElement_` of an
!          internal facet element.
!
! - First we obtain the local facet ID of the internal facet element in the
! masterCell
! - Then we use this localFacetID to get the reference element stored in the
! mesh object.
!
! We can also follow this procedure for slaveCell number.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: iel, ii, masterCell, slaveCell
  CHARACTER(*), PARAMETER :: filename="./mesh.h5"

  ! Initiate and open the mesh file which is in [[HDF5File_]] format.
  ! Then, create an instance of mesh.

  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

  ! Getting the [[ReferenceElement_]] of an internal facet element.

  iel = 1
  CALL Display("facetElement="//tostring(iel))

  ! Getting the master and slave cell number of the facet element.
  CALL Display(obj%getCellNumber(facetElement=iel, &
    & elementType=INTERNAL_ELEMENT), &
    & "master and slave cell number = ")

  ! Getting the connectivity information of facet element.
  CALL Display(obj%getFacetConnectivity(facetElement=iel, &
    & elementType=INTERNAL_ELEMENT, &
    & isMaster=.TRUE.), &
    & "facet connectivity=")

  ! Obtaining the local facet number of given facet element.
  ii = obj%getLocalFacetID(facetElement=iel, &
    & elementType=INTERNAL_ELEMENT, &
    & isMaster = .TRUE.)
  CALL Display(ii, "localFacetID=")

  ! Accessing the reference facet element by using localFacetID.
  CALL Display(obj%facetElements(ii), "refElem=")

  ! If we know the cell number and the localFacetID then we can get the connectivity
  ! of that facet element by using the following.
  masterCell = obj%getMasterCellNumber(facetElement=iel, &
    & elementType=INTERNAL_ELEMENT)
  CALL Display(obj%getFacetConnectivity(globalElement=masterCell, &
    & iface=ii), "connectivity of iface=")

  ! Now let us repeat this process for slave Cell number.
  iel = 1
  CALL Display("== slave cell number ==")
  CALL Display("facetElement="//tostring(iel))
  CALL Display(obj%getFacetConnectivity(facetElement=iel, &
    & elementType=INTERNAL_ELEMENT, &
    & isMaster=.FALSE.), &
    & "facet connectivity=")
  ii = obj%getLocalFacetID(facetElement=iel, &
    & elementType=INTERNAL_ELEMENT, &
    & isMaster = .FALSE.)
  CALL Display(ii, "localFacetID=")
  CALL Display(obj%facetElements(ii), "refElem=")
  slaveCell = obj%getSlaveCellNumber(facetElement=iel, &
    & elementType=INTERNAL_ELEMENT)
  CALL Display(obj%getFacetConnectivity(globalElement=slaveCell, &
    & iface=ii), "connectivity of iface=")

  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
