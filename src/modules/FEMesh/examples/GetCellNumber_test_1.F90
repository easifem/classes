!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the cells of a facet element
!
! Facet 2 is the internal facet [5, 9]. Its master cell is element
! 13 (local facet 2) and its slave cell is element 15 (local facet
! 4). Facet 1 is the boundary facet [1, 5] of element 13; it has
! no slave cell.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (global numbers 13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMeshGetFacetConnectivity
  USE BaseType, ONLY: math => TypeMathOpt
  USE ElemData_Class, ONLY: DOMAIN_BOUNDARY_ELEMENT
  USE ElemData_Class, ONLY: INTERNAL_ELEMENT
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: etype
  INTEGER(I4B), ALLOCATABLE :: con(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%InitiateFacetElements()

  CALL obj%GetFacetParam(facetElement=2_I4B, elementType=etype)
  CALL OK(etype .EQ. INTERNAL_ELEMENT, "GetFacetParam")
  CALL OK(ALL(obj%GetCellNumber(2_I4B, etype) .EQ. [13, 15]), &
    "GetCellNumber")
  CALL OK(obj%GetMasterCellNumber(2_I4B, etype) .EQ. 13_I4B, &
    "GetMasterCellNumber")
  CALL OK(obj%GetSlaveCellNumber(2_I4B, etype) .EQ. 15_I4B, &
    "GetSlaveCellNumber")
  CALL OK(obj%GetLocalFacetID(2_I4B, etype, .TRUE.) .EQ. 2_I4B, &
    "GetLocalFacetID master")
  CALL OK(obj%GetLocalFacetID(2_I4B, etype, .FALSE.) .EQ. 4_I4B, &
    "GetLocalFacetID slave")
  con = AbstractMeshGetFacetConnectivity(obj, 2_I4B, etype, .TRUE.)
  CALL OK(ALL(con .EQ. [5, 9]), "AbstractMeshGetFacetConnectivity")
  CALL OK(.NOT. obj%IsDomainFacetElement(2_I4B), "internal facet")

  CALL obj%GetFacetParam(facetElement=1_I4B, elementType=etype)
  CALL OK(etype .EQ. DOMAIN_BOUNDARY_ELEMENT, "boundary facet")
  CALL OK(obj%IsDomainFacetElement(1_I4B), "IsDomainFacetElement")
  CALL OK(ALL(obj%GetCellNumber(1_I4B, etype) .EQ. [13, 0]), &
    "no slave cell")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
