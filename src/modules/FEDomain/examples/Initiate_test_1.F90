!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate a domain from a HDF5 mesh file
!
! Initiate reads the meta data and node coordinates of the domain,
! and makes one mesh for each dimension up to nsd: points, curves,
! and surfaces (and volumes for nsd = 3).
!
! The mesh very_small_quad4_mesh.h5 is the unit square: four
! Quadrangle4 elements (13 to 16), eight Line2 elements (5 to 12) on
! the boundary curves, four Point elements (1 to 4), and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL OK(obj%IsInitiated(), "IsInitiated")
  CALL OK(obj%GetNSD() .EQ. 2_I4B, "GetNSD")
  CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
  CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "surface elements")
  CALL OK(obj%GetTotalElements(DIM=1_I4B) .EQ. 8_I4B, "curve elements")
  CALL OK(obj%GetTotalElements(DIM=0_I4B) .EQ. 4_I4B, "point elements")
  CALL OK(obj%GetTotalEntities(DIM=2_I4B) .EQ. 1_I4B, "surface entities")
  CALL OK(obj%GetTotalEntities(DIM=1_I4B) .EQ. 4_I4B, "curve entities")
  CALL OK(obj%GetMinNodeNumber() .EQ. 1_I4B, "GetMinNodeNumber")
  CALL OK(obj%GetMaxNodeNumber() .EQ. 9_I4B, "GetMaxNodeNumber")
  CALL OK(obj%GetMinElemNumber() .EQ. 1_I4B, "GetMinElemNumber")
  CALL OK(obj%GetMaxElemNumber() .EQ. 16_I4B, "GetMaxElemNumber")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
