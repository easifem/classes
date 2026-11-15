!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get and set parameters of a mesh
!
! GetParam returns the scalar parameters of a mesh, SetParam sets
! them. SetBoundingBox sets the bounding box parameters (minX, ...)
! from a box or from node coordinates.
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
  USE BaseType, ONLY: BoundingBox_
  USE BaseType, ONLY: math => TypeMathOpt
  USE BoundingBox_Method, ONLY: BoundingBoxInitiate => Initiate
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
  TYPE(BoundingBox_) :: box
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  REAL(DFP) :: maxX, minX, maxY
  INTEGER(I4B) :: tNodes, tElements, nsd, uid

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%GetParam(tNodes=tNodes, tElements=tElements, nsd=nsd, &
    uid=uid, maxX=maxX)
  CALL OK(tNodes .EQ. 9_I4B .AND. tElements .EQ. 4_I4B, "counts")
  CALL OK(nsd .EQ. 2_I4B .AND. uid .EQ. 1_I4B, "nsd and uid")
  CALL OK(ABS(maxX - 1.0_DFP) .LT. tol, "maxX")

  CALL obj%SetParam(uid=42_I4B)
  CALL obj%GetParam(uid=uid)
  CALL OK(uid .EQ. 42_I4B, "SetParam")

  CALL BoundingBoxInitiate(obj=box, nsd=3_I4B, &
    lim=[-1.0_DFP, 2.0_DFP, -3.0_DFP, &
    4.0_DFP, 0.0_DFP, 0.0_DFP])
  CALL obj%SetBoundingBox(box=box)
  CALL obj%GetParam(minX=minX, maxY=maxY)
  CALL OK(ABS(minX + 1.0_DFP) .LT. tol .AND. &
    ABS(maxY - 4.0_DFP) .LT. tol, "SetBoundingBox(box)")

  CALL obj%GetNodeCoord(nodeCoord=xij, hdf5=meshfile, group="")
  CALL obj%SetBoundingBox(nodes=xij)
  CALL obj%GetParam(minX=minX, maxY=maxY)
  CALL OK(ABS(minX) .LT. tol .AND. ABS(maxY - 1.0_DFP) .LT. tol, &
    "SetBoundingBox(nodes)")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
