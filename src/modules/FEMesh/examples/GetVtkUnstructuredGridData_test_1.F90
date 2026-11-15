!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: VTK unstructured grid data of a mesh
!
! GetVtkUnstructuredGridData returns the points, connectivity,
! offsets and VTK cell types of the active elements of a mesh.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (global numbers 13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2
!
! Element 13 has the nodes [1, 5, 9, 7], so its zero based point ids
! are [0, 4, 8, 6]. All cells are VTK_QUAD (9).

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: INT8
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  REAL(DFP), ALLOCATABLE :: points(:, :)
  INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%GetVtkUnstructuredGridData(points=points, &
                                      connectivity=connectivity, &
                                      offsets=offsets, types=types)

  CALL OK(ALL(SHAPE(points) .EQ. [3, 9]), "points: shape")
  CALL OK(ALL(ABS(points(:, 9) - [0.5_DFP, 0.5_DFP, 0.0_DFP]) &
              .LT. tol), "points: node 9")
  CALL OK(ALL(ABS(points(3, :)) .LT. tol), "points: z = 0 in 2D")

  CALL OK(SIZE(types) .EQ. 4, "types: number of cells")
  CALL OK(ALL(types .EQ. 9_INT8), "types: VTK_QUAD")

  CALL OK(ALL(offsets .EQ. [0, 4, 8, 12, 16]), "offsets")
  CALL OK(SIZE(connectivity) .EQ. 16, "connectivity: size")
  CALL OK(ALL(connectivity(1:4) .EQ. [0, 4, 8, 6]), &
          "connectivity: element 13")
  CALL OK(ALL(connectivity .GE. 0 .AND. connectivity .LT. 9), &
          "connectivity: zero based")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
