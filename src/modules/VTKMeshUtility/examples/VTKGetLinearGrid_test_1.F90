!> author: Shion Shimizu
! date: 2026-10-24
! summary: VTK linear grid from a mesh

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
USE VTKMeshUtility, ONLY: VTKGetLinearGrid
IMPLICIT NONE
TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
REAL(DFP), ALLOCATABLE :: points(:, :)
INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
CALL meshfile%Initiate(fileName="./meshdata/small_tri3_mesh.h5", &
                       MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=2)
CALL VTKGetLinearGrid(mesh=amesh, points=points, &
                      connectivity=connectivity, offsets=offsets, &
                      types=types)
CALL OK(SIZE(points, 2) .EQ. amesh%GetTotalNodes(), "nPoints")
CALL OK(SIZE(types) .EQ. amesh%GetTotalElements(), "nCells")
CALL OK(ALL(types .EQ. 5_INT8), "VTK_TRIANGLE")
CALL OK(offsets(SIZE(offsets)) .EQ. SIZE(connectivity), "offsets")
CALL amesh%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
