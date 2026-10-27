! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Reader and writer of VTKHDF (UnstructuredGrid) files
!
! VTKHDF is a VTK file format based on HDF5, which can be opened by
! ParaView (VTK >= 9.3). See
! https://docs.vtk.org/en/latest/vtk_file_formats/vtkhdf_file_format/
!
! VtkHDF5File_ extends HDF5File_, so all the methods of HDF5File_ can be
! used to write additional data in the same file.

MODULE VtkHDF5File_Class
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: math => TypeMathOpt
USE FEDOF_Class, ONLY: FEDOF_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE HDF5File_Class, ONLY: HDF5ChunkPolicy_
USE String_Class, ONLY: String
USE tomlf, ONLY: toml_table
IMPLICIT NONE
PRIVATE

PUBLIC :: VtkHDF5File_
PUBLIC :: VtkHDF5FilePointer_

!----------------------------------------------------------------------------
!                                                               VtkHDF5File_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: VTKHDF file for unstructured grids and time series
!
! The following layout is written (Fortran shape in parenthesis, the
! shape in C order, which is used by VTK and h5py, is reversed):
!
!```txt
! /VTKHDF                  attrs: Version = [2, 2], Type = UnstructuredGrid
!   NumberOfPoints(nParts), NumberOfCells(nParts),
!   NumberOfConnectivityIds(nParts)
!   Points(3, nPoints), Connectivity(nIds), Offsets(nCells+1),
!   Types(nCells) (uint8)
!   PointData/<name>(nComp, nPoints) or (nPoints)
!   CellData/<name>(nComp, nCells) or (nCells)
!   FieldData/<name>(nComp, nTuples)
!   Steps                  attr: NSteps (only if isTransient)
!     Values(NSteps), PartOffsets(NSteps), NumberOfParts(NSteps),
!     PointOffsets(NSteps), CellOffsets(1, NSteps),
!     ConnectivityIdOffsets(1, NSteps),
!     PointDataOffsets/<name>(NSteps), CellDataOffsets/<name>(NSteps),
!     FieldDataOffsets/<name>(NSteps), FieldDataSizes/<name>(2, NSteps)
!```
!
! All the datasets are extendable (see [[HDF5File_:Append]]).
! A mesh is written by WriteMesh. One mesh is one part (piece). If the
! mesh does not change, it is written only once and all the steps
! refer to it (the geometry offsets are repeated). If WriteMesh is
! called in a step, then the new mesh is used from that step.
!
! Two kinds of grids can be written:
!
! - Linear grid, WriteMesh(mesh): the nodes and elements of the mesh.
!   The vertex DOFs of fields are written as point data.
! - Lagrange grid, WriteMesh(fedof, geofedof): each cell has its own
!   equidistance points of the order of fedof, and higher order VTK
!   cells are used. Fields are evaluated at these points.
!
! Usage in a time dependent simulation:
!
!```fortran
! CALL obj%ImportFromToml(table)              ! reads [<table>.vtkhdf]
! IF (obj%IsActive()) THEN
!   CALL obj%InitiateVTKHDF5(filename="result.vtkhdf", mode="NEW", &
!                            vtkType="UnstructuredGrid", &
!                            isTransient=math%yes)
!   CALL obj%WriteMesh(mesh)                  ! or WriteMesh(fedof, ...)
! END IF
! ! every time step
! CALL obj%BeginStep(time=t)
! IF (obj%IsOutputStep()) THEN
!   CALL obj%WritePointData(name="u", field=u)
! END IF
! CALL obj%EndStep()
! ! end of simulation
! CALL obj%DEALLOCATE()
!```

TYPE, EXTENDS(HDF5File_) :: VtkHDF5File_
  PRIVATE
  LOGICAL(LGT) :: isRequested = math%no
  !! output is requested (ImportFromToml)
  LOGICAL(LGT) :: isTransient = math%no
  !! write time steps (Steps group)
  LOGICAL(LGT) :: isLagrange = math%no
  !! the current mesh is a Lagrange grid (WriteMesh(fedof, geofedof))
  LOGICAL(LGT) :: isMeshWritten = math%no
  !! a mesh has been written
  LOGICAL(LGT) :: isStepOpen = math%no
  !! BeginStep has been called, EndStep is not called yet
  LOGICAL(LGT) :: isSkipped = math%no
  !! the current step is skipped (frequency)
  INTEGER(I4B) :: frequency = 1
  !! write every frequency steps
  INTEGER(I4B) :: stepCounter = 0
  !! number of calls of BeginStep
  INTEGER(I4B) :: nSteps = 0
  !! number of steps written in the file
  INTEGER(I4B) :: nPoints = 0, nCells = 0, nIds = 0
  !! size of the current mesh
  INTEGER(I4B) :: partOffset = 0, pointOffset = 0, cellOffset = 0, &
    connOffset = 0
  !! offsets of the current mesh in the datasets
  TYPE(String) :: vtkType
  !! name of vtk type
  INTEGER(I4B) :: nArrays = 0
  !! number of data arrays written in the file
  TYPE(String), ALLOCATABLE :: arrayNames(:)
  !! names of data arrays
  INTEGER(I4B), ALLOCATABLE :: arrayLoc(:)
  !! 1: PointData, 2: CellData, 3: FieldData
  INTEGER(I4B), ALLOCATABLE :: arrayOffset(:)
  !! offset (in tuples) of the latest data of arrays
  INTEGER(I4B), ALLOCATABLE :: arraySize(:, :)
  !! number of components and tuples of the latest data of arrays
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateVTKHDF5 => obj_InitiateVTKHDF5
  !! Open a VTKHDF file for writing or reading
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml => obj_ImportFromToml
  !! Import options from toml table [<table>.vtkhdf]
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Close the file and deallocate the data
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: IsActive => obj_IsActive
  !! Returns true if output is requested
  PROCEDURE, PUBLIC, PASS(obj) :: IsOutputStep => obj_IsOutputStep
  !! Returns true if data should be written in the current step
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalPoints => obj_GetTotalPoints
  !! Number of points of the current mesh
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCells => obj_GetTotalCells
  !! Number of cells of the current mesh
  PROCEDURE, PUBLIC, PASS(obj) :: WriteUnstructuredGrid => &
    obj_WriteUnstructuredGrid
  !! Write unstructured grid without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: BeginStep => obj_BeginStep
  !! Start a time step
  PROCEDURE, PUBLIC, PASS(obj) :: EndStep => obj_EndStep
  !! Finish a time step
  PROCEDURE, PASS(obj) :: WritePointData1 => obj_WritePointData1
  !! Write a nodal field as point data
  PROCEDURE, PASS(obj) :: WritePointData2 => obj_WritePointData2
  !! Write a rank-1 array as point data
  PROCEDURE, PASS(obj) :: WritePointData3 => obj_WritePointData3
  !! Write a rank-2 array as point data
  GENERIC, PUBLIC :: WritePointData => WritePointData1, &
    WritePointData2, WritePointData3
  PROCEDURE, PASS(obj) :: WriteCellData1 => obj_WriteCellData1
  !! Write a rank-1 array as cell data
  PROCEDURE, PASS(obj) :: WriteCellData2 => obj_WriteCellData2
  !! Write a rank-2 array as cell data
  GENERIC, PUBLIC :: WriteCellData => WriteCellData1, WriteCellData2
  PROCEDURE, PUBLIC, PASS(obj) :: WriteFieldData => obj_WriteFieldData
  !! Write a rank-1 array as field data
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalSteps => obj_GetTotalSteps
  !! Number of steps in the file
  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeValues => obj_GetTimeValues
  !! Time values of steps
  PROCEDURE, PUBLIC, PASS(obj) :: ReadMesh => obj_ReadMesh
  !! Read the grid of a step
  PROCEDURE, PUBLIC, PASS(obj) :: ReadPointData => obj_ReadPointData
  !! Read point data of a step
  PROCEDURE, PUBLIC, PASS(obj) :: ReadCellData => obj_ReadCellData
  !! Read cell data of a step
END TYPE VtkHDF5File_

!----------------------------------------------------------------------------
!                                                         VtkHDF5FilePointer_
!----------------------------------------------------------------------------

TYPE :: VtkHDF5FilePointer_
  CLASS(VtkHDF5File_), POINTER :: ptr => NULL()
END TYPE VtkHDF5FilePointer_

!----------------------------------------------------------------------------
!                                      InitiateVTKHDF5@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-26: only modes NEW and READ; vtkType is checked by
!     CheckVtkTypeSupport; zlibOpt is passed to HDF5File_%Initiate
! summary: Open a VTKHDF file for writing or reading
!
! The object is deallocated first, then the HDF5 file is initiated and
! opened (see [[HDF5File_:Initiate]]).
!
! - Mode "NEW" creates a new file (an existing file is truncated), and
!   writes the root group /VTKHDF with the attributes Version = [2, 2]
!   and Type = vtkType.
! - Mode "READ" opens an existing file and reads the Type attribute
!   into vtkType. In a transient file (isTransient = .TRUE.), the group
!   /VTKHDF/Steps must exist.
! - Modes "WRITE" and "OVERWRITE" are not supported, because the state
!   of the steps and data arrays is not read from an existing file.
!
! Only vtkType = "UnstructuredGrid" is supported. In DEBUG mode, the
! vtkType argument and the Type attribute of a file opened in mode READ
! are checked, as well as the mode and the groups of the file.
!
! The default values of the optional arguments are isTransient =
! .FALSE. and frequency = 1. The layout and compression of the datasets
! are set by zlibOpt, chunkPolicy, libverLow and compactBytes, as in
! [[HDF5File_:Initiate]]; zlibOpt replaces chunkPolicy%deflateLevel.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!   filename : CHARACTER(*) - name of file (.vtkhdf or .hdf)
!   mode : CHARACTER(*) - "NEW" or "READ"
!   vtkType : CHARACTER(*) - VTK data type, "UnstructuredGrid"
!   zlibOpt : INTEGER(I4B) - (optional) gzip compression level 0-9,
!     -1 for no compression
!   chunkPolicy : TYPE(HDF5ChunkPolicy_) - (optional) layout and
!     compression of datasets
!   libverLow : INTEGER(I4B) - (optional) lower bound of the file
!     format version of a new file
!   compactBytes : INTEGER(INT64) - (optional) limit of the compact
!     layout in bytes
!   isTransient : LOGICAL(LGT) - (optional) write time steps (Steps
!     group), default .FALSE.
!   frequency : INTEGER(I4B) - (optional) write every frequency steps,
!     default 1; values less than 1 are taken as 1
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
! CHARACTER(*), PARAMETER :: filename = "VtkHDF5File_test_1.vtkhdf"
! TYPE(VtkHDF5File_) :: obj
! LOGICAL :: isok
! CALL obj%InitiateVTKHDF5(filename=filename, mode="NEW", &
!                          vtkType="UnstructuredGrid")
! CALL OK(obj%IsInitiated(), "InitiateVTKHDF5 NEW: initiated")
! CALL OK(obj%PathExists("/VTKHDF"), "InitiateVTKHDF5 NEW: /VTKHDF")
! CALL OK(obj%IsOutputStep(), "IsOutputStep: NEW, not transient")
! CALL obj%DEALLOCATE()
! INQUIRE (FILE=filename, EXIST=isok)
! CALL OK(isok, "InitiateVTKHDF5 NEW: file exists")
! CALL obj%InitiateVTKHDF5(filename=filename, mode="READ", &
!                          vtkType="UnstructuredGrid")
! CALL OK(obj%IsInitiated(), "InitiateVTKHDF5 READ: initiated")
! CALL OK(.NOT. obj%IsOutputStep(), "IsOutputStep: READ")
! CALL OK(obj%GetTotalSteps() .EQ. 0, "InitiateVTKHDF5 READ: no steps")
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "Deallocate")
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate]], [[obj_WriteMesh1]], [[obj_BeginStep]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateVTKHDF5(obj, filename, mode, vtkType, &
                                        zlibOpt, chunkPolicy, &
                                        libverLow, compactBytes, &
                                        isTransient, frequency)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(LEN=*), INTENT(IN) :: filename
    !! path of the file on the file system
    CHARACTER(LEN=*), INTENT(IN) :: mode
    !! access mode: 'NEW' or 'READ'
    !! 'NEW' creates the file and truncates an existing one
    !! 'READ' opens an existing file
    !! 'WRITE' and 'OVERWRITE' are not supported (DEBUG error)
    CHARACTER(LEN=*), INTENT(IN) :: vtkType
    !! VTK data type written in the Type attribute of /VTKHDF;
    !! only "UnstructuredGrid" is supported
    INTEGER(I4B), INTENT(IN), OPTIONAL :: zlibOpt
    !! gzip compression level [0-9], -1 for no compression;
    !! replaces chunkPolicy%deflateLevel
    TYPE(HDF5ChunkPolicy_), INTENT(IN), OPTIONAL :: chunkPolicy
    !! layout and compression settings; default values if absent
    INTEGER(I4B), INTENT(IN), OPTIONAL :: libverLow
    !! lower bound of the file format version of a new file:
    !! 0 (library default), 18, 110 (default), 112, 114 or 200
    INTEGER(INT64), INTENT(IN), OPTIONAL :: compactBytes
    !! datasets with less data use the compact layout; default 4096
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTransient
    !! write time steps; default .FALSE.
    INTEGER(I4B), INTENT(IN), OPTIONAL :: frequency
    !! write every frequency steps; default 1
  END SUBROUTINE obj_InitiateVTKHDF5
END INTERFACE

!----------------------------------------------------------------------------
!                                       ImportFromToml@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Import VTKHDF output options from toml
!
! table is the parent table (e.g. kernel), and options are read from
! the subtable `vtkhdf`. If the subtable is not found, then output is
! not active.
!
!```toml
![kernel.vtkhdf]
!isActive = true
!filename = "./results/result.vtkhdf"
!isTransient = true
!frequency = 1
!zlib = -1
!```
!
! The file is not opened here. Call InitiateVTKHDF5() after this.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!   table : TYPE(toml_table) - parent table
!
! Examples
!
!```fortran
! PROGRAM main
! USE Test_Method, ONLY: OK
! USE tomlf, ONLY: toml_table
! USE tomlf, ONLY: toml_load
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
! TYPE(VtkHDF5File_) :: obj
! TYPE(toml_table), ALLOCATABLE :: table
! CALL toml_load(table, "VtkHDF5File_test.toml")
! CALL obj%ImportFromToml(table)
! CALL OK(obj%IsActive(), "ImportFromToml")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_InitiateVTKHDF5]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml(obj, table)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    TYPE(toml_table), INTENT(INOUT) :: table
    !! parent toml table
  END SUBROUTINE obj_ImportFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                           Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Close the file and deallocate the data
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!   Delete : LOGICAL(LGT) - (optional) delete the file
!
! See also: [[obj_InitiateVTKHDF5]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj, Delete)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: Delete
    !! delete the file
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                              Display@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Display the content of VtkHDF5File_
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!   msg : CHARACTER(*) - message
!   unitno : INTEGER(I4B) - (optional) unit number
!
! See also: [[obj_InitiateVTKHDF5]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: msg
    !! message
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                             IsActive@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Returns true if VTKHDF output is requested
!
! The default value is true. It is set by ImportFromToml.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!
! Returns:
!   ans : LOGICAL(LGT) - output is requested
!
! See also: [[obj_ImportFromToml]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_IsActive(obj) RESULT(ans)
    CLASS(VtkHDF5File_), INTENT(IN) :: obj
    !! VTKHDF file
    LOGICAL(LGT) :: ans
    !! output is requested
  END FUNCTION obj_IsActive
END INTERFACE

!----------------------------------------------------------------------------
!                                           IsOutputStep@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-26: false for a file which is not opened for writing
! summary: Returns true if data should be written now
!
! Returns false if the file is not initiated or not opened for writing
! (mode READ). For a non-transient file, true is returned otherwise.
! For a transient file, true is returned only between BeginStep and
! EndStep of a step which is not skipped by frequency. Use it to avoid
! the computation of output data in the skipped steps.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!
! Returns:
!   ans : LOGICAL(LGT) - data should be written
!
! See also: [[obj_InitiateVTKHDF5]], [[obj_BeginStep]] (both contain
! an example)

INTERFACE
  MODULE FUNCTION obj_IsOutputStep(obj) RESULT(ans)
    CLASS(VtkHDF5File_), INTENT(IN) :: obj
    !! VTKHDF file
    LOGICAL(LGT) :: ans
    !! data should be written
  END FUNCTION obj_IsOutputStep
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalPoints@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Number of points of the current mesh
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!
! Returns:
!   ans : INTEGER(I4B) - number of points (size of point data)
!
! See also: [[obj_GetTotalCells]], [[obj_WriteMesh1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalPoints(obj) RESULT(ans)
    CLASS(VtkHDF5File_), INTENT(IN) :: obj
    !! VTKHDF file
    INTEGER(I4B) :: ans
    !! number of points
  END FUNCTION obj_GetTotalPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalCells@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Number of cells of the current mesh
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!
! Returns:
!   ans : INTEGER(I4B) - number of cells (size of cell data)
!
! See also: [[obj_GetTotalPoints]], [[obj_WriteMesh1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalCells(obj) RESULT(ans)
    CLASS(VtkHDF5File_), INTENT(IN) :: obj
    !! VTKHDF file
    INTEGER(I4B) :: ans
    !! number of cells
  END FUNCTION obj_GetTotalCells
END INTERFACE

!----------------------------------------------------------------------------
!                                          WriteUnstructuredGrid@WriteMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write unstructured grid
!
! This method writes the unstructued grid
INTERFACE
  MODULE SUBROUTINE obj_WriteUnstructuredGrid(obj, points, connectivity, &
                                              offsets, types)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: points(:, :)
    !! nodal coordinates, it should have three columns
    INTEGER(I4B), INTENT(IN) :: connectivity(:)
    !! connectivity
    INTEGER(I4B), INTENT(IN) :: offsets(:)
    !! offsets
    INTEGER(INT8), INTENT(IN) :: types(:)
    !! types
  END SUBROUTINE obj_WriteUnstructuredGrid
END INTERFACE

!----------------------------------------------------------------------------
!                                                  BeginStep@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Start a time step
!
! The step counter is increased. If MOD(counter - 1, frequency) is not
! zero, the step is skipped, and all the write methods do nothing
! until EndStep. Otherwise, time is appended to Steps/Values.
!
! Nothing is done if the file is not initiated or not transient.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   time : REAL(DFP) - time value of the step
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
! CHARACTER(*), PARAMETER :: filename = "VtkHDF5File_test_3.vtkhdf"
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(VtkHDF5File_) :: obj
! REAL(DFP), ALLOCATABLE :: u(:), vals(:, :), times(:), xij(:, :), &
!                           points(:, :)
! INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:)
! INTEGER(INT8), ALLOCATABLE :: types(:)
! INTEGER(I4B) :: istep, nPoints, nrow, ncol
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL meshfile%Initiate(fileName="./meshdata/small_tri3_mesh.h5", &
!                        MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=2)
! nPoints = amesh%GetTotalNodes()
! ALLOCATE (u(nPoints), xij(3, nPoints))
! xij = 0.0_DFP
! CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)
! ! 4 steps, every 2 steps are written (steps 1 and 3)
! CALL obj%InitiateVTKHDF5(filename=filename, mode="NEW", &
!                          vtkType="UnstructuredGrid", &
!                          isTransient=math%yes, frequency=2)
! CALL obj%WriteMesh(amesh)
! DO istep = 1, 4
!   CALL obj%BeginStep(time=0.1_DFP * istep)
!   IF (obj%IsOutputStep()) THEN
!     u = REAL(istep, DFP)
!     CALL obj%WritePointData(name="u", vals=u)
!   END IF
!   CALL obj%EndStep()
! END DO
! CALL obj%DEALLOCATE()
! CALL obj%InitiateVTKHDF5(filename=filename, mode="READ", &
!                          vtkType="UnstructuredGrid")
! CALL OK(obj%GetTotalSteps() .EQ. 2, "GetTotalSteps")
! CALL obj%GetTimeValues(times)
! CALL OK(ABS(times(2) - 0.3_DFP) .LT. 1.0E-12_DFP, "GetTimeValues")
! CALL obj%ReadPointData(name="u", vals=vals, step=2)
! CALL OK(ALL(ABS(vals - 3.0_DFP) .LT. 1.0E-12_DFP), "ReadPointData step")
! CALL obj%ReadMesh(points=points, connectivity=conn, offsets=offsets, &
!                   types=types, step=2)
! CALL OK(MAXVAL(ABS(points - xij)) .LT. 1.0E-12_DFP, "ReadMesh step")
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_EndStep]], [[obj_IsOutputStep]]

INTERFACE
  MODULE SUBROUTINE obj_BeginStep(obj, time)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    REAL(DFP), INTENT(IN) :: time
    !! time value
  END SUBROUTINE obj_BeginStep
END INTERFACE

!----------------------------------------------------------------------------
!                                                    EndStep@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Finish a time step
!
! The offsets of the current mesh (PartOffsets, PointOffsets, etc.) and
! the offsets of all the data arrays are appended to the Steps group,
! and the NSteps attribute is updated. Therefore, the file can be read
! after each EndStep.
!
! The data arrays which are not written in the step refer to their
! latest data. A data array which is written for the first time has
! offset zero in the previous steps.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!
! See also: [[obj_BeginStep]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_EndStep(obj)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
  END SUBROUTINE obj_EndStep
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritePointData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write a nodal field as point data
!
! - Linear grid: the vertex DOFs of the field are written
!   ([[VTKGetVertexValues]]).
! - Lagrange grid: the field is evaluated at the points of the grid
!   ([[VTKGetLagrangeValues]]).
!
! ScalarField_ is written with 1 component and VectorField_ with 3
! components. In a transient file, it should be called between
! BeginStep and EndStep.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   field : CLASS(AbstractNodeField_) - ScalarField_ or VectorField_
!
! See also: [[obj_WriteMesh2]] (contains an example),
! [[obj_WritePointData2]]

INTERFACE
  MODULE SUBROUTINE obj_WritePointData1(obj, name, field)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    CLASS(AbstractNodeField_), INTENT(INOUT) :: field
    !! nodal field
  END SUBROUTINE obj_WritePointData1
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritePointData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write a rank-1 array as point data (one component)
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - values, SIZE(vals) = GetTotalPoints()
!
! See also: [[obj_BeginStep]] (contains an example),
! [[obj_WritePointData3]]

INTERFACE
  MODULE SUBROUTINE obj_WritePointData2(obj, name, vals)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), INTENT(IN) :: vals(:)
    !! values at points
  END SUBROUTINE obj_WritePointData2
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritePointData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write a rank-2 array as point data (multiple components)
!
! vals(i, j) is the i-th component at the j-th point.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - SIZE(vals) = (nComp, GetTotalPoints())
!
! See also: [[obj_WriteMesh1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_WritePointData3(obj, name, vals)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), INTENT(IN) :: vals(:, :)
    !! values at points, shape (nComp, nPoints)
  END SUBROUTINE obj_WritePointData3
END INTERFACE

!----------------------------------------------------------------------------
!                                              WriteCellData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write a rank-1 array as cell data (one component)
!
! The order of cells is the order of active elements of the mesh.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - values, SIZE(vals) = GetTotalCells()
!
! See also: [[obj_WriteMesh1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_WriteCellData1(obj, name, vals)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), INTENT(IN) :: vals(:)
    !! values at cells
  END SUBROUTINE obj_WriteCellData1
END INTERFACE

!----------------------------------------------------------------------------
!                                              WriteCellData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write a rank-2 array as cell data (multiple components)
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - SIZE(vals) = (nComp, GetTotalCells())
!
! See also: [[obj_WriteCellData1]]

INTERFACE
  MODULE SUBROUTINE obj_WriteCellData2(obj, name, vals)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), INTENT(IN) :: vals(:, :)
    !! values at cells, shape (nComp, nCells)
  END SUBROUTINE obj_WriteCellData2
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteFieldData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write a rank-1 array as field data (one tuple)
!
! Field data are global values which are not attached to points or
! cells (e.g., energy, number of iterations). vals is written as one
! tuple with SIZE(vals) components.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - values
!
! See also: [[obj_WritePointData2]]

INTERFACE
  MODULE SUBROUTINE obj_WriteFieldData(obj, name, vals)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), INTENT(IN) :: vals(:)
    !! values
  END SUBROUTINE obj_WriteFieldData
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalSteps@ReadMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Number of steps in the file
!
! Returns the NSteps attribute of the Steps group, or zero if the file
! is not transient.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - opened VTKHDF file
!
! Returns:
!   ans : INTEGER(I4B) - number of steps
!
! See also: [[obj_BeginStep]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_GetTotalSteps(obj) RESULT(ans)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    INTEGER(I4B) :: ans
    !! number of steps
  END FUNCTION obj_GetTotalSteps
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTimeValues@ReadMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Time values of all steps
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - opened VTKHDF file
!   vals : REAL(DFP) - time values (allocatable), SIZE(vals) = NSteps
!
! See also: [[obj_BeginStep]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetTimeValues(obj, vals)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: vals(:)
    !! time values
  END SUBROUTINE obj_GetTimeValues
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ReadMesh@ReadMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read the grid of a step
!
! The grid (part) used in the step is read. If step is not present or
! the file is not transient, the first part is read.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - opened VTKHDF file
!   points : REAL(DFP) - SIZE(points) = (3, nPoints)
!   connectivity : INTEGER(I4B) - zero based point ids
!   offsets : INTEGER(I4B) - SIZE(offsets) = nCells + 1
!   types : INTEGER(INT8) - VTK cell types, SIZE(types) = nCells
!   step : INTEGER(I4B) - (optional) step number, 1 to NSteps
!
! See also: [[obj_WriteMesh1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_ReadMesh(obj, points, connectivity, offsets, &
                                 types, step)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: points(:, :)
    !! coordinates of points, shape (3, nPoints)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: connectivity(:)
    !! zero based point ids
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: offsets(:)
    !! offsets of cells, size nCells + 1
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: types(:)
    !! VTK cell types
    INTEGER(I4B), INTENT(IN), OPTIONAL :: step
    !! step number (1 to NSteps)
  END SUBROUTINE obj_ReadMesh
END INTERFACE

!----------------------------------------------------------------------------
!                                               ReadPointData@ReadMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read point data of a step
!
! vals(i, j) is the i-th component at the j-th point. A one component
! array gives SIZE(vals, 1) = 1. If step is not present or the file is
! not transient, the first data is read.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - opened VTKHDF file
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - values (allocatable), (nComp, nPoints)
!   step : INTEGER(I4B) - (optional) step number, 1 to NSteps
!
! See also: [[obj_BeginStep]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_ReadPointData(obj, name, vals, step)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! values, shape (nComp, nPoints)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: step
    !! step number (1 to NSteps)
  END SUBROUTINE obj_ReadPointData
END INTERFACE

!----------------------------------------------------------------------------
!                                                ReadCellData@ReadMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read cell data of a step
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - opened VTKHDF file
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - values (allocatable), (nComp, nCells)
!   step : INTEGER(I4B) - (optional) step number, 1 to NSteps
!
! See also: [[obj_ReadPointData]]

INTERFACE
  MODULE SUBROUTINE obj_ReadCellData(obj, name, vals, step)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! values, shape (nComp, nCells)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: step
    !! step number (1 to NSteps)
  END SUBROUTINE obj_ReadCellData
END INTERFACE

END MODULE VtkHDF5File_Class
