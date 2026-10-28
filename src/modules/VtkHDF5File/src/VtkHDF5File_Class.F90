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

!> author: Shion Shimizu and Vikas Sharma
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
USE BaseType, ONLY: math => TypeMathOpt
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
!                                                              VtkDataArray_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Record of a data array written in a VTKHDF file
!
! A data array is identified by its location loc and its name. The
! record keeps the offset and the size of the latest data of the array
! (set by [[VtkHDF5File_:RegisterArray]]) and the number of steps
! written in Steps/<loc>DataOffsets/<name> (set by
! [[VtkHDF5File_:EndTimeStep]]), so that EndTimeStep does not have to read the
! extent of the datasets.

TYPE :: VtkDataArray_
  TYPE(String) :: name
  !! name of data array
  INTEGER(I4B) :: loc = 0
  !! location: 1: PointData, 2: CellData, 3: FieldData
  INTEGER(I4B) :: offset = 0
  !! offset (in tuples) of the latest data in the dataset
  INTEGER(I4B) :: ncomp = 0
  !! number of components of the latest data
  INTEGER(I4B) :: ntuples = 0
  !! number of tuples of the latest data
  INTEGER(I4B) :: nStepsWritten = 0
  !! number of steps written in the Steps group for this array
END TYPE VtkDataArray_

!----------------------------------------------------------------------------
!                                                        VtkReadDataArray_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Cached information of a data array for reading
!
! A data array is identified by its group ("PointData" or "CellData")
! and its name. The entry is made by the first ReadPointData or
! ReadCellData of the array: it keeps the rank of the dataset
! <group>/<name> and the offsets Steps/<group>Offsets/<name> of all the
! steps, so that the next reads do not open these datasets again.

TYPE :: VtkReadDataArray_
  TYPE(String) :: group
  !! "PointData" or "CellData"
  TYPE(String) :: name
  !! name of data array
  INTEGER(I4B) :: RANK = 0
  !! rank of the dataset <group>/<name> (1 or 2)
  LOGICAL(LGT) :: hasOffsets = .FALSE.
  !! Steps/<group>Offsets/<name> exists
  INTEGER(I4B), ALLOCATABLE :: offsets(:)
  !! offsets(k): data offset (in tuples) of step k, if hasOffsets
END TYPE VtkReadDataArray_

!----------------------------------------------------------------------------
!                                                            VtkReadCache_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Cache of the Steps group and of the parts for reading
!
! The read methods ([[VtkHDF5File_:GetTotalSteps]],
! [[VtkHDF5File_:GetTimeValues]], [[VtkHDF5File_:ReadMesh]],
! [[VtkHDF5File_:ReadPointData]], [[VtkHDF5File_:ReadCellData]]) need
! a few values of the Steps group and of the NumberOf* datasets in each
! call. These datasets are small (one value per step or per part), so
! they are read once, at the first read call, and kept here. A read of a
! step then reads only the slabs of the grid or of the data.
!
! The cache is reset (isSet is false) by WriteUnstructuredGrid and
! EndTimeStep, which change these datasets, and by Deallocate. It is
! loaded again by the next read call.

TYPE :: VtkReadCache_
  LOGICAL(LGT) :: isSet = .FALSE.
  !! the cache is loaded
  INTEGER(I4B) :: nSteps = 0
  !! NSteps attribute of the Steps group, 0 if there are no steps
  REAL(DFP), ALLOCATABLE :: times(:)
  !! Steps/Values, SIZE(times) = nSteps
  INTEGER(I4B), ALLOCATABLE :: partOffsets(:)
  !! Steps/PartOffsets, SIZE = nSteps
  INTEGER(I4B), ALLOCATABLE :: pointOffsets(:)
  !! Steps/PointOffsets, SIZE = nSteps
  INTEGER(I4B), ALLOCATABLE :: cellOffsets(:)
  !! Steps/CellOffsets, SIZE = nSteps
  INTEGER(I4B), ALLOCATABLE :: connOffsets(:)
  !! Steps/ConnectivityIdOffsets, SIZE = nSteps
  INTEGER(I4B), ALLOCATABLE :: nPoints(:)
  !! NumberOfPoints, SIZE = number of parts
  INTEGER(I4B), ALLOCATABLE :: nCells(:)
  !! NumberOfCells, SIZE = number of parts
  INTEGER(I4B), ALLOCATABLE :: nIds(:)
  !! NumberOfConnectivityIds, SIZE = number of parts
  INTEGER(I4B) :: nArrays = 0
  !! number of data arrays in the cache
  TYPE(VtkReadDataArray_), ALLOCATABLE :: arrays(:)
  !! data arrays read so far, arrays(1:nArrays) are used;
  !! SIZE(arrays) is the capacity (doubled when it is full)
END TYPE VtkReadCache_

!----------------------------------------------------------------------------
!                                                               VtkHDF5File_
!----------------------------------------------------------------------------

!> author: Shion Shimizu and Vikas Sharma
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
! A grid is written by WriteUnstructuredGrid. One grid is one part
! (piece). If the grid does not change, it is written only once and all
! the steps refer to it (the geometry offsets are repeated). If
! WriteUnstructuredGrid is called in a step, then the new grid is used
! from that step.
!
! The grid data (points, connectivity, offsets, VTK cell types) are
! made by the caller, for example by
! [[AbstractMesh_:GetVtkUnstructuredGridData]]. The point data are
! given as arrays, for example from GetVtkUnstructuredValue of a field.
!
! Usage in a time dependent simulation:
!
!```fortran
! CALL obj%ImportFromToml(table)              ! reads [<table>.vtkhdf]
! IF (obj%IsActive()) THEN
!   CALL obj%InitiateVTKHDF5(filename="result.vtkhdf", mode="NEW", &
!                            vtkType="UnstructuredGrid", &
!                            isTransient=math%yes)
!   CALL mesh%GetVtkUnstructuredGridData(points=points, &
!     connectivity=conn, offsets=offsets, types=types)
!   CALL obj%WriteUnstructuredGrid(points, conn, offsets, types)
! END IF
! ! every time step
! CALL obj%BeginTimeStep(time=t)
! IF (obj%IsOutputStep()) THEN
!   CALL u%GetVtkUnstructuredValue(VALUE=vals)
!   CALL obj%WritePointData(name="u", VALUE=vals)
! END IF
! CALL obj%EndTimeStep()
! ! end of simulation
! CALL obj%DEALLOCATE()
!```

TYPE, EXTENDS(HDF5File_) :: VtkHDF5File_
  PRIVATE
  LOGICAL(LGT) :: isRequested = math%no
  !! output is requested (ImportFromToml)
  LOGICAL(LGT) :: isTransient = math%no
  !! write time steps (Steps group)
  LOGICAL(LGT) :: isMeshWritten = math%no
  !! a mesh has been written
  LOGICAL(LGT) :: isStepOpen = math%no
  !! BeginTimeStep has been called, EndTimeStep is not called yet
  LOGICAL(LGT) :: isSkipped = math%no
  !! the current step is skipped (frequency)
  INTEGER(I4B) :: frequency = 1
  !! write every frequency steps
  INTEGER(I4B) :: stepCounter = 0
  !! number of calls of BeginTimeStep
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
  TYPE(VtkDataArray_), ALLOCATABLE :: arrays(:)
  !! data arrays written in the file, arrays(1:nArrays) are used;
  !! SIZE(arrays) is the capacity (doubled when it is full)
  TYPE(VtkReadCache_) :: readCache
  !! Steps group and parts, read once by the read methods
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
  !! Write an unstructured grid as a new part
  PROCEDURE, PUBLIC, PASS(obj) :: BeginTimeStep => obj_BeginTimeStep
  !! Start a time step
  PROCEDURE, PUBLIC, PASS(obj) :: EndTimeStep => obj_EndTimeStep
  !! Finish a time step
  PROCEDURE, PASS(obj) :: WritePointData1 => obj_WritePointData1
  !! Write a rank-1 array as point data
  PROCEDURE, PASS(obj) :: WritePointData2 => obj_WritePointData2
  !! Write a rank-2 array as point data
  GENERIC, PUBLIC :: WritePointData => WritePointData1, &
    WritePointData2
  PROCEDURE, PRIVATE, PASS(obj) :: WriteCellData1 => obj_WriteCellData1
  !! Write a rank-1 array as cell data
  PROCEDURE, PRIVATE, PASS(obj) :: WriteCellData2 => obj_WriteCellData2
  !! Write a rank-2 array as cell data
  GENERIC, PUBLIC :: WriteCellData => WriteCellData1, WriteCellData2
  PROCEDURE, PUBLIC, PASS(obj) :: WriteFieldData => obj_WriteFieldData
  !! Write a rank-1 array as field data
  PROCEDURE, PRIVATE, PASS(obj) :: RegisterArray => obj_RegisterArray
  !! Add a data array (if new) and set its latest offset and size
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

!> author: Shion Shimizu and Vikas Sharma
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
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
! CHARACTER(*), PARAMETER :: filename = "InitiateVTKHDF5_test_1.vtkhdf"
! TYPE(VtkHDF5File_) :: obj
! LOGICAL :: isok
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
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
! See also: [[obj_Deallocate]], [[obj_WriteUnstructuredGrid]],
! [[obj_BeginTimeStep]]

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

!> author: Shion Shimizu and Vikas Sharma
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
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE Test_Method, ONLY: OK
! USE tomlf, ONLY: toml_table
! USE tomlf, ONLY: toml_load
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
! TYPE(VtkHDF5File_) :: obj
! TYPE(toml_table), ALLOCATABLE :: table
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
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
! For a transient file, true is returned only between BeginTimeStep and
! EndTimeStep of a step which is not skipped by frequency. Use it to avoid
! the computation of output data in the skipped steps.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!
! Returns:
!   ans : LOGICAL(LGT) - data should be written
!
! See also: [[obj_InitiateVTKHDF5]], [[obj_BeginTimeStep]] (both contain
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
! See also: [[obj_GetTotalCells]], [[obj_WriteUnstructuredGrid]]

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
! See also: [[obj_GetTotalPoints]], [[obj_WriteUnstructuredGrid]]

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
! update:
!   - 2026-10-28: debug checks of the grid data
! summary: Write an unstructured grid as a new part
!
! The grid is appended as a new part (piece) to the datasets Points,
! Connectivity, Offsets and Types of /VTKHDF, and its sizes are
! appended to NumberOfPoints, NumberOfCells and NumberOfConnectivityIds.
! The offsets of the new part are stored in obj; point and cell data
! written after this refer to this grid.
!
! The grid data can be obtained, for example, from
! [[AbstractMesh_:GetVtkUnstructuredGridData]] (linear grid of a mesh)
! or [[AbstractNodeField_:GetVtkUnstructuredGridData]] (Lagrange grid
! of a field).
!
! If it is called before the first BeginTimeStep (or in a non-transient
! file), the grid is used for all steps. If it is called in a step
! (moving or adaptive mesh), the grid is used from the current step;
! it is written also in a step which is skipped by frequency, and it is
! then used from the next output step.
!
! In the debug version it is checked that the file is initiated for
! writing, SIZE(points, 1) = 3, SIZE(offsets) = SIZE(types) + 1,
! offsets(1) = 0, offsets is non decreasing, offsets(nCells + 1) =
! SIZE(connectivity), and the connectivity ids are in [0, nPoints - 1].
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   points : REAL(DFP) - coordinates, SIZE(points) = (3, nPoints)
!   connectivity : INTEGER(I4B) - zero based point ids of all cells,
!     SIZE(connectivity) = offsets(nCells + 1)
!   offsets : INTEGER(I4B) - SIZE(offsets) = nCells + 1, offsets(1) = 0
!   types : INTEGER(INT8) - VTK cell types, SIZE(types) = nCells
!
! Formula:
!
! The point ids of cell \(i\) are
!
! $$ \text{connectivity}(\text{offsets}(i)+1:\text{offsets}(i+1)) $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: meshname = &
!   "./meshdata/very_small_quad4_mesh.h5"
! CHARACTER(*), PARAMETER :: filename = &
!   "WriteUnstructuredGrid_test_1.vtkhdf"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(HDF5File_) :: meshfile, h5
! TYPE(FEMesh_) :: amesh
! TYPE(VtkHDF5File_) :: obj
! REAL(DFP), ALLOCATABLE :: points(:, :), cellIds(:), rvals1(:), &
!   rvals2(:, :)
! INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:), ivals(:)
! INTEGER(INT8), ALLOCATABLE :: types(:), tvals(:)
! INTEGER(I4B) :: nPoints, nCells, nIds, ii
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(filename=meshname, mode="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, DIM=2)
!
! ! grid of the mesh
! CALL amesh%GetVtkUnstructuredGridData(points=points, &
!                                       connectivity=conn, &
!                                       offsets=offsets, types=types)
! nPoints = SIZE(points, 2)
! nCells = SIZE(types)
! nIds = SIZE(conn)
! cellIds = [(REAL(ii, DFP), ii=1, nCells)]
!
! ! write a static grid with point, cell and field data
! CALL obj%InitiateVTKHDF5(filename=filename, mode="NEW", &
!                          vtkType="UnstructuredGrid")
! CALL obj%WriteUnstructuredGrid(points=points, connectivity=conn, &
!                                offsets=offsets, types=types)
! CALL OK(obj%GetTotalPoints() .EQ. nPoints, "GetTotalPoints")
! CALL OK(obj%GetTotalCells() .EQ. nCells, "GetTotalCells")
! CALL OK(obj%IsOutputStep(), "IsOutputStep: not transient")
! CALL obj%WritePointData(name="x", VALUE=points)
! CALL obj%WritePointData(name="xcoord", VALUE=points(1, :))
! CALL obj%WriteCellData(name="id", vals=cellIds)
! CALL obj%WriteFieldData(name="energy", vals=[1.5_DFP, 2.5_DFP])
! CALL obj%DEALLOCATE()
!
! ! read the datasets back
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
!
! CALL OK(.NOT. h5%PathExists("/VTKHDF/Steps"), "no Steps group")
!
! CALL h5%ReadSlab("/VTKHDF/NumberOfPoints", 0, 1, ivals)
! CALL OK(ivals(1) .EQ. nPoints, "NumberOfPoints")
! CALL h5%ReadSlab("/VTKHDF/NumberOfCells", 0, 1, ivals)
! CALL OK(ivals(1) .EQ. nCells, "NumberOfCells")
! CALL h5%ReadSlab("/VTKHDF/NumberOfConnectivityIds", 0, 1, ivals)
! CALL OK(ivals(1) .EQ. nIds, "NumberOfConnectivityIds")
!
! CALL h5%ReadSlab("/VTKHDF/Points", 0, nPoints, rvals2)
! CALL OK(ALL(SHAPE(rvals2) .EQ. [3, nPoints]), "Points: shape")
! CALL OK(MAXVAL(ABS(rvals2 - points)) .LT. tol, "Points: values")
! CALL h5%ReadSlab("/VTKHDF/Connectivity", 0, nIds, ivals)
! CALL OK(ALL(ivals .EQ. conn), "Connectivity")
! CALL h5%ReadSlab("/VTKHDF/Offsets", 0, nCells + 1, ivals)
! CALL OK(ALL(ivals .EQ. offsets), "Offsets")
! CALL h5%ReadSlab("/VTKHDF/Types", 0, nCells, tvals)
! CALL OK(ALL(tvals .EQ. 9_INT8), "Types: VTK_QUAD")
!
! CALL h5%ReadSlab("/VTKHDF/PointData/x", 0, nPoints, rvals2)
! CALL OK(MAXVAL(ABS(rvals2 - points)) .LT. tol, "PointData rank-2")
! CALL h5%ReadSlab("/VTKHDF/PointData/xcoord", 0, nPoints, rvals1)
! CALL OK(MAXVAL(ABS(rvals1 - points(1, :))) .LT. tol, &
!         "PointData rank-1")
! CALL h5%ReadSlab("/VTKHDF/CellData/id", 0, nCells, rvals1)
! CALL OK(MAXVAL(ABS(rvals1 - cellIds)) .LT. tol, "CellData")
! CALL h5%ReadSlab("/VTKHDF/FieldData/energy", 0, 1, rvals2)
! CALL OK(ALL(SHAPE(rvals2) .EQ. [2, 1]), "FieldData: shape")
! CALL OK(MAXVAL(ABS(rvals2(:, 1) - [1.5_DFP, 2.5_DFP])) .LT. tol, &
!         "FieldData: values")
!
! CALL h5%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_WritePointData1]], [[obj_WriteCellData1]],
! [[obj_WriteFieldData]], [[obj_BeginTimeStep]]

INTERFACE
  MODULE SUBROUTINE obj_WriteUnstructuredGrid(obj, points, connectivity, &
                                              offsets, types)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    REAL(DFP), INTENT(IN) :: points(:, :)
    !! coordinates of points, shape (3, nPoints)
    INTEGER(I4B), INTENT(IN) :: connectivity(:)
    !! zero based point ids of all cells
    INTEGER(I4B), INTENT(IN) :: offsets(:)
    !! offsets of cells in connectivity, size nCells + 1
    INTEGER(INT8), INTENT(IN) :: types(:)
    !! VTK cell types, size nCells
  END SUBROUTINE obj_WriteUnstructuredGrid
END INTERFACE

!----------------------------------------------------------------------------
!                                                  BeginTimeStep@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-28: example uses WriteUnstructuredGrid
! summary: Start a time step
!
! The step counter is increased. If MOD(counter - 1, frequency) is not
! zero, the step is skipped, and all the write methods do nothing
! until EndTimeStep. Otherwise, time is appended to Steps/Values.
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
! USE BaseType, ONLY: TypeFileOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: meshname = "./meshdata/small_tri3_mesh.h5"
! CHARACTER(*), PARAMETER :: filename = "BeginTimeStep_test_1.vtkhdf"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(HDF5File_) :: meshfile, h5
! TYPE(FEMesh_) :: amesh
! TYPE(VtkHDF5File_) :: obj
! REAL(DFP), ALLOCATABLE :: points(:, :), u(:), rvals1(:)
! INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:), ivals(:), &
!   ivals2(:, :)
! INTEGER(INT8), ALLOCATABLE :: types(:)
! INTEGER(I4B) :: istep, nPoints, nsteps
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(filename=meshname, mode=TypeFileOpt%read)
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, DIM=2)
!
! CALL amesh%GetVtkUnstructuredGridData(points=points, &
!                                       connectivity=conn, &
!                                       offsets=offsets, types=types)
! nPoints = SIZE(points, 2)
! ALLOCATE (u(nPoints))
!
! ! 4 steps, every 2 steps are written (steps 1 and 3)
! CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
!                          vtkType="UnstructuredGrid", &
!                          isTransient=math%yes, frequency=2)
! CALL obj%WriteUnstructuredGrid(points=points, connectivity=conn, &
!                                offsets=offsets, types=types)
! DO istep = 1, 4
!   CALL obj%BeginTimeStep(time=0.1_DFP * istep)
!   CALL OK(obj%IsOutputStep() .EQV. (MOD(istep, 2) .EQ. 1), &
!           "IsOutputStep: step "//CHAR(48 + istep))
!   IF (obj%IsOutputStep()) THEN
!     u = REAL(istep, DFP)
!     CALL obj%WritePointData(NAME="u", VALUE=u)
!   END IF
!   CALL obj%EndTimeStep()
! END DO
! CALL obj%DEALLOCATE()
!
! ! read the Steps group back
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
!
! CALL h5%ReadAttribute("/VTKHDF/Steps", "NSteps", nsteps)
! CALL OK(nsteps .EQ. 2, "NSteps")
!
! CALL h5%ReadSlab("/VTKHDF/Steps/Values", 0, 2, rvals1)
! CALL OK(MAXVAL(ABS(rvals1 - [0.1_DFP, 0.3_DFP])) .LT. tol, &
!         "Steps/Values")
!
! ! the grid is written once, all steps refer to it
! CALL h5%ReadSlab("/VTKHDF/NumberOfPoints", 0, 1, ivals)
! CALL OK(SIZE(ivals) .EQ. 1 .AND. ivals(1) .EQ. nPoints, &
!         "one part")
! CALL h5%ReadSlab("/VTKHDF/Steps/PartOffsets", 0, 2, ivals)
! CALL OK(ALL(ivals .EQ. 0), "Steps/PartOffsets")
! CALL h5%ReadSlab("/VTKHDF/Steps/NumberOfParts", 0, 2, ivals)
! CALL OK(ALL(ivals .EQ. 1), "Steps/NumberOfParts")
! CALL h5%ReadSlab("/VTKHDF/Steps/PointOffsets", 0, 2, ivals)
! CALL OK(ALL(ivals .EQ. 0), "Steps/PointOffsets")
! CALL h5%ReadSlab("/VTKHDF/Steps/CellOffsets", 0, 2, ivals2)
! CALL OK(ALL(SHAPE(ivals2) .EQ. [1, 2]) .AND. ALL(ivals2 .EQ. 0), &
!         "Steps/CellOffsets")
!
! ! u is written in two steps, one after the other
! CALL h5%ReadSlab("/VTKHDF/Steps/PointDataOffsets/u", 0, 2, ivals)
! CALL OK(ALL(ivals .EQ. [0, nPoints]), "Steps/PointDataOffsets/u")
! CALL h5%ReadSlab("/VTKHDF/PointData/u", 0, 2 * nPoints, rvals1)
! CALL OK(MAXVAL(ABS(rvals1(1:nPoints) - 1.0_DFP)) .LT. tol, &
!         "PointData/u: step 1")
! CALL OK(MAXVAL(ABS(rvals1(nPoints + 1:) - 3.0_DFP)) .LT. tol, &
!         "PointData/u: step 3")
!
! CALL h5%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_EndTimeStep]], [[obj_IsOutputStep]]

INTERFACE
  MODULE SUBROUTINE obj_BeginTimeStep(obj, time)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    REAL(DFP), INTENT(IN) :: time
    !! time value
  END SUBROUTINE obj_BeginTimeStep
END INTERFACE

!----------------------------------------------------------------------------
!                                                    EndTimeStep@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Finish a time step
!
! The offsets of the current mesh (PartOffsets, PointOffsets, etc.) and
! the offsets of all the data arrays are appended to the Steps group,
! and the NSteps attribute is updated. Therefore, the file can be read
! after each EndTimeStep.
!
! The data arrays which are not written in the step refer to their
! latest data. A data array which is written for the first time has
! offset zero in the previous steps.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!
! See also: [[obj_BeginTimeStep]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_EndTimeStep(obj)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
  END SUBROUTINE obj_EndTimeStep
END INTERFACE

!----------------------------------------------------------------------------
!                                             WritePointData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-27: checks only in DEBUG mode
! summary: Write a rank-1 array as point data (one component)
!
! vals(j) is the value at the j-th point of the current grid. The data
! is appended to /VTKHDF/PointData/<name>.
!
! In a transient file, it should be called between BeginTimeStep and
! EndTimeStep; nothing is written in a step which is skipped by frequency.
! In DEBUG mode, an error is raised if the file is not opened with mode
! NEW, if no grid has been written (see [[obj_WriteUnstructuredGrid]]),
! if it is called outside a step of a transient file, or if the number
! of tuples is not GetTotalPoints().
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   VALUE : REAL(DFP) - values, SIZE(VALUE) = GetTotalPoints()
!
! See also: [[obj_BeginTimeStep]] (contains an example),
! [[obj_WritePointData2]]

INTERFACE
  MODULE SUBROUTINE obj_WritePointData1(obj, name, VALUE)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values at points
  END SUBROUTINE obj_WritePointData1
END INTERFACE

!----------------------------------------------------------------------------
!                                               WritePointData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-27: checks only in DEBUG mode
! summary: Write a rank-2 array as point data (multiple components)
!
! vals(i, j) is the i-th component at the j-th point of the current
! grid. The data is appended to /VTKHDF/PointData/<name>.
!
! In a transient file, it should be called between BeginTimeStep and
! EndTimeStep; nothing is written in a step which is skipped by frequency.
! In DEBUG mode, an error is raised if the file is not opened with mode
! NEW, if no grid has been written (see [[obj_WriteUnstructuredGrid]]),
! if it is called outside a step of a transient file, or if the number
! of tuples is not GetTotalPoints().
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   VALUE : REAL(DFP) - SIZE(VALUE) = (nComp, GetTotalPoints())
!
! See also: [[obj_WritePointData1]], [[obj_BeginTimeStep]] (contains an
! example)

INTERFACE
  MODULE SUBROUTINE obj_WritePointData2(obj, name, VALUE)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! values at points, shape (nComp, nPoints)
  END SUBROUTINE obj_WritePointData2
END INTERFACE

!----------------------------------------------------------------------------
!                                              WriteCellData@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-27: checks only in DEBUG mode
! summary: Write a rank-1 array as cell data (one component)
!
! vals(j) is the value at the j-th cell of the current grid (the order
! of cells in the grid). The data is appended to
! /VTKHDF/CellData/<name>.
!
! In a transient file, it should be called between BeginTimeStep and
! EndTimeStep; nothing is written in a step which is skipped by frequency.
! In DEBUG mode, an error is raised if the file is not opened with mode
! NEW, if no grid has been written (see [[obj_WriteUnstructuredGrid]]),
! if it is called outside a step of a transient file, or if the number
! of tuples is not GetTotalCells().
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - values, SIZE(vals) = GetTotalCells()
!
! See also: [[obj_WriteCellData2]], [[obj_WritePointData1]]

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
! update:
!   - 2026-10-27: checks only in DEBUG mode
! summary: Write a rank-2 array as cell data (multiple components)
!
! vals(i, j) is the i-th component at the j-th cell of the current
! grid. The data is appended to /VTKHDF/CellData/<name>.
!
! In a transient file, it should be called between BeginTimeStep and
! EndTimeStep; nothing is written in a step which is skipped by frequency.
! In DEBUG mode, an error is raised if the file is not opened with mode
! NEW, if no grid has been written (see [[obj_WriteUnstructuredGrid]]),
! if it is called outside a step of a transient file, or if the number
! of tuples is not GetTotalCells().
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file opened with mode "NEW"
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - SIZE(vals) = (nComp, GetTotalCells())
!
! See also: [[obj_WriteCellData1]], [[obj_WritePointData2]]

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
! update:
!   - 2026-10-27: checks only in DEBUG mode
! summary: Write a rank-1 array as field data (one tuple)
!
! Field data are global values which are not attached to points or
! cells (e.g., energy, number of iterations). vals is written as one
! tuple with SIZE(vals) components, appended to
! /VTKHDF/FieldData/<name>. The number of components may change from
! step to step; it is stored in Steps/FieldDataSizes/<name>.
!
! In a transient file, it should be called between BeginTimeStep and
! EndTimeStep; nothing is written in a step which is skipped by frequency.
! In DEBUG mode, an error is raised if the file is not opened with mode
! NEW, if no grid has been written (see [[obj_WriteUnstructuredGrid]]),
! or if it is called outside a step of a transient file.
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
!                                               RegisterArray@WriteMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-27: type-bound procedure (was a private subroutine);
!     records in VtkDataArray_, capacity doubling
! summary: Add a data array (if new) and set its latest offset and size
!
! The data arrays written in the file are recorded in obj%arrays(1:
! nArrays) (see VtkDataArray_). An array is identified by its location
! loc and its name; it is searched linearly.
!
! If the array (loc, name) is not recorded yet, a new entry is added.
! The capacity of obj%arrays starts at 8 and is doubled when it is
! full, so that adding n arrays costs O(n) copies. Then the offset and
! the size of the latest data of the array are set.
! EndTimeStep uses these entries to append the offsets of all data arrays
! to the Steps group (see [[obj_EndTimeStep]]).
!
! It is called by the write methods of point, cell and field data after
! the data is appended.
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - VTKHDF file
!   loc : INTEGER(I4B) - 1: PointData, 2: CellData, 3: FieldData
!   name : CHARACTER(*) - name of data array
!   offset : INTEGER(I4B) - offset (in tuples) of the latest data in
!     the dataset of the array
!   ncomp : INTEGER(I4B) - number of components of the latest data
!   ntuples : INTEGER(I4B) - number of tuples of the latest data
!
! See also: [[obj_WritePointData1]], [[obj_EndTimeStep]]

INTERFACE
  MODULE SUBROUTINE obj_RegisterArray(obj, loc, name, offset, ncomp, &
                                      ntuples)
    CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
    !! VTKHDF file
    INTEGER(I4B), INTENT(IN) :: loc
    !! location of the array: 1: PointData, 2: CellData, 3: FieldData
    CHARACTER(*), INTENT(IN) :: name
    !! name of data array
    INTEGER(I4B), INTENT(IN) :: offset
    !! offset (in tuples) of the latest data in the dataset
    INTEGER(I4B), INTENT(IN) :: ncomp
    !! number of components of the latest data
    INTEGER(I4B), INTENT(IN) :: ntuples
    !! number of tuples of the latest data
  END SUBROUTINE obj_RegisterArray
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalSteps@ReadMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-28: the Steps group and the part sizes are read once
!     and cached (see VtkReadCache_)
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
! See also: [[obj_GetTimeValues]] (contains an example),
! [[obj_BeginTimeStep]]

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
! update:
!   - 2026-10-28: the Steps group and the part sizes are read once
!     and cached (see VtkReadCache_)
! summary: Time values of all steps
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - opened VTKHDF file
!   vals : REAL(DFP) - time values (allocatable), SIZE(vals) = NSteps
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFileOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: file1 = "GetTimeValues_test_1a.vtkhdf", &
!                            file2 = "GetTimeValues_test_1b.vtkhdf"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(VtkHDF5File_) :: obj
! REAL(DFP), ALLOCATABLE :: times(:)
! REAL(DFP) :: points(3, 4)
! INTEGER(I4B) :: istep
! LOGICAL :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! ! one quadrangle
! points = RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                   1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP], &
!                  [3, 4])
!
! ! file which is not transient
! CALL obj%InitiateVTKHDF5(filename=file1, mode=TypeFileOpt%new, &
!                          vtkType="UnstructuredGrid")
! CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 3], [0, 4], [9_INT8])
! CALL obj%DEALLOCATE()
!
! CALL obj%InitiateVTKHDF5(filename=file1, mode=TypeFileOpt%READ, &
!                          vtkType="UnstructuredGrid")
! CALL OK(obj%GetTotalSteps() .EQ. 0, "GetTotalSteps: not transient")
! CALL obj%GetTimeValues(times)
! CALL OK(SIZE(times) .EQ. 0, "GetTimeValues: not transient")
! CALL obj%DEALLOCATE()
!
! ! transient file with three steps
! CALL obj%InitiateVTKHDF5(filename=file2, mode=TypeFileOpt%new, &
!                          vtkType="UnstructuredGrid", &
!                          isTransient=math%yes)
! CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 3], [0, 4], [9_INT8])
! isok = .TRUE.
! DO istep = 1, 3
!   CALL obj%BeginTimeStep(time=0.5_DFP * istep)
!   CALL obj%EndTimeStep()
!   isok = isok .AND. (obj%GetTotalSteps() .EQ. istep)
! END DO
! CALL OK(isok, "GetTotalSteps: while writing")
! CALL obj%DEALLOCATE()
!
! CALL obj%InitiateVTKHDF5(filename=file2, mode=TypeFileOpt%READ, &
!                          vtkType="UnstructuredGrid")
! CALL OK(obj%GetTotalSteps() .EQ. 3, "GetTotalSteps: transient")
! CALL obj%GetTimeValues(times)
! CALL OK(SIZE(times) .EQ. 3, "GetTimeValues: size")
! CALL OK(MAXVAL(ABS(times - [0.5_DFP, 1.0_DFP, 1.5_DFP])) .LT. tol, &
!         "GetTimeValues: values")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalSteps]], [[obj_BeginTimeStep]]

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
! update:
!   - 2026-10-28: the Steps group and the part sizes are read once
!     and cached (see VtkReadCache_)
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
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFileOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "ReadMesh_test_1.vtkhdf"
! TYPE(VtkHDF5File_) :: obj
! REAL(DFP) :: pointsA(3, 4), pointsB(3, 4)
! INTEGER(I4B), PARAMETER :: connA(4) = [0, 1, 2, 3], &
!                            offsetsA(2) = [0, 4], &
!                            connB(6) = [0, 1, 2, 0, 2, 3], &
!                            offsetsB(3) = [0, 3, 6]
! INTEGER(INT8), PARAMETER :: typesA(1) = [9_INT8], &
!                             typesB(2) = [5_INT8, 5_INT8]
! INTEGER(I4B) :: istep
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! pointsA = RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, &
!                    0.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                    1.0_DFP, 0.0_DFP], [3, 4])
! pointsB = 2.0_DFP * pointsA
!
! CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
!                          vtkType="UnstructuredGrid", &
!                          isTransient=math%yes)
! CALL obj%WriteUnstructuredGrid(pointsA, connA, offsetsA, typesA)
! DO istep = 1, 3
!   CALL obj%BeginTimeStep(time=REAL(istep, DFP))
!   IF (istep .EQ. 2) THEN
!     CALL obj%WriteUnstructuredGrid(pointsB, connB, offsetsB, typesB)
!   END IF
!   CALL obj%EndTimeStep()
! END DO
! CALL obj%DEALLOCATE()
!
! CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%READ, &
!                          vtkType="UnstructuredGrid")
! CALL CheckMesh(obj, pointsA, connA, offsetsA, typesA, "no step")
! CALL CheckMesh(obj, pointsA, connA, offsetsA, typesA, "step 1", 1)
! CALL CheckMesh(obj, pointsB, connB, offsetsB, typesB, "step 2", 2)
! CALL CheckMesh(obj, pointsB, connB, offsetsB, typesB, "step 3", 3)
! CALL obj%DEALLOCATE()
!
! CONTAINS
!
! SUBROUTINE CheckMesh(obj, points0, conn0, offsets0, types0, msg, step)
!   TYPE(VtkHDF5File_), INTENT(INOUT) :: obj
!   REAL(DFP), INTENT(IN) :: points0(:, :)
!   INTEGER(I4B), INTENT(IN) :: conn0(:), offsets0(:)
!   INTEGER(INT8), INTENT(IN) :: types0(:)
!   CHARACTER(*), INTENT(IN) :: msg
!   INTEGER(I4B), INTENT(IN), OPTIONAL :: step
!
!   REAL(DFP), ALLOCATABLE :: points(:, :)
!   INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:)
!   INTEGER(INT8), ALLOCATABLE :: types(:)
!   LOGICAL :: isok
!
!   CALL obj%ReadMesh(points, conn, offsets, types, step)
!
!   isok = ALL(SHAPE(points) .EQ. SHAPE(points0))
!   IF (isok) isok = MAXVAL(ABS(points - points0)) .LT. 1.0E-12_DFP
!   CALL OK(isok, "ReadMesh: points, "//msg)
!
!   isok = SIZE(conn) .EQ. SIZE(conn0)
!   IF (isok) isok = ALL(conn .EQ. conn0)
!   CALL OK(isok, "ReadMesh: connectivity, "//msg)
!
!   isok = SIZE(offsets) .EQ. SIZE(offsets0)
!   IF (isok) isok = ALL(offsets .EQ. offsets0)
!   CALL OK(isok, "ReadMesh: offsets, "//msg)
!
!   isok = SIZE(types) .EQ. SIZE(types0)
!   IF (isok) isok = ALL(types .EQ. types0)
!   CALL OK(isok, "ReadMesh: types, "//msg)
! END SUBROUTINE CheckMesh
!
! END PROGRAM main
!```
!
! See also: [[obj_WriteUnstructuredGrid]], [[obj_ReadPointData]]

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
! update:
!   - 2026-10-28: the Steps group and the part sizes are read once
!     and cached (see VtkReadCache_)
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
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFileOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "ReadPointData_test_1.vtkhdf"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(VtkHDF5File_) :: obj
! REAL(DFP) :: points(3, 4), u0(4), v0(2, 4)
! REAL(DFP), ALLOCATABLE :: vals(:, :)
! INTEGER(I4B) :: istep
! LOGICAL :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! points = RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                   1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP], &
!                  [3, 4])
! u0 = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
! v0(1, :) = u0
! v0(2, :) = -u0
!
! CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
!                          vtkType="UnstructuredGrid", &
!                          isTransient=math%yes)
! CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 3], [0, 4], [9_INT8])
! DO istep = 1, 3
!   CALL obj%BeginTimeStep(time=REAL(istep, DFP))
!   CALL obj%WritePointData(NAME="u", VALUE=istep * u0)
!   IF (istep .EQ. 1) CALL obj%WritePointData(NAME="v", VALUE=v0)
!   CALL obj%EndTimeStep()
! END DO
! CALL obj%DEALLOCATE()
!
! CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%READ, &
!                          vtkType="UnstructuredGrid")
!
! ! u in each step
! isok = .TRUE.
! DO istep = 1, 3
!   CALL obj%ReadPointData(NAME="u", vals=vals, step=istep)
!   isok = isok .AND. ALL(SHAPE(vals) .EQ. [1, 4])
!   IF (isok) isok = MAXVAL(ABS(vals(1, :) - istep * u0)) .LT. tol
! END DO
! CALL OK(isok, "ReadPointData: u in each step")
!
! ! u without step: first data
! CALL obj%ReadPointData(NAME="u", vals=vals)
! isok = ALL(SHAPE(vals) .EQ. [1, 4])
! IF (isok) isok = MAXVAL(ABS(vals(1, :) - u0)) .LT. tol
! CALL OK(isok, "ReadPointData: u without step")
!
! ! v is static: the same in all steps
! isok = .TRUE.
! DO istep = 1, 3
!   CALL obj%ReadPointData(NAME="v", vals=vals, step=istep)
!   isok = isok .AND. ALL(SHAPE(vals) .EQ. [2, 4])
!   IF (isok) isok = MAXVAL(ABS(vals - v0)) .LT. tol
! END DO
! CALL OK(isok, "ReadPointData: static v")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ReadCellData]], [[obj_ReadMesh]], [[obj_BeginTimeStep]]

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
! update:
!   - 2026-10-28: the Steps group and the part sizes are read once
!     and cached (see VtkReadCache_)
! summary: Read cell data of a step
!
! Arguments:
!   obj : CLASS(VtkHDF5File_) - opened VTKHDF file
!   name : CHARACTER(*) - name of data array
!   vals : REAL(DFP) - values (allocatable), (nComp, nCells)
!   step : INTEGER(I4B) - (optional) step number, 1 to NSteps
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFileOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: INT8
! USE Test_Method, ONLY: OK
! USE VtkHDF5File_Class, ONLY: VtkHDF5File_
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "ReadCellData_test_1.vtkhdf"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(VtkHDF5File_) :: obj
! REAL(DFP) :: points(3, 4)
! REAL(DFP), ALLOCATABLE :: vals(:, :)
! LOGICAL :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! points = RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                   1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP], &
!                  [3, 4])
!
! CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
!                          vtkType="UnstructuredGrid", &
!                          isTransient=math%yes)
! ! step 1: grid A
! CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 3], [0, 4], [9_INT8])
! CALL obj%BeginTimeStep(time=1.0_DFP)
! CALL obj%WriteCellData(NAME="c", vals=[10.0_DFP])
! CALL obj%EndTimeStep()
! ! step 2: grid B
! CALL obj%BeginTimeStep(time=2.0_DFP)
! CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 0, 2, 3], [0, 3, 6], &
!                                [5_INT8, 5_INT8])
! CALL obj%WriteCellData(NAME="c", vals=[20.0_DFP, 21.0_DFP])
! CALL obj%EndTimeStep()
! CALL obj%DEALLOCATE()
!
! CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%READ, &
!                          vtkType="UnstructuredGrid")
!
! CALL obj%ReadCellData(NAME="c", vals=vals, step=1)
! isok = ALL(SHAPE(vals) .EQ. [1, 1])
! IF (isok) isok = ABS(vals(1, 1) - 10.0_DFP) .LT. tol
! CALL OK(isok, "ReadCellData: step 1")
!
! CALL obj%ReadCellData(NAME="c", vals=vals, step=2)
! isok = ALL(SHAPE(vals) .EQ. [1, 2])
! IF (isok) isok = MAXVAL(ABS(vals(1, :) - [20.0_DFP, 21.0_DFP])) .LT. tol
! CALL OK(isok, "ReadCellData: step 2")
!
! CALL obj%ReadCellData(NAME="c", vals=vals)
! isok = ALL(SHAPE(vals) .EQ. [1, 1])
! IF (isok) isok = ABS(vals(1, 1) - 10.0_DFP) .LT. tol
! CALL OK(isok, "ReadCellData: without step")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ReadPointData]], [[obj_ReadMesh]]

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
