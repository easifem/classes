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

SUBMODULE(VectorField_Class) VTKMethods
USE String_Class, ONLY: String
USE VTKFile_Class, ONLY: VTK_BINARY_APPENDED
USE VTKFile_Class, ONLY: VTK_UNSTRUCTUREDGRID
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
USE GlobalData, ONLY: INT8
USE AbstractFE_Class, ONLY: AbstractFE_
USE ReferenceElement_Method, ONLY: GetVtkElementTypeFromTopology_
USE FEDOF_Class, ONLY: FEDOF_
USE Display_Method, ONLY: Display
USE BaseType, ONLY: TypeInterpolationOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "VectorField_Class@VTKMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
#endif

INTEGER(I4B) :: tsize, value_i, value_j
REAL(DFP), ALLOCATABLE :: VALUE(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%fedof%GetTotalDOF()
CALL Reallocate(VALUE, tsize, 3)

CALL obj%Get(VALUE=VALUE, nrow=value_i, ncol=value_j, &
             storageFMT=obj%GetStorageFMT())

CALL vtk%WriteDataArray(name=obj%name, x=VALUE(1:value_i, 1), &
                        y=VALUE(1:value_i, 2), z=VALUE(1:value_i, 3))

IF (ALLOCATED(VALUE)) DEALLOCATE (VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                               ExportHighOrderNodeCoordToVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportHigherOrderNodeCoordToVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExportHigherOrderNodeCoordToVTK()"
#endif

LOGICAL(LGT) :: opentag_, closetag_, content_, isok
INTEGER(I4B) :: ncells(1), npoints(1), cellOrder(3)
INTEGER(I4B) :: iel, ii, jj, maxNNS, topoType
INTEGER(INT8), ALLOCATABLE :: types(:)
INTEGER(I4B), ALLOCATABLE :: vtkindx(:), connectivity(:), &
                             offsets(:), cellcon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :)
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(FEDOF_), POINTER :: fedof
CLASS(AbstractFE_), POINTER :: feptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mesh => NULL()
fedof => NULL()
feptr => NULL()

IF (.NOT. vtk%isInitiated) THEN

#ifdef DEBUG_VER
  isok = PRESENT(filename)
  CALL AssertError1(isok, myname, &
                    "VTKFile_ is not initiated, and filename is not present.")
#endif

  CALL vtk%InitiateVTKFile( &
    filename=filename, mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
    DataStructureType=VTK_UNSTRUCTUREDGRID)
END IF

fedof => obj%fedof
mesh => fedof%GetMeshPointer()

ncells(1) = mesh%GetTotalElements()
npoints = obj%GetTotalDOF(math%one_i)

opentag_ = Input(default=math%yes, option=OpenTag)
closetag_ = Input(default=math%yes, option=CloseTag)
content_ = Input(default=math%yes, option=Content)

! Write piece information if OpenTag is true
IF (opentag_) THEN
  CALL vtk%WritePiece(nPoints=npoints(1), nCells=ncells(1))
END IF

CALL Reallocate(xij, 3, npoints(1))
CALL obj%Get(VALUE=xij, nrow=ii, ncol=jj, &
             storageFMT=TypeFieldOpt%storageFormatNodes, &
             force3D=math%yes)
CALL vtk%WritePoints(x=xij)

! Write Cells
IF (Content_) THEN

  maxNNS = fedof%GetMaxTotalConnectivity()

#ifdef DEBUG_VER
  isok = maxNNS .GT. 0
  CALL AssertError1(isok, myname, &
                    "maxNNS should be greater than zero")
#endif

  ALLOCATE (types(ncells(1)), offsets(0:ncells(1)), &
            connectivity(maxNNS * ncells(1)), cellcon(maxNNS), &
            vtkindx(maxNNS))

  offsets(0) = 0

  cell_loop: DO iel = 1, ncells(1)
    isok = mesh%isElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE cell_loop

    CALL fedof%GetCellOrder(ans=cellOrder, tsize=jj, globalElement=iel, &
                            islocal=math%yes)
    cellOrder(2:3) = cellOrder(1)

    topoType = mesh%GetElemTopology(globalElement=iel, islocal=math%yes)

    CALL GetVtkElementTypeFromTopology_(topoType=topoType, &
                                        order=cellOrder(1), &
                                        vtk_type=types(iel), &
                                        nptrs=vtkindx, &
                                        tsize=ii)

    offsets(iel) = offsets(iel - 1) + ii

    CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
    feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    ! Get local connectivity
    CALL fedof%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                                ans=cellcon, tsize=jj, opt="A")

    ! Fix the orientation of cellcon
    CALL feptr%FixOrientOfNptrs(nptrs=cellcon, tsize=jj)

#ifdef DEBUG_VER
    CALL AssertError2(ii, jj, myName, &
                      "a=size of vtkindx, b=size of cellcon, ")
#endif

    DO ii = 1, jj
      connectivity(offsets(iel) - jj + ii) = cellcon(vtkindx(ii)) - 1
    END DO

  END DO cell_loop

  ii = offsets(ncells(1))

  CALL vtk%WriteCells(connectivity=connectivity(1:ii), &
                      offsets=offsets(1:ncells(1)), &
                      types=types(1:ncells(1)))

END IF

IF (CloseTag_) THEN
  CALL vtk%WritePiece()
  CALL vtk%CLOSE()
END IF

NULLIFY (mesh, fedof)

IF (ALLOCATED(types)) DEALLOCATE (types)
IF (ALLOCATED(vtkindx)) DEALLOCATE (vtkindx)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(offsets)) DEALLOCATE (offsets)
IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
IF (ALLOCATED(xij)) DEALLOCATE (xij)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ExportHigherOrderNodeCoordToVTK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE VTKMethods
