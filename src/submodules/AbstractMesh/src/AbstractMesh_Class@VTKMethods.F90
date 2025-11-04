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

SUBMODULE(AbstractMesh_Class) VTKMethods
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input
USE ReferenceElement_Method, ONLY: GetVTKelementType_
USE GlobalData, ONLY: INT8
USE VTKFile_Class, ONLY: VTK_BINARY_APPENDED, VTK_UNSTRUCTUREDGRID

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                ExportToVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
#endif

LOGICAL(LGT) :: opentag_, closetag_, content_, isok, isNodeCoord
INTEGER(INT8), ALLOCATABLE :: types(:)
INTEGER(I4B) :: ncells, npoints, ii, jj, nne, maxnne, elemtype, tsize
INTEGER(I4B), ALLOCATABLE :: vtkindx(:), connectivity(:), &
                             offsets(:), cellcon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main
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

ncells = obj%GetTotalElements()
npoints = obj%GetTotalNodes()

opentag_ = Input(default=.TRUE., option=OpenTag)
closetag_ = Input(default=.TRUE., option=CloseTag)
content_ = Input(default=.TRUE., option=Content)

! Write piece information if OpenTag is true
IF (opentag_) CALL vtk%WritePiece(nPoints=npoints, nCells=ncells)

!  Write Points information
isNodeCoord = PRESENT(nodeCoord)
IF (isNodeCoord) THEN

#ifdef DEBUG_VER
  isok = (SIZE(nodecoord, 1) .EQ. 3) .AND. (SIZE(nodecoord, 2) .EQ. npoints)
  CALL AssertError1(isok, myname, &
                    "Shape of nodeCoord should be [3, nPoints]")
#endif

  CALL vtk%WritePoints(x=nodeCoord)

ELSE

  CALL Reallocate(xij, 3, npoints)
  CALL obj%GetNodeCoord(nodecoord=xij, nrow=ii, ncol=jj)
  CALL vtk%WritePoints(x=xij)

END IF

! Write Cells
IF (Content_) THEN

  maxnne = obj%GetMaxNNE()

#ifdef DEBUG_VER
  isok = maxnne .GT. 0
  CALL AssertError1(isok, myname, &
                    "maxnne should be greater than zero")
#endif

  ALLOCATE (types(ncells), offsets(0:ncells), connectivity(maxnne * ncells), &
            cellcon(maxnne), vtkindx(maxnne))

  offsets(0) = 0

  DO ii = 1, ncells
    isok = obj%isElementActive(globalElement=ii, islocal=.TRUE.)
    IF (.NOT. isok) CYCLE

    elemtype = obj%GetElemType(globalElement=ii, islocal=.TRUE.)

    CALL GetVTKelementType_(elemType=elemtype, vtk_type=types(ii), &
                            nptrs=vtkindx, tsize=nne)

    offsets(ii) = offsets(ii - 1) + nne

    CALL obj%GetConnectivity_(globalElement=ii, islocal=.TRUE., &
                              ans=cellcon, tsize=tsize)

#ifdef DEBUG_VER
    isok = tsize .EQ. nne
    CALL AssertError1(isok, myname, &
                      "tsize should be equal to nne")
#endif

    CALL obj%GetLocalNodeNumber_(globalNode=cellcon(1:tsize), &
                                 ans=cellcon(1:tsize), islocal=.FALSE.)

    DO jj = 1, nne
      connectivity(offsets(ii) - nne + jj) = cellcon(vtkindx(jj)) - 1
    END DO

  END DO

  tsize = offsets(ncells)

  CALL vtk%WriteCells(connectivity=connectivity(1:tsize), &
                      offsets=offsets(1:ncells), &
                      types=types(1:ncells))

END IF

IF (CloseTag_) THEN
  CALL vtk%WritePiece()
  CALL vtk%CLOSE()
END IF

! clean up
IF (ALLOCATED(types)) DEALLOCATE (types)
IF (ALLOCATED(vtkindx)) DEALLOCATE (vtkindx)
IF (ALLOCATED(connectivity)) DEALLOCATE (connectivity)
IF (ALLOCATED(offsets)) DEALLOCATE (offsets)
IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                                  WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk()"
#endif

TYPE(VTKFile_) :: vtk

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL vtk%InitiateVTKFile( &
  filename=filename, mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
  DataStructureType=VTK_UNSTRUCTUREDGRID)

CALL obj%ExportToVTK(vtk=vtk)

CALL vtk%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteData_vtk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE VTKMethods
