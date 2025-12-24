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

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
#endif

INTEGER(I4B) :: tsize, tnodes, nrow, ncol
REAL(DFP), ALLOCATABLE :: VALUE(:, :)
TYPE(String) :: name
CHARACTER(1), ALLOCATABLE :: dofnames(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalPhysicalVars()
ALLOCATE (dofnames(tsize))
CALL obj%GetPhysicalNames(dofnames)

ncol = obj%spaceCompo
tsize = obj%fedof%GetTotalDOF()
tnodes = obj%fedof%GetTotalVertexDOF()

ALLOCATE (VALUE(tsize, 3))
CALL obj%Get(VALUE=VALUE, nrow=nrow, ncol=ncol, storageFMT=MYSTORAGEFORMAT)

VALUE(:, 3) = 0.0_DFP
! name = obj%name%chars()//"_"//dofnames(1)
name = obj%name%Join(array=dofnames, sep="_")

! CALL vtk%WriteDataArray(name=name, x=VALUE(1:nrow, 1:ncol), &
!                         numberOfComponents=1)
CALL vtk%WriteDataArray(name=name, x=VALUE(1:nrow, 1), &
                        y=VALUE(1:nrow, 2), z=VALUE(1:nrow, 3))

name = ''
DEALLOCATE (dofnames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE VTKMethods
