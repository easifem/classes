! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(ScalarWave2DFEM_Class) WriteDataMethods

USE VTKFile_Class, ONLY: VTKFile_, VTK_BINARY_APPENDED, &
                         VTK_UnstructuredGrid

USE AbstractNodeField_Class, ONLY: AbstractNodeFieldPointer_

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData
CHARACTER(*), PARAMETER :: myName = "obj_WriteData()"
TYPE(VTKFile_) :: avtk
TYPE(String) :: path, filename, suffix
CHARACTER(*), PARAMETER :: ext = ".vtu"
LOGICAL(LGT) :: isOK

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')

isOk = MOD(obj%currentTimeStep - 1, obj%OutputFreq) .NE. 0_I4B
IF (isOk) RETURN

suffix = tostring(obj%currentTimeStep - 1)
path = obj%result_dir

isok = obj%saveData(1)
IF (isok) THEN
  filename = path%chars()//obj%filename//"_disp_"//suffix//ext
  CALL avtk%InitiateVTKFile(filename=filename%chars(), mode="NEW", &
       DataFormat=VTK_BINARY_APPENDED, DataStructureType=VTK_UnStructuredGrid)
  CALL obj%u0%WriteData(vtk=avtk)
  CALL avtk%DEALLOCATE()
END IF

isok = obj%saveData(2)
IF (isok) THEN
  filename = path%chars()//obj%filename//"_velo_"//suffix//ext
  CALL avtk%InitiateVTKFile(filename=filename%chars(), mode="NEW", &
       DataFormat=VTK_BINARY_APPENDED, DataStructureType=VTK_UnStructuredGrid)
  CALL obj%v0%WriteData(vtk=avtk)
  CALL avtk%DEALLOCATE()
END IF

isok = obj%saveData(3)
IF (isok) THEN
  filename = path%chars()//obj%filename//"_acc_"//suffix//ext
  CALL avtk%InitiateVTKFile(filename=filename%chars(), mode="NEW", &
       DataFormat=VTK_BINARY_APPENDED, DataStructureType=VTK_UnStructuredGrid)
  CALL obj%a0%WriteData(vtk=avtk)
  CALL avtk%DEALLOCATE()
END IF

IF (debug) CALL e%RaiseInformation(modName//'::'// &
                                   myName//' - '//'[END]')
END PROCEDURE obj_WriteData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteDataMethods
