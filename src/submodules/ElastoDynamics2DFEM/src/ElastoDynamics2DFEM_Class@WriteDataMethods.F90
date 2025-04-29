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

SUBMODULE(ElastoDynamics2DFEM_Class) WriteDataMethods

USE VTKFile_Class, ONLY: VTKFile_, VTK_BINARY_APPENDED, &
                         VTK_UnstructuredGrid
USE VTKPlot_Class, ONLY: VTKPlot_

USE AbstractNodeField_Class, ONLY: AbstractNodeFieldPointer_
USE ElemshapeData_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: Get_
USE basetype, ONLY: TypeFEVariableVector, TypeFEVariableSpace

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

CALL obj%UpdateStressStrain()

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

IF (ANY(obj%saveMeshFieldData)) CALL writeData_StressStrain(obj)

IF (debug) CALL e%RaiseInformation(modName//'::'// &
                                   myName//' - '//'[END]')
END PROCEDURE obj_WriteData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE writeData_StressStrain(obj)
  CLASS(ElastoDynamics2DFEM_), INTENT(INOUT) :: obj

  ! plot gauss points data as point plotting
  LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.
  INTEGER(I4B) :: iel, nrow, ncol, ii, jj, ind, order
  REAL(DFP) :: xij_qp(3, obj%maxNIP * obj%totalSpaceElements), &
               xij(3, obj%maxNNE), &
               stressval(3, obj%maxNIP * obj%totalSpaceElements), &
               strainval(3, obj%maxNIP * obj%totalSpaceElements)
  TYPE(FEVariable_) :: fevar
  TYPE(VTKPlot_) :: vtkplt
  TYPE(String) :: filename, path, suffix
  CHARACTER(*), PARAMETER :: ext = ".vtp"
  LOGICAL(LGT) :: isok

  suffix = tostring(obj%currentTimeStep - 1)
  path = obj%result_dir

  ind = 0

  IF (obj%saveStressAtCenter) THEN
    order = 1
  ELSE
    order = 2 * obj%spaceOrder
  END IF

  DO iel = 1, obj%totalSpaceElements

    CALL obj%fedof%GetQuadraturePoints1(quad=obj%quadForStress, &
                                        globalElement=iel, &
                                        quadratureType=obj%quadTypeForSpace, &
                                        order=order, &
                                        islocal=.TRUE.)

    CALL obj%fedof%GetLocalElemShapeData(globalElement=iel, &
                                         elemsd=obj%elemsdForSpace, &
                                         quad=obj%quadForStress, &
                                         islocal=.TRUE.)

    CALL obj%cellmesh%GetNodeCoord(nodecoord=xij, nrow=ii, ncol=jj, &
                                   globalElement=iel, islocal=.TRUE.)

    CALL obj%fedof%GetGlobalElemShapeData(globalElement=iel, &
                                          elemsd=obj%elemsdForSpace, &
                                          xij=xij, islocal=.TRUE.)

    CALL GetInterpolation_(obj=obj%elemsdForSpace, &
                           interpol=xij_qp(:, ind + 1:), &
                           val=xij(1:ii, 1:jj), nrow=nrow, ncol=ncol)

    isok = obj%saveMeshFieldData(1)
    IF (isok) THEN
      CALL obj%stress%Get(iel, fevar, islocal=.TRUE.)
      CALL Get_(obj=fevar, rank=TypeFEVariableVector, &
                vartype=TypeFEVariableSpace, &
                val=stressval(:, ind + 1:), nrow=nrow, ncol=ncol)
    END IF

    isok = obj%saveMeshFieldData(2)
    IF (isok) THEN
      CALL obj%strain%Get(iel, fevar, islocal=.TRUE.)
      CALL Get_(obj=fevar, rank=TypeFEVariableVector, &
                vartype=TypeFEVariableSpace, &
                val=strainval(:, ind + 1:), nrow=nrow, ncol=ncol)
    END IF

    ind = ind + ncol
  END DO

  xij_qp(3, :) = zero
  isok = obj%saveMeshFieldData(1)
  IF (isOK) THEN
    filename = path%chars()//obj%filename//"_stress_"//suffix//".vtp"
    CALL vtkplt%Scatter3D(x=xij_qp(1, 1:ind), y=xij_qp(2, 1:ind), &
                          z=xij_qp(3, 1:ind), &
                          w=TRANSPOSE(stressval(1:nrow, 1:ind)), &
                          label="stress", &
                          filename=filename%chars())
  END IF

  isok = obj%saveMeshFieldData(2)
  IF (isOK) THEN
    filename = path%chars()//obj%filename//"_strain_"//suffix//".vtp"
    CALL vtkplt%Scatter3D(x=xij_qp(1, 1:ind), y=xij_qp(2, 1:ind), &
                          z=xij_qp(3, 1:ind), &
                          w=TRANSPOSE(strainval(1:nrow, 1:ind)), &
                          label="strain", &
                          filename=filename%chars())
  END IF

END SUBROUTINE writeData_StressStrain

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteDataMethods
