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

SUBMODULE(ScalarField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
USE Mesh_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_Import
CHARACTER(*), PARAMETER :: myName = "sField_Import"
TYPE(String) :: dsetname
LOGICAL(LGT) :: bools(3)
TYPE(ParameterList_) :: param

! info
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Import()")

CALL AbstractNodeFieldImport( &
  & obj=obj, &
  & hdf5=hdf5, &
  & group=group, &
  & dom=dom, &
  & domains=domains)

dsetname = TRIM(group)//"/tSize"
bools(1) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/dof"
bools(2) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/realVec"
bools(3) = hdf5%pathExists(dsetname%chars())

IF (.NOT. ALL(bools)) THEN
! Initiate
  CALL param%initiate()
  CALL SetScalarFieldParam( &
    & param=param, &
    & name=obj%name%chars(), &
    & engine=obj%engine%chars(), &
    & fieldType=obj%fieldType)
  obj%isInitiated = .FALSE.
  CALL obj%initiate(param=param, dom=dom)
  CALL param%DEALLOCATE()
END IF

! info
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Import()")

END PROCEDURE sField_Import

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_WriteData_vtk
CHARACTER(*), PARAMETER :: myName = "sField_WriteData_vtk()"
LOGICAL(LGT) :: isOK
TYPE(Domain_), POINTER :: dom
TYPE(Mesh_), POINTER :: meshPtr
INTEGER(I4B) :: imesh, tMesh, nsd
INTEGER(I4B), PARAMETER :: tPhysicalVars = 1
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
REAL(DFP), ALLOCATABLE :: nodalval(:), xij(:, :)
CHARACTER(1) :: names(tPhysicalVars)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] WriteData()')
#endif

isOK = obj%isInitiated
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: ScalarNodeField_::obj is not isInitiated.')
  RETURN
END IF

isOK = vtk%isOpen()
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: VTKFile_::vtk is not open.')
  RETURN
END IF

isOK = ASSOCIATED(obj%domain)
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Either AbstractNodeField_::obj%domain, '// &
    & ' not allocated.')
  RETURN
END IF

CALL obj%GetPhysicalNames(names)

dom => obj%domain
nsd = dom%GetNSD()
tMesh = dom%GetTotalMesh(dim=nsd)

DO imesh = 1, tMesh
  meshptr => dom%GetMeshPointer(dim=nsd, entityNum=imesh)

  CALL dom%GetNodeCoord(nodeCoord=xij, dim=nsd, entityNum=imesh)

  CALL meshPtr%ExportToVTK(vtkfile=vtk, nodeCoord=xij,  &
    & openTag=.TRUE., content=.TRUE., closeTag=.FALSE.)

  CALL vtk%WriteDataArray(location=String('node'), action=String('open'))

  nptrs = meshPtr%GetNptrs()

  CALL obj%Get(globalNode=nptrs, VALUE=nodalval)

  CALL vtk%WriteDataArray(name=String(names(1)), x=nodalval,  &
    & numberOfComponents=1)

  CALL vtk%WriteDataArray(location=String('node'), action=String('close'))

  CALL vtk%WritePiece()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] WriteData()')
#endif
END PROCEDURE sField_WriteData_vtk

END SUBMODULE IOMethods
