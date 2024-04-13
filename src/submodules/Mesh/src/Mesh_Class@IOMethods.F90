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

SUBMODULE(Mesh_Class) IOMethods
USE Display_Method
USE ReallocateUtility
USE ReferenceElement_Method
USE InputUtility
USE HDF5File_Method, ONLY: HDF5ReadScalar, HDF5ReadVector,  &
  & HDF5ReadMatrix

USE NodeData_Class, ONLY: NodeData_Display
USE ElemData_Class, ONLY: ElemData_Display
USE FacetData_Class, ONLY: BoundaryFacetData_Display,  &
& InternalFacetData_Display
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL AbstractMeshDisplay(obj=obj, msg=msg, unitno=unitno)

CALL Display(obj%elemType, "elemType: ", unitno=unitno)
abool = ASSOCIATED(obj%refElem)
CALL Display(abool, "refElem ASSOCIATED: ", unitno=unitno)
abool = ALLOCATED(obj%facetElements)
CALL Display(abool, "facetElements ALLOCATED: ", unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
CHARACTER(:), ALLOCATABLE :: dsetname
LOGICAL(LGT) :: isok
INTEGER(I4B) :: temp4(4), xidim, nsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

dsetname = TRIM(group)
CALL AbstractMeshImport(obj=obj, hdf5=hdf5, group=group)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%elemType, group=dsetname,  &
  & fieldname="elemType", myname=myname, modname=modname, check=.TRUE.)

xidim = obj%GetXidimension()
nsd = obj%GetNSD()

obj%refelem => ReferenceElement_Pointer(xidim=xidim, &
  & nsd=nsd, elemType=obj%elemType, ipType=Equidistance)

isok = xidim .GT. 0
IF (isok) THEN
  temp4 = TotalEntities(obj%elemType)
  ALLOCATE (obj%facetElements(temp4(xidim)))
  CALL GetFacetElements(refelem=obj%refelem, ans=obj%facetElements)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                      DisplayFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayFacetElements
INTEGER(I4B) :: ii, n
LOGICAL(LGT) :: abool

abool = ALLOCATED(obj%facetElements)
IF (abool) THEN; n = SIZE(obj%facetElements); ELSE; n = 0; END IF

CALL Display(msg, unitno=unitno)
CALL Display(abool, "boundaryFacetData ALLOCATED: ", unitno=unitno)

DO ii = 1, n

  CALL Display(obj%facetElements(ii), &
    & "obj%facetElements("//tostring(ii)//"): ", unitno=unitno)

  CALL BlankLines(nol=1, unitno=unitno)

END DO
END PROCEDURE obj_DisplayFacetElements

END SUBMODULE IOMethods
