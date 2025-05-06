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

SUBMODULE(Domain_Class) IOMethods
USE Display_Method

USE AbstractDomain_Class, ONLY: AbstractDomainDisplay, &
                                AbstractDomainDisplayDomainInfo, &
                                AbstractDomainImport
USE Mesh_Class, ONLY: Mesh_Pointer

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL AbstractDomainDisplay(obj=obj, msg=msg, unitno=unitno)
abool = obj%IsInit()
IF (.NOT. abool) RETURN

abool = ALLOCATED(obj%local_nptrs)
CALL Display(abool, "local_nptrs Allocated: ", unitno=unitno)

abool = ALLOCATED(obj%global_nptrs)
CALL Display(abool, "global_nptrs Allocated: ", unitno=unitno)

abool = ALLOCATED(obj%meshVolume)
CALL Display(abool, "meshVolume Allocated: ", unitno=unitno)

abool = ALLOCATED(obj%meshSurface)
CALL Display(abool, "meshSurface Allocated: ", unitno=unitno)

abool = ALLOCATED(obj%meshCurve)
CALL Display(abool, "meshCurve Allocated: ", unitno=unitno)

abool = ALLOCATED(obj%meshPoint)
CALL Display(abool, "meshPoint Allocated: ", unitno=unitno)

abool = ALLOCATED(obj%meshFacetData)
CALL Display(abool, "meshFacetData Allocated: ", unitno=unitno)

CALL Display(obj%meshMap%isInitiated, "meshMap Initiated: ", unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                          DisplaDomainInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayDomainInfo
LOGICAL(LGT) :: abool
INTEGER(I4B) :: nsd

CALL AbstractDomainDisplayDomainInfo(obj=obj, unitno=unitno, msg=msg)

nsd = obj%GetNSD()

SELECT CASE (nsd)
CASE (0)
  CALL callme(obj%meshPoint)
CASE (1)
  CALL callme(obj%meshCurve)
CASE (2)
  CALL callme(obj%meshSurface)
CASE (3)
  CALL callme(obj%meshVolume)
END SELECT

CONTAINS
SUBROUTINE callme(meshes)
  TYPE(AbstractMeshPointer_), ALLOCATABLE, INTENT(IN) :: meshes(:)

  ! internal variables
  CLASS(AbstractMesh_), POINTER :: meshptr
  INTEGER(I4B) :: tsize, ii
  LOGICAL(LGT) :: isok

  isok = ALLOCATED(meshes)
  IF (.NOT. isok) RETURN

  tsize = SIZE(meshes)
  DO ii = 1, tsize
    meshptr => meshes(ii)%ptr
    abool = ASSOCIATED(meshptr)

    CALL Display(abool, "mesh ASSOCIATED: ", unitno=unitno)

    IF (.NOT. abool) CYCLE

    CALL meshptr%DisplayMeshInfo("Mesh Info:", unitno=unitno)

  END DO

  meshptr => NULL()

END SUBROUTINE callme

END PROCEDURE obj_DisplayDomainInfo

!----------------------------------------------------------------------------
!                                                       DisplayMeshFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayMeshFacetData
INTEGER(I4B) :: telements, ii
LOGICAL(LGT) :: abool

CALL Display(msg, unitNo=unitNo)

abool = ALLOCATED(obj%meshFacetData)
CALL Display(abool, "meshFacetData ALLOCATED: ", unitNo=unitNo)
IF (.NOT. abool) RETURN

telements = SIZE(obj%meshFacetData)
DO ii = 1, telements
  CALL obj%meshFacetData(ii)%Display(msg="meshFacetData(" &
                                     //tostring(ii)//"): ", unitno=unitno)
  CALL BlankLines(nol=2, unitno=unitno)
END DO

END PROCEDURE obj_DisplayMeshFacetData

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "AbstractDomain_Import()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL AbstractDomainImport(obj=obj, hdf5=hdf5, group=group)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Calling DomainImportMesh() for volume')
#endif

CALL DomainImportMesh(obj=obj, hdf5=hdf5, group=group, myName=myName, &
                nsd=3_I4B, ent_name="/volumeEntities_", meshes=obj%meshVolume)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Calling DomainImportMesh() for surface')
#endif

CALL DomainImportMesh(obj=obj, hdf5=hdf5, group=group, myName=myName, &
              nsd=2_I4B, ent_name="/surfaceEntities_", meshes=obj%meshSurface)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Calling DomainImportMesh() for curve')
#endif

CALL DomainImportMesh(obj=obj, hdf5=hdf5, group=group, myName=myName, &
                  nsd=1_I4B, ent_name="/curveEntities_", meshes=obj%meshCurve)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Calling DomainImportMesh() for point')
#endif

CALL DomainImportMesh(obj=obj, hdf5=hdf5, group=group, myName=myName, &
                  nsd=0_I4B, ent_name="/pointEntities_", meshes=obj%meshPoint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                        DomainImportMesh
!----------------------------------------------------------------------------

SUBROUTINE DomainImportMesh(obj, hdf5, group, myName, nsd, ent_name, &
                            meshes)
  CLASS(Domain_), INTENT(INOUT) :: obj
  TYPE(HDF5File_), INTENT(INOUT) :: hdf5
  CHARACTER(*), INTENT(IN) :: group
  CHARACTER(*), INTENT(IN) :: myName
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: ent_name
  TYPE(AbstractMeshPointer_), INTENT(INOUT) :: meshes(:)

  ! internal variables
  INTEGER(I4B) :: ii, jj
  CLASS(AbstractMesh_), POINTER :: meshptr
  LOGICAL(LGT) :: problem
  CHARACTER(:), ALLOCATABLE :: dsetname

  meshptr => NULL()
  obj%tElements(nsd) = 0

  DO ii = 1, obj%tEntities(nsd)

    dsetname = group//ent_name//tostring(ii)
    meshptr => Mesh_Pointer(hdf5=hdf5, group=dsetname)

#ifdef DEBUG_VER
    problem = .NOT. ASSOCIATED(meshptr)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//" - "// &
        & '[INTERNAL ERROR] :: mesh for '//dsetname//" not ASSOCIATED.")
      RETURN
    END IF
#endif

    meshes(ii)%ptr => meshptr
    jj = meshptr%GetTotalElements()
    obj%tElements(nsd) = obj%tElements(nsd) + jj

  END DO

  NULLIFY (meshptr)
  dsetname = ""

END SUBROUTINE DomainImportMesh

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
