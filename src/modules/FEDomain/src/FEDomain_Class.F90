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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! update:
!   - 12 Nov 2021
!   - 4 Nov 2022
! summary: This module contains methods for domain data type

MODULE FEDomain_Class
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE HDF5File_Class, ONLY: HDF5File_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: LGT, I4B, DFP
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table
USE BaseType, ONLY: BoundingBox_

IMPLICIT NONE
PRIVATE

PUBLIC :: FEDomain_
PUBLIC :: FEDomainPointer_
PUBLIC :: FEDomain_Pointer

CHARACTER(*), PARAMETER :: modName = "FEDomain_Class"

!----------------------------------------------------------------------------
!                                                                   FEDomain_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: FEDomain_ contains finite element mesh data of a domain
!
!{!pages/docs-api/FEDomain/FEDomain_.md!}

TYPE, EXTENDS(AbstractDomain_) :: FEDomain_
  PRIVATE
  CLASS(AbstractMesh_), POINTER :: meshVolume => NULL()
    !! meshVolume list of meshes of volume entities
  CLASS(AbstractMesh_), POINTER :: meshSurface => NULL()
    !! meshSurface list of meshes of surface entities
  CLASS(AbstractMesh_), POINTER :: meshCurve => NULL()
    !! meshCurve list of meshes of curve entities
  CLASS(AbstractMesh_), POINTER :: meshPoint => NULL()
    !! meshPoint list of meshes of point entities
  CLASS(AbstractMesh_), POINTER :: mesh => NULL()
    !! mesh points to meshVolume for nsd = 3
    !! mesh points to meshSurface for nsd = 2
    !! mesh points to meshCurve for nsd = 1
    !! mesh points to meshPoint for nsd = 0
CONTAINS

  ! CONSTRUCTOR:
  !@ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate an instance of domain
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate data stored inside an instance of domain
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Initiates an instance of domain by importing data from meshfile

  !! TODO Add an export method to [[obj_]] class

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayDomainInfo =>  &
    & obj_DisplayDomainInfo

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshPointer => obj_GetMeshPointer1

  ! SET:
  ! @MeshDataMethods

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    & obj_InitiateNodeToElements
  !! Initiate node to element data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    & obj_InitiateNodeToNodes
  !! Initiate node to node data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
      & obj_InitiateElementToElements
  !! Initiate element to element data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
      & obj_InitiateBoundaryData
  !! Initiate element to element data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
      & obj_InitiateFacetElements
  !! Initiate element to element data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
      & obj_InitiateExtraNodeToNodes
  !! Initiate extra node to nodes information for edge based methods

END TYPE FEDomain_

!----------------------------------------------------------------------------
!                                                             FEDomainPointer
!----------------------------------------------------------------------------

TYPE :: FEDomainPointer_
  CLASS(FEDomain_), POINTER :: ptr => NULL()
END TYPE FEDomainPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Initiate the instance of [[AbstractDomain_]] object

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, hdf5, group)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! AbstractDomainData object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! Group name (directory name)
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Deallocate data stored in AbstractDomain object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! AbstractDomain object
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Finalizer

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(FEDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                        FEDomain_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: This function returns pointer to a newly constructed FEDomain obj

INTERFACE FEDomain_Pointer
  MODULE FUNCTION obj_Constructor_1(hdf5, group) RESULT(ans)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDomain_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE FEDomain_Pointer

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Construct an instance of domain by importing data from mesh

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the domain

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                               DisplayDomainInfo@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the domain

INTERFACE
  MODULE SUBROUTINE obj_DisplayDomainInfo(obj, msg, unitno)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayDomainInfo
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This rotuine returns mesh pointer
!
!# Introduction
!
! This returns the pointer to the abtract mesh object
! - dim is the dimension of the mesh; dim=0,1,2,3 corresponds to the point,
! curve, surface, volume meshes.
! - the default value of dim is obj%nsd

INTERFACE
  MODULE FUNCTION obj_GetMeshPointer1(obj, dim) RESULT(Ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of mesh entity
    !! The default value of dim is obj%nsd
    CLASS(AbstractMesh_), POINTER :: ans
  END FUNCTION obj_GetMeshPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-elements data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToElements(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-nodes data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToNodes(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the element-to-element data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateElementToElements(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateBoundaryData@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the boundarydata info in mesh of domain
!
!# Introduction
!
! This routine sets the boundary data info in mesh of domain.
! This routine calls `InitiateBoundarydata` on each mesh
! Then, it calls SetFacetElementType() on domain object.

INTERFACE
  MODULE SUBROUTINE obj_InitiateBoundaryData(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                      InitiateFacetElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the facet elements data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetElements(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateExtraNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-nodes data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateExtraNodeToNodes(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateExtraNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEDomain_Class
