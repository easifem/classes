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

MODULE Domain_Class
USE BaseType, ONLY: CSRSparsity_, CSRMatrix_, BoundingBox_
USE GlobalData, ONLY: DFP, I4B, LGT
USE AbstractMesh_Class, ONLY: AbstractMesh_, AbstractMeshPointer_
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE MeshFacetData_Class, ONLY: MeshFacetData_
USE AbstractMesh_Class, ONLY: AbstractMesh_

USE AbstractDomain_Class

IMPLICIT NONE
PRIVATE

PUBLIC :: Domain_
PUBLIC :: DomainPointer_
PUBLIC :: DomainDeallocate
PUBLIC :: Domain_Pointer
PUBLIC :: DomainSetSparsity

CHARACTER(*), PARAMETER :: modName = "Domain_Class"

!----------------------------------------------------------------------------
!                                                                   obj_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: obj_ contains finite element mesh data of a domain
!
!{!pages/docs-api/Domain/obj_.md!}

TYPE, EXTENDS(AbstractDomain_) :: Domain_
  PRIVATE
  TYPE(AbstractMeshPointer_), ALLOCATABLE :: meshVolume(:)
    !! meshVolume list of meshes of volume entities
  TYPE(AbstractMeshPointer_), ALLOCATABLE :: meshSurface(:)
    !! meshSurface list of meshes of surface entities
  TYPE(AbstractMeshPointer_), ALLOCATABLE :: meshCurve(:)
    !! meshCurve list of meshes of curve entities
  TYPE(AbstractMeshPointer_), ALLOCATABLE :: meshPoint(:)
    !! meshPoint list of meshes of point entities
  TYPE(MeshFacetData_), ALLOCATABLE :: meshFacetData(:)
  !! Mesh facet data
  TYPE(CSRSparsity_) :: meshMap
  !! Sparse mesh data in CSR format
  INTEGER(I4B), ALLOCATABLE :: local_nptrs(:)
  INTEGER(I4B), ALLOCATABLE :: global_nptrs(:)

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate data stored inside an instance of domain
  !! TODO Rename Deallocate to Deallocate
  FINAL :: obj_Final
  !! Finalizer for domain

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(Obj) :: IMPORT => obj_Import
  !! Initiates an instance of domain by importing data from meshfile

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of domain

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayDomainInfo => obj_DisplayDomainInfo
  !! Display the domain info

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayMeshFacetData => &
    & obj_DisplayMeshFacetData
  !! Display mesh facet data

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshPointer => obj_GetMeshPointer1
  !! This routine a pointer to Abstract mesh object

  PROCEDURE, PUBLIC, PASS(obj) :: IsElementPresent => obj_IsElementPresent
  !! Returns true if the global element is present in the mesh

  PROCEDURE, PASS(obj) :: GetNodeToElements1 => obj_GetNodeToElements1
  !! Get the list of elements connnected to a specified node
  PROCEDURE, PASS(obj) :: GetNodeToElements2 => obj_GetNodeToElements2
  !! Get the list of elements connnected to many specified nodes

  PROCEDURE, PASS(obj) :: GetNodeToElements1_ => obj_GetNodeToElements1_
  !! Get the list of elements connnected to a specified node
  PROCEDURE, PASS(obj) :: GetNodeToElements2_ => obj_GetNodeToElements2_
  !! Get the list of elements connnected to many specified nodes

  PROCEDURE, PASS(obj) :: GetLocalNodeNumber1 => obj_GetLocalNodeNumber1
  !! Local element number
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber2 => obj_GetLocalNodeNumber2
  !! Local element number

  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber1 => obj_GetGlobalNodeNumber1
  !! Returns the global node number of a local node number
  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber2 => obj_GetGlobalNodeNumber2
  !! Returns the global node number of a local node number

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs => obj_GetNptrs
  !! Get the node numbers

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs_ => obj_GetNptrs_
  !! Get the node numbers

  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs => &
    & obj_GetInternalNptrs
  ! Get the internal node numbers in the mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder => obj_GetOrder
  !! Get Order of meshes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMeshFacetData => &
    & obj_GetTotalMeshFacetData

  ! PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMaterial => obj_GetTotalMaterial1
  ! !! return the total materials
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: GetElemType => obj_GetElemType
  ! !! Returns the element type of each mesh
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: GetUniqueElemType =>  &
  !   & obj_GetUniqueElemType
  ! !! Returns the unique element type in each mesh
  ! !! The size of returned integer vector can be different from
  ! !! the total number of meshes present in domain.
  !
  ! ! SET:
  ! ! @SetMethods
  !
  ! PROCEDURE, PASS(obj) :: SetSparsity1 => obj_SetSparsity1
  ! !! Set sparsity
  ! PROCEDURE, NOPASS :: SetSparsity2 => obj_SetSparsity2
  ! !! Set sparsity
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetTotalMaterial => obj_SetTotalMaterial
  ! !! set the total number of materials
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetMaterial => obj_SetMaterial
  ! !! set the material
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetNodeCoord => obj_SetNodeCoord1
  ! !! setNodeCoord
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => obj_SetQuality
  !
  ! ! SET:
  ! ! @MeshDataMethods
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
  !   & obj_InitiateNodeToElements
  ! !! Initiate node to element data
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
  !   & obj_InitiateNodeToNodes
  ! !! Initiate node to node data
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
  !     & obj_InitiateElementToElements
  ! !! Initiate element to element data
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
  !     & obj_InitiateBoundaryData
  ! !! Initiate element to element data
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
  !     & obj_InitiateFacetElements
  ! !! Initiate element to element data
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
  !     & obj_InitiateExtraNodeToNodes
  ! !! Initiate extra node to nodes information for edge based methods
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetFacetElementType => &
  !   & obj_SetFacetElementType
  ! !! Set facet element of meshes
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetDomainFacetElement => &
  !   & obj_SetDomainFacetElement
  ! !! Set facet element of meshes
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetMeshmap => obj_SetMeshmap
  !
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetMeshFacetElement => &
  !   & obj_SetMeshFacetElement

END TYPE Domain_

!----------------------------------------------------------------------------
!                                                             DomainPointer
!----------------------------------------------------------------------------

TYPE :: DomainPointer_
  CLASS(Domain_), POINTER :: ptr => NULL()
END TYPE DomainPointer_

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Deallocate data stored in Domain object

INTERFACE DomainDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
    !! Domain object
  END SUBROUTINE obj_Deallocate
END INTERFACE DomainDeallocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Finalizer

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                          obj_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 June 2021
! summary: This function returns pointer to a newly constructed Domain obj

INTERFACE Domain_Pointer
  MODULE FUNCTION obj_Constructor_1(hdf5, group) RESULT(ans)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(Domain_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE Domain_Pointer

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Construct an instance of domain by importing data from mesh

INTERFACE DomainImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(Domain_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE DomainImport

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the domain

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(Domain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                               DisplayDomainInfo@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Display the domain

INTERFACE
  MODULE SUBROUTINE obj_DisplayDomainInfo(obj, msg, unitno)
    CLASS(Domain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayDomainInfo
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display mesh facet data

INTERFACE
  MODULE SUBROUTINE obj_DisplayMeshFacetData(obj, msg, unitno)
    CLASS(Domain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayMeshFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This rotuine returns mesh pointer
!
!# Introduction
!
! This returns the mesh Entity pointer.
! - dim is the dimension of the mesh; dim=0,1,2,3 corresponds to the point,
! curve, surface, volume meshes.
! - tag, is the number of mesh
!
! Calling example:
!
!```fortran
! GetMeshPointer(dim, entityNum)
! GetMeshPointer(globalElement)
! GetMeshPointer(dim, globalElement, islocal)
!```

INTERFACE
  MODULE FUNCTION obj_GetMeshPointer1(obj, dim, entityNum, &
                                      globalElement, islocal) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of mesh entity
    !! The default value of dim is obj%nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! entity number, it is used for domain_
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElement
    !! global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is global element a local element
    CLASS(AbstractMesh_), POINTER :: ans
    !! abstract mesh pointer
  END FUNCTION obj_GetMeshPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                               IsElementPresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Returns true if the element number is present inside the domain

INTERFACE
  MODULE FUNCTION obj_IsElementPresent(obj, globalElement, dim, entityNum, &
    & islocal) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension, if dim is present then
    !! if dim=0, then search is performed in meshPoint
    !! if dim=1, then search is performed in meshCurve
    !! if dim=2, then search is performed in meshSurface
    !! if dim=3, then search is performed in meshVolume
    !! The default value of dim is obj%nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! entity number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: returns the elements connected to a node

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements1(obj, globalNode, islocal) &
    & RESULT(ans)
    CLASS(Domain_), INTENT(INOUT) :: obj
      !! we can init the node to element data if necessary
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END FUNCTION obj_GetNodeToElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: returns the elements connected to a node

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements2(obj, globalNode, islocal) &
    & RESULT(ans)
    CLASS(Domain_), INTENT(INOUT) :: obj
      !! we can init the node to element data if necessary
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END FUNCTION obj_GetNodeToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: returns the elements connected to a node
!
!# Introduction
!
! For obj%nsd = 3, we use meshVolume
! For obj%nsd = 2, we use meshSurface
! For obj%nsd = 1, we use meshCurve
! for obj%nsd = 0, we use meshPoint

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements1_(obj, ans, tsize, &
                                            globalNode, islocal)
    CLASS(Domain_), INTENT(INOUT) :: obj
      !! We can init the node to element
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node to elements, it should be atleast tsize long
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! actual size of ans, it is returned by this routine
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is true it means globalNode is actually local node
  END SUBROUTINE obj_GetNodeToElements1_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: returns the elements connected to a node
!
!# Introduction
!
! For obj%nsd = 3, we use meshVolume
! For obj%nsd = 2, we use meshSurface
! For obj%nsd = 1, we use meshCurve
! for obj%nsd = 0, we use meshPoint

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements2_(obj, ans, tsize, &
                                            globalNode, islocal)
    CLASS(Domain_), INTENT(INOUT) :: obj
      !! We can ionit the node to element data
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node to elements, it should be atleast tsize long
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! actual size of ans, it is returned by this routine
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is true it means globalNode is actually local node
  END SUBROUTINE obj_GetNodeToElements2_
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber1(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! Global node number in mesh of obj%nsd dimension
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
    !! Local node number in mesh of obj%nsd dimension
  END FUNCTION obj_GetLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans(SIZE(globalNode))
  END FUNCTION obj_GetLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                             getGlobalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-16
! summary: Returns local node number of a global node number

INTERFACE
  MODULE PURE FUNCTION obj_GetGlobalNodeNumber1(obj, localNode) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getGlobalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-16
! summary: Returns local node number of a global node number

INTERFACE
  MODULE PURE FUNCTION obj_GetGlobalNodeNumber2(obj, localNode) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode(:)
    INTEGER(I4B) :: ans(SIZE(localNode))
  END FUNCTION obj_GetGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Sept 2021
! summary: this routine returns the global node number
!
!# Introduction
! This routine returns the global node number
! xidim is the dimension of the mesh

INTERFACE
  MODULE FUNCTION obj_GetNptrs(obj, dim, entityNum) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-17
! summary: this routine returns the global node number
!
!# Introduction
! This routine returns the global node number
! xidim is the dimension of the mesh

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs_(obj, nptrs, dim, entityNum, tsize)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim = 0 meshPoint is called
    !! dim=1 meshCurve is called
    !! dim=2, meshSurface is called
    !! dim=~3, meshVolume is called
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tsize
    !! Returns the size of nptrs where data has been written
  END SUBROUTINE obj_GetNptrs_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetInternalNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Sept 2021
! summary: this routine returns the global node number
!
!# Introduction
! This routine returns the global node number
! xidim is the dimension of the mesh

INTERFACE
  MODULE FUNCTION obj_GetInternalNptrs(obj, dim, entityNum) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This routine returns the order of meshes of dimensions=dim

INTERFACE
  MODULE FUNCTION obj_GetOrder(obj, dim) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalMeshFacetData@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: returns size of meshFacetData

INTERFACE
  MODULE FUNCTION obj_GetTotalMeshFacetData(obj, imeshFacetData) &
    RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: imeshFacetData
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalMeshFacetData
END INTERFACE

! !----------------------------------------------------------------------------
! !                                               GetTotalMaterial@GetMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 2021-12-09
! ! update: 2021-12-09
! ! summary: Returns the materials id of a given medium
!
! INTERFACE
!   MODULE FUNCTION obj_GetTotalMaterial1(obj, dim, globalElement, &
!                                         islocal, entityNum) RESULT(ans)
!     CLASS(Domain_), INTENT(IN) :: obj
!     INTEGER(I4B), INTENT(IN) :: dim
!     !! which dimension of the mesh we should search
!     INTEGER(I4B), INTENT(IN) :: globalElement
!     !! global element number
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
!     !! is globalElement a local one
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
!     !! This is used for backward compatibility, default is 1
!     INTEGER(I4B) :: ans
!     !! returns the total materials in the element
!   END FUNCTION obj_GetTotalMaterial1
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                                 GetElemType@GetMethods
! !----------------------------------------------------------------------------
!
! !> author: Vikas Sharma, Ph. D.
! ! date:  2023-09-23
! ! summary:  Returns the element type of each mesh in domain
!
! INTERFACE
!   MODULE FUNCTION obj_GetElemType(obj, dim) RESULT(ans)
!     CLASS(Domain_), INTENT(IN) :: obj
!     INTEGER(I4B), INTENT(IN) :: dim
!     INTEGER(I4B), ALLOCATABLE :: ans(:)
!   END FUNCTION obj_GetElemType
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                               GetUniqueElemType@GetMethods
! !----------------------------------------------------------------------------
!
! !> author: Vikas Sharma, Ph. D.
! ! date:  2023-09-23
! ! summary: Returns only the unique elements in the meshes of domain
!
! INTERFACE
!   MODULE FUNCTION obj_GetUniqueElemType(obj, dim) RESULT(ans)
!     CLASS(Domain_), INTENT(IN) :: obj
!     INTEGER(I4B), INTENT(IN) :: dim
!     INTEGER(I4B), ALLOCATABLE :: ans(:)
!   END FUNCTION obj_GetUniqueElemType
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                                     SetSparsity@setMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 12 Oct 2021
! ! summary: Set sparsity in [[CSRMatrix_]] from [[obj_]]
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetSparsity1(obj, mat)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!     TYPE(CSRMatrix_), INTENT(INOUT) :: mat
!   END SUBROUTINE obj_SetSparsity1
! END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-16
! summary: Set sparsity in [[CSRMatrix_]] from [[Domain_]]

INTERFACE DomainSetSparsity
  MODULE SUBROUTINE obj_SetSparsity2(domains, mat)
    CLASS(AbstractDomainPointer_), INTENT(INOUT) :: domains(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE obj_SetSparsity2
END INTERFACE DomainSetSparsity

! !----------------------------------------------------------------------------
! !                                               setTotalMaterial@setMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 2021-12-09
! ! update: 2021-12-09
! ! summary:
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetTotalMaterial(obj, dim, n)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: dim
!     INTEGER(I4B), INTENT(IN) :: n
!   END SUBROUTINE obj_SetTotalMaterial
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                                     SetMaterial@setMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 2021-12-09
! ! update: 2021-12-09
! ! summary: Set the materials id of a given medium
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetMaterial(obj, dim, entityNum, &
!     & medium, material)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: dim
!     INTEGER(I4B), INTENT(IN) :: entityNum
!     INTEGER(I4B), INTENT(IN) :: medium
!     INTEGER(I4B), INTENT(IN) :: material
!   END SUBROUTINE obj_SetMaterial
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                                   SetNodeCoord@SetMethods
! !----------------------------------------------------------------------------
!
! !> author: Vikas Sharma, Ph. D.
! ! date:  2023-02-24
! ! summary: SetNodeCoord
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetNodeCoord1(obj, nodeCoord, scale, &
!     & addContribution)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!     REAL(DFP), INTENT(IN) :: nodeCoord(:, :)
!     !! nodal coordinate in xij Format
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!   END SUBROUTINE obj_SetNodeCoord1
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                                   SetQuality@SetMethods
! !----------------------------------------------------------------------------
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetQuality(obj, measures, max_measures, &
!     & min_measures, dim, entityNum)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: measures(:)
!     REAL(DFP), INTENT(OUT) :: max_measures(:)
!     REAL(DFP), INTENT(OUT) :: min_measures(:)
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
!   END SUBROUTINE obj_SetQuality
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                     InitiateNodeToElements@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 4 Nov 2022
! ! summary: This routine sets the node-to-elements data in mesh of domain
!
! INTERFACE
!   MODULE SUBROUTINE obj_InitiateNodeToElements(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_InitiateNodeToElements
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                        InitiateNodeToNodes@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 4 Nov 2022
! ! summary: This routine sets the node-to-nodes data in mesh of domain
!
! INTERFACE
!   MODULE SUBROUTINE obj_InitiateNodeToNodes(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_InitiateNodeToNodes
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                  InitiateElementToElements@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 4 Nov 2022
! ! summary: This routine sets the element-to-element data in mesh of domain
!
! INTERFACE
!   MODULE SUBROUTINE obj_InitiateElementToElements(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_InitiateElementToElements
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                       InitiateBoundaryData@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 4 Nov 2022
! ! summary: This routine sets the boundarydata info in mesh of domain
! !
! !# Introduction
! !
! ! This routine sets the boundary data info in mesh of domain.
! ! This routine calls `InitiateBoundarydata` on each mesh
! ! Then, it calls SetFacetElementType() on domain object.
!
! INTERFACE
!   MODULE SUBROUTINE obj_InitiateBoundaryData(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_InitiateBoundaryData
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                      InitiateFacetElements@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 4 Nov 2022
! ! summary: This routine sets the facet elements data in mesh of domain
!
! INTERFACE
!   MODULE SUBROUTINE obj_InitiateFacetElements(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_InitiateFacetElements
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                   InitiateExtraNodeToNodes@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 4 Nov 2022
! ! summary: This routine sets the node-to-nodes data in mesh of domain
!
! INTERFACE
!   MODULE SUBROUTINE obj_InitiateExtraNodeToNodes(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_InitiateExtraNodeToNodes
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                       SetFacetElementType@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 14 April 2022
! ! summary: This routine sets the domain boundary element for cells and faces
! !
! !# Introduction
! !
! ! The boudnary element of mesh may not be domain boundary element. This
! ! is because mesh does not have information of surrounding mesh. Therefore
! ! for mesh methods there is no distinction between boundary element
! ! and domain-boundary-element. And mesh-method set all of its boundary-elem
! ! to domain-elem.
! !
! ! This methods correctly identifies the domain-boundary-element from
! ! mesh boundary-element.
! ! In this way the mesh-boundary-element, which are not domain-boundary-element
! ! can be treated as the interface element between two meshes.
! !
! ! This methods needs following information:
! !
! !- boundary element data should be initiated for each mesh, this means
! ! a call to InitiateBoundaryElementData is necessary
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetFacetElementType(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_SetFacetElementType
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                      SetDomainFacetElement@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 14 April 2022
! ! summary: This routine sets the domain boundary element for cells and faces
! !
! !# Introduction
! !
! ! This routine sets the domain boundary element for cells and faces.
! !
! ! When we call [InitiateFacetElement](../Mesh/InitiateFacetElement.md)
! ! for mesh,
! ! we can only identify boundary-facet-elements (i.e., boundary elements
! ! of the mesh).
! ! Moreover, when we call
! ! [InitiateFacetElement](../Mesh/InitiateFacetElement.md)
! ! from mesh or domain, all the facet elements are tagged
! ! as `DOMAIN_BOUNDARY_ELEMENT`.
! !
! ! However, some of these boundary facet-elements will be located at the
! ! domain’s boundary.
! ! These facet elements are called `DOMAIN_BOUNDARY_ELEMENT`.
! !
! ! Some of the facet elements will be at located at the interface of two
! ! mesh regions, these facet elements are called `BOUNDARY_ELEMENT`.
! !
! ! This method correctly differentiates between `BOUNDARY_ELEMENT`  and
! ! `DOMAIN_BOUNDARY_ELEMENT`.
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetDomainFacetElement(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_SetDomainFacetElement
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                                 SetMeshmap@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 20 May 2022
! ! summary: This routine sets meshMap
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetMeshmap(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_SetMeshmap
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                        SetMeshFacetElement@MeshDataMethods
! !----------------------------------------------------------------------------
!
! !> authors: Vikas Sharma, Ph. D.
! ! date: 20 May 2022
! ! summary: This routine sets meshFacetData
!
! INTERFACE
!   MODULE SUBROUTINE obj_SetMeshFacetElement(obj)
!     CLASS(Domain_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_SetMeshFacetElement
! END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Domain_Class
