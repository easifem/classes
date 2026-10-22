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

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-18
! summary: Finite element domain class
!
! This module defines FEDomain_, the finite element domain, and
! FEDomainPointer_.

MODULE FEDomain_Class
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE HDF5File_Class, ONLY: HDF5File_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: LGT, I4B, DFP
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table
USE BaseType, ONLY: BoundingBox_
USE ElemData_Class, ONLY: ElemData_

IMPLICIT NONE
PRIVATE

PUBLIC :: FEDomain_
PUBLIC :: FEDomainPointer_
PUBLIC :: FEDomain_Pointer

CHARACTER(*), PARAMETER :: modName = "FEDomain_Class"

!----------------------------------------------------------------------------
!                                                                   FEDomain_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Finite element domain
!
! FEDomain_ extends AbstractDomain_. It holds one FEMesh_ for each dimension
! up to nsd. The mesh of a dimension contains the elements of all entities of
! that dimension.
!
! - meshVolume, meshSurface, meshCurve, meshPoint: meshes of dimension 3, 2,
!   1, 0 (not associated if the dimension is larger than nsd);
! - mesh: points to the mesh of dimension nsd.
!
! See also: [[obj_Import]]

TYPE, EXTENDS(AbstractDomain_) :: FEDomain_
  PRIVATE
  CLASS(AbstractMesh_), POINTER :: meshVolume => NULL()
  !! mesh of all volume entities
  CLASS(AbstractMesh_), POINTER :: meshSurface => NULL()
  !! mesh of all surface entities
  CLASS(AbstractMesh_), POINTER :: meshCurve => NULL()
  !! mesh of all curve entities
  CLASS(AbstractMesh_), POINTER :: meshPoint => NULL()
  !! mesh of all point entities
  CLASS(AbstractMesh_), POINTER :: mesh => NULL()
  !! mesh points to meshVolume for nsd = 3
  !! mesh points to meshSurface for nsd = 2
  !! mesh points to meshCurve for nsd = 1
  !! mesh points to meshPoint for nsd = 0
CONTAINS

  ! CONSTRUCTOR:
  !@ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data of the domain
  FINAL :: obj_Final
  !! Finalizer, calls Deallocate

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the domain from a HDF5 mesh file

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of the domain

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayDomainInfo => &
    obj_DisplayDomainInfo
  !! Display the meta data of the domain

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshPointer => obj_GetMeshPointer1
  !! Get a pointer to a mesh of the domain
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber1 => obj_GetLocalNodeNumber1
  !! Get the local number of a global node
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber2 => obj_GetLocalNodeNumber2
  !! Get the local numbers of global nodes
  PROCEDURE, PASS(obj) :: GetLocalElemNumber1 => obj_GetLocalElemNumber1
  !! Get the local numbers of global elements
  PROCEDURE, PASS(obj) :: GetLocalElemNumber2 => obj_GetLocalElemNumber2
  !! Get the local number of a global element
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalEdgeNumber => &
    obj_GetGlobalEdgeNumber
  !! Get the global edge number of a local edge
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFaceNumber => &
    obj_GetGlobalFaceNumber
  !! Get the global face number of a local face
  PROCEDURE, PUBLIC, PASS(obj) :: GetElemData => obj_GetElemData
  !! Get a copy of the element data
  PROCEDURE, PUBLIC, PASS(obj) :: GetElemDataPointer => obj_GetElemDataPointer
  !! Get a pointer to the element data
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEntitiesList => &
    obj_GetTotalEntitiesList
  !! Get the number of entities of an element

  PROCEDURE, PASS(obj) :: GetConnectivity1_ => obj_GetConnectivity1_
  !! Get the connectivity of an element

  PROCEDURE, PASS(obj) :: GetConnectivity2_ => obj_GetConnectivity2_
  !! Get all connectivities of an element

  PROCEDURE, PASS(obj) :: GetTotalVertexNodes1 => obj_GetTotalVertexNodes1
  !! Get the number of vertex nodes
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes2 => obj_GetTotalVertexNodes2
  !! Get the number of vertex nodes of a list of elements

  ! SET:
  ! @MeshDataMethods

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    obj_InitiateNodeToElements
  !! Initiate the node to elements data of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    obj_InitiateNodeToNodes
  !! Initiate the node to nodes data of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
    obj_InitiateElementToElements
  !! Initiate the element to elements data of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
    obj_InitiateFacetElements
  !! Initiate the facet elements of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
    obj_InitiateExtraNodeToNodes
  !! Initiate the extra node to nodes data of the meshes

END TYPE FEDomain_

!----------------------------------------------------------------------------
!                                                             FEDomainPointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Pointer to a FEDomain_
!
! ptr points to a FEDomain_.

TYPE :: FEDomainPointer_
  CLASS(FEDomain_), POINTER :: ptr => NULL()
END TYPE FEDomainPointer_

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Deallocate the data of the domain
!
! Deallocate the data of AbstractDomain_, call Deallocate on every mesh of the
! domain and nullify the mesh pointers. The mesh objects themselves are not
! freed.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%DEALLOCATE()
!   CALL OK(.NOT. obj%IsInitiated(), "IsInitiated")
!   CALL obj%Initiate(hdf5=meshfile, group="")
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "Initiate again")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:DEALLOCATE]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Finalizer of FEDomain_
!
! Call Deallocate.
!
! Arguments:
!   obj : TYPE(FEDomain_), intent(inout) - domain object
!
! See also: [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                        FEDomain_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Get a pointer to a new domain
!
! Allocate a new FEDomain_, initiate it from the mesh file, and return a
! pointer to it.
!
! Arguments:
!   hdf5 : TYPE(HDF5File_), intent(inout) - mesh file, open with read access
!   group : CHARACTER(*), intent(in) - group in the mesh file
!
! Returns:
!   ans : CLASS(FEDomain_), pointer - pointer to the new domain
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE FEDomain_Class, ONLY: FEDomain_Pointer
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(FEDomain_), POINTER :: ptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!
!   ptr => FEDomain_Pointer(hdf5=meshfile, group="")
!   CALL OK(ptr%IsInitiated(), "IsInitiated")
!   CALL OK(ptr%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
!   CALL ptr%DEALLOCATE()
!   DEALLOCATE (ptr)
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE FEDomain_Pointer
  MODULE FUNCTION obj_Constructor_1(hdf5, group) RESULT(ans)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! mesh file, open with read access
    CHARACTER(*), INTENT(IN) :: group
    !! group in the mesh file
    CLASS(FEDomain_), POINTER :: ans
    !! pointer to the new domain
  END FUNCTION obj_Constructor_1
END INTERFACE FEDomain_Pointer

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Import the domain from a HDF5 mesh file
!
! Import the meta data of the domain (see AbstractDomain_ Import). Then make
! the meshes: the volume mesh if nsd = 3, the surface mesh if nsd > 1, the
! curve mesh if nsd >= 1, and always the point mesh. Each mesh is initiated
! with all entities of its dimension, and tElements is set. mesh points to the
! mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!   hdf5 : TYPE(HDF5File_), intent(inout) - mesh file, open with read access
!   group : CHARACTER(*), intent(in) - group in the mesh file
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!
!   CALL obj%IMPORT(hdf5=meshfile, group="")
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:IMPORT]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! mesh file, open with read access
    CHARACTER(*), INTENT(IN) :: group
    !! group in the mesh file
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-20
! summary: Display the content of the domain
!
! Display the data of AbstractDomain_ and the mesh info of each mesh.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%Display("domain:")
!   CALL obj%DisplayDomainInfo("domain info:")
!   CALL OK(obj%IsInitiated(), "domain is not changed")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_DisplayDomainInfo]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                               DisplayDomainInfo@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-20
! summary: Display the meta data of the domain
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! See also: [[obj_Display]]

INTERFACE
  MODULE SUBROUTINE obj_DisplayDomainInfo(obj, msg, unitno)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_DisplayDomainInfo
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Get a pointer to a mesh of the domain
!
! Return the mesh of dimension dim, or the mesh of dimension nsd if dim is
! absent. In FEDomain_ the mesh of a dimension contains all entities of that
! dimension; entityNum, globalElement and islocal are not used. The pointer is
! not associated if the domain has no mesh of dimension dim.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number; not used
!     by FEDomain_
!   globalElement : INTEGER(I4B), optional, intent(in) - global element
!     number; not used by FEDomain_
!   islocal : LOGICAL(LGT), optional, intent(in) - not used by FEDomain_
!
! Returns:
!   ans : CLASS(AbstractMesh_), pointer - pointer to the mesh
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   mesh => obj%GetMeshPointer()
!   CALL OK(mesh%GetXidimension() .EQ. 2_I4B, "default is nsd")
!   CALL OK(mesh%GetTotalElements() .EQ. 4_I4B, "surface mesh")
!   mesh => obj%GetMeshPointer(dim=1_I4B)
!   CALL OK(mesh%GetTotalElements() .EQ. 8_I4B, "curve mesh")
!   mesh => obj%GetMeshPointer(dim=0_I4B)
!   CALL OK(mesh%GetTotalElements() .EQ. 4_I4B, "point mesh")
!   mesh => obj%GetMeshPointer(dim=3_I4B)
!   CALL OK(.NOT. ASSOCIATED(mesh), "no volume mesh")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_GetMeshPointer1(obj, dim, entityNum, &
    globalElement, islocal) RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number; not used by FEDomain_
    INTEGER(I4B), INTENT(IN), OPTIONAL :: globalElement
    !! global element number; not used by FEDomain_
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! not used by FEDomain_
    CLASS(AbstractMesh_), POINTER :: ans
    !! pointer to the mesh
  END FUNCTION obj_GetMeshPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the local number of a global node
!
! Implementation of AbstractDomain_ GetLocalNodeNumber for FEDomain_. It uses
! the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - local node number
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetLocalNodeNumber(7_I4B) .EQ. 7_I4B, &
!           "GetLocalNodeNumber")
!   CALL OK(ALL(obj%GetLocalNodeNumber([2_I4B, 3_I4B]) .EQ. [2, 3]), &
!           "GetLocalNodeNumber(:)")
!   CALL OK(obj%GetGlobalNodeNumber(7_I4B) .EQ. 7_I4B, &
!           "GetGlobalNodeNumber")
!   CALL OK(ALL(obj%GetGlobalNodeNumber([2_I4B, 3_I4B]) .EQ. [2, 3]), &
!           "GetGlobalNodeNumber(:)")
!   CALL OK(obj%GetLocalElemNumber(15_I4B) .EQ. 3_I4B, &
!           "GetLocalElemNumber")
!   CALL OK(ALL(obj%GetLocalElemNumber([13_I4B, 16_I4B]) .EQ. [1, 4]), &
!           "GetLocalElemNumber(:)")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:GetLocalNodeNumber]]

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber1(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! local node number
  END FUNCTION obj_GetLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the local numbers of global nodes
!
! Implementation of AbstractDomain_ GetLocalNodeNumber for FEDomain_. It uses
! the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(SIZE(globalNode)) : INTEGER(I4B) - local node numbers
!
! See also: [[AbstractDomain_:GetLocalNodeNumber]]

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(SIZE(globalNode))
    !! local node numbers
  END FUNCTION obj_GetLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local numbers of global elements
!
! Implementation of AbstractDomain_ GetLocalElemNumber for FEDomain_. It uses
! the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement(:) : INTEGER(I4B), intent(in) - global element numbers
!     (local if islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(SIZE(globalElement)) : INTEGER(I4B) - local element numbers
!
! See also: [[AbstractDomain_:GetLocalElemNumber]]

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global element numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(SIZE(globalElement))
    !! local element numbers
  END FUNCTION obj_GetLocalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local number of a global element
!
! Implementation of AbstractDomain_ GetLocalElemNumber for FEDomain_. It uses
! the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - local element number
!
! See also: [[AbstractDomain_:GetLocalElemNumber]]

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber2(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! local element number
  END FUNCTION obj_GetLocalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalEdgeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the global edge number of a local edge
!
! Implementation of AbstractDomain_ GetGlobalEdgeNumber for FEDomain_. It uses
! the mesh of dimension nsd. The edges are numbered by
! InitiateEdgeConnectivity of the mesh.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localEdgeNumber : INTEGER(I4B), intent(in) - local edge number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - global edge number
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetGlobalFaceNumber(globalElement=13_I4B, &
!                                   localFaceNumber=1_I4B) .EQ. 1_I4B, &
!           "GetGlobalFaceNumber")
!   mesh => obj%GetMeshPointer()
!   CALL mesh%InitiateEdgeConnectivity()
!   CALL OK(obj%GetGlobalEdgeNumber(globalElement=13_I4B, &
!                                   localEdgeNumber=2_I4B) .EQ. &
!           obj%GetGlobalEdgeNumber(globalElement=14_I4B, &
!                                   localEdgeNumber=1_I4B), &
!           "GetGlobalEdgeNumber")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:GetGlobalEdgeNumber]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalEdgeNumber(obj, globalElement, &
    localEdgeNumber, islocal) RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local edge number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! global edge number
  END FUNCTION obj_GetGlobalEdgeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalFaceNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-21
! summary: Get the global face number of a local face
!
! Implementation of AbstractDomain_ GetGlobalFaceNumber for FEDomain_. It uses
! the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localFaceNumber : INTEGER(I4B), intent(in) - local face number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - global face number
!
! See also: [[AbstractDomain_:GetGlobalFaceNumber]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalFaceNumber(obj, globalElement, &
    localFaceNumber, islocal) RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! global face number
  END FUNCTION obj_GetGlobalFaceNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetElemDataPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-18
! summary: Get a copy of the element data
!
! Implementation of AbstractDomain_ GetElemData for FEDomain_. It uses the
! mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   elemdata : TYPE(ElemData_), intent(inout) - copy of the element data
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ElemData_Class, ONLY: ElemData_
!   USE ElemData_Class, ONLY: Elemdata_globalElemNum
!   USE ElemData_Class, ONLY: Elemdata_localElemNum
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   TYPE(ElemData_) :: elemdata
!   TYPE(ElemData_), POINTER :: ptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%GetElemData(elemdata=elemdata, globalElement=15_I4B)
!   CALL OK(Elemdata_localElemNum(elemdata) .EQ. 3_I4B, "GetElemData")
!   ptr => obj%GetElemDataPointer(globalElement=16_I4B)
!   CALL OK(Elemdata_globalElemNum(ptr) .EQ. 16_I4B, &
!           "GetElemDataPointer")
!   ptr => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:GetElemData]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemData(obj, elemdata, globalElement, islocal)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    TYPE(ElemData_), INTENT(INOUT) :: elemdata
    !! copy of the element data
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetElemData
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetElemDataPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-18
! summary: Get a pointer to the element data
!
! Implementation of AbstractDomain_ GetElemDataPointer for FEDomain_. It uses
! the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : TYPE(ElemData_), pointer - pointer to the element data
!
! See also: [[AbstractDomain_:GetElemDataPointer]]

INTERFACE
  MODULE FUNCTION obj_GetElemDataPointer(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(FEDomain_), TARGET, INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    TYPE(ElemData_), POINTER :: ans
    !! pointer to the element data
  END FUNCTION obj_GetElemDataPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalEntitiesList@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-23
! summary: Get the number of entities of an element
!
! Implementation of AbstractDomain_ GetTotalEntitiesList for FEDomain_. It
! uses the mesh of dimension nsd. It returns the number of nodes, edges, faces
! and 1 of an element.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(4) : INTEGER(I4B) - nodes, edges, faces, 1
!
! See also: [[AbstractDomain_:GetTotalEntitiesList]]

INTERFACE
  MODULE FUNCTION obj_GetTotalEntitiesList(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(4)
    !! nodes, edges, faces, 1
  END FUNCTION obj_GetTotalEntitiesList
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Get the connectivity of an element
!
! Call GetConnectivity_ of the mesh of dimension dim (default nsd).
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   ans(:) : INTEGER(I4B), intent(inout) - connectivity, size must be at least
!     tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written
!   opt : CHARACTER(*), optional, intent(in) - connectivity: V vertex
!     (default), E edges, F faces, C cell, A all
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number; not used
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   INTEGER(I4B), ALLOCATABLE :: con(:)
!   INTEGER(I4B) :: buf(20), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   con = obj%GetConnectivity(globalElement=14_I4B)
!   CALL OK(ALL(con .EQ. [7, 9, 6, 3]), "element 14")
!   con = obj%GetConnectivity(globalElement=6_I4B, dim=1_I4B)
!   CALL OK(ALL(con .EQ. [5, 2]), "line element 6")
!   CALL OK(obj%GetNNE(13_I4B) .EQ. 4_I4B, "GetNNE")
!   CALL OK(obj%GetNNE(6_I4B, dim=1_I4B) .EQ. 2_I4B, "GetNNE dim=1")
!   CALL obj%GetConnectivity_(globalElement=13_I4B, ans=buf, &
!                             tsize=tsize, opt="V")
!   CALL OK(ALL(buf(1:tsize) .EQ. [1, 5, 9, 7]), "GetConnectivity_")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:GetConnectivity_]]

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity1_(obj, globalElement, ans, tsize, &
    opt, dim, entityNum, islocal)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! connectivity, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written
    CHARACTER(*), INTENT(IN), OPTIONAL :: opt
    !! connectivity: V vertex (default), E edges, F faces, C cell, A all
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number; not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetConnectivity1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-14
! summary: Get all connectivities of an element
!
! Call GetConnectivity_ of the mesh of dimension dim (default nsd).
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   cellCon(:) : INTEGER(I4B), intent(inout) - cell number of the element
!   faceCon(:) : INTEGER(I4B), intent(inout) - global face numbers of the
!     element
!   edgeCon(:) : INTEGER(I4B), intent(inout) - global edge numbers of the
!     element
!   nodeCon(:) : INTEGER(I4B), intent(inout) - global node numbers of the
!     element
!   tCellCon : INTEGER(I4B), intent(out) - number of entries written in
!     cellCon
!   tFaceCon : INTEGER(I4B), intent(out) - number of entries written in
!     faceCon
!   tEdgeCon : INTEGER(I4B), intent(out) - number of entries written in
!     edgeCon
!   tnodeCon : INTEGER(I4B), intent(out) - number of entries written in
!     nodeCon
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number; not used
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[AbstractDomain_:GetConnectivity_]]

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity2_(obj, cellCon, faceCon, edgeCon, &
    nodeCon, tCellCon, tFaceCon, &
    tEdgeCon, tnodeCon, globalElement, &
    dim, entityNum, islocal)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(INOUT) :: cellCon(:)
    !! cell number of the element
    INTEGER(I4B), INTENT(INOUT) :: faceCon(:)
    !! global face numbers of the element
    INTEGER(I4B), INTENT(INOUT) :: edgeCon(:)
    !! global edge numbers of the element
    INTEGER(I4B), INTENT(INOUT) :: nodeCon(:)
    !! global node numbers of the element
    INTEGER(I4B), INTENT(OUT) :: tCellCon
    !! number of entries written in cellCon
    INTEGER(I4B), INTENT(OUT) :: tFaceCon
    !! number of entries written in faceCon
    INTEGER(I4B), INTENT(OUT) :: tEdgeCon
    !! number of entries written in edgeCon
    INTEGER(I4B), INTENT(OUT) :: tnodeCon
    !! number of entries written in nodeCon
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number; not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetConnectivity2_
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalVertexNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Get the number of vertex nodes
!
! Use the mesh of dimension dim (default nsd): with globalElement return the
! vertex nodes of the element, with entityNum those of the entity, else those
! of the mesh.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), optional, intent(in) - global element number
!     (local if islocal is true)
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex nodes
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   INTEGER(I4B) :: ent(4)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetTotalVertexNodes() .EQ. 9_I4B, "mesh")
!   CALL OK(obj%GetTotalVertexNodes(globalElement=13_I4B) .EQ. 4_I4B, &
!           "element 13")
!   CALL OK(obj%GetTotalVertexNodes(globalElement=[13_I4B, 14_I4B]) &
!           .EQ. 6_I4B, "elements 13 and 14")
!   ent = obj%GetTotalEntitiesList(globalElement=13_I4B)
!   CALL OK(ent(1) .EQ. 4_I4B .AND. ent(4) .EQ. 1_I4B, &
!           "GetTotalEntitiesList")
!   CALL OK(obj%GetElemTopologyIndx(13_I4B) .EQ. 4_I4B, &
!           "GetElemTopologyIndx")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:GetTotalVertexNodes]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes1(obj, globalElement, dim, &
    entityNum, islocal) RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: globalElement
    !! global element number (local if islocal is true)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of vertex nodes
  END FUNCTION obj_GetTotalVertexNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Get the number of vertex nodes of a list of elements
!
! Use the mesh of dimension dim (default nsd).
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(in) - domain object
!   globalElement(:) : INTEGER(I4B), intent(in) - global element numbers
!     (local if islocal is true)
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number; not used
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex nodes of the elements
!
! See also: [[AbstractDomain_:GetTotalVertexNodes]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes2(obj, globalElement, dim, &
    entityNum, islocal) RESULT(ans)
    CLASS(FEDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global element numbers (local if islocal is true)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number; not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of vertex nodes of the elements
  END FUNCTION obj_GetTotalVertexNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the node to elements data of the meshes
!
! Call InitiateNodeToElements of every mesh of the domain (volume, surface,
! curve, point).
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%InitiateNodeToElements()
!   mesh => obj%GetMeshPointer(dim=2_I4B)
!   CALL OK(mesh%IsNodeToElements(), "surface mesh")
!   mesh => obj%GetMeshPointer(dim=1_I4B)
!   CALL OK(mesh%IsNodeToElements(), "curve mesh")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:InitiateNodeToElements]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToElements(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the node to nodes data of the meshes
!
! Call InitiateNodeToNodes of every mesh of the domain (volume, surface,
! curve, point).
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%InitiateNodeToNodes()
!   mesh => obj%GetMeshPointer(dim=2_I4B)
!   CALL OK(mesh%IsNodeToNodes(), "surface mesh")
!   mesh => obj%GetMeshPointer(dim=1_I4B)
!   CALL OK(mesh%IsNodeToNodes(), "curve mesh")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:InitiateNodeToNodes]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToNodes(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the element to elements data of the meshes
!
! Call InitiateElementToElements of every mesh of the domain (volume, surface,
! curve, point).
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%InitiateElementToElements()
!   mesh => obj%GetMeshPointer(DIM=2_I4B)
!   CALL OK(mesh%IsElementToElements(), "surface mesh")
!   mesh => obj%GetMeshPointer(DIM=1_I4B)
!   CALL OK(mesh%IsElementToElements(), "curve mesh")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:InitiateElementToElements]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateElementToElements(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                      InitiateFacetElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the facet elements of the meshes
!
! Call InitiateFacetElements of every mesh of the domain (volume, surface,
! curve, point). Point and curve meshes have no facet elements.
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%InitiateFacetElements()
!   mesh => obj%GetMeshPointer(dim=2_I4B)
!   CALL OK(mesh%IsFacetData(), "surface mesh")
!   CALL OK(mesh%GetTotalFacetElements() .EQ. 12_I4B, &
!           "facet elements of the surface mesh")
!   mesh => obj%GetMeshPointer(dim=1_I4B)
!   CALL OK(.NOT. mesh%IsFacetData(), "no facet data in the curve mesh")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:InitiateFacetElements]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetElements(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateExtraNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the extra node to nodes data of the meshes
!
! Call InitiateExtraNodeToNodes of every mesh of the domain (volume, surface,
! curve, point).
!
! Arguments:
!   obj : CLASS(FEDomain_), intent(inout) - domain object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%InitiateExtraNodeToNodes()
!   mesh => obj%GetMeshPointer(dim=2_I4B)
!   CALL OK(mesh%IsExtraNodeToNodes(), "surface mesh")
!   mesh => obj%GetMeshPointer(dim=1_I4B)
!   CALL OK(mesh%IsExtraNodeToNodes(), "curve mesh")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractDomain_:InitiateExtraNodeToNodes]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateExtraNodeToNodes(obj)
    CLASS(FEDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateExtraNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEDomain_Class
