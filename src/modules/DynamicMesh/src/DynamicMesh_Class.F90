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

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! summary: [[Mesh_Class]] module contains data type for handling the mesh.

MODULE DynamicMesh_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE ElemData_Class
USE ElemDataList_Class
USE NodeData_Class
USE NodeDataBinaryTree_Class
USE AbstractMesh_Class

IMPLICIT NONE
PRIVATE

PUBLIC :: DynamicMesh_
PUBLIC :: DynamicMeshPointer_

CHARACTER(*), PARAMETER :: modName = "Mesh_Class"

!----------------------------------------------------------------------------
!                                                                     Mesh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-28
! summary: Dynamic mesh
!
!{!pages/docs-api/DynamicMesh/DynamicMesh_.md!}

TYPE, EXTENDS(AbstractMesh_) :: DynamicMesh_
  TYPE(ElemDataList_) :: elementDataList
  !! element data
  TYPE(NodeDataBinaryTree_) :: nodeDataBinaryTree
  !! node data
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
    !! Deallocate memory occupied by the mesh instance
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
    !! Read mesh from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! SET:
  ! @NodeDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    & obj_InitiateNodeToElements
  !! Initiate node to node data
  ! PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
  !   & obj_InitiateNodetoNodes
  ! !! Initiate Node to nodes mapping
  ! PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
  !   & obj_InitiateExtraNodetoNodes
  ! !! Initiate Node to nodes mapping

END TYPE DynamicMesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Sept 2021
! summary: Userdefine datatype which contains the pointer to a mesh

TYPE :: DynamicMeshPointer_
  TYPE(DynamicMeshPointer_), POINTER :: Ptr => NULL()
END TYPE DynamicMeshPointer_

!----------------------------------------------------------------------------
!                                                    Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-29
! summary: Free up the memory stored

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(DynamicMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  Finalizer

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(DynamicMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Reads the mesh from a meshFile which is an hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(DynamicMesh_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  Display the content of the mesh

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(DynamicMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message on screen
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! unit number of ouput file
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateNodeToElements@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-29
! summary: generate Elements surrounding a node mapping

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToElements(obj)
    CLASS(DynamicMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DynamicMesh_Class
