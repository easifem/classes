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
! date:         9 June 2021
! summary:         This Module contains data-structure and methods to deal with MSH4 format of Gmsh.

MODULE mshEntity_Class
USE BaseType
USE GlobalData
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "mshEntity_Class"

!----------------------------------------------------------------------------
!                                                                 mshEntity_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary: This class handles the mesh entities defined in msh file

TYPE :: mshEntity_
  PRIVATE
  INTEGER(I4B) :: uid = 0
    !! unique id of entity
  INTEGER(I4B) :: xiDim = 0
    !! for point=0, curve=1, surface = 2, volume = 3
  INTEGER(I4B) :: elemType = 0
    !! element type in meshing
  INTEGER(I4B), ALLOCATABLE :: physicalTag(:)
    !! Physical tags associated
  INTEGER(I4B), ALLOCATABLE :: intNodeNumber(:)
    !! node numbers in mesh
  INTEGER(I4B), ALLOCATABLE :: elemNumber(:)
    !! element numbers in mesh
  INTEGER(I4B), ALLOCATABLE :: connectivity(:, :)
    !! connectivity
  INTEGER(I4B), ALLOCATABLE :: boundingEntity(:)
    !! tag of bounding entity
  REAL(DFP) :: minX = 0.0_DFP
    !! bounding box of entity
  REAL(DFP) :: minY = 0.0_DFP
    !! bounding box of entity
  REAL(DFP) :: minZ = 0.0_DFP
    !! bounding box of entity
  REAL(DFP) :: maxX = 0.0_DFP
    !! bounding box of entity
  REAL(DFP) :: maxY = 0.0_DFP
    !! bounding box of entity
  REAL(DFP) :: maxZ = 0.0_DFP
    !! bounding box of entity
  REAL(DFP) :: x = 0.0_DFP
    !! used only for point entity
  REAL(DFP) :: y = 0.0_DFP
    !! used only for point entity
  REAL(DFP) :: z = 0.0_DFP
    !! used only for point entity
  REAL(DFP), ALLOCATABLE :: nodeCoord(:, :)
    !! nodal coordinates in xiJ format

CONTAINS
  PRIVATE
  FINAL :: ent_Final
      !! Finalizer
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => ent_Deallocate
      !! To deallocate data
  PROCEDURE, PUBLIC, PASS(obj) :: GotoTag => ent_GotoTag
      !! To find tag
  PROCEDURE, PUBLIC, PASS(obj) :: WRITE => ent_Write
      !! Write data to a file
  PROCEDURE, PUBLIC, PASS(Obj) :: Display => ent_Display
      !! Display the content
  PROCEDURE, PUBLIC, PASS(Obj) :: READ => ent_Read
      !! Read Point, Curve, Surface, and Volume Entity
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalPhysicalTags => ent_getTotalPhysicalTags
      !! Return total physical tags associated
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalBoundingTags => ent_getTotalBoundingTags
      !! Returns the total bounding tags
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalElements => ent_getTotalElements
      !! Returns the total elements
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalIntNodes => ent_getTotalIntNodes
      !! Returns the total Nodes
  PROCEDURE, PUBLIC, PASS(Obj) :: getPhysicalTag => ent_getPhysicalTag
      !! Returns the physical tags
  PROCEDURE, PUBLIC, PASS(Obj) :: setIntNodeNumber => ent_setIntNodeNumber
      !! Set Node number
  PROCEDURE, PUBLIC, PASS(Obj) :: setNodeCoord => ent_setNodeCoord
      !! Set Node coord
  PROCEDURE, PUBLIC, PASS(Obj) :: setElemType => ent_setElemType
  PROCEDURE, PUBLIC, PASS(Obj) :: setElemNumber => ent_setElemNumber
  PROCEDURE, PUBLIC, PASS(Obj) :: setConnectivity => ent_setConnectivity
  PROCEDURE, PUBLIC, PASS(Obj) :: getUid => ent_getUid
  PROCEDURE, PUBLIC, PASS(Obj) :: getXiDim => ent_getXiDim
  PROCEDURE, PUBLIC, PASS(Obj) :: getElemType => ent_getElemType
  PROCEDURE, PUBLIC, PASS(Obj) :: getMinX => ent_getMinX
  PROCEDURE, PUBLIC, PASS(Obj) :: getMinY => ent_getMinY
  PROCEDURE, PUBLIC, PASS(Obj) :: getMinZ => ent_getMinZ
  PROCEDURE, PUBLIC, PASS(Obj) :: getMaxX => ent_getMaxX
  PROCEDURE, PUBLIC, PASS(Obj) :: getMaxY => ent_getMaxY
  PROCEDURE, PUBLIC, PASS(Obj) :: getMaxZ => ent_getMaxZ
  PROCEDURE, PUBLIC, PASS(Obj) :: getX => ent_getX
  PROCEDURE, PUBLIC, PASS(Obj) :: getY => ent_getY
  PROCEDURE, PUBLIC, PASS(Obj) :: getZ => ent_getZ
  PROCEDURE, PUBLIC, PASS(Obj) :: getNodeCoord => ent_getNodeCoord
  PROCEDURE, PUBLIC, PASS(Obj) :: getIntNodeNumber => ent_getIntNodeNumber
  PROCEDURE, PUBLIC, PASS(Obj) :: getElemNumber => ent_getElemNumber
  PROCEDURE, PUBLIC, PASS(Obj) :: getBoundingEntity => ent_getBoundingEntity
  PROCEDURE, PASS(Obj) :: ent_getConnectivity_a, ent_getConnectivity_b
    GENERIC, PUBLIC :: getConnectivity => ent_getConnectivity_a, ent_getConnectivity_b
END TYPE mshEntity_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: mshEntity_

TYPE(mshEntity_), PUBLIC, PARAMETER :: &
  & TypeMshEntity = &
  & mshEntity_( &
    & PhysicalTag=NULL(), &
    & IntNodeNumber=NULL(), &
    & ElemNumber=NULL(), &
    & Connectivity=NULL(), &
    & NodeCoord=NULL(), &
    & BoundingEntity=NULL())

!----------------------------------------------------------------------------
!                                                          mshEntityPointer_
!----------------------------------------------------------------------------

TYPE :: mshEntityPointer_
  CLASS(mshEntity_), POINTER :: Ptr => NULL()
END TYPE mshEntityPointer_

PUBLIC :: mshEntityPointer_

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE ent_Final(obj)
    TYPE(mshEntity_), INTENT(INOUT) :: obj
  END SUBROUTINE ent_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary: This subroutine deallocate the data from [[mshentity_]]

INTERFACE
  MODULE SUBROUTINE ent_Deallocate(obj)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
  END SUBROUTINE ent_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE ent_Deallocate
END INTERFACE DEALLOCATE

PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                         GotoTag
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary: This subroutine finds the tag in the mesh file

INTERFACE
  MODULE SUBROUTINE ent_GotoTag(obj, mshFile, error)
    CLASS(mshEntity_), INTENT(IN) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    INTEGER(I4B), INTENT(INOUT) :: error
  END SUBROUTINE ent_GotoTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary: This subroutine write the data to a file

INTERFACE
  MODULE SUBROUTINE ent_Write(obj, mshFile, StartStr, EndStr)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    CHARACTER(*), INTENT(IN), OPTIONAL :: StartStr, EndStr
  END SUBROUTINE ent_Write
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary:         This subroutine writes the content of [[mshEntity_]]

INTERFACE
  MODULE SUBROUTINE ent_Display(obj, Msg, UnitNo)
    CLASS(mshEntity_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE ent_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE ent_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 June 2021
! summary: Read Entities from msh file

INTERFACE
  MODULE SUBROUTINE ent_Read(obj, mshFile, dim, readTag, error)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    INTEGER(I4B), INTENT(IN) :: dim
    LOGICAL(LGT), INTENT(IN) :: readTag
    INTEGER(I4B), INTENT(INOUT) :: error
  END SUBROUTINE ent_Read
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ReadPointEntity
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary: This subroutine reads the entry for point entity

INTERFACE
  MODULE SUBROUTINE ReadPointEntity(obj, mshFile, readTag, error)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    INTEGER(I4B), INTENT(INOUT) :: error
    LOGICAL(LGT), INTENT(IN) :: readTag
  END SUBROUTINE ReadPointEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ReadCurveEntity
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary:         This subroutine reads the entry for curve entity

INTERFACE
  MODULE SUBROUTINE ReadCurveEntity(obj, mshFile, readTag, error)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    INTEGER(I4B), INTENT(INOUT) :: error
    LOGICAL(LGT), INTENT(IN) :: readTag
  END SUBROUTINE ReadCurveEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                               ReadSurfaceEntity
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary:         This subroutine reads the entry for surface entity

INTERFACE
  MODULE SUBROUTINE ReadSurfaceEntity(obj, mshFile, readTag, error)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    INTEGER(I4B), INTENT(INOUT) :: error
    LOGICAL(LGT), INTENT(IN) :: readTag
  END SUBROUTINE ReadSurfaceEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                ReadVolumeEntity
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary:         This subroutine reads the entry for volume entity

INTERFACE
  MODULE SUBROUTINE ReadVolumeEntity(obj, mshFile, readTag, error)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    INTEGER(I4B), INTENT(INOUT) :: error
    LOGICAL(LGT), INTENT(IN) :: readTag
  END SUBROUTINE ReadVolumeEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary:          This function finds the index of a tag/uid in list of entities

INTERFACE
  MODULE PURE FUNCTION ent_getIndex(mshEntities, Uid) RESULT(ans)
    TYPE(mshEntity_), INTENT(IN) :: mshEntities(:)
    INTEGER(I4B), INTENT(IN) :: Uid
    INTEGER(I4B) :: ans
  END FUNCTION ent_getIndex
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE ent_getIndex
END INTERFACE getIndex

PUBLIC :: getIndex

!----------------------------------------------------------------------------
!                                                getTotalPhysicalTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary:          This function returns the total number of physical tags in entity

INTERFACE
  MODULE PURE FUNCTION ent_getTotalPhysicalTags(obj) RESULT(ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ent_getTotalPhysicalTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                getTotalBoundingTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary: This function returns the total number of bounding tags in entity

INTERFACE
  MODULE PURE FUNCTION ent_getTotalBoundingTags(obj) RESULT(ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ent_getTotalBoundingTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalElements
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary: This function returns the total number of elements in entity

INTERFACE
  MODULE PURE FUNCTION ent_getTotalElements(obj) RESULT(ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ent_getTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getTotalNodes
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         10 June 2021
! summary: This function returns the total number of nodes in entity

INTERFACE
  MODULE PURE FUNCTION ent_getTotalIntNodes(obj) RESULT(ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ent_getTotalIntNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getPhysicalTag
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getPhysicalTag(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION ent_getPhysicalTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                            setIntNodeNumber
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE ent_setIntNodeNumber(obj, IntNodeNumber)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: IntNodeNumber(:)
  END SUBROUTINE ent_setIntNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                             setNodeCoord
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE ent_setNodeCoord(obj, NodeCoord)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: NodeCoord(:, :)
  END SUBROUTINE ent_setNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                                setElemType
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE ent_setElemType(obj, ElemType)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ElemType
  END SUBROUTINE ent_setElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                                             setElemNumber
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE ent_setElemNumber(obj, ElemNumber)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ElemNumber(:)
  END SUBROUTINE ent_setElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                            setConnectivity
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE ent_setConnectivity(obj, Connectivity)
    CLASS(mshEntity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Connectivity(:, :)
  END SUBROUTINE ent_setConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getUid
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getUid(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ent_getUid
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getXiDim
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getXidim(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ent_getXidim
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getElemType
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getElemType(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ent_getElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getMinX
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getMinX(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION ent_getMinX
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getMinY
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getMinY(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION ent_getMinY
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getMinZ
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getMinZ(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
    ! INTEGER(I4B) :: ans
  END FUNCTION ent_getMinZ
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getMaxX
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getMaxX(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
    ! INTEGER(I4B) :: ans
  END FUNCTION ent_getMaxX
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getMaxY
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getMaxY(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
    ! INTEGER(I4B) :: ans
  END FUNCTION ent_getMaxY
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getMaxZ
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getMaxZ(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
    ! INTEGER(I4B) :: ans
  END FUNCTION ent_getMaxZ
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getX
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getX(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
    ! INTEGER(I4B) :: ans
  END FUNCTION ent_getX
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getY
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getY(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
    ! INTEGER(I4B) :: ans
  END FUNCTION ent_getY
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   getZ
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getZ(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP) :: ans
    ! INTEGER(I4B) :: ans
  END FUNCTION ent_getZ
END INTERFACE

!----------------------------------------------------------------------------
!                                                              getNodeCoord
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getNodeCoord(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION ent_getNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getIntNodeNumber
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getIntNodeNumber(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION ent_getIntNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                              getElemNumber
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getElemNumber(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION ent_getElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getBoundingEntity
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getBoundingEntity(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION ent_getBoundingEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                            getConnectivity
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getConnectivity_a(obj, elemNum) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: elemNum
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION ent_getConnectivity_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                            getConnectivity
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ent_getConnectivity_b(obj) RESULT(Ans)
    CLASS(mshEntity_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: Ans(:, :)
  END FUNCTION ent_getConnectivity_b
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE mshEntity_Class
