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

SUBMODULE(AbstractMesh_Class) SetMethods
USE BoundingBox_Method
USE ReallocateUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetShowTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetShowTime
obj%showTime = VALUE
END PROCEDURE obj_SetShowTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetBoundingBox1
obj%minX = (.Xmin.box)
obj%minY = (.Ymin.box)
obj%minZ = (.Zmin.box)
obj%maxX = (.Xmax.box)
obj%maxY = (.Ymax.box)
obj%maxZ = (.Zmax.box)
END PROCEDURE obj_SetBoundingBox1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetBoundingBox2
TYPE(BoundingBox_) :: box
Box = obj%GetBoundingBox(nodes=nodes, local_nptrs=local_nptrs)
CALL obj%SetBoundingBox(box=box)
CALL DEALLOCATE (box)
END PROCEDURE obj_SetBoundingBox2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity1()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity3()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity4
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity4()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetSparsity4

!----------------------------------------------------------------------------
!                                                           setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalMaterial
INTEGER(I4B), ALLOCATABLE :: temp_material(:)
INTEGER(I4B) :: n0

IF (ALLOCATED(obj%material)) THEN
  n0 = SIZE(obj%material)
  CALL Reallocate(temp_material, n0 + n)
  temp_material(1:n0) = obj%material(1:n0)
  CALL MOVE_ALLOC(from=temp_material, to=obj%material)
  RETURN
END IF

CALL Reallocate(obj%material, n)
END PROCEDURE obj_SetTotalMaterial

!----------------------------------------------------------------------------
!                                                                setMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial
obj%material(medium) = material
END PROCEDURE obj_SetMaterial

!----------------------------------------------------------------------------
!                                                        setFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFacetElementType
INTEGER(I4B) :: localElem
localElem = obj%GetLocalElemNumber(globalElement=globalElement)
obj%facetElementType(iface, localElem) = facetElementType
obj%elementData(localElem)%elementType = facetElementType
END PROCEDURE obj_SetFacetElementType

!----------------------------------------------------------------------------
!                                                           setQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuality
CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetQuality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
