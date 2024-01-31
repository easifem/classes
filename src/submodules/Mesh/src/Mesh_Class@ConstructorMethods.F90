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

SUBMODULE(Mesh_Class) ConstructorMethods
USE ReferenceElement_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (Mesh_ :: ans)
CALL ans%Initiate(hdf5=hdf5, group=group)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ii

CALL AbstractMeshDeallocate(obj)
obj%xidim = 0
obj%elemType = 0

IF (ALLOCATED(obj%facetElements)) THEN
  DO ii = 1, SIZE(obj%facetElements)
    CALL DEALLOCATE (obj%facetElements(ii))
  END DO
  DEALLOCATE (obj%facetElements)
END IF

obj%refelem => NULL()
END PROCEDURE obj_Deallocate

END SUBMODULE ConstructorMethods
