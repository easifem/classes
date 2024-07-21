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
! date: 2024-04-16
! summary: This submodule contains methods for domain object

SUBMODULE(Domain_Class) ConstructorMethods
USE AbstractMesh_Class, ONLY: AbstractMeshPointerDeallocate
USE CSRSparsity_Method, ONLY: CSRSparsity_Deallocate => DEALLOCATE
USE MeshFacetData_Class, ONLY: MeshFacetDataDeallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL AbstractDomainDeallocate(obj)
CALL AbstractMeshPointerDeallocate(obj%meshVolume)
CALL AbstractMeshPointerDeallocate(obj%meshSurface)
CALL AbstractMeshPointerDeallocate(obj%meshCurve)
CALL AbstractMeshPointerDeallocate(obj%meshPoint)
CALL CSRSparsity_Deallocate(obj%meshmap)
IF (ALLOCATED(obj%meshFacetData)) &
  CALL MeshFacetDataDeallocate(obj%meshFacetData)
IF (ALLOCATED(obj%local_nptrs)) DEALLOCATE (obj%local_nptrs)
IF (ALLOCATED(obj%global_nptrs)) DEALLOCATE (obj%global_nptrs)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                              Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL Obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                            obj_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (Domain_ :: ans)
CALL ans%Initiate(hdf5=hdf5, group=group)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE ConstructorMethods
