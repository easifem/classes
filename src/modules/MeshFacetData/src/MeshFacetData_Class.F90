! This program is a part of EASIFEM library
! Copyright (C) (Since 2000)  Vikas Sharma, Ph.D
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

MODULE MeshFacetData_Class
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "MeshFacetData_Class"

PUBLIC :: MeshFacetData_
PUBLIC :: MeshFacetDataDeallocate

!----------------------------------------------------------------------------
!                                                             MeshFacetData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Data storage for mesh-facets
!
!# Introduction
!
! Mesh facet elements are located on mesh boundary which is connected to
! other mesh region.
!
! In this way, the `slaveCell` of a `meshFacet` is inside some other mesh.
! The information of `slaveCell` number will be accessed through the
! Halo of the mesh.
!
! The `halo` of the mesh will be stored inside the instance of `Mesh_`
!
! For each Halo (neighbouring mesh) we have an instance of obj_.
! therefore, I have defined obj_ as the collection of
! all meshfacets.

TYPE MeshFacetData_
  INTEGER(I4B) :: masterMesh = 0
  INTEGER(I4B) :: slaveMesh = 0
  INTEGER(I4B), ALLOCATABLE :: masterCellNumber(:)
  INTEGER(I4B), ALLOCATABLE :: slaveCellNumber(:)
  INTEGER(I4B), ALLOCATABLE :: masterLocalFacetID(:)
  INTEGER(I4B), ALLOCATABLE :: slaveLocalFacetID(:)
  ! CLASS( Halo_ ), POINTER :: halo => NULL()
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate1
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: isInitiated => obj_isInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  ! PROCEDURE, PUBLIC, PASS( obj ) :: Set => MeshFacet_Set
  ! PROCEDURE, PUBLIC, PASS( obj ) :: Size => MeshFacet_Size
  ! PROCEDURE, PUBLIC, PASS( obj ) :: SetSlaveCellNumber => &
  !   & MeshFacet_SetSlaveCellNumber
  ! PROCEDURE, PUBLIC, PASS( obj ) :: SetSlaveLocalFacetID => &
  !   & MeshFacet_SetSlaveLocalFacetID
  ! PROCEDURE, PUBLIC, PASS( obj ) :: SetSlaveData => &
  !   & MeshFacet_SetSlaveData
  ! !!
END TYPE MeshFacetData_

!----------------------------------------------------------------------------
!                                                       Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-16
! summary:  Deallocate the data stored in obj_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate1(obj)
    CLASS(MeshFacetData_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-16
! summary:  Deallocate the data stored in obj_

INTERFACE MeshFacetDataDeallocate
  MODULE SUBROUTINE obj_Deallocate2(obj)
    TYPE(MeshFacetData_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate2
END INTERFACE MeshFacetDataDeallocate

!----------------------------------------------------------------------------
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Initiate an instance of MeshFacetData

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, n)
    CLASS(MeshFacetData_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: Returns true if MeshFacetData initiated

INTERFACE
  MODULE FUNCTION obj_isInitiated(obj) RESULT(ans)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: Returns the size of MeshFacetData

INTERFACE
  MODULE FUNCTION obj_Size(obj) RESULT(ans)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display mesh facet data

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

END MODULE MeshFacetData_Class
