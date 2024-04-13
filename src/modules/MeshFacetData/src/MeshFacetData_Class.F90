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

CHARACTER(*), PARAMETER :: modName = "MeshFacetData_Class"
PRIVATE

PUBLIC :: MeshFacetData_

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
! For each Halo (neighbouring mesh) we have an instance of MeshFacetData_.
! therefore, I have defined MeshFacetData_ as the collection of
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
  PROCEDURE, PUBLIC, PASS(obj) :: Display => MeshFacetData_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => MeshFacetData_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: isInitiated => MeshFacetData_isInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: Size => MeshFacetData_Size
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
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Initiate an instance of MeshFacetData

INTERFACE
  MODULE SUBROUTINE MeshFacetData_Initiate(obj, n)
    CLASS(MeshFacetData_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE MeshFacetData_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: Returns true if MeshFacetData initiated

INTERFACE
  MODULE FUNCTION MeshFacetData_isInitiated(obj) RESULT(ans)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION MeshFacetData_isInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: Returns the size of MeshFacetData

INTERFACE
  MODULE FUNCTION MeshFacetData_Size(obj) RESULT(ans)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION MeshFacetData_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display mesh facet data

INTERFACE
  MODULE SUBROUTINE MeshFacetData_Display(obj, msg, unitno)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE MeshFacetData_Display
END INTERFACE

END MODULE MeshFacetData_Class
