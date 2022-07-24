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

SUBMODULE(MSHFile_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Final
  CALL obj%Deallocate()
END PROCEDURE msh_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Deallocate
  CALL obj%Format%Deallocate()
  CALL obj%PhysicalNames%Deallocate()
  CALL obj%Nodes%Deallocate()
  CALL obj%Elements%Deallocate()
  IF( ALLOCATED( obj%PointEntities ) ) DEALLOCATE( obj%PointEntities )
  IF( ALLOCATED( obj%CurveEntities ) ) DEALLOCATE( obj%CurveEntities )
  IF( ALLOCATED( obj%SurfaceEntities ) ) DEALLOCATE( obj%SurfaceEntities )
  IF( ALLOCATED( obj%VolumeEntities ) ) DEALLOCATE( obj%VolumeEntities )
  obj%nsd = 0
  IF( ASSOCIATED( obj % buffer ) ) DEALLOCATE( obj%buffer )
  NULLIFY( obj%buffer )
  CALL TxtFileDeallocate( obj, Delete )
END PROCEDURE msh_Deallocate

! !----------------------------------------------------------------------------
! !                                                                        msh4
! !----------------------------------------------------------------------------

! MODULE PROCEDURE msh_constuctor1
!   CALL ans%Initiate( Path, FileName, Extension, NSD )
! END PROCEDURE msh_constuctor1


END SUBMODULE Methods