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

SUBMODULE(FEMesh_Class) ConstructorMethods
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
ALLOCATE (FEMesh_ :: ans)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                               FEMeshPointerDeallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE FEMeshPointerDeallocate
CLASS(FEMesh_), POINTER :: meshObj
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

meshObj => NULL()
IF (ALLOCATED(obj)) THEN
  tsize = SIZE(obj)

  DO ii = 1, tsize

    meshObj => obj(ii)%ptr
    isok = ASSOCIATED(meshObj)
    IF (isok) THEN
      CALL meshobj%DEALLOCATE()
      meshObj => NULL()
    END IF

  END DO

  DEALLOCATE (obj)
END IF
END PROCEDURE FEMeshPointerDeallocate

END SUBMODULE ConstructorMethods
