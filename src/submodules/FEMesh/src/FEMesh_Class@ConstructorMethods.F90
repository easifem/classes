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

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "FEMesh_Class@ConstructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Final()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Constructor_1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ALLOCATE(FEMesh_ :: ans)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                               FEMeshPointerDeallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE FEMeshPointerDeallocate
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEMeshPointerDeallocate()"
#endif
  INTEGER(I4B) :: ii, tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (ALLOCATED(obj)) THEN
    tsize = SIZE(obj)

    DO ii = 1, tsize

      isok = ASSOCIATED(obj(ii)%ptr)
      IF (isok) THEN
        CALL obj(ii)%ptr%DEALLOCATE()
        ! free the mesh object; obj(ii)%ptr must own it
        DEALLOCATE(obj(ii)%ptr)
      END IF

    END DO

    DEALLOCATE(obj)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE FEMeshPointerDeallocate

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
