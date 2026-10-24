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
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(FEDomain_Class) ConstructorMethods
USE AbstractDomain_Class, ONLY: AbstractDomainInitiate, &
  AbstractDomainDeallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL AbstractDomainDeallocate(obj=obj)

! obj%mesh points to one of the meshes below
  obj%mesh => NULL()

! The meshes are created by FEMesh_Pointer() in Import; the domain owns
! them, so they are freed here.

  IF (ASSOCIATED(obj%meshVolume)) THEN
    CALL obj%meshVolume%DEALLOCATE()
    DEALLOCATE(obj%meshVolume)
  END IF

  IF (ASSOCIATED(obj%meshSurface)) THEN
    CALL obj%meshSurface%DEALLOCATE()
    DEALLOCATE(obj%meshSurface)
  END IF

  IF (ASSOCIATED(obj%meshCurve)) THEN
    CALL obj%meshCurve%DEALLOCATE()
    DEALLOCATE(obj%meshCurve)
  END IF

  IF (ASSOCIATED(obj%meshPoint)) THEN
    CALL obj%meshPoint%DEALLOCATE()
    DEALLOCATE(obj%meshPoint)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                              Final
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
!                                                            FEDomain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Constructor_1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ALLOCATE(FEDomain_ :: ans)
  CALL ans%Initiate(hdf5=hdf5, group=group)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
