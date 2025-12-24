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

CALL AbstractDomainDeallocate(obj=obj)

obj%mesh => NULL()

IF (ASSOCIATED(obj%meshVolume)) THEN
  CALL obj%meshVolume%DEALLOCATE()
  obj%meshVolume => NULL()
END IF

IF (ASSOCIATED(obj%meshSurface)) THEN
  CALL obj%meshSurface%DEALLOCATE()
  obj%meshSurface => NULL()
END IF

IF (ASSOCIATED(obj%meshCurve)) THEN
  CALL obj%meshCurve%DEALLOCATE()
  obj%meshCurve => NULL()
END IF

IF (ASSOCIATED(obj%meshPoint)) THEN
  CALL obj%meshPoint%DEALLOCATE()
  obj%meshPoint => NULL()
END IF
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                              Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                            FEDomain_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (FEDomain_ :: ans)
CALL ans%Initiate(hdf5=hdf5, group=group)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
