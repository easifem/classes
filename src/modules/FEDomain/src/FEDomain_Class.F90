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
! update:
!   - 12 Nov 2021
!   - 4 Nov 2022
! summary: This module contains methods for domain data type

MODULE FEDomain_Class
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE HDF5File_Class, ONLY: HDF5File_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE
PRIVATE

PUBLIC :: FEDomain_
PUBLIC :: FEDomainPointer_
PUBLIC :: FEDomain_Pointer

CHARACTER(*), PARAMETER :: modName = "FEDomain_Class"

!----------------------------------------------------------------------------
!                                                                   FEDomain_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: FEDomain_ contains finite element mesh data of a domain
!
!{!pages/docs-api/FEDomain/FEDomain_.md!}

TYPE, EXTENDS(AbstractDomain_) :: FEDomain_
END TYPE FEDomain_

!----------------------------------------------------------------------------
!                                                             FEDomainPointer
!----------------------------------------------------------------------------

TYPE :: FEDomainPointer_
  CLASS(FEDomain_), POINTER :: ptr => NULL()
END TYPE FEDomainPointer_

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Finalizer

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(FEDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                        FEDomain_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: This function returns pointer to a newly constructed FEDomain obj

INTERFACE FEDomain_Pointer
  MODULE FUNCTION obj_Constructor_1(hdf5, group) RESULT(ans)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDomain_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE FEDomain_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEDomain_Class
