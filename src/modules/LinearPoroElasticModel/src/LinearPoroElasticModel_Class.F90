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

!> authors: Vikas Sharma, Ph. D.
! date: 4 Oct 2021
! summary: Data type of linear elastic model

MODULE LinearPoroElasticModel_Class
USE LinearElasticModel_Class, ONLY: LinearElasticModel_

IMPLICIT NONE

PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "LinearPoroElasticModel_Class"
#endif

PUBLIC :: LinearPoroElasticModel_
PUBLIC :: LinearPoroElasticModelPointer_

!----------------------------------------------------------------------------
!                                                       LinearPoroElasticModel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Datatype for modeling Linear elastic behavior of solids

TYPE, EXTENDS(LinearElasticModel_) :: LinearPoroElasticModel_
END TYPE LinearPoroElasticModel_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: LinearPoroElasticModelPointer_
  CLASS(LinearPoroElasticModel_), POINTER :: ptr => NULL()
END TYPE LinearPoroElasticModelPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LinearPoroElasticModel_Class
