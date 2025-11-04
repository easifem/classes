! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(UP2DFEM_Class) ApplyDirichletBCMethods

USE Display_Method

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC()"
REAL(DFP) :: times(1)
LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')
isok = ALLOCATED(obj%dbc)

IF (isok) THEN
  times = obj%currentTime + &
          obj%timeStepSize(obj%currentTimeStep)
  CALL obj%sol%Set(VALUE=zero)
  CALL obj%sol%ApplyDirichletBC(dbc=obj%dbc, times=times)
  CALL obj%tanmat%ApplyDBCtoRHS(x=obj%sol, y=obj%rhs, &
                                scale=minus_one, addContribution=.TRUE.)
  CALL obj%rhs%ApplyDirichletBC(dbc=obj%dbc, times=times)
END IF

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_ApplyDirichletBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ApplyDirichletBCMethods
