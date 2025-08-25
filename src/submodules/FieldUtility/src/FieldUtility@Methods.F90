! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(FieldUtility) Methods
USE Display_Method, ONLY: ToString, Display

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     VectorFieldSetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorFieldSetNodeCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VectorFieldSetNodeCoord()"
#endif

LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .TRUE.
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, jj, minNodeNum, maxNodeNum, tsize, temp(1), nsd
REAL(DFP) :: xij(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

minNodeNum = mesh%GetMinNodeNumber()
maxNodeNum = mesh%GetMaxNodeNumber()
temp = obj%GetSpaceCompo(tPhysicalVars=1)
nsd = temp(1)

DO ii = minNodeNum, maxNodeNum
  isok = mesh%IsNodePresent(ii)
  IF (.NOT. isok) CYCLE

  jj = mesh%GetLocalNodeNumber(globalNode=ii, islocal=no)

  CALL mesh%GetNodeCoord(globalNode=jj, islocal=yes, nodeCoord=xij, &
                         tsize=tsize)

  CALL obj%Set(globalNode=jj, islocal=yes, VALUE=xij(1:nsd))
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VectorFieldSetNodeCoord

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
