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

SUBMODULE(AssemblePointSourceUtility) ScalarFieldMethods
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate

CONTAINS

!----------------------------------------------------------------------------
!                                              ScalarFieldAssemblePointSource
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssemblePointSource1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssemblePointSource1()"
#endif

CLASS(NeumannBC_), POINTER :: nbcptr
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tnbc, inbc, ii, tsize, jj, totalNodes, iNodeOnNode, &
                iNodeOnEdge, iNodeOnFace
REAL(DFP), ALLOCATABLE :: nodalValue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodeNum(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tnbc = SIZE(nbc)

totalNodes = 0
DO inbc = 1, tnbc
  nbcptr => nbc(inbc)%ptr
  isok = ASSOCIATED(nbcptr)
  IF (.NOT. isok) CYCLE
  ii = nbcptr%GetTotalNodeNum(fedof=fedof)
  totalNodes = MAX(totalNodes, ii)
END DO

CALL Reallocate(nodeNum, totalNodes)
CALL Reallocate(nodalValue, totalNodes, 1)

DO inbc = 1, tnbc
  nbcptr => nbc(inbc)%ptr
  isok = ASSOCIATED(nbcptr)
  IF (.NOT. isok) CYCLE

  ! CALL nbcptr%GetParam(isSelectionByNodeNum=isok)
  ! IF (.NOT. isok) CYCLE

  CALL nbcptr%Get(nodeNum=nodeNum, tsize=tsize, fedof=fedof, &
                  iNodeOnNode=iNodeOnNode, iNodeOnEdge=iNodeOnEdge, &
                  iNodeOnFace=iNodeOnFace)

  CALL nbcptr%Get(fedof=fedof, nodeNum=nodeNum(1:tsize), &
                  nodalValue=nodalValue, nrow=ii, ncol=jj)

  CALL rhs%Set(globalNode=nodeNum(1:ii), VALUE=nodalValue(1:ii, 1), &
               scale=scale, addContribution=defaultOpt%yes, &
               islocal=defaultOpt%yes)
END DO

NULLIFY (nbcptr)
DEALLOCATE (nodalValue, nodeNum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE ScalarFieldAssemblePointSource1

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ScalarFieldMethods
