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

SUBMODULE(FEDomainConnectivity_Class) CellMethods
! USE BaseMethod
USE IntegerUtility, ONLY: OPERATOR(.in.)
USE ReallocateUtility
USE Display_Method
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateCellToCellData1
CHARACTER(*), PARAMETER :: myName = "obj_InitiateCellToCellData1()"
INTEGER(I4B) :: ii, nsd, order1, order2, iel1, jj
! some counters and indices
! element numbers in mesh2
INTEGER(I4B), POINTER :: nodeToNode(:)
LOGICAL(LGT) :: isok
INTEGER(I4B) :: nptrs1(PARAM_MAX_NNE), nptrs2(PARAM_MAX_NNE), &
                nptrs3(PARAM_MAX_NNE), elem2(PARAM_MAX_NODE_TO_ELEM)

INTEGER(I4B) :: minelem, maxelem, telem2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER

isok = obj%isCellToCell
IF (isok) THEN
  CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "[INFO] :: It seems, obj%cellToCell data is already initiated")
  RETURN
END IF

isok = obj%isNodeToNode

IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & '[INFO] :: NodeToNode data is not initiated!')
  CALL obj%InitiateNodeToNodeData(domain1=domain1, domain2=domain2)
END IF

#endif

isok = obj%isCellToCell
IF (isok) RETURN

isok = obj%isNodeToNode
IF (.NOT. isok) &
  CALL obj%InitiateNodeToNodeData(domain1=domain1, domain2=domain2)

CALL domain1%GetParam(maxElemNum=maxelem, minElemNum=minelem)
CALL Reallocate(obj%cellToCell, maxelem)

obj%isCellToCell = .TRUE.
nsd = domain1%GetNSD()

nodeToNode => obj%GetNodeToNodePointer()

! Get mesh pointer
DO iel1 = minelem, maxelem
  isok = domain1%isElementPresent(globalElement=iel1)
  IF (.NOT. isok) CYCLE

 CALL domain1%GetConnectivity_(globalElement=iel1, ans=nptrs1, tsize=order1, &
                                islocal=.FALSE., dim=nsd)
  DO ii = 1, order1
    nptrs2(ii) = nodeToNode(nptrs1(ii))
  END DO

  DO ii = 1, order1
    IF (nptrs2(ii) .EQ. 0) CYCLE

    CALL domain2%GetNodeToElements_(GlobalNode=nptrs2(ii), ans=elem2, &
                                    tsize=telem2, islocal=.FALSE.)

    DO jj = 1, telem2

      CALL domain2%GetConnectivity_(globalElement=elem2(jj), &
                           ans=nptrs3, tsize=order2, dim=nsd, islocal=.FALSE.)

      IF (order1 .GE. order2) THEN
        IF (nptrs3(1:order2) .in.nptrs2(1:order1)) THEN
          obj%cellToCell(iel1) = elem2(jj)
          EXIT
        END IF
      ELSE
        IF (nptrs2(1:order1) .in.nptrs3(1:order2)) THEN
          obj%cellToCell(iel1) = elem2(jj)
          EXIT
        END IF
      END IF

    END DO

  END DO

END DO

NULLIFY (nodeToNode)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateCellToCellData1

!----------------------------------------------------------------------------
!                                                      GetCellToCellPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellToCellPointer
ans => obj%cellTocell
END PROCEDURE obj_GetCellToCellPointer

!----------------------------------------------------------------------------
!                                                            GetDimEntityNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDimEntityNum
ans = obj%cellToCellExtraData(1:2, globalElement)
END PROCEDURE obj_GetDimEntityNum

END SUBMODULE CellMethods
