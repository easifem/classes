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

SUBMODULE(BasisOpt_Class) HierarchicalMethods
! USE FPL_Method, ONLY: GetValue, CheckEssentialParam, Set
USE HierarchicalPolynomialUtility, ONLY: HierarchicalDOF
USE Display_Method, ONLY: ToString
USE BasisOptUtility, ONLY: SetIntegerType

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                      CheckErrorHierarchical
!----------------------------------------------------------------------------

SUBROUTINE CheckErrorHierarchical(obj, cellOrder, faceOrder, &
           edgeOrder, cellOrient, faceOrient, edgeOrient, tcell, tface, tedge)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! cell orient
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! face orient
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! eddge orient
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tcell
    !! size of cellOrder
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tface
    !! number of columns in faceOrder
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tedge
    !! size of edgeorder

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_CheckErrorHierarchical()"
  LOGICAL(LGT) :: isok, abool
  INTEGER(I4B) :: tsize
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')

  isok = PRESENT(cellOrder)
  CALL AssertError1(isok, myName, "cellOrder is not present")

  isok = PRESENT(cellOrient)
  CALL AssertError1(isok, myName, "cellOrient is not present")

  isok = PRESENT(tcell)
  CALL AssertError1(isok, myName, "tcell is not present")

  tsize = SIZE(cellOrder)
  isok = tcell .LE. tsize
  CALL AssertError1(isok, myName, &
                    "size of cellOrder is not enough. tcell = "// &
                    ToString(tcell)//", size(cellOrder) = "// &
                    ToString(tsize))

  tsize = SIZE(cellOrient)
  isok = tcell .LE. tsize
  CALL AssertError1(isok, myName, &
                    "size of cellOrient is not enough. tcell = "// &
                    ToString(tcell)//", size(cellOrient) = "// &
                    ToString(tsize))

  abool = obj%xidim .GE. 2
  IF (abool) THEN

    isok = PRESENT(faceOrder)
    CALL AssertError1(isok, myName, "faceOrder is not present")

    isok = PRESENT(tface)
    CALL AssertError1(isok, myName, "tface is not present")

    tsize = SIZE(faceOrder, 1)
    isok = tsize .EQ. 3
    CALL AssertError1(isok, myName, &
                      "rowsize in faceOrder should be 3. tsize = "// &
                      ToString(tsize))

    tsize = SIZE(faceOrder, 2)
    isok = tsize .GE. tface
    CALL AssertError1(isok, myName, &
                      "colsize in faceOrder is not enough. tface = "// &
                      ToString(tface)//", size(faceOrder, 2) = "// &
                      ToString(tsize))

    isok = PRESENT(faceOrient)
    CALL AssertError1(isok, myName, "faceOrient is not present")

    tsize = SIZE(faceOrient, 1)
    isok = tsize .EQ. 3
    CALL AssertError1(isok, myName, &
                      "rowsize in faceOrient should be 3. tsize = "// &
                      ToString(tsize))

    tsize = SIZE(faceOrient, 2)
    isok = tsize .GE. tface
    CALL AssertError1(isok, myName, &
                      "colsize in faceOrient is not enough. tface = "// &
                      ToString(tface)//", size(faceOrient, 2) = "// &
                      ToString(tsize))

  END IF

  abool = obj%xidim .GE. 3
  IF (abool) THEN

    isok = PRESENT(edgeOrder)
    CALL AssertError1(isok, myName, "edgeOrder is not present")

    isok = PRESENT(edgeOrient)
    CALL AssertError1(isok, myName, "edgeOrient is not present")

    isok = PRESENT(tedge)
    CALL AssertError1(isok, myName, "tedge is not present")

    tsize = SIZE(edgeOrder)
    isok = tsize .GE. tedge
    CALL AssertError1(isok, myName, &
                      "size of edgeOrder is not enough. tedge = "// &
                      ToString(tedge)//", size(edgeOrder) = "// &
                      ToString(tsize))

    tsize = SIZE(edgeOrient)
    isok = tsize .GE. tedge
    CALL AssertError1(isok, myName, &
                      "size of edgeOrient is not enough. tedge = "// &
                      ToString(tedge)//", size(edgeOrient) = "// &
                      ToString(tsize))

  END IF

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE CheckErrorHierarchical

!----------------------------------------------------------------------------
!                                                       SetHierarchicalOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetHierarchicalOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetHierarchicalOrder()"
LOGICAL(LGT) :: errCheck0
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
errCheck0 = .FALSE.
IF (errCheck) errCheck0 = errCheck
IF (errCheck0) THEN
  CALL CheckErrorHierarchical(obj=obj, cellOrder=cellOrder, &
            faceOrder=faceOrder, edgeOrder=edgeOrder, cellOrient=cellOrient, &
     faceOrient=faceOrient, edgeOrient=edgeOrient, tcell=tcell, tface=tface, &
                              tedge=tedge)
END IF
#endif

obj%tdof = HierarchicalDOF(elemType=obj%topoName, cellOrder=cellOrder, &
                           faceOrder=faceOrder, edgeOrder=edgeOrder)

obj%isCellOrder = PRESENT(cellOrder)
obj%isCellOrient = PRESENT(cellOrient)
IF (obj%isCellOrder) THEN
  obj%tCellOrder = tcell
  CALL SetIntegerType(a=obj%cellOrder, n=obj%tCellOrder, b=cellOrder)
END IF

IF (obj%isCellOrient) THEN
  CALL SetIntegerType(a=obj%cellOrient, n=obj%tCellOrder, b=cellOrient)
END IF

obj%isFaceOrder = PRESENT(faceOrder)
obj%isFaceOrient = PRESENT(faceOrient)
IF (obj%isFaceOrder) THEN
  ! isok = obj%xidim .GE. 2
  ! IF (isok) THEN
  obj%tFaceOrder = tface
  CALL SetIntegerType(a=obj%faceOrder, b=faceOrder, nrow=3, &
                      ncol=obj%tFaceOrder)
  ! END IF
END IF

IF (obj%isFaceOrient) THEN
  CALL SetIntegerType(a=obj%faceOrient, b=faceOrient, nrow=3, &
                      ncol=obj%tFaceOrder)
END IF

obj%isEdgeOrder = PRESENT(edgeOrder)
obj%isEdgeOrient = PRESENT(edgeOrient)
IF (obj%isEdgeOrder) THEN
  ! IF (obj%xidim .GE. 3) THEN
  obj%tEdgeOrder = tedge
  CALL SetIntegerType(a=obj%edgeOrder, n=obj%tEdgeOrder, b=edgeOrder)
  ! END IF
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetHierarchicalOrder

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HierarchicalMethods
