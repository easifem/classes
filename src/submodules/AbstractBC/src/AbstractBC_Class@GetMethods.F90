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

SUBMODULE(AbstractBC_Class) GetMethods
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: CHAR_LF
USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                       IsElemToEdgeInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElemToEdgeInitiated
ans = obj%isElemToEdge
END PROCEDURE obj_IsElemToEdgeInitiated

!----------------------------------------------------------------------------
!                                                       IsElemToFaceInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElemToFaceInitiated
ans = obj%isElemToFace
END PROCEDURE obj_IsElemToFaceInitiated

!----------------------------------------------------------------------------
!                                                          GetTotalElemToEdge
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElemToEdge
ans = obj%tElemToEdge
END PROCEDURE obj_GetTotalElemToEdge

!----------------------------------------------------------------------------
!                                                          GetTotalElemToFace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElemToFace
ans = obj%tElemToFace
END PROCEDURE obj_GetTotalElemToFace

!----------------------------------------------------------------------------
!                                                               GetElemToFace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemToFace
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemToFace()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = indx .LE. obj%tElemToFace
CALL AssertError1(isok, myName, &
                  "indx ("//ToString(indx)//") > tElemToFace ("// &
                  ToString(obj%tElemToFace)//")")
#endif

localCellNumber = obj%elemToFace(1, indx)
localFaceNumber = obj%elemToFace(2, indx)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemToFace

!----------------------------------------------------------------------------
!                                                               GetElemToEdge
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemToEdge
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemToEdge()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = indx .LE. obj%tElemToEdge
CALL AssertError1(isok, myName, &
                  "indx ("//ToString(indx)//") > tElemToEdge ("// &
                  ToString(obj%tElemToEdge)//")")
#endif

localCellNumber = obj%elemToEdge(1, indx)
localEdgeNumber = obj%elemToEdge(2, indx)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemToEdge

!----------------------------------------------------------------------------
!                                                                 GetMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshID
ans = obj%boundary%GetMeshID(dim=dim)
END PROCEDURE obj_GetMeshID

!----------------------------------------------------------------------------
!                                                           GetMeshIDPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshIDPointer
CALL obj%boundary%GetMeshIDPointer(dim=dim, ans=ans, tsize=tsize)
END PROCEDURE obj_GetMeshIDPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFNo
ans = obj%idof
END PROCEDURE obj_GetDOFNo

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

CHARACTER(6) :: casename

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

casename = fedof%GetCaseName()

SELECT CASE (casename)

CASE ("H1LAGR")

  CALL obj%GetH1Lagrange(fedof=fedof, nodenum=nodenum, &
                         nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                         times=times)

CASE ("H1HIER", "H1HEIR")

  CALL obj%GetH1Hierarchical(fedof=fedof, nodenum=nodenum, &
                             nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                             times=times)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    "No case found for fedof casename="//casename)
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
#endif
LOGICAL(LGT), PARAMETER :: no = .FALSE., yes = .TRUE.

INTEGER(I4B) :: mysize, localCellNumber, localFaceNumber, ii, &
                localEdgeNumber, jj, indx(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')
#endif

! Vertex nodes
tsize = 0
CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=mysize)
tsize = tsize + mysize

iNodeOnNode = 1
iNodeOnFace = tsize + 1

DO ii = 1, tsize
  CALL fedof%GetVertexDOF(globalNode=nodenum(ii), ans=indx, islocal=no, &
                          tsize=jj)
  nodenum(ii) = indx(1)
END DO

CALL obj%SetElemToLocalBoundary()

! Face nodes
DO ii = 1, obj%tElemToFace
  CALL obj%GetElemToFace(indx=ii, localFaceNumber=localFaceNumber, &
                         localCellNumber=localCellNumber)

  CALL fedof%GetFaceDOF( &
    globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
    ans=nodenum(tsize + 1:), tsize=mysize, islocal=yes)

  tsize = tsize + mysize
END DO

! Edge nodes
iNodeOnEdge = tsize + 1
DO ii = 1, obj%tElemToEdge
  CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
                         localCellNumber=localCellNumber)

  CALL fedof%GetEdgeDOF( &
    globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
    ans=nodenum(tsize + 1:), tsize=mysize, islocal=yes)

  tsize = tsize + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                           GetTotalNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeNum
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodeNum()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, localFaceNumber, localEdgeNumber, localCellNumber, &
                mysize
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')
#endif

ans = 0
mysize = obj%boundary%GetTotalNodeNum(dom=obj%dom)
ans = ans + mysize

CALL obj%SetElemToLocalBoundary()

DO ii = 1, obj%tElemToFace
  CALL obj%GetElemToFace(indx=ii, localCellNumber=localCellNumber, &
                         localFaceNumber=localFaceNumber)

  mysize = fedof%GetTotalFaceDOF(globalElement=localCellNumber, &
                                 localFaceNumber=localFaceNumber, islocal=yes)
  ans = ans + mysize
END DO

DO ii = 1, obj%tElemToEdge
  CALL obj%GetElemToEdge(indx=ii, localCellNumber=localCellNumber, &
                         localEdgeNumber=localEdgeNumber)

  mysize = fedof%GetTotalEdgeDOF(globalElement=localCellNumber, &
                                 localEdgeNumber=localEdgeNumber, islocal=yes)

  ans = ans + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodeNum

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isuseFunction
ans = obj%isUserFunction
END PROCEDURE obj_isuseFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
CALL obj%boundary%GetParam(isSelectionByBox=isSelectionByBox, &
                           isSelectionByMeshID=isSelectionByMeshID, &
                           isSelectionByElemNum=isSelectionByElemNum, &
                           isSelectionByNodeNum=isSelectionByNodeNum)

IF (PRESENT(idof)) idof = obj%idof
IF (PRESENT(isTangent)) isTangent = obj%isTangent
IF (PRESENT(isNormal)) isNormal = obj%isNormal
IF (PRESENT(useFunction)) useFunction = obj%isUserFunction
IF (PRESENT(isUserFunction)) isUserFunction = obj%isUserFunction
IF (PRESENT(nodalValueType)) nodalValueType = obj%nodalValueType
IF (PRESENT(isInitiated)) isInitiated = obj%isInit
IF (PRESENT(isUseExternal)) isUseExternal = obj%isUseExternal
IF (PRESENT(isElemToFace)) isElemToFace = obj%isElemToFace
IF (PRESENT(isElemToEdge)) isElemToEdge = obj%isElemToEdge
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
