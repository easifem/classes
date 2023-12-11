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

SUBMODULE(AbstractRefElement_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
CHARACTER(*), PARAMETER :: myName = "refelem_Initiate()"
INTEGER(I4B) :: name, aint
REAL(DFP), ALLOCATABLE :: xij0(:, :)
LOGICAL(LGT) :: isok, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%DEALLOCATE()

IF (PRESENT(xij)) THEN

  aint = SIZE(xij, 1)
  problem = aint .NE. nsd
  IF (problem) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: size(xij, 1) which is '//  &
      & tostring(aint)//' is not equal to '//  &
      & ' NSD which is '//tostring(nsd))
    RETURN
  END IF
  xij0 = xij

ELSE

  IF (.NOT. PRESENT(baseContinuity)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: baseContinuity should be present.')
    RETURN
  END IF

  IF (.NOT. PRESENT(baseInterpolation)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: baseInterpolation should be present.')
    RETURN
  END IF

  xij0 = obj%RefCoord(baseInterpolation=baseInterpolation,  &
    & baseContinuity=baseContinuity)

END IF

name = obj%GetName()

SELECT CASE (name)
CASE (Point)
  obj%refelem = ReferencePoint(nsd=nsd, xij=xij0)
CASE (Line)
  obj%refelem = ReferenceLine(nsd=nsd, xij=xij0)
CASE (Triangle)
  obj%refelem = ReferenceTriangle(nsd=nsd, xij=xij0)
CASE (Quadrangle)
  obj%refelem = ReferenceQuadrangle(nsd=nsd, xij=xij0)
CASE (Tetrahedron)
  obj%refelem = ReferenceTetrahedron(nsd=nsd, xij=xij0)
CASE (Hexahedron)
  obj%refelem = ReferenceHexahedron(nsd=nsd, xij=xij0)
CASE (Prism)
  obj%refelem = ReferencePrism(nsd=nsd, xij=xij0)
CASE (Pyramid)
  obj%refelem = ReferencePyramid(nsd=nsd, xij=xij0)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[NO CASE FOUND] no case found for given refelem name')
END SELECT

CALL obj%SetParam( &
  & nameStr=ElementName(name),  &
  & baseContinuity=baseContinuity,  &
  & baseInterpolation=baseInterpolation)

! CALL obj%GenerateTopology()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE refelem_Initiate

!----------------------------------------------------------------------------
!                                                               GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
INTEGER(I4B) :: ii, n, jj

IF (PRESENT(xidim)) THEN
  n = obj%refelem%entityCounts(xidim + 1)

  ALLOCATE (ans(n))

  IF (xidim .EQ. 0) THEN
    jj = 0
  ELSE
    jj = SUM(obj%refelem%entityCounts(1:xidim))
  END IF

  DO ii = 1, n
    ans(ii) = obj%refelem%topology(jj + ii)
  END DO

ELSE
  n = SUM(obj%refelem%entityCounts)
  ALLOCATE (ans(n))
  DO ii = 1, n
    ans(ii) = obj%refelem%topology(ii)
  END DO
END IF

END PROCEDURE refelem_GetTopology

!----------------------------------------------------------------------------
!                                                                      Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Copy
CHARACTER(*), PARAMETER :: myName = "refelem_Copy()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL obj%DEALLOCATE()
obj%refelem = obj2%refelem
obj%nameStr = obj2%nameStr
!! baseInterpolation
IF (ALLOCATED(obj2%baseInterpolation)) THEN
  ALLOCATE (obj%baseInterpolation, source=obj2%baseInterpolation)
END IF
!! baseContinuity
IF (ALLOCATED(obj2%baseContinuity)) THEN
  ALLOCATE (obj%baseContinuity, source=obj2%baseContinuity)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE refelem_Copy

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Deallocate
CALL DEALLOCATE (obj%refelem)
obj%nameStr = ""
IF (ALLOCATED(obj%baseContinuity)) DEALLOCATE (obj%baseContinuity)
IF (ALLOCATED(obj%baseInterpolation)) DEALLOCATE (obj%baseInterpolation)

END PROCEDURE refelem_Deallocate

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Display
!! Define internal variable
LOGICAL(LGT) :: notFull0
CHARACTER(*), PARAMETER :: myName = "refelem_Display()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

notFull0 = INPUT(option=notFull, default=.FALSE.)
CALL Display(msg, unitno=unitno)
CALL Display(obj%refelem, "refelem: ", unitno=unitno)
!! baseContinuity
IF (ALLOCATED(obj%baseContinuity)) THEN
  CALL Display( &
  & "baseContinuity: "//BaseContinuity_toString(obj%baseContinuity), &
  & unitno=unitno)
ELSE
  CALL Display("baseContinuity: NOT ALLOCATED")
END IF
!! baseInterpolation
IF (ALLOCATED(obj%baseInterpolation)) THEN
  CALL Display( &
 & "baseInterpolation: "//BaseInterpolation_toString(obj%baseInterpolation), &
  & unitno=unitno)
ELSE
  CALL Display("baseInterpolation: NOT ALLOCATED")
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE refelem_Display

!----------------------------------------------------------------------------
!                                                               MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_MdEncode
CHARACTER(*), PARAMETER :: myName = "refelem_MdEncode()"
TYPE(String) :: astr(2)
TYPE(String) :: rowTitle(2), colTitle(1)
colTitle(1) = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

rowTitle(1) = "BaseContinuity"
!! baseContinuity
IF (ALLOCATED(obj%baseContinuity)) THEN
  astr(1) = BaseContinuity_toString(obj%baseContinuity)
ELSE
  astr(1) = "NOT ALLOCATED"
END IF

rowTitle(2) = "BaseInterpolation"
!! baseContinuity
IF (ALLOCATED(obj%baseInterpolation)) THEN
  astr(2) = BaseInterpolation_toString(obj%baseInterpolation)
ELSE
  astr(2) = "NOT ALLOCATED"
END IF

ans = MdEncode(obj%refelem)  &
  & //MdEncode(val=astr(1:2), rh=rowTitle(1:2), ch=colTitle)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE refelem_MdEncode

!----------------------------------------------------------------------------
!                                                        ReactEncode@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_ReactEncode
! !! Define internal variable
! INTEGER(I4B) :: j, tsize
! LOGICAL(LGT) :: notFull0
! TYPE(String) :: rowTitle(20), colTitle(1)
! TYPE(String) :: astr(20)
! CHARACTER(1), PARAMETER, DIMENSION(3) :: xyz = ["x", "y", "z"]
!
! colTitle(1) = ""
! rowTitle(1) = "Element type"; astr(1) = ElementName(obj%name)
! rowTitle(2) = "Xidimension"; astr(2) = tostring(obj%xiDimension)
! rowTitle(3) = "NSD"; astr(3) = tostring(obj%nsd)
! rowTitle(4) = "tPoints"; astr(4) = tostring(obj%entityCounts(1))
! rowTitle(5) = "tLines"; astr(5) = tostring(obj%entityCounts(2))
! rowTitle(6) = "tSurfaces"; astr(6) = tostring(obj%entityCounts(3))
! rowTitle(7) = "tVolumes"; astr(7) = tostring(obj%entityCounts(4))
!
! rowTitle(8) = "BaseContinuity"
! !! baseContinuity
! IF (ALLOCATED(obj%baseContinuity)) THEN
!   astr(8) = BaseContinuity_toString(obj%baseContinuity)
! ELSE
!   astr(8) = "NOT ALLOCATED"
! END IF
!
! rowTitle(9) = "BaseInterpolation"
! !! baseContinuity
! IF (ALLOCATED(obj%baseInterpolation)) THEN
!   astr(9) = BaseInterpolation_toString(obj%baseInterpolation)
! ELSE
!   astr(9) = "NOT ALLOCATED"
! END IF
!
! tsize = SIZE(obj%xij, 1)
! DO j = 1, tsize
!   rowTitle(9 + j) = xyz(j)
! END DO
!
! ans = MdEncode(val=astr(1:9), rh=rowTitle(1:9), ch=colTitle)// &
!     & char_lf//"Nodal Coordinates:"//char_lf//char_lf// &
!     & MdEncode(obj%xij, rh=rowTitle(10:9 + tsize), ch=colTitle)
!
! IF (obj%entityCounts(1) .GT. 0_I4B) THEN
!   ans = ans//React_StartTabs()//char_lf
!
!   !! pointTopology
!   DO j = 1, obj%entityCounts(1)
!     ans = ans//React_StartTabItem( &
!     & VALUE=tostring(j), &
!     & label="PointTopology( "//tostring(j)//" ) : ")//char_lf//  &
!     & obj%pointTopology(j)%MdEncode()//char_lf  &
!     & //React_EndTabItem()//char_lf
!   END DO
!
!   ans = ans//React_EndTabs()//char_lf
! END IF
!
! IF (obj%entityCounts(2) .GT. 0_I4B) THEN
!   ans = ans//React_StartTabs()//char_lf
!
!   !! pointTopology
!   DO j = 1, obj%entityCounts(2)
!     ans = ans//React_StartTabItem( &
!     & VALUE=tostring(j), &
!     & label="EdgeTopology( "//tostring(j)//" ) : ")//char_lf//  &
!     & obj%EdgeTopology(j)%MdEncode()//char_lf  &
!     & //React_EndTabItem()//char_lf
!   END DO
!
!   ans = ans//React_EndTabs()//char_lf
! END IF
!
! IF (obj%entityCounts(3) .GT. 0_I4B) THEN
!   ans = ans//React_StartTabs()//char_lf
!
!   !! pointTopology
!   DO j = 1, obj%entityCounts(3)
!     ans = ans//React_StartTabItem( &
!     & VALUE=tostring(j), &
!     & label="FaceTopology( "//tostring(j)//" ) : ")//char_lf//  &
!     & obj%FaceTopology(j)%MdEncode()//char_lf &
!     & //React_EndTabItem()//char_lf
!   END DO
!
!   ans = ans//React_EndTabs()//char_lf
! END IF
!
! IF (obj%entityCounts(4) .GT. 0_I4B) THEN
!   ans = ans//React_StartTabs()//char_lf
!
!   !! pointTopology
!   DO j = 1, obj%entityCounts(4)
!     ans = ans//React_StartTabItem( &
!     & VALUE=tostring(j), &
!     & label="CellTopology( "//tostring(j)//" ) : ")//char_lf//  &
!     & obj%CellTopology(j)%MdEncode()//char_lf  &
!     & //React_EndTabItem()//char_lf
!   END DO
!
!   ans = ans//React_EndTabs()//char_lf
! END IF

END PROCEDURE refelem_ReactEncode

!----------------------------------------------------------------------------
!                                                                     GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetNNE
ans = .NNE.obj%refelem
END PROCEDURE refelem_GetNNE

!----------------------------------------------------------------------------
!                                                                    GetNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetNSD
ans = obj%refelem%NSD
END PROCEDURE refelem_GetNSD

!----------------------------------------------------------------------------
!                                                            GetXidimension
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetXidimension
ans = obj%refelem%xidimension
END PROCEDURE refelem_GetXidimension

!----------------------------------------------------------------------------
!                                                         GetElementTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementTopology
ans = ElementTopology(obj%refelem%name)
END PROCEDURE refelem_GetElementTopology

!----------------------------------------------------------------------------
!                                                                  GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetNptrs
ans = GetConnectivity(obj%refelem)
END PROCEDURE refelem_GetNptrs

!----------------------------------------------------------------------------
!                                                            GetFacetMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetMatrix
ans = FacetMatrix(obj%refelem)
END PROCEDURE refelem_GetFacetMatrix

!----------------------------------------------------------------------------
!                                                               GetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetNodeCoord
ans = LocalNodeCoord(obj%refelem)
END PROCEDURE refelem_GetNodeCoord

!----------------------------------------------------------------------------
!                                                      GetInterpolationPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetInterpolationPoint
IF (isPoint(obj%refelem%name)) THEN
  CALL Reallocate(ans, 3_I4B, 1_I4B)
ELSE
  ans = InterpolationPoint( &
    & order=order, &
    & ipType=ipType, &
    & elemType=obj%refelem%name, &
    & layout=layout, &
    & xij=obj%refelem%xij)
END IF
END PROCEDURE refelem_GetInterpolationPoint

!----------------------------------------------------------------------------
!                                                                 SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_SetParam
INTEGER(I4B) :: ii, n

IF (PRESENT(xij)) obj%refelem%xij = xij
IF (PRESENT(entityCounts)) obj%refelem%entityCounts = entityCounts
IF (PRESENT(xidimension)) obj%refelem%xidimension = xidimension
IF (PRESENT(name)) obj%refelem%name = name
IF (PRESENT(nameStr)) obj%nameStr = nameStr
IF (PRESENT(nsd)) obj%refelem%nsd = nsd

IF (PRESENT(topology)) THEN
  IF (ALLOCATED(obj%refelem%topology)) DEALLOCATE (obj%refelem%topology)
  n = SIZE(topology)
  ALLOCATE (obj%refelem%topology(n))
  DO ii = 1, n
    obj%refelem%topology(ii) = topology(ii)
  END DO
END IF

IF (PRESENT(baseContinuity)) THEN
  CALL BaseContinuity_fromString(obj=obj%baseContinuity, name=baseContinuity)
END IF

IF (PRESENT(baseInterpolation)) THEN
  CALL BaseInterpolation_fromString( &
    & obj=obj%baseInterpolation, &
    & name=baseInterpolation)
END IF

IF (PRESENT(refelem)) THEN
  obj%refelem = refelem
END IF

END PROCEDURE refelem_SetParam

!----------------------------------------------------------------------------
!                                                                 GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetParam
INTEGER(I4B) :: ii, n
CHARACTER(*), PARAMETER :: myName = "refelem_GetParam"

IF (PRESENT(entityCounts)) entityCounts = obj%refelem%entityCounts
IF (PRESENT(xidimension)) xidimension = obj%refelem%xidimension
IF (PRESENT(name)) name = obj%refelem%name
IF (PRESENT(nameStr)) nameStr = obj%nameStr
IF (PRESENT(nsd)) nsd = obj%refelem%nsd

IF (PRESENT(xij)) THEN
  IF (ALLOCATED(obj%refelem%xij)) THEN
    xij = obj%refelem%xij
  END IF
END IF

IF (PRESENT(topology)) THEN
  IF (ALLOCATED(obj%refelem%topology)) THEN
    IF (ALLOCATED(topology)) DEALLOCATE (topology)
    n = SIZE(obj%refelem%topology)
    ALLOCATE (topology(n))
    DO ii = 1, n
      topology(ii) = obj%refelem%topology(ii)
    END DO
  END IF
END IF

IF (PRESENT(baseInterpolation)) THEN
  IF (ALLOCATED(obj%baseInterpolation)) THEN
    baseInterpolation = BaseInterpolation_toString(obj%baseInterpolation)
  ELSE
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'AbstractRefElement_::obj%baseInterpolation is not allocated.')
  END IF
END IF

IF (PRESENT(baseContinuity)) THEN
  IF (ALLOCATED(obj%baseContinuity)) THEN
    baseContinuity = baseContinuity_toString(obj%baseContinuity)
  ELSE
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'AbstractRefElement_::obj%baseContinuity is not allocated.')
  END IF
END IF

IF (PRESENT(refelem)) THEN
  refelem = obj%refelem
END IF

END PROCEDURE refelem_GetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
