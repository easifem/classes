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
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(Domain_Class) GetMethods
USE BaseType, ONLY: IntVector_
USE IntVector_Method
USE AppendUtility
USE ReallocateUtility
USE InputUtility
USE ArangeUtility
USE IntegerUtility

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             getMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer1
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshPointer1()"
LOGICAL(LGT) :: acase

ans => NULL()

acase = PRESENT(entityNum) .AND. PRESENT(dim)
IF (acase) THEN
  CALL getmeshpointer_case1(obj, dim, entityNum, ans)
  RETURN
END IF

acase = PRESENT(globalElement) .AND. (.NOT. PRESENT(dim))
IF (acase) THEN
  CALL getmeshpointer_case2(obj, globalElement, ans, islocal)
  RETURN
END IF

acase = PRESENT(globalElement) .AND. PRESENT(dim)
IF (acase) THEN
  CALL getmeshpointer_case3(obj, globalElement, dim, ans, islocal)
  RETURN
END IF

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[INTERNAL ERROR] :: No case found')

END PROCEDURE obj_GetMeshPointer1

!----------------------------------------------------------------------------
!                                                            GetMeshPointer
!----------------------------------------------------------------------------

SUBROUTINE getmeshpointer_case3(obj, globalElement, dim, ans, islocal)
  CLASS(Domain_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: globalElement, dim
  CLASS(AbstractMesh_), POINTER, INTENT(INOUT) :: ans
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal

  ! internal variables
  INTEGER(I4B) :: ii, jj, nsd, tsize
  LOGICAL(LGT) :: isok, found

  ans => NULL()
  nsd = obj%GetNSD()
  found = .FALSE.
  ii = dim

  tsize = obj%GetTotalEntities(dim=ii)

  elemloop: DO jj = 1, tsize

    ans => obj%GetMeshPointer(dim=ii, entityNum=jj)

    CALL getmeshpointer_case1(obj, ii, jj, ans)

    isok = ASSOCIATED(ans)
    IF (.NOT. isok) CYCLE elemloop

    found = ans%IsElementPresent(globalElement=globalElement, &
                                 islocal=islocal)

    IF (found) EXIT elemloop

  END DO elemloop

  IF (.NOT. found) ans => NULL()

END SUBROUTINE getmeshpointer_case3

!----------------------------------------------------------------------------
!                                                            GetMeshPointer
!----------------------------------------------------------------------------

SUBROUTINE getmeshpointer_case2(obj, globalElement, ans, islocal)
  CLASS(Domain_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: globalElement
  CLASS(AbstractMesh_), POINTER, INTENT(INOUT) :: ans
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal

  ! internal variables
  INTEGER(I4B) :: ii, jj, nsd, tsize
  LOGICAL(LGT) :: isok, found

  ans => NULL()
  nsd = obj%GetNSD()
  found = .FALSE.

  dimloop: DO ii = 0, nsd

    tsize = obj%GetTotalEntities(dim=ii)

    elemloop: DO jj = 1, tsize

      ans => obj%GetMeshPointer(dim=ii, entityNum=jj)

      CALL getmeshpointer_case1(obj, ii, jj, ans)

      isok = ASSOCIATED(ans)
      IF (.NOT. isok) CYCLE

      found = ans%IsElementPresent(globalElement=globalElement, &
                                   islocal=islocal)

      IF (found) EXIT dimloop

    END DO elemloop

  END DO dimloop

  IF (.NOT. found) ans => NULL()

END SUBROUTINE getmeshpointer_case2

!----------------------------------------------------------------------------
!                                                                    case1
!----------------------------------------------------------------------------

SUBROUTINE getmeshpointer_case1(obj, dim, entityNum, ans)
  CLASS(Domain_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: dim, entityNum
  CLASS(AbstractMesh_), POINTER, INTENT(INOUT) :: ans

  CHARACTER(*), PARAMETER :: myName = "obj_GetMeshPointer1()"
  LOGICAL(LGT) :: problem
  INTEGER(I4B) :: aint

  ans => NULL()

  aint = obj%GetTotalEntities(dim)
  problem = entityNum .GT. aint

  IF (problem) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
      & "[INTERNAL ERROR] :: entityNum are out of bound")
    RETURN
  END IF

  SELECT CASE (dim)
  CASE (0)
    ans => obj%meshPoint(entityNum)%ptr
  CASE (1)
    ans => obj%meshCurve(entityNum)%ptr
  CASE (2)
    ans => obj%meshSurface(entityNum)%ptr
  CASE (3)
    ans => obj%meshVolume(entityNum)%ptr
  CASE DEFAULT
    CALL e%RaiseError(modName//"::"//myName//" - "// &
      & "[INTERNAL ERROR] :: no case found for nsd")
  END SELECT

END SUBROUTINE getmeshpointer_case1

!----------------------------------------------------------------------------
!                                                          isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElementPresent
CLASS(AbstractMesh_), POINTER :: meshptr
INTEGER(I4B) :: ii, jj, tsize, nsd
LOGICAL(LGT) :: acase

ans = .FALSE.

!! case1
acase = PRESENT(dim) .AND. PRESENT(entityNum)
IF (acase) THEN

  ii = entityNum

  meshptr => obj%GetMeshPointer(dim=dim, entityNum=ii)
  ans = meshptr%IsElementPresent(globalElement=globalElement, &
                                 islocal=islocal)
  NULLIFY (meshptr)

  RETURN
END IF

!! case 2
acase = PRESENT(dim) .AND. (.NOT. PRESENT(entityNum))
IF (acase) THEN

  tsize = obj%GetTotalEntities(dim=dim)

  ent_loop: DO ii = 1, tsize

    meshptr => obj%GetMeshPointer(dim=dim, entityNum=ii)
    ans = meshptr%IsElementPresent(globalElement=globalElement, &
                                   islocal=islocal)
    IF (ans) EXIT ent_loop

  END DO ent_loop

  NULLIFY (meshptr)

  RETURN
END IF

nsd = obj%GetNSD()

dimloop: DO ii = 0, nsd

  tsize = obj%GetTotalEntities(dim=ii)

  DO jj = 1, tsize

    meshptr => obj%GetMeshPointer(dim=ii, entityNum=jj)
    ans = meshptr%IsElementPresent(globalElement=globalElement, &
                                   islocal=islocal)

    IF (ans) EXIT dimloop

  END DO

END DO dimloop

NULLIFY (meshptr)

END PROCEDURE obj_IsElementPresent

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
CLASS(AbstractMesh_), POINTER :: meshptr
INTEGER(I4B) :: dim, entityNum
INTEGER(I4B), ALLOCATABLE :: ivec(:)
LOGICAL(LGT) :: isok

meshptr => NULL()
isok = obj%IsNodePresent(globalNode=globalNode)
IF (.NOT. isok) THEN
  CALL Reallocate(ans, 0)
  RETURN
END IF

dimloop: DO dim = 0, obj%GetNSD()
  DO entityNum = 1, obj%GetTotalEntities(dim=dim)
    meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
    ivec = meshptr%GetNodeToElements(globalNode=globalNode, &
                                     islocal=islocal)
    CALL Append(ans, ivec)
  END DO
END DO dimloop

meshptr => NULL()

IF (ALLOCATED(ivec)) DEALLOCATE (ivec)

END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: ivec(:)
INTEGER(I4B) :: ii

DO ii = 1, SIZE(globalNode)
  ivec = obj%GetNodeToElements(globalNode=GlobalNode(ii), islocal=islocal)
  CALL Append(intvec, ivec)
END DO

ans = intvec
CALL DEALLOCATE (intvec)
IF (ALLOCATED(ivec)) DEALLOCATE (ivec)

END PROCEDURE obj_GetNodeToElements2

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1_
INTEGER(I4B), ALLOCATABLE :: temp(:)
INTEGER(I4B) :: ii

temp = obj%GetNodeToElements(globalNode=globalNode, islocal=islocal)
tsize = SIZE(temp)
DO ii = 1, tsize
  ans(ii) = temp(ii)
END DO

IF (ALLOCATED(temp)) DEALLOCATE (temp)

END PROCEDURE obj_GetNodeToElements1_

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2_
INTEGER(I4B), ALLOCATABLE :: temp(:)
INTEGER(I4B) :: ii

temp = obj%GetNodeToElements(globalNode=globalNode, islocal=islocal)
tsize = SIZE(temp)
DO ii = 1, tsize
  ans(ii) = temp(ii)
END DO

IF (ALLOCATED(temp)) DEALLOCATE (temp)

END PROCEDURE obj_GetNodeToElements2_

!----------------------------------------------------------------------------
!                                                         GetLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber1
LOGICAL(LGT) :: isok
ans = 0
isok = obj%IsNodePresent(globalNode=globalNode, islocal=islocal)
IF (.NOT. isok) RETURN

isok = Input(default=.FALSE., option=islocal)
IF (isok) THEN
  ans = globalNode
ELSE
  ans = obj%local_nptrs(globalNode)
END IF
END PROCEDURE obj_GetLocalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getLocalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber2
INTEGER(I4B) :: ii, tsize
tsize = SIZE(globalNode)
DO ii = 1, tsize
  ans(ii) = obj%GetLocalNodeNumber(globalNode=globalNode(ii), &
                                   islocal=islocal)
END DO
END PROCEDURE obj_GetLocalNodeNumber2

!----------------------------------------------------------------------------
!                                                       getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber1
! LOGICAL(LGT) :: isok
! ans = 0
! isok = localNode .LE. obj%GetTotalNodes()
! IF (.NOT. isok) RETURN
ans = obj%global_nptrs(localNode)
END PROCEDURE obj_GetGlobalNodeNumber1

!----------------------------------------------------------------------------
!                                                         getGlobalNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalNodeNumber2
INTEGER(I4B) :: ii, tsize
tsize = SIZE(localNode)
DO ii = 1, tsize
  ans(ii) = obj%GetGlobalNodeNumber(localNode=localNode(ii))
END DO
END PROCEDURE obj_GetGlobalNodeNumber2

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs
CHARACTER(*), PARAMETER :: myName = "obj_GetNptrs()"
INTEGER(I4B) :: ii, tentity, tnodes
CLASS(AbstractMesh_), POINTER :: meshptr
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: nptrs(:), ent0(:)
LOGICAL(LGT) :: problem

meshptr => NULL()
IF (PRESENT(entityNum)) THEN
  ent0 = entityNum
  tentity = SIZE(ent0)
ELSE
  tentity = obj%GetTotalEntities(dim=dim)
  ent0 = arange(1_I4B, tentity)
END IF

DO ii = 1, tentity

  meshptr => obj%GetMeshPointer(dim=dim, entityNum=ent0(ii))

  problem = .NOT. ASSOCIATED(meshptr)
  IF (problem) CYCLE

  problem = meshptr%isEmpty()
  IF (problem) CYCLE

  nptrs = meshptr%GetNptrs()

  CALL APPEND(intvec, nptrs)

END DO

CALL RemoveDuplicates(intvec)

IF (isAllocated(intvec)) THEN
  ans = intvec
ELSE
  CALL Reallocate(ans, 0)
END IF

CALL DEALLOCATE (intvec)

NULLIFY (meshptr)
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)

END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                                 GetNptrs_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
INTEGER(I4B) :: ii, tentity, offset, n, jj
CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: problem

meshptr => NULL()
offset = 1

IF (PRESENT(entityNum)) THEN
  tentity = SIZE(entityNum)

  DO ii = 1, tentity
    jj = entityNum(ii)
    CALL macro
  END DO

ELSE

  tentity = obj%GetTotalEntities(dim=dim)
  DO ii = 1, tentity
    jj = ii
    CALL macro
  END DO

END IF

n = offset - 1
CALL RemoveDuplicates_(obj=nptrs(1:n), tsize=n, isSorted=.FALSE.)
IF (PRESENT(tsize)) tsize = n

NULLIFY (meshptr)

CONTAINS

SUBROUTINE macro
  meshptr => obj%GetMeshPointer(dim=dim, entityNum=jj)

  problem = .NOT. ASSOCIATED(meshptr)
  IF (problem) RETURN

  problem = meshptr%isEmpty()
  IF (problem) RETURN

  CALL meshptr%GetNptrs_(nptrs=nptrs(offset:), tsize=n)

  offset = offset + n
END SUBROUTINE macro

END PROCEDURE obj_GetNptrs_

!----------------------------------------------------------------------------
!                                                         GetInternalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInternalNptrs
CHARACTER(*), PARAMETER :: myName = "obj_GetInternalNptrs()"
INTEGER(I4B) :: ii, tentity, tnodes
CLASS(AbstractMesh_), POINTER :: meshptr
TYPE(IntVector_) :: intvec
INTEGER(I4B), ALLOCATABLE :: nptrs(:), ent0(:)
LOGICAL(LGT) :: problem

meshptr => NULL()
IF (PRESENT(entityNum)) THEN
  ent0 = entityNum
  tentity = SIZE(ent0)
ELSE
  tentity = obj%GetTotalEntities(dim=dim)
  ent0 = arange(1_I4B, tentity)
END IF

DO ii = 1, tentity

  meshptr => obj%GetMeshPointer(dim=dim, entityNum=ent0(ii))

  problem = .NOT. ASSOCIATED(meshptr)
  IF (problem) CYCLE

  problem = meshptr%isEmpty()
  IF (problem) CYCLE

  nptrs = meshptr%GetInternalNptrs()

  CALL APPEND(intvec, nptrs)

END DO

CALL RemoveDuplicates(intvec)

IF (isAllocated(intvec)) THEN
  ans = intvec
ELSE
  CALL Reallocate(ans, 0)
END IF

CALL DEALLOCATE (intvec)

NULLIFY (meshptr)
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
END PROCEDURE obj_GetInternalNptrs

!----------------------------------------------------------------------------
!                                                                    getNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
INTEGER(I4B) :: ii
CLASS(AbstractMesh_), POINTER :: meshptr

CALL Reallocate(ans, obj%GetTotalMesh(dim=dim))

DO ii = 1, SIZE(ans)
  meshptr => obj%GetMeshPointer(dim=dim, entityNum=ii)
  IF (meshptr%GetTotalElements() .EQ. 0_I4B) THEN
    ans(ii) = 0
  ELSE
    ans(ii) = meshptr%GetOrder()
  END IF
  meshptr => NULL()
END DO

END PROCEDURE obj_GetOrder

! !----------------------------------------------------------------------------
! !                                                     getTotalMeshFacetData
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetTotalMeshFacetData
! IF (PRESENT(imeshFacetData)) THEN
!   IF (ALLOCATED(obj%meshFacetData)) THEN
!     IF (obj%meshFacetData(imeshFacetData)%isInitiated()) THEN
!       ans = obj%meshFacetData(imeshFacetData)%SIZE()
!     ELSE
!       ans = 0
!     END IF
!   ELSE
!     ans = 0
!   END IF
! ELSE
!   IF (ALLOCATED(obj%meshFacetData)) THEN
!     ans = SIZE(obj%meshFacetData)
!   ELSE
!     ans = 0
!   END IF
! END IF
! END PROCEDURE obj_GetTotalMeshFacetData
!
! !----------------------------------------------------------------------------
! !                                                         GetTotalMaterial
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetTotalMaterial1
! CLASS(mesh_), POINTER :: meshptr
!
! meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
! ans = meshptr%GetTotalMaterial(globalElement=globalElement, &
!                                islocal=islocal)
! meshptr => NULL()
! END PROCEDURE obj_GetTotalMaterial1
!
! !----------------------------------------------------------------------------
! !                                                       obj_GetElemType
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetElemType
! CHARACTER(*), PARAMETER :: myName = "obj_GetElemType()"
! CLASS(mesh_), POINTER :: meshptr
! INTEGER(I4B) :: ii, tMesh, idim, nsd, jj
!
! #ifdef DEBUG_VER
!
! IF (dim .GT. 3) THEN
!   CALL e%RaiseError(modName//"::"//myName//" - "// &
!     & "[INTERNAL ERROR] :: Dim of the mesh should be in [0,1,2,3]"//  &
!     & " given dim is equal to "//tostring(dim))
!   RETURN
! END IF
!
! #endif
!
! IF (dim .LT. 0) THEN
!   tMesh = 0
!   nsd = obj%GetNSD()
!   jj = 0
!
!   DO idim = 1, nsd
!     tMesh = tMesh + obj%GetTotalMesh(dim=idim)
!   END DO
!
!   CALL Reallocate(ans, tMesh)
!
!   DO idim = 1, nsd
!     DO ii = 1, obj%GetTotalMesh(dim=idim)
!       meshptr => obj%GetMeshPointer( &
!         & dim=idim, &
!         & entityNum=ii)
!       jj = jj + 1
!       CALL meshptr%GetParam(elemType=ans(jj))
!     END DO
!   END DO
!
!   meshptr => NULL()
!   RETURN
! END IF
!
! tMesh = obj%GetTotalMesh(dim=dim)
! CALL Reallocate(ans, tMesh)
!
! DO ii = 1, tMesh
!   meshptr => obj%GetMeshPointer( &
!     & dim=dim, &
!     & entityNum=ii)
!   CALL meshptr%GetParam(elemType=ans(ii))
! END DO
!
! meshptr => NULL()
!
! END PROCEDURE obj_GetElemType
!
! !----------------------------------------------------------------------------
! !                                                         GetUniqueElemType
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_GetUniqueElemType
! ans = obj%GetElemType(dim=dim)
! CALL RemoveDuplicates(ans)
! END PROCEDURE obj_GetUniqueElemType
!
! !----------------------------------------------------------------------------
! !                                                         IsInit
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE obj_IsInit
! ans = obj%isInitiated
! END PROCEDURE obj_IsInit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE GetMethods
