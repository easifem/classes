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

SUBMODULE(AbstractField_Class) GetMethods
USE Display_Method, ONLY: ToString
USE BaseType, ONLY: TypeDOFOpt
USE InputUtility, ONLY: Input
USE DOF_Method, ONLY: GetNodeLoc_
USE ArangeUtility, ONLY: Arange
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

IF (PRESENT(isInitiated)) isInitiated = obj%isInit
IF (PRESENT(fieldType)) fieldType = obj%fieldType
IF (PRESENT(name)) name = obj%name%chars()
IF (PRESENT(engine)) engine = obj%engine%chars()
IF (PRESENT(comm)) comm = obj%comm
IF (PRESENT(myRank)) myRank = obj%myRank
IF (PRESENT(numProcs)) numProcs = obj%numProcs
IF (PRESENT(global_n)) global_n = obj%global_n
IF (PRESENT(local_n)) local_n = obj%local_n
IF (PRESENT(is)) is = obj%is
IF (PRESENT(ie)) ie = obj%ie
IF (PRESENT(lis_ptr)) lis_ptr = obj%lis_ptr

IF (PRESENT(fedof)) fedof => obj%fedof

IF (PRESENT(fedofs)) THEN

  isok = ALLOCATED(obj%fedofs)
  IF (.NOT. isok) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
           '[INTERNAL ERROR] :: AbstractField_::obj%fedofs is not allocated ')
    RETURN
  END IF

  isok = SIZE(obj%fedofs) .EQ. SIZE(fedofs)

  IF (.NOT. isok) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: AbstractField_::obj%fedofs size mismatch')
    RETURN
  END IF

  DO ii = 1, SIZE(fedofs)
    fedofs(ii)%ptr => obj%fedofs(ii)%ptr
  END DO

END IF

!SELECT TYPE (obj)
!CLASS IS (AbstractNodeField_)
!  IF (PRESENT(tSize)) tSize = obj%tSize
!  IF (PRESENT(realVec)) realVec = obj%realVec
!  IF (PRESENT(dof)) dof = obj%dof
!CLASS IS (AbstractMatrixField_)
!  IF (PRESENT(isPMatInitiated)) isPMatInitiated = obj%isPMatInitiated
!END SELECT
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                                     GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%name%chars()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetName

!----------------------------------------------------------------------------
!                                                               GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDOF()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%fedof)
IF (isok) THEN
  DO ii = 1, tPhysicalVars
    ans(ii) = obj%fedof%GetTotalDOF()
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = ALLOCATED(obj%fedofs)
IF (isok) THEN
  DO ii = 1, tPhysicalVars
    ans(ii) = obj%fedofs(ii)%ptr%GetTotalDOF()
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDOF

!----------------------------------------------------------------------------
!                                                          GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalVertexDOF

!----------------------------------------------------------------------------
!                                                          GetTotalEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalEdgeDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalEdgeDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalEdgeDOF

!----------------------------------------------------------------------------
!                                                          GetTotalFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFaceDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalFaceDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalFaceDOF

!----------------------------------------------------------------------------
!                                                          GetTotalCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalCellDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalCellDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                                                 isConstant
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isConstant
ans = obj%fieldType .EQ. TypeField%constant
END PROCEDURE obj_isConstant

!----------------------------------------------------------------------------
!                                                           GetFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEDOFPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEDOFPointer1()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = PRESENT(indx) .AND. ALLOCATED(obj%fedofs)

IF (abool) THEN

#ifdef DEBUG_VER
  tsize = SIZE(obj%fedofs)
  isok = indx .LE. tsize
  CALL AssertError1(isok, myName, &
                  "indx="//ToString(indx)//" should be <= size of fedofs="// &
                    ToString(tsize))
#endif

  ans => obj%fedofs(indx)%ptr

ELSE

  ans => obj%fedof

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFEDOFPointer1

!----------------------------------------------------------------------------
!                                                          GetFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEDOFPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEDOFPointer2()"
#endif

INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%fedofs)
tsize = 0

IF (isok) tsize = SIZE(obj%fedofs)

ALLOCATE (ans(tsize))

DO ii = 1, tsize
  ans(ii)%ptr => obj%fedofs(ii)%ptr
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetFEDOFPointer2

!----------------------------------------------------------------------------
!                                                        GetTimeFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeFEDOFPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeFEDOFPointer1()"
#endif

#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize
#endif

LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = PRESENT(indx) .AND. ALLOCATED(obj%timefedofs)

IF (abool) THEN

#ifdef DEBUG_VER
  tsize = SIZE(obj%timefedofs)
  isok = indx .LE. tsize

  CALL AssertError1(isok, myName, &
                    "indx should be less than or equal to size of timefedofs")
#endif

  ans => obj%timefedofs(indx)%ptr

ELSE

  ans => obj%timefedof

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTimeFEDOFPointer1

!----------------------------------------------------------------------------
!                                                        GetTimeFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeFEDOFPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeFEDOFPointer2()"
#endif

INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%timefedofs)
tsize = 0; IF (isok) tsize = SIZE(obj%timefedofs)

ALLOCATE (ans(tsize))

DO ii = 1, tsize
  ans(ii)%ptr => obj%timefedofs(ii)%ptr
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTimeFEDOFPointer2

!----------------------------------------------------------------------------
!                                                              GetEngineName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEngineName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetEngineName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%engine%chars()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetEngineName

!----------------------------------------------------------------------------
!                                                                GetTotalNBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNBC()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%nbc)
ans = 0
IF (isok) ans = SIZE(obj%nbc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNBC

!----------------------------------------------------------------------------
!                                                           GetTotalPointNBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalPointNBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNBC()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%nbc_point)
ans = 0
IF (isok) ans = SIZE(obj%nbc_point)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalPointNBC

!----------------------------------------------------------------------------
!                                                               GetNBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNBCPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNBCPointer()"
INTEGER(I4B) :: tsize
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%nbc)

IF (.NOT. isok) THEN
  ans => NULL()
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
tsize = SIZE(obj%nbc)
isok = indx .LE. tsize
CALL AssertError1(isok, myName, &
                  "indx should be less than or equal to size of nbc")

#endif

ans => obj%nbc(indx)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNBCPointer

!----------------------------------------------------------------------------
!                                                          GetPointNBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointNBCPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPointNBCPointer()"
INTEGER(I4B) :: tsize
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%nbc_point)

IF (.NOT. isok) THEN
  ans => NULL()
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
tsize = SIZE(obj%nbc_point)
isok = indx .LE. tsize
CALL AssertError1(isok, myName, &
                  "indx should be less than or equal to size of nbc_point")

#endif

ans => obj%nbc_point(indx)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPointNBCPointer

!----------------------------------------------------------------------------
!                                                               GetMeshField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshField()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshField

!----------------------------------------------------------------------------
!                                                    GetMaxTotalNodeNumForBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalNodeNumForBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalNodeNumForBC1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isMaxTotalNodeNumForBCSet) THEN
  ans = obj%maxTotalNodeNumForBC

ELSE
  CALL obj%SetMaxTotalNodeNumForBC()
  ans = obj%maxTotalNodeNumForBC
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalNodeNumForBC1

!----------------------------------------------------------------------------
!                                                    GetMaxTotalNodeNumForBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalNodeNumForBC2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalNodeNumForBC2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isMaxTotalNodeNumForBCSet) THEN
  ans = obj%maxTotalNodeNumForBC

ELSE
  CALL obj%SetMaxTotalNodeNumForBC(ivar=ivar)
  ans = obj%maxTotalNodeNumForBC
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalNodeNumForBC2

!----------------------------------------------------------------------------
!                                                             GetTotalNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeLoc1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodeLoc1()"
#endif

INTEGER(I4B) :: ttime, tspace, tnode
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tspace = 1
isok = PRESENT(spaceCompo)
IF (isok) tspace = SIZE(spaceCompo)

ttime = 1
isok = PRESENT(timeCompo)
IF (isok) ttime = SIZE(timeCompo)

tnode = SIZE(globalNode)

ans = ttime * tspace * tnode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetTotalNodeLoc1

!----------------------------------------------------------------------------
!                                                           GetTotalNodeLoc2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeLoc2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc2()"
#endif

#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: timeCompo(TypeDOFOpt%maxPhysicalVars)
INTEGER(I4B) :: tPhysicalVars, ivar0
INTEGER(I4B) :: tsize, tspace, ttime, tnode
LOGICAL(LGT) :: isfedof, isfedofs, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ivar0 = Input(default=math%one_i, option=ivar)
tspace = 1

tPhysicalVars = obj%GetTotalPhysicalVars()

#ifdef DEBUG_VER
isok = tPhysicalVars .LE. TypeDOFOpt%maxPhysicalVars
CALL AssertError1(isok, myName, &
                  "Total physical variables = "//ToString(tPhysicalVars)// &
                  " is greater than TypeDOFOpt%maxPhysicalVars = "// &
                  ToString(TypeDOFOpt%maxPhysicalVars))
#endif

timeCompo(1:tPhysicalVars) = obj%GetTimeCompo(tPhysicalVars)
ttime = timeCompo(ivar0)

isfedof = ASSOCIATED(obj%fedof)
isfedofs = ALLOCATED(obj%fedofs)

#ifdef DEBUG_VER
isok = isfedof .OR. isfedofs
CALL AssertError1(isok, myName, &
                  "Neither fedof is associated nor fedofs is allocated.")

isok = .NOT. (isfedof .AND. isfedofs)
CALL AssertError1(isok, myName, &
                  "Both fedof and fedofs are allocated/associated.")
#endif

#ifdef DEBUG_VER
IF (isfedofs) THEN
  tsize = SIZE(obj%fedofs)
  isok = ivar0 .LE. tsize
  CALL AssertError1(isok, myName, &
                    'ivar='//ToString(ivar0)//' is greater than size of &
                    &obj%fedofs='//ToString(tsize))

  isok = ASSOCIATED(obj%fedofs(ivar0)%ptr)
  CALL AssertError1(isok, myName, &
                   'obj%fedofs('//ToString(ivar0)//')%ptr is not associated.')
END IF
#endif

IF (isfedofs) tnode = dbc%GetTotalNodeNum(obj%fedofs(ivar0)%ptr)
IF (isfedof) tnode = dbc%GetTotalNodeNum(obj%fedof)

ans = tnode * tspace * ttime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodeLoc2

!----------------------------------------------------------------------------
!                                                           GetTotalNodeLoc3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeLoc3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodeLoc3()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(dbc)

ans = 0

DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = ASSOCIATED(dbc(ii)%ptr)
  IF (.NOT. isok) CYCLE
#endif
  ans = ans + obj%GetTotalNodeLoc(dbc=dbc(ii)%ptr, ivar=ivar)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodeLoc3

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc1()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalNodeLoc(globalNode=globalNode, ivar=ivar, &
                            spaceCompo=spaceCompo, timeCompo=timeCompo)

CALL Reallocate(ans, tsize)

CALL obj%GetNodeLoc_(globalNode=globalNode, ivar=ivar, &
                     spaceCompo=spaceCompo, timeCompo=timeCompo, &
                     ans=ans, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetNodeLoc1

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc2()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalNodeLoc(dbc=dbc, ivar=ivar)
CALL Reallocate(ans, tsize)
CALL obj%GetNodeLoc_(ans=ans, tsize=tsize, dbc=dbc, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeLoc2

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc3()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalNodeLoc(dbc=dbc, ivar=ivar)
CALL Reallocate(ans, tsize)
CALL obj%GetNodeLoc_(ans=ans, tsize=tsize, dbc=dbc, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeLoc3

!----------------------------------------------------------------------------
!                                                                 GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc_1()"
#endif

INTEGER(I4B) :: spaceCompo0(TypeDOFOpt%maxPhysicalVars), &
                timeCompo0(TypeDOFOpt%maxPhysicalVars)
INTEGER(I4B) :: ivar0, ttime, tspace, ii, jj
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Check errors here
ivar0 = Input(default=math%one_i, option=ivar)

#ifdef DEBUG_VER
isok = PRESENT(islocal)
IF (isok) THEN
  CALL AssertError1(islocal, myName, "islocal should be .true.")
END IF
#endif

tspace = 1
spaceCompo0(1:tspace) = 1
isok = PRESENT(spaceCompo)

IF (isok) THEN
  tspace = SIZE(spaceCompo)

#ifdef DEBUG_VER
  isok = tspace .LE. TypeDOFOpt%maxPhysicalVars
  CALL AssertError1(isok, myName, &
                    "Size of spaceCompo = "//ToString(tspace)// &
                    " is greater than TypeDOFOpt%maxPhysicalVars = "// &
                    ToString(TypeDOFOpt%maxPhysicalVars))
#endif

  spaceCompo0(1:tspace) = spaceCompo(1:tspace)
END IF

ttime = 1
timeCompo0(1:ttime) = 1
isok = PRESENT(timeCompo)

IF (isok) THEN
  ttime = SIZE(timeCompo)

#ifdef DEBUG_VER
  isok = ttime .LE. TypeDOFOpt%maxPhysicalVars
  CALL AssertError1(isok, myName, &
                    "Size of timeCompo = "//ToString(ttime)// &
                    " is greater than TypeDOFOpt%maxPhysicalVars = "// &
                    ToString(TypeDOFOpt%maxPhysicalVars))
#endif

  timeCompo0(1:ttime) = timeCompo(1:ttime)
END IF

tsize = 0
DO ii = 1, ttime
  CALL GetNodeLoc_( &
    obj=obj%dof, nodenum=globalNode, ivar=ivar0, &
    spaceCompo=spaceCompo0(1:tspace), timeCompo=timeCompo0(ii), &
    ans=ans(tsize + 1:), tsize=jj)
  tsize = tsize + jj
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetNodeLoc_1

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc_2()"
INTEGER(I4B) :: tfedofs
#endif

INTEGER(I4B), ALLOCATABLE :: globalNode(:)
INTEGER(I4B) :: tPhysicalVars, spaceCompo(1), ivar0, tnode, &
            timeCompo(TypeDOFOpt%maxPhysicalVars), iNodeOnNode, iNodeOnEdge, &
                iNodeOnFace, timeCompo2(TypeDOFOpt%maxTimeCompo)
LOGICAL(LGT) :: isok, isfedof, isfedofs
CLASS(FEDOF_), POINTER :: fedof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ivar0 = Input(default=math%one_i, option=ivar)

tPhysicalVars = obj%GetTotalPhysicalVars()

#ifdef DEBUG_VER
isok = tPhysicalVars .LE. TypeDOFOpt%maxPhysicalVars
CALL AssertError1(isok, myName, &
                  "Total physical variables = "//ToString(tPhysicalVars)// &
                  " is greater than TypeDOFOpt%maxPhysicalVars = "// &
                  ToString(TypeDOFOpt%maxPhysicalVars))
#endif

timeCompo(1:tPhysicalVars) = obj%GetTimeCompo(tPhysicalVars)
spaceCompo(1) = dbc%GetDOFNo()

isfedof = ASSOCIATED(obj%fedof)
isfedofs = ALLOCATED(obj%fedofs)

#ifdef DEBUG_VER
isok = isfedof .OR. isfedofs
CALL AssertError1(isok, myName, &
                  "Neither fedof is associated nor fedofs is allocated.")
isok = .NOT. (isfedof .AND. isfedofs)
CALL AssertError1(isok, myName, &
                  "Both fedof and fedofs are allocated/associated.")
#endif

#ifdef DEBUG_VER
IF (isfedofs) THEN
  tfedofs = SIZE(obj%fedofs)
  isok = ivar0 .LE. tfedofs
  CALL AssertError1(isok, myName, &
                    'ivar='//ToString(ivar0)//' is greater than size of &
                    &obj%fedofs='//ToString(tfedofs))

  isok = ASSOCIATED(obj%fedofs(ivar0)%ptr)
  CALL AssertError1(isok, myName, &
                   'obj%fedofs('//ToString(ivar0)//')%ptr is not associated.')

END IF
#endif

IF (isfedofs) fedof => obj%fedofs(ivar0)%ptr
IF (isfedof) fedof => obj%fedof

tnode = dbc%GetTotalNodeNum(fedof)
ALLOCATE (globalNode(tnode))
CALL dbc%GetNodeNumber( &
  nodeNum=globalNode, tsize=tnode, fedof=fedof, iNodeOnNode=iNodeOnNode, &
  iNodeOnEdge=iNodeOnEdge, iNodeOnFace=iNodeOnFace)

#ifdef DEBUG_VER
isok = timeCompo(ivar0) .LE. TypeDOFOpt%maxTimeCompo
CALL AssertError1(isok, myName, &
                  "timeCompo("//ToString(ivar0)//") = "// &
                  ToString(timeCompo(ivar0))//" is greater than "// &
                  "TypeDOFOpt%maxTimeCompo = "// &
                  ToString(TypeDOFOpt%maxTimeCompo))
#endif

timeCompo2(1:timeCompo(ivar0)) = Arange(math%one_i, timeCompo(ivar0))

CALL obj%GetNodeLoc_( &
  globalNode=globalNode, ans=ans, tsize=tsize, ivar=ivar0, &
  spaceCompo=spaceCompo, timeCompo=timeCompo2(1:timeCompo(ivar0)))

IF (ALLOCATED(globalNode)) DEALLOCATE (globalNode)
fedof => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeLoc_2

!----------------------------------------------------------------------------
!                                                                GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeLoc_3()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, tdbc, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tdbc = SIZE(dbc)

#ifdef DEBUG_VER
DO ii = 1, tdbc
  isok = ASSOCIATED(dbc(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'dbc('//ToString(ii)//')%ptr is not associated.')
END DO
#endif

tsize = 0
DO ii = 1, tdbc
  CALL obj%GetNodeLoc_(dbc=dbc(ii)%ptr, ivar=ivar, ans=ans(tsize + 1:), &
                       tsize=jj)
  tsize = tsize + jj
END DO
!! Here we are calling GetNodeLoc_2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeLoc_3

!----------------------------------------------------------------------------
!                                                    GetTotalDirichletBCIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDirichletBCIndex
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDirichletBCIndex()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%dbc)
ans = 0
IF (isok) ans = obj%GetTotalNodeLoc(dbc=obj%dbc, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDirichletBCIndex

!----------------------------------------------------------------------------
!                                                         GetDirichletBCIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCIndex
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCIndex()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalDirichletBCIndex(ivar=ivar)
CALL Reallocate(ans, tsize)
CALL obj%GetDirichletBCIndex_(ans=ans, tsize=tsize, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCIndex

!----------------------------------------------------------------------------
!                                                       GetDirichletBCIndex_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCIndex_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCIndex_()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%dbc)
tsize = 0
IF (isok) CALL obj%GetNodeLoc_(dbc=obj%dbc, ivar=ivar, ans=ans, tsize=tsize)
!! Here we are calling GetNodeLoc_3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCIndex_

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
