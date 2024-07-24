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

SUBMODULE(AbstractBC_Class) SetMethods
USE GlobalData, ONLY: CHAR_LF
USE ReallocateUtility, ONLY: Reallocate
USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
LOGICAL(LGT) :: abool

INTEGER(I4B) :: acase

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL set_check_error(obj, constantNodalValue, spaceNodalValue, &
                     timeNodalValue, spaceTimeNodalValue, userFunction)
#endif

abool = (.NOT. obj%isUserFunction) .AND. (.NOT. obj%isUseExternal)

acase = 0
IF (abool) THEN
  IF (PRESENT(constantNodalValue)) THEN
    acase = 1
  ELSE IF (PRESENT(spaceNodalValue)) THEN
    acase = 2
  ELSEIF (PRESENT(timeNodalValue)) THEN
    acase = 3
  ELSEIF (PRESENT(spaceTimeNodalValue)) THEN
    acase = 4
  END IF

ELSE

  IF (obj%isUserFunction .AND. PRESENT(userFunction)) THEN
    acase = 5
  END IF

END IF

SELECT CASE (acase)
CASE (1)
  ! constantNodalValue
  obj%nrow = 1
  obj%ncol = 1
  CALL Reallocate(obj%nodalvalue, obj%nrow, obj%ncol)
  obj%nodalvalue(1:obj%nrow, 1:obj%ncol) = constantNodalValue

CASE (2)
  ! spaceNodalValue
  obj%nrow = SIZE(spaceNodalValue, 1)
  obj%ncol = 1
  CALL Reallocate(obj%nodalvalue, obj%nrow, obj%ncol)
  obj%nodalvalue(1:obj%nrow, 1) = spaceNodalValue

CASE (3)
  ! timeNodalValue
  obj%nrow = SIZE(timeNodalValue)
  obj%ncol = 1

  CALL Reallocate(obj%nodalvalue, obj%nrow, obj%ncol)
  obj%nodalvalue(1:obj%nrow, 1) = timeNodalValue

CASE (4)
  obj%nrow = SIZE(spaceTimeNodalValue, 1)
  obj%ncol = SIZE(spaceTimeNodalValue, 2)
  CALL Reallocate(obj%NodalValue, obj%nrow, obj%ncol)
  obj%nodalvalue(1:obj%nrow, 1:obj%ncol) = spaceTimeNodalValue

CASE (5)
  obj%func => userFunction

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[CONFIG ERROR] :: Invalid case')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_check_error(obj, constantNodalValue, spaceNodalValue, &
                           timeNodalValue, spaceTimeNodalValue, userFunction)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: constantNodalValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: spaceNodalValue(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: timeNodalValue(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: spaceTimeNodalValue(:, :)
  TYPE(UserFunction_), TARGET, OPTIONAL, INTENT(IN) :: userFunction

  LOGICAL(LGT) :: isConstVal, isSpaceVal, isTimeVal, isSTVal, &
                  isUserFunction, bool1, bool2, notFunc_notExt

  CHARACTER(*), PARAMETER :: myname = "set_check_error()"

  IF (.NOT. obj%isInitiated) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[CONFIG ERROR ] :: AbstractBC_ object is not initiated.')
    RETURN
  END IF

  notFunc_notExt = (.NOT. obj%isUserFunction) .AND. (.NOT. obj%isUseExternal)

  isUserFunction = PRESENT(userFunction)
  isConstVal = PRESENT(constantNodalValue)
  isSpaceVal = PRESENT(spaceNodalValue)
  isTimeVal = PRESENT(timeNodalValue)
  isSTVal = PRESENT(spaceTimeNodalValue)

  IF (notFunc_notExt .AND. isUserFunction) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
               "[CONFIG ERROR] :: AbstractBC_::obj is initiated "//CHAR_LF// &
             "with useFunction=.FALSE. and isUseExternal=.FALSE."//CHAR_LF// &
                      "So you cannot provide userFunction.")
    RETURN
  END IF

  bool1 = notFunc_notExt .AND. isConstVal
  bool2 = bool1 .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%constant)
  IF (bool2) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
                      "with nodalValueType=Constant "//CHAR_LF// &
                      'So, constantNodalValue cannot be present.')
    RETURN
  END IF

  !! spaceNodalValue
  bool1 = notFunc_notExt .AND. isSpaceVal
  bool2 = bool1 .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%space)
  IF (bool2) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
                      "with nodalValueType=Space"//CHAR_LF// &
                      'So, spaceNodalValue cannot be present.')
    RETURN
  END IF

  bool1 = notFunc_notExt .AND. isTimeVal
  bool2 = bool1 .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%time)
  IF (bool2) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
                      "with nodalValueType=Time"//CHAR_LF// &
                      'So, timeNodalValue cannot be present.')
    RETURN
  END IF

  bool1 = notFunc_notExt .AND. isSTVal
  bool2 = bool1 .AND. (obj%nodalValueType .NE. TypeFeVariableOpt%spacetime)
  IF (bool2) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
               "[CONFIG ERROR] :: AbstractBC_::obj is not initiated with "// &
                      " nodalValueType=SpaceTime"// &
                      CHAR_LF// &
                      'So, spaceTimeNodalValue cannot be present')
    RETURN
  END IF

  IF (isUserFunction) THEN

    bool1 = obj%isUserFunction
    IF (.NOT. bool1) THEN
      CALL e%RaiseError(modName//'::'//myName//" - "// &
           "[CONFIG ERROR] :: AbstractBC_::obj is not correctly initiated"// &
                        " for userFunction")
      RETURN
    END IF
  END IF
END SUBROUTINE set_check_error

!----------------------------------------------------------------------------
!                                                 SetElemToLocalBoundary
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElemToLocalBoundary
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetElemToLocalBoundary()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL set_elem_to_faces(obj)
CALL set_elem_to_edges(obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetElemToLocalBoundary

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_elem_to_faces(obj)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj

  CHARACTER(*), PARAMETER :: myName = "set_elem_to_faces()"
  LOGICAL(LGT), PARAMETER :: onlyBoundaryElement = .TRUE., yes = .TRUE., &
                             no = .FALSE.

  ! INTEGER(I4B), PARAMETER :: expandFactor = 2

  INTEGER(I4B) :: ii, jj, nsd, tsize, indx(4), tmeshid, maxnode2elem, &
                  localFaceNumber, localCellNumber
  INTEGER(I4B), POINTER :: intptr(:)
  CLASS(AbstractMesh_), POINTER :: bmesh, cmesh
  INTEGER(I4B), ALLOCATABLE :: bndy2cell(:), bndy_con(:), n2e(:), &
                               cell_con(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (obj%isElemToFace) RETURN

  obj%isElemToFace = .TRUE.

  nsd = obj%dom%GetNSD()

  tsize = nsd - 1
  CALL obj%boundary%GetMeshIDPointer(dim=tsize, ans=intptr, tsize=tmeshid)

  isok = tmeshid .EQ. 0 .OR. (nsd .LT. 2)

  IF (isok) THEN
    obj%tElemToFace = 0
    CALL Reallocate(obj%elemToFace, 0, 0)
    CALL finishme
    RETURN
  END IF

  cmesh => obj%dom%GetMeshPointer(dim=nsd)
  maxnode2elem = cmesh%GetMaxNodeToElements()
  ALLOCATE (n2e(maxnode2elem))

  ! here tsize = nsd - 1
  bmesh => obj%dom%GetMeshPointer(dim=tsize)

  indx = 0

  indx(1) = bmesh%GetTotalElements(meshid=intptr(1:tmeshid))
  tsize = indx(1)
  ALLOCATE (bndy2cell(tsize))
  CALL Reallocate(obj%elemToFace, 2, tsize)
  obj%tElemToFace = tsize

  ! INFO: here we are getting the local element number of boundary
  CALL bmesh%GetElemNum_(meshid=intptr, islocal=yes, ans=bndy2cell, &
                         tsize=indx(2))

  tsize = cmesh%GetMaxNNE()
  ALLOCATE (bndy_con(tsize), cell_con(tsize))

  ! INFO: A loop over all boundary elements, tsize is total num of bndy elem
  tsize = indx(2)
  boundary_loop: DO ii = 1, tsize

    CALL bmesh%GetConnectivity_(globalElement=bndy2cell(ii), &
                            ans=bndy_con, tsize=indx(3), islocal=yes, opt="V")

    isok = indx(3) .NE. 0
    IF (.NOT. isok) CYCLE

    !INFO: select a node, bndy_con(1)
    CALL cmesh%GetNodeToElements_(ans=n2e, tsize=indx(4), &
                                  globalNode=bndy_con(1), islocal=no)

    !INFO: loop over all elements connected to con(1)
    node_to_element_loop: DO jj = 1, indx(4)
      localCellNumber = cmesh%GetLocalElemNumber(globalElement=n2e(jj))

      CALL cmesh%FindFace(globalElement=localCellNumber, &
                          faceCon=bndy_con(1:indx(3)), &
                          isFace=isok, islocal=yes, &
                          localFaceNumber=localFaceNumber, &
                          onlyBoundaryElement=onlyBoundaryElement)

      IF (isok) THEN
        obj%elemToFace(1, ii) = localCellNumber
        obj%elemToFace(2, ii) = localFaceNumber
        EXIT node_to_element_loop
      END IF

    END DO node_to_element_loop

  END DO boundary_loop

  CALL finishme

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

CONTAINS
  SUBROUTINE finishme
    IF (ALLOCATED(n2e)) DEALLOCATE (n2e)
    IF (ALLOCATED(bndy_con)) DEALLOCATE (bndy_con)
    IF (ALLOCATED(cell_con)) DEALLOCATE (cell_con)
    IF (ALLOCATED(bndy2cell)) DEALLOCATE (bndy2cell)
    intptr => NULL()
    bmesh => NULL()
    cmesh => NULL()
  END SUBROUTINE finishme

END SUBROUTINE set_elem_to_faces

!----------------------------------------------------------------------------
!                                                          set_elem_to_edges
!----------------------------------------------------------------------------

SUBROUTINE set_elem_to_edges(obj)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj

  CHARACTER(*), PARAMETER :: myName = "set_elem_to_edges()"
  LOGICAL(LGT), PARAMETER :: onlyBoundaryElement = .TRUE., yes = .TRUE., &
                             no = .FALSE.
  INTEGER(I4B), PARAMETER :: dim = 1

  ! INTEGER(I4B), PARAMETER :: expandFactor = 2

  INTEGER(I4B) :: ii, jj, tsize, indx(4), tmeshid, maxnode2elem, &
                  localEdgeNumber, localCellNumber, nsd
  INTEGER(I4B), POINTER :: intptr(:)
  CLASS(AbstractMesh_), POINTER :: bmesh, cmesh
  INTEGER(I4B), ALLOCATABLE :: bndy2cell(:), bndy_con(:), n2e(:), &
                               cell_con(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (obj%isElemToEdge) RETURN

  obj%isElemToEdge = .TRUE.

  nsd = obj%dom%GetNSD()

  CALL obj%boundary%GetMeshIDPointer(dim=dim, ans=intptr, tsize=tmeshid)

  isok = tmeshid .EQ. 0 .OR. (nsd .NE. 3)

  IF (isok) THEN
    obj%tElemToEdge = 0
    CALL Reallocate(obj%elemToEdge, 0, 0)
    CALL finishme
    RETURN
  END IF

  cmesh => obj%dom%GetMeshPointer(dim=nsd)
  maxnode2elem = cmesh%GetMaxNodeToElements()
  ALLOCATE (n2e(maxnode2elem))

  bmesh => obj%dom%GetMeshPointer(dim=dim)

  indx = 0

  indx(1) = bmesh%GetTotalElements(meshid=intptr(1:tmeshid))
  tsize = indx(1)
  ALLOCATE (bndy2cell(tsize))
  CALL Reallocate(obj%elemToEdge, 2, tsize)
  obj%tElemToEdge = tsize

  ! INFO: here we are getting the local element number of boundary
  CALL bmesh%GetElemNum_(meshid=intptr, islocal=yes, ans=bndy2cell, &
                         tsize=indx(2))

  tsize = cmesh%GetMaxNNE()
  ALLOCATE (bndy_con(tsize), cell_con(tsize))

  ! INFO: A loop over all boundary elements, tsize is total num of bndy elem
  tsize = indx(2)
  boundary_loop: DO ii = 1, tsize

    CALL bmesh%GetConnectivity_(globalElement=bndy2cell(ii), &
                            ans=bndy_con, tsize=indx(3), islocal=yes, opt="V")

    isok = indx(3) .NE. 0
    IF (.NOT. isok) CYCLE

    !INFO: select a node, bndy_con(1)
    CALL cmesh%GetNodeToElements_(ans=n2e, tsize=indx(4), &
                                  globalNode=bndy_con(1), islocal=no)

    !INFO: loop over all elements connected to con(1)
    node_to_element_loop: DO jj = 1, indx(4)
      localCellNumber = cmesh%GetLocalElemNumber(globalElement=n2e(jj))

      CALL cmesh%FindEdge(globalElement=localCellNumber, &
                          edgeCon=bndy_con(1:indx(3)), &
                          isEdge=isok, islocal=yes, &
                          localEdgeNumber=localEdgeNumber, &
                          onlyBoundaryElement=onlyBoundaryElement)

      IF (isok) THEN
        obj%elemToEdge(1, ii) = localCellNumber
        obj%elemToEdge(2, ii) = localEdgeNumber
        EXIT node_to_element_loop
      END IF

    END DO node_to_element_loop

  END DO boundary_loop

  DEALLOCATE (n2e, bndy_con, cell_con)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

CONTAINS
  SUBROUTINE finishme
    IF (ALLOCATED(n2e)) DEALLOCATE (n2e)
    IF (ALLOCATED(bndy_con)) DEALLOCATE (bndy_con)
    IF (ALLOCATED(cell_con)) DEALLOCATE (cell_con)
    IF (ALLOCATED(bndy2cell)) DEALLOCATE (bndy2cell)
    intptr => NULL()
    bmesh => NULL()
    cmesh => NULL()
  END SUBROUTINE finishme

END SUBROUTINE set_elem_to_edges

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
