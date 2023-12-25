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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) SetMethods
USE BaseMethod
USE Mesh_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: val1, val2, val3, nodenum(SIZE(globalNode))
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

! check: this routine should not be called for rectangle matrix
IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

#ifdef DEBUG_VER
! check:
val1 = SIZE(VALUE, 1)
val2 = SIZE(VALUE, 2)
val3 = (.tdof.obj%mat%csr%idof) * SIZE(globalNode)
IF (&
    &      val1 .NE. val2  &
    & .OR. val1 .NE. val3) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
   & "value is not square matrix, or its shape is inconsistent "// &
   & "with the degree of freedom stored in MatrixField")
END IF
#endif

nodenum = obj%domain%GetLocalNodeNumber(globalNode)
IF (add0) THEN
  CALL Add(obj=obj%mat, VALUE=VALUE, nodenum=nodenum, &
    & storageFMT=storageFMT, scale=scale0)
ELSE
  CALL Set(obj=obj%mat, nodenum=nodenum, VALUE=VALUE, storageFMT=storageFMT)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
LOGICAL(LGT) :: add0, isnode
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)
isnode = PRESENT(globalNode)

! check: this routine should not be called for rectangle matrix
IF (isnode .AND. obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

! Add
IF (add0 .AND. isnode) THEN
  CALL Add(obj=obj%mat,  &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & scale=scale0, &
    & VALUE=VALUE)
  RETURN
END IF

IF (add0) THEN
  CALL Add(obj=obj%mat, VALUE=VALUE, scale=scale0)
  RETURN
END IF

! Set
IF (isnode) THEN
  ! check: this routine should not be called for rectangle matrix
  CALL Set(obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: inodenum0, jnodenum0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (obj%isRectangle) THEN
  inodenum0 = obj%domains(1)%ptr%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jnodenum)
ELSE
  inodenum0 = obj%domain%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domain%GetLocalNodeNumber(jnodenum)
END IF

IF (add0) THEN
  CALL Add(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: inodenum0(SIZE(iNodeNum)), jnodenum0(SIZE(jNodeNum))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (obj%isRectangle) THEN
  inodenum0 = obj%domains(1)%ptr%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jnodenum)
ELSE
  inodenum0 = obj%domain%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domain%GetLocalNodeNumber(jnodenum)
END IF

IF (add0) THEN
  CALL Add(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & VALUE=VALUE, &
    & scale=scale0)

ELSE

  CALL Set(obj=obj%mat, &
      & inodenum=inodenum0, &
      & jnodenum=jnodenum0, &
      & ivar=ivar, &
      & jvar=jvar, &
      & VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: inodenum0(SIZE(iNodeNum)), jnodenum0(SIZE(jNodeNum))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (obj%isRectangle) THEN
  inodenum0 = obj%domains(1)%ptr%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jnodenum)
ELSE
  inodenum0 = obj%domain%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domain%GetLocalNodeNumber(jnodenum)
END IF

IF (add0) THEN

  CALL Add(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)

ELSE

  CALL Set(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: inodenum0, jnodenum0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (obj%isRectangle) THEN
  inodenum0 = obj%domains(1)%ptr%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jnodenum)
ELSE
  inodenum0 = obj%domain%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domain%GetLocalNodeNumber(jnodenum)
END IF

IF (add0) THEN
  CALL Add(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE, &
    & scale=scale0)
ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & idof=idof, &
    & jdof=jdof, &
    & VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: inodenum0, jnodenum0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (obj%isRectangle) THEN
  inodenum0 = obj%domains(1)%ptr%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jnodenum)
ELSE
  inodenum0 = obj%domain%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domain%GetLocalNodeNumber(jnodenum)
END IF

IF (add0) THEN
  CALL Add(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)

ELSE

  CALL Set(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: inodenum0(SIZE(iNodeNum)), jnodenum0(SIZE(jNodeNum))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (obj%isRectangle) THEN
  inodenum0 = obj%domains(1)%ptr%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jnodenum)
ELSE
  inodenum0 = obj%domain%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domain%GetLocalNodeNumber(jnodenum)
END IF

IF (add0) THEN
  CALL Add(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)

ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: inodenum0(SIZE(iNodeNum)), jnodenum0(SIZE(jNodeNum))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (obj%isRectangle) THEN
  inodenum0 = obj%domains(1)%ptr%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jnodenum)
ELSE
  inodenum0 = obj%domain%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domain%GetLocalNodeNumber(jnodenum)
END IF

IF (add0) THEN
  CALL Add(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)

ELSE
  CALL Set(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: inodenum0(SIZE(iNodeNum)), jnodenum0(SIZE(jNodeNum))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

IF (obj%isRectangle) THEN
  inodenum0 = obj%domains(1)%ptr%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jnodenum)
ELSE
  inodenum0 = obj%domain%GetLocalNodeNumber(inodenum)
  jnodenum0 = obj%domain%GetLocalNodeNumber(jnodenum)
END IF

IF (add0) THEN

  CALL Add(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)

ELSE

  CALL Set(obj=obj%mat, &
    & inodenum=inodenum0, &
    & jnodenum=jnodenum0, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"
REAL(DFP) :: scale0
LOGICAL(LGT) :: add0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)

SELECT TYPE (VALUE)
CLASS IS (MatrixField_)
  IF (add0) THEN
    CALL Add(obj=obj%mat, VALUE=VALUE%mat, scale=scale0,  &
      & isSameStructure=.TRUE.)
    RETURN
  END IF

  CALL Set(obj=obj%mat, VALUE=VALUE%mat, scale=scale0)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: This method is available for MatrixField_ only')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                          SetFromSTMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFromSTMatrix
CHARACTER(*), PARAMETER :: myName = "obj_SetFromSTMatrix()"
INTEGER(I4B) :: spaceCompo
TYPE(DOF_), POINTER :: dof_obj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

SELECT TYPE (VALUE)
CLASS is (MatrixField_)

  dof_obj => GetDOFPointer(obj%mat, 1)
  spaceCompo = dof_obj.spacecomponents.1
  CALL obj_SetFromSTMatrix_help(obj=obj, VALUE=VALUE,  &
    & dom=obj%domain, a=a, b=b, spaceCompo=spaceCompo)

CLASS default
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found.')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_SetFromSTMatrix

SUBROUTINE obj_SetFromSTMatrix_help(obj, VALUE, &
  & dom, spaceCompo, a, b)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  CLASS(MatrixField_), INTENT(INOUT) :: VALUE
  CLASS(Domain_), INTENT(INOUT) :: dom
    !! Space-time matrix field
  INTEGER(I4B), INTENT(IN) :: spaceCompo
  !!
  INTEGER(I4B), INTENT(IN) :: a
    !! itimecompo
  INTEGER(I4B), INTENT(IN) :: b
    !! jtimecompo

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_SetFromSTMatrix_help()"
  INTEGER(I4B) :: tmesh, id, nsd, iel, nns, ispacecompo, jspacecompo,  &
    & r1, r2, c1, c2
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: problem
  REAL(DFP), ALLOCATABLE :: elem_value(:, :)
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  NULLIFY (meshptr, refelem)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)

    problem = .NOT. ASSOCIATED(meshptr)
    IF (problem) CYCLE

    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)
    CALL Reallocate(nptrs, nns)
    CALL Reallocate(elem_value, spaceCompo * nns, spaceCompo * nns)

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      problem = .NOT. meshptr%isElementPresent(iel)
      IF (problem) CYCLE

      nptrs = meshptr%GetConnectivity(iel)

      DO ispacecompo = 1, spaceCompo
        r1 = 1 + (ispacecompo - 1) * nns
        r2 = ispacecompo * nns
        DO jspacecompo = 1, spaceCompo
          c1 = 1 + (jspacecompo - 1) * nns
          c2 = jspacecompo * nns
          CALL VALUE%Get(inodenum=nptrs, jnodenum=nptrs,  &
            & VALUE=elem_value(r1:r2, c1:c2),  &
            & ivar=1, jvar=1, ispacecompo=ispacecompo,  &
            & jspacecompo=jspacecompo,  &
            & itimecompo=a, jtimecompo=b)

        END DO
      END DO

      CALL obj%Set(globalNode=nptrs, VALUE=elem_value, storageFMT=FMT_DOF)

    END DO

  END DO

  NULLIFY (meshptr, refelem)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(elem_value)) DEALLOCATE (elem_value)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE obj_SetFromSTMatrix_help

!----------------------------------------------------------------------------
!                                                             SetToSTMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetToSTMatrix
CHARACTER(*), PARAMETER :: myName = "obj_SetToSTMatrix()"
INTEGER(I4B) :: spaceCompo
TYPE(DOF_), POINTER :: dof_obj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

SELECT TYPE (VALUE)
CLASS is (MatrixField_)

  dof_obj => GetDOFPointer(obj%mat, 1)
  spaceCompo = dof_obj.spacecomponents.1
  CALL obj_SetToSTMatrix_help(obj=obj, VALUE=VALUE,  &
    & dom=obj%domain, spaceCompo=spaceCompo,  &
    & a=a, b=b)

CLASS default
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found.')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_SetToSTMatrix

SUBROUTINE obj_SetToSTMatrix_help(obj, VALUE, dom, spaceCompo, a, b)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! Space time matrix
  CLASS(MatrixField_), INTENT(INOUT) :: VALUE
    !! space matrix
  CLASS(Domain_), INTENT(INOUT) :: dom
    !! Space-time matrix field
  INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space-components
  INTEGER(I4B), INTENT(IN) :: a
    !! itimecompo
  INTEGER(I4B), INTENT(IN) :: b
    !! jtimecompo

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_SetToSTMatrix_help()"
  INTEGER(I4B) :: tmesh, id, nsd, iel, nns, ispacecompo, jspacecompo,  &
    & idof, jdof
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: problem
  REAL(DFP), ALLOCATABLE :: elem_value(:, :)
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  TYPE(DOF_), POINTER :: idof_obj, jdof_obj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  NULLIFY (meshptr, refelem, idof_obj, jdof_obj)

  idof_obj => GetDOFPointer(obj%mat, 1)
  jdof_obj => GetDOFPointer(obj%mat, 2)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)

    problem = .NOT. ASSOCIATED(meshptr)
    IF (problem) CYCLE

    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)
    CALL Reallocate(nptrs, nns)
    CALL Reallocate(elem_value, nns, nns)

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      problem = .NOT. meshptr%isElementPresent(iel)
      IF (problem) CYCLE

      nptrs = meshptr%GetConnectivity(iel)

      DO ispacecompo = 1, spaceCompo
        idof = GetIDOF(obj=idof_obj, ivar=1, spaceCompo=ispacecompo,  &
          & timeCompo=a)

        DO jspacecompo = 1, spaceCompo
          jdof = GetIDOF(obj=jdof_obj, ivar=1, spaceCompo=jspacecompo,  &
            & timeCompo=b)

          CALL VALUE%Get(inodenum=nptrs, jnodenum=nptrs,  &
            & VALUE=elem_value,  &
            & ivar=1, jvar=1, ispacecompo=ispacecompo,  &
            & jspacecompo=jspacecompo,  &
            & itimecompo=1, jtimecompo=1)

          CALL obj%Set(inodenum=nptrs,  &
            & jnodenum=nptrs, VALUE=elem_value,  &
            & ivar=1, jvar=1, idof=idof, jdof=jdof)

        END DO
      END DO

    END DO

  END DO

  NULLIFY (meshptr, refelem, idof_obj, jdof_obj)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(elem_value)) DEALLOCATE (elem_value)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE obj_SetToSTMatrix_help

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
