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
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE InputUtility, ONLY: Input

USE DOF_Method, ONLY: OPERATOR(.tdof.), &
                      OPERATOR(.spacecomponents.), &
                      OPERATOR(.timecomponents.)

USE CSRMatrix_Method, ONLY: Add, Set, GetDOFPointer
USE BaseType, ONLY: DOF_
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: val1, val2, val3
LOGICAL(LGT) :: problem
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! check: this routine should not be called for rectangle matrix
#ifdef DEBUG_VER
isok = .NOT. obj%isRectangle
CALL AssertError1(isok, myName, &
                  'This routine is not for rectangle matrix')
#endif

#ifdef DEBUG_VER
val1 = SIZE(VALUE, 1)
val2 = SIZE(VALUE, 2)
isok = val1 .EQ. val2
CALL AssertError1(isok, myName, &
                  'value is not square matrix, nrow='//ToString(val1)// &
                  ' ncol='//ToString(val2))
#endif

#ifdef DEBUG_VER
val3 = (.tdof.obj%mat%csr%idof) * SIZE(globalNode)
isok = val1 .EQ. val3
CALL AssertError1(isok, myName, &
                  "The shape of value is inconsistent, &
                   &nrow of value = "//ToString(val1)//" val3 = "// &
                   ToString(val3))
#endif

#include "./localNodeError.F90"

abool = Input(default=.FALSE., option=addContribution)

IF (abool) THEN
  areal = Input(default=1.0_DFP, option=scale)
  CALL Add(obj=obj%mat, VALUE=VALUE, nodenum=globalNode, scale=areal, &
           storageFMT=storageFMT)
  RETURN
END IF

CALL Set(obj=obj%mat, nodenum=globalNode, VALUE=VALUE, storageFMT=storageFMT)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
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
                        '[START] ')
#endif

#ifdef DEBUG_VER

IF (PRESENT(islocal)) THEN
#include "./localNodeError.F90"
END IF

#endif

isnode = PRESENT(globalNode)

#ifdef DEBUG_VER
! check: this routine should not be called for rectangle matrix
IF (isnode .AND. obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

#endif

add0 = Input(default=.FALSE., option=addContribution)
! Add
IF (add0 .AND. isnode) THEN
  scale0 = Input(default=1.0_DFP, option=scale)
  CALL Add(obj=obj%mat, nodenum=globalNode, scale=scale0, VALUE=VALUE)
  RETURN
END IF

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)
  CALL Add(obj=obj%mat, VALUE=VALUE, scale=scale0)
  RETURN
END IF

! Set
IF (isnode) THEN
  ! check: this routine should not be called for rectangle matrix
  CALL Set(obj=obj%mat, nodenum=globalNode, VALUE=VALUE)
  RETURN
END IF

CALL Set(obj=obj%mat, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
#endif

LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=.FALSE., option=addContribution)

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           idof=idof, jdof=jdof, VALUE=VALUE, scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, idof=idof, &
         jdof=jdof, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=.FALSE., option=addContribution)

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, ivar=ivar, &
           jvar=jvar, VALUE=VALUE, scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, ivar=ivar, &
         jvar=jvar, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=.FALSE., option=addContribution)

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)

  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
           ivar=ivar, jvar=jvar, idof=idof, jdof=jdof, VALUE=VALUE, &
           scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
         ivar=ivar, jvar=jvar, idof=idof, jdof=jdof, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=.FALSE., option=addContribution)

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
        ivar=ivar, jvar=jvar, idof=idof, jdof=jdof, VALUE=VALUE, scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
         ivar=ivar, jvar=jvar, idof=idof, jdof=jdof, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=.FALSE., option=addContribution)

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)

  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
       ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, itimecompo=itimecompo, &
           jspacecompo=jspacecompo, jtimecompo=jtimecompo, VALUE=VALUE, &
           scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
       ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, itimecompo=itimecompo, &
         jspacecompo=jspacecompo, jtimecompo=jtimecompo, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=.FALSE., option=addContribution)

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
       ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, itimecompo=itimecompo, &
    jspacecompo=jspacecompo, jtimecompo=jtimecompo, VALUE=VALUE, scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
       ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, itimecompo=itimecompo, &
         jspacecompo=jspacecompo, jtimecompo=jtimecompo, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=.FALSE., option=addContribution)

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)
  CALL Add(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, ivar=ivar, &
           jvar=jvar, ispacecompo=ispacecompo, itimecompo=itimecompo, &
    jspacecompo=jspacecompo, jtimecompo=jtimecompo, VALUE=VALUE, scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, inodenum=inodenum, jnodenum=jnodenum, &
       ivar=ivar, jvar=jvar, ispacecompo=ispacecompo, itimecompo=itimecompo, &
         jspacecompo=jspacecompo, jtimecompo=jtimecompo, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

add0 = Input(default=.FALSE., option=addContribution)

IF (add0) THEN
  scale0 = Input(default=1.0_DFP, option=scale)

  CALL Add(obj=obj%mat, &
    & inodenum=inodenum, &
    & jnodenum=jnodenum, &
    & ivar=ivar, &
    & jvar=jvar, &
    & ispacecompo=ispacecompo, &
    & itimecompo=itimecompo, &
    & jspacecompo=jspacecompo, &
    & jtimecompo=jtimecompo, &
    & VALUE=VALUE, &
    & scale=scale0)
  RETURN
END IF

CALL Set(obj=obj%mat, &
  & inodenum=inodenum, &
  & jnodenum=jnodenum, &
  & ivar=ivar, &
  & jvar=jvar, &
  & ispacecompo=ispacecompo, &
  & itimecompo=itimecompo, &
  & jspacecompo=jspacecompo, &
  & jtimecompo=jtimecompo, &
  & VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
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
                        '[START] ')
#endif

add0 = Input(default=.FALSE., option=addContribution)

SELECT TYPE (VALUE); CLASS IS (MatrixField_)
  IF (add0) THEN
    scale0 = Input(default=1.0_DFP, option=scale)
    CALL Add(obj=obj%mat, VALUE=VALUE%mat, scale=scale0, &
             isSameStructure=.TRUE.)
    RETURN
  END IF

  CALL Set(obj=obj%mat, VALUE=VALUE%mat, scale=scale0)

CLASS DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
         '[INTERNAL ERROR] :: This method is available for MatrixField_ only')
  RETURN

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
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
                        '[START] ')
#endif

IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

SELECT TYPE (VALUE); CLASS is (MatrixField_)

  dof_obj => GetDOFPointer(obj%mat, 1)

  spaceCompo = dof_obj.spacecomponents.1

  CALL obj_SetFromSTMatrix_help(obj=obj, VALUE=VALUE, fedof=obj%fedof, &
                                a=a, b=b, spaceCompo=spaceCompo)

CLASS default
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found.')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFromSTMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_SetFromSTMatrix_help(obj, VALUE, &
  & fedof, spaceCompo, a, b)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  CLASS(MatrixField_), INTENT(INOUT) :: VALUE
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! Space-time matrix field
  INTEGER(I4B), INTENT(IN) :: spaceCompo
  !!
  INTEGER(I4B), INTENT(IN) :: a
    !! itimecompo
  INTEGER(I4B), INTENT(IN) :: b
    !! jtimecompo

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_SetFromSTMatrix_help()"
  INTEGER(I4B) :: id, nsd, iel, nns, ispacecompo, jspacecompo, &
                  r1, r2, c1, c2
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: problem
  REAL(DFP), ALLOCATABLE :: elem_value(:, :)
  CLASS(AbstractMesh_), POINTER :: meshptr
  ! CLASS(ReferenceElement_), POINTER :: refelem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  meshptr => fedof%GetMeshPointer()
  nsd = meshptr%GetNSD()

  problem = .NOT. ASSOCIATED(meshptr)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: mesh pointer is not associted.')
    RETURN
  END IF

  problem = meshptr%isEmpty()
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: mesh is empty.')
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

!     nns = (.NNE.refelem)
!     CALL Reallocate(nptrs, nns)
!     CALL Reallocate(elem_value, spaceCompo * nns, spaceCompo * nns)
!
!     DO iel = meshptr%GetMinElemNumber(), meshptr%GetMaxElemNumber()
!
!       problem = .NOT. meshptr%isElementPresent(iel)
!       IF (problem) CYCLE
!
!       nptrs = meshptr%GetConnectivity(iel)
!
!       DO ispacecompo = 1, spaceCompo
!         r1 = 1 + (ispacecompo - 1) * nns
!         r2 = ispacecompo * nns
!         DO jspacecompo = 1, spaceCompo
!           c1 = 1 + (jspacecompo - 1) * nns
!           c2 = jspacecompo * nns
!           CALL VALUE%Get(inodenum=nptrs, jnodenum=nptrs,  &
!             & VALUE=elem_value(r1:r2, c1:c2),  &
!             & ivar=1, jvar=1, ispacecompo=ispacecompo,  &
!             & jspacecompo=jspacecompo,  &
!             & itimecompo=a, jtimecompo=b)
!
!         END DO
!       END DO
!
!       CALL obj%Set(globalNode=nptrs, VALUE=elem_value, storageFMT=FMT_DOF)
!
!     END DO
!
!   END DO
!
!   NULLIFY (meshptr, refelem)
!   IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
!   IF (ALLOCATED(elem_value)) DEALLOCATE (elem_value)
!
! #ifdef DEBUG_VER
!   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!     & '[END] ')
! #endif

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
                        '[START] ')
#endif

IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: This routine is not for rectangle matrix')
  RETURN
END IF

SELECT TYPE (VALUE); CLASS is (MatrixField_)

  dof_obj => GetDOFPointer(obj%mat, 1)
  spaceCompo = dof_obj.spacecomponents.1

  CALL obj_SetToSTMatrix_help(obj=obj, VALUE=VALUE, fedof=obj%fedof, &
                              spaceCompo=spaceCompo, a=a, b=b)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found.')
  RETURN

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetToSTMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_SetToSTMatrix_help(obj, VALUE, fedof, spaceCompo, a, b)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! Space time matrix
  CLASS(MatrixField_), INTENT(INOUT) :: VALUE
    !! space matrix
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! Space-time matrix field
  INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space-components
  INTEGER(I4B), INTENT(IN) :: a
    !! itimecompo
  INTEGER(I4B), INTENT(IN) :: b
    !! jtimecompo

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_SetToSTMatrix_help()"
! !   INTEGER(I4B) :: tmesh, id, nsd, iel, nns, ispacecompo, jspacecompo,  &
! !     & idof, jdof
! !   INTEGER(I4B), ALLOCATABLE :: nptrs(:)
! !   LOGICAL(LGT) :: problem
! !   REAL(DFP), ALLOCATABLE :: elem_value(:, :)
! !   CLASS(AbstractMesh_), POINTER :: meshptr
! !   CLASS(ReferenceElement_), POINTER :: refelem
! !   TYPE(DOF_), POINTER :: idof_obj, jdof_obj

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

! ! #ifdef DEBUG_VER
! !   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
! !     & '[START] ')
! ! #endif
! !
! !   nsd = dom%GetNSD()
! !   tmesh = dom%GetTotalEntities(dim=nsd)
! !
! !   NULLIFY (meshptr, refelem, idof_obj, jdof_obj)
! !
! !   idof_obj => GetDOFPointer(obj%mat, 1)
! !   jdof_obj => GetDOFPointer(obj%mat, 2)
! !
! !   DO id = 1, tmesh
! !     meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
! !
! !     problem = .NOT. ASSOCIATED(meshptr)
! !     IF (problem) CYCLE
! !
! !     problem = meshptr%isEmpty()
! !     IF (problem) CYCLE
! !
! !     refelem => meshptr%GetRefElemPointer()
! !     nns = (.NNE.refelem)
! !     CALL Reallocate(nptrs, nns)
! !     CALL Reallocate(elem_value, nns, nns)
! !
! !     DO iel = meshptr%GetMinElemNumber(), meshptr%GetMaxElemNumber()
! !
! !       problem = .NOT. meshptr%isElementPresent(iel)
! !       IF (problem) CYCLE
! !
! !       nptrs = meshptr%GetConnectivity(iel)
! !
! !       DO ispacecompo = 1, spaceCompo
! !         idof = GetIDOF(obj=idof_obj, ivar=1, spaceCompo=ispacecompo,  &
! !           & timeCompo=a)
! !
! !         DO jspacecompo = 1, spaceCompo
! !           jdof = GetIDOF(obj=jdof_obj, ivar=1, spaceCompo=jspacecompo,  &
! !             & timeCompo=b)
! !
! !           CALL VALUE%Get(inodenum=nptrs, jnodenum=nptrs,  &
! !             & VALUE=elem_value,  &
! !             & ivar=1, jvar=1, ispacecompo=ispacecompo,  &
! !             & jspacecompo=jspacecompo,  &
! !             & itimecompo=1, jtimecompo=1)
! !
! !           CALL obj%Set(inodenum=nptrs,  &
! !             & jnodenum=nptrs, VALUE=elem_value,  &
! !             & ivar=1, jvar=1, idof=idof, jdof=jdof)
! !
! !         END DO
! !       END DO
! !
! !     END DO
! !
! !   END DO
! !
! !   NULLIFY (meshptr, refelem, idof_obj, jdof_obj)
! !   IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
! !   IF (ALLOCATED(elem_value)) DEALLOCATE (elem_value)
! !
! ! #ifdef DEBUG_VER
! !   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
! !     & '[END] ')
! ! #endif
! !
END SUBROUTINE obj_SetToSTMatrix_help
!
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
