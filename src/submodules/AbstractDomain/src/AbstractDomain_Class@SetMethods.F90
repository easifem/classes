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

SUBMODULE(AbstractDomain_Class) SetMethods
USE FEDomainConnectivity_Class, ONLY: FEDomainConnectivity_
USE CSRMatrix_Method, ONLY: CSRMatrix_SetSparsity => SetSparsity
USE CSRMatrix_Method, ONLY: CSRMatrix_GetMatrixProp => GetMatrixProp
USE Display_Method, ONLY: ToString, Display
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractDomain_Class@SetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                               SetShowTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetShowTime
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetShowTime()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%showTime = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetShowTime

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity1()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, "obj is not initiated.")
#endif

meshptr => obj%GetMeshPointer()
CALL meshptr%SetSparsity(mat)
CALL CSRMatrix_SetSparsity(mat)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
INTEGER(I4B) :: ivar, tsize
INTEGER(I4B), ALLOCATABLE :: nsd(:)
LOGICAL(LGT) :: isok
#endif

CHARACTER(:), ALLOCATABLE :: matProp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Check if domains are associated and initiated
#ifdef DEBUG_VER
tsize = SIZE(domains)
CALL Reallocate(nsd, tsize)

DO ivar = 1, tsize

  isok = ASSOCIATED(domains(ivar)%ptr)
  CALL AssertError1(isok, myName, &
                    'domains('//Tostring(ivar)//') NOT ASSOCIATED')

  isok = domains(ivar)%ptr%isInit
  CALL AssertError1(isok, myName, &
                    'domains('//Tostring(ivar)//') NOT Initiated.')

  nsd(ivar) = domains(ivar)%ptr%GetNSD()

END DO

! NSD of all domains should be identical
isok = ALL(nsd .EQ. nsd(1))
CALL AssertError1(isok, myName, &
                  'It seems that nsd of domains are not same.')

DEALLOCATE (nsd)
#endif

matProp = CSRMatrix_GetMatrixProp(mat)

IF (matProp .EQ. "RECTANGLE") THEN
  CALL part2_obj_Set_sparsity2(domains=domains, mat=mat)
ELSE
  CALL part1_obj_Set_sparsity2(domains=domains, mat=mat)
END IF

matProp = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                   part1_obj_Set_sparsity2
!----------------------------------------------------------------------------

SUBROUTINE part1_obj_Set_sparsity2(domains, mat)
  CLASS(AbstractDomainPointer_), INTENT(IN) :: domains(:)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "part1_obj_Set_sparsity2()"
#endif
  INTEGER(I4B) :: ivar, jvar
  CLASS(AbstractDomain_), POINTER :: rowDomain, colDomain
  CLASS(AbstractMesh_), POINTER :: rowMesh, colMesh
  TYPE(FEDomainConnectivity_) :: domainConn
  INTEGER(I4B), POINTER :: nodeToNode(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! nullify first for safety
  rowMesh => NULL()
  colMesh => NULL()
  rowDomain => NULL()
  colDomain => NULL()

  DO ivar = 1, SIZE(domains)

    rowDomain => domains(ivar)%ptr
    isok = ASSOCIATED(rowDomain)
    IF (.NOT. isok) CYCLE

    rowMesh => rowDomain%GetMeshPointer(dim=rowDomain%GetNSD())
    isok = ASSOCIATED(rowMesh)
    IF (.NOT. isok) CYCLE
    IF (rowMesh%isEmpty()) CYCLE

    DO jvar = 1, SIZE(domains)

      colDomain => domains(jvar)%ptr
      isok = ASSOCIATED(colDomain)
      IF (.NOT. isok) CYCLE

      colMesh => colDomain%GetMeshPointer(dim=colDomain%GetNSD())
      isok = ASSOCIATED(colMesh)
      IF (.NOT. isok) CYCLE
      IF (colMesh%isEmpty()) CYCLE

      CALL domainConn%DEALLOCATE()
      CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
                                             domain2=colDomain)
      nodeToNode => domainConn%GetNodeToNodePointer()

      CALL rowMesh%SetSparsity(mat=mat, colMesh=colMesh, &
                               nodeToNode=nodeToNode, ivar=ivar, jvar=jvar)

    END DO
  END DO

  CALL CSRMatrix_SetSparsity(mat)

  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)

  CALL domainConn%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE part1_obj_Set_sparsity2

!----------------------------------------------------------------------------
!                                                   part2_obj_Set_sparsity2
!----------------------------------------------------------------------------

SUBROUTINE part2_obj_Set_sparsity2(domains, mat)
  CLASS(AbstractDomainPointer_), INTENT(IN) :: domains(2)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "part2_obj_Set_sparsity2()"
#endif
  INTEGER(I4B), PARAMETER :: tvar = 2, ivar = 1, jvar = 1
  INTEGER(I4B) :: nsd(tvar), ii
  CLASS(AbstractMesh_), POINTER :: rowMesh, colMesh
  CLASS(AbstractDomain_), POINTER :: rowDomain, colDomain
  TYPE(FEDomainConnectivity_) :: domainConn
  INTEGER(I4B), POINTER :: nodeToNode(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  DO ii = 1, tvar
    nsd(ii) = domains(ii)%ptr%GetNSD()
  END DO

  rowDomain => NULL()
  colDomain => NULL()
  rowDomain => domains(1)%ptr
  colDomain => domains(2)%ptr

#ifdef DEBUG_VER
  isok = ASSOCIATED(rowDomain)
  CALL AssertError1(isok, myName, &
                    "rowDomain is not associated.")

  isok = ASSOCIATED(colDomain)
  CALL AssertError1(isok, myName, &
                    "colDomain is not associated.")
#endif

  rowMesh => NULL()
  colMesh => NULL()
  rowMesh => rowDomain%GetMeshPointer(dim=nsd(1))
  colMesh => colDomain%GetMeshPointer(dim=nsd(2))

#ifdef DEBUG_VER
  isok = ASSOCIATED(rowMesh)
  CALL AssertError1(isok, myName, &
                    "rowMesh is not associated.")

  isok = ASSOCIATED(colMesh)
  CALL AssertError1(isok, myName, &
                    "colMesh is not associated.")
#endif

  CALL domainConn%InitiateNodeToNodeData(domain1=rowDomain, &
                                         domain2=colDomain)
  nodeToNode => domainConn%GetNodeToNodePointer()

  CALL rowMesh%SetSparsity(mat=mat, colMesh=colMesh, &
                           nodeToNode=nodeToNode, ivar=ivar, &
                           jvar=jvar)

  CALL CSRMatrix_SetSparsity(mat)

  NULLIFY (rowMesh, colMesh, rowDomain, colDomain, nodeToNode)

  CALL domainConn%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE part2_obj_Set_sparsity2

!----------------------------------------------------------------------------
!                                                           SetTotalMedium
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalMedium
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTotalMedium()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
CALL meshptr%SetTotalMedium(n)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTotalMedium

!----------------------------------------------------------------------------
!                                                                   SetMedium
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMaterial()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)

#ifdef DEBUG_VER
isok = ASSOCIATED(meshptr)
CALL AssertError1(isok, myName, &
                  "meshptr is not associated.")
#endif

CALL meshptr%SetMaterial(medium=medium, material=material, &
                         entityNum=entityNum)
meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMaterial

!----------------------------------------------------------------------------
!                                                              SetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetNodeCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetNodeCoord1()"
LOGICAL(LGT) :: problem, isok
#endif

REAL(DFP) :: scale0
LOGICAL(LGT) :: add0
INTEGER(I4B) :: ii, tnodes, nsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%nodeCoord)
CALL AssertError1(isok, myName, &
                  "obj%nodeCoord not allocated.")

CALL AssertError2(SIZE(nodeCoord, 1), SIZE(obj%nodeCoord, 1), myName, &
                  "a=nrow in nodeCoord, b=nrow in obj%nodeCoord")

CALL AssertError2(SIZE(nodeCoord, 2), SIZE(obj%nodeCoord, 2), myName, &
                  "a=ncol in nodeCoord, b=ncol in obj%nodeCoord")
#endif

scale0 = Input(option=scale, default=math%one)
add0 = Input(option=addContribution, default=math%no)
tnodes = SIZE(nodeCoord, 2)
nsd = obj%nsd

IF (add0) THEN
  DO CONCURRENT(ii=1:tnodes)
    obj%nodeCoord(1:nsd, ii) = nodeCoord(1:nsd, ii) * scale0 &
                               + obj%nodeCoord(1:nsd, ii)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

! make do concurrent loop for setting obj%nodeCoord to nodeCoord
DO CONCURRENT(ii=1:tnodes)
  obj%nodeCoord(1:nsd, ii) = nodeCoord(1:nsd, ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetNodeCoord

!----------------------------------------------------------------------------
!                                                                 SetQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuality
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"
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

! CLASS(Mesh_), POINTER :: meshptr
! CHARACTER(*), PARAMETER :: myName = "obj_SetQuality"
! REAL(DFP), ALLOCATABLE :: max_(:, :), min_(:, :)
! INTEGER(I4B) :: tmesh, imesh, dim0
!
!
! dim0 = Input(default=obj%nsd, option=dim)
!
! IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
!   meshptr => obj%GetMeshPointer(dim=dim, entityNum=entityNum)
!   IF (meshptr%GetTotalElements() .EQ. 0) THEN
!     CALL e%RaiseWarning(modName//'::'//myName//' - '// &
!     & 'mesh if empty')
!   ELSE
!     CALL meshptr%SetQuality(&
!       & measures=measures, &
!       & max_measures=max_measures, &
!       & min_measures=min_measures, &
!       & nodeCoord=obj%nodeCoord, &
!       & local_nptrs=obj%local_nptrs &
!       & )
!   END IF
!   NULLIFY (meshptr)
!   RETURN
! END IF
!
! IF (PRESENT(dim) .AND. .NOT. PRESENT(entityNum)) THEN
!   tmesh = obj%GetTotalMesh(dim=dim)
!   CALL Reallocate(max_, SIZE(measures), tmesh)
!   min_ = max_
!
!   DO imesh = 1, tmesh
!     meshptr => obj%GetMeshPointer(dim=dim, entityNum=imesh)
!     IF (meshptr%GetTotalElements() .EQ. 0) THEN
!       max_(:, imesh) = -1 * MaxDFP
!       min_(:, imesh) = MaxDFP
!     ELSE
!       CALL meshptr%SetQuality(&
!         & measures=measures, &
!         & max_measures=max_(:, imesh), &
!         & min_measures=min_(:, imesh), &
!         & nodeCoord=obj%nodeCoord, &
!         & local_nptrs=obj%local_nptrs &
!         & )
!     END IF
!   END DO
!
!   max_measures = MAXVAL(max_, dim=2)
!   min_measures = MINVAL(min_, dim=2)
!   NULLIFY (meshptr)
!   DEALLOCATE (max_, min_)
!   RETURN
! END IF
!
! CALL e%RaiseError(modName//'::'//myName//' - '// &
!   & 'No case found')
END PROCEDURE obj_SetQuality

!----------------------------------------------------------------------------
!                                                           SetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTotalElements()"
INTEGER(I4B) :: aint
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
aint = SIZE(obj%tElements)
CALL AssertError3(indx, aint, myName, &
                  "indx is out of bound, a=indx, b=size(obj%tElements)")
#endif

obj%tElements(indx) = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetTotalElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
