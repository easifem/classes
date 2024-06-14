! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(FEDOF_Class) SetSparsityMethods
USE CSRMatrix_Method, ONLY: CSRMatrix_SetSparsity => SetSparsity, &
                            CSRMatrix_GetMatrixProp => GetMatrixProp
USE Display_Method, ONLY: ToString, Display

USE FEDomainConnectivity_Class, ONLY: FEDomainConnectivity_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
CLASS(AbstractMesh_), POINTER :: meshptr
meshptr => obj%GetMeshPointer()
CALL meshptr%SetSparsity(mat=mat)
CALL CSRMatrix_SetSparsity(mat)
meshptr => NULL()

END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
INTEGER(I4B) :: ivar, nsd(SIZE(fedofs))
LOGICAL(LGT) :: problem, isok
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(:), ALLOCATABLE :: matProp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
DO ivar = 1, SIZE(fedofs)

  isok = ASSOCIATED(fedofs(ivar)%ptr)

  CALL AssertError1(isok, myName, &
                    'fedofs('//Tostring(ivar)//')%ptr NOT ASSOCIATED')

  meshptr => fedofs(ivar)%ptr%GetMeshPointer()

  isok = ASSOCIATED(meshptr)
  CALL AssertError1(isok, myName, 'meshptr NOT ASSOCIATED')

  nsd(ivar) = meshptr%GetNSD()

END DO

isok = ALL(nsd .EQ. nsd(1))
CALL AssertError1(isok, myName, 'NSD of fedofsare not identical')

#endif

matProp = CSRMatrix_GetMatrixProp(mat)

IF (matProp .EQ. "RECTANGLE") THEN
  CALL part2_obj_Set_sparsity2(fedofs=fedofs, mat=mat)
ELSE
  CALL part1_obj_Set_sparsity2(fedofs=fedofs, mat=mat)
END IF

matProp = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE part1_obj_Set_sparsity2(fedofs, mat)
  TYPE(FEDOFPointer_), INTENT(IN) :: fedofs(:)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "part1_obj_Set_sparsity2()"
  INTEGER(I4B) :: ivar, jvar
  CLASS(AbstractMesh_), POINTER :: rowMesh, colMesh
  CLASS(FEDOF_), POINTER :: rowfedof, colfedof

  TYPE(FEDomainConnectivity_) :: domainConn
  INTEGER(I4B), POINTER :: nodeToNode(:)
  LOGICAL(LGT) :: isdebug

  isdebug = .FALSE.

#ifdef DEBUG_VER
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
  isdebug = .TRUE.
#endif

  ! nullify first for safety
  rowMesh => NULL()
  colMesh => NULL()

  DO ivar = 1, SIZE(fedofs)

    IF (isdebug) CALL Display("row domain = "//tostring(ivar))

    rowfedof => fedofs(ivar)%ptr
    IF (.NOT. ASSOCIATED(rowfedof)) CYCLE

    rowMesh => rowfedof%GetMeshPointer()
    IF (.NOT. ASSOCIATED(rowMesh)) CYCLE
    IF (rowMesh%isEmpty()) CYCLE

    DO jvar = 1, SIZE(fedofs)

      IF (isdebug) CALL Display("col domain = "//tostring(jvar))

      colfedof => fedofs(jvar)%ptr
      IF (.NOT. ASSOCIATED(colfedof)) CYCLE

      colMesh => colfedof%GetMeshPointer()
      IF (.NOT. ASSOCIATED(colMesh)) CYCLE
      IF (colMesh%isEmpty()) CYCLE

      CALL domainConn%DEALLOCATE()
      CALL domainConn%InitiateNodeToNodeData(mesh1=rowmesh, mesh2=colmesh)
      nodeToNode => domainConn%GetNodeToNodePointer()

      CALL rowMesh%SetSparsity(mat=mat, colMesh=colMesh, &
                               nodeToNode=nodeToNode, ivar=ivar, jvar=jvar)

    END DO
  END DO

  CALL CSRMatrix_SetSparsity(mat)
  NULLIFY (rowMesh, colMesh, rowfedof, colfedof, nodeToNode)
  CALL domainConn%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE part1_obj_Set_sparsity2

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

SUBROUTINE part2_obj_Set_sparsity2(fedofs, mat)
  CLASS(FEDOFPointer_), INTENT(IN) :: fedofs(2)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "part2_obj_Set_sparsity2()"
  INTEGER(I4B), PARAMETER :: tvar = 2, ivar = 1, jvar = 1
  INTEGER(I4B) :: ii
  CLASS(AbstractMesh_), POINTER :: rowMesh, colMesh
  CLASS(FEDOF_), POINTER :: rowfedof, colfedof
  TYPE(FEDomainConnectivity_) :: domainConn
  INTEGER(I4B), POINTER :: nodeToNode(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  rowfedof => NULL()
  colfedof => NULL()
  rowfedof => fedofs(1)%ptr
  colfedof => fedofs(2)%ptr

  isok = ASSOCIATED(rowfedof) .AND. ASSOCIATED(colfedof)
  CALL AssertError1(isok, myName, 'rowfedof or colfedof NOT ASSOCIATED')

  rowMesh => NULL()
  colMesh => NULL()
  rowMesh => rowfedof%GetMeshPointer()
  colMesh => colfedof%GetMeshPointer()

  isok = ASSOCIATED(rowMesh) .AND. ASSOCIATED(colMesh)
  CALL AssertError1(isok, myName, 'rowMesh or colMesh NOT ASSOCIATED')

  CALL domainConn%InitiateNodeToNodeData(mesh1=rowMesh, mesh2=colMesh)
  nodeToNode => domainConn%GetNodeToNodePointer()

  CALL rowMesh%SetSparsity(mat=mat, colMesh=colMesh, &
                           nodeToNode=nodeToNode, ivar=ivar, jvar=jvar)

  CALL CSRMatrix_SetSparsity(mat)

  NULLIFY (rowMesh, colMesh, rowfedof, colfedof, nodeToNode)

  CALL domainConn%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE part2_obj_Set_sparsity2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetSparsityMethods
