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

SUBMODULE(OneDimFEDOF_Class) SetMethods
USE Display_Method, ONLY: ToString, Display

USE ReallocateUtility, ONLY: Reallocate

! USE CSRMatrix_Method, ONLY: CSRMatrix_SetSparsity => SetSparsity, &
!                             CSRMatrix_GetMatrixProp => GetMatrixProp
!
!
! USE FEDomainConnectivity_Class, ONLY: FEDomainConnectivity_
!
! USE AbstractMeshParam, ONLY: PARAM_MAX_NODE_TO_NODE, &
!                              PARAM_MAX_NODE_TO_ELEM, &
!                              PARAM_MAX_CONNECTIVITY_SIZE, &
!                              PARAM_MAX_NNE

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               SetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellOrder
CHARACTER(*), PARAMETER :: myName = "obj_SetCellOrder()"
INTEGER(I4B) :: tsize, ii, jj, tcells
LOGICAL(LGT) :: isok, no_true
INTEGER(INT8) :: int8_order

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tcells = obj%mesh%GetTotalElements()
CALL Reallocate(obj%cellOrder, tcells)

tsize = SIZE(order)
IF (tsize .EQ. 1) THEN

  int8_order = INT(order(1), kind=INT8)

  DO ii = 1, tcells
    obj%cellOrder(ii) = int8_order
  END DO

ELSE

  isok = tsize .LE. tcells
  CALL AssertError1(isok, myName, "Size of order array is out of bounds")

  DO ii = 1, tsize
    isok = obj%mesh%IsElementPresent(globalElement=ii, islocal=no_true)
    IF (.NOT. isok) CYCLE

    jj = obj%mesh%GetLocalElemNumber(globalElement=ii, islocal=islocal)
    int8_order = INT(order(ii), kind=INT8)
    obj%cellOrder(jj) = int8_order
  END DO

END IF

obj%maxCellOrder = MAXVAL(obj%cellOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCellOrder

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
CHARACTER(*), PARAMETER :: myName = "obj_setSparsity1()"

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

! INTEGER(I4B) :: tsize, ii, tdof, iel, telements
! INTEGER(I4B), ALLOCATABLE :: conn(:)
! LOGICAL(LGT) :: isok
! CLASS(AbstractMesh_), POINTER :: mesh
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! mesh => obj%GetMeshPointer()
!
! #ifdef DEBUG_VER
! isok = ASSOCIATED(mesh)
! CALL AssertError1(isok, myName, 'obj%mesh NOT ASSOCIATED')
!
! tsize = mesh%GetTotalElements()
! isok = tsize .NE. 0_I4B
! CALL AssertError1(isok, myName, "Empty mesh found, returning")
! #endif
!
! tdof = obj%GetMaxTotalConnectivity()
! ALLOCATE (conn(tdof))
!
! telements = mesh%GetTotalElements()
!
! DO iel = 1, telements
!   CALL obj%GetConnectivity_(globalElement=iel, islocal=.TRUE., &
!                             ans=conn, tsize=tdof, opt="ALL")
!   DO ii = 1, tdof
!     CALL CSRMatrix_SetSparsity(obj=mat, row=conn(ii), col=conn(1:tdof))
!   END DO
! END DO
!
! CALL CSRMatrix_SetSparsity(mat)
!
! mesh => NULL()
! DEALLOCATE (conn)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
! INTEGER(I4B) :: tsize, ii, tdof, iel, telements, col_telements, col_tdof, &
!                 col_iel
! INTEGER(I4B), ALLOCATABLE :: conn(:), col_conn(:)
! LOGICAL(LGT) :: isok
! CLASS(AbstractMesh_), POINTER :: mesh, col_mesh
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! mesh => obj%GetMeshPointer()
! col_mesh => col_fedof%GetMeshPointer()
!
! #ifdef DEBUG_VER
!
! isok = ASSOCIATED(mesh)
! CALL AssertError1(isok, myName, 'obj%mesh NOT ASSOCIATED')
!
! isok = ASSOCIATED(col_mesh)
! CALL AssertError1(isok, myName, 'col_fedof%mesh NOT ASSOCIATED')
!
! tsize = mesh%GetTotalElements()
! isok = tsize .NE. 0_I4B
! CALL AssertError1(isok, myName, "Empty mesh found, returning")
!
! tsize = col_mesh%GetTotalElements()
! isok = tsize .NE. 0_I4B
! CALL AssertError1(isok, myName, "Empty mesh found, returning")
!
! #endif
!
! tdof = obj%GetMaxTotalConnectivity()
! ALLOCATE (conn(tdof))
!
! col_tdof = col_fedof%GetMaxTotalConnectivity()
! ALLOCATE (col_conn(col_tdof))
!
! telements = mesh%GetTotalElements()
!
! #ifdef DEBUG_VER
! col_telements = SIZE(cellToCell)
! CALL AssertError2(telements, col_telements, myName, &
!                   "a=telements, b=size(cellToCell)")
! #endif
!
! DO iel = 1, telements
!
!   CALL obj%GetConnectivity_(globalElement=iel, islocal=.TRUE., &
!                             ans=conn, tsize=tdof, opt="ALL")
!
!   col_iel = cellToCell(iel)
!
!   CALL col_fedof%GetConnectivity_(globalElement=col_iel, islocal=.FALSE., &
!                                   ans=col_conn, tsize=col_tdof, opt="ALL")
!
!   DO ii = 1, tdof
!     CALL CSRMatrix_SetSparsity(obj=mat, row=conn(ii), &
!                                col=col_conn(1:col_tdof), ivar=ivar, jvar=jvar)
!
!   END DO
!
! END DO
!
! ! CALL CSRMatrix_SetSparsity(mat)
!
! mesh => NULL()
! col_mesh => NULL()
! DEALLOCATE (conn)
! DEALLOCATE (col_conn)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif
END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity3()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

! INTEGER(I4B) :: ivar, jvar, nsd(SIZE(fedofs))
! LOGICAL(LGT) :: isok
! CLASS(AbstractMesh_), POINTER :: meshptr, rowMesh, colMesh
! CLASS(FEDOF_), POINTER :: rowfedof, colfedof
! INTEGER(I4B), POINTER :: cellToCell(:)
! TYPE(FEDomainConnectivity_) :: domainConn
!
! #ifdef DEBUG_VER
! LOGICAL(LGT), PARAMETER :: isdebug = .TRUE.
! #else
! LOGICAL(LGT), PARAMETER :: isdebug = .FALSE.
! #endif
!
! #ifdef DEBUG_VER
! !-------------------------- debug (o) ---------------------------
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! DO ivar = 1, SIZE(fedofs)
!
!   isok = ASSOCIATED(fedofs(ivar)%ptr)
!
!   CALL AssertError1(isok, myName, &
!                     'fedofs('//Tostring(ivar)//')%ptr NOT ASSOCIATED')
!
!   meshptr => fedofs(ivar)%ptr%GetMeshPointer()
!
!   isok = ASSOCIATED(meshptr)
!   CALL AssertError1(isok, myName, 'meshptr NOT ASSOCIATED')
!
!   nsd(ivar) = meshptr%GetNSD()
!
! END DO
!
! isok = ALL(nsd .EQ. nsd(1))
! CALL AssertError1(isok, myName, 'NSD of fedofsare not identical')
!
! !-------------------------- debug (x) ---------------------------
! #endif
!
! rowMesh => NULL()
! colMesh => NULL()
!
! SELECT CASE (mat%matrixProp(1:4))
! CASE ("RECT")
!   IF (isdebug) CALL Display("matrix is rectangular")
!   rowfedof => fedofs(1)%ptr
!   rowMesh => rowfedof%GetMeshPointer()
!
!   colfedof => fedofs(2)%ptr
!   colMesh => colfedof%GetMeshPointer()
!
!   CALL domainConn%DEALLOCATE()
!   CALL domainConn%InitiateCellToCellData(mesh1=rowmesh, mesh2=colmesh)
!   cellToCell => domainConn%GetCellToCellPointer()
!
!   CALL rowfedof%SetSparsity(mat=mat, col_fedof=colfedof, &
!                             cellToCell=cellToCell, ivar=1, jvar=1)
!
! CASE default
!   DO ivar = 1, SIZE(fedofs)
!
!     IF (isdebug) CALL Display("row domain = "//tostring(ivar))
!
!     rowfedof => fedofs(ivar)%ptr
!     IF (.NOT. ASSOCIATED(rowfedof)) CYCLE
!
!     rowMesh => rowfedof%GetMeshPointer()
!     IF (.NOT. ASSOCIATED(rowMesh)) CYCLE
!     IF (rowMesh%isEmpty()) CYCLE
!
!     DO jvar = 1, SIZE(fedofs)
!
!       IF (isdebug) CALL Display("col domain = "//tostring(jvar))
!
!       colfedof => fedofs(jvar)%ptr
!       IF (.NOT. ASSOCIATED(colfedof)) CYCLE
!
!       colMesh => colfedof%GetMeshPointer()
!       IF (.NOT. ASSOCIATED(colMesh)) CYCLE
!       IF (colMesh%isEmpty()) CYCLE
!
!       CALL domainConn%DEALLOCATE()
!       CALL domainConn%InitiateCellToCellData(mesh1=rowmesh, mesh2=colmesh)
!       cellToCell => domainConn%GetCellToCellPointer()
!
!       CALL rowfedof%SetSparsity(mat=mat, col_fedof=colfedof, &
!                                 cellToCell=cellToCell, ivar=ivar, jvar=jvar)
!
!     END DO
!   END DO
! END SELECT
!
! CALL CSRMatrix_SetSparsity(mat)
!
! NULLIFY (rowMesh, colMesh, rowfedof, colfedof, cellToCell)
!
! CALL domainConn%DEALLOCATE()
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif

END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
