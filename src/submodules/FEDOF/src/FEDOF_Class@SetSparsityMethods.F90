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
USE CSRMatrix_Method, ONLY: CSRMatrix_SetSparsity => SetSparsity
USE Display_Method, ONLY: ToString
USE FEDomainConnectivity_Class, ONLY: FEDomainConnectivity_
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "FEDOF_Class@SetSparsityMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity1()"
#endif
CHARACTER(*), PARAMETER :: opt_all = "A"
INTEGER(I4B) :: ii, jj, te2eConn, tconn, iel, telements, te2e
INTEGER(I4B), ALLOCATABLE :: conn(:), e2e(:), e2eConn(:)
LOGICAL(LGT) :: isok, onlyCell0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%mesh)
CALL AssertError1(isok, myName, 'obj%mesh NOT ASSOCIATED')

ii = obj%mesh%GetTotalElements()
isok = ii .NE. 0_I4B
CALL AssertError1(isok, myName, "Empty mesh found.")
#endif

tconn = obj%GetMaxTotalConnectivity()
CALL Reallocate(conn, tconn)
telements = obj%mesh%GetTotalElements()

onlyCell0 = Input(option=onlyCell, default=math%no)

IF (.NOT. obj%isBrokenSpace .OR. onlyCell0) THEN
  elemLoop: DO iel = 1, telements
    isok = obj%mesh%isElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE elemLoop
    CALL obj%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                              ans=conn, tsize=tconn, opt=opt_all)
    DO ii = 1, tconn
      ! TODO: Check if the following method use reallocation at every call
      CALL CSRMatrix_SetSparsity(obj=mat, row=conn(ii), col=conn(1:tconn))
    END DO
  END DO elemLoop

  CALL CSRMatrix_SetSparsity(mat)

  IF (ALLOCATED(conn)) DEALLOCATE (conn)
  IF (ALLOCATED(e2e)) DEALLOCATE (e2e)
  IF (ALLOCATED(e2eConn)) DEALLOCATE (e2eConn)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! The following code is executed for brokenSpaces
CALL Reallocate(e2e, obj%mesh%GetMaxElementToElements())
ii = (SIZE(e2e) + 1) * obj%GetMaxTotalConnectivity()
CALL Reallocate(e2eConn, ii)

elemLoop2: DO iel = 1, telements
  isok = obj%mesh%isElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE elemLoop2

  ! Get connectivity of fedof for iel
  CALL obj%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                            ans=conn, tsize=tconn, opt=opt_all)

  ! Get element to element data for iel
  CALL obj%mesh%GetElementToElements_(ans=e2e, tsize=te2e, &
                                      globalElement=iel, islocal=math%yes)

  ! Get connectvity of fedof for each element in e2e
  te2eConn = tconn
  e2eConn(1:tconn) = conn(1:tconn)
  elemLoop3: DO ii = 1, te2e
    ! Check which neighbor element is active
    isok = obj%mesh%IsElementActive(globalElement=e2e(ii), &
                                    islocal=math%no)
    IF (.NOT. isok) CYCLE elemLoop3

    CALL obj%GetConnectivity_(globalElement=e2e(ii), islocal=math%no, &
                              ans=e2eConn(te2eConn + 1:), tsize=jj, &
                              opt=opt_all)
    te2eConn = te2eConn + jj
  END DO elemLoop3

  ! add sparsity for each dof value in conn
  DO ii = 1, tconn
    ! TODO: Check if the following method use reallocation at every call
    CALL CSRMatrix_SetSparsity(obj=mat, row=conn(ii), &
                               col=e2eConn(1:te2eConn))
  END DO

END DO elemLoop2

CALL CSRMatrix_SetSparsity(mat)

IF (ALLOCATED(conn)) DEALLOCATE (conn)
IF (ALLOCATED(e2e)) DEALLOCATE (e2e)
IF (ALLOCATED(e2eConn)) DEALLOCATE (e2eConn)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
#endif
CHARACTER(*), PARAMETER :: opt_all = "A"
INTEGER(I4B) :: tsize, ii, tdof, iel, telements, col_telements, col_tdof, &
                col_iel
LOGICAL(LGT) :: isok
INTEGER(I4B), ALLOCATABLE :: conn(:), col_conn(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%mesh)
CALL AssertError1(isok, myName, 'obj%mesh NOT ASSOCIATED')

isok = ASSOCIATED(col_fedof%mesh)
CALL AssertError1(isok, myName, 'col_fedof%mesh NOT ASSOCIATED')

tsize = obj%mesh%GetTotalElements()
isok = tsize .NE. 0_I4B
CALL AssertError1(isok, myName, "Empty mesh found for obj, returning")

tsize = col_fedof%mesh%GetTotalElements()
isok = tsize .NE. 0_I4B
CALL AssertError1(isok, myName, "Empty mesh found for col_fedof, returning")
#endif

tdof = obj%GetMaxTotalConnectivity()
ALLOCATE (conn(tdof))

col_tdof = col_fedof%GetMaxTotalConnectivity()
ALLOCATE (col_conn(col_tdof))

telements = obj%mesh%GetTotalElements()

#ifdef DEBUG_VER
col_telements = SIZE(cellToCell)
CALL AssertError2(telements, col_telements, myName, &
                  "a=telements, b=size(cellToCell)")
#endif

DO iel = 1, telements
  ! Check if the element is active
  CALL obj%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                            ans=conn, tsize=tdof, opt=opt_all)

  col_iel = cellToCell(iel)

  CALL col_fedof%GetConnectivity_(globalElement=col_iel, islocal=math%no, &
                                  ans=col_conn, tsize=col_tdof, opt=opt_all)

  DO ii = 1, tdof
    CALL CSRMatrix_SetSparsity(obj=mat, row=conn(ii), &
                               col=col_conn(1:col_tdof), ivar=ivar, &
                               jvar=jvar)
  END DO
END DO

! CALL CSRMatrix_SetSparsity(mat)
IF (ALLOCATED(conn)) DEALLOCATE (conn)
IF (ALLOCATED(col_conn)) DEALLOCATE (col_conn)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity3()"
INTEGER(I4B) :: ivar, jvar, nsd(SIZE(fedofs))
LOGICAL(LGT) :: isok
CLASS(AbstractMesh_), POINTER :: meshptr, rowMesh, colMesh
CLASS(FEDOF_), POINTER :: rowfedof, colfedof
INTEGER(I4B), POINTER :: cellToCell(:)
TYPE(FEDomainConnectivity_) :: domainConn

#ifdef DEBUG_VER
LOGICAL(LGT), PARAMETER :: isdebug = math%yes
#else
LOGICAL(LGT), PARAMETER :: isdebug = math%no
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
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

rowMesh => NULL()
colMesh => NULL()

SELECT CASE (mat%matrixProp(1:4))
CASE ("RECT")
  ! IF (isdebug) CALL Display("matrix is rectangular")
  rowfedof => fedofs(1)%ptr
  rowMesh => rowfedof%GetMeshPointer()

  colfedof => fedofs(2)%ptr
  colMesh => colfedof%GetMeshPointer()

  CALL domainConn%DEALLOCATE()
  CALL domainConn%InitiateCellToCellData(mesh1=rowMesh, mesh2=colMesh)
  cellToCell => domainConn%GetCellToCellPointer()

  CALL rowfedof%SetSparsity(mat=mat, col_fedof=colfedof, &
                            cellToCell=cellToCell, ivar=1, jvar=1)

CASE DEFAULT
  loop1: DO ivar = 1, SIZE(fedofs)
    ! IF (isdebug) CALL Display("row domain = "//tostring(ivar))

    rowfedof => fedofs(ivar)%ptr
    IF (.NOT. ASSOCIATED(rowfedof)) CYCLE loop1

    rowMesh => rowfedof%GetMeshPointer()
    IF (.NOT. ASSOCIATED(rowMesh)) CYCLE loop1
    IF (rowMesh%isEmpty()) CYCLE loop1

    loop2: DO jvar = 1, SIZE(fedofs)
      ! IF (isdebug) CALL Display("col domain = "//tostring(jvar))

      colfedof => fedofs(jvar)%ptr
      IF (.NOT. ASSOCIATED(colfedof)) CYCLE loop2

      colMesh => colfedof%GetMeshPointer()
      IF (.NOT. ASSOCIATED(colMesh)) CYCLE loop2
      IF (colMesh%isEmpty()) CYCLE loop2

      CALL domainConn%DEALLOCATE()
      CALL domainConn%InitiateCellToCellData(mesh1=rowMesh, mesh2=colMesh)
      cellToCell => domainConn%GetCellToCellPointer()

      CALL rowfedof%SetSparsity(mat=mat, col_fedof=colfedof, &
                                cellToCell=cellToCell, ivar=ivar, &
                                jvar=jvar)
    END DO loop2
  END DO loop1
END SELECT

CALL CSRMatrix_SetSparsity(mat)

NULLIFY (rowMesh, colMesh, rowfedof, colfedof, cellToCell)

CALL domainConn%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetSparsityMethods
