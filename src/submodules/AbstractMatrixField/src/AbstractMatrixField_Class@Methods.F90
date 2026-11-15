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

SUBMODULE(AbstractMatrixField_Class) Methods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractField_Class, ONLY: AbstractFieldDeallocate
USE AbstractField_Class, ONLY: AbstractFieldDisplay
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE ElemshapeData_Method, ONLY: ElemshapeData_Deallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Initiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMatrixField_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldDisplay(obj=obj, msg=msg, unitNo=unitNo)
CALL Display(obj%isPmatInitiated, msg="PmatInitiated: ", unitNo=unitNo)
CALL Display(ASSOCIATED(obj%rowFedof), msg="rowFedof ASSOCIATED: ", &
             unitNo=unitNo)
CALL Display(ASSOCIATED(obj%colFedof), msg="colFedof ASSOCIATED: ", &
             unitNo=unitNo)
CALL Display(ASSOCIATED(obj%rowGeoFedof), msg="rowGeoFedof ASSOCIATED: ", &
             unitNo=unitNo)
CALL Display(ASSOCIATED(obj%colGeoFedof), msg="colGeoFedof ASSOCIATED: ", &
             unitNo=unitNo)
CALL Display(ALLOCATED(obj%rowFedofs), msg="rowFedofs ALLOCATED: ", &
             unitNo=unitNo)
IF (ALLOCATED(obj%rowFedofs)) THEN
  CALL Display("SIZE(rowFedofs): "//ToString(SIZE(obj%rowFedofs)), &
               unitNo=unitNo)
END IF
CALL Display(ALLOCATED(obj%colFedofs), msg="colFedofs ALLOCATED: ", &
             unitNo=unitNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractFieldDeallocate(obj)
obj%isPmatInitiated = math%no
CALL NullifyFEDOF(obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                   isPreconditionSet
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsPreconditionSet
ans = obj%isPmatInitiated
END PROCEDURE obj_IsPreconditionSet

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SPY
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SPY()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'This routine should be implemented by the child class.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SymSchurLargestEigenVal
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SymSchurLargestEigenVal()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'This routine should be implemented by the child class.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SymSchurLargestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SymLargestEigenVal
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SymLargestEigenVal()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'This routine should be implemented by the child class.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SymLargestEigenVal

!----------------------------------------------------------------------------
!                                                                  SetFEDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFEDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFEDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL NullifyFEDOF(obj)

! rows
IF (PRESENT(rowFedof)) obj%rowFedof => rowFedof
IF (PRESENT(rowGeoFedof)) obj%rowGeoFedof => rowGeoFedof
IF (PRESENT(rowFedofs)) CALL CopyFEDOFPointer(rowFedofs, obj%rowFedofs)
IF (PRESENT(rowGeoFedofs)) THEN
  CALL CopyFEDOFPointer(rowGeoFedofs, obj%rowGeoFedofs)
END IF

! columns, the default is the row value (square matrix)
IF (PRESENT(colFedof)) THEN
  obj%colFedof => colFedof
ELSE
  obj%colFedof => obj%rowFedof
END IF

IF (PRESENT(colGeoFedof)) THEN
  obj%colGeoFedof => colGeoFedof
ELSE
  obj%colGeoFedof => obj%rowGeoFedof
END IF

IF (PRESENT(colFedofs)) THEN
  CALL CopyFEDOFPointer(colFedofs, obj%colFedofs)
ELSE IF (ALLOCATED(obj%rowFedofs)) THEN
  CALL CopyFEDOFPointer(obj%rowFedofs, obj%colFedofs)
END IF

IF (PRESENT(colGeoFedofs)) THEN
  CALL CopyFEDOFPointer(colGeoFedofs, obj%colGeoFedofs)
ELSE IF (ALLOCATED(obj%rowGeoFedofs)) THEN
  CALL CopyFEDOFPointer(obj%rowGeoFedofs, obj%colGeoFedofs)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFEDOF

!----------------------------------------------------------------------------
!                                                        GetRowFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRowFEDOFPointer1
ans => GetFEDOFPointer(obj%rowFedof, obj%rowFedofs, indx)
END PROCEDURE obj_GetRowFEDOFPointer1

!----------------------------------------------------------------------------
!                                                        GetRowFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRowFEDOFPointer2
IF (ALLOCATED(obj%rowFedofs)) THEN
  CALL CopyFEDOFPointer(obj%rowFedofs, ans)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE obj_GetRowFEDOFPointer2

!----------------------------------------------------------------------------
!                                                        GetColFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColFEDOFPointer1
ans => GetFEDOFPointer(obj%colFedof, obj%colFedofs, indx)
END PROCEDURE obj_GetColFEDOFPointer1

!----------------------------------------------------------------------------
!                                                        GetColFEDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColFEDOFPointer2
IF (ALLOCATED(obj%colFedofs)) THEN
  CALL CopyFEDOFPointer(obj%colFedofs, ans)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE obj_GetColFEDOFPointer2

!----------------------------------------------------------------------------
!                                                           GetFEDOFPointer
!----------------------------------------------------------------------------

! Returns fedofs(indx)%ptr when indx is present and fedofs is allocated,
! otherwise fedof.
FUNCTION GetFEDOFPointer(fedof, fedofs, indx) RESULT(ans)
  CLASS(FEDOF_), POINTER, INTENT(IN) :: fedof
  TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(IN) :: fedofs(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: indx
  CLASS(FEDOF_), POINTER :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetFEDOFPointer()"
#endif

  IF (PRESENT(indx) .AND. ALLOCATED(fedofs)) THEN
#ifdef DEBUG_VER
    CALL AssertError3(indx, SIZE(fedofs), myName, &
                      "a=indx, b=SIZE(fedofs),")
#endif
    ans => fedofs(indx)%ptr
  ELSE
    ans => fedof
  END IF
END FUNCTION GetFEDOFPointer

!----------------------------------------------------------------------------
!                                                          CopyFEDOFPointer
!----------------------------------------------------------------------------

! Allocates dst with the size of src and points each entry to the
! FEDOF of src. The FEDOF objects are not copied.
SUBROUTINE CopyFEDOFPointer(src, dst)
  TYPE(FEDOFPointer_), INTENT(IN) :: src(:)
  TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: dst(:)

  INTEGER(I4B) :: ii

  IF (ALLOCATED(dst)) DEALLOCATE (dst)
  ALLOCATE (dst(SIZE(src)))
  DO ii = 1, SIZE(src)
    dst(ii)%ptr => src(ii)%ptr
  END DO
END SUBROUTINE CopyFEDOFPointer

!----------------------------------------------------------------------------
!                                                              NullifyFEDOF
!----------------------------------------------------------------------------

! Nullifies the row and column FEDOF pointers of obj. The FEDOF objects
! are not deallocated, because a square matrix shares them between the
! rows and the columns, and the node field owns them.
SUBROUTINE NullifyFEDOF(obj)
  CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj

  obj%rowFedof => NULL()
  obj%colFedof => NULL()
  obj%rowGeoFedof => NULL()
  obj%colGeoFedof => NULL()
  CALL NullifyFEDOFPointer(obj%rowFedofs)
  CALL NullifyFEDOFPointer(obj%colFedofs)
  CALL NullifyFEDOFPointer(obj%rowGeoFedofs)
  CALL NullifyFEDOFPointer(obj%colGeoFedofs)
END SUBROUTINE NullifyFEDOF

!----------------------------------------------------------------------------
!                                                       NullifyFEDOFPointer
!----------------------------------------------------------------------------

! Nullifies each entry of fedofs and deallocates the array.
SUBROUTINE NullifyFEDOFPointer(fedofs)
  TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedofs(:)

  INTEGER(I4B) :: ii

  IF (.NOT. ALLOCATED(fedofs)) RETURN
  DO ii = 1, SIZE(fedofs)
    fedofs(ii)%ptr => NULL()
  END DO
  DEALLOCATE (fedofs)
END SUBROUTINE NullifyFEDOFPointer

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
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
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                              SetBrokenLagrangeMatrixForVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetBrokenLagrangeMatrixForVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetBrokenLagrangeMatrixForVTK()"
#endif
INTEGER(I4B) :: tElements, maxNNE, iel, jj, &
                refElemCoord_nrow, refElemCoord_ncol, xij_nrow, xij_ncol, &
                rowFecon_size, colFecon_size, cellOrder_size
INTEGER(I4B) :: cellOrder(3), ipType(3)
REAL(DFP), ALLOCATABLE :: refElemCoord(:, :), xij(:, :), VALUE(:, :)
INTEGER(I4B), ALLOCATABLE :: rowFecon(:), colFecon(:)
LOGICAL(LGT) :: isok
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: elemsd
CLASS(AbstractMesh_), POINTER :: colMesh
CLASS(FEDOF_), POINTER :: rowFedof, colFedof
CLASS(AbstractFE_), POINTER :: feptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (ivar .GT. math%one_i) THEN
  isok = ALLOCATED(obj%rowFedofs)
  CALL AssertError1(isok, myName, "rowFedofs not allocated.")
END IF
#endif

#ifdef DEBUG_VER
IF (jvar .GT. math%one_i) THEN
  isok = ALLOCATED(obj%colFedofs)
  CALL AssertError1(isok, myName, "colFedofs not allocated.")
END IF
#endif

rowFedof => GetFEDOFPointer(obj%rowFedof, obj%rowFedofs, ivar)
colFedof => GetFEDOFPointer(obj%colFedof, obj%colFedofs, jvar)

#ifdef DEBUG_VER
isok = ASSOCIATED(rowFedof)
CALL AssertError1(isok, myName, "rowFedof is not associated.")
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(colFedof)
CALL AssertError1(isok, myName, "colFedof is not associated.")
#endif

#ifdef DEBUG_VER
isok = rowFedof%IsInitiated()
CALL AssertError1(isok, myName, "rowFedof is not initiated")
#endif

#ifdef DEBUG_VER
isok = colFedof%IsInitiated()
CALL AssertError1(isok, myName, "colFedof is not initiated")
#endif

colMesh => colFedof%GetMeshPointer()

#ifdef DEBUG_VER
isok = ASSOCIATED(rowFedof%GetMeshPointer(), colMesh)
CALL AssertError1(isok, myName, &
                  "rowMesh and colMesh are not same.")
#endif

tElements = colMesh%GetTotalElements()
ipType = TypeInterpolationOpt%Equidistance
maxNNE = colMesh%GetMaxNNE()

! The VTK points of an element are the dofs of the broken Lagrange
! rowFedof, so xij and rowFecon have the same size.
CALL Reallocate(refElemCoord, 3, maxNNE)
refElemCoord = math%zero
CALL Reallocate(xij, 4, rowFedof%GetMaxTotalConnectivity())
xij = math%zero
CALL Reallocate(rowFecon, rowFedof%GetMaxTotalConnectivity())
CALL Reallocate(colFecon, colFedof%GetMaxTotalConnectivity())
CALL Reallocate(VALUE, SIZE(rowFecon), SIZE(colFecon))

elemLoop: DO iel = 1, tElements
  isok = colMesh%IsElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE elemLoop

  ! order of the VTK cell, isotropic order is assumed
  CALL colFedof%GetCellOrder(ans=cellOrder, tsize=cellOrder_size, &
                             globalElement=iel, islocal=math%yes)
  cellOrder(2:3) = cellOrder(1)

  CALL colFedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => colFedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  ! points of the vtk cell in the reference element
  CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=refElemCoord_nrow, &
                             ncol=refElemCoord_ncol)

  CALL feptr%GetInterpolationPoints(xij=refElemCoord(1:refElemCoord_nrow, &
                                                     1:refElemCoord_ncol), &
                                    ans=xij, nrow=xij_nrow, ncol=xij_ncol, &
                                    order=cellOrder, ipType=ipType)

  CALL QuadraturePoint_Initiate(obj=quad, points=xij(1:4, 1:xij_ncol))

  CALL feptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

  CALL colFedof%GetConnectivity_(ans=colFecon, &
                                 tsize=colFecon_size, opt="A", &
                                 globalElement=iel, islocal=math%yes)
  CALL feptr%FixOrientOfNptrs(colFecon, colFecon_size)

  CALL rowFedof%GetConnectivity_(ans=rowFecon, &
                                 tsize=rowFecon_size, opt="A", &
                                 globalElement=iel, islocal=math%yes)

  VALUE(1:rowFecon_size, 1:colFecon_size) = &
    TRANSPOSE(elemsd%N(1:elemsd%nns, 1:elemsd%nips))

  DO jj = 1, nCopy
    CALL obj%Set(iNodeNum=rowFecon(1:rowFecon_size), &
                 jNodeNum=colFecon(1:colFecon_size), &
                 islocal=math%yes, &
                 ivar=ivar, jvar=jvar, idof=jj, jdof=jj, &
                 VALUE=VALUE(1:rowFecon_size, 1:colFecon_size))
  END DO

END DO elemLoop

IF (ALLOCATED(refElemCoord)) DEALLOCATE (refElemCoord)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(VALUE)) DEALLOCATE (VALUE)
IF (ALLOCATED(rowFecon)) DEALLOCATE (rowFecon)
IF (ALLOCATED(colFecon)) DEALLOCATE (colFecon)
CALL QuadraturePoint_Deallocate(quad)
CALL ElemshapeData_Deallocate(elemsd)
NULLIFY (colMesh, rowFedof, colFedof, feptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetBrokenLagrangeMatrixForVTK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
