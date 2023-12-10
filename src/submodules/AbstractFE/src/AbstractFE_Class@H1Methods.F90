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

SUBMODULE(AbstractFE_Class) H1Methods
USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE

INTERFACE GetLocalElemShapeData_H1
  MODULE PROCEDURE obj_GetLocalElemshapeData_H1_Lagrange
  MODULE PROCEDURE obj_GetLocalElemshapeData_H1_Orthogonal
  MODULE PROCEDURE obj_GetLocalElemshapeData_H1_Hierarchy
  MODULE PROCEDURE obj_GetLocalElemshapeData_H1_Hermit
  MODULE PROCEDURE obj_GetLocalElemshapeData_H1_Serendipity
END INTERFACE GetLocalElemShapeData_H1

CONTAINS

!----------------------------------------------------------------------------
!                                                 GetLocalElemShapeData_H1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData_H1_Master
CHARACTER(*), PARAMETER :: myName = "GetLocalElemShapeData_H1_Master()"

SELECT TYPE (baseInterpolation => obj%baseInterpolation)
CLASS IS (LagrangeInterpolation_)
  CALL GetLocalElemShapeData_H1(obj, elemsd, quad, baseInterpolation)
CLASS IS (OrthogonalInterpolation_)
  CALL GetLocalElemShapeData_H1(obj, elemsd, quad, baseInterpolation)
CLASS IS (HierarchyInterpolation_)
  CALL GetLocalElemShapeData_H1(obj, elemsd, quad, baseInterpolation)
CLASS IS (HermitInterpolation_)
  CALL GetLocalElemShapeData_H1(obj, elemsd, quad, baseInterpolation)
CLASS IS (SerendipityInterpolation_)
  CALL GetLocalElemShapeData_H1(obj, elemsd, quad, baseInterpolation)
CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[NO CASE FOUND] no case found for AbstractFE_::obj%baseInterpolation')
END SELECT
END PROCEDURE obj_GetLocalElemShapeData_H1_Master

!----------------------------------------------------------------------------
!                                                 GetGlobalElemShapeData_H1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData_H1_Master
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData_H1_Master"
IF (SIZE(xij, 1) .NE. obj%nsd) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[WRONG ARGS] size(xij, 1) .NE. obj%nsd')
END IF
IF (PRESENT(geoElemsd)) THEN
  IF (SIZE(xij, 2) .NE. SIZE(geoElemsd%N, 1)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[WRONG ARGS] size(xij, 2) .NE. size(geoElemsd%N, 1)')
  END IF
  CALL Set(obj=elemsd, val=xij, N=geoElemsd%N, dNdXi=geoElemsd%dNdXi)
  RETURN
END IF
IF (SIZE(xij, 2) .NE. SIZE(elemsd%N, 1)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[WRONG ARGS] size(xij, 2) .NE. size(elemsd%N, 1)')
END IF
CALL Set(obj=elemsd, val=xij, N=elemsd%N, dNdXi=elemsd%dNdXi)
END PROCEDURE obj_GetGlobalElemShapeData_H1_Master

!----------------------------------------------------------------------------
!                                         GetLocalElemshapeData_H1_Lagrange
!----------------------------------------------------------------------------

SUBROUTINE obj_GetLocalElemshapeData_H1_Lagrange(obj, elemsd, quad,  &
  & baseInterpolation)
  CLASS(AbstractFE_), INTENT(INOUT) :: obj
  CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  CLASS(QuadraturePoint_), INTENT(IN) :: quad
  CLASS(LagrangeInterpolation_), INTENT(IN) :: baseInterpolation
  CHARACTER(*), PARAMETER :: myName = "GetLocalElemshapeData_H1_Lagrange"
  IF (obj%isIsotropicOrder) THEN
    CALL Initiate( &
      & obj=elemsd,  &
      & quad=quad,  &
      & refelem=obj%refelem0,  &
      & baseContinuity=TypeH1,  &
      & baseInterpolation=baseInterpolation,  &
      & order=obj%order,  &
      & ipType=obj%ipType,  &
      & basisType=obj%basisType(1),  &
      & coeff=obj%coeff,  &
      & firstCall=obj%firstCall,  &
      & alpha=obj%alpha(1),  &
      & beta=obj%beta(1), &
      & lambda=obj%lambda(1))
    obj%firstCall = .TRUE.
    RETURN
  END IF
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[WIP] This routine at present support isIsotropicOrder  &
    & for H1 Lagrange shape functions')
! TODO: Implement GetLocalElemshapeData_H1_Lagrange for anisotropic order
END SUBROUTINE obj_GetLocalElemshapeData_H1_Lagrange

!----------------------------------------------------------------------------
!                                         GetLocalElemshapeData_H1_Orthogonal
!----------------------------------------------------------------------------

SUBROUTINE obj_GetLocalElemshapeData_H1_Orthogonal(obj, elemsd, quad,  &
  & baseInterpolation)
  CLASS(AbstractFE_), INTENT(INOUT) :: obj
  CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  CLASS(QuadraturePoint_), INTENT(IN) :: quad
  CLASS(OrthogonalInterpolation_), INTENT(IN) :: baseInterpolation
  CHARACTER(*), PARAMETER :: myName = "GetLocalElemshapeData_H1_Orthogonal"
  TYPE(ReferenceElement_) :: refelem

  CALL obj%refelem%GetParam(refelem=refelem)
  IF (obj%isIsotropicOrder) THEN
    CALL Initiate( &
      & obj=elemsd,  &
      & quad=quad,  &
      & refelem=obj%refelem0,  &
      & baseContinuity=TypeH1,  &
      & baseInterpolation=baseInterpolation,  &
      & order=obj%order,  &
      & ipType=obj%ipType,  &
      & basisType=obj%basisType(1),  &
      & alpha=obj%alpha(1),  &
      & beta=obj%beta(1), &
      & lambda=obj%lambda(1))
    RETURN
  END IF

  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[WIP] This routine at present support isIsotropicOrder  &
    & for H1 Lagrange shape functions')

! TODO: Implement GetLocalElemshapeData_H1_Orthogonal for anisotropic order

END SUBROUTINE obj_GetLocalElemshapeData_H1_Orthogonal

!----------------------------------------------------------------------------
!                                         GetLocalElemshapeData_H1_Hierarchy
!----------------------------------------------------------------------------

SUBROUTINE obj_GetLocalElemshapeData_H1_Hierarchy(obj, elemsd, quad,  &
  & baseInterpolation)
  CLASS(AbstractFE_), INTENT(INOUT) :: obj
  CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  CLASS(QuadraturePoint_), INTENT(IN) :: quad
  CLASS(HierarchyInterpolation_), INTENT(IN) :: baseInterpolation
  CHARACTER(*), PARAMETER :: myName = "GetLocalElemshapeData_H1_Hierarchy"
  TYPE(ReferenceElement_) :: refelem

  CALL obj%refelem%GetParam(refelem=refelem)

  IF (obj%isIsotropicOrder) THEN
    CALL Initiate( &
      & obj=elemsd,  &
      & quad=quad,  &
      & refelem=obj%refelem0,  &
      & baseContinuity=TypeH1,  &
      & baseInterpolation=baseInterpolation,  &
      & order=obj%order,  &
      & ipType=obj%ipType,  &
      & basisType=obj%basisType(1),  &
      & alpha=obj%alpha(1),  &
      & beta=obj%beta(1), &
      & lambda=obj%lambda(1))
    RETURN
  END IF

  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[WIP] This routine at present support isIsotropicOrder  &
    & for H1 Lagrange shape functions')

! TODO: Implement GetLocalElemshapeData_H1_Hierarchy for anisotropic order
END SUBROUTINE obj_GetLocalElemshapeData_H1_Hierarchy

!----------------------------------------------------------------------------
!                                         GetLocalElemshapeData_H1_Hermit
!----------------------------------------------------------------------------

SUBROUTINE obj_GetLocalElemshapeData_H1_Serendipity(obj, elemsd, quad,  &
  & baseInterpolation)
  CLASS(AbstractFE_), INTENT(INOUT) :: obj
  CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  CLASS(QuadraturePoint_), INTENT(IN) :: quad
  CLASS(SerendipityInterpolation_), INTENT(IN) :: baseInterpolation

  CHARACTER(*), PARAMETER :: myName = "GetLocalElemshapeData_H1_Hermit"
  TYPE(ReferenceElement_) :: refelem

  CALL obj%refelem%GetParam(refelem=refelem)

  IF (obj%isIsotropicOrder) THEN
    CALL Initiate( &
      & obj=elemsd,  &
      & quad=quad,  &
      & refelem=obj%refelem0,  &
      & baseContinuity=TypeH1,  &
      & baseInterpolation=baseInterpolation,  &
      & order=obj%order,  &
      & ipType=obj%ipType,  &
      & basisType=obj%basisType(1),  &
      & alpha=obj%alpha(1),  &
      & beta=obj%beta(1), &
      & lambda=obj%lambda(1))
    RETURN
  END IF

  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[WIP] This routine at present support isIsotropicOrder  &
    & for H1 Lagrange shape functions')

! TODO: Implement GetLocalElemshapeData_H1_Serendipity for anisotropic order
END SUBROUTINE obj_GetLocalElemshapeData_H1_Serendipity

!----------------------------------------------------------------------------
!                                         GetLocalElemshapeData_H1_Serendipity
!----------------------------------------------------------------------------

SUBROUTINE obj_GetLocalElemshapeData_H1_Hermit(obj, elemsd, quad,  &
  & baseInterpolation)
  CLASS(AbstractFE_), INTENT(INOUT) :: obj
  CLASS(ElemShapedata_), INTENT(INOUT) :: elemsd
  CLASS(QuadraturePoint_), INTENT(IN) :: quad
  CLASS(HermitInterpolation_), INTENT(IN) :: baseInterpolation
  CHARACTER(*), PARAMETER :: myName = "GetLocalElemshapeData_H1_Serendipity"
  TYPE(ReferenceElement_) :: refelem

  CALL obj%refelem%GetParam(refelem=refelem)

  IF (obj%isIsotropicOrder) THEN
    CALL Initiate( &
      & obj=elemsd,  &
      & quad=quad,  &
      & refelem=obj%refelem0,  &
      & baseContinuity=TypeH1,  &
      & baseInterpolation=baseInterpolation,  &
      & order=obj%order,  &
      & ipType=obj%ipType,  &
      & basisType=obj%basisType(1),  &
      & alpha=obj%alpha(1),  &
      & beta=obj%beta(1), &
      & lambda=obj%lambda(1))
    RETURN
  END IF

  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[WIP] This routine at present support isIsotropicOrder  &
    & for H1 Lagrange shape functions')

! TODO: Implement GetLocalElemshapeData_H1_Serendipity
! for anisotropic order
END SUBROUTINE obj_GetLocalElemshapeData_H1_Hermit

!----------------------------------------------------------------------------
!                                               GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetElemShapeData
CHARACTER(*), PARAMETER :: myName="get_GetLocalFacetElemShapeData"

CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[WIP] This routine is not avaiable yet.')

! TODO: Implement obj_GetLocalFacetElemShapeData
! for anisotropic order

! CALL Set(  &
! & facetobj=facetobj,  &
! & cellobj=cellobj,  &
! & cellval=cellval, &
! & cellN=cellN, &
! & celldNdXi=celldNdXi, &
! & facetN=facetN, &
! & ffacetdNdXi=facetdNdXi)
END PROCEDURE obj_GetLocalFacetElemShapeData

END SUBMODULE H1Methods
