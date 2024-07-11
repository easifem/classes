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

SUBMODULE(AbstractFE_Class) ConstructorMethods
USE BaseType, ONLY: TypeElemNameOpt, &
                    TypePolynomialOpt, &
                    TypeQuadratureOpt
USE GlobalData, ONLY: Scalar

USE StringUtility, ONLY: UpperCase

USE Display_Method, ONLY: ToString

USE InputUtility, ONLY: Input

USE ExceptionHandler_Class, ONLY: e

USE FPL_Method, ONLY: GetValue, CheckEssentialParam, Set

USE RefElementFactory, ONLY: RefElement_Pointer

USE InterpolationUtility, ONLY: RefElemDomain

USE ReferenceElement_Method, ONLY: ElementTopology, &
                                   XiDimension, &
                                   GetTotalEdges, &
                                   GetTotalFaces, &
                                   GetTotalCells, &
                                   RefCoord_

USE LagrangePolynomialUtility, ONLY: LagrangeDOF

USE HierarchicalPolynomialUtility, ONLY: HierarchicalDOF

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
CHARACTER(4) :: astr

astr = UpperCase(baseInterpolation(1:4))

SELECT CASE (astr)
CASE ("LAGR")
  CALL obj_LagrangeFE(obj=obj, fetype=fetype, elemType=elemType, nsd=nsd, &
         baseContinuity=baseContinuity, baseInterpolation=baseInterpolation, &
                 ipType=ipType, basisType=basisType, alpha=alpha, beta=beta, &
                      lambda=lambda, order=order, anisoOrder=anisoOrder)

CASE ("HIER", "HEIR")

 CALL obj_HierarchicalFE(obj=obj, fetype=fetype, elemType=elemType, nsd=nsd, &
         baseContinuity=baseContinuity, baseInterpolation=baseInterpolation, &
                          cellOrder=cellOrder, &
            faceOrder=faceOrder, edgeOrder=edgeOrder, cellOrient=cellOrient, &
                          faceOrient=faceOrient, edgeOrient=edgeOrient)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[INTERNAL ERROR] :: No case found for baseInterpolation(1:4)='//astr)
  RETURN

END SELECT

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LagrangeFE
CHARACTER(*), PARAMETER :: myname = "obj_LagrangeFE()"

INTEGER(I4B), PARAMETER :: default_basis(3) = [TypePolynomialOpt%Monomial, &
                                               TypePolynomialOpt%Monomial, &
                                               TypePolynomialOpt%Monomial]
INTEGER(I4B), PARAMETER :: default_anisoOrder(3) = [0, 0, 0]

REAL(DFP), PARAMETER :: default_alpha(3) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
REAL(DFP), PARAMETER :: default_beta(3) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
REAL(DFP), PARAMETER :: default_lambda(3) = [0.5_DFP, 0.5_DFP, 0.5_DFP]
TYPE(String) :: mystr
INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInitiated = .TRUE.
obj%elemType = elemType
obj%nsd = nsd
obj%baseInterpolation = UpperCase(baseInterpolation(1:4))
obj%baseContinuity = UpperCase(baseContinuity(1:2))
obj%ipType = ipType
obj%topoType = ElementTopology(elemType)
obj%fetype = fetype
obj%xidim = XiDimension(elemType)
mystr = RefElemDomain(elemType=elemType, baseContinuity=baseContinuity, &
                      baseInterpol=baseInterpolation)
obj%refelemDomain = mystr%Slice(1, 1)
mystr = ""

CALL RefCoord_(elemType=elemType, ans=obj%refelemCoord, nrow=ii, ncol=jj, &
               refelem=obj%refelemDomain)

CALL obj_SetIntegerType(a=obj%basisType, default_a=default_basis, b=basisType, &
                        n=obj%xidim)

CALL obj_SetRealType(a=obj%alpha, default_a=default_alpha, b=alpha, &
                     n=obj%xidim)

CALL obj_SetRealType(a=obj%beta, default_a=default_beta, b=beta, &
                     n=obj%xidim)

CALL obj_SetRealType(a=obj%lambda, default_a=default_lambda, b=lambda, &
                     n=obj%xidim)

obj%isIsotropicOrder = PRESENT(order)

IF (obj%isIsotropicOrder) THEN
  obj%order = order
  obj%tdof = LagrangeDOF(order=order, elemType=elemType)
END IF

obj%isAnisotropicOrder = PRESENT(anisoOrder)
IF (obj%isAnisotropicOrder) THEN
  CALL obj_SetIntegerType(a=obj%anisoOrder, default_a=default_anisoOrder, &
                          b=anisoOrder, n=obj%xidim)

  obj%tdof = LagrangeDOF(p=obj%anisoOrder(1), q=obj%anisoOrder(2), &
                         r=obj%anisoOrder(3), elemType=elemType)

END IF

obj%edgeOrder = 0
obj%tEdgeOrder = 0
obj%isEdgeOrder = .FALSE.

obj%faceOrder = 0
obj%tFaceOrder = 0
obj%isFaceOrder = .FALSE.

obj%cellOrder = 0
obj%tCellOrder = 0
obj%isCellOrder = .FALSE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_LagrangeFE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_SetIntegerType(a, default_a, n, b)
  INTEGER(I4B), INTENT(INOUT) :: a(:)
  INTEGER(I4B), INTENT(IN) :: default_a(:)
  INTEGER(I4B), INTENT(IN) :: n
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: b(:)

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, tsize

  isok = PRESENT(b)

  IF (.NOT. isok) THEN

    DO ii = 1, n
      a(ii) = default_a(ii)
    END DO

    RETURN

  END IF

  tsize = SIZE(b)

  isok = tsize .EQ. 1
  IF (isok) THEN
    DO ii = 1, n
      a(ii) = b(1)
    END DO
    RETURN
  END IF

  ! isok = tsize .EQ. n
  ! IF (isok) THEN

  DO ii = 1, n
    a(ii) = b(ii)
  END DO
  ! RETURN

  ! END IF

END SUBROUTINE obj_SetIntegerType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_SetRealType(a, default_a, n, b)
  REAL(DFP), INTENT(INOUT) :: a(:)
  REAL(DFP), INTENT(IN) :: default_a(:)
  INTEGER(I4B), INTENT(IN) :: n
  REAL(DFP), OPTIONAL, INTENT(IN) :: b(:)

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, tsize

  isok = PRESENT(b)

  IF (.NOT. isok) THEN

    DO ii = 1, n
      a(ii) = default_a(ii)
    END DO

    RETURN

  END IF

  tsize = SIZE(b)

  isok = tsize .EQ. 1
  IF (isok) THEN
    DO ii = 1, n
      a(ii) = b(1)
    END DO
    RETURN
  END IF

  ! isok = tsize .EQ. n
  ! IF (isok) THEN

  DO ii = 1, n
    a(ii) = b(ii)
  END DO
  ! RETURN

  ! END IF

END SUBROUTINE obj_SetRealType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_HierarchicalFE
CHARACTER(*), PARAMETER :: myname = "obj_HierarchicalFE()"
INTEGER(I4B) :: ii
TYPE(String) :: mystr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInitiated = .TRUE.
obj%elemType = elemType
obj%nsd = nsd
obj%baseInterpolation = UpperCase(baseInterpolation(1:4))
obj%baseContinuity = UpperCase(baseContinuity(1:2))
obj%topoType = ElementTopology(elemType)
obj%fetype = fetype

mystr = RefElemDomain(elemType=elemType, baseContinuity=baseContinuity, &
                      baseInterpol=baseInterpolation)
obj%refelemDomain = mystr%Slice(1, 1)
mystr = ""

obj%xidim = XiDimension(elemType)

! For 1D elements cellOrder should be present
! For 2D elements cellOrder, faceOrder should be present
! For 3D elements cellOrder, faceOrder, and edgeOrder should should be present

#ifdef DEBUG_VER
CALL checkerror
#endif

obj%tdof = HierarchicalDOF(elemType=elemType, cellOrder=cellOrder, &
                           faceOrder=faceOrder, edgeOrder=edgeOrder)

obj%isCellOrder = .TRUE.
obj%tCellOrder = SIZE(cellOrder)
DO ii = 1, obj%tCellOrder
  obj%cellOrder(ii) = cellOrder(ii)
END DO

! IF (obj%xidim .GE. 2) THEN
!
!   obj%isFaceOrder = .TRUE.
!   obj%tFaceOrder = SIZE(faceOrder)
!
!   DO ii = 1, obj%tFaceOrder
!     obj%cellOrder(ii) = cellOrder(ii)
!   END DO
!
! END IF

IF (obj%xidim .GE. 3) THEN

  obj%isEdgeOrder = .TRUE.
  obj%tEdgeOrder = SIZE(edgeOrder)
  DO ii = 1, obj%tEdgeOrder
    obj%edgeOrder(ii) = edgeOrder(ii)
  END DO

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

#ifdef DEBUG_VER

CONTAINS

SUBROUTINE checkerror

  LOGICAL(LGT) :: isok, abool

  isok = PRESENT(cellOrder)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: cellOrder is not present.')
    RETURN
  END IF

  abool = obj%xidim .GE. 2
  IF (abool) THEN
    isok = PRESENT(faceOrder)

    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: faceOrder is not present.')
      RETURN
    END IF
  END IF

  abool = obj%xidim .GE. 3
  IF (abool) THEN
    isok = PRESENT(edgeOrder)

    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: edgeOrder is not present.')
      RETURN
    END IF
  END IF

END SUBROUTINE checkerror

#endif

END PROCEDURE obj_HierarchicalFE

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%firstCall = obj2%firstCall
obj%isInitiated = obj2%isInitiated
obj%nsd = obj2%nsd
obj%order = obj2%order
obj%isIsotropicOrder = obj2%isIsotropicOrder
obj%anisoOrder = obj2%anisoOrder
obj%isAnisotropicOrder = obj2%isAnisotropicOrder
obj%edgeOrder = obj2%edgeOrder
obj%tEdgeOrder = obj2%tEdgeOrder
obj%isEdgeOrder = obj2%isEdgeOrder
obj%faceOrder = obj2%faceOrder
obj%tFaceOrder = obj2%tFaceOrder
obj%isFaceOrder = obj2%isFaceOrder
obj%cellOrder = obj2%cellOrder
obj%tCellOrder = obj2%tCellOrder
obj%isCellOrder = obj2%isCellOrder
obj%feType = obj2%feType
obj%elemType = obj2%elemType
obj%ipType = obj2%ipType
obj%dofType = obj2%dofType
obj%transformType = obj2%transformType
obj%baseContinuity = obj2%baseContinuity
obj%baseInterpolation = obj2%baseInterpolation
obj%basisType = obj2%basisType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%refElemDomain = obj2%refElemDomain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Copy

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

obj%firstCall = .TRUE.

obj%nsd = 0
obj%order = 0
obj%isIsotropicOrder = .FALSE.
obj%anisoOrder = 0_I4B
obj%isAnisotropicOrder = .FALSE.
obj%edgeOrder = 0_I4B
obj%tEdgeOrder = 0_I4B
obj%isEdgeOrder = .FALSE.
obj%faceOrder = 0
obj%tFaceOrder = 0
obj%isFaceOrder = .FALSE.
obj%cellOrder = 0
obj%tCellOrder = 0
obj%isCellOrder = .FALSE.
obj%feType = 0
obj%elemType = 0
obj%ipType = 0
obj%dofType = 0
obj%transformType = 0
obj%basisType = 0
obj%alpha = 0.0
obj%beta = 0.0
obj%lambda = 0.0
obj%baseContinuity = "  "
obj%baseInterpolation = "    "
obj%refElemDomain = " "

obj%isInitiated = .FALSE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Ptr_Vector()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
