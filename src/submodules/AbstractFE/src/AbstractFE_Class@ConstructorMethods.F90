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
USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
USE FPL_Method, ONLY: GetValue, CheckEssentialParam
USE RefElementFactory, ONLY: RefElement_Pointer
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL CheckEssentialParam(obj=param,  &
  & keys=AbstractFEEssentialParams,  &
  & prefix=obj%GetPrefix(),  &
  & myName=myName,  &
  & modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                       SetAbstractFEParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractFEParam
INTEGER(I4B) :: ierr, ipType0
TYPE(String) :: astr
CHARACTER(*), PARAMETER :: myName = "SetAbstractFEParam()"
TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

sublist => NULL()
sublist => param%NewSubList(key=prefix)

ierr = sublist%Set(key=prefix//"/nsd", VALUE=nsd)
ierr = sublist%Set(key=prefix//"/elemType", VALUE=elemType)
ierr = sublist%Set(key=prefix//"/baseContinuity", VALUE=baseContinuity)
ierr = sublist%Set(key=prefix//"/baseInterpolation", VALUE=baseInterpolation)

! TODO finite element type
CALL e%RaiseWarning(modName//'::'//myName//' - '// &
  & '[BUG] feType, dofType, transformType are not handled properly.')

ierr = sublist%Set(key=prefix//"/feType", VALUE=Scalar)
ierr = sublist%Set(key=prefix//"/dofType", VALUE=DEFAULT_DOF_TYPE)
ierr = sublist%Set(key=prefix//"/transformType", VALUE=DEFAULT_TRANSFORM_TYPE)

astr = UpperCase(baseInterpolation)
IF (astr%chars() .EQ. "LAGRANGE" .OR.  &
  & astr%chars() .EQ. "LAGRANGEPOLYNOMIAL") THEN
  IF (.NOT. PRESENT(ipType)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[ARGUMENT ERROR] In case of LAGRANGE polynomials &
      & ipType should be present.')
  END IF
END IF
ipType0 = input(default=Equidistance, option=ipType)
ierr = sublist%Set(key=prefix//"/ipType", VALUE=ipType)

astr = RefElemDomain( &
  & baseInterpol=baseInterpolation, &
  & baseContinuity=baseContinuity, &
  & elemType=elemType)
ierr = sublist%Set(key=prefix//"/refElemDomain", VALUE=astr%chars())

CALL SetFEParam_BasisType( &
  & param=sublist, &
  & elemType=elemType, &
  & nsd=nsd, &
  & baseContinuity0=UpperCase(baseContinuity),  &
  & baseInterpol0=UpperCase(baseInterpolation),  &
  & basisType=basisType,  &
  & alpha=alpha,  &
  & beta=beta,  &
  & lambda=lambda,  &
  & prefix=prefix)

IF (PRESENT(order)) THEN
  CALL SetFEParam_Order(param=sublist, order=order, elemType=elemType,  &
    & prefix=prefix)
  sublist => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
  RETURN

ELSE

  IF (nsd .EQ. 1_I4B) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[ARGUMENT ERROR] For 1D elements Order must be present.')
    sublist => NULL()
    RETURN
  END IF
END IF

IF (PRESENT(anisoOrder)) THEN
  CALL SetFEParam_AnisoOrder( &
  & param=sublist, &
  & anisoOrder=anisoOrder, &
  & elemType=elemType,  &
  & nsd=nsd,  &
  & prefix=prefix)
  sublist => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

  RETURN
END IF

SELECT CASE (nsd)
CASE (2)
  CALL SetFEParam_Heirarchy2D( &
  & param=sublist,  &
  & elemType=elemType,  &
  & nsd=nsd, &
  & edgeOrder=edgeOrder, &
  & faceOrder=faceOrder,  &
  & prefix=prefix)
CASE (3)
  CALL SetFEParam_Heirarchy3D(  &
    & param=sublist, &
    & elemType=elemType, &
    & nsd=nsd, &
    & edgeOrder=edgeOrder, &
    & faceOrder=faceOrder, &
    & cellOrder=cellOrder,  &
    & prefix=prefix)
END SELECT

sublist => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE SetAbstractFEParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType_Line( &
  & param, elemType, nsd, baseContinuity0, baseInterpol0, &
  & basisType, alpha, beta, lambda, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpol0
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
  CHARACTER(*), INTENT(IN) :: prefix

  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType_Line()"
  INTEGER(I4B) :: basisType0(3), ierr
  REAL(DFP) :: alpha0(3), beta0(3), lambda0(3)
  LOGICAL(LGT) :: isLagrange, isOrthogonal, isBasis

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  alpha0 = 0.0_DFP
  beta0 = 0.0_DFP
  lambda0 = 0.0_DFP
  basisType0 = -1
  isLagrange = baseInterpol0(1:8) .EQ. "LAGRANGE"
  isOrthogonal = baseInterpol0(1:10) .EQ. "ORTHOGONAL"
  isBasis = PRESENT(basisType)

  IF (isLagrange) basisType0 = Monomial
  IF (isOrthogonal) basisType0 = Legendre
  IF (isBasis) basisType0 = basisType(1)
  alpha0 = 0.0_DFP; IF (PRESENT(alpha)) alpha0 = alpha(1)
  beta0 = 0.0_DFP; IF (PRESENT(beta)) beta0 = beta(1)
  lambda0 = 0.5_DFP; IF (PRESENT(lambda)) lambda0 = lambda(1)

  ierr = param%Set(key=prefix//"/alpha", VALUE=alpha0)
  ierr = param%Set(key=prefix//"/beta", VALUE=beta0)
  ierr = param%Set(key=prefix//"/lambda", VALUE=lambda0)
  ierr = param%Set(key=prefix//"/basisType", VALUE=basisType0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetFEParam_BasisType_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType_Simplex( &
  & param, elemType, nsd, baseContinuity0, baseInterpol0, &
  & basisType, alpha, beta, lambda, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpol0
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
  CHARACTER(*), INTENT(IN) :: prefix

  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType_Simplex()"
  INTEGER(I4B) :: basisType0(3), ierr
  REAL(DFP) :: alpha0(3), beta0(3), lambda0(3)
  LOGICAL(LGT) :: isLagrange, isOrthogonal, isBasis

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  alpha0 = 0.0_DFP
  beta0 = 0.0_DFP
  lambda0 = 0.0_DFP
  basisType0 = -1
  isLagrange = baseInterpol0(1:8) .EQ. "LAGRANGE"
  isOrthogonal = baseInterpol0(1:10) .EQ. "ORTHOGONAL"
  isBasis = PRESENT(basisType)
  IF (isLagrange) basisType0 = Monomial
  IF (isOrthogonal) basisType0 = Legendre
  IF (isBasis) basisType0 = basisType(1)
  ierr = param%Set(key=prefix//"/alpha", VALUE=alpha0)
  ierr = param%Set(key=prefix//"/beta", VALUE=beta0)
  ierr = param%Set(key=prefix//"/lambda", VALUE=lambda0)
  ierr = param%Set(key=prefix//"/basisType", VALUE=basisType0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetFEParam_BasisType_Simplex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType_Cartesian( &
  & param, elemType, nsd, baseContinuity0, baseInterpol0, &
  & basisType, alpha, beta, lambda, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpol0
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
  CHARACTER(*), INTENT(IN) :: prefix

  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType_Cartesian()"
  INTEGER(I4B) :: xidim, basisType0(3), ierr
  REAL(DFP) :: alpha0(3), beta0(3), lambda0(3)
  LOGICAL(LGT) :: isLagrange, isOrthogonal, isBasis

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  alpha0 = 0.0_DFP
  beta0 = 0.0_DFP
  lambda0 = 0.5_DFP
  basisType0 = -1
  isLagrange = baseInterpol0(1:8) .EQ. "LAGRANGE"
  isOrthogonal = baseInterpol0(1:10) .EQ. "ORTHOGONAL"
  isBasis = PRESENT(basisType)

  xidim = XiDimension(elemType)

  IF (isLagrange) basisType0(1:xidim) = Monomial
  IF (isOrthogonal) basisType0(1:xidim) = Legendre

  IF (isBasis) THEN
    IF (SIZE(basisType) .EQ. 1_I4B) THEN
      basisType0 = basisType(1)
    ELSE
      basisType0(1:xidim) = basisType(1:xidim)
    END IF
  END IF

  IF (PRESENT(alpha)) THEN
    IF (SIZE(alpha) .EQ. 1) THEN
      alpha0 = alpha(1)
    ELSE
      alpha0(1:xidim) = alpha(1:xidim)
    END IF
  END IF

  IF (PRESENT(beta)) THEN
    IF (SIZE(beta) .EQ. 1) THEN
      beta0 = beta(1)
    ELSE
      beta0(1:xidim) = beta(1:xidim)
    END IF
  END IF

  IF (PRESENT(lambda)) THEN
    IF (SIZE(lambda) .EQ. 1) THEN
      lambda0 = lambda(1)
    ELSE
      lambda0(1:xidim) = lambda(1:xidim)
    END IF
  END IF

  ierr = param%Set(key=prefix//"/alpha", VALUE=alpha0)
  ierr = param%Set(key=prefix//"/beta", VALUE=beta0)
  ierr = param%Set(key=prefix//"/lambda", VALUE=lambda0)
  ierr = param%Set(key=prefix//"/basisType", VALUE=basisType0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetFEParam_BasisType_Cartesian

!----------------------------------------------------------------------------
!                                                       SetFEParam_BasisType
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType( &
  & param, elemType, nsd, baseContinuity0, baseInterpol0, &
  & basisType, alpha, beta, lambda, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpol0
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
  CHARACTER(*), INTENT(IN) :: prefix

  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType()"
  INTEGER(I4B) :: topoType

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  topoType = ElementTopology(elemType)
  SELECT CASE (topoType)
  CASE (Line)
    CALL SetFEParam_BasisType_Line( &
      & param, elemType, nsd, baseContinuity0, baseInterpol0, &
      & basisType, alpha, beta, lambda, prefix)

  CASE (Triangle, Tetrahedron, Prism, Pyramid)
    CALL SetFEParam_BasisType_Simplex( &
      & param, elemType, nsd, baseContinuity0, baseInterpol0, &
      & basisType, alpha, beta, lambda, prefix)

  CASE (Quadrangle, Hexahedron)
    CALL SetFEParam_BasisType_Cartesian( &
      & param, elemType, nsd, baseContinuity0, baseInterpol0, &
      & basisType, alpha, beta, lambda, prefix)

  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: No case found for given element type')
    RETURN
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetFEParam_BasisType

!----------------------------------------------------------------------------
!                                                          SetFEParam_Order
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_Order(param, order, elemType, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B), INTENT(IN) :: elemType
  CHARACTER(*), INTENT(IN) :: prefix
  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_Order()"
  INTEGER(I4B) :: tEdgeOrder, tFaceOrder, tCellOrder, order0, &
    &  cellOrder0(3), anisoOrder0(3), ierr, ii
  INTEGER(I4B), ALLOCATABLE :: edgeOrder0(:), faceOrder0(:)
  LOGICAL(LGT) :: isIsotropicOrder, isEdgeOrder, isFaceOrder, &
    & isCellOrder, isAnisotropicOrder

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  tEdgeOrder = 0_I4B
  tFaceOrder = 0_I4B
  tCellOrder = 0_I4B

  isIsotropicOrder = .FALSE.
  isAnisotropicOrder = .FALSE.
  isEdgeOrder = .FALSE.
  isFaceOrder = .FALSE.
  isCellOrder = .FALSE.

  tEdgeOrder = GetTotalEdges(elemType)
  tFaceOrder = GetTotalFaces(elemType) * XiDimension(elemType)

  CALL Reallocate(edgeOrder0, tEdgeOrder)
  CALL Reallocate(faceOrder0, tFaceOrder)

  order0 = -1
  anisoOrder0 = -1
  cellOrder0 = -1

  DO ii = 1, tEdgeOrder
    edgeOrder0(ii) = -1
  END DO

  DO ii = 1, tFaceOrder
    faceOrder0(ii) = -1
  END DO

  isIsotropicOrder = .TRUE.
  order0 = order
  ierr = param%Set(key=prefix//"/order", VALUE=order0)
  ierr = param%Set(key=prefix//"/anisoOrder", VALUE=anisoOrder0)

  IF (tEdgeOrder .NE. 0) THEN
    ierr = param%Set(key=prefix//"/edgeOrder", VALUE=edgeOrder0)
  ELSE
    ierr = param%Set(key=prefix//"/edgeOrder", VALUE=[0])
  END IF

  IF (tFaceOrder .NE. 0) THEN
    ierr = param%Set(key=prefix//"/faceOrder", VALUE=faceOrder0)
  ELSE
    ierr = param%Set(key=prefix//"/faceOrder", VALUE=[0])
  END IF

  ierr = param%Set(key=prefix//"/cellOrder", VALUE=cellOrder0)
  ierr = param%Set(key=prefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
  ierr = param%Set(key=prefix//"/isAnisotropicOrder", &
    & VALUE=isAnisotropicOrder)
  ierr = param%Set(key=prefix//"/isEdgeOrder", VALUE=isEdgeOrder)
  ierr = param%Set(key=prefix//"/isFaceOrder", VALUE=isFaceOrder)
  ierr = param%Set(key=prefix//"/isCellOrder", VALUE=isCellOrder)
  ierr = param%Set(key=prefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  ierr = param%Set(key=prefix//"/tFaceOrder", VALUE=tFaceOrder)
  ierr = param%Set(key=prefix//"/tCellOrder", VALUE=tCellOrder)

  DEALLOCATE (edgeOrder0, faceOrder0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetFEParam_Order

!----------------------------------------------------------------------------
!                                                       SetFEParam_AnisoOrder
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_AnisoOrder(param, anisoOrder, elemType, nsd, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: anisoOrder(3)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: prefix

  ! internal variables
  INTEGER(I4B) :: tEdgeOrder, tFaceOrder, tCellOrder, order0, &
    &  cellOrder0(3), anisoOrder0(3), ierr
  INTEGER(I4B), ALLOCATABLE :: edgeOrder0(:), faceOrder0(:)
  LOGICAL(LGT) :: isIsotropicOrder, isEdgeOrder, isFaceOrder, &
    & isCellOrder, isAnisotropicOrder
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_AnisoOrder()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (.NOT. isQuadrangle(elemType)  &
    & .AND. .NOT. isHexahedron(elemType)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[ARGUMENT ERROR] anisoOrder is allowed '//  &
      & 'for Quadrangle and Hexahedron only')
  END IF

  IF (SIZE(anisoOrder) .NE. nsd) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[ARGUMENT ERROR] The size of anisoOrder should be nsd')
  END IF

  isIsotropicOrder = .FALSE.
  isAnisotropicOrder = .TRUE.
  isEdgeOrder = .FALSE.
  isFaceOrder = .FALSE.
  isCellOrder = .FALSE.

  tEdgeOrder = 0_I4B
  tFaceOrder = 0_I4B
  tCellOrder = 0_I4B

  CALL Reallocate(edgeOrder0, tEdgeOrder)
  CALL Reallocate(faceOrder0, tFaceOrder)

  order0 = -1
  anisoOrder0 = anisoOrder
  cellOrder0 = -1

  ierr = param%Set(key=prefix//"/order", VALUE=order0)
  ierr = param%Set(key=prefix//"/anisoOrder", VALUE=anisoOrder0)
  IF (tEdgeOrder .NE. 0) THEN
    ierr = param%Set(key=prefix//"/edgeOrder", VALUE=edgeOrder0)
  ELSE
    ierr = param%Set(key=prefix//"/edgeOrder", VALUE=[0])
  END IF

  IF (tFaceOrder .NE. 0) THEN
    ierr = param%Set(key=prefix//"/faceOrder", VALUE=faceOrder0)
  ELSE
    ierr = param%Set(key=prefix//"/faceOrder", VALUE=[0])
  END IF
  ierr = param%Set(key=prefix//"/cellOrder", VALUE=cellOrder0)
  ierr = param%Set(key=prefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
  ierr = param%Set( &
    & key=prefix//"/isAnisotropicOrder", &
    & VALUE=isAnisotropicOrder)
  ierr = param%Set(key=prefix//"/isEdgeOrder", VALUE=isEdgeOrder)
  ierr = param%Set(key=prefix//"/isFaceOrder", VALUE=isFaceOrder)
  ierr = param%Set(key=prefix//"/isCellOrder", VALUE=isCellOrder)
  ierr = param%Set(key=prefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  ierr = param%Set(key=prefix//"/tFaceOrder", VALUE=tFaceOrder)
  ierr = param%Set(key=prefix//"/tCellOrder", VALUE=tCellOrder)

  DEALLOCATE (edgeOrder0, faceOrder0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetFEParam_AnisoOrder

!----------------------------------------------------------------------------
!                                                     SetFEParam_Heirarchy2D
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_Heirarchy2D(param, elemType, nsd, edgeOrder, &
  & faceOrder, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:)
  CHARACTER(*), INTENT(IN) :: prefix
  ! internal variables
  INTEGER(I4B) :: tEdgeOrder, tFaceOrder, tCellOrder, order0, &
    &  cellOrder0(3), anisoOrder0(3), ierr, xidim
  INTEGER(I4B), ALLOCATABLE :: edgeOrder0(:), faceOrder0(:)
  LOGICAL(LGT) :: isIsotropicOrder, isEdgeOrder, isFaceOrder, &
    & isCellOrder, isAnisotropicOrder
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_Heirarchy2D()"
  TYPE(String) :: amsg

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  isIsotropicOrder = .FALSE.
  isAnisotropicOrder = .FALSE.
  isEdgeOrder = .TRUE.
  isFaceOrder = .TRUE.
  isCellOrder = .FALSE.

  tEdgeOrder = GetTotalEdges(elemType)
  xidim = XiDimension(elemType)
  tFaceOrder = GetTotalFaces(elemType) * xidim
  tCellOrder = 0_I4B

  IF (.NOT. PRESENT(edgeOrder) .OR. .NOT. PRESENT(faceOrder)) THEN
    amsg = "[ARGUMENT ERROR] For 2D elements, you should specify \n"//  &
    & "one of the entries from following Sets: \n"//  &
    & "[order, anisoOrder, (edgeOrder, faceOrder)]"
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & amsg)
    RETURN
  END IF

  IF (SIZE(edgeOrder) .NE. tEdgeOrder) THEN
    amsg = "[ARGUMENT ERROR] The size of edgeOrder \n"//  &
          & "should be equal to the \n"// &
          & "total number of edges in the element."
    CALL e%raiseError(modName//'::'//myName//' - '//amsg)
    RETURN
  END IF

  IF (isQuadrangle(elemType)) THEN

    IF (SIZE(faceOrder) .NE. xidim) THEN
      amsg = "[ARGUMENT ERROR] In case of a Quadrangle element \n"//  &
          & "the size of faceOrder="//tostring(SIZE(faceOrder))//  &
          & " should be equal to xidim="//tostring(xidim)
      CALL e%raiseError(modName//'::'//myName//' - '//amsg)
      RETURN
    ELSE
      tFaceOrder = xidim
    END IF

  ELSE

    IF (SIZE(faceOrder) .NE. 1) THEN
      amsg = "[ARGUMENT ERROR] In case of a Triangle element \n"//  &
          & "the size of faceOrder="//tostring(SIZE(faceOrder))//  &
          & " should be equal to 1="
      CALL e%raiseError(modName//'::'//myName//' - '// &
        & amsg)
      RETURN
    ELSE
      tFaceOrder = 1_I4B
    END IF

  END IF

  CALL Reallocate(edgeOrder0, tEdgeOrder)
  CALL Reallocate(faceOrder0, tFaceOrder)

  edgeOrder0 = edgeOrder
  faceOrder0(1:tFaceOrder) = faceOrder(1:tFaceOrder)
  order0 = -1
  anisoOrder0 = -1
  cellOrder0 = -1

  ierr = param%Set(key=prefix//"/order", VALUE=order0)
  ierr = param%Set(key=prefix//"/anisoOrder", VALUE=anisoOrder0)

  IF (tEdgeOrder .NE. 0) THEN
    ierr = param%Set(key=prefix//"/edgeOrder", VALUE=edgeOrder0)
  ELSE
    ierr = param%Set(key=prefix//"/edgeOrder", VALUE=[0])
  END IF

  IF (tFaceOrder .NE. 0) THEN
    ierr = param%Set(key=prefix//"/faceOrder", VALUE=faceOrder0)
  ELSE
    ierr = param%Set(key=prefix//"/faceOrder", VALUE=[0])
  END IF

  ierr = param%Set(key=prefix//"/cellOrder", VALUE=cellOrder0)
  ierr = param%Set(key=prefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
  ierr = param%Set(key=prefix//"/isAnisotropicOrder",  &
    & VALUE=isAnisotropicOrder)
  ierr = param%Set(key=prefix//"/isEdgeOrder", VALUE=isEdgeOrder)
  ierr = param%Set(key=prefix//"/isFaceOrder", VALUE=isFaceOrder)
  ierr = param%Set(key=prefix//"/isCellOrder", VALUE=isCellOrder)
  ierr = param%Set(key=prefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  ierr = param%Set(key=prefix//"/tFaceOrder", VALUE=tFaceOrder)
  ierr = param%Set(key=prefix//"/tCellOrder", VALUE=tCellOrder)

  DEALLOCATE (edgeOrder0, faceOrder0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetFEParam_Heirarchy2D

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
INTEGER(I4B) :: ierr, nsd, elemType, order, anisoOrder(3), &
  & cellOrder(3), feType, ipType, dofType(4), transformType, basisType(3), &
  & tEdgeOrder, tFaceOrder, tCellOrder, ii
INTEGER(I4B), ALLOCATABLE :: edgeOrder(:), faceOrder(:)
TYPE(String) :: baseInterpol, baseCont, refElemDomain0
REAL(DFP) :: alpha(3), beta(3), lambda(3)
LOGICAL(LGT) :: isEdgeOrder, isFaceOrder, isCellOrder,  &
  & isIsotropicOrder, isAnisotropicOrder
TYPE(AbstractRefElementPointer_), ALLOCATABLE :: facetElemPtrs(:)
TYPE(ParameterList_), POINTER :: sublist
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

prefix = obj%GetPrefix()
sublist => NULL()
ierr = param%GetSubList(key=prefix, sublist=sublist)

CALL obj%DEALLOCATE()
CALL obj%CheckEssentialParam(sublist)

!! Get sublisteters
ierr = sublist%Get(key=prefix//"/nsd", VALUE=nsd)
ierr = sublist%Get(key=prefix//"/elemType", VALUE=elemType)
CALL GetValue(obj=sublist, key=prefix//"/baseContinuity", VALUE=baseCont)
CALL GetValue( obj=sublist, key=prefix//"/baseInterpolation", VALUE=baseInterpol)

ierr = sublist%Get(key=prefix//"/feType", VALUE=feType)
ierr = sublist%Get(key=prefix//"/ipType", VALUE=ipType)
ierr = sublist%Get(key=prefix//"/dofType", VALUE=dofType)
ierr = sublist%Get(key=prefix//"/transformType", VALUE=transformType)
ierr = sublist%Get(key=prefix//"/basisType", VALUE=basisType)
ierr = sublist%Get(key=prefix//"/alpha", VALUE=alpha)
ierr = sublist%Get(key=prefix//"/beta", VALUE=beta)
ierr = sublist%Get(key=prefix//"/lambda", VALUE=lambda)

CALL GetValue(obj=sublist, key=prefix//"/refElemDomain", VALUE=refElemDomain0)

!! Initiate ReferenceElement
obj%refelem => RefElement_Pointer(elemType)
!! NOTE: RefElement_Pointer is defined in RefElementFactory
CALL obj%refelem%Initiate(nsd=nsd, baseContinuity=baseCont%chars(), &
  & baseInterpolation=baseInterpol%chars())

!! Set parameters

CALL obj%SetParam(&
  & nsd=nsd, &
  & elemType=elemType, &
  & feType=feType, &
  & baseContinuity=baseCont%chars(), &
  & baseInterpolation=baseInterpol%chars(),  &
  & refElemDomain=refElemDomain0%chars(), &
  & transformType=transformType,  &
  & dofType=dofType,  &
  & ipType=ipType,  &
  & basisType=basisType,  &
  & alpha=alpha,  &
  & beta=beta,  &
  & lambda=lambda)

ierr = sublist%Get(key=prefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
ierr = sublist%Get(key=prefix//"/isAnisotropicOrder", VALUE=isAnisotropicOrder)
ierr = sublist%Get(key=prefix//"/isEdgeOrder", VALUE=isEdgeOrder)
ierr = sublist%Get(key=prefix//"/isFaceOrder", VALUE=isFaceOrder)
ierr = sublist%Get(key=prefix//"/isCellOrder", VALUE=isCellOrder)

IF (isIsotropicOrder) THEN
  ierr = sublist%Get(key=prefix//"/order", VALUE=order)
  CALL obj%SetParam(order=order, isIsotropicOrder=isIsotropicOrder)
END IF

IF (isAnisotropicOrder) THEN
  ierr = sublist%Get(key=prefix//"/anisoOrder", VALUE=anisoOrder)
  CALL obj%SetParam(anisoOrder=anisoOrder, &
    & isAnisotropicOrder=isAnisotropicOrder)
END IF

IF (isEdgeOrder) THEN
  ierr = sublist%Get(key=prefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  IF (tEdgeOrder .GT. 0_I4B) THEN
    CALL Reallocate(edgeOrder, tEdgeOrder)
    ierr = sublist%Get(key=prefix//"/edgeOrder", VALUE=edgeOrder)
    CALL obj%SetParam(isEdgeOrder=isEdgeOrder, edgeOrder=edgeOrder,  &
      & tEdgeOrder=tEdgeOrder)
  END IF
END IF

IF (isFaceOrder) THEN
  ierr = sublist%Get(key=prefix//"/tFaceOrder", VALUE=tFaceOrder)
  IF (tFaceOrder .GT. 0_I4B) THEN
    CALL Reallocate(faceOrder, tFaceOrder)
    ierr = sublist%Get(key=prefix//"/faceOrder", VALUE=faceOrder)
    CALL obj%SetParam(isFaceOrder=isFaceOrder, faceOrder=faceOrder,  &
      & tFaceOrder=tFaceOrder)
  END IF
END IF

IF (isCellOrder) THEN
  ierr = sublist%Get(key=prefix//"/tCellOrder", VALUE=tCellOrder)
  ierr = sublist%Get(key=prefix//"/cellOrder", VALUE=cellOrder)
  CALL obj%SetParam( &
    & isCellOrder=isCellOrder, &
    & cellOrder=cellOrder,  &
    & tCellOrder=tCellOrder)
END IF

obj%isInitiated = .TRUE.
CALL obj%refelem%GetParam(refelem=obj%refelem0)
CALL obj%refelem%GetFacetElements(ans=facetElemPtrs)

DO ii = 1, SIZE(facetElemPtrs)
  CALL facetElemPtrs(ii)%ptr%GetParam(refelem=obj%facetElem0(ii))
  CALL facetElemPtrs(ii)%ptr%DEALLOCATE()
  facetElemPtrs(ii)%ptr => NULL()
END DO
DEALLOCATE (facetElemPtrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
INTEGER(I4B) :: ii, elemType
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

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
obj%baseContinuity0 = obj2%baseContinuity0
obj%baseInterpolation0 = obj2%baseInterpolation0
obj%basisType = obj2%basisType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%refElemDomain = obj2%refElemDomain
obj%refelem0 = obj2%refelem0

IF (ALLOCATED(obj2%baseContinuity)) THEN
  ALLOCATE (obj%baseContinuity, source=obj2%baseContinuity)
END IF

IF (ALLOCATED(obj2%baseInterpolation)) THEN
  ALLOCATE (obj%baseInterpolation, source=obj2%baseInterpolation)
END IF

! obj%refelem
IF (ASSOCIATED(obj2%refelem)) THEN
  elemType = obj2%refelem%GetName()
  obj%refelem => RefElement_Pointer(elemType)
  CALL obj%refelem%Copy(obj2%refelem)
END IF

DO ii = 1, SIZE(obj2%facetElem0)
  obj%facetElem0(ii) = obj2%facetElem0(ii)
END DO

IF (ALLOCATED(obj2%coeff)) THEN
  obj%coeff = obj2%coeff
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                     SetFEParam_Heirarchy2D
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_Heirarchy3D(param, elemType, nsd, edgeOrder, &
  &  faceOrder, cellOrder, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
  CHARACTER(*), INTENT(IN) :: prefix

  INTEGER(I4B) :: tEdgeOrder, tFaceOrder, tCellOrder, order0, &
    &  cellOrder0(3), anisoOrder0(3), ierr, xidim
  INTEGER(I4B), ALLOCATABLE :: edgeOrder0(:), faceOrder0(:)
  LOGICAL(LGT) :: isIsotropicOrder, isEdgeOrder, isFaceOrder, &
    & isCellOrder, isAnisotropicOrder
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_Heirarchy2D()"
  TYPE(String) :: amsg

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  isIsotropicOrder = .FALSE.
  isAnisotropicOrder = .FALSE.
  isEdgeOrder = .TRUE.
  isFaceOrder = .TRUE.
  isCellOrder = .TRUE.

  tEdgeOrder = GetTotalEdges(elemType)
  xidim = XiDimension(elemType)
  tFaceOrder = GetTotalFaces(elemType)
  tCellOrder = GetTotalCells(elemType)

  IF (.NOT. PRESENT(edgeOrder) .OR.  &
    & .NOT. PRESENT(faceOrder) .OR.  &
    & .NOT. PRESENT(cellOrder)) THEN
    amsg = "[ARGUMENT ERROR] For 3D elements, you should specify \n"//  &
    & "one of the entries from following Sets: \n"//  &
    & "[order, anisoOrder, (edgeOrder, faceOrder, cellOrder)]"
    CALL e%raiseError(modName//'::'//myName//' - '//amsg)
    RETURN
  END IF

  IF (SIZE(edgeOrder) .NE. tEdgeOrder) THEN
    amsg = "[ARGUMENT ERROR] The size of edgeOrder \n"//  &
          & "should be equal to the \n"// &
          & "total number of edges in the element."
    CALL e%raiseError(modName//'::'//myName//' - '//amsg)
    RETURN
  END IF

  IF (isHexahedron(elemType)) THEN

    tFaceOrder = tFaceOrder * xidim
    IF (SIZE(faceOrder) .NE. tFaceOrder) THEN
      amsg = "[ARGUMENT ERROR] In case of a Hexahedron element \n"//  &
          & "the size of faceOrder="//tostring(SIZE(cellOrder))//  &
          & " should be equal to "//tostring(tFaceOrder)
      CALL e%raiseError(modName//'::'//myName//' - '//amsg)
      RETURN
    END IF

    tCellOrder = tCellOrder * xidim
    IF (SIZE(cellOrder) .NE. tCellOrder) THEN
      amsg = "[ARGUMENT ERROR] In case of a Hexahedron element \n"//  &
          & "the size of cellOrder="//tostring(SIZE(cellOrder))//  &
          & " should be equal to "//tostring(tCellOrder)
      CALL e%raiseError(modName//'::'//myName//' - '//amsg)
      RETURN
    END IF

  END IF

  IF (isTetrahedron(elemType)) THEN

    tFaceOrder = tFaceOrder
    IF (SIZE(faceOrder) .NE. tFaceOrder) THEN
      amsg = "[ARGUMENT ERROR] In case of a Tetrahedron element \n"//  &
          & "the size of faceOrder="//tostring(SIZE(cellOrder))//  &
          & " should be equal to "//tostring(tFaceOrder)
      CALL e%raiseError(modName//'::'//myName//' - '//amsg)
      RETURN
    END IF

    ! tCellOrder = tCellOrder
    IF (SIZE(cellOrder) .NE. tCellOrder) THEN
      amsg = "[ARGUMENT ERROR] In case of a Tetrahedron element \n"//  &
          & "the size of cellOrder="//tostring(SIZE(cellOrder))//  &
          & " should be equal to "//tostring(tCellOrder)
      CALL e%raiseError(modName//'::'//myName//' - '//amsg)
      RETURN
    END IF

  END IF

  IF (isPrism(elemType) .OR. isPyramid(elemType)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[WORK IN PROGRESS] Currently Prism and Pyramid elements  &
      & are not supported')
    RETURN
  END IF

  CALL Reallocate(edgeOrder0, tEdgeOrder)
  CALL Reallocate(faceOrder0, tFaceOrder)

  order0 = -1
  anisoOrder0 = -1
  edgeOrder0 = edgeOrder
  faceOrder0(1:tFaceOrder) = faceOrder(1:tFaceOrder)
  cellOrder0(1:tCellOrder) = cellOrder(1:tCellOrder)

  ierr = param%Set(key=prefix//"/order", VALUE=order0)
  ierr = param%Set(key=prefix//"/anisoOrder", VALUE=anisoOrder0)
  ierr = param%Set(key=prefix//"/edgeOrder", VALUE=edgeOrder0)
  ierr = param%Set(key=prefix//"/faceOrder", VALUE=faceOrder0)
  ierr = param%Set(key=prefix//"/cellOrder", VALUE=cellOrder0)
  ierr = param%Set(key=prefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
  ierr = param%Set( &
    & key=prefix//"/isAnisotropicOrder", &
    & VALUE=isAnisotropicOrder)
  ierr = param%Set(key=prefix//"/isEdgeOrder", VALUE=isEdgeOrder)
  ierr = param%Set(key=prefix//"/isFaceOrder", VALUE=isFaceOrder)
  ierr = param%Set(key=prefix//"/isCellOrder", VALUE=isCellOrder)
  ierr = param%Set(key=prefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  ierr = param%Set(key=prefix//"/tFaceOrder", VALUE=tFaceOrder)
  ierr = param%Set(key=prefix//"/tCellOrder", VALUE=tCellOrder)

  DEALLOCATE (edgeOrder0, faceOrder0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetFEParam_Heirarchy3D

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ii
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO ii = 1, SIZE(obj%facetElem0)
  CALL DEALLOCATE (obj%facetElem0(ii))
END DO

CALL DEALLOCATE (obj%refelem0)
IF (ALLOCATED(obj%coeff)) DEALLOCATE (obj%coeff)
obj%firstCall = .TRUE.
IF (ASSOCIATED(obj%refelem)) THEN
  CALL obj%refelem%DEALLOCATE()
  DEALLOCATE (obj%refelem)
  obj%refelem => NULL()
END IF
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
obj%baseContinuity0 = ""
obj%baseInterpolation0 = ""
obj%basisType = 0
obj%alpha = 0.0
obj%beta = 0.0
obj%lambda = 0.0
obj%refElemDomain = ""
IF (ALLOCATED(obj%baseContinuity)) DEALLOCATE (obj%baseContinuity)
IF (ALLOCATED(obj%baseInterpolation)) DEALLOCATE (obj%baseInterpolation)
obj%isInitiated = .FALSE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
CHARACTER(*), PARAMETER :: myName = "Deallocate_Ptr_Vector()"
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

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
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
