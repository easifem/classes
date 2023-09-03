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

SUBMODULE(AbstractFE_Class) Methods
USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
USE FPL_Method, ONLY: GetValue
USE RefElementFactory, ONLY: RefElement_Pointer
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "fe_checkEssentialParam"
INTEGER(I4B), PARAMETER :: jj = 26
TYPE(String) :: necessary(jj)
INTEGER(I4B) :: ii

necessary(1) = myprefix//"/nsd"
necessary(2) = myprefix//"/order"
necessary(3) = myprefix//"/anisoOrder"
necessary(4) = myprefix//"/tEdgeOrder"
necessary(5) = myprefix//"/edgeOrder"
necessary(6) = myprefix//"/tFaceOrder"
necessary(7) = myprefix//"/faceOrder"
necessary(8) = myprefix//"/cellOrder"
necessary(9) = myprefix//"/feType"
necessary(10) = myprefix//"/elemType"
necessary(11) = myprefix//"/ipType"
necessary(12) = myprefix//"/dofType"
necessary(13) = myprefix//"/transformType"
necessary(14) = myprefix//"/refElemDomain"
necessary(15) = myprefix//"/baseContinuity"
necessary(16) = myprefix//"/baseInterpol"
necessary(17) = myprefix//"/isIsotropicOrder"
necessary(18) = myprefix//"/isAnisotropicOrder"
necessary(19) = myprefix//"/isEdgeOrder"
necessary(20) = myprefix//"/isFaceOrder"
necessary(21) = myprefix//"/isCellOrder"
necessary(22) = myprefix//"/tCellOrder"
necessary(23) = myprefix//"/basisType"
necessary(24) = myprefix//"/alpha"
necessary(25) = myprefix//"/beta"
necessary(26) = myprefix//"/lambda"

DO ii = 1, jj
  IF (.NOT. param%isPresent(key=necessary(ii)%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & necessary(ii)//' should be present in param')
  END IF
END DO
END PROCEDURE fe_checkEssentialParam

!----------------------------------------------------------------------------
!                                                       SetAbstractFEParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractFEParam
INTEGER(I4B) :: ierr, ii, ipType0
TYPE(String) :: astr
CHARACTER(*), PARAMETER :: myName = "SetAbstractFEParam()"

ierr = param%set(key=myprefix//"/nsd", VALUE=nsd)
ierr = param%set(key=myprefix//"/elemType", VALUE=elemType)
ierr = param%set(key=myprefix//"/baseContinuity", VALUE=baseContinuity)
ierr = param%set(key=myprefix//"/baseInterpol", VALUE=baseInterpol)

! TODO finite element type
CALL e%raiseWarning(modName//'::'//myName//' - '// &
  & '[BUG] feType, dofType, transformType are not handled properly.')

ierr = param%set(key=myprefix//"/feType", VALUE=Scalar)
ierr = param%set(key=myprefix//"/dofType", VALUE=DEFAULT_DOF_TYPE)
ierr = param%set(key=myprefix//"/transformType", VALUE=DEFAULT_TRANSFORM_TYPE)

astr = UpperCase(baseInterpol)
IF (astr%chars() .EQ. "LAGRANGE" .OR.  &
  & astr%chars() .EQ. "LAGRANGEPOLYNOMIAL") THEN
  IF (.NOT. PRESENT(ipType)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[ARGUMENT ERROR] In case of LAGRANGE polynomials &
      & ipType should be present.')
  END IF
END IF
ipType0 = input(default=Equidistance, option=ipType)
ierr = param%set(key=myprefix//"/ipType", VALUE=ipType)

astr = RefElemDomain( &
  & baseInterpol=baseInterpol, &
  & baseContinuity=baseContinuity, &
  & elemType=elemType)
ierr = param%set(key=myprefix//"/refElemDomain", VALUE=astr%chars())

CALL SetFEPram_BasisType( &
  & param=param, &
  & elemType=elemType, &
  & nsd=nsd, &
  & baseContinuity0=UpperCase(baseContinuity),  &
  & baseInterpol0=UpperCase(baseInterpol),  &
  & basisType=basisType,  &
  & alpha=alpha,  &
  & beta=beta,  &
  & lambda=lambda)

IF (PRESENT(order)) THEN
  CALL SetFEPram_Order(param, order, elemType)
  RETURN
ELSE
  IF (nsd .EQ. 1_I4B) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[ARGUMENT ERROR] For 1D elements Order must be present.')
    RETURN
  END IF
END IF

IF (PRESENT(anisoOrder)) THEN
  CALL SetFEPram_AnisoOrder(param, anisoOrder, elemType, nsd)
  RETURN
END IF

SELECT CASE (nsd)
CASE (2)
  CALL SetFEPram_Heirarchy2D(param, elemType, nsd, edgeOrder, faceOrder)
CASE (3)
  CALL SetFEPram_Heirarchy3D(param, elemType, nsd, edgeOrder, &
    &  faceOrder, cellOrder)
END SELECT

END PROCEDURE SetAbstractFEParam

!----------------------------------------------------------------------------
!                                                       SetFEPram_BasisType
!----------------------------------------------------------------------------

SUBROUTINE SetFEPram_BasisType( &
  & param, &
  & elemType, &
  & nsd, &
  & baseContinuity0, &
  & baseInterpol0, &
  & basisType, &
  & alpha, &
  & beta, lambda)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpol0
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)

  CHARACTER(*), PARAMETER :: myName = "SetFEPram_AnisoOrder()"
  INTEGER(I4B) :: xidim, basisType0(3), ii, ierr
  REAL(DFP) :: alpha0(3), beta0(3), lambda0(3)

  alpha0 = 0.0_DFP
  beta0 = 0.0_DFP
  lambda0 = 0.0_DFP

  SELECT CASE (elemType)
  CASE (Line)
    IF (baseInterpol0 .EQ. "LAGRANGE" .OR.  &
      & baseInterpol0 .EQ. "LAGRANGEPOLYNOMIAL") THEN
      IF (.NOT. PRESENT(basisType)) THEN
        basisType0 = Monomial
      ELSE
        basisType0 = basisType(1)
      END IF
    END IF

    IF (baseInterpol0 .EQ. "ORTHOGONAL" .OR.  &
      & baseInterpol0 .EQ. "ORTHOGONALPOLYNOMIAL") THEN
      IF (.NOT. PRESENT(basisType)) THEN
        basisType0 = Legendre
      ELSE
        basisType0 = basisType(1)
      END IF

      IF (basisType0(1) .EQ. Jacobi) THEN
        IF (PRESENT(alpha)) THEN
          alpha0 = alpha(1)
        ELSE
          alpha0 = 0.0_DFP
        END IF

        IF (PRESENT(beta)) THEN
          beta0 = beta(1)
        ELSE
          beta0 = 0.0_DFP
        END IF
      END IF

      IF (basisType0(1) .EQ. Ultraspherical) THEN
        IF (PRESENT(lambda)) THEN
          lambda0 = lambda(1)
        ELSE
          lambda0 = 0.5_DFP
        END IF
      END IF
    END IF

  CASE (Triangle, Tetrahedron, Prism, Pyramid)
    IF (baseInterpol0 .EQ. "LAGRANGE" .OR.  &
      & baseInterpol0 .EQ. "LAGRANGEPOLYNOMIAL") THEN
      IF (.NOT. PRESENT(basisType)) THEN
        basisType0 = Monomial
      ELSE
        basisType0 = basisType(1)
      END IF
    END IF

  CASE (Quadrangle, Hexahedron)
    xidim = XiDimension(elemType)

    IF (baseInterpol0 .EQ. "LAGRANGE" .OR.  &
      & baseInterpol0 .EQ. "LAGRANGEPOLYNOMIAL") THEN
      IF (.NOT. PRESENT(basisType)) THEN
        basisType0(1:xidim) = Monomial * ones(xidim, 1_I4B)
      ELSE
        IF (SIZE(basisType) .EQ. 1_I4B) THEN
          basisType0 = basisType(1)
        ELSE
          basisType0(1:xidim) = basisType(1:xidim)
        END IF
      END IF
    END IF

    IF (baseInterpol0 .EQ. "ORTHOGONAL" .OR.  &
      & baseInterpol0 .EQ. "ORTHOGONALPOLYNOMIAL") THEN

      IF (.NOT. PRESENT(basisType)) THEN
        basisType0(1:xidim) = Legendre * ones(xidim, 1_I4B)
      ELSE
        IF (SIZE(basisType) .EQ. 1_I4B) THEN
          basisType0 = basisType(1)
        ELSE
          basisType0(1:xidim) = basisType(1:xidim)
        END IF
      END IF

      DO ii = 1, xidim

        IF (basisType0(ii) .EQ. Jacobi) THEN
          IF (PRESENT(alpha)) THEN
            IF (SIZE(alpha) .EQ. xidim) THEN
              alpha0(ii) = alpha(ii)
            ELSE
              alpha0(ii) = alpha(1)
            END IF
          END IF

          IF (PRESENT(beta)) THEN
            IF (SIZE(beta) .EQ. xidim) THEN
              beta0(ii) = beta(ii)
            ELSE
              beta0(ii) = beta(1)
            END IF
          END IF
        END IF

        IF (basisType0(ii) .EQ. Ultraspherical) THEN
          IF (PRESENT(lambda)) THEN
            IF (SIZE(lambda) .EQ. xidim) THEN
              lambda0(ii) = lambda(ii)
            ELSE
              lambda0(ii) = lambda(1)
            END IF
          END IF
        END IF

      END DO
    END IF

  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & '[NO CASE FOUND] No case found for given element type')
  END SELECT

  ierr = param%set(key=myprefix//"/alpha", VALUE=alpha0)
  ierr = param%set(key=myprefix//"/beta", VALUE=beta0)
  ierr = param%set(key=myprefix//"/lambda", VALUE=lambda0)
  ierr = param%set(key=myprefix//"/basisType", VALUE=basisType0)

END SUBROUTINE SetFEPram_BasisType

!----------------------------------------------------------------------------
!                                                          SetFEPram_Order
!----------------------------------------------------------------------------

SUBROUTINE SetFEPram_Order(param, order, elemType)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B), INTENT(IN) :: elemType

  INTEGER(I4B) :: tEdgeOrder, tFaceOrder, tCellOrder, order0, &
    &  cellOrder0(3), anisoOrder0(3), ierr, ii
  INTEGER(I4B), ALLOCATABLE :: edgeOrder0(:), faceOrder0(:)
  LOGICAL(LGT) :: isIsotropicOrder, isEdgeOrder, isFaceOrder, &
    & isCellOrder, isAnisotropicOrder

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
  DO ii = 1, SIZE(edgeOrder0)
    edgeOrder0(ii) = -1
  END DO

  DO ii = 1, SIZE(faceOrder0)
    faceOrder0(ii) = -1
  END DO

  isIsotropicOrder = .TRUE.
  order0 = order
  ierr = param%set(key=myprefix//"/order", VALUE=order0)
  ierr = param%set(key=myprefix//"/anisoOrder", VALUE=anisoOrder0)
  ierr = param%set(key=myprefix//"/edgeOrder", VALUE=edgeOrder0)
  ierr = param%set(key=myprefix//"/faceOrder", VALUE=faceOrder0)
  ierr = param%set(key=myprefix//"/cellOrder", VALUE=cellOrder0)
  ierr = param%set(key=myprefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
  ierr = param%set( &
    & key=myprefix//"/isAnisotropicOrder", &
    & VALUE=isAnisotropicOrder)
  ierr = param%set(key=myprefix//"/isEdgeOrder", VALUE=isEdgeOrder)
  ierr = param%set(key=myprefix//"/isFaceOrder", VALUE=isFaceOrder)
  ierr = param%set(key=myprefix//"/isCellOrder", VALUE=isCellOrder)
  ierr = param%set(key=myprefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  ierr = param%set(key=myprefix//"/tFaceOrder", VALUE=tFaceOrder)
  ierr = param%set(key=myprefix//"/tCellOrder", VALUE=tCellOrder)

  DEALLOCATE (edgeOrder0, faceOrder0)

END SUBROUTINE SetFEPram_Order

!----------------------------------------------------------------------------
!                                                       SetFEPram_AnisoOrder
!----------------------------------------------------------------------------

SUBROUTINE SetFEPram_AnisoOrder(param, anisoOrder, elemType, nsd)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: anisoOrder(3)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd

  INTEGER(I4B) :: tEdgeOrder, tFaceOrder, tCellOrder, order0, &
    &  cellOrder0(3), anisoOrder0(3), ierr, ii
  INTEGER(I4B), ALLOCATABLE :: edgeOrder0(:), faceOrder0(:)
  LOGICAL(LGT) :: isIsotropicOrder, isEdgeOrder, isFaceOrder, &
    & isCellOrder, isAnisotropicOrder
  CHARACTER(*), PARAMETER :: myName = "SetFEPram_AnisoOrder()"

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

  ierr = param%set(key=myprefix//"/order", VALUE=order0)
  ierr = param%set(key=myprefix//"/anisoOrder", VALUE=anisoOrder0)
  ierr = param%set(key=myprefix//"/edgeOrder", VALUE=edgeOrder0)
  ierr = param%set(key=myprefix//"/faceOrder", VALUE=faceOrder0)
  ierr = param%set(key=myprefix//"/cellOrder", VALUE=cellOrder0)
  ierr = param%set(key=myprefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
  ierr = param%set( &
    & key=myprefix//"/isAnisotropicOrder", &
    & VALUE=isAnisotropicOrder)
  ierr = param%set(key=myprefix//"/isEdgeOrder", VALUE=isEdgeOrder)
  ierr = param%set(key=myprefix//"/isFaceOrder", VALUE=isFaceOrder)
  ierr = param%set(key=myprefix//"/isCellOrder", VALUE=isCellOrder)
  ierr = param%set(key=myprefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  ierr = param%set(key=myprefix//"/tFaceOrder", VALUE=tFaceOrder)
  ierr = param%set(key=myprefix//"/tCellOrder", VALUE=tCellOrder)

  DEALLOCATE (edgeOrder0, faceOrder0)

END SUBROUTINE SetFEPram_AnisoOrder

!----------------------------------------------------------------------------
!                                                     SetFEPram_Heirarchy2D
!----------------------------------------------------------------------------

SUBROUTINE SetFEPram_Heirarchy2D(param, elemType, nsd, edgeOrder, faceOrder)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:)

  INTEGER(I4B) :: tEdgeOrder, tFaceOrder, tCellOrder, order0, &
    &  cellOrder0(3), anisoOrder0(3), ierr, ii, xidim
  INTEGER(I4B), ALLOCATABLE :: edgeOrder0(:), faceOrder0(:)
  LOGICAL(LGT) :: isIsotropicOrder, isEdgeOrder, isFaceOrder, &
    & isCellOrder, isAnisotropicOrder
  CHARACTER(*), PARAMETER :: myName = "SetFEPram_Heirarchy2D()"
  TYPE(String) :: amsg

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
    & "one of the entries from following sets: \n"//  &
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

  ierr = param%set(key=myprefix//"/order", VALUE=order0)
  ierr = param%set(key=myprefix//"/anisoOrder", VALUE=anisoOrder0)
  ierr = param%set(key=myprefix//"/edgeOrder", VALUE=edgeOrder0)
  ierr = param%set(key=myprefix//"/faceOrder", VALUE=faceOrder0)
  ierr = param%set(key=myprefix//"/cellOrder", VALUE=cellOrder0)
  ierr = param%set(key=myprefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
  ierr = param%set( &
    & key=myprefix//"/isAnisotropicOrder", &
    & VALUE=isAnisotropicOrder)
  ierr = param%set(key=myprefix//"/isEdgeOrder", VALUE=isEdgeOrder)
  ierr = param%set(key=myprefix//"/isFaceOrder", VALUE=isFaceOrder)
  ierr = param%set(key=myprefix//"/isCellOrder", VALUE=isCellOrder)
  ierr = param%set(key=myprefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  ierr = param%set(key=myprefix//"/tFaceOrder", VALUE=tFaceOrder)
  ierr = param%set(key=myprefix//"/tCellOrder", VALUE=tCellOrder)

  DEALLOCATE (edgeOrder0, faceOrder0)
END SUBROUTINE SetFEPram_Heirarchy2D

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Initiate
CHARACTER(*), PARAMETER :: myName = "fe_Initiate"
INTEGER(I4B) :: ierr, nsd, elemType, order, anisoOrder(3), &
& cellOrder(3), feType, ipType, dofType(4), transformType, basisType(3), &
& tEdgeOrder, tFaceOrder, tCellOrder
INTEGER(I4B), ALLOCATABLE :: edgeOrder(:), faceOrder(:)
TYPE(String) :: baseInterpol, baseCont, refElemDomain0
REAL(DFP) :: alpha(3), beta(3), lambda(3)

LOGICAL(LGT) :: isEdgeOrder, isFaceOrder, isCellOrder,  &
& isIsotropicOrder, isAnisotropicOrder

CALL obj%DEALLOCATE()
CALL obj%checkEssentialParam(param)

!! Get parameters
ierr = param%get(key=myprefix//"/nsd", VALUE=nsd)
ierr = param%get(key=myprefix//"/elemType", VALUE=elemType)
CALL GetValue( &
  & obj=param, &
  & key=myprefix//"/baseContinuity", &
  & VALUE=baseCont)

CALL GetValue( &
  & obj=param, &
  & key=myprefix//"/baseInterpol", &
  & VALUE=baseInterpol)

ierr = param%get(key=myprefix//"/feType", VALUE=feType)
ierr = param%get(key=myprefix//"/ipType", VALUE=ipType)
ierr = param%get(key=myprefix//"/dofType", VALUE=dofType)
ierr = param%get(key=myprefix//"/transformType", VALUE=transformType)
ierr = param%get(key=myprefix//"/basisType", VALUE=basisType)
ierr = param%get(key=myprefix//"/alpha", VALUE=alpha)
ierr = param%get(key=myprefix//"/beta", VALUE=beta)
ierr = param%get(key=myprefix//"/lambda", VALUE=lambda)

CALL GetValue( &
  & obj=param, &
  & key=myprefix//"/refElemDomain", &
  & VALUE=refElemDomain0)

!! Initiate ReferenceElement
obj%refelem => RefElement_Pointer(elemType)
CALL obj%refelem%Initiate( &
  & nsd=nsd, &
  & baseContinuity=baseCont%chars(), &
  & baseInterpol=baseInterpol%chars())

!! Set parameters

CALL obj%SetParam(&
  & nsd=nsd, &
  & elemType=elemType, &
  & feType=feType, &
  & baseContinuity=baseCont%chars(), &
  & baseInterpol=baseInterpol%chars(),  &
  & refElemDomain=refElemDomain0%chars(), &
  & transformType=transformType,  &
  & dofType=dofType,  &
  & ipType=ipType,  &
  & basisType=basisType,  &
  & alpha=alpha,  &
  & beta=beta,  &
  & lambda=lambda)

ierr = param%get(key=myprefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
ierr = param%get(key=myprefix//"/isAnisotropicOrder", VALUE=isAnisotropicOrder)
ierr = param%get(key=myprefix//"/isEdgeOrder", VALUE=isEdgeOrder)
ierr = param%get(key=myprefix//"/isFaceOrder", VALUE=isFaceOrder)
ierr = param%get(key=myprefix//"/isCellOrder", VALUE=isCellOrder)

IF (isIsotropicOrder) THEN
  ierr = param%get(key=myprefix//"/order", VALUE=order)
  CALL obj%SetParam(order=order, isIsotropicOrder=isIsotropicOrder)
END IF

IF (isAnisotropicOrder) THEN
  ierr = param%get(key=myprefix//"/anisoOrder", VALUE=anisoOrder)
  CALL obj%SetParam( &
    & anisoOrder=anisoOrder, &
    & isAnisotropicOrder=isAnisotropicOrder)
END IF

IF (isEdgeOrder) THEN
  ierr = param%get(key=myprefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  IF (tEdgeOrder .GT. 0_I4B) THEN
    CALL Reallocate(edgeOrder, tEdgeOrder)
    ierr = param%get(key=myprefix//"/edgeOrder", VALUE=edgeOrder)
    CALL obj%SetParam( &
      & isEdgeOrder=isEdgeOrder, &
      & edgeOrder=edgeOrder,  &
      & tEdgeOrder=tEdgeOrder)
  END IF
END IF

IF (isFaceOrder) THEN
  ierr = param%get(key=myprefix//"/tFaceOrder", VALUE=tFaceOrder)
  IF (tFaceOrder .GT. 0_I4B) THEN
    CALL Reallocate(faceOrder, tFaceOrder)
    ierr = param%get(key=myprefix//"/faceOrder", VALUE=faceOrder)
    CALL obj%SetParam( &
      & isFaceOrder=isFaceOrder, &
      & faceOrder=faceOrder,  &
      & tFaceOrder=tFaceOrder)
  END IF
END IF

IF (isCellOrder) THEN
  ierr = param%get(key=myprefix//"/tCellOrder", VALUE=tCellOrder)
  ierr = param%get(key=myprefix//"/cellOrder", VALUE=cellOrder)
  CALL obj%SetParam( &
    & isCellOrder=isCellOrder, &
    & cellOrder=cellOrder,  &
    & tCellOrder=tCellOrder)
END IF
END PROCEDURE fe_Initiate

!----------------------------------------------------------------------------
!                                                     SetFEPram_Heirarchy2D
!----------------------------------------------------------------------------

SUBROUTINE SetFEPram_Heirarchy3D(param, elemType, nsd, edgeOrder, &
  &  faceOrder, cellOrder)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)

  INTEGER(I4B) :: tEdgeOrder, tFaceOrder, tCellOrder, order0, &
    &  cellOrder0(3), anisoOrder0(3), ierr, ii, xidim
  INTEGER(I4B), ALLOCATABLE :: edgeOrder0(:), faceOrder0(:)
  LOGICAL(LGT) :: isIsotropicOrder, isEdgeOrder, isFaceOrder, &
    & isCellOrder, isAnisotropicOrder
  CHARACTER(*), PARAMETER :: myName = "SetFEPram_Heirarchy2D()"
  TYPE(String) :: amsg

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
    & "one of the entries from following sets: \n"//  &
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

  ierr = param%set(key=myprefix//"/order", VALUE=order0)
  ierr = param%set(key=myprefix//"/anisoOrder", VALUE=anisoOrder0)
  ierr = param%set(key=myprefix//"/edgeOrder", VALUE=edgeOrder0)
  ierr = param%set(key=myprefix//"/faceOrder", VALUE=faceOrder0)
  ierr = param%set(key=myprefix//"/cellOrder", VALUE=cellOrder0)
  ierr = param%set(key=myprefix//"/isIsotropicOrder", VALUE=isIsotropicOrder)
  ierr = param%set( &
    & key=myprefix//"/isAnisotropicOrder", &
    & VALUE=isAnisotropicOrder)
  ierr = param%set(key=myprefix//"/isEdgeOrder", VALUE=isEdgeOrder)
  ierr = param%set(key=myprefix//"/isFaceOrder", VALUE=isFaceOrder)
  ierr = param%set(key=myprefix//"/isCellOrder", VALUE=isCellOrder)
  ierr = param%set(key=myprefix//"/tEdgeOrder", VALUE=tEdgeOrder)
  ierr = param%set(key=myprefix//"/tFaceOrder", VALUE=tFaceOrder)
  ierr = param%set(key=myprefix//"/tCellOrder", VALUE=tCellOrder)

  DEALLOCATE (edgeOrder0, faceOrder0)
END SUBROUTINE SetFEPram_Heirarchy3D

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Deallocate
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
obj%baseInterpol0 = ""
obj%basisType = 0
obj%alpha = 0.0
obj%beta = 0.0
obj%lambda = 0.0
obj%refElemDomain = ""
IF (ALLOCATED(obj%baseContinuity)) DEALLOCATE (obj%baseContinuity)
IF (ALLOCATED(obj%baseInterpol)) DEALLOCATE (obj%baseInterpol)
END PROCEDURE fe_Deallocate

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Display
IF (ASSOCIATED(obj%refelem)) THEN
  CALL obj%refelem%Display( &
    & msg="ReferenceElement: ",  &
    & unitno=unitno, &
    & notFull=notFull)
END IF

CALL Display(obj%nsd, msg="nsd: ", unitno=unitno)
CALL Display(obj%feType, msg="feType: ", unitno=unitno)
CALL Display(obj%elemType, msg="elemType: ", unitno=unitno)
CALL Display(obj%ipType, msg="ipType: ", unitno=unitno)
CALL Display(obj%basisType, msg="basisType: ", unitno=unitno)
CALL Display(obj%alpha, msg="alpha: ", unitno=unitno)
CALL Display(obj%beta, msg="beta: ", unitno=unitno)
CALL Display(obj%lambda, msg="lambda: ", unitno=unitno)
CALL Display(obj%dofType, msg="dofType: ", unitno=unitno)
CALL Display(obj%transformType, msg="transformType: ", unitno=unitno)
CALL Display(obj%baseContinuity0, msg="baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpol0, msg="baseInterpol: ", unitno=unitno)
CALL Display(obj%refElemDomain, msg="refElemDomain: ", unitno=unitno)

IF (obj%isIsotropicOrder) THEN
  CALL Display("isIsotropicOrder: TRUE", unitno=unitno)
  CALL Display(obj%order, msg="order: ", unitno=unitno)
END IF

IF (obj%isAnisotropicOrder) THEN
  CALL Display("isAnisotropicOrder: TRUE", unitno=unitno)
  CALL Display(obj%anisoOrder, msg="anisoOrder: ", unitno=unitno)
END IF

IF (obj%isEdgeOrder) THEN
  CALL Display("isEdgeOrder: TRUE", unitno=unitno)
  IF (obj%tEdgeOrder .GT. 0_I4B) THEN
    CALL Display( &
      & obj%edgeOrder(:obj%tEdgeOrder), &
      & msg="edgeOrder: ", &
      & unitno=unitno)
  END IF
END IF

IF (obj%isFaceOrder) THEN
  CALL Display("isFaceOrder: TRUE", unitno=unitno)
  IF (obj%tFaceOrder .GT. 0_I4B) THEN
    CALL Display( &
      & obj%faceOrder(:obj%tFaceOrder), &
      & msg="faceOrder: ", &
      & unitno=unitno)
  END IF
END IF

IF (obj%isCellOrder) THEN
  CALL Display("isCellOrder: TRUE", unitno=unitno)
  IF (obj%tCellOrder .GT. 0_I4B) THEN
    CALL Display( &
      & obj%cellOrder(:obj%tCellOrder), &
      & msg="cellOrder: ", &
      & unitno=unitno)
  END IF
END IF

END PROCEDURE fe_Display

!----------------------------------------------------------------------------
!                                                                MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_MdEncode
CHARACTER(*), PARAMETER :: myName = "fe_MdEncode"
INTEGER(I4B), PARAMETER :: jj = 21
TYPE(String) :: rowTitle(jj), colTitle(1), astr(jj)

colTitle(1) = ""
rowTitle(1) = "**nsd**"; astr(1) = tostring(obj%nsd)
rowTitle(2) = "**feType**"; astr(2) = tostring(obj%feType)
rowTitle(3) = "**elemType**"; astr(3) = ElementName(obj%elemType)
rowTitle(4) = "**ipType**"; astr(4) = tostring(obj%ipType)
rowTitle(5) = "**basisType**"; astr(5) = tostring(obj%basisType)
rowTitle(6) = "**alpha**"; astr(6) = tostring(obj%alpha)
rowTitle(7) = "**beta**"; astr(7) = tostring(obj%beta)
rowTitle(8) = "**lambda**"; astr(8) = tostring(obj%lambda)
rowTitle(9) = "**dofType**"; astr(9) = tostring(obj%dofType)
rowTitle(10) = "**transformType**"; astr(10) = tostring(obj%transformType)
rowTitle(11) = "**baseContinuity**"; astr(11) = obj%baseContinuity0%chars()
rowTitle(12) = "**baseInterpolion**"; astr(12) = obj%baseInterpol0%chars()
rowTitle(13) = "**refElemDomain**"; astr(13) = obj%refElemDomain%chars()
rowTitle(14) = "**isIsotropicOrder**"; astr(14) = tostring(obj%isIsotropicOrder)
rowTitle(15) = "**isAnisotropicOrder**"; astr(15) = tostring(obj%isAnisotropicOrder)
rowTitle(16) = "**isEdgeOrder**"; astr(16) = tostring(obj%isEdgeOrder)
rowTitle(17) = "**isFaceOrder**"; astr(17) = tostring(obj%isFaceOrder)
rowTitle(18) = "**isCellOrder**"; astr(18) = tostring(obj%isCellOrder)
IF (obj%isEdgeOrder) THEN
  rowTitle(19) = "**edgeOrder**"; astr(19) = tostring(obj%edgeOrder)
ELSE
  rowTitle(19) = "**edgeOrder**"; astr(19) = " "
END IF

IF (obj%isFaceOrder) THEN
  rowTitle(20) = "**faceOrder**"; astr(20) = tostring(obj%faceOrder)
ELSE
  rowTitle(20) = "**faceOrder**"; astr(20) = " "
END IF

IF (obj%iscellOrder) THEN
  rowTitle(21) = "**cellOrder**"; astr(21) = tostring(obj%cellOrder)
ELSE
  rowTitle(21) = "**cellOrder**"; astr(21) = " "
END IF

ans = MdEncode( &
  & val=astr(1:21), &
  & rh=rowTitle(1:21), &
  & ch=colTitle)//char_lf//"**Reference Element**"// &
  & char_lf//char_lf//obj%refelem%MdEncode()

END PROCEDURE fe_MdEncode

!----------------------------------------------------------------------------
!                                                                MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_ReactEncode
CHARACTER(*), PARAMETER :: myName = "fe_ReactEncode"
INTEGER(I4B), PARAMETER :: jj = 21
TYPE(String) :: rowTitle(jj), colTitle(1), astr(jj)

colTitle(1) = ""
rowTitle(1) = "**nsd**"
astr(1) = tostring(obj%nsd)

rowTitle(2) = "**feType**"
astr(2) = tostring(obj%feType)

rowTitle(3) = "**elemType**"
astr(3) = ElementName(obj%elemType)

rowTitle(4) = "**ipType**"
astr(4) = tostring(obj%ipType)

rowTitle(5) = "**basisType**"
astr(5) = tostring(obj%basisType)

rowTitle(6) = "**alpha**"
astr(6) = tostring(obj%alpha)

rowTitle(7) = "**beta**"
astr(7) = tostring(obj%beta)

rowTitle(8) = "**lambda**"
astr(8) = tostring(obj%lambda)

rowTitle(9) = "**dofType**"
astr(9) = tostring(obj%dofType)

rowTitle(10) = "**transformType**"
astr(10) = tostring(obj%transformType)

rowTitle(11) = "**baseContinuity**"
astr(11) = obj%baseContinuity0%chars()

rowTitle(12) = "**baseInterpolion**"
astr(12) = obj%baseInterpol0%chars()

rowTitle(13) = "**refElemDomain**"
astr(13) = obj%refElemDomain%chars()

rowTitle(14) = "**isIsotropicOrder**"
astr(14) = tostring(obj%isIsotropicOrder)

rowTitle(15) = "**isAnisotropicOrder**"
astr(15) = tostring(obj%isAnisotropicOrder)

rowTitle(16) = "**isEdgeOrder**"
astr(16) = tostring(obj%isEdgeOrder)

rowTitle(17) = "**isFaceOrder**"
astr(17) = tostring(obj%isFaceOrder)

rowTitle(18) = "**isCellOrder**"
astr(18) = tostring(obj%isCellOrder)

IF (obj%isEdgeOrder) THEN
  rowTitle(19) = "**edgeOrder**"
  astr(19) = tostring(obj%edgeOrder)
ELSE
  rowTitle(19) = "**edgeOrder**"
  astr(19) = " "
END IF

IF (obj%isFaceOrder) THEN
  rowTitle(20) = "**faceOrder**"
  astr(20) = tostring(obj%faceOrder)
ELSE
  rowTitle(20) = "**faceOrder**"
  astr(20) = " "
END IF

IF (obj%iscellOrder) THEN
  rowTitle(21) = "**cellOrder**"
  astr(21) = tostring(obj%cellOrder)
ELSE
  rowTitle(21) = "**cellOrder**"
  astr(21) = " "
END IF

ans = React_StartTabs()//char_lf
ans = ans//React_StartTabItem(VALUE="0", label="Finite Element")//char_lf// &
  & MdEncode( &
  & val=astr(1:21), &
  & rh=rowTitle(1:21), &
  & ch=colTitle)//char_lf// &
  & React_EndTabItem()//char_lf// &
  & React_StartTabItem(VALUE="1", label="Reference Element")//char_lf// &
  & char_lf//obj%refelem%ReactEncode()//char_lf// &
  & React_EndTabItem()//char_lf//React_EndTabs()//char_lf
END PROCEDURE fe_ReactEncode

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_SetParam
IF (PRESENT(nsd)) obj%nsd = nsd
IF (PRESENT(order)) obj%order = order
IF (PRESENT(anisoOrder)) obj%anisoOrder = anisoOrder
IF (PRESENT(edgeOrder)) obj%edgeOrder(1:SIZE(edgeOrder)) = edgeOrder
IF (PRESENT(faceOrder)) obj%faceOrder(1:SIZE(faceOrder)) = faceOrder
IF (PRESENT(cellOrder)) obj%cellOrder(1:SIZE(cellOrder)) = cellOrder
IF (PRESENT(feType)) obj%feType = feType
IF (PRESENT(elemType)) obj%elemType = elemType
IF (PRESENT(ipType)) obj%ipType = ipType
IF (PRESENT(dofType)) obj%dofType = dofType
IF (PRESENT(transformType)) obj%transformType = transformType
IF (PRESENT(baseContinuity)) THEN
  CALL BaseContinuity_fromString( &
    & obj=obj%baseContinuity, &
    & name=baseContinuity)
  obj%baseContinuity0 = baseContinuity
END IF
IF (PRESENT(baseInterpol)) THEN
  CALL BaseInterpolation_fromString( &
    & obj=obj%baseInterpol,   &
    & name=baseInterpol)
  obj%baseInterpol0 = baseInterpol
END IF
IF (PRESENT(refElemDomain)) obj%refElemDomain = refElemDomain
IF (PRESENT(isIsotropicOrder)) obj%isIsotropicOrder = isIsotropicOrder
IF (PRESENT(isAnisotropicOrder)) obj%isAnisotropicOrder = isAnisotropicOrder
IF (PRESENT(isEdgeOrder)) obj%isEdgeOrder = isEdgeOrder
IF (PRESENT(isFaceOrder)) obj%isFaceOrder = isFaceOrder
IF (PRESENT(isCellOrder)) obj%isCellOrder = isCellOrder

IF (PRESENT(tEdgeOrder)) obj%tEdgeOrder = tEdgeOrder
IF (PRESENT(tFaceOrder)) obj%tFaceOrder = tFaceOrder
IF (PRESENT(tCellOrder)) obj%tCellOrder = tCellOrder

IF (PRESENT(basisType)) obj%basisType = basisType
IF (PRESENT(alpha)) obj%alpha = alpha
IF (PRESENT(beta)) obj%beta = beta
IF (PRESENT(lambda)) obj%lambda = lambda
END PROCEDURE fe_SetParam

!----------------------------------------------------------------------------
!                                                                GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_GetParam
CHARACTER(*), PARAMETER :: myName = "fe_GetParam()"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[WORK IN PROGRESS]')
END PROCEDURE fe_GetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
