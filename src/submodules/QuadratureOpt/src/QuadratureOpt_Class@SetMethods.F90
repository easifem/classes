! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(QuadratureOpt_Class) SetMethods
USE Display_Method, ONLY: Display, ToString
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar, &
                                  QuadraturePoint_ToInteger, &
                                  QuadraturePoint_Initiate => Initiate, &
                                  GetTotalQuadraturePoints, &
                                  InitiateFacetQuadrature
USE InputUtility, ONLY: Input
USE BaseType, ONLY: TypeElemNameOpt

USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_, &
                                    QuadratureNumber_Line

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(isHomogeneous); IF (isok) obj%isHomogeneous = isHomogeneous
isok = PRESENT(nsd); IF (isok) obj%nsd = nsd
isok = PRESENT(xidim); IF (isok) obj%xidim = xidim
isok = PRESENT(topoType); IF (isok) obj%topoType = topoType
isok = PRESENT(refelemDomain); IF (isok) obj%refelemDomain = refelemDomain

CALL obj%SetQuadratureType(quadratureType=quadratureType, &
                           quadratureType1=quadratureType1, &
                           quadratureType2=quadratureType2, &
                           quadratureType3=quadratureType3)

CALL obj%SetOrder(order=order, order1=order1, order2=order2, order3=order3)

CALL obj%SetNips(nips=nips, nips1=nips1, nips2=nips2, nips3=nips3)

CALL obj%SetAlpha(alpha=alpha, alpha1=alpha1, alpha2=alpha2, alpha3=alpha3)

CALL obj%SetBeta(beta=beta, beta1=beta1, beta2=beta2, beta3=beta3)

CALL obj%SetLambda(lambda=lambda, lambda1=lambda1, lambda2=lambda2, &
                   lambda3=lambda3)

CALL obj%SetRefElemCoord(refelemCoord=refelemCoord)

isok = PRESENT(isOrder)
IF (isok) obj%isOrder = isOrder

isok = PRESENT(isNips)
IF (isok) obj%isNips = isNips

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                           SetRefElemCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRefElemCoord
! Internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRefElemCoord()"
#endif
INTEGER(I4B) :: nrow, ncol, s(2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(refelemCoord)
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

s = SHAPE(refelemCoord)
nrow = MIN(s(1), 3)
ncol = MIN(s(2), 8)
obj%refelemCoord(1:nrow, 1:ncol) = refelemCoord(1:nrow, 1:ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetRefElemCoord

!----------------------------------------------------------------------------
!                                                           SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureType
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureType()"
LOGICAL(LGT) :: problem
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(quadratureType) .AND. obj%isHomogeneous
IF (isok) THEN

  DO ii = 1, 3
    obj%quadratureType(ii) = quadratureType(1)
    obj%quadratureType_char(ii) = QuadraturePoint_ToChar( &
                                  obj%quadratureType(ii), isUpper=.TRUE.)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(quadratureType)
IF (isok) THEN
  tsize = MIN(SIZE(quadratureType), 3)
  DO ii = 1, tsize
    obj%quadratureType(ii) = quadratureType(ii)
    obj%quadratureType_char(ii) = QuadraturePoint_ToChar( &
                                  obj%quadratureType(ii), isUpper=.TRUE.)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
problem = obj%isHomogeneous .AND. PRESENT(quadratureType1)
CALL AssertError1(.NOT. problem, myName, &
    'obj%isHomogeneous is true, so quadratureType1 should not be present.'// &
               'Either set obj%isHomogeneous to false or pass quadratureType')

problem = obj%isHomogeneous .AND. PRESENT(quadratureType2)
CALL AssertError1(.NOT. problem, myName, &
    'obj%isHomogeneous is true, so quadratureType2 should not be present.'// &
               'Either set obj%isHomogeneous to false or pass quadratureType')

problem = obj%isHomogeneous .AND. PRESENT(quadratureType3)
CALL AssertError1(.NOT. problem, myName, &
    'obj%isHomogeneous is true, so quadratureType3 should not be present.'// &
               'Either set obj%isHomogeneous to false or pass quadratureType')
#endif

isok = PRESENT(quadratureType1)
IF (isok) THEN
  obj%quadratureType(1) = quadratureType1
  obj%quadratureType_char(1) = QuadraturePoint_ToChar( &
                               obj%quadratureType(1), isUpper=.TRUE.)
END IF

isok = PRESENT(quadratureType2)
IF (isok) THEN
  obj%quadratureType(2) = quadratureType2
  obj%quadratureType_char(2) = QuadraturePoint_ToChar( &
                               obj%quadratureType(2), isUpper=.TRUE.)
END IF

isok = PRESENT(quadratureType3)
IF (isok) THEN
  obj%quadratureType(3) = quadratureType3
  obj%quadratureType_char(3) = QuadraturePoint_ToChar( &
                               obj%quadratureType(3), isUpper=.TRUE.)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureType

!----------------------------------------------------------------------------
!                                                      Line_SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_SetQuadratureType
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Line_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isHomogeneous = .TRUE.
obj%quadratureType(1:3) = quadratureType
obj%quadratureType_char(1) = QuadraturePoint_ToChar(quadratureType, &
                                                    isUpper=.TRUE.)
obj%quadratureType_char(2) = obj%quadratureType_char(1)
obj%quadratureType_char(3) = obj%quadratureType_char(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Line_SetQuadratureType

!----------------------------------------------------------------------------
!                                                  Triangle_SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE Triangle_SetQuadratureType
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Triangle_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isHomogeneous = .TRUE.
obj%quadratureType(1:3) = quadratureType
obj%quadratureType_char(1) = QuadraturePoint_ToChar(quadratureType, &
                                                    isUpper=.TRUE.)
obj%quadratureType_char(2) = obj%quadratureType_char(1)
obj%quadratureType_char(3) = obj%quadratureType_char(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Triangle_SetQuadratureType

!----------------------------------------------------------------------------
!                                                Quadrangle_SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Quadrangle_SetQuadratureType()"
#endif

LOGICAL(LGT) :: isQuadratureType, isQuadratureType1, isQuadratureType2
INTEGER(I4B) :: tsize, quadratureType0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isQuadratureType = PRESENT(quadratureType)
tsize = 0; IF (isQuadratureType) tsize = SIZE(quadratureType)

IF (tsize .EQ. 1) THEN
  obj%quadratureType = quadratureType(1)
  obj%isHomogeneous = .TRUE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (tsize .GT. 1) THEN
  obj%quadratureType(1:2) = quadratureType(1:2)
  obj%isHomogeneous = .FALSE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isQuadratureType1 = PRESENT(quadratureType1)
isQuadratureType2 = PRESENT(quadratureType2)

IF (isQuadratureType1 .AND. isQuadratureType2) THEN
  obj%quadratureType(1) = quadratureType1
  obj%quadratureType(2) = quadratureType2
  obj%isHomogeneous = .FALSE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (isQuadratureType1) quadratureType0 = quadratureType1
IF (isQuadratureType2) quadratureType0 = quadratureType2

IF (obj%isHomogeneous) THEN
  obj%quadratureType = quadratureType0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (isQuadratureType1) THEN
  obj%quadratureType(1) = quadratureType1

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (isQuadratureType2) THEN
  obj%quadratureType(2) = quadratureType2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Quadrangle_SetQuadratureType

!----------------------------------------------------------------------------
!                                                                    SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
LOGICAL(LGT) :: problem
#endif
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(order) .AND. obj%isHomogeneous

IF (isok) THEN

  obj%order = order(1)
  obj%isOrder = .TRUE.
  obj%isNips = .FALSE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(order)
IF (isok) THEN
  tsize = MIN(SIZE(order), 3)
  obj%order(1:tsize) = order(1:tsize)
  obj%isOrder = .TRUE.
  obj%isNips = .FALSE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
problem = obj%isHomogeneous .AND. PRESENT(order1)
CALL AssertError1(.NOT. problem, myName, &
             'obj%isHomogeneous is true, so order1 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass order')

problem = obj%isHomogeneous .AND. PRESENT(order2)
CALL AssertError1(.NOT. problem, myName, &
             'obj%isHomogeneous is true, so order2 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass order')

problem = obj%isHomogeneous .AND. PRESENT(order3)
CALL AssertError1(.NOT. problem, myName, &
             'obj%isHomogeneous is true, so order3 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass order')
#endif

isok = PRESENT(order1)
IF (isok) THEN
  obj%order(1) = order1
  obj%isOrder = .TRUE.
  obj%isNips = .FALSE.
END IF

isok = PRESENT(order2)

IF (isok) THEN
  obj%order(2) = order2
  obj%isOrder = .TRUE.
  obj%isNips = .FALSE.
END IF

isok = PRESENT(order3)
IF (isok) THEN
  obj%order(3) = order3
  obj%isOrder = .TRUE.
  obj%isNips = .FALSE.
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                              Line_SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_SetOrder
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Line_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%order = order
obj%isHomogeneous = .TRUE.
obj%isOrder = .TRUE.
obj%isNips = .FALSE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Line_SetOrder

!----------------------------------------------------------------------------
!                                                           Triangle_SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Triangle_SetOrder
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Triangle_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%order = order
obj%isHomogeneous = .TRUE.
obj%isOrder = .TRUE.
obj%isNips = .FALSE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Triangle_SetOrder

!----------------------------------------------------------------------------
!                                                        Quadrangle_SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_SetOrder
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Quadrangle_SetOrder()"
#endif

LOGICAL(LGT) :: isOrder, isOrder1, isOrder2
INTEGER(I4B) :: tsize, order0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isOrder = .TRUE.
obj%isNips = .FALSE.

isOrder = PRESENT(order)
tsize = 0; IF (isOrder) tsize = SIZE(order)

IF (tsize .EQ. 1) THEN
  obj%order = order(1)
  obj%isHomogeneous = .TRUE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (tsize .GT. 1) THEN
  obj%order(1:2) = order(1:2)
  obj%isHomogeneous = .FALSE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isOrder1 = PRESENT(order1)
isOrder2 = PRESENT(order2)

IF (isOrder1 .AND. isOrder2) THEN
  obj%order(1) = order1
  obj%order(2) = order2
  obj%isHomogeneous = .FALSE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (isOrder1) order0 = order1
IF (isOrder2) order0 = order2

IF (obj%isHomogeneous) THEN
  obj%order = order0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (isOrder1) THEN
  obj%order(1) = order1

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (isOrder2) THEN
  obj%order(2) = order2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Quadrangle_SetOrder

!----------------------------------------------------------------------------
!                                                                     SetNips
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetNips
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetNips()"
LOGICAL(LGT) :: problem
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(nips) .AND. obj%isHomogeneous
IF (isok) THEN
  obj%nips = nips(1)
  obj%isNips = .TRUE.
  obj%isOrder = .FALSE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(nips)
IF (isok) THEN
  tsize = MIN(SIZE(nips), 3)
  obj%nips(1:tsize) = nips(1:tsize)
  obj%isNips = .TRUE.
  obj%isOrder = .FALSE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
problem = obj%isHomogeneous .AND. PRESENT(nips1)
CALL AssertError1(.NOT. problem, myName, &
              'obj%isHomogeneous is true, so nips1 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass nips')

problem = obj%isHomogeneous .AND. PRESENT(nips2)
CALL AssertError1(.NOT. problem, myName, &
              'obj%isHomogeneous is true, so nips2 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass nips')

problem = obj%isHomogeneous .AND. PRESENT(nips3)
CALL AssertError1(.NOT. problem, myName, &
              'obj%isHomogeneous is true, so nips3 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass nips')
#endif

isok = PRESENT(nips1)
IF (isok) THEN
  obj%nips(1) = nips1
  obj%isNips = .TRUE.
  obj%isOrder = .FALSE.
END IF

isok = PRESENT(nips2)
IF (isok) THEN
  obj%nips(2) = nips2
  obj%isNips = .TRUE.
  obj%isOrder = .FALSE.
END IF

isok = PRESENT(nips3)
IF (isok) THEN
  obj%nips(3) = nips3
  obj%isNips = .TRUE.
  obj%isOrder = .FALSE.
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetNips

!----------------------------------------------------------------------------
!                                                                    SetAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAlpha
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetAlpha()"
LOGICAL(LGT) :: problem
#endif
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(alpha) .AND. obj%isHomogeneous
IF (isok) THEN
  obj%alpha = alpha(1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(alpha)
IF (isok) THEN
  tsize = MIN(SIZE(alpha), 3)
  obj%alpha(1:tsize) = alpha(1:tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
problem = obj%isHomogeneous .AND. PRESENT(alpha1)
CALL AssertError1(.NOT. problem, myName, &
             'obj%isHomogeneous is true, so alpha1 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass alpha')

problem = obj%isHomogeneous .AND. PRESENT(alpha2)
CALL AssertError1(.NOT. problem, myName, &
             'obj%isHomogeneous is true, so alpha2 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass alpha')

problem = obj%isHomogeneous .AND. PRESENT(alpha3)
CALL AssertError1(.NOT. problem, myName, &
             'obj%isHomogeneous is true, so alpha3 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass alpha')
#endif

isok = PRESENT(alpha1)
IF (isok) obj%alpha(1) = alpha1

isok = PRESENT(alpha2)
IF (isok) obj%alpha(2) = alpha2

isok = PRESENT(alpha3)
IF (isok) obj%alpha(3) = alpha3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetAlpha

!----------------------------------------------------------------------------
!                                                                     Setbeta
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetBeta
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetBeta()"
LOGICAL(LGT) :: problem
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(beta) .AND. obj%isHomogeneous
IF (isok) THEN
  obj%beta = beta(1)
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(beta)
IF (isok) THEN
  tsize = MIN(SIZE(beta), 3)
  obj%beta(1:tsize) = beta(1:tsize)
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
problem = obj%isHomogeneous .AND. PRESENT(beta1)
CALL AssertError1(.NOT. problem, myName, &
              'obj%isHomogeneous is true, so beta1 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass beta')

problem = obj%isHomogeneous .AND. PRESENT(beta2)
CALL AssertError1(.NOT. problem, myName, &
              'obj%isHomogeneous is true, so beta2 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass beta')

problem = obj%isHomogeneous .AND. PRESENT(beta3)
CALL AssertError1(.NOT. problem, myName, &
              'obj%isHomogeneous is true, so beta3 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass beta')
#endif

isok = PRESENT(beta1)
IF (isok) obj%beta(1) = beta1

isok = PRESENT(beta2)
IF (isok) obj%beta(2) = beta2

isok = PRESENT(beta3)
IF (isok) obj%beta(3) = beta3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetBeta

!----------------------------------------------------------------------------
!                                                                   SetLambda
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLambda
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetLambda()"
LOGICAL(LGT) :: problem
#endif
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(lambda) .AND. obj%isHomogeneous
IF (isok) THEN
  obj%lambda = lambda(1)
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(lambda)
IF (isok) THEN
  tsize = MIN(SIZE(lambda), 3)
  obj%lambda(1:tsize) = lambda(1:tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
problem = obj%isHomogeneous .AND. PRESENT(lambda1)
CALL AssertError1(.NOT. problem, myName, &
            'obj%isHomogeneous is true, so lambda1 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass lambda')

problem = obj%isHomogeneous .AND. PRESENT(lambda2)
CALL AssertError1(.NOT. problem, myName, &
            'obj%isHomogeneous is true, so lambda2 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass lambda')

problem = obj%isHomogeneous .AND. PRESENT(lambda3)
CALL AssertError1(.NOT. problem, myName, &
            'obj%isHomogeneous is true, so lambda3 should not be present.'// &
                  'Either set obj%isHomogeneous to false or pass lambda')
#endif

isok = PRESENT(lambda1)
IF (isok) obj%lambda(1) = lambda1

isok = PRESENT(lambda2)
IF (isok) obj%lambda(2) = lambda2

isok = PRESENT(lambda3)
IF (isok) obj%lambda(3) = lambda3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetLambda

!----------------------------------------------------------------------------
!                                                              Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
