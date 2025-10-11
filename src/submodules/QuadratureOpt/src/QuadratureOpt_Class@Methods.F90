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

SUBMODULE(QuadratureOpt_Class) Methods
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
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Copy the content from obj2 to obj

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = obj2%isInit
obj%isHomogeneous = obj2%isHomogeneous
obj%isOrder = obj2%isOrder
obj%isNips = obj2%isNips
obj%topoType = obj2%topoType
obj%nsd = obj2%nsd
obj%xidim = obj2%xidim
obj%quadratureType = obj2%quadratureType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%refelemCoord = obj2%refelemCoord
obj%order = obj2%order
obj%nips = obj2%nips
obj%quadratureType_char = obj2%quadratureType_char
obj%refelemDomain = obj2%refelemDomain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isInit, "isInit: ", unitNo=unitNo)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isHomogeneous, "isHomogeneous: ", unitNo=unitNo)
CALL Display(obj%isOrder, "isOrder: ", unitNo=unitNo)
CALL Display(obj%isNips, "isNips: ", unitNo=unitNo)

CALL Display(obj%topoType, "topoType: ", unitNo=unitNo)
CALL Display(obj%nsd, "nsd: ", unitNo=unitNo)
CALL Display(obj%xidim, "xidim: ", unitNo=unitNo)

DO ii = 1, 3
  CALL Display(obj%quadratureType(ii), &
               "quadratureType("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(TRIM(obj%quadratureType_char(ii)), &
               "quadratureType_char("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(obj%alpha(ii), "alpha("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(obj%beta(ii), "beta("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%lambda(ii), "lambda("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%order(ii), "order("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%nips(ii), "nips("//ToString(ii)//', 1): ', &
               unitNo=unitNo)
END DO

CALL Display(obj%refelemCoord, "refelemCoord: ", unitNo=unitNo)

CALL Display(obj%refelemDomain, "refelemDomain: ", unitNo=unitNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                SetParam
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
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL obj%SetParam(isHomogeneous=isHomogeneous,quadratureType=quadratureType,&
                  order=order, isOrder=isOrder, nips=nips, isNips=isNips, &
                  alpha=alpha, beta=beta, lambda=lambda, topoType=topoType, &
                  nsd=nsd, xidim=xidim, refelemDomain=refelemDomain, &
                  refelemCoord=refelemCoord)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                   GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(isHomogeneous)) isHomogeneous = obj%isHomogeneous
IF (PRESENT(nsd)) nsd = obj%nsd
IF (PRESENT(topoType)) topoType = obj%topoType
IF (PRESENT(isOrder)) isOrder = obj%isOrder
IF (PRESENT(isNips)) isNips = obj%isNips

IF (PRESENT(quadratureType)) quadratureType(1:3) = obj%quadratureType
IF (PRESENT(quadratureType1)) quadratureType1 = obj%quadratureType(1)
IF (PRESENT(quadratureType2)) quadratureType2 = obj%quadratureType(2)
IF (PRESENT(quadratureType3)) quadratureType3 = obj%quadratureType(3)

IF (PRESENT(order)) order(1:3) = obj%order
IF (PRESENT(order1)) order1 = obj%order(1)
IF (PRESENT(order2)) order2 = obj%order(2)
IF (PRESENT(order3)) order3 = obj%order(3)

IF (PRESENT(nips)) nips(1:3) = obj%nips(1:3)
IF (PRESENT(nips1)) nips1 = obj%nips(1)
IF (PRESENT(nips2)) nips2 = obj%nips(2)
IF (PRESENT(nips3)) nips3 = obj%nips(3)

IF (PRESENT(alpha)) alpha(1:3) = obj%alpha
IF (PRESENT(alpha1)) alpha1 = obj%alpha(1)
IF (PRESENT(alpha2)) alpha2 = obj%alpha(2)
IF (PRESENT(alpha3)) alpha3 = obj%alpha(3)

IF (PRESENT(beta)) beta(1:3) = obj%beta
IF (PRESENT(beta1)) beta1 = obj%beta(1)
IF (PRESENT(beta2)) beta2 = obj%beta(2)
IF (PRESENT(beta3)) beta3 = obj%beta(3)

IF (PRESENT(lambda)) lambda(1:3) = obj%lambda
IF (PRESENT(lambda1)) lambda1 = obj%lambda(1)
IF (PRESENT(lambda2)) lambda2 = obj%lambda(2)
IF (PRESENT(lambda3)) lambda3 = obj%lambda(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL obj%Copy(TypeQuadratureOpt)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                          GetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

IF (obj%isHomogeneous .AND. obj%isOrder) THEN
  CALL QuadraturePoint_Initiate(obj=quad, &
                                elemType=obj%topoType, &
                                domainName=obj%refelemDomain, &
                                order=obj%order(1), &
                                quadratureType=obj%quadratureType(1), &
                                alpha=obj%alpha(1), &
                                beta=obj%beta(1), &
                                lambda=obj%lambda(1), &
                                xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (obj%isHomogeneous .AND. obj%isNips) THEN
  CALL QuadraturePoint_Initiate(obj=quad, &
                                elemType=obj%topoType, &
                                domainName=obj%refelemDomain, &
                                nips=obj%nips(1:1), &
                                quadratureType=obj%quadratureType(1), &
                                alpha=obj%alpha(1), &
                                beta=obj%beta(1), &
                                lambda=obj%lambda(1), &
                                xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (.NOT. obj%isHomogeneous .AND. obj%isOrder) THEN
  CALL QuadraturePoint_Initiate(obj=quad, &
                                elemType=obj%topoType, &
                                domainName=obj%refelemDomain, &
                                p=obj%order(1), &
                                q=obj%order(2), &
                                r=obj%order(3), &
                                quadratureType1=obj%quadratureType(1), &
                                quadratureType2=obj%quadratureType(2), &
                                quadratureType3=obj%quadratureType(3), &
                                alpha1=obj%alpha(1), &
                                alpha2=obj%alpha(2), &
                                alpha3=obj%alpha(3), &
                                beta1=obj%beta(1), &
                                beta2=obj%beta(2), &
                                beta3=obj%beta(3), &
                                lambda1=obj%lambda(1), &
                                lambda2=obj%lambda(2), &
                                lambda3=obj%lambda(3), &
                                xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

IF (.NOT. obj%isHomogeneous .AND. obj%isNips) THEN
  CALL QuadraturePoint_Initiate(obj=quad, &
                                elemType=obj%topoType, &
                                domainName=obj%refelemDomain, &
                                nipsx=obj%order(1:1), &
                                nipsy=obj%order(2:2), &
                                nipsz=obj%order(3:3), &
                                quadratureType1=obj%quadratureType(1), &
                                quadratureType2=obj%quadratureType(2), &
                                quadratureType3=obj%quadratureType(3), &
                                alpha1=obj%alpha(1), &
                                alpha2=obj%alpha(2), &
                                alpha3=obj%alpha(3), &
                                beta1=obj%beta(1), &
                                beta2=obj%beta(2), &
                                beta3=obj%beta(3), &
                                lambda1=obj%lambda(1), &
                                lambda2=obj%lambda(2), &
                                lambda3=obj%lambda(3), &
                                xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END IF

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'No valid case found')
#endif

END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                     Line_GetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Line_GetQuadraturePoints()"
#endif

INTEGER(I4B) :: nips(1), nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

IF (obj%isOrder) THEN
  nips(1) = QuadratureNumber_Line(order=obj%order(1), &
                                  quadtype=obj%quadratureType(1))
ELSE
  nips(1) = obj%nips(1)
END IF

nrow = obj%xidim + 1
CALL Reallocate(quad%points, nrow, nips(1))

CALL QuadraturePoint_Line_( &
  nips=nips, quadType=obj%quadratureType(1), layout="INCREASING", &
  xij=obj%refelemCoord(1:obj%xidim, 1:2), alpha=obj%alpha(1), &
  beta=obj%beta(1), lambda=obj%lambda(1), ans=quad%points, &
  nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Line_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                          GetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

IF (obj%isHomogeneous .AND. obj%isOrder) THEN
  CALL InitiateFacetQuadrature(obj=quad, facetQuad=facetQuad, &
                               localFaceNumber=localFaceNumber, &
                               elemType=obj%topoType, &
                               domainName=obj%refelemDomain, &
                               order=obj%order(1), &
                               quadratureType=obj%quadratureType(1), &
                               alpha=obj%alpha(1), &
                               beta=obj%beta(1), &
                               lambda=obj%lambda(1), &
                               xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (obj%isHomogeneous .AND. obj%isNips) THEN
  CALL InitiateFacetQuadrature(obj=quad, facetQuad=facetQuad, &
                               localFaceNumber=localFaceNumber, &
                               elemType=obj%topoType, &
                               domainName=obj%refelemDomain, &
                               nips=obj%nips(1:1), &
                               quadratureType=obj%quadratureType(1), &
                               alpha=obj%alpha(1), &
                               beta=obj%beta(1), &
                               lambda=obj%lambda(1), &
                               xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (.NOT. obj%isHomogeneous .AND. obj%isOrder) THEN
  CALL InitiateFacetQuadrature(obj=quad, facetQuad=facetQuad, &
                               localFaceNumber=localFaceNumber, &
                               elemType=obj%topoType, &
                               domainName=obj%refelemDomain, &
                               p=obj%order(1), &
                               q=obj%order(2), &
                               r=obj%order(3), &
                               quadratureType1=obj%quadratureType(1), &
                               quadratureType2=obj%quadratureType(2), &
                               quadratureType3=obj%quadratureType(3), &
                               alpha1=obj%alpha(1), &
                               alpha2=obj%alpha(2), &
                               alpha3=obj%alpha(3), &
                               beta1=obj%beta(1), &
                               beta2=obj%beta(2), &
                               beta3=obj%beta(3), &
                               lambda1=obj%lambda(1), &
                               lambda2=obj%lambda(2), &
                               lambda3=obj%lambda(3), &
                               xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

IF (.NOT. obj%isHomogeneous .AND. obj%isNips) THEN
  CALL InitiateFacetQuadrature(obj=quad, facetQuad=facetQuad, &
                               localFaceNumber=localFaceNumber, &
                               elemType=obj%topoType, &
                               domainName=obj%refelemDomain, &
                               nipsx=obj%order(1:1), &
                               nipsy=obj%order(2:2), &
                               nipsz=obj%order(3:3), &
                               quadratureType1=obj%quadratureType(1), &
                               quadratureType2=obj%quadratureType(2), &
                               quadratureType3=obj%quadratureType(3), &
                               alpha1=obj%alpha(1), &
                               alpha2=obj%alpha(2), &
                               alpha3=obj%alpha(3), &
                               beta1=obj%beta(1), &
                               beta2=obj%beta(2), &
                               beta3=obj%beta(3), &
                               lambda1=obj%lambda(1), &
                               lambda2=obj%lambda(2), &
                               lambda3=obj%lambda(3), &
                               xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END IF

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'No valid case found')
#endif

END PROCEDURE obj_GetFacetQuadraturePoints

!----------------------------------------------------------------------------
!                                                    GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints()"
#endif

LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
abool = obj%isOrder .AND. obj%isNips
IF (abool) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

abool = obj%isHomogeneous .AND. obj%isNips
IF (abool) THEN

  SELECT CASE (obj%topoType)
  CASE (TypeElemNameOpt%quadrangle)
    ans = obj%nips(1) * obj%nips(1)
  CASE (TypeElemNameOpt%hexahedron)
    ans = obj%nips(1) * obj%nips(1) * obj%nips(1)
  CASE DEFAULT
    ans = obj%nips(1)
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

abool = .NOT. obj%isHomogeneous .AND. obj%isNips
IF (abool) THEN
  SELECT CASE (obj%topoType)
  CASE (TypeElemNameOpt%quadrangle)
    ans = PRODUCT(obj%nips(1:2))
  CASE (TypeElemNameOpt%hexahedron)
    ans = PRODUCT(obj%nips(1:3))
  CASE DEFAULT
    ans = obj%nips(1)
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

abool = obj%isHomogeneous .AND. obj%isOrder
IF (abool) THEN
  ans = GetTotalQuadraturePoints( &
        elemType=obj%topoType, &
        p=obj%order(1), &
        q=obj%order(1), &
        r=obj%order(1), &
        quadratureType1=obj%quadratureType(1), &
        quadratureType2=obj%quadratureType(1), &
        quadratureType3=obj%quadratureType(1))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

abool = .NOT. obj%isHomogeneous .AND. obj%isOrder
IF (abool) THEN
  ans = GetTotalQuadraturePoints( &
        elemType=obj%topoType, &
        p=obj%order(1), &
        q=obj%order(2), &
        r=obj%order(3), &
        quadratureType1=obj%quadratureType(1), &
        quadratureType2=obj%quadratureType(2), &
        quadratureType3=obj%quadratureType(3))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'No valid case found')
#endif
END PROCEDURE obj_GetTotalQuadraturePoints

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
