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
                                   isQuadrangle, &
                                   isHexahedron, &
                                   ReferenceElement_DEALLOCATE => DEALLOCATE

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL CheckEssentialParam(obj=param, keys=AbstractFEEssentialParams, &
                       prefix=obj%GetPrefix(), myName=myName, modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                       SetAbstractFEParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractFEParam
CHARACTER(*), PARAMETER :: myName = "SetAbstractFEParam()"

INTEGER(I4B) :: ierr, ipType0, topoType, xidim

CHARACTER(:), ALLOCATABLE :: baseContinuity0, baseInterpolation0

TYPE(String) :: astr

LOGICAL(LGT) :: isok

TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

sublist => NULL()
sublist => param%NewSubList(key=prefix)

CALL Set(obj=sublist, prefix=prefix, key="nsd", datatype=nsd, VALUE=nsd)

CALL Set(obj=sublist, prefix=prefix, key="elemType", datatype=elemType, &
         VALUE=elemType)

baseContinuity0 = UpperCase(baseContinuity)
baseInterpolation0 = UpperCase(baseInterpolation)

CALL Set(obj=sublist, prefix=prefix, key="baseContinuity", &
         datatype=baseContinuity0, VALUE=baseContinuity0)

CALL Set(obj=sublist, prefix=prefix, key="baseInterpolation", &
         datatype=baseContinuity0, VALUE=baseInterpolation0)

CALL Set(obj=sublist, prefix=prefix, key="feType", &
         datatype=1_I4B, VALUE=Input(option=feType, default=Scalar))

! CALL Set(obj=sublist, prefix=prefix, key="dofType", &
!         datatype=1_I4B, VALUE=Input(option=dofType, default=DEFAULT_DOF_TYPE))

CALL Set(obj=sublist, prefix=prefix, key="transformType", &
         datatype=1_I4B, VALUE=Input(option=transformType, &
                                     default=DEFAULT_TRANSFORM_TYPE))

IF (baseInterpolation0(1:4) .EQ. "LAGR") THEN
  isok = PRESENT(ipType)
  CALL AssertError1(isok, myName, &
                  'In case of LAGRANGE polynomials ipType should be present.')
END IF
ipType0 = Input(default=Equidistance, option=ipType)
CALL Set(obj=sublist, prefix=prefix, key="ipType", &
         datatype=ipType0, VALUE=ipType0)

! RefElemDomain is defined in InterpolationUtility_Method
astr = RefElemDomain(baseInterpol=baseInterpolation0, &
                     baseContinuity=baseContinuity0, elemType=elemType)

CALL Set(obj=sublist, prefix=prefix, key="refElemDomain", &
         datatype=astr, VALUE=astr)

topoType = ElementTopology(elemType)

! What is happening here?
CALL SetFEParam_BasisType(param=sublist, elemType=elemType, nsd=nsd, &
                         topoType=topoType, baseContinuity0=baseContinuity0, &
                 baseInterpolation0=baseInterpolation0, basisType=basisType, &
                         alpha=alpha, beta=beta, lambda=lambda, prefix=prefix)

IF (PRESENT(order)) THEN
  CALL SetFEParam_Order(param=sublist, order=order, elemType=elemType, &
                        prefix=prefix)
  CALL finishme
  RETURN
END IF

IF (nsd .EQ. 1_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[ARGUMENT ERROR] For 1D elements Order must be present.')
  RETURN
END IF

IF (PRESENT(anisoOrder)) THEN
  CALL SetFEParam_AnisoOrder(param=sublist, anisoOrder=anisoOrder, &
                             elemType=elemType, nsd=nsd, prefix=prefix)

  CALL finishme
  RETURN

END IF

xidim = XiDimension(elemType)

SELECT CASE (topoType)
CASE (Triangle)
  CALL SetFEParam_Heirarchy2D(param=sublist, elemType=elemType, xidim=xidim, &
          isQuad=.FALSE., nsd=nsd, edgeOrder=edgeOrder, faceOrder=faceOrder, &
                              prefix=prefix)
CASE (Quadrangle)
  CALL SetFEParam_Heirarchy2D(param=sublist, elemType=elemType, xidim=xidim, &
           isQuad=.TRUE., nsd=nsd, edgeOrder=edgeOrder, faceOrder=faceOrder, &
                              prefix=prefix)

CASE (Tetrahedron)
CALL SetFEParam_Heirarchy3D(param=sublist, elemType=elemType, isHexa=.FALSE.,&
          isTetra=.TRUE., nsd=nsd, edgeOrder=edgeOrder, faceOrder=faceOrder, &
                              cellOrder=cellOrder, prefix=prefix)

CASE (Hexahedron)
CALL SetFEParam_Heirarchy3D(param=sublist, elemType=elemType, isHexa=.TRUE., &
         isTetra=.FALSE., nsd=nsd, edgeOrder=edgeOrder, faceOrder=faceOrder, &
                              cellOrder=cellOrder, prefix=prefix)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                   '[INTERNAL ERROR] :: No case found for given element type')
  RETURN
END SELECT

CALL finishme

CONTAINS
SUBROUTINE finishme

  astr = ""
  baseInterpolation0 = ""
  baseContinuity0 = ""

  sublist => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE finishme

END PROCEDURE SetAbstractFEParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType_Line(param, elemType, nsd, baseContinuity0, &
                   baseInterpolation0, basisType, alpha, beta, lambda, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpolation0
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
  CHARACTER(*), INTENT(IN) :: prefix

  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType_Line()"
  INTEGER(I4B) :: AINT(3)
  REAL(DFP) :: areal(3)
  CHARACTER(4) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (PRESENT(basisType)) THEN

    aint = basisType(1)

  ELSE

    astr = baseInterpolation0(1:4)

    SELECT CASE (astr)
    CASE ("LAGR")
      aint = Monomial
    CASE ("ORTH")
      aint = Legendre
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - '// &
       '[INTERNAL ERROR] :: No case found for baseInterpolation0(1:4)='//astr)
      RETURN
    END SELECT
  END IF

  CALL Set(obj=param, prefix=prefix, key="basisType", datatype=aint, &
           VALUE=aint)

  areal = 0.0_DFP; IF (PRESENT(alpha)) areal = alpha(1)
  CALL Set(obj=param, prefix=prefix, key="alpha", datatype=areal, &
           VALUE=areal)

  areal = 0.0_DFP; IF (PRESENT(beta)) areal = beta(1)
  CALL Set(obj=param, prefix=prefix, key="beta", datatype=areal, &
           VALUE=areal)

  areal = 0.5_DFP; IF (PRESENT(lambda)) areal = lambda(1)
  CALL Set(obj=param, prefix=prefix, key="lambda", datatype=areal, &
           VALUE=areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetFEParam_BasisType_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType_Simplex(param, elemType, nsd, &
                       baseContinuity0, baseInterpolation0, basisType, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpolation0
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  CHARACTER(*), INTENT(IN) :: prefix

  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType_Simplex()"
  CHARACTER(4) :: astr
  INTEGER(I4B) :: AINT(3)
  REAL(DFP), PARAMETER :: areal(3) = 0.0_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (PRESENT(basisType)) THEN

    aint = basisType(1)

  ELSE

    astr = baseInterpolation0(1:4)

    SELECT CASE (astr)
    CASE ("LAGR")
      aint = Monomial
    CASE ("ORTH")
      aint = Legendre
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - '// &
       '[INTERNAL ERROR] :: No case found for baseInterpolation0(1:4)='//astr)
      RETURN
    END SELECT
  END IF

  CALL Set(obj=param, prefix=prefix, key="basisType", datatype=aint, &
           VALUE=aint)

  CALL Set(obj=param, prefix=prefix, key="alpha", datatype=areal, VALUE=areal)

  CALL Set(obj=param, prefix=prefix, key="beta", datatype=areal, VALUE=areal)

  CALL Set(obj=param, prefix=prefix, key="lambda", datatype=areal, &
           VALUE=areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetFEParam_BasisType_Simplex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType_Cartesian(param, elemType, nsd, xidim, &
  baseContinuity0, baseInterpolation0, basisType, alpha, beta, lambda, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), INTENT(IN) :: xidim
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpolation0
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
  CHARACTER(*), INTENT(IN) :: prefix

  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType_Cartesian()"
  CHARACTER(4) :: astr
  INTEGER(I4B) :: AINT(3)
  REAL(DFP) :: areal(3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (PRESENT(basisType)) THEN
    IF (SIZE(basisType) .EQ. 1_I4B) THEN
      aint = basisType(1)
    ELSE
      AINT(1:xidim) = basisType(1:xidim)
    END IF
  ELSE
    astr = baseInterpolation0(1:4)

    SELECT CASE (astr)
    CASE ("LAGR")
      aint = Monomial
    CASE ("ORTH")
      aint = Legendre
    CASE DEFAULT
      CALL e%RaiseError(modName//'::'//myName//' - '// &
       '[INTERNAL ERROR] :: No case found for baseInterpolation0(1:4)='//astr)
      RETURN
    END SELECT

  END IF

  CALL Set(obj=param, prefix=prefix, key="basisType", datatype=aint, &
           VALUE=aint)

  IF (PRESENT(alpha)) THEN
    IF (SIZE(alpha) .EQ. 1) THEN
      areal = alpha(1)
    ELSE
      areal(1:xidim) = alpha(1:xidim)
    END IF
  ELSE
    areal = 0.0_DFP
  END IF
  CALL Set(obj=param, prefix=prefix, key="alpha", datatype=areal, VALUE=areal)

  IF (PRESENT(beta)) THEN
    IF (SIZE(beta) .EQ. 1) THEN
      areal = beta(1)
    ELSE
      areal(1:xidim) = beta(1:xidim)
    END IF
  ELSE
    areal = 0.0_DFP
  END IF
  CALL Set(obj=param, prefix=prefix, key="beta", datatype=areal, VALUE=areal)

  IF (PRESENT(lambda)) THEN
    IF (SIZE(lambda) .EQ. 1) THEN
      areal = lambda(1)
    ELSE
      areal(1:xidim) = lambda(1:xidim)
    END IF
  ELSE
    areal = 0.5_DFP
  END IF
 CALL Set(obj=param, prefix=prefix, key="lambda", datatype=areal, VALUE=areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetFEParam_BasisType_Cartesian

!----------------------------------------------------------------------------
!                                                       SetFEParam_BasisType
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType(param, elemType, nsd, baseContinuity0, &
         baseInterpolation0, topoType, basisType, alpha, beta, lambda, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: baseContinuity0
  CHARACTER(*), INTENT(IN) :: baseInterpolation0
  INTEGER(I4B), INTENT(IN) :: topoType
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
  CHARACTER(*), INTENT(IN) :: prefix

  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (topoType)
  CASE (Line)
    CALL SetFEParam_BasisType_Line(param=param, elemType=elemType, nsd=nsd, &
                                   baseContinuity0=baseContinuity0, &
                                   baseInterpolation0=baseInterpolation0, &
                                basisType=basisType, alpha=alpha, beta=beta, &
                                   lambda=lambda, prefix=prefix)

  CASE (Triangle, Tetrahedron, Prism, Pyramid)
    CALL SetFEParam_BasisType_Simplex(param=param, elemType=elemType, &
                                   nsd=nsd, baseContinuity0=baseContinuity0, &
                                      baseInterpolation0=baseInterpolation0, &
                                      basisType=basisType, prefix=prefix)

  CASE (Quadrangle)
    CALL SetFEParam_BasisType_Cartesian(param=param, elemType=elemType, &
                      nsd=nsd, xidim=2_I4B, baseContinuity0=baseContinuity0, &
                                      baseInterpolation0=baseInterpolation0, &
                                        basisType=basisType, alpha=alpha, &
                                      beta=beta, lambda=lambda, prefix=prefix)

  CASE (Hexahedron)
    CALL SetFEParam_BasisType_Cartesian(param=param, elemType=elemType, &
                      nsd=nsd, xidim=2_I4B, baseContinuity0=baseContinuity0, &
                 baseInterpolation0=baseInterpolation0, basisType=basisType, &
                         alpha=alpha, beta=beta, lambda=lambda, prefix=prefix)

  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                   '[INTERNAL ERROR] :: No case found for given element type')
    RETURN
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

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
  INTEGER(I4B) :: AINT(3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Set(obj=param, prefix=prefix, key="isIsotropicOrder", &
           datatype=.TRUE., VALUE=.TRUE.)

  CALL Set(obj=param, prefix=prefix, key="isAnisotropicOrder", &
           datatype=.FALSE., VALUE=.FALSE.)

  CALL Set(obj=param, prefix=prefix, key="isEdgeOrder", &
           datatype=.FALSE., VALUE=.FALSE.)

  CALL Set(obj=param, prefix=prefix, key="isFaceOrder", &
           datatype=.FALSE., VALUE=.FALSE.)

  CALL Set(obj=param, prefix=prefix, key="isCellOrder", &
           datatype=.FALSE., VALUE=.FALSE.)

  aint = order
  CALL Set(obj=param, prefix=prefix, key="order", datatype=aint, VALUE=aint)

  aint = -1
  CALL Set(obj=param, prefix=prefix, key="anisoOrder", datatype=aint, &
           VALUE=aint)

  CALL Set(obj=param, prefix=prefix, key="edgeOrder", datatype=aint, &
           VALUE=AINT(1:1))

  CALL Set(obj=param, prefix=prefix, key="faceOrder", datatype=aint, &
           VALUE=AINT(1:1))

  CALL Set(obj=param, prefix=prefix, key="cellOrder", datatype=aint, &
           VALUE=AINT(1:1))

  CALL Set(obj=param, prefix=prefix, key="faceOrder", datatype=aint, &
           VALUE=AINT(1:1))

  CALL Set(obj=param, prefix=prefix, key="tEdgeOrder", datatype=0_I4B, &
           VALUE=0_I4B)

  CALL Set(obj=param, prefix=prefix, key="tFaceOrder", datatype=0_I4B, &
           VALUE=0_I4B)

  CALL Set(obj=param, prefix=prefix, key="tCellOrder", datatype=0_I4B, &
           VALUE=0_I4B)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

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

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_AnisoOrder()"
  INTEGER(I4B) :: AINT(3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Set(obj=param, prefix=prefix, key="isIsotropicOrder", &
           datatype=.FALSE., VALUE=.FALSE.)

  CALL Set(obj=param, prefix=prefix, key="isAnisotropicOrder", &
           datatype=.TRUE., VALUE=.TRUE.)

  CALL Set(obj=param, prefix=prefix, key="isEdgeOrder", &
           datatype=.FALSE., VALUE=.FALSE.)

  CALL Set(obj=param, prefix=prefix, key="isFaceOrder", &
           datatype=.FALSE., VALUE=.FALSE.)

  CALL Set(obj=param, prefix=prefix, key="isCellOrder", &
           datatype=.FALSE., VALUE=.FALSE.)

  aint = anisoOrder
  CALL Set(obj=param, prefix=prefix, key="anisoOrder", datatype=aint, &
           VALUE=aint)

  aint = -1
  CALL Set(obj=param, prefix=prefix, key="order", datatype=aint, VALUE=aint)

  CALL Set(obj=param, prefix=prefix, key="edgeOrder", datatype=aint, &
           VALUE=AINT(1:1))

  CALL Set(obj=param, prefix=prefix, key="faceOrder", datatype=aint, &
           VALUE=AINT(1:1))

  CALL Set(obj=param, prefix=prefix, key="cellOrder", datatype=aint, &
           VALUE=AINT(1:1))

  CALL Set(obj=param, prefix=prefix, key="faceOrder", datatype=aint, &
           VALUE=AINT(1:1))

  CALL Set(obj=param, prefix=prefix, key="tEdgeOrder", datatype=0_I4B, &
           VALUE=0_I4B)

  CALL Set(obj=param, prefix=prefix, key="tFaceOrder", datatype=0_I4B, &
           VALUE=0_I4B)

  CALL Set(obj=param, prefix=prefix, key="tCellOrder", datatype=0_I4B, &
           VALUE=0_I4B)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetFEParam_AnisoOrder

!----------------------------------------------------------------------------
!                                                     SetFEParam_Heirarchy2D
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_Heirarchy2D(param, elemType, nsd, xidim, isQuad, edgeOrder, &
                                  faceOrder, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), INTENT(IN) :: xidim
  LOGICAL(LGT), INTENT(IN) :: isQuad
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:)
  CHARACTER(*), INTENT(IN) :: prefix

  ! internal variables

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_Heirarchy2D()"
  CHARACTER(:), ALLOCATABLE :: amsg
#endif

  INTEGER(I4B) :: AINT(3), tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = PRESENT(edgeOrder) .AND. PRESENT(faceOrder)
  amsg = "For 2D elements, you should specify edgeOrder and faceOrder"
  CALL AssertError1(isok, myname, amsg)

  tSize = GetTotalEdges(elemType)
  isok = SIZE(edgeOrder) .EQ. tSize
  amsg = "Size of edgeOrder is not equal to total number of edges in element."
  CALL AssertError1(isok, myname, amsg)

  IF (isQuad) THEN
    isok = SIZE(faceOrder) .EQ. xidim
    amsg = "In case of Quadrangle element size of faceOrder="// &
           tostring(SIZE(faceOrder))//" should be equal to xidim=2"
    CALL AssertError1(isok, myname, amsg)
  ELSE
    isok = SIZE(faceOrder) .EQ. 1
    amsg = "In case of Triangle element size of faceOrder="// &
           tostring(SIZE(faceOrder))//" should be equal to 1"

    CALL AssertError1(isok, myname, amsg)
  END IF

#endif

  aint = -1
  CALL Set(obj=param, prefix=prefix, key="isIsotropicOrder", &
           datatype=.FALSE., VALUE=.FALSE.)
  CALL Set(obj=param, prefix=prefix, key="order", datatype=aint, &
           VALUE=aint)

  CALL Set(obj=param, prefix=prefix, key="isAnisotropicOrder", &
           datatype=.FALSE., VALUE=.FALSE.)
  CALL Set(obj=param, prefix=prefix, key="anisoOrder", datatype=aint, &
           VALUE=aint)

  CALL Set(obj=param, prefix=prefix, key="isCellOrder", datatype=.FALSE., &
           VALUE=.FALSE.)
  CALL Set(obj=param, prefix=prefix, key="tCellOrder", datatype=0_I4B, &
           VALUE=0_I4B)
  CALL Set(obj=param, prefix=prefix, key="cellOrder", datatype=aint, &
           VALUE=aint)

  CALL Set(obj=param, prefix=prefix, key="isEdgeOrder", datatype=.TRUE., &
           VALUE=.TRUE.)
  tsize = SIZE(edgeOrder)
  CALL Set(obj=param, prefix=prefix, key="tEdgeOrder", datatype=tsize, &
           VALUE=tsize)
  CALL Set(obj=param, prefix=prefix, key="edgeOrder", datatype=edgeOrder, &
           VALUE=edgeOrder)

  CALL Set(obj=param, prefix=prefix, key="isFaceOrder", datatype=.TRUE., &
           VALUE=.TRUE.)
  tsize = SIZE(faceOrder)
  CALL Set(obj=param, prefix=prefix, key="tFaceOrder", datatype=tsize, &
           VALUE=tsize)
  CALL Set(obj=param, prefix=prefix, key="faceOrder", datatype=faceOrder, &
           VALUE=faceOrder)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetFEParam_Heirarchy2D

!----------------------------------------------------------------------------
!                                                     SetFEParam_Heirarchy2D
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_Heirarchy3D(param, elemType, nsd, isHexa, isTetra, &
                                  edgeOrder, faceOrder, cellOrder, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(IN) :: nsd
  LOGICAL(LGT), INTENT(IN) :: isHexa
  LOGICAL(LGT), INTENT(IN) :: isTetra
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
  CHARACTER(*), INTENT(IN) :: prefix

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_Heirarchy2D()"
  CHARACTER(:), ALLOCATABLE :: amsg
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: AINT(3), tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = PRESENT(edgeOrder) .AND. PRESENT(faceOrder) .AND. PRESENT(cellOrder)
  amsg = "For 3D, (edgeOrder, faceOrder, cellOrder) should be present."
  CALL AssertError1(isok, myname, amsg)

  tSize = GetTotalEdges(elemType)
  isok = SIZE(edgeOrder) .EQ. tSize
  amsg = "Size of edgeOrder is not same as the total edges in element."
  CALL AssertError1(isok, myname, amsg)

  tSize = GetTotalFaces(elemType)

  IF (isHexa) THEN
    tSize = tSize * 3
    isok = SIZE(faceOrder) .EQ. tsize
    amsg = "In Hexahedron element  size of faceOrder is not correct"
    CALL AssertError1(isok, myname, amsg)
  END IF

  IF (isTetra) THEN
    isok = SIZE(faceOrder) .EQ. tsize
    amsg = "In Tetrahedron element size of faceOrder is not correct"
    CALL AssertError1(isok, myname, amsg)
  END IF

  tsize = GetTotalCells(elemType)

  IF (isHexa) THEN
    tsize = tsize * 3
    isok = SIZE(cellOrder) .EQ. tsize
    amsg = "In Hexahedron element  size of cellOrder is not correct"
    CALL AssertError1(isok, myname, amsg)
  END IF

  IF (isTetra) THEN
    isok = SIZE(cellOrder) .EQ. tsize
    amsg = "In Tetrahedron element size of cellOrder is not correct"
    CALL AssertError1(isok, myname, amsg)
  END IF
#endif

  aint = -1
  CALL Set(obj=param, prefix=prefix, key="isIsotropicOrder", &
           datatype=.FALSE., VALUE=.FALSE.)
  CALL Set(obj=param, prefix=prefix, key="order", datatype=aint, &
           VALUE=aint)

  CALL Set(obj=param, prefix=prefix, key="isAnisotropicOrder", &
           datatype=.FALSE., VALUE=.FALSE.)
  CALL Set(obj=param, prefix=prefix, key="anisoOrder", datatype=aint, &
           VALUE=aint)

  CALL Set(obj=param, prefix=prefix, key="isEdgeOrder", datatype=.TRUE., &
           VALUE=.TRUE.)
  tsize = SIZE(edgeOrder)
  CALL Set(obj=param, prefix=prefix, key="tEdgeOrder", datatype=tsize, &
           VALUE=tsize)
  CALL Set(obj=param, prefix=prefix, key="edgeOrder", datatype=edgeOrder, &
           VALUE=edgeOrder)

  CALL Set(obj=param, prefix=prefix, key="isFaceOrder", datatype=.TRUE., &
           VALUE=.TRUE.)
  tsize = SIZE(faceOrder)
  CALL Set(obj=param, prefix=prefix, key="tFaceOrder", datatype=tsize, &
           VALUE=tsize)
  CALL Set(obj=param, prefix=prefix, key="faceOrder", datatype=faceOrder, &
           VALUE=faceOrder)

  CALL Set(obj=param, prefix=prefix, key="isCellOrder", datatype=.TRUE., &
           VALUE=.TRUE.)
  tsize = SIZE(cellOrder)
  CALL Set(obj=param, prefix=prefix, key="tCellOrder", datatype=tsize, &
           VALUE=tsize)
  CALL Set(obj=param, prefix=prefix, key="cellOrder", datatype=cellOrder, &
           VALUE=cellOrder)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetFEParam_Heirarchy3D

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
INTEGER(I4B) :: ierr, nsd, elemType, order, anisoOrder(3), &
      cellOrder(3), feType, ipType, dofType(4), transformType, basisType(3), &
                tEdgeOrder, tFaceOrder, tCellOrder, ii
INTEGER(I4B), ALLOCATABLE :: edgeOrder(:), faceOrder(:)
TYPE(String) :: baseInterpol, baseCont, refElemDomain0
REAL(DFP) :: alpha(3), beta(3), lambda(3)
LOGICAL(LGT) :: isEdgeOrder, isFaceOrder, isCellOrder, &
                isIsotropicOrder, isAnisotropicOrder
TYPE(AbstractRefElementPointer_), ALLOCATABLE :: facetElemPtrs(:)
TYPE(ParameterList_), POINTER :: sublist
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

prefix = obj%GetPrefix()
sublist => NULL()
ierr = param%GetSubList(key=prefix, sublist=sublist)

CALL obj%DEALLOCATE()
CALL obj%CheckEssentialParam(sublist)

CALL GetValue(obj=sublist, prefix=prefix, key="nsd", VALUE=nsd)

CALL GetValue(obj=sublist, prefix=prefix, key="elemType", VALUE=elemType)

CALL GetValue(obj=sublist, prefix=prefix, key="baseContinuity", &
              VALUE=baseCont)

CALL GetValue(obj=sublist, prefix=prefix, key="baseInterpolation", &
              VALUE=baseInterpol)

CALL GetValue(obj=sublist, prefix=prefix, key="feType", &
              VALUE=feType)

CALL GetValue(obj=sublist, prefix=prefix, key="ipType", &
              VALUE=ipType)

CALL GetValue(obj=sublist, prefix=prefix, key="dofType", &
              VALUE=dofType)

CALL GetValue(obj=sublist, prefix=prefix, key="transformType", &
              VALUE=transformType)

CALL GetValue(obj=sublist, prefix=prefix, key="basisType", &
              VALUE=basisType)

CALL GetValue(obj=sublist, prefix=prefix, key="alpha", &
              VALUE=alpha)

CALL GetValue(obj=sublist, prefix=prefix, key="beta", &
              VALUE=beta)

CALL GetValue(obj=sublist, prefix=prefix, key="lambda", &
              VALUE=lambda)

CALL GetValue(obj=sublist, prefix=prefix, key="refElemDomain", &
              VALUE=refElemDomain0)

CALL GetValue(obj=sublist, prefix=prefix, key="isIsotropicOrder", &
              VALUE=isIsotropicOrder)

CALL GetValue(obj=sublist, prefix=prefix, key="isAnisotropicOrder", &
              VALUE=isAnisotropicOrder)

CALL GetValue(obj=sublist, prefix=prefix, key="isEdgeOrder", &
              VALUE=isEdgeOrder)

CALL GetValue(obj=sublist, prefix=prefix, key="isFaceOrder", &
              VALUE=isFaceOrder)

CALL GetValue(obj=sublist, prefix=prefix, key="isCellOrder", &
              VALUE=isCellOrder)

IF (isIsotropicOrder) THEN
  CALL GetValue(obj=sublist, prefix=prefix, key="order", VALUE=order)
  CALL obj%SetParam(order=order, isIsotropicOrder=isIsotropicOrder)
END IF

IF (isAnisotropicOrder) THEN
 CALL GetValue(obj=sublist, prefix=prefix, key="anisoOrder", VALUE=anisoOrder)
  CALL obj%SetParam(anisoOrder=anisoOrder, &
                    isAnisotropicOrder=isAnisotropicOrder)
END IF

IF (isEdgeOrder) THEN
 CALL GetValue(obj=sublist, prefix=prefix, key="tEdgeOrder", VALUE=tEdgeOrder)
  CALL Reallocate(edgeOrder, tEdgeOrder)

  IF (tEdgeOrder .GT. 0_I4B) THEN
   CALL GetValue(obj=sublist, prefix=prefix, key="edgeOrder", VALUE=edgeOrder)
    CALL obj%SetParam(isEdgeOrder=isEdgeOrder, edgeOrder=edgeOrder, &
                      tEdgeOrder=tEdgeOrder)
  END IF
END IF

IF (isFaceOrder) THEN
 CALL GetValue(obj=sublist, prefix=prefix, key="tFaceOrder", VALUE=tFaceOrder)
  CALL Reallocate(faceOrder, tFaceOrder)

  IF (tFaceOrder .GT. 0_I4B) THEN
   CALL GetValue(obj=sublist, prefix=prefix, key="faceOrder", VALUE=faceOrder)
    CALL obj%SetParam(isFaceOrder=isFaceOrder, faceOrder=faceOrder, &
                      tFaceOrder=tFaceOrder)
  END IF
END IF

IF (isCellOrder) THEN
 CALL GetValue(obj=sublist, prefix=prefix, key="tCellOrder", VALUE=tCellOrder)

  IF (tCellOrder .GT. 0_I4B) THEN
   CALL GetValue(obj=sublist, prefix=prefix, key="cellOrder", VALUE=cellOrder)
    CALL obj%SetParam(isCellOrder=isCellOrder, cellOrder=cellOrder, &
                      tCellOrder=tCellOrder)
  END IF
END IF

!! Initiate ReferenceElement
obj%refelem => RefElement_Pointer(elemType)
!! NOTE: RefElement_Pointer is defined in RefElementFactory
CALL obj%refelem%Initiate(nsd=nsd, baseContinuity=baseCont%chars(), &
                          baseInterpolation=baseInterpol%chars())

!! Set parameters
CALL obj%SetParam(nsd=nsd, elemType=elemType, feType=feType, &
    baseContinuity=baseCont%chars(), baseInterpolation=baseInterpol%chars(), &
          refElemDomain=refElemDomain0%chars(), transformType=transformType, &
           dofType=dofType, ipType=ipType, basisType=basisType, alpha=alpha, &
                  beta=beta, lambda=lambda)

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
                        '[END] ')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                        InitiateLagrangeFE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateLagrangeFE
TYPE(ParameterList_) :: param
CHARACTER(*), PARAMETER :: myName = "obj_InitiateLagrangeFE()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

problem = baseInterpolation(1:8) .NE. "Lagrange"

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
           '[ARG ERROR] :: This routine is valid for baseInterpolation = '// &
                    'LagrangePolynomial or LagrangeInterpolation '// &
             ' given value of baseInterpolation is '//TRIM(baseInterpolation))
  RETURN
END IF

CALL param%Initiate()
CALL SetFiniteElementParam(param=param, &
                           prefix=obj%GetPrefix(), &
                           nsd=nsd, &
                           elemType=elemType, &
                           baseContinuity=baseContinuity, &
                           baseInterpolation=baseInterpolation, &
                           ipType=ipType, &
                           basisType=[basisType], &
                           alpha=[alpha], &
                           beta=[beta], &
                           lambda=[lambda], &
                           order=order)
CALL obj%Initiate(param)
CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateLagrangeFE

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
INTEGER(I4B) :: ii, elemType
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
#endif

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO ii = 1, SIZE(obj%facetElem0)
  CALL ReferenceElement_Deallocate(obj%facetElem0(ii))
END DO

CALL ReferenceElement_Deallocate(obj%refelem0)

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
