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

SUBMODULE(BasisOpt_Class) ParameterConstructorMethods

USE FPL_Method, ONLY: GetValue, CheckEssentialParam, Set

USE StringUtility, ONLY: UpperCase

USE InputUtility, ONLY: Input

USE InterpolationUtility, ONLY: RefElemDomain

USE ReferenceElement_Method, ONLY: ElementTopology, &
                                   XiDimension, &
                                   GetTotalEdges, &
                                   GetTotalFaces, &
                                   GetTotalCells

USE Display_Method, ONLY: ToString

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL CheckEssentialParam(obj=param, keys=BasisOptEssentialParams, &
                       prefix=obj%GetPrefix(), myName=myName, modName=modName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                           SetBasisOptParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetBasisOptParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetBasisOptParam()"
#endif

INTEGER(I4B) :: topoName, myint, myintvec(3)
CHARACTER(:), ALLOCATABLE :: baseContinuity0, baseInterpolation0, key
TYPE(String) :: astr
LOGICAL(LGT) :: isok
TYPE(ParameterList_), POINTER :: sublist

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = nsd .EQ. 1_I4B .AND. PRESENT(order)
CALL AssertError1(isok, myName, &
                  "In case of 1D elements order must be present.")
#endif

sublist => NULL()
sublist => param%NewSubList(key=prefix)

! First we set the default parameters in the sublist
CALL SetDefaultFEParam(param=sublist, prefix=prefix)

key = "nsd"
CALL Set(obj=sublist, prefix=prefix, key=key, datatype=nsd, VALUE=nsd)

key = "elemType"
CALL Set(obj=sublist, prefix=prefix, key=key, datatype=elemType, &
         VALUE=elemType)

topoName = ElementTopology(elemType)
key = "topoName"
CALL Set(obj=sublist, prefix=prefix, key=key, datatype=topoName, &
         VALUE=topoName)

baseContinuity0 = UpperCase(baseContinuity)
key = "baseContinuity"
CALL Set(obj=sublist, prefix=prefix, key=key, &
         datatype=baseContinuity0, VALUE=baseContinuity0)

baseInterpolation0 = UpperCase(baseInterpolation)
#ifdef DEBUG_VER
isok = (baseInterpolation0(1:4) .EQ. "LAGR") .AND. PRESENT(ipType)
CALL AssertError1(isok, myName, &
                  'In case of LAGRANGE polynomials ipType should be present.')
#endif
key = "baseInterpolation"
CALL Set(obj=sublist, prefix=prefix, key=key, &
         datatype=baseContinuity0, VALUE=baseInterpolation0)

! RefElemDomain is defined in InterpolationUtility_Method
astr = RefElemDomain(baseInterpol=baseInterpolation0, &
                     baseContinuity=baseContinuity0, elemType=elemType)
key = "refElemDomain"
CALL Set(obj=sublist, prefix=prefix, key=key, datatype=astr, VALUE=astr)

! If feType is not present, the following routine does nothing
! so it is safe to call it
key = "feType"
CALL Set(obj=sublist, prefix=prefix, key=key, datatype=myint, &
         VALUE=feType)

key = "dofType"
CALL Set(obj=sublist, prefix=prefix, key=key, datatype=myintvec, &
         VALUE=dofType)

key = "transformType"
CALL Set(obj=sublist, prefix=prefix, key=key, datatype=myint, &
         VALUE=transformType)

key = "ipType"
CALL Set(obj=sublist, prefix=prefix, key=key, datatype=myint, &
         VALUE=ipType)

! Here we set basisType, alpha, beta, and lambda
CALL SetFEParam_BasisType(param=sublist, basisType=basisType, &
                         alpha=alpha, beta=beta, lambda=lambda, prefix=prefix)

isok = PRESENT(order)
IF (isok) THEN
  key = "isIsotropicOrder"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=.TRUE., &
           VALUE=.TRUE.)

  key = "order"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=order, VALUE=order)

  CALL finishme
  RETURN
END IF

isok = PRESENT(anisoOrder)
IF (isok) THEN
  key = "isAnisotropicOrder"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=.TRUE., &
           VALUE=.TRUE.)

  key = "anisoOrder"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=anisoOrder, &
           VALUE=anisoOrder)

  CALL finishme
  RETURN
END IF

CALL SetFEParam_OrderForHierarchy(param=sublist, topoName=topoName, &
                                  edgeOrder=edgeOrder, faceOrder=faceOrder, &
                                  cellOrder=cellOrder, prefix=prefix)

CALL finishme

CONTAINS
SUBROUTINE finishme
  astr = ""
  baseInterpolation0 = ""
  baseContinuity0 = ""
  key = ""
  sublist => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE finishme

END PROCEDURE SetBasisOptParam

!----------------------------------------------------------------------------
!                                                       SetFEParam_BasisType
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_BasisType(param, basisType, alpha, beta, &
                                lambda, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda(:)
  CHARACTER(*), INTENT(IN) :: prefix

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_BasisType()"
#endif
  CHARACTER(:), ALLOCATABLE :: key
  INTEGER(I4B) :: myintvec(3), tsize, ii
  REAL(DFP) :: areal(3)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  key = "basisType"
  myintvec = TypeBasisOpt%basisType
  isok = PRESENT(basisType)
  IF (isok) THEN
    myintvec = basisType(1)
    tsize = MIN(SIZE(basisType), 3)
    DO ii = 2, tsize
      myintvec(ii) = basisType(ii)
    END DO
  END IF
  CALL Set(obj=param, prefix=prefix, key=key, datatype=myintvec, &
           VALUE=myintvec)

  areal = TypeBasisOpt%alpha
  isok = PRESENT(alpha)
  IF (isok) THEN
    areal = alpha(1)
    tsize = MIN(SIZE(alpha), 3)
    DO ii = 2, tsize
      areal(ii) = alpha(ii)
    END DO
  END IF
  key = "alpha"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=areal, VALUE=areal)

  areal = TypeBasisOpt%beta
  isok = PRESENT(beta)
  IF (isok) THEN
    areal = beta(1)
    tsize = MIN(SIZE(beta), 3)
    DO ii = 2, tsize
      areal(ii) = beta(ii)
    END DO
  END IF
  key = "beta"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=areal, VALUE=areal)

  areal = TypeBasisOpt%lambda
  isok = PRESENT(lambda)
  IF (isok) THEN
    areal = lambda(1)
    tsize = MIN(SIZE(lambda), 3)
    DO ii = 2, tsize
      areal(ii) = lambda(ii)
    END DO
  END IF
  key = "lambda"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=areal, VALUE=areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetFEParam_BasisType

!----------------------------------------------------------------------------
!                                                          SetFEParam_Order
!----------------------------------------------------------------------------

SUBROUTINE SetDefaultFEParam(param, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  CHARACTER(*), INTENT(IN) :: prefix

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetDefaultFEParam()"
#endif

  CHARACTER(:), ALLOCATABLE :: key
  LOGICAL(LGT), PARAMETER :: tt = .TRUE., ff = .FALSE.
  INTEGER(I4B), PARAMETER :: zz = 0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  key = "firstCall"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%firstCall, VALUE=TypeBasisOpt%firstCall)

  key = "isInit"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%isInit, VALUE=TypeBasisOpt%isInit)

  key = "isIsotropicOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%isIsotropicOrder, &
           VALUE=TypeBasisOpt%isIsotropicOrder)

  key = "isAnisotropicOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%isAnisotropicOrder, VALUE=TypeBasisOpt%isAnisotropicOrder)

  key = "isEdgeOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%isEdgeOrder, VALUE=TypeBasisOpt%isEdgeOrder)

  key = "isFaceOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%isFaceOrder, VALUE=TypeBasisOpt%isFaceOrder)

  key = "isCellOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%isCellOrder, VALUE=TypeBasisOpt%isCellOrder)

  key = "tdof"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%tdof, VALUE=TypeBasisOpt%tdof)

  key = "nsd"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%nsd, VALUE=TypeBasisOpt%nsd)

  key = "xidim"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%xidim, VALUE=TypeBasisOpt%xidim)

  key = "order"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%order, VALUE=TypeBasisOpt%order)

  key = "anisotropicOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%anisoOrder, VALUE=TypeBasisOpt%anisoOrder)

  key = "tEdgeOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%tEdgeOrder, VALUE=TypeBasisOpt%tEdgeOrder)

  key = "edgeOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%edgeOrder, VALUE=TypeBasisOpt%edgeOrder)

  key = "edgeOrient"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%edgeOrient, VALUE=TypeBasisOpt%edgeOrient)

  key = "tFaceOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%tFaceOrder, VALUE=TypeBasisOpt%tFaceOrder)

  key = "faceOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%faceOrder, VALUE=TypeBasisOpt%faceOrder)

  key = "faceOrient"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%faceOrient, VALUE=TypeBasisOpt%faceOrient)

  key = "tCellOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%tCellOrder, VALUE=TypeBasisOpt%tCellOrder)

  key = "cellOrder"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%cellOrder, VALUE=TypeBasisOpt%cellOrder)

  key = "cellOrient"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%cellOrient, VALUE=TypeBasisOpt%cellOrient)

  key = "feType"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%feType, VALUE=TypeBasisOpt%feType)

  key = "topoName"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%topoName, VALUE=TypeBasisOpt%topoName)

  key = "elemType"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%elemType, VALUE=TypeBasisOpt%elemType)

  key = "ipType"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%ipType, VALUE=TypeBasisOpt%ipType)

  key = "dofType"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%dofType, VALUE=TypeBasisOpt%dofType)

  key = "transformType"
  CALL Set(obj=param, prefix=prefix, key=key, &
        datatype=TypeBasisOpt%transformType, VALUE=TypeBasisOpt%transformType)

  key = "basisType"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%basisType, VALUE=TypeBasisOpt%basisType)

  key = "alpha"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%alpha, VALUE=TypeBasisOpt%alpha)

  key = "beta"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%beta, VALUE=TypeBasisOpt%beta)

  key = "lambda"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%lambda, VALUE=TypeBasisOpt%lambda)

  key = "refelemDomain"
  CALL Set(obj=param, prefix=prefix, key=key, &
        datatype=TypeBasisOpt%refelemDomain, VALUE=TypeBasisOpt%refelemDomain)

  key = "baseContinuity"
  CALL Set(obj=param, prefix=prefix, key=key, &
      datatype=TypeBasisOpt%baseContinuity, VALUE=TypeBasisOpt%baseContinuity)

  key = "baseInterpolation"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%baseInterpolation, &
           VALUE=TypeBasisOpt%baseInterpolation)

  key = "basisType_char"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%basisType_char, &
           VALUE=TypeBasisOpt%basisType_char)

  key = "ipType_char"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%ipType_char, &
           VALUE=TypeBasisOpt%ipType_char)

  key = "feType_char"
  CALL Set(obj=param, prefix=prefix, key=key, &
           datatype=TypeBasisOpt%feType_char, &
           VALUE=TypeBasisOpt%feType_char)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetDefaultFEParam

!----------------------------------------------------------------------------
!                                               SetFEParam_OrderForHierarchy
!----------------------------------------------------------------------------

SUBROUTINE SetFEParam_OrderForHierarchy(param, topoName, edgeOrder, &
                                        faceOrder, cellOrder, prefix)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  INTEGER(I4B), INTENT(IN) :: topoName
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
  CHARACTER(*), INTENT(IN) :: prefix

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetFEParam_Heirarchy2D()"
#endif
  INTEGER(I4B) :: tsize, tsize2, jj, ii, tedges, tfaces, &
                  edgeOrder0(eleminfo%maxEdges), &
                  faceOrder0(3, eleminfo%maxFaces), &
                  cellOrder0(3)
  LOGICAL(LGT) :: isEdgeOrder, isFaceOrder, isCellOrder
  CHARACTER(:), ALLOCATABLE :: key

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isEdgeOrder = PRESENT(edgeOrder)
  isFaceOrder = PRESENT(faceOrder)
  isCellOrder = PRESENT(cellOrder)

  key = "isEdgeOrder"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=isEdgeOrder, &
           VALUE=isEdgeOrder)

  IF (isEdgeOrder) THEN
    tedges = GetTotalEdges(topoName)
    key = "tEdgeOrder"
    CALL Set(obj=param, prefix=prefix, key=key, datatype=tedges, &
             VALUE=tedges)

    tsize = MIN(SIZE(edgeOrder), tedges)
    edgeOrder0 = edgeOrder(1)
    DO ii = 2, tsize
      edgeOrder0(ii) = edgeOrder(ii)
    END DO

    key = "edgeOrder"
    CALL Set(obj=param, prefix=prefix, key=key, datatype=edgeOrder0, &
             VALUE=edgeOrder0)

  END IF

  key = "isFaceOrder"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=isFaceOrder, &
           VALUE=isFaceOrder)
  IF (isFaceOrder) THEN
    tfaces = GetTotalFaces(topoName)
    key = "tFaceOrder"
    CALL Set(obj=param, prefix=prefix, key=key, datatype=tfaces, &
             VALUE=tfaces)

    tsize2 = MIN(SIZE(faceOrder, 1), 3)
    tsize = MIN(SIZE(faceOrder, 2), tfaces)
    faceOrder0 = faceOrder(1, 1)
    DO ii = 1, tsize2
      DO jj = 1, tsize
        faceOrder0(ii, jj) = faceOrder(ii, jj)
      END DO
    END DO

    key = "faceOrder"
    CALL Set(obj=param, prefix=prefix, key=key, datatype=faceOrder0, &
             VALUE=faceOrder0)
  END IF

  key = "isCellOrder"
  CALL Set(obj=param, prefix=prefix, key=key, datatype=isCellOrder, &
           VALUE=isCellOrder)
  IF (isCellOrder) THEN
    tsize = 3
    key = "tCellOrder"
    CALL Set(obj=param, prefix=prefix, key=key, datatype=tsize, &
             VALUE=tsize)

    tsize = MIN(SIZE(cellOrder), 3)
    cellOrder0 = cellOrder(1)
    DO ii = 1, tsize
      cellOrder0(ii) = cellOrder(ii)
    END DO

    key = "cellOrder"
    CALL Set(obj=param, prefix=prefix, key=key, datatype=cellOrder0, &
             VALUE=cellOrder0)
  END IF

  key = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetFEParam_OrderForHierarchy

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
INTEGER(I4B) :: ierr, nsd, elemType, order, anisoOrder(3), &
      cellOrder(3), feType, ipType, dofType(4), transformType, basisType(3), &
                tEdgeOrder, tFaceOrder, tCellOrder
INTEGER(I4B), ALLOCATABLE :: edgeOrder(:), faceOrder(:, :)
TYPE(String) :: baseInterpol, baseCont, refElemDomain0
REAL(DFP) :: alpha(3), beta(3), lambda(3)
LOGICAL(LGT) :: isEdgeOrder, isFaceOrder, isCellOrder, &
                isIsotropicOrder, isAnisotropicOrder, isok
TYPE(ParameterList_), POINTER :: sublist
CHARACTER(:), ALLOCATABLE :: prefix, key

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

prefix = obj%GetPrefix()
sublist => NULL()
ierr = param%GetSubList(key=prefix, sublist=sublist)

#ifdef DEBUG_VER
isok = ierr .EQ. 0_I4B
CALL AssertError1(isok, myName, &
                  "Error in getting sublist from parameter list.")

isok = ASSOCIATED(sublist)
CALL AssertError1(isok, myName, &
                  "sublist is not associated.")
#endif

CALL obj%DEALLOCATE()

CALL obj%CheckEssentialParam(sublist)

key = "firstCall"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%firstCall)

key = "isInit"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%isInit)

key = "isIsotropicOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%isIsotropicOrder)

key = "isAnisotropicOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%isAnisotropicOrder)

key = "isEdgeOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%isEdgeOrder)

key = "isFaceOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%isFaceOrder)

key = "isCellOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%isCellOrder)

key = "tdof"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%tdof)

key = "nsd"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%nsd)

key = "xidim"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%xidim)

key = "order"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%order)

key = "anisotropicOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%anisoOrder)

key = "tEdgeOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%tEdgeOrder)

key = "edgeOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%edgeOrder)

key = "edgeOrient"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%edgeOrient)

key = "tFaceOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%tFaceOrder)

key = "faceOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%faceOrder)

key = "faceOrient"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%faceOrient)

key = "tCellOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%tCellOrder)

key = "cellOrder"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%cellOrder)

key = "cellOrient"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%cellOrient)

key = "feType"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%feType)

key = "topoName"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%topoName)

key = "elemType"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%elemType)

key = "ipType"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%ipType)

key = "dofType"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%dofType)

key = "transformType"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%transformType)

key = "basisType"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%basisType)

key = "alpha"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%alpha)

key = "beta"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%beta)

key = "lambda"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%lambda)

key = "refelemDomain"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%refelemDomain)

key = "baseContinuity"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%baseContinuity)

key = "baseInterpolation"
CALL GetValue(obj=param, prefix=prefix, key=key, VALUE=obj%baseInterpolation)

obj%isInit = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                              Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ParameterConstructorMethods
