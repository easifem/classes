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

SUBMODULE(AbstractFE_Class) CellDOFMethods
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString, Display
USE TomlUtility, ONLY: GetValue, GetValue_
USE tomlf, ONLY: toml_get => get_value
USE MassMatrix_Method, ONLY: MassMatrix_
USE ForceVector_Method, ONLY: ForceVector_
USE Lapack_Method, ONLY: GetLU, LUSolve, GetInvMat
USE InputUtility, ONLY: Input
USE Projection_Method, ONLY: GetL2ProjectionDOFValueFromQuadrature

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 GetDOFValueFromUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFValueFromSTFunc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDOFValueFromSTFunc()"
#endif

INTEGER(I4B) :: tVertex, nsd, tFace, tFaceDOF, iface, tCellDOF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

nsd = obj%opt%GetNSD()

CALL obj%GetVertexDOFValue(ans=ans, tsize=tVertex, func=func, xij=xij, &
                           times=times, icompo=icompo)

tsize = tsize + tVertex

tFace = obj%opt%GetTotalFace()

DO iface = 1, tFace
  CALL obj%GetFacetDOFValue( &
    elemsd=elemsd(iface), facetElemsd=facetElemsd(iface), xij=xij, &
    times=times, localFaceNumber=iface, func=func, ans=temp, &
    tsize=tFaceDOF, massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
    onlyFaceBubble=.TRUE., icompo=icompo)
  ans(tsize + 1:tsize + tFaceDOF) = temp(1:tFaceDOF)
  tsize = tsize + tFaceDOF
END DO

CALL obj%GetInCellDOFValue( &
  cellElemsd=cellElemsd, func=func, times=times, ans=ans, &
  temp=temp, tsize=tCellDOF, massMat=massMat, ipiv=ipiv, &
  funcValue=funcValue, offset=tsize, icompo=icompo)
! 1:offset are vertex, edge and face dofs
! from offset+1 inside cell dof values start

tsize = tsize + tCellDOF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDOFValueFromSTFunc

!----------------------------------------------------------------------------
!                                              GetFacetDOFValueFromQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFValueFromQuadrature
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDOFValueFromQuadrature()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: info, nrow, ncol, n1, n2, ii, nns

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

nns = elemsd%nns

#ifdef DEBUG_VER
n1 = SIZE(func)
isok = n1 .GE. elemsd%nips
CALL AssertError1(isok, myName, &
             'Size of func='//ToString(n1)//' is lesser than elemsd%nips='// &
                  ToString(elemsd%nips))
#endif

#ifdef DEBUG_VER
n1 = SIZE(ans)
isok = n1 .GE. elemsd%nns
CALL AssertError1(isok, myName, &
               'Size of ans='//ToString(n1)//' is lesser than elemsd%nns='// &
                  ToString(elemsd%nns))
#endif

#ifdef DEBUG_VER
n1 = SIZE(ipiv)
isok = n1 .GE. elemsd%nns
CALL AssertError1(isok, myName, &
              'Size of ipiv='//ToString(n1)//' is lesser than elemsd%nns='// &
                  ToString(elemsd%nns))
#endif

#ifdef DEBUG_VER
n1 = SIZE(massMat, 1)
isok = n1 .GE. elemsd%nns
CALL AssertError1(isok, myName, &
                  'Number of rows in massMat='//ToString(n1)// &
                  ' is lesser than elemsd%nns='// &
                  ToString(elemsd%nns))

n1 = SIZE(massMat, 2)
isok = n1 .GE. elemsd%nns
CALL AssertError1(isok, myName, &
                  'Number of cols in massMat='//ToString(n1)// &
                  ' is lesser than elemsd%nns='// &
                  ToString(elemsd%nns))
#endif

massMat(1:nns, 1:nns) = 0.0_DFP

n1 = 1; n2 = nns

IF (onlyInside) THEN
  n1 = tVertices + 1; n2 = nns
END IF

tsize = n2 - n1 + 1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'calling MassMatrix_...')
#endif

CALL MassMatrix_(test=elemsd, trial=elemsd, ans=massMat, &
                 nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'calling ForceVector_...')
#endif

CALL ForceVector_(test=elemsd, c=func, ans=ans, tsize=nrow)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'calling GetLU...')
#endif

CALL GetLU(A=massMat(n1:n2, n1:n2), IPIV=ipiv(n1:n2), info=info)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'calling LUSolve')
#endif

CALL LUSolve(A=massMat(n1:n2, n1:n2), B=ans(n1:n2), &
             IPIV=ipiv(n1:n2), info=info)

IF (onlyInside) THEN
  DO ii = tVertices + 1, nns
    ans(ii - tVertices) = ans(ii)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDOFValueFromQuadrature

!----------------------------------------------------------------------------
!                                                 GetVertexDOFValueFromSTFunc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVertexDOFValueFromSTFunc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVertexDOFValueFromSTFunc()"
INTEGER(I4B) :: tReturns, tArgs
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, nsd, returnType, icompo0
REAL(DFP) :: args(4), temp_ans(10)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nsd = obj%opt%GetNSD()
tsize = obj%opt%GetTotalVertex()

#ifdef DEBUG_VER
tArgs = func%GetNumArgs()
isok = tArgs .GE. 4_I4B
CALL AssertError1(isok, myName, &
           "WIP: the user function must have at least 4 arguments, (x,y,z,t)")
#endif

args(1:3) = 0.0_DFP
args(4) = times

returnType = func%GetReturnType()
tReturns = func%GetNumReturns()

SELECT CASE (returnType)
CASE (TypeFEVariableOpt%scalar)

#ifdef DEBUG_VER
  isok = tReturns .EQ. 1
  CALL AssertError1(isok, myName, &
                    "WIP: the user function must return a single value")
#endif

  DO ii = 1, tsize
    args(1:nsd) = xij(1:nsd, ii)
    CALL func%GetScalarValue(args=args, val=ans(ii))
  END DO

CASE (TypeFEVariableOpt%vector)

  icompo0 = Input(default=1_I4B, option=icompo)

#ifdef DEBUG_VER
  isok = tReturns .GE. icompo0
  CALL AssertError1(isok, myName, &
                    "WIP: the user function must return " &
                    //ToString(icompo0)//" values")
#endif

  DO ii = 1, tsize
    args(1:nsd) = xij(1:nsd, ii)
    CALL func%GetVectorValue(args=args, val=temp_ans(1:tReturns), &
                             n=tReturns)
    ans(ii) = temp_ans(icompo0)
  END DO

CASE default
#ifdef DEBUG_VER
  CALL AssertError1(.FALSE., myName, &
                    "Return type of user function must be scalar or vector")
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVertexDOFValueFromSTFunc

!----------------------------------------------------------------------------
!                                                 GetInCellDOFValueFromSTFunc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInCellDOFValueFromSTFunc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetInCellDOFValueFromSTFunc()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: mysize
#endif

INTEGER(I4B) :: ii, nips, nsd, returnType, icompo0, tReturns
REAL(DFP) :: args(4), ainterpol, temp_ans(10)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
ii = SIZE(funcValue)
isok = ii .GE. cellElemsd%nips
CALL AssertError1(isok, myName, &
    'Size of funcValue='//ToString(ii)//' is lesser than cellElemsd%nips='// &
                  ToString(cellElemsd%nips))
#endif

tsize = 0

nips = cellElemsd%nips
nsd = cellElemsd%nsd

args(1:3) = 0.0_DFP
args(4) = times

returnType = func%GetReturnType()
SELECT CASE (returnType)
CASE (TypeFEVariableOpt%scalar)
  DO ii = 1, nips
    args(1:nsd) = cellElemsd%coord(1:nsd, ii)
    CALL func%GetScalarValue(args=args, val=funcValue(ii))

    ainterpol = DOT_PRODUCT(cellElemsd%N(1:offset, ii), ans(1:offset))

    funcValue(ii) = funcValue(ii) - ainterpol
  END DO
CASE (TypeFEVariableOpt%vector)
  icompo0 = Input(default=1_I4B, option=icompo)
  tReturns = func%GetNumReturns()
  DO ii = 1, nips
    args(1:nsd) = cellElemsd%coord(1:nsd, ii)
    CALL func%GetVectorValue(args=args, val=temp_ans(1:tReturns), &
                             n=tReturns)
    funcValue(ii) = temp_ans(icompo0)

    ainterpol = DOT_PRODUCT(cellElemsd%N(1:offset, ii), ans(1:offset))

    funcValue(ii) = funcValue(ii) - ainterpol
  END DO
END SELECT

#ifdef DEBUG_VER
mysize = SIZE(temp)
isok = mysize .GE. cellElemsd%nns
CALL AssertError1(isok, myName, &
      'Size of temp='//ToString(mysize)//' is lesser than cellElemsd%nns='// &
                  ToString(cellElemsd%nns))
#endif

CALL obj%GetDOFValueFromQuadrature( &
  elemsd=cellElemsd, func=funcValue, ans=temp, tsize=tsize, &
  massMat=massMat, ipiv=ipiv, onlyInside=.TRUE., tVertices=offset)

#ifdef DEBUG_VER
mysize = SIZE(ans)
isok = mysize .GE. offset + tsize
CALL AssertError1(isok, myName, &
         'Size of ans='//ToString(mysize)//' is lesser than offset+tsize='// &
                  ToString(offset + tsize))
#endif

ans(offset + 1:offset + tsize) = temp(1:tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetInCellDOFValueFromSTFunc

!----------------------------------------------------------------------------
!                                                  GetDOFValueFromQuadrature2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFValueFromSTFunc2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDOFValueFromSTFunc2()"
#endif
INTEGER(I4B) :: tFace

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!  Getting quadrature for all facets and edges
CALL obj%GetAllFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, tsize=tFace)

! Getting quadrature value for internal cell dof
CALL obj%GetQuadraturePoints(quad=cellQuad)

! geofeptr: Get local element shape data for in facet dof
CALL geofeptr%GetAllLocalFacetElemShapeData( &
  elemsd=geoElemsd, quad=quad, facetElemsd=geoFacetElemsd, &
  facetQuad=facetQuad, tsize=tFace)

! geofeptr: Get local element shape data for in cell dof
CALL geofeptr%GetLocalElemShapeData(elemsd=geoCellElemsd, quad=cellQuad)

! feptr: Get local element shape data for in facet dof
CALL obj%GetAllLocalFacetElemShapeData( &
  elemsd=elemsd, quad=quad, facetElemsd=facetElemsd, &
  facetQuad=facetQuad, tsize=tFace)

! feptr: Get local element shape data for in cell dof
CALL obj%GetLocalElemShapeData(elemsd=cellElemsd, quad=cellQuad)

! feptr: Get global element shape data for in facet dof
CALL obj%GetAllGlobalFacetElemShapeData( &
  elemsd=elemsd, facetElemsd=facetElemsd, tsize=tFace, &
  geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, xij=xij)

! feptr: Get global element shape data for in cell dof
CALL obj%GetGlobalElemShapeData( &
  elemsd=cellElemsd, xij=xij, geoElemsd=geoCellElemsd)

CALL obj%GetDOFValue( &
  cellElemsd=cellElemsd, elemsd=elemsd, facetElemsd=facetElemsd, &
  xij=xij, times=times, func=func, ans=ans, tsize=tsize, &
  massMat=massMat, ipiv=ipiv, funcValue=funcValue, temp=temp, &
  icompo=icompo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDOFValueFromSTFunc2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE CellDOFMethods
