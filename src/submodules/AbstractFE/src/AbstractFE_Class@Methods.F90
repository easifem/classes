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
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: ToString, Display
USE BasisOpt_Class, ONLY: SetBasisOptParam
USE TomlUtility, ONLY: GetValue, GetValue_
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%CheckEssentialParam(param=param)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                       SetAbstractFEParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractFEParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetAbstractFEParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SetBasisOptParam(param=param, &
                      prefix=prefix, &
                      nsd=nsd, &
                      elemType=elemType, &
                      baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      ipType=ipType, &
                      basisType=basisType, &
                      alpha=alpha, &
                      beta=beta, &
                      lambda=lambda, &
                      order=order, &
                      anisoOrder=anisoOrder, &
                      edgeOrder=edgeOrder, &
                      faceOrder=faceOrder, &
                      cellOrder=cellOrder, &
                      fetype=fetype, &
                      dofType=dofType, &
                      transformType=transformType)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetAbstractFEParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL obj%opt%Initiate(param=param)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .TRUE.
CALL obj%opt%Initiate(elemType=elemType, &
                      nsd=nsd, &
                      baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      ipType=ipType, &
                      basisType=basisType, &
                      alpha=alpha, &
                      beta=beta, &
                      lambda=lambda, &
                      fetype=fetype, &
                      dofType=dofType, &
                      transformType=transformType, &
                      order=order, &
                      anisoOrder=anisoOrder, &
                      cellOrder=cellOrder, &
                      faceOrder=faceOrder, &
                      edgeOrder=edgeOrder, &
                      cellOrient=cellOrient, &
                      faceOrient=faceOrient, &
                      edgeOrient=edgeOrient, &
                      tcell=tcell, &
                      tface=tface, &
                      tedge=tedge, &
                      errCheck=errCheck, &
                      quadratureIsHomogeneous=quadratureIsHomogeneous, &
                      quadratureType=quadratureType, &
                      quadratureOrder=quadratureOrder, &
                      quadratureIsOrder=quadratureIsOrder, &
                      quadratureNips=quadratureNips, &
                      quadratureIsNips=quadratureIsNips, &
                      quadratureAlpha=quadratureAlpha, &
                      quadratureBeta=quadratureBeta, &
                      quadratureLambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = obj2%isInit
CALL obj%opt%Copy(obj2%opt)

isok = ALLOCATED(obj2%coeff)
IF (isok) THEN
  obj%coeff = obj2%coeff
END IF

isok = ALLOCATED(obj2%xij)
IF (isok) THEN
  obj%xij = obj2%xij
END IF

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

obj%isInit = .FALSE.
IF (ALLOCATED(obj%coeff)) DEALLOCATE (obj%coeff)
IF (ALLOCATED(obj%xij)) DEALLOCATE (obj%xij)
CALL obj%opt%DEALLOCATE()

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
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, msg="isInit: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL obj%opt%Display(msg="BasisOpt: ", unitno=unitno)

isok = ALLOCATED(obj%coeff)
CALL Display(isok, msg="obj%coeff allocated: ", unitno=unitno)
IF (isok) THEN
  CALL Display(SHAPE(obj%coeff), msg="obj%coeff shape: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%xij)
CALL Display(isok, msg="obj%xij allocated: ", unitno=unitno)
IF (isok) THEN
  CALL Display(SHAPE(obj%xij), msg="obj%xij shape: ", unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MdEncode
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MdEncode()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%opt%MdEncode()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_MdEncode

!----------------------------------------------------------------------------
!                                                                MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReactEncode
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReactEncode()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%opt%ReactEncode()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ReactEncode

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .TRUE.
CALL obj%opt%ImportFromToml(table=table, elemType=elemType, nsd=nsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node, elemType=elemType, nsd=nsd)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                                GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetParam(nsd=nsd, &
                      order=order, &
                      anisoOrder=anisoOrder, &
                      edgeOrder=edgeOrder, &
                      faceOrder=faceOrder, &
                      cellOrder=cellOrder, &
                      fetype=fetype, &
                      elemType=elemType, &
                      ipType=ipType, &
                      basisType=basisType, &
                      topoType=topoType, &
                      elemIndx=elemIndx, &
                      alpha=alpha, &
                      beta=beta, &
                      lambda=lambda, &
                      dofType=dofType, &
                      transformType=transformType, &
                      refElemDomain=refElemDomain, &
                      baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      isIsotropicOrder=isIsotropicOrder, &
                      isAnisotropicOrder=isAnisotropicOrder, &
                      isEdgeOrder=isEdgeOrder, &
                      isFaceOrder=isFaceOrder, &
                      isCellOrder=isCellOrder, &
                      tEdgeOrder=tEdgeOrder, &
                      tFaceOrder=tFaceOrder, &
                      tCellOrder=tCellOrder, &
                      quadratureIsHomogeneous=quadratureIsHomogeneous, &
                      quadratureType=quadratureType, &
                      quadratureOrder=quadratureOrder, &
                      quadratureNips=quadratureNips, &
                      quadratureAlpha=quadratureAlpha, &
                      quadratureBeta=quadratureBeta, &
                      quadratureLambda=quadratureLambda, &
                      quadratureIsOrder=quadratureIsOrder, &
                      quadratureIsNips=quadratureIsNips)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                            GetTopologyType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTopologyType
ans = obj%opt%GetTopologyType()
END PROCEDURE obj_GetTopologyType

!----------------------------------------------------------------------------
!                                                     GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetLocalElemShapeData(elemsd=elemsd, quad=quad, coeff=obj%coeff)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
                                    geoelemsd=geoelemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                                 GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalFacetElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetLocalFacetElemShapeData(cellElemsd=cellElemsd, &
                                        facetElemsd=facetElemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                         GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                         GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetFacetQuadraturePoints(quad=quad, facetQuad=facetQuad, &
                                      localFaceNumber=localFaceNumber)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetQuadraturePoints

!----------------------------------------------------------------------------
!                                                    GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%opt%GetTotalQuadraturePoints()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                                          SetQuadratureOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%SetQuadratureOrder(order=order, order1=order1, order2=order2, &
                                order3=order3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureOrder

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%SetParam(nsd=nsd, &
                      order=order, &
                      anisoOrder=anisoOrder, &
                      edgeOrder=edgeOrder, &
                      faceOrder=faceOrder, &
                      cellOrder=cellOrder, &
                      fetype=fetype, &
                      elemType=elemType, &
                      topoType=topoType, &
                      elemIndx=elemIndx, &
                      ipType=ipType, &
                      basisType=basisType, &
                      alpha=alpha, &
                      beta=beta, &
                      lambda=lambda, &
                      dofType=dofType, &
                      transformType=transformType, &
                      refElemDomain=refElemDomain, &
                      baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      isIsotropicOrder=isIsotropicOrder, &
                      isAnisotropicOrder=isAnisotropicOrder, &
                      isEdgeOrder=isEdgeOrder, &
                      isFaceOrder=isFaceOrder, &
                      isCellOrder=isCellOrder, &
                      tEdgeOrder=tEdgeOrder, &
                      tFaceOrder=tFaceOrder, &
                      tCellOrder=tCellOrder, &
                      quadratureIsHomogeneous=quadratureIsHomogeneous, &
                      quadratureType=quadratureType, &
                      quadratureType1=quadratureType1, &
                      quadratureType2=quadratureType2, &
                      quadratureType3=quadratureType3, &
                      quadratureOrder=quadratureOrder, &
                      quadratureOrder1=quadratureOrder1, &
                      quadratureOrder2=quadratureOrder2, &
                      quadratureOrder3=quadratureOrder3, &
                      quadratureNips=quadratureNips, &
                      quadratureNips1=quadratureNips1, &
                      quadratureNips2=quadratureNips2, &
                      quadratureNips3=quadratureNips3, &
                      quadratureAlpha=quadratureAlpha, &
                      quadratureAlpha1=quadratureAlpha1, &
                      quadratureAlpha2=quadratureAlpha2, &
                      quadratureAlpha3=quadratureAlpha3, &
                      quadratureBeta=quadratureBeta, &
                      quadratureBeta1=quadratureBeta1, &
                      quadratureBeta2=quadratureBeta2, &
                      quadratureBeta3=quadratureBeta3, &
                      quadratureLambda=quadratureLambda, &
                      quadratureLambda1=quadratureLambda1, &
                      quadratureLambda2=quadratureLambda2, &
                      quadratureLambda3=quadratureLambda3, &
                      quadratureIsOrder=quadratureIsOrder, &
                      quadratureIsNips=quadratureIsNips)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

INTEGER(I4B) :: tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%SetOrder(order=order, &
                      anisoOrder=anisoOrder, &
                      cellOrder=cellOrder, &
                      faceOrder=faceOrder, &
                      edgeOrder=edgeOrder, &
                      cellOrient=cellOrient, &
                      faceOrient=faceOrient, &
                      edgeOrient=edgeOrient, &
                      errCheck=errCheck, &
                      tcell=tcell, &
                      tface=tface, &
                      tedge=tedge)

tdof = obj%opt%GetTotalDOF()

CALL Reallocate(obj%coeff, tdof, tdof, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL Reallocate(obj%xij, 3, tdof, isExpand=.TRUE., expandFactor=2_I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                     GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseInterpolation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseInterpolation()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%opt%GetBaseInterpolation()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                          GetBaseContinuity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseContinuity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseContinuity()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%opt%GetBaseContinuity()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetBaseContinuity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
