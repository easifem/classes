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
USE TomlUtility, ONLY: GetValue, GetValue_
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize

USE MassMatrix_Method, ONLY: MassMatrix_
USE ForceVector_Method, ONLY: ForceVector_
USE Lapack_Method, ONLY: GetLU, LUSolve, GetInvMat
USE InputUtility, ONLY: Input

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
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

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = obj2%isInit
CALL obj%opt%Copy(obj2%opt)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, msg="isInit: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL obj%opt%Display(msg="BasisOpt: ", unitno=unitno)

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

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalElemShapeData

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

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalFacetElemShapeData

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

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalFacetElemShapeData

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

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

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

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

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

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureOrder

!----------------------------------------------------------------------------
!                                                          SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureType

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                                 SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrientation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrientation()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrientation

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
!                                                      GetInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetInterpolationPoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetInterpolationPoints

!----------------------------------------------------------------------------
!                                                 GetTotalInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalInterpolationPoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalInterpolationPoints

!----------------------------------------------------------------------------
!                                                            GetRefElemCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRefElemCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRefElemCoord()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetRefElemCoord(ans=ans, nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRefElemCoord

!----------------------------------------------------------------------------
!                                              GetFacetDOFValueFromQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromQuadrature
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromQuadrature()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: info, nrow, ncol, n1, n2, ii, nns
LOGICAL(LGT) :: onlyFaceBubble0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

onlyFaceBubble0 = Input(option=onlyFaceBubble, default=.FALSE.)

#ifdef DEBUG_VER
IF (onlyFaceBubble0) THEN
  isok = PRESENT(tVertices)
  CALL AssertError1(isok, myName, &
                    'tVertices must be provided when onlyFaceBubble is true')
END IF
#endif

nns = facetElemsd%nns

#ifdef DEBUG_VER
n1 = SIZE(func)
isok = n1 .GE. facetElemsd%nns
CALL AssertError1(isok, myName, &
         'Size of func='//ToString(n1)//' is lesser than facetElemsd%nns='// &
                  ToString(facetElemsd%nns))
#endif

massMat(1:nns, 1:nns) = 0.0_DFP

n1 = 1; n2 = nns

IF (onlyFaceBubble0) THEN
  n1 = tVertices + 1; n2 = nns
END IF

tsize = n2 - n1 + 1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'calling MassMatrix_')
#endif

CALL MassMatrix_(test=facetElemsd, trial=facetElemsd, ans=massMat, &
                 nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'calling ForceVector_')
#endif

CALL ForceVector_(test=facetElemsd, c=func, ans=ans, tsize=nrow)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'calling GetLU')
#endif

CALL GetLU(A=massMat(n1:n2, n1:n2), IPIV=ipiv(n1:n2), info=info)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'calling LUSolve')
#endif

CALL LUSolve(A=massMat(n1:n2, n1:n2), B=ans(n1:n2), &
             IPIV=ipiv(n1:n2), info=info)

IF (onlyFaceBubble0) THEN
  DO ii = tVertices + 1, nns
    ans(ii - 2) = ans(ii)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromQuadrature

!----------------------------------------------------------------------------
!                                                GetFacetDOFValueFromConstant
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromConstant
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromConstant()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: info, nrow, ncol, n1, n2, ii, nns
LOGICAL(LGT) :: onlyFaceBubble0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

onlyFaceBubble0 = Input(option=onlyFaceBubble, default=.FALSE.)

#ifdef DEBUG_VER
IF (onlyFaceBubble0) THEN
  isok = PRESENT(tVertices)
  CALL AssertError1(isok, myName, &
                    'tVertices must be provided when onlyFaceBubble is true')
END IF
#endif

nns = facetElemsd%nns
massMat(1:nns, 1:nns) = 0.0_DFP
ans(1:nns) = 0.0_DFP

n1 = 1; n2 = nns
IF (onlyFaceBubble0) THEN
  n1 = tVertices + 1; n2 = nns
END IF

tsize = n2 - n1 + 1

CALL MassMatrix_(test=facetElemsd, trial=facetElemsd, ans=massMat, &
                 nrow=nrow, ncol=ncol)

CALL ForceVector_(test=facetElemsd, ans=ans, tsize=nrow)

CALL GetLU(A=massMat(n1:n2, n1:n2), IPIV=ipiv(n1:n2), info=info)

CALL LUSolve(A=massMat(n1:n2, n1:n2), B=ans(n1:n2), &
             IPIV=ipiv(n1:n2), info=info)

IF (onlyFaceBubble0) THEN
  DO ii = tVertices + 1, nns
    ans(ii - 2) = ans(ii)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromConstant

!----------------------------------------------------------------------------
!                                                 GetFacetDOFValueFromVertex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromVertex
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromVertex()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, nns, nips

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = geoFacetElemsd%nns
nips = geoFacetElemsd%nips

#ifdef DEBUG_VER
isok = SIZE(func) .GE. nns
CALL AssertError1(isok, myName, &
                  "Size of func is less than nns in geoFacetElemsd")

isok = SIZE(funcValue) .GE. nips
CALL AssertError1(isok, myName, &
                  "Size of funcValue is less than nips in facetElemsd")
#endif

! Now we will perform interpolation from vertex to quadrature points
! The result will be stored in funcValue
DO ii = 1, nips
  funcValue(ii) = DOT_PRODUCT(facetElemsd%N(1:nns, ii), func(1:nns))
END DO

CALL obj%GetFacetDOFValueFromQuadrature( &
  elemsd=elemsd, facetElemsd=facetElemsd, xij=xij, &
  localFaceNumber=localFaceNumber, func=funcValue, ans=ans, tsize=tsize, &
  massMat=massMat, ipiv=ipiv, onlyFaceBubble=onlyFaceBubble, &
  tVertices=tVertices)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromVertex

!----------------------------------------------------------------------------
!                                      GetFacetDOFValueFromSpaceUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromSpaceUserFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "obj_GetFacetDOFValueFromSpaceUserFunction()"
#endif

REAL(DFP), PARAMETER :: times = 0.0_DFP

CALL obj%GetFacetDOFValue( &
  elemsd=elemsd, facetElemsd=facetElemsd, xij=xij, times=times, &
  localFaceNumber=localFaceNumber, func=func, ans=ans, tsize=tsize, &
  massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
  onlyFaceBubble=onlyFaceBubble)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromSpaceUserFunction

!----------------------------------------------------------------------------
!                                            GetFacetDOFValueFromUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromSpaceTimeUserFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "obj_GetFacetDOFValueFromSpaceTimeUserFunction()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromSpaceTimeUserFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
