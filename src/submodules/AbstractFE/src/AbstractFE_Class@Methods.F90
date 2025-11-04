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
!                                              GetAllLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAllLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAllLocalFacetElemShapeData()"
#endif

INTEGER(I4B) :: iface

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%opt%GetTotalFace()
DO iface = 1, tsize
  CALL obj%GetLocalFacetElemShapeData( &
    elemsd=elemsd(iface), facetElemsd=facetElemsd(iface), quad=quad(iface), &
    facetQuad=facetQuad(iface), localFaceNumber=iface)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetAllLocalFacetElemShapeData

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

MODULE PROCEDURE obj_GetGlobalElemShapeData2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%GetQuadraturePoints(quad=quad)
CALL obj%GetLocalElemShapeData(elemsd=elemsd, quad=quad)
CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)
CALL obj%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
                                  geoelemsd=geoelemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData2

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
!                                             GetAllGlobalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAllGlobalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAllGlobalFacetElemShapeData()"
#endif

INTEGER(I4B) :: iface

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%opt%GetTotalFace()
DO iface = 1, tsize
  CALL obj%GetGlobalFacetElemShapeData( &
    elemsd=elemsd(iface), facetElemsd=facetElemsd(iface), &
    localFaceNumber=iface, geoElemsd=geoElemsd(iface), &
    geoFacetElemsd=geoFacetElemsd(iface), xij=xij)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetAllGlobalFacetElemShapeData

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
!                                                GetAllFacetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAllFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAllFacetQuadraturePoints()"
#endif

INTEGER(I4B) :: iface

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%opt%GetTotalFace()
DO iface = 1, tsize
  CALL obj%GetFacetQuadraturePoints( &
    quad=quad(iface), facetQuad=facetQuad(iface), localFaceNumber=iface)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetAllFacetQuadraturePoints

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
!                                            GetFacetDOFValueFromUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromSTFunc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromSTFunc()"
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
END PROCEDURE obj_GetFacetDOFValueFromSTFunc

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
                           times=times)

tsize = tsize + tVertex

tFace = obj%opt%GetTotalFace()

DO iface = 1, tFace
  CALL obj%GetFacetDOFValue( &
    elemsd=elemsd(iface), facetElemsd=facetElemsd(iface), xij=xij, &
    times=times, localFaceNumber=iface, func=func, ans=temp, &
    tsize=tFaceDOF, massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
    onlyFaceBubble=.TRUE.)
  ans(tsize + 1:tsize + tFaceDOF) = temp(1:tFaceDOF)
  tsize = tsize + tFaceDOF
END DO

CALL obj%GetInCellDOFValue( &
  cellElemsd=cellElemsd, func=func, times=times, ans=ans, &
  temp=temp, tsize=tCellDOF, massMat=massMat, ipiv=ipiv, &
  funcValue=funcValue, offset=tsize)
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
LOGICAL(LGT) :: onlyInside0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

onlyInside0 = Input(option=onlyInside, default=.FALSE.)

#ifdef DEBUG_VER
IF (onlyInside0) THEN
  isok = PRESENT(tVertices)
  CALL AssertError1(isok, myName, &
                    'tVertices must be provided when onlyInside is true')
END IF
#endif

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

IF (onlyInside0) THEN
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

IF (onlyInside0) THEN
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

INTEGER(I4B) :: ii, nsd
REAL(DFP) :: args(4)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
tReturns = func%GetNumReturns()
isok = tReturns .EQ. 1
CALL AssertError1(isok, myName, &
                  "WIP: the user function must return a single value")
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
DO ii = 1, tsize
  args(1:nsd) = xij(1:nsd, ii)
  CALL func%GetScalarValue(args=args, val=ans(ii))
END DO

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

INTEGER(I4B) :: ii, nips, nsd
REAL(DFP) :: args(4), ainterpol

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
DO ii = 1, nips
  args(1:nsd) = cellElemsd%coord(1:nsd, ii)
  CALL func%GetScalarValue(args=args, val=funcValue(ii))

  ainterpol = DOT_PRODUCT(cellElemsd%N(1:offset, ii), ans(1:offset))

  funcValue(ii) = funcValue(ii) - ainterpol
END DO

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
  massMat=massMat, ipiv=ipiv, funcValue=funcValue, temp=temp)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDOFValueFromSTFunc2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
