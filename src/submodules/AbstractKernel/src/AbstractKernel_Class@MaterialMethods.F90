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

SUBMODULE(AbstractKernel_Class) MaterialMethods
USE BaseMethod
USE FieldFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         AddSolidMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddSolidMaterial
CHARACTER(*), PARAMETER :: myName = "obj_AddSolidMaterial"
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

isok = ALLOCATED(obj%solidMaterialToMesh)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%solidMaterialToMesh '//  &
    & 'is not allocated!')
  RETURN
END IF

isok = ALLOCATED(obj%solidMaterial)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%solidMaterial '//  &
    & 'is not allocated!')
  RETURN
END IF

CALL AddSolidMaterial(obj=obj%solidMaterial,  &
  & tMaterials=obj%tSolidMaterials,  &
  & materialNo=materialNo,  &
  & param=param,  &
  & materialName=materialName,  &
  & region=region,  &
  & solidMaterialToMesh=obj%solidMaterialToMesh)
!! INFO: AddSolidMaterial is defined in SolidMaterial_Class

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_AddSolidMaterial

!----------------------------------------------------------------------------
!                                                   GetSolidMaterialPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSolidMaterialPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetSolidMaterialPointer"
LOGICAL(LGT) :: isok
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

ans => NULL()

isok = ALLOCATED(obj%solidMaterialToMesh)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%solidMaterialToMesh '// &
    & 'is not allocated!')
  RETURN
END IF

ans => GetSolidMaterialPointer(obj=obj%solidMaterial, materialNo=materialNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_GetSolidMaterialPointer

!----------------------------------------------------------------------------
!                                                       InitiateMassDensity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMassDensity
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMassDensity()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

isok = ALLOCATED(obj%solidMaterial)
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[NOTHING TODO] :: AbstractKernel_::obj%solidMaterial is not allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom not ASSOCIATED.')
  RETURN
END IF

tsize = obj%dom%GetTotalMesh(dim=obj%nsd)
ALLOCATE (obj%massDensity(tsize))
DO ii = 1, tsize; obj%massDensity(ii)%ptr => NULL(); END DO

CALL KernelInitiateScalarProperty(vars=obj%massDensity,  &
  & materials=obj%solidMaterial, dom=obj%dom, nnt=obj%nnt,  &
  & varname="massDensity", matid=obj%SOLID_MATERIAL_ID,  &
  & engine=obj%engine%chars())

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_InitiateMassDensity

!----------------------------------------------------------------------------
!                                               InitiateElasticityProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElasticityProperties
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElasticityProperties()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

isok = ALLOCATED(obj%solidMaterial)
IF (.NOT. isok) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[NOTHING TODO] :: AbstractKernel_::obj%solidMaterial is not allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom not ASSOCIATED.')
  RETURN
END IF

tsize = obj%dom%GetTotalMesh(dim=obj%nsd)
ALLOCATE (obj%lame_lambda(tsize))
DO ii = 1, tsize; obj%lame_lambda(ii)%ptr => NULL(); END DO

CALL KernelInitiateScalarProperty(vars=obj%lame_lambda,  &
  & materials=obj%solidMaterial, dom=obj%dom, nnt=obj%nnt,  &
  & varname="lambda", matid=obj%SOLID_MATERIAL_ID,  &
  & engine=obj%engine%chars())

tsize = obj%dom%GetTotalMesh(dim=obj%nsd)
ALLOCATE (obj%lame_mu(tsize))
DO ii = 1, tsize; obj%lame_mu(ii)%ptr => NULL(); END DO

CALL KernelInitiateScalarProperty(vars=obj%lame_mu,  &
  & materials=obj%solidMaterial, dom=obj%dom, nnt=obj%nnt,  &
  & varname="mu", matid=obj%SOLID_MATERIAL_ID,  &
  & engine=obj%engine%chars())

tsize = obj%dom%GetTotalMesh(dim=obj%nsd)
ALLOCATE (obj%Cijkl(tsize))
DO ii = 1, tsize; obj%Cijkl(ii)%ptr => NULL(); END DO

CALL KernelInitiateTensorProperty(vars=obj%Cijkl,  &
  & materials=obj%solidMaterial, dom=obj%dom, nnt=obj%nnt,  &
  & varname="Cijkl", matid=obj%SOLID_MATERIAL_ID,  &
  & engine=obj%engine%chars())

CALL KernelInitiateConstantElasticityProperties(lambda=obj%lame_lambda,  &
  & mu=obj%lame_mu, Cijkl=obj%Cijkl, dom=obj%dom,  &
  & nnt=obj%nnt, engine=obj%engine%chars())

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_InitiateElasticityProperties

!----------------------------------------------------------------------------
!                                                  InitiateConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateMaterialProperties
CHARACTER(*), PARAMETER :: myName = "obj_InitiateMaterialProperties"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by subclass')
END PROCEDURE obj_InitiateMaterialProperties

!----------------------------------------------------------------------------
!                                                             SetMassDensity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMassDensity
CHARACTER(*), PARAMETER :: myName = "obj_SetMassDensity()"

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL KernelSetScalarProperty(vars=obj%massDensity,  &
  & materials=obj%solidMaterial, dom=obj%dom, timeVec=obj%timeVec,  &
  & varname="massDensity", matid=obj%SOLID_MATERIAL_ID)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_SetMassDensity

!----------------------------------------------------------------------------
!                                                    SetElasticityProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElasticityProperties
CHARACTER(*), PARAMETER :: myName = "obj_SetElasticityProperties()"
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL KernelSetScalarProperty(vars=obj%lame_lambda,  &
  & materials=obj%solidMaterial, dom=obj%dom, timeVec=obj%timeVec,  &
  & varname="lambda", matid=obj%SOLID_MATERIAL_ID)

CALL KernelSetScalarProperty(vars=obj%lame_mu,  &
  & materials=obj%solidMaterial, dom=obj%dom, timeVec=obj%timeVec,  &
  & varname="mu", matid=obj%SOLID_MATERIAL_ID)

CALL KernelSetTensorProperty(vars=obj%Cijkl,  &
  & materials=obj%solidMaterial, dom=obj%dom, timeVec=obj%timeVec,  &
  & varname="cijkl", matid=obj%SOLID_MATERIAL_ID)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_SetElasticityProperties

!----------------------------------------------------------------------------
!                                                       SetConstantMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterialProperties
CHARACTER(*), PARAMETER :: myName = "obj_SetConstantMatProp"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is should be implemented by '// &
  & ' subclass.')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_SetMaterialProperties

!----------------------------------------------------------------------------
!                                                       SetMaterialToDomain
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterialToDomain
CHARACTER(*), PARAMETER :: myName = "obj_SetMaterialToDomain()"
INTEGER(I4B) :: ii, kk, jj, nsd
INTEGER(I4B), ALLOCATABLE :: indx(:)
LOGICAL(LGT) :: isok
CLASS(Domain_), POINTER :: dom

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

isok = ALLOCATED(obj%solidMaterialToMesh)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%solidMaterialToMesh '//  &
    & 'not allocated.')
  RETURN
END IF

isok = ASSOCIATED(obj%dom)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractKernel_::obj%dom is not ASSOCIATED.')
  RETURN
END IF

CALL MeshSelectionSet(obj%solidMaterialToMesh)

dom => obj%dom
nsd = dom%GetNSD()
DO ii = 1, nsd
  CALL dom%SetTotalMaterial(dim=ii, n=obj%tOverlappedMaterials)
  !! Add one material to all meshes of domain
  indx = dom%GetTotalMaterial(dim=ii)
  indx = indx - indx(1)
  IF (ANY(indx .NE. 0)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Some error occured.')
    RETURN
  END IF
END DO

obj%SOLID_MATERIAL_ID = dom%GetTotalMaterial(dim=nsd, entityNum=1)

DO ii = 1, obj%tSolidMaterials
  DO kk = 1, obj%nsd
    indx = obj%solidMaterialToMesh(ii)%GetMeshID(dim=kk)
    DO jj = 1, SIZE(indx)
      CALL dom%SetMaterial( &
        & dim=kk, &
        & entityNum=indx(jj), &
        & medium=obj%SOLID_MATERIAL_ID, &
        & material=ii)
    END DO
  END DO
END DO

IF (ALLOCATED(indx)) DEALLOCATE (indx)
NULLIFY (dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_SetMaterialToDomain

!----------------------------------------------------------------------------
!                                                         SetElementToMatID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElementToMatID
CHARACTER(*), PARAMETER :: myName = "obj_SetElementToMatID()"
INTEGER(I4B) :: ii
INTEGER(I4B), ALLOCATABLE :: indx(:)
#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

ii = obj%dom%GetTotalElements()
CALL Reallocate(obj%elemToMatId, ii, obj%tOverlappedMaterials)
DO ii = 1, obj%tSolidMaterials
  indx = obj%solidMaterialToMesh(ii)%GetElemNum(domain=obj%dom)
  obj%elemToMatId(indx, obj%SOLID_MATERIAL_ID) = ii
END DO
IF (ALLOCATED(indx)) DEALLOCATE (indx)

#ifdef DEBUG_VER
CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_SetElementToMatID

END SUBMODULE MaterialMethods
