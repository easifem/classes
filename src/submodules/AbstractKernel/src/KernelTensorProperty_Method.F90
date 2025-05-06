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

MODULE KernelTensorProperty_Method
USE GlobalData, ONLY: LGT, I4B, DFP

USE Display_Method, ONLY: ToString

USE FEDomain_Class, ONLY: FEDomain_, FEDomainPointer_

USE AbstractMeshField_Class, ONLY: AbstractTensorMeshField_, &
                                   AbstractTensorMeshFieldPointer_

USE AbstractMesh_Class, ONLY: AbstractMesh_

USE ExceptionHandler_Class, ONLY: e

USE TensorMeshField_Class, ONLY: SetTensorMeshFieldParam

USE STTensorMeshField_Class, ONLY: SetSTTensorMeshFieldParam

USE FPL, ONLY: ParameterList_

USE String_Class, ONLY: String

USE SolidMaterial_Class, ONLY: SolidMaterial_, SolidMaterialPointer_

USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE

PRIVATE

PUBLIC :: KernelInitiateTensorProperty
PUBLIC :: KernelSetTensorProperty

CHARACTER(*), PARAMETER :: modName = "KernelTensorProperty_Method"

INTERFACE KernelInitiateTensorProperty
  MODULE PROCEDURE KernelInitiateTensorProperty1
  MODULE PROCEDURE KernelInitiateTensorProperty2
  MODULE PROCEDURE KernelInitiateTensorProperty3
  MODULE PROCEDURE KernelInitiateTensorProperty4
END INTERFACE KernelInitiateTensorProperty

INTERFACE KernelSetTensorProperty
  MODULE PROCEDURE KernelSetTensorProperty1
END INTERFACE KernelSetTensorProperty

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateTensorProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateTensorProperty1(prop, dom, maxNNS, maxNNT, &
                   propname, engine, fieldType, varType, defineOn, dim1, dim2)

  CLASS(AbstractTensorMeshField_), INTENT(INOUT) :: prop
  !! Tensor mesh field
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  !! finite element domain
  INTEGER(I4B), INTENT(IN) :: maxNNS
  !! maximum number of nodes in space-element
  !! it can be maximum number of quadrature points used in mesh
  INTEGER(I4B), INTENT(IN) :: maxNNT
  !! maximum number of nodes in time-element
  !! it can be maximum number of quadrature points used in mesh (time domain)
  CHARACTER(*), INTENT(IN) :: propname
  !! name of the property
  CHARACTER(*), INTENT(IN) :: engine
  !! engine of the field
  INTEGER(I4B), INTENT(IN) :: fieldType
  !! field Type
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: varType
  !! varType
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: defineOn
  !! Defined on
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: dim1
  !! dimension 1 of tensor property
  INTEGER(I4B), INTENT(IN) :: dim2
  !! dimension 2 of tensor property

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTensorProperty1()"
  LOGICAL(LGT) :: isok
  CHARACTER(:), ALLOCATABLE :: errmsg
  CLASS(AbstractMesh_), POINTER :: mesh
  TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  mesh => dom%GetMeshPointer()
  isok = ASSOCIATED(mesh)
  errmsg = 'mesh is not ASSOCIATED.'
  CALL AssertError1(isok, myname, errmsg)

  isok = .NOT. mesh%isEmpty()
  errmsg = "mesh is EMPTY"
  CALL AssertError1(isok, myname, errmsg)

  CALL param%Initiate()

  IF (maxNNT .GT. 1) THEN

    CALL SetSTTensorMeshFieldParam(param=param, name=propname, &
                        fieldType=fieldType, varType=varType, engine=engine, &
              defineOn=defineOn, nns=maxNNS, nnt=maxNNT, dim1=dim1, dim2=dim2)

  ELSE

    CALL SetTensorMeshFieldParam(param=param, name=propname, &
                        fieldType=fieldType, varType=varType, engine=engine, &
                          defineOn=defineOn, nns=maxNNS, dim1=dim1, dim2=dim2)

  END IF

  CALL prop%Initiate(param=param, mesh=mesh)

  CALL param%DEALLOCATE()

  NULLIFY (mesh)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE KernelInitiateTensorProperty1

!----------------------------------------------------------------------------
!                                                 InitiateTensorProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateTensorProperty2(prop, dom, maxNNS, maxNNT, &
                   propname, engine, fieldType, varType, defineOn, dim1, dim2)

  CLASS(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: prop(:)
  !! Tensor mesh field
  TYPE(FEDomainPointer_), INTENT(INOUT) :: dom(:)
  !! finite element domain
  INTEGER(I4B), INTENT(IN) :: maxNNS(:)
  !! maximum number of nodes in space-element
  !! it can be maximum number of quadrature points used in mesh
  INTEGER(I4B), INTENT(IN) :: maxNNT(:)
  !! maximum number of nodes in time-element
  !! it can be maximum number of quadrature points used in mesh (time domain)
  TYPE(String), INTENT(IN) :: propname(:)
  !! name of the property
  CHARACTER(*), INTENT(IN) :: engine
  !! engine of the field
  INTEGER(I4B), INTENT(IN) :: fieldType(:)
  !! field Type
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: varType(:)
  !! varType
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: defineOn(:)
  !! Defined on
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: dim1(:)
  !! dimension 1 of tensor property
  INTEGER(I4B), INTENT(IN) :: dim2(:)
  !! dimension 2 of tensor property

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTensorProperty2()"
  INTEGER(I4B) :: ii, tsize, a, b

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  a = SIZE(prop)
  b = SIZE(dom)
  CALL AssertError2(a, b, myname, 'size of prop and dom are not same')

  b = SIZE(maxNNS)
  CALL AssertError2(a, b, myname, 'size of prop and maxNNS are not same')

  b = SIZE(maxNNT)
  CALL AssertError2(a, b, myname, 'size of prop and maxNNT are not same')

  b = SIZE(propname)
  CALL AssertError2(a, b, myname, 'size of prop and propname are not same')

  b = SIZE(fieldType)
  CALL AssertError2(a, b, myname, 'size of prop and fieldType are not same')

  b = SIZE(varType)
  CALL AssertError2(a, b, myname, 'size of prop and varType are not same')

  b = SIZE(defineOn)
  CALL AssertError2(a, b, myname, 'size of prop and defineOn are not same')

  b = SIZE(dim1)
  CALL AssertError2(a, b, myname, 'size of prop and dim1 are not same')

  b = SIZE(dim2)
  CALL AssertError2(a, b, myname, 'size of prop and dim2 are not same')

  tsize = SIZE(prop)

  DO ii = 1, tsize
    CALL KernelInitiateTensorProperty1(prop=prop(ii)%ptr, dom=dom(ii)%ptr, &
                                       maxNNS=maxNNS(ii), maxNNT=maxNNT(ii), &
                               propname=propname(ii)%chars(), engine=engine, &
                               fieldType=fieldType(ii), varType=varType(ii), &
                          defineOn=defineOn(ii), dim1=dim1(ii), dim2=dim2(ii))
  END DO

END SUBROUTINE KernelInitiateTensorProperty2

!----------------------------------------------------------------------------
!                                              KernelInitiateTensorProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateTensorProperty3(prop, dom, maxNNS, maxNNT, &
                   propname, engine, fieldType, varType, defineOn, dim1, dim2)

  CLASS(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: prop(:)
  !! Tensor mesh field
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  !! finite element domain
  INTEGER(I4B), INTENT(IN) :: maxNNS(:)
  !! maximum number of nodes in space-element
  !! it can be maximum number of quadrature points used in mesh
  INTEGER(I4B), INTENT(IN) :: maxNNT(:)
  !! maximum number of nodes in time-element
  !! it can be maximum number of quadrature points used in mesh (time domain)
  TYPE(String), INTENT(IN) :: propname(:)
  !! name of the property
  CHARACTER(*), INTENT(IN) :: engine
  !! engine of the field
  INTEGER(I4B), INTENT(IN) :: fieldType(:)
  !! field Type
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: varType(:)
  !! varType
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: defineOn(:)
  !! Defined on
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: dim1(:)
  !! dimension 1 of tensor property
  INTEGER(I4B), INTENT(IN) :: dim2(:)
  !! dimension 2 of tensor property

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTensorProperty3()"
  INTEGER(I4B) :: ii, tsize, a, b

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  a = SIZE(prop)
  b = SIZE(maxNNS)
  CALL AssertError2(a, b, myname, 'size of prop and maxNNS are not same')

  b = SIZE(maxNNT)
  CALL AssertError2(a, b, myname, 'size of prop and maxNNT are not same')

  b = SIZE(propname)
  CALL AssertError2(a, b, myname, 'size of prop and propname are not same')

  b = SIZE(fieldType)
  CALL AssertError2(a, b, myname, 'size of prop and fieldType are not same')

  b = SIZE(varType)
  CALL AssertError2(a, b, myname, 'size of prop and varType are not same')

  b = SIZE(defineOn)
  CALL AssertError2(a, b, myname, 'size of prop and defineOn are not same')

  b = SIZE(dim1)
  CALL AssertError2(a, b, myname, 'size of prop and dim1 are not same')

  b = SIZE(dim2)
  CALL AssertError2(a, b, myname, 'size of prop and dim2 are not same')

  tsize = SIZE(prop)

  DO ii = 1, tsize
    CALL KernelInitiateTensorProperty1(prop=prop(ii)%ptr, dom=dom, &
                                       maxNNS=maxNNS(ii), maxNNT=maxNNT(ii), &
                               propname=propname(ii)%chars(), engine=engine, &
                               fieldType=fieldType(ii), varType=varType(ii), &
                          defineOn=defineOn(ii), dim1=dim1(ii), dim2=dim2(ii))
  END DO

END SUBROUTINE KernelInitiateTensorProperty3

!----------------------------------------------------------------------------
!                                               KernelInitiateTensorProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateTensorProperty4(prop, dom, maxNNS, maxNNT, &
                   propname, engine, fieldType, varType, defineOn, dim1, dim2)

  CLASS(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: prop(:)
  !! Tensor mesh field
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  !! finite element domain
  INTEGER(I4B), INTENT(IN) :: maxNNS
  !! maximum number of nodes in space-element
  !! it can be maximum number of quadrature points used in mesh
  INTEGER(I4B), INTENT(IN) :: maxNNT
  !! maximum number of nodes in time-element
  !! it can be maximum number of quadrature points used in mesh (time domain)
  TYPE(String), INTENT(IN) :: propname(:)
  !! name of the property
  CHARACTER(*), INTENT(IN) :: engine
  !! engine of the field
  INTEGER(I4B), INTENT(IN) :: fieldType
  !! field Type
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: varType
  !! varType
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: defineOn
  !! Defined on
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: dim1
  !! dimension 1 of tensor property
  INTEGER(I4B), INTENT(IN) :: dim2
  !! dimension 2 of tensor property

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTensorProperty4()"
  INTEGER(I4B) :: ii, tsize, a, b

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  a = SIZE(prop)
  b = SIZE(propname)
  CALL AssertError2(a, b, myname, 'size of prop and propname are not same')

  tsize = SIZE(prop)

  DO ii = 1, tsize
    CALL KernelInitiateTensorProperty1(prop=prop(ii)%ptr, dom=dom, &
                                       maxNNS=maxNNS, maxNNT=maxNNT, &
                               propname=propname(ii)%chars(), engine=engine, &
                                       fieldType=fieldType, varType=varType, &
                                      defineOn=defineOn, dim1=dim1, dim2=dim2)
  END DO

END SUBROUTINE KernelInitiateTensorProperty4

!----------------------------------------------------------------------------
!                                                         SetScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelSetTensorProperty1(prop, materials, dom, propname, &
                                    medium, times)
  TYPE(AbstractTensorMeshField_), INTENT(INOUT) :: prop
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  CHARACTER(*), INTENT(IN) :: propname
  INTEGER(I4B), INTENT(IN) :: medium
  REAL(DFP), INTENT(IN) :: times(:)

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelSetTensorProperty1()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: nsd, telem, iel, matid1, matid2
  CLASS(AbstractMesh_), POINTER :: mesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(UserFunction_), POINTER :: func

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nsd = dom%GetNSD()
  mesh => dom%GetMeshPointer(dim=nsd)
  telem = mesh%GetTotalElements()

  isok = .NOT. mesh%isEmpty()
  CALL AssertError1(isok, myname, 'mesh is EMPTY')

  matid1 = mesh%GetMaterial(globalElement=1, islocal=.TRUE., medium=medium)

#ifdef DEBUG_VER
  isok = matid1 .NE. 0_I4B
  CALL AssertError1(isok, myname, 'matid1 is zero')
#endif

  material => materials(matid1)%ptr

#ifdef DEBUG_VER
  isok = ASSOCIATED(material)
  CALL AssertError1(isok, myname, 'material is not associated')

  isok = material%IsMaterialPresent(name=propname)

  CALL AssertError1(isok, myname, 'material is not present')
#endif

  func => material%GetMaterialPointer(propname)
  ! check if func is associated

#ifdef DEBUG_VER
  isok = ASSOCIATED(func)
  CALL AssertError1(isok, myname, 'func is not associated')
#endif

  DO iel = 1, telem
   matid2 = mesh%GetMaterial(globalElement=iel, islocal=.TRUE., medium=medium)
    !! check if matid is not zero

#ifdef DEBUG_VER
    isok = matid2 .NE. 0_I4B
    CALL AssertError1(isok, myname, 'matid is zero')
#endif

    IF (matid2 .NE. matid1) THEN

      material => materials(matid2)%ptr

#ifdef DEBUG_VER
      isok = ASSOCIATED(material)
      CALL AssertError1(isok, myname, 'material is not associated')

      isok = material%IsMaterialPresent(name=propname)

      CALL AssertError1(isok, myname, 'material is not present')
#endif

      func => material%GetMaterialPointer(propname)
      ! check if func is associated

#ifdef DEBUG_VER
      isok = ASSOCIATED(func)
      CALL AssertError1(isok, myname, 'func is not associated')
#endif

    END IF

    CALL prop%Set(func=func, times=times, globalElement=iel, &
                  islocal=.TRUE.)

    matid1 = matid2

  END DO

  NULLIFY (mesh, material, func)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE KernelSetTensorProperty1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END MODULE KernelTensorProperty_Method
