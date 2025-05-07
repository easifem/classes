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
!

MODULE KernelScalarProperty_Method
USE GlobalData, ONLY: LGT, I4B, DFP

USE String_Class, ONLY: String

USE Display_Method, ONLY: ToString

USE FEDomain_Class, ONLY: FEDomain_, FEDomainPointer_

USE AbstractMeshField_Class, ONLY: AbstractScalarMeshField_, &
                                   AbstractScalarMeshFieldPointer_

USE AbstractMesh_Class, ONLY: AbstractMesh_

USE SolidMaterial_Class, ONLY: SolidMaterialPointer_, &
                               SolidMaterial_

USE ExceptionHandler_Class, ONLY: e

USE ScalarMeshField_Class, ONLY: SetScalarMeshFieldParam
USE STScalarMeshField_Class, ONLY: SetSTScalarMeshFieldParam

USE FPL, ONLY: ParameterList_

USE UserFunction_Class, ONLY: UserFunction_

PRIVATE

PUBLIC :: KernelInitiateScalarProperty
PUBLIC :: KernelSetScalarProperty

CHARACTER(*), PARAMETER :: modName = "KernelScalarProperty_Method"

INTERFACE KernelInitiateScalarProperty
  MODULE PROCEDURE KernelInitiateScalarProperty1
  MODULE PROCEDURE KernelInitiateScalarProperty2
  MODULE PROCEDURE KernelInitiateScalarProperty3
  MODULE PROCEDURE KernelInitiateScalarProperty4
END INTERFACE KernelInitiateScalarProperty

INTERFACE KernelSetScalarProperty
  MODULE PROCEDURE KernelSetScalarProperty1
END INTERFACE KernelSetScalarProperty

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateScalarProperty1(prop, dom, maxNNS, maxNNT, &
                               propname, engine, fieldType, varType, defineOn)
  CLASS(AbstractScalarMeshField_), INTENT(INOUT) :: prop
  !! Scalar property
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  !! Finite element domain
  INTEGER(I4B), INTENT(IN) :: maxNNS
  !! maximum number of nodes in space-element
  !! it can be maximum number of quadrature points used in mesh
  INTEGER(I4B), INTENT(IN) :: maxNNT
  !! maximum number of nodes in time-element
  !! it can be maximum number of quadrature points used in time domain
  CHARACTER(*), INTENT(IN) :: propname
  !! name of scalar property
  CHARACTER(*), INTENT(IN) :: engine
  !! engine name
  INTEGER(I4B), INTENT(IN) :: fieldType
  !! field Type
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: varType
  !! varType
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: defineOn
  !! Defined on
  !! more details are given in AbstractMeshField_Class

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateScalarProperty1()"
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
    CALL SetSTScalarMeshFieldParam(param=param, name=propname, &
                        fieldType=fieldType, varType=varType, engine=engine, &
                                   defineOn=defineOn, nns=maxNNS, nnt=maxNNT)

  ELSE

    CALL SetScalarMeshFieldParam(param=param, name=propname, &
                        fieldType=fieldType, varType=varType, engine=engine, &
                                 defineOn=defineOn, nns=maxNNS)

  END IF

  CALL prop%Initiate(param=param, mesh=mesh)

  CALL param%DEALLOCATE()

  NULLIFY (mesh)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE KernelInitiateScalarProperty1

!----------------------------------------------------------------------------
!                                                     InitiateScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateScalarProperty2(prop, dom, maxNNS, maxNNT, &
                               propname, engine, fieldType, varType, defineOn)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: prop(:)
  !! Scalar property
  TYPE(FEDomainPointer_), INTENT(INOUT) :: dom(:)
  !! Finite element domain
  INTEGER(I4B), INTENT(IN) :: maxNNS(:)
  !! maximum number of nodes in space-element
  !! it can be maximum number of quadrature points used in mesh
  INTEGER(I4B), INTENT(IN) :: maxNNT(:)
  !! maximum number of nodes in time-element
  !! it can be maximum number of quadrature points used in time domain
  TYPE(String), INTENT(IN) :: propname(:)
  !! name of scalar property
  CHARACTER(*), INTENT(IN) :: engine
  !! engine name
  INTEGER(I4B), INTENT(IN) :: fieldType(:)
  !! field Type
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: varType(:)
  !! varType
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: defineOn(:)
  !! Defined on
  !! more details are given in AbstractMeshField_Class

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateScalarProperty2()"
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

  tsize = SIZE(prop)

  DO ii = 1, tsize
    CALL KernelInitiateScalarProperty1(prop=prop(ii)%ptr, dom=dom(ii)%ptr, &
                                       maxNNS=maxNNS(ii), maxNNT=maxNNT(ii), &
                               propname=propname(ii)%chars(), engine=engine, &
                               fieldType=fieldType(ii), varType=varType(ii), &
                                       defineOn=defineOn(ii))
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE KernelInitiateScalarProperty2

!----------------------------------------------------------------------------
!                                             KernelInitiateScalarProperty3
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateScalarProperty3(prop, dom, maxNNS, maxNNT, &
                               propname, engine, fieldType, varType, defineOn)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: prop(:)
  !! Scalar property
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  !! Finite element domain
  INTEGER(I4B), INTENT(IN) :: maxNNS(:)
  !! maximum number of nodes in space-element
  !! it can be maximum number of quadrature points used in mesh
  INTEGER(I4B), INTENT(IN) :: maxNNT(:)
  !! maximum number of nodes in time-element
  !! it can be maximum number of quadrature points used in time domain
  TYPE(String), INTENT(IN) :: propname(:)
  !! name of scalar property
  CHARACTER(*), INTENT(IN) :: engine
  !! engine name
  INTEGER(I4B), INTENT(IN) :: fieldType(:)
  !! field Type
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: varType(:)
  !! varType
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: defineOn(:)
  !! Defined on
  !! more details are given in AbstractMeshField_Class

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateScalarProperty3()"
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

  tsize = SIZE(prop)

  DO ii = 1, tsize
    CALL KernelInitiateScalarProperty1(prop=prop(ii)%ptr, dom=dom, &
                                       maxNNS=maxNNS(ii), maxNNT=maxNNT(ii), &
                               propname=propname(ii)%chars(), engine=engine, &
                               fieldType=fieldType(ii), varType=varType(ii), &
                                       defineOn=defineOn(ii))
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE KernelInitiateScalarProperty3

!----------------------------------------------------------------------------
!                                             KernelInitiateScalarProperty4
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateScalarProperty4(prop, dom, maxNNS, maxNNT, &
                               propname, engine, fieldType, varType, defineOn)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: prop(:)
  !! Scalar property
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  !! Finite element domain
  INTEGER(I4B), INTENT(IN) :: maxNNS
  !! maximum number of nodes in space-element
  !! it can be maximum number of quadrature points used in mesh
  INTEGER(I4B), INTENT(IN) :: maxNNT
  !! maximum number of nodes in time-element
  !! it can be maximum number of quadrature points used in time domain
  TYPE(String), INTENT(IN) :: propname(:)
  !! name of scalar property
  CHARACTER(*), INTENT(IN) :: engine
  !! engine name
  INTEGER(I4B), INTENT(IN) :: fieldType
  !! field Type
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: varType
  !! varType
  !! more details are given in AbstractMeshField_Class
  INTEGER(I4B), INTENT(IN) :: defineOn
  !! Defined on
  !! more details are given in AbstractMeshField_Class

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateScalarProperty4()"
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
    CALL KernelInitiateScalarProperty1(prop=prop(ii)%ptr, dom=dom, &
                                       maxNNS=maxNNS, maxNNT=maxNNT, &
                               propname=propname(ii)%chars(), engine=engine, &
                                       fieldType=fieldType, varType=varType, &
                                       defineOn=defineOn)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE KernelInitiateScalarProperty4

!----------------------------------------------------------------------------
!                                                          SetScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelSetScalarProperty1(prop, materials, dom, propname, &
                                    medium, times)
  TYPE(AbstractScalarMeshField_), INTENT(INOUT) :: prop
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  CHARACTER(*), INTENT(IN) :: propname
  INTEGER(I4B), INTENT(IN) :: medium
  REAL(DFP), INTENT(IN) :: times(:)

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelSetScalarProperty1()"
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
END SUBROUTINE KernelSetScalarProperty1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END MODULE KernelScalarProperty_Method
