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

SUBMODULE(ScalarField_Class) SetMethods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE FEVariable_Method, ONLY: FEVariableGet => GET
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
! USE DOF_Method, ONLY: GetIDOF
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: math => TypeMathOpt
USE ElemShapeData_Method, ONLY: ElemShapeDataDeallocate => DEALLOCATE
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePointInitiate => Initiate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
  "ScalarField_Class@SetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: indx

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%IsInitiated()
  CALL AssertError1(isok, myName, &
                    "ScalarField_::obj not initiated")
#endif

#include "./include/localNodeError.F90"

  indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, idof=math%one_i)
  CALL obj%SetSingle(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: i1, i2
#endif

  INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%IsInitiated()
  CALL AssertError1(isok, myName, "ScalarField_::obj not initiated")
#endif

#ifdef DEBUG_VER
  isok = obj%fieldType .NE. TypeFieldOpt%constant
  CALL AssertError1(isok, myName, "Not callable for Constant field")
#endif

#ifdef DEBUG_VER
  i1 = SIZE(VALUE)
  i2 = obj%dof .tNodes. math%one_i
  isok = i1 .GE. i2
  CALL AssertError1(isok, myName, "Size of value is not enought")
#endif

  s = GetNodeLoc(obj=obj%dof, idof=math%one_i)

  CALL obj%SetMultiple(VALUE=VALUE, scale=scale, &
                       addContribution=addContribution, istart=s(1), &
                       iend=s(2), stride=s(3))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%IsInitiated()
  CALL AssertError1(isok, myName, "ScalarField_::obj not initiated")
#endif

#ifdef DEBUG_VER
  isok = obj%fieldType .NE. TypeFieldOpt%constant
  CALL AssertError1(isok, myName, "Not callable for Constant field")
#endif

#include "./include/localNodeError.F90"

  tsize = SIZE(globalNode)
  DO ii = 1, tsize
    CALL obj%SetSingle( &
                        indx=globalNode(ii), VALUE=VALUE, scale=scale, &
                        addContribution=addContribution)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: i1, i2
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%IsInitiated()
  CALL AssertError1(isok, myName, "ScalarField_::obj not initiated")
#endif

#ifdef DEBUG_VER
  isok = obj%fieldType .NE. TypeFieldOpt%constant
  CALL AssertError1(isok, myName, "Not callable for Constant field")
#endif

#ifdef DEBUG_VER
  i1 = SIZE(VALUE)
  i2 = SIZE(globalNode)

  isok = i1 .GE. i2
  CALL AssertError1(isok, myName, "Size of value is not enought")
#endif

#include "./include/localNodeError.F90"

  CALL obj%SetMultiple(indx=globalNode, VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  SELECT CASE (VALUE%vartype)

  CASE (TypeFEVariableOpt%constant)

    CALL obj%Set(VALUE=FEVariableGet(VALUE, TypeFEVariableScalar, &
                                     TypeFEVariableConstant), globalNode= &
                 globalNode, scale=scale, addContribution=addContribution, &
                 islocal=islocal)

  CASE (TypeFEVariableOpt%space)

    CALL obj%Set(VALUE=FEVariableGet(VALUE, TypeFEVariableScalar, &
                                     TypeFEVariableSpace), globalNode= &
                 globalNode, scale=scale, addContribution=addContribution, &
                 islocal=islocal)

  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, "Unknown vartype in VALUE")
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%Set(VALUE=VALUE, scale=math%one, addContribution=math%no)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: s(3)
  REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%IsInitiated()
  CALL AssertError1(isok, myName, "ScalarField_::obj not initiated")
#endif

#ifdef DEBUG_VER
  isok = VALUE%IsInitiated()
  CALL AssertError1(isok, myName, "ScalarField_::value not initiated")
#endif

  s = GetNodeLoc(obj=obj%dof, idof=math%one_i)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                       scale=scale, addContribution=addContribution)
  realvec => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                             SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
  LOGICAL(LGT) :: isok
#endif

  CLASS(AbstractMesh_), POINTER :: meshptr
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  INTEGER(I4B) :: telements, iel, maxNNS, maxGeoNNS, maxNips, tans, &
    xij_i, xij_j, tcon, ii
  TYPE(QuadraturePoint_) :: quad(8), facetQuad(8), cellQuad
  TYPE(ElemShapeData_) :: cellElemsd, geoCellElemsd, geoElemsd(8), &
    geoFacetElemsd(8), elemsd(8), facetElemsd(8)
  REAL(DFP) :: times0
  REAL(DFP), ALLOCATABLE :: xij(:, :), ans(:), massMat(:, :), &
    funcValue(:), temp(:)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:), con(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  meshptr => obj%fedof%GetMeshPointer()

#ifdef DEBUG_VER
  isok = ASSOCIATED(meshptr)
  CALL AssertError1(isok, myName, &
                    "mesh pointer obtained from fedof is not associated...")
#endif

  times0 = Input(option=times, DEFAULT=math%zero)

  maxNNS = obj%fedof%GetMaxTotalConnectivity()
  maxGeoNNS = obj%geofedof%GetMaxTotalConnectivity()
  maxNips = obj%fedof%GetMaxTotalQuadraturePoints()

  CALL Reallocate(massMat, maxNNS, maxNNS)
  CALL Reallocate(ipiv, maxNNS)
  CALL Reallocate(xij, 3, maxGeoNNS)
  CALL Reallocate(ans, maxNNS)
  CALL Reallocate(temp, maxNNS)
  CALL Reallocate(con, maxNNS)
  CALL Reallocate(funcValue, maxNips)

  telements = meshptr%GetTotalElements()

  DO iel = 1, telements
    CALL obj%fedof%SetFE(globalElement=iel, islocal=math%yes)
    feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    CALL obj%geofedof%SetFE(globalElement=iel, islocal=math%yes)
    geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    CALL meshptr%GetNodeCoord(nodeCoord=xij, nrow=xij_i, &
                              ncol=xij_j, globalElement=iel, islocal=math%yes)

    CALL feptr%GetDOFValue(geofeptr=geofeptr, elemsd=elemsd, &
                           geoElemsd=geoElemsd, facetElemsd=facetElemsd, &
                           geoFacetElemsd=geoFacetElemsd, &
                           cellElemsd=cellElemsd, &
                           geoCellElemsd=geoCellElemsd, quad=quad, &
                           facetQuad=facetQuad, cellQuad=cellQuad, xij=xij, &
                           times=times0, func=func, ans=ans, tsize=tans, &
                           massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
                           temp=temp)

    ! obj, geofeptr, elemsd, geoelemsd, facetElemsd, geoFacetElemsd, &
    ! cellElemsd, geoCellElemsd, quad, facetQuad, cellQuad, xij, times, &
    ! func, ans, tsize, massMat, ipiv, funcValue, temp)
    CALL obj%fedof%GetConnectivity_(ans=con, tsize=tcon, opt="A", &
                                    globalElement=iel, islocal=math%yes)

    ! (obj, ans, tsize, opt, globalElement, islocal)
    CALL obj%Set(VALUE=ans(1:tans), globalNode=con(1:tcon), &
                 islocal=math%yes)
  END DO

  DEALLOCATE(massMat, ipiv, xij, ans, temp, con, funcValue)

  DO ii = 1, SIZE(quad)
    CALL QuadraturePointDeallocate(quad(ii))
    CALL QuadraturePointDeallocate(facetQuad(ii))
    CALL ElemShapeDataDeallocate(elemsd(ii))
    CALL ElemShapeDataDeallocate(facetElemsd(ii))
    CALL ElemShapeDataDeallocate(geoElemsd(ii))
    CALL ElemShapeDataDeallocate(geoFacetElemsd(ii))
  END DO

  CALL QuadraturePointDeallocate(cellQuad)
  CALL ElemShapeDataDeallocate(cellElemsd)
  CALL ElemShapeDataDeallocate(geoCellElemsd)

  NULLIFY(meshptr, feptr, geofeptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!                                                           SetLagrangeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLagrangeDOF
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetLagrangeDOF()"
#endif
  INTEGER(I4B) :: iel, tElements, ii, maxCon, &
    tfecon, tElements2, tfecon2, maxCon2, maxGeoNNS2, &
    refElemCoord2_i, refElemCoord2_j, &
    xij_i, xij_j, elemCoord2_i, elemCoord2_j, &
    nodalValue2_i, nodalValue1_i
  INTEGER(I4B) :: cellOrder(3), ipType(3)
  LOGICAL(LGT) :: isok
  TYPE(QuadraturePoint_) :: quad
  TYPE(ElemShapeData_) :: geoelemsd2, elemsd2
  INTEGER(I4B), ALLOCATABLE :: fecon(:), fecon2(:)
  REAL(DFP), ALLOCATABLE :: elemCoord2(:, :), &
    refElemCoord2(:, :), &
    xij(:, :), &
    nodalValue1(:), &
    nodalValue2(:)
  CLASS(AbstractFE_), POINTER :: feptr, feptr2, geofeptr2
  CLASS(AbstractMesh_), POINTER :: mesh, mesh2
  CLASS(FEDOF_), POINTER :: fedof, fedof2, geofedof2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  fedof => obj%fedof
  mesh => fedof%GetMeshPointer()
  tElements = mesh%GetTotalElements()
  maxCon = fedof%GetMaxTotalConnectivity()

  fedof2 => obj2%fedof
  geofedof2 => obj2%geofedof
  mesh2 => fedof2%GetMeshPointer()
  tElements2 = mesh2%GetTotalElements()
  maxGeoNNS2 = geofedof2%GetMaxTotalConnectivity()
  maxCon2 = fedof2%GetMaxTotalConnectivity()

!check
#ifdef DEBUG_VER
  CALL AssertError2(tElements, tElements2, myName, &
                    "a=tElements in obj, b=tElements in obj2")
#endif

  CALL Reallocate(fecon, maxCon)
  CALL Reallocate(nodalValue1, maxCon)
  CALL Reallocate(xij, math%four_i, maxCon)

  CALL Reallocate(refElemCoord2, math%three_i, maxGeoNNS2)
  CALL Reallocate(fecon2, maxCon2)
  CALL Reallocate(elemCoord2, math%three_i, maxGeoNNS2)
  CALL Reallocate(nodalValue2, maxCon2)

  cell_loop: DO iel = 1, tElements
    isok = mesh2%isElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE cell_loop

    ! get connectivity of obj2
    CALL fedof2%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                                 ans=fecon2, tsize=tfecon2, opt="A")

    ! get finite element from obj2
    CALL fedof2%SetFE(globalElement=iel, islocal=math%yes)
    feptr2 => fedof2%GetFEPointer(globalElement=iel, islocal=math%yes)

    ! get reference element nodal coordinates
    CALL feptr2%GetRefElemCoord(ans=refElemCoord2, nrow=refElemCoord2_i, &
                                ncol=refElemCoord2_j)

    ! get cellorder and ipType from obj%fedof
    CALL fedof%GetCellOrder(ans=cellOrder, tsize=ii, globalElement=iel, &
                            islocal=math%yes)
    cellOrder(2:3) = cellOrder(1)

    ! get finite element from obj
    CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
    feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
    ipType(1) = feptr%GetIpType()
    ipType(2:3) = ipType(1)

    ! Get interpolation points in reference element of (obj2)
    CALL feptr2%GetInterpolationPoints(xij=refElemCoord2(1:refElemCoord2_i, &
                                                         1:refElemCoord2_j), &
                                       ans=xij, nrow=xij_i, ncol=xij_j, &
                                       order=cellOrder, ipType=ipType)

    ! Create quadrature points as interpolation points in ref element (obj2)
    CALL QuadraturePointInitiate(obj=quad, points=xij(:, 1:xij_j))

    ! Get nodal coordinates of physical element in obj2
    CALL mesh2%GetNodeCoord(nodeCoord=elemCoord2, nrow=elemCoord2_i, &
                            ncol=elemCoord2_j, globalElement=iel, &
                            islocal=math%yes)

    ! Get finite element form geofedof of obj2
    CALL geofedof2%SetFE(globalElement=iel, islocal=math%yes)
    geofeptr2 => geofedof2%GetFEPointer(globalElement=iel, &
                                        islocal=math%yes)

    ! Get element shape data on obj2
    CALL feptr2%GetGlobalElemShapeData2(geofeptr=geofeptr2, elemsd=elemsd2, &
                                        geoelemsd=geoelemsd2, &
                                        xij=elemCoord2, quad=quad, &
                                        doNotInitQuad=math%yes)

    ! Get nodal value of obj2
    CALL obj2%Get(VALUE=nodalValue2, globalNode=fecon2(1:tfecon2), &
                  islocal=math%yes, tsize=nodalValue2_i)

    ! Use element shape data on obj2 to get the interpolation value
    ! nodalValue1 are Lagrange DOF of obj
    CALL GetInterpolation_(obj=elemsd2, ans=nodalValue1, &
                           val=nodalValue2(1:nodalValue2_i), &
                           tsize=nodalValue1_i, scale=math%one, &
                           addContribution=math%no)

    ! Get the connectivity of obj
    CALL fedof%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                                ans=fecon, tsize=tfecon, opt="A")

    ! Fix the orientation of fecon for H1Conformity
    ! tsize in code below is read only:
    CALL feptr%FixOrientOfNptrs(nptrs=fecon, tsize=tfecon)

    ! Set the values in obj
    CALL obj%Set(VALUE=nodalValue1(1:nodalValue1_i), &
                 globalNode=fecon(1:tfecon), &
                 islocal=math%yes, scale=math%one, &
                 addContribution=math%no)
  END DO cell_loop

  CALL ElemShapeDataDeallocate(elemsd2)
  CALL ElemShapeDataDeallocate(geoelemsd2)
  CALL QuadraturePointDeallocate(quad)
  IF (ALLOCATED(fecon)) DEALLOCATE(fecon)
  IF (ALLOCATED(fecon2)) DEALLOCATE(fecon2)
  IF (ALLOCATED(elemCoord2)) DEALLOCATE(elemCoord2)
  IF (ALLOCATED(refElemCoord2)) DEALLOCATE(refElemCoord2)
  IF (ALLOCATED(xij)) DEALLOCATE(xij)
  IF (ALLOCATED(nodalValue1)) DEALLOCATE(nodalValue1)
  IF (ALLOCATED(nodalValue2)) DEALLOCATE(nodalValue2)
  NULLIFY(feptr, feptr2, geofeptr2, mesh, mesh2, fedof, fedof2, geofedof2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetLagrangeDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
