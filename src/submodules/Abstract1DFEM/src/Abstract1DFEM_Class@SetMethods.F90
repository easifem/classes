! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(Abstract1DFEM_Class) SetMethods

USE BaseType, ONLY: elem => TypeElemNameOpt

USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate, &
                                  Quad_Size => Size

USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                OrthogonalElemShapeData

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   obj_Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')

#endif

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                           SetTotalDOFSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalDOFSpace

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetTotalDOFSpace()"
#endif

INTEGER(I4B) :: iel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Reallocate(obj%totalDOFSpace, obj%totalSpaceElements)

obj%totalVertexDOFSpace = obj%totalSpaceNodes
obj%totalEdgeDOFSpace = 0

DO iel = 1, obj%totalSpaceElements
  obj%totalDOFSpace(iel) = obj%spaceOrder(iel) + 1

  IF (obj%totalDOFSpace(iel) .GE. 2) THEN
    obj%totalEdgeDOFSpace = obj%totalEdgeDOFSpace &
                            + obj%totalDOFSpace(iel) - 2
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetTotalDOFSpace

!----------------------------------------------------------------------------
!                                                            SetQuadForSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadForSpace
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadForSpace()"
#endif

INTEGER(I4B) :: order, integralOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = obj%spaceOrder(spaceElemNum)
integralOrder = 2 * order

CALL QuadPoint_Initiate(obj=obj%quadForSpace, elemType=elem%line, &
                        domainName="B", order=integralOrder, &
                        quadratureType=obj%quadTypeForSpace)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetQuadForSpace

!----------------------------------------------------------------------------
!                                                          SetElemsdForSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElemsdForSpace
CHARACTER(*), PARAMETER :: myName = "obj_SetElemsdForSpace()"
INTEGER(I4B) :: nips, nns, cellOrder(1), order, cellOrient(1)
REAL(DFP) :: refElemCoord(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = obj%spaceOrder(spaceElemNum)
nips = Quad_Size(obj%quadForSpace, 2)
nns = obj%totalDOFSpace(spaceElemNum)
refElemCoord(1, 1) = -1.0_DFP
refElemCoord(1, 2) = 1.0_DFP
cellOrient = 1

CALL Elemsd_Allocate(obj=obj%elemsdForSpace, nsd=1_I4B, xidim=1_I4B, &
                     nns=nns, nips=nips)

CALL LagrangeElemShapeData(obj=obj%linElemsdForSpace, &
                           quad=obj%quadForSpace, &
                           nsd=obj%elemsdForSpace%nsd, &
                           xidim=obj%elemsdForSpace%xidim, &
                           elemtype=elem%line, &
                           refelemCoord=refelemCoord, &
                           domainName="B", &
                           order=1_I4B)

SELECT CASE (obj%baseInterpolationForSpace)
CASE ("LAGR")

  CALL LagrangeElemShapeData(obj=obj%elemsdForSpace, &
                             quad=obj%quadForSpace, &
                             nsd=obj%elemsdForSpace%nsd, &
                             xidim=obj%elemsdForSpace%xidim, &
                             elemtype=elem%line, &
                             refelemCoord=refelemCoord, &
                             domainName="B", &
                             order=order, &
                             ipType=obj%ipTypeForSpace, &
                             basisType=obj%baseTypeForSpace)

  obj%spaceShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%spaceShapeFuncBndy(1, 1) = 1.0_DFP

  obj%spaceShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%spaceShapeFuncBndy(2, 2) = 1.0_DFP

CASE ("HIER", "HEIR")

  cellOrder = order
  CALL HierarchicalElemShapeData(obj=obj%elemsdForSpace, &
                                 quad=obj%quadForSpace, &
                                 nsd=obj%elemsdForSpace%nsd, &
                                 xidim=obj%elemsdForSpace%xidim, &
                                 elemtype=elem%line, &
                                 refelemCoord=refelemCoord, &
                                 domainName="B", &
                                 cellOrder=cellOrder, &
                                 cellOrient=cellOrient)

  obj%spaceShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%spaceShapeFuncBndy(1, 1) = 1.0_DFP

  obj%spaceShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%spaceShapeFuncBndy(2, 2) = 1.0_DFP

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'no case found for baseInterpolationForSpace')

END SELECT

CALL Elemsd_Set(obj=obj%elemsdForSpace, val=xij, &
                N=obj%linElemsdForSpace%N(1:2, 1:nips), &
                dNdXi=obj%linElemsdForSpace%dNdXi(1:2, 1:1, 1:nips))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetElemsdForSpace

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
