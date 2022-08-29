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

MODULE AbstractFE_Class
USE GlobalData
USE AbstractRefElement_Class
IMPLICIT NONE
PRIVATE
!!
CHARACTER(LEN=*), PARAMETER :: modName = "AbstractFE_Class"
!!
!! @DOFType
!!
INTEGER(I4B), PARAMETER, PUBLIC :: FE_DOF_POINT_EVAL = 1_I4B
!!
!! @TransformType
!!
INTEGER(I4B), PARAMETER, PUBLIC :: FE_TRANSFORM_IDENTITY = 1_I4B
!!
!! @FEType
!!
INTEGER(I4B), PARAMETER, PUBLIC :: H1_LAGRANGE = LagrangePolynomial
INTEGER(I4B), PARAMETER, PUBLIC :: H1_SERENDIPITY = SerendipityPolynomial
INTEGER(I4B), PARAMETER, PUBLIC :: H1_HEIRARCHICAL = HeirarchicalPolynomial
!!
!! @ipType
!!
INTEGER(I4B), PARAMETER, PUBLIC :: IP_EQUIDISTANCE = EquidistanceLIP
INTEGER(I4B), PARAMETER, PUBLIC :: IP_GAUSS_LOBATTO = GaussLobattoLIP
INTEGER(I4B), PARAMETER, PUBLIC :: IP_GAUSS_LEGENDRE = GaussLegendreLIP
INTEGER(I4B), PARAMETER, PUBLIC :: IP_CHEBYSHEV = ChebyshevLIP

!----------------------------------------------------------------------------
!                                                        AbstractRefElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Abstract class for finite element is defined
!
!{!pages/AbstractFE_.md!}

TYPE, ABSTRACT :: AbstractFE_
  PRIVATE
  CLASS(AbstractRefElement_), PUBLIC, POINTER :: refelem => NULL()
  !! reference element
  INTEGER(I4B) :: nsd = 0
  !! spatial dimension
  INTEGER(I4B) :: order = 0
  !! Isotropic order of polynomial space
  INTEGER(I4B) :: feType = 0
  !! type of finite element
  INTEGER(I4B) :: ipType = 0
  !! type of lattice (interpolation point type) point
  INTEGER(I4B) :: dofType(4) = 0
  !! type of dof for shape function defined on vertex
  !! type of dof for shape functions on edge
  !! type of dof for shape functions on face
  !! type of dof for shape functions in cell
  INTEGER(I4B) :: transformType = 0
  !! type of Tranformation usef for polynomial space
CONTAINS
  !!
  PROCEDURE(fe_Initiate), DEFERRED, PUBLIC, PASS(obj) :: Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => fe_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => fe_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => fe_SetParam
END TYPE AbstractFE_
!!
PUBLIC :: AbstractFE_
!!
!----------------------------------------------------------------------------
!                                                         AbstractFEPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractFEPointer_
  CLASS(AbstractFE_), POINTER :: ptr => NULL()
END TYPE AbstractFEPointer_

PUBLIC :: AbstractFEPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary:         Initiate the finite element

ABSTRACT INTERFACE
  SUBROUTINE fe_Initiate(obj, elemType, order, ipType)
    IMPORT :: AbstractFE_, I4B
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
  END SUBROUTINE fe_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:         Deallocate the data

INTERFACE
  MODULE SUBROUTINE fe_Deallocate(obj)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
  END SUBROUTINE fe_Deallocate
END INTERFACE

INTERFACE AbstractFEDeallocate
  MODULE PROCEDURE fe_Deallocate
END INTERFACE AbstractFEDeallocate

PUBLIC :: AbstractFEDeallocate

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE fe_Display(obj, msg, unitno)
    CLASS(AbstractFE_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE fe_Display
END INTERFACE

INTERFACE AbstractFEDisplay
  MODULE PROCEDURE fe_Display
END INTERFACE AbstractFEDisplay

PUBLIC :: AbstractFEDisplay

!----------------------------------------------------------------------------
!                                                          SetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Set the parameters

INTERFACE
  MODULE PURE SUBROUTINE fe_SetParam(obj, nsd, order, &
    & feType, ipType, dofType, transformType)
    CLASS(AbstractFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: feType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType(4)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
  END SUBROUTINE fe_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AbstractScalarFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Abstract class for scalar finite element is defined
!
!{!pages/AbstractScalarFE_.md!}

TYPE, ABSTRACT, EXTENDS(AbstractFE_) :: &
  & AbstractScalarFE_
END TYPE AbstractScalarFE_
!!
PUBLIC :: AbstractScalarFE_
!!
!----------------------------------------------------------------------------
!                                                  AbstractScalarFEPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractScalarFEPointer_
  CLASS(AbstractScalarFE_), POINTER :: ptr => NULL()
END TYPE AbstractScalarFEPointer_

PUBLIC :: AbstractScalarFEPointer_

!----------------------------------------------------------------------------
!                                                          AbstractVectorFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Abstract class for scalar finite element is defined
!
!{!pages/AbstractVectorFE_.md!}

TYPE, ABSTRACT, EXTENDS(AbstractFE_) :: &
  & AbstractVectorFE_
END TYPE AbstractVectorFE_
!!
PUBLIC :: AbstractVectorFE_
!!
!----------------------------------------------------------------------------
!                                                  AbstractVectorFEPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractVectorFEPointer_
  CLASS(AbstractVectorFE_), POINTER :: ptr => NULL()
END TYPE AbstractVectorFEPointer_

PUBLIC :: AbstractVectorFEPointer_

!----------------------------------------------------------------------------
!                                                          AbstractMatrixFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Abstract class for scalar finite element is defined
!
!{!pages/AbstractMatrixFE_.md!}

TYPE, ABSTRACT, EXTENDS(AbstractFE_) :: &
  & AbstractMatrixFE_
END TYPE AbstractMatrixFE_
!!
PUBLIC :: AbstractMatrixFE_
!!
!----------------------------------------------------------------------------
!                                                  AbstractMatrixFEPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractMatrixFEPointer_
  CLASS(AbstractMatrixFE_), POINTER :: ptr => NULL()
END TYPE AbstractMatrixFEPointer_

PUBLIC :: AbstractMatrixFEPointer_

END MODULE AbstractFE_Class
