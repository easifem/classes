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

MODULE AbstractBasis_Class
USE GlobalData
USE AbstractFunction_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractBasis_Class"

!----------------------------------------------------------------------------
!                                                          AbstractBasis1D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: AbstractMonomial class is defined

TYPE, ABSTRACT, EXTENDS(AbstractFunction1D_) :: AbstractBasis1D_
  INTEGER(I4B) :: uid = 0
CONTAINS
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_Deallocate1
END TYPE AbstractBasis1D_

TYPE :: AbstractBasis1DPointer_
  CLASS(AbstractBasis1D_), POINTER :: ptr => NULL()
END TYPE AbstractBasis1DPointer_

PUBLIC :: AbstractBasis1D_
PUBLIC :: AbstractBasis1DPointer_

!----------------------------------------------------------------------------
!                                                        AbstractBasis2D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: AbstractBasis class is defined

TYPE, ABSTRACT, EXTENDS(AbstractFunction2D_) :: AbstractBasis2D_
  INTEGER(I4B) :: uid = 0
CONTAINS
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_Deallocate2
END TYPE AbstractBasis2D_

TYPE :: AbstractBasis2DPointer_
  CLASS(AbstractBasis2D_), POINTER :: ptr => NULL()
END TYPE AbstractBasis2DPointer_

PUBLIC :: AbstractBasis2D_
PUBLIC :: AbstractBasis2DPointer_

!----------------------------------------------------------------------------
!                                                        AbstractBasis3D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: AbstractBasis class is defined

TYPE, ABSTRACT, EXTENDS(AbstractFunction3D_) :: AbstractBasis3D_
  INTEGER(I4B) :: uid = 0
CONTAINS
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_Deallocate3
END TYPE AbstractBasis3D_

TYPE :: AbstractBasis3DPointer_
  CLASS(AbstractBasis3D_), POINTER :: ptr => NULL()
END TYPE AbstractBasis3DPointer_

PUBLIC :: AbstractBasis3D_
PUBLIC :: AbstractBasis3DPointer_

!----------------------------------------------------------------------------
!                                                        AbstractBasisND_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: AbstractBasis class is defined

TYPE, ABSTRACT, EXTENDS(AbstractFunctionND_) :: AbstractBasisND_
  INTEGER(I4B) :: uid = 0
CONTAINS
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => func_DeallocateN
END TYPE AbstractBasisND_

TYPE :: AbstractBasisNDPointer_
  CLASS(AbstractBasisND_), POINTER :: ptr => NULL()
END TYPE AbstractBasisNDPointer_

PUBLIC :: AbstractBasisND_
PUBLIC :: AbstractBasisNDPointer_

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Deallocate

INTERFACE
  MODULE SUBROUTINE func_Deallocate1(obj)
    CLASS(AbstractBasis1D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate1
END INTERFACE

INTERFACE AbstractBasis1DDeallocate
  MODULE PROCEDURE func_Deallocate1
END INTERFACE AbstractBasis1DDeallocate

PUBLIC :: AbstractBasis1DDeallocate

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the Basis

INTERFACE
  MODULE SUBROUTINE func_Deallocate2(obj)
    CLASS(AbstractBasis2D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate2
END INTERFACE

INTERFACE AbstractBasis2DDeallocate
  MODULE PROCEDURE func_Deallocate2
END INTERFACE AbstractBasis2DDeallocate

PUBLIC :: AbstractBasis2DDeallocate

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the Basis

INTERFACE
  MODULE SUBROUTINE func_Deallocate3(obj)
    CLASS(AbstractBasis3D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate3
END INTERFACE

INTERFACE AbstractBasis3DDeallocate
  MODULE PROCEDURE func_Deallocate3
END INTERFACE AbstractBasis3DDeallocate

PUBLIC :: AbstractBasis3DDeallocate

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the Basis

INTERFACE
  MODULE SUBROUTINE func_DeallocateN(obj)
    CLASS(AbstractBasisND_), INTENT(INOUT) :: obj
  END SUBROUTINE func_DeallocateN
END INTERFACE

INTERFACE AbstractBasisNDDeallocate
  MODULE PROCEDURE func_DeallocateN
END INTERFACE AbstractBasisNDDeallocate

PUBLIC :: AbstractBasisNDDeallocate

END MODULE AbstractBasis_Class
