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

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This module defines a class called [[PorousMaterial_]]
!
!# Introduction
!
! This module defines a class called [[PorousMaterial_]], which defines a
! Porous material and its behavior. Other than defining the class,
! this module makes a routine called
! [[PorousMaterial_Class:SetPorousMaterialParam]] public.
! This routine can be used for Setting the options in [[ParameterList_]]
! object.

MODULE PorousMaterial_Class
USE GlobalData
USE String_Class
USE BaSetype
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractMaterial_Class
USE AbstractPoroMechanicsModel_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "PorousMaterial_Class"
CHARACTER(*), PARAMETER :: myprefix = "PorousMaterial"
PUBLIC :: PorousMaterial_
PUBLIC :: TypePorousMaterial
PUBLIC :: PorousMaterialPointer_
PUBLIC :: SetPorousMaterialParam

!----------------------------------------------------------------------------
!                                                            PorousMaterial_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Oct 2021
! summary: porousMaterial class for material modeling of Porouss
!
!# Introduction
! porousMaterial class is a child of [[AbstractMaterial_]].
! It is used for modeling the behavior of Porouss.

TYPE, EXTENDS(AbstractMaterial_) :: PorousMaterial_
  CLASS(AbstractPoroMechanicsModel_), POINTER :: stressStrainModel => NULL()
    !! Pointer to stress strain material behavior of Porouss
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & Porous_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => Porous_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => Porous_Deallocate
  FINAL :: Porous_Final
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => Porous_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => Porous_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => Porous_Display
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => Porous_GetPrefix
END TYPE PorousMaterial_

TYPE(PorousMaterial_), PARAMETER :: TypePorousMaterial = PorousMaterial_()

!----------------------------------------------------------------------------
!                                                     PorousMaterialPointer_
!----------------------------------------------------------------------------

TYPE :: PorousMaterialPointer_
  CLASS(PorousMaterial_), POINTER :: ptr => NULL()
END TYPE PorousMaterialPointer_

!----------------------------------------------------------------------------
!                                  SetPorousMaterialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine Sets the essential parameter for [[PorousMaterial_]]
!
!# Introduction
!
! This routine Sets the essential parameter for [[PorousMaterial_]].
! It Sets values for
!
! - `porousMaterial/name`
! - `porousMaterial/massDensity`
! - `porousMaterial/stresStrainModel`

INTERFACE
  MODULE SUBROUTINE SetPorousMaterialParam(param, name, stressStrainModel)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    !! It is the name of the material
    CHARACTER(*), OPTIONAL, INTENT(IN) :: stressStrainModel
    !! Name of the child-class of `AbstractPorousMechanicsModel_`
    !! For example `LinearElasticModel`
  END SUBROUTINE SetPorousMaterialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine Checks the essential parameter for [[PorousMaterial_]]
!
!# Introduction
!
! This routine Checks the essential parameter for [[PorousMaterial_]].
! It Checks the existance of
!
! - `porousMaterial/name`

INTERFACE
  MODULE SUBROUTINE Porous_CheckEssentialParam(obj, param)
    CLASS(PorousMaterial_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE Porous_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the instance of `porousMaterial`

INTERFACE
  MODULE SUBROUTINE Porous_Initiate(obj, param, prefix)
    CLASS(PorousMaterial_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE Porous_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine deallocates the instance
!
!# Introduction
!
! This routine deallocates the memory allocated to the instance of
! `PorousMaterial_`.
!
!@warning
! This routine also deallocates [[PorousMaterial_:stressStrainModel]], if
! it is associated.
!@endwarning

INTERFACE
  MODULE SUBROUTINE Porous_Deallocate(obj)
    CLASS(PorousMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE Porous_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine deallocates the instance

INTERFACE
  MODULE SUBROUTINE Porous_Final(obj)
    TYPE(PorousMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE Porous_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the instance from hdf5 file
!
!# Introduction
! This routine also deallocates the data associated
! summary: This routine initiates the instance from hdf5 file

INTERFACE
  MODULE SUBROUTINE Porous_Import(obj, hdf5, group)
    CLASS(PorousMaterial_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE Porous_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine exports the information to external hdf5 file

INTERFACE
  MODULE SUBROUTINE Porous_Export(obj, hdf5, group)
    CLASS(PorousMaterial_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE Porous_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine displays the content of the instance

INTERFACE
  MODULE SUBROUTINE Porous_Display(obj, msg, unitNo)
    CLASS(PorousMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE Porous_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION Porous_GetPrefix(obj) RESULT(ans)
    CLASS(PorousMaterial_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION Porous_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE PorousMaterial_Class
