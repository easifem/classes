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
! date: 2 Oct 2021
! summary: Data type of Newtonian fluid model
!
!# Introduction
!
! This module defines a class [[NewtonianFluidModel_]] to
! model the fluid behavior. In addition, it defines a subroutine
! [[SetNewtonianFluidModelParam]] to Set the option
! to construct an instance of [[NewtonianFluidModel_]].

MODULE NewtonianFluidModel_Class
USE GlobalData
USE String_Class
USE BaSetype
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractMaterialModel_Class
USE AbstractFluidMechanicsModel_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "NewtonianFluidModel_CLASS"
CHARACTER(*), PARAMETER :: myprefix = "NewtonianFluidModel"
PUBLIC :: NewtonianFluidModel_
PUBLIC :: TypeNewtonianFluidModel
PUBLIC :: NewtonianFluidModelPointer_
PUBLIC :: SetNewtonianFluidModelParam

!----------------------------------------------------------------------------
!                                                       NewtonianFluidModel_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: A class for modeling Newtonian fluids
!
!# Introduction
!
! [[NewtonianFluidModel_]] class defines a Newtonian fluid.
!
! - `mu` represents the dynamicviscosity of the fluid.
! - dynamicViscosity $\mu$ of [[NewtonianFluidModel_]] is independent of the
! stress, and strain rate.
! - However, $\mu$ can depends upon the temperature, in case of
! non-isothermal fluid flow applications.
!
!@todo
! The dynamicviscosity of the fluid usually depends upon the temperature.
! Add this facility to the current class.
!@endtodo

TYPE, EXTENDS(AbstractFluidMechanicsModel_) :: NewtonianFluidModel_
  PRIVATE
  REAL(DFP) :: mu = 0.0_DFP
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & nfm_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => nfm_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => nfm_Deallocate
  FINAL :: nfm_FINAL
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => nfm_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => nfm_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => nfm_Display
  PROCEDURE, PUBLIC, PASS(obj) :: GetdynamicViscosity =>  &
    & nfm_GetdynamicViscosity
  PROCEDURE, PUBLIC, PASS(obj) :: GetModelParameters => &
    & nfm_GetModelParameters
  PROCEDURE, PUBLIC, PASS(obj) :: SetModelParameters => &
    & nfm_SetModelParameters
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => nfm_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => nfm_GetParam
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => nfm_SetParam
END TYPE NewtonianFluidModel_

TYPE(NewtonianFluidModel_), PARAMETER :: TypeNewtonianFluidModel = &
  & NewtonianFluidModel_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: NewtonianFluidModelPointer_
  CLASS(NewtonianFluidModel_), POINTER :: ptr => NULL()
END TYPE NewtonianFluidModelPointer_

!----------------------------------------------------------------------------
!                             SetNewtonianFluidModelParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Check the essential parameter
!
!# Introduction
!
! This routine Sets the options in `param` to construct an instance of
! [[NewtonianFluidModelPointer_]].
!
!@note
! Always construct the parameter for [[NewtonianFluidModel_]] by using
! [[SetNewtonianFluidModelParam]]
!@endnote
!
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( ParameterList_ ) :: param
! CALL FPL_INIT; CALL param%initiate()
! CALL SetNewtonianFluidModelParam( param = param, dynamicViscosity=0.001_DFP )
! CALL obj%initiate( param )
! CALL param%print()
! CALL obj%display( msg="Test-1 : " )
! CALL param%Deallocate(); CALL FPL_FINALIZE
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE SetNewtonianFluidModelParam(param, dynamicViscosity)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    REAL(DFP), OPTIONAL, INTENT(IN) :: dynamicViscosity
  END SUBROUTINE SetNewtonianFluidModelParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Check the essential parameter
!
!# Introduction
!
! - This routine checks the availability of options in param
! - These options are necessary for constructing an instance of
!  [[NewtonianFluidModel_]].
! - This routine is called by [[NewtonianFluidModel_:Initiate]]

INTERFACE
  MODULE SUBROUTINE nfm_checkEssentialParam(obj, param)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE nfm_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: This routine initiates the the Newtonian fluid model
!
!# Introduction
! This routine initiates an instance of [[NewtonianFluidModel_]]. The usage
! is shown below.
!
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( ParameterList_ ) :: param
! CALL FPL_INIT; CALL param%initiate()
! CALL SetNewtonianFluidModelParam( param = param, &
! & dynamicViscosity=0.001_DFP )
! CALL obj%initiate( param )
! CALL param%print()
! CALL obj%display( msg="Test-1 : " )
! CALL param%Deallocate(); CALL FPL_FINALIZE
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE nfm_Initiate(obj, param)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE nfm_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary:         Deallocate data stored in the object
!
!# Introduction
!
! This routine deallocates the memory occupied by [[NewtonianFluidModel_]]
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( ParameterList_ ) :: param
! CALL FPL_INIT; CALL param%initiate()
! CALL SetNewtonianFluidModelParam( param = param, &
! & dynamicViscosity=0.001_DFP )
! CALL obj%initiate( param )
! CALL param%print()
! CALL obj%display( msg="Test-1 : " )
! CALL obj%Deallocate()
! CALL param%Deallocate(); CALL FPL_FINALIZE
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE nfm_Deallocate(obj)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
  END SUBROUTINE nfm_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary:         Deallocate data stored in the object
!
!# Introduction
!
! This routine deallocates the memory occupied by [[NewtonianFluidModel_]]
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( ParameterList_ ) :: param
! CALL FPL_INIT; CALL param%initiate()
! CALL SetNewtonianFluidModelParam( param = param, &
! & dynamicViscosity=0.001_DFP )
! CALL obj%initiate( param )
! CALL param%print()
! CALL obj%display( msg="Test-1 : " )
! CALL obj%Deallocate()
! CALL param%Deallocate(); CALL FPL_FINALIZE
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE nfm_Final(obj)
    TYPE(NewtonianFluidModel_), INTENT(INOUT) :: obj
  END SUBROUTINE nfm_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Initiate the Newtonian fluid model from hdf5 file
!
!# Introduction
!
! This routine initiates an instance of [[NewtonianFluidModel_]] by
! importing data from [[HDF5File_]] class. The content of [[HDF5File_]]
! is shown below.
!
! ![](../|media|/NewtonianFluidModel_template.png ""){: width="400"}
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( HDF5File_ ) :: hdf5file
! CALL hdf5file%initiate( "./TemplateNewtonianFluidModel1.hdf5", mode="READ")
! CALL hdf5file%open()
! CALL obj%import(hdf5file, "")
! CALL obj%Display("Test-3 : ")
! CALL hdf5file%close(); CALL hdf5file%Deallocate()
! CALL obj%Deallocate()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE nfm_Import(obj, hdf5, group)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE nfm_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Export the Newtonian fluid model to an hdf5 file
!
!# Introduction
!
! This routine exports the content of [[NewtonianFluidModel_]] in an
! [[HDF5File_]].
!
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( ParameterList_ ) :: param
! TYPE( HDF5File_ ) :: hdf5file
! CALL FPL_INIT; CALL param%initiate()
! CALL SetNewtonianFluidModelParam( param = param, &
! & dynamicViscosity=0.001_DFP )
! CALL obj%initiate( param )
! CALL hdf5file%initiate( "./TemplateNewtonianFluidModel1.", mode="NEW ")
! CALL hdf5file%open()
! CALL obj%export(hdf5file, "")
! CALL hdf5file%close(); CALL hdf5file%Deallocate()
! CALL obj%Deallocate()
! CALL param%Deallocate(); CALL FPL_FINALIZE
! END PROGRAM main
!```
!
! The above program generates a [[HDF5File_]], which is given below
!
! ![](../|media|/NewtonianFluidModel_template.png ""){: width="400"}
!

INTERFACE
  MODULE SUBROUTINE nfm_Export(obj, hdf5, group)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE nfm_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Displays the content of Newtonian fluid model
!
!# Introduction
!
! This routine displays the content of [[NewtonianFluidModel_]]
!
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( ParameterList_ ) :: param
! CALL FPL_INIT; CALL param%initiate()
! CALL SetNewtonianFluidModelParam( param = param, &
! & dynamicViscosity=0.001_DFP )
! CALL obj%initiate( param )
! CALL param%print()
! CALL obj%display( msg="Test-1 : " )
! CALL obj%Deallocate()
! CALL param%Deallocate(); CALL FPL_FINALIZE
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE nfm_Display(obj, msg, unitNo)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE nfm_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetdynamicViscosity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Returns the dynamicviscosity
!
!# Introduction
!
! This routine returns the [[NewtonianFluidModel_:mu]], &
! & dynamicviscosity $\mu$
!
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! USE FPL, ONLY: ParameterList_
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( HDF5File_ ) :: hdf5file
! TYPE( ParameterList_ ) :: param
! REAL( DFP ) :: dynamicViscosity
! CALL hdf5file%initiate( "./TemplateNewtonianFluidModel1.hdf5", mode="READ")
! CALL hdf5file%open()
! CALL obj%import(hdf5file, "")
! CALL obj%GetdynamicViscosity(dynamicViscosity=dynamicViscosity)
! CALL Display( dynamicViscosity, "dynamicViscosity : ")
! CALL FPL_INIT(); CALL param%initiate()
! CALL obj%GetModelParameters(param)
! CALL param%print()
! CALL FPL_FINALIZE(); CALL param%Deallocate()
! CALL hdf5file%close(); CALL hdf5file%Deallocate()
! CALL obj%Deallocate()
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE nfm_GetdynamicViscosity(obj, dynamicViscosity)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: dynamicViscosity
  END SUBROUTINE nfm_GetdynamicViscosity
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetModelParameters@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Returns the model parameters in [[ParameterList_]]
!
!# Introduction
!
! This routine returns the model parameters in [[ParameterList_]]
!
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! USE easifemClasses
! USE easifemMaterials
! USE FPL, ONLY: ParameterList_
! IMPLICIT NONE
! TYPE( NewtonianFluidModel_ ) :: obj
! TYPE( HDF5File_ ) :: hdf5file
! TYPE( ParameterList_ ) :: param
! CALL hdf5file%initiate( "./TemplateNewtonianFluidModel1.hdf5", mode="READ")
! CALL hdf5file%open()
! CALL obj%import(hdf5file, "")
! CALL FPL_INIT(); CALL param%initiate()
! CALL obj%GetModelParameters(param)
! CALL param%print()
! CALL FPL_FINALIZE(); CALL param%Deallocate()
! CALL hdf5file%close(); CALL hdf5file%Deallocate()
! CALL obj%Deallocate()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE nfm_GetModelParameters(obj, param)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
  END SUBROUTINE nfm_GetModelParameters
END INTERFACE

!----------------------------------------------------------------------------
!                                              SetModelParameters@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Oct 2021
! summary: Sets the model parameters from param
!
!# Introduction
!
! This subroutine Sets the models parameter by reading them from param
! If an option is not available, then it does not Set that parameter.

INTERFACE
  MODULE SUBROUTINE nfm_SetModelParameters(obj, param)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
  END SUBROUTINE nfm_SetModelParameters
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION nfm_GetPrefix(obj) RESULT(ans)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION nfm_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Get Parameters

INTERFACE
  MODULE SUBROUTINE nfm_GetParam(obj, dynamicViscosity)
    CLASS(NewtonianFluidModel_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: dynamicViscosity
  END SUBROUTINE nfm_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Set Parameters

INTERFACE
  MODULE SUBROUTINE nfm_SetParam(obj, dynamicViscosity)
    CLASS(NewtonianFluidModel_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: dynamicViscosity
  END SUBROUTINE nfm_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE NewtonianFluidModel_Class
