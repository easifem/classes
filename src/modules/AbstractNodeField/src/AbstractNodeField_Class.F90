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

MODULE AbstractNodeField_Class
USE GlobalData
USE BaseType
USE RealVector_Method
USE DOF_Method
USE AbstractField_Class
USE FPL, ONLY: ParameterList_
USE Domain_Class, ONLY: DomainPointer_, Domain_
USE HDF5File_Class, ONLY: HDF5File_
USE VTKFile_Class, ONLY: VTKFile_
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractField_Class"

!----------------------------------------------------------------------------
!                                                         AbstractNodeField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Sept 2021
! summary: Abstract node field

TYPE, ABSTRACT, EXTENDS(AbstractField_) :: AbstractNodeField_
  INTEGER(I4B) :: tSize = 0
  !! Total length of the nodal field = tdof * tNodes
  TYPE(RealVector_) :: realVec
  !! Vector of reals to contains the nodes
  TYPE(DOF_) :: dof
  !! Degree of freedom object, which contains the information about
  !! how the different components are stored inside the realVec
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Display => anf_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => anf_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => anf_Export
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => anf_GetPointer
  PROCEDURE, PUBLIC, PASS(obj) :: Size => anf_Size
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => anf_Initiate2
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => anf_Initiate3
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => anf_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => anf_Norm2
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => anf_SetSingle
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => anf_GetSingle
END TYPE AbstractNodeField_

PUBLIC :: AbstractNodeField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractNodeFieldPointer_
  CLASS(AbstractNodeField_), POINTER :: ptr => NULL()
END TYPE AbstractNodeFieldPointer_

PUBLIC :: AbstractNodeFieldPointer_

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE anf_Display(obj, msg, unitNo)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE anf_Display
END INTERFACE

INTERFACE AbstractNodeFieldDisplay
  MODULE PROCEDURE anf_Display
END INTERFACE AbstractNodeFieldDisplay

PUBLIC :: AbstractNodeFieldDisplay

!----------------------------------------------------------------------------
!                                                                 IMPORT
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE anf_Import(obj, hdf5, group, dom, domains)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE anf_Import
END INTERFACE

INTERFACE AbstractNodeFieldImport
  MODULE PROCEDURE anf_Import
END INTERFACE AbstractNodeFieldImport

PUBLIC :: AbstractNodeFieldImport

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE anf_Export(obj, hdf5, group)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE anf_Export
END INTERFACE

INTERFACE AbstractNodeFieldExport
  MODULE PROCEDURE anf_Export
END INTERFACE AbstractNodeFieldExport

PUBLIC :: AbstractNodeFieldExport

!----------------------------------------------------------------------------
!                                                                getPointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: Returns the pointer to a fortran real vector stored inside realVec

INTERFACE
  MODULE FUNCTION anf_getPointer(obj) RESULT(ans)
    CLASS(AbstractNodeField_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION anf_getPointer
END INTERFACE

INTERFACE AbstractNodeFieldGetPointer
  MODULE PROCEDURE anf_getPointer
END INTERFACE AbstractNodeFieldGetPointer

PUBLIC :: AbstractNodeFieldGetPointer

!----------------------------------------------------------------------------
!                                                                    Size
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: This function returns the size of the field

INTERFACE
  MODULE FUNCTION anf_Size(obj, dims) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION anf_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                            anf_Initiate3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: Initiate AbstractNodeField_ from another instance
!
!# Introduction
!
! This method initiates an AbstractNodeField_ instance
! by copying all or some contents from another instance of AbstractNodeField_
!
! If obj is not initiated then we copy everything
! For domain and domains we always use pointers
!
! If obj is initiated then we only copy the data stored in realvec
!
!
! Currently, copyStructure and usePointer is not used

INTERFACE
  MODULE SUBROUTINE anf_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE anf_Initiate2
END INTERFACE

INTERFACE AbstractNodeFieldInitiate2
  MODULE PROCEDURE anf_Initiate2
END INTERFACE AbstractNodeFieldInitiate2

PUBLIC :: AbstractNodeFieldInitiate2

!----------------------------------------------------------------------------
!                                                            anf_Initiate3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: Initiates AbstractNodeField_ from parameters and domain

INTERFACE
  MODULE SUBROUTINE anf_Initiate3(obj, param, dom)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE anf_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Oct 2021
! summary: Deallocates data in [[AbstractNodeField_]]

INTERFACE
  MODULE SUBROUTINE anf_Deallocate(obj)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
  END SUBROUTINE anf_Deallocate
END INTERFACE

INTERFACE AbstractNodeFieldDeallocate
  MODULE PROCEDURE anf_Deallocate
END INTERFACE AbstractNodeFieldDeallocate

PUBLIC :: AbstractNodeFieldDeallocate

!----------------------------------------------------------------------------
!                                                                    Norm2
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Jan 2022
! summary: This function returns NORM2

INTERFACE
  MODULE FUNCTION anf_Norm2(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION anf_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetSingle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE
  MODULE SUBROUTINE anf_setSingle(obj, indx, VALUE, scale, &
    & addContribution)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE anf_setSingle
END INTERFACE

INTERFACE AbstractNodeFieldSetSingle
  MODULE PROCEDURE anf_setSingle
END INTERFACE AbstractNodeFieldSetSingle

PUBLIC :: AbstractNodeFieldSetSingle

!----------------------------------------------------------------------------
!                                                          GetSingle@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE
  MODULE SUBROUTINE anf_getSingle(obj, indx, VALUE)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE anf_getSingle
END INTERFACE

INTERFACE AbstractNodeFieldGetSingle
  MODULE PROCEDURE anf_getSingle
END INTERFACE AbstractNodeFieldGetSingle

PUBLIC :: AbstractNodeFieldGetSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractNodeField_Class
