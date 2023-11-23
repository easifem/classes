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
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
PUBLIC :: AbstractNodeFieldDisplay
PUBLIC :: AbstractNodeField_
PUBLIC :: AbstractNodeFieldPointer_
PUBLIC :: AbstractNodeFieldImport
PUBLIC :: AbstractNodeFieldExport
PUBLIC :: AbstractNodeFieldGetPointer
PUBLIC :: AbstractNodeFieldInitiate2
PUBLIC :: AbstractNodeFieldDeallocate
PUBLIC :: AbstractNodeFieldSetSingle
PUBLIC :: AbstractNodeFieldGetSingle
PUBLIC :: AbstractNodeFieldInitiate
PUBLIC :: AbstractNodeFieldSetParam

CHARACTER(*), PARAMETER :: modName = "AbstractField_Class"
CHARACTER(*), PARAMETER :: myprefix = "AbstractNodeField"

!----------------------------------------------------------------------------
!                                                         AbstractNodeField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Sept 2021
! summary: Abstract node field

TYPE, ABSTRACT, EXTENDS(AbstractField_) :: AbstractNodeField_
  INTEGER(I4B) :: dof_tPhysicalVars = 0_I4B
  !! Total number of physical variables
  !! NOTE: This variable is only for internal use
  INTEGER(I4B) :: dof_storageFMT = NODES_FMT
  !! Storage format
  !! NOTE: This variable is only for internal use
  INTEGER(I4B), ALLOCATABLE :: dof_spaceCompo(:)
  !! Spatial components
  !! NOTE: This variable is only for internal use
  INTEGER(I4B), ALLOCATABLE :: dof_timeCompo(:)
  !! NOTE: This variable is only for internal use
  INTEGER(I4B), ALLOCATABLE :: dof_tNodes(:)
  !! Total number of nodes
  !! NOTE: This variable is only for internal use
  CHARACTER(1), ALLOCATABLE :: dof_names_char(:)
  !! Single character name of physical variable
  !! NOTE: This variable is only for internal use
  INTEGER(I4B) :: tSize = 0
  !! Total length of the nodal field = tdof * tNodes
  !! NOTE: This variable is only for internal use
  TYPE(RealVector_) :: realVec
  !! Vector of reals to contains the nodes
  !! NOTE: This variable is only for internal use
  TYPE(DOF_) :: dof
  !! Degree of freedom object, which contains the information about
  !! how the different components are stored inside the realVec
  !! NOTE: This variable is only for internal use
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => anf_Initiate2
  !! Initiate an instance of AbstrtactNodeField
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => anf_Initiate3
  !! Initiate an instance of AbstrtactNodeField
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => anf_Deallocate
  !! Deallocate the data stored inside

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => anf_Display
  !! Display the content of AbstractNodeField
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => anf_Import
  !! Import AbstractNodeField from HDF5File_
  PROCEDURE, PUBLIC, PASS(obj) :: Export => anf_Export
  !! Export AbstractNodeField to HDF5File_
  ! PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk => anf_WriteData_vtk
  !! Export data in VTKformat

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => anf_GetPointer
  !! GetPointer to the fortran vector stored inside the realvec
  !! This function should be called for Native engine only
  PROCEDURE, PUBLIC, PASS(obj) :: Size => anf_Size
  !! Returns the length of data stored inside the fortran vector
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => anf_Norm2
  !! Returns the L2 norm
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => anf_GetSingle
  !! Get single entry

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => anf_SetSingle
  !! Set single entry
END TYPE AbstractNodeField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractNodeFieldPointer_
  CLASS(AbstractNodeField_), POINTER :: ptr => NULL()
END TYPE AbstractNodeFieldPointer_

!----------------------------------------------------------------------------
!                                                                 SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-25
! summary:  Set parameters of AbstractNodeField_

INTERFACE AbstractNodeFieldSetParam
  MODULE SUBROUTINE anf_SetParam(obj, dof_tPhysicalVars,  &
      & dof_storageFMT, dof_spaceCompo, dof_timeCompo,  &
      & dof_tNodes, dof_names_char, tSize)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dof_tPhysicalVars
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dof_storageFMT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dof_spaceCompo(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dof_timeCompo(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dof_tNodes(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: dof_names_char(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSize
  END SUBROUTINE anf_SetParam
END INTERFACE AbstractNodeFieldSetParam

!----------------------------------------------------------------------------
!                                                             Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Initiate an instance of AbstractNodeField

INTERFACE
  MODULE SUBROUTINE AbstractNodeFieldInitiate(obj, param, dom, prefix)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE AbstractNodeFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

INTERFACE AbstractNodeFieldDisplay
  MODULE SUBROUTINE anf_Display(obj, msg, unitNo)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE anf_Display
END INTERFACE AbstractNodeFieldDisplay

!----------------------------------------------------------------------------
!                                                                 IMPORT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Import data into HDF5File_

INTERFACE AbstractNodeFieldImport
  MODULE SUBROUTINE anf_Import(obj, hdf5, group, dom, domains)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE anf_Import
END INTERFACE AbstractNodeFieldImport

!----------------------------------------------------------------------------
!                                                         Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data into HDF5File_

INTERFACE AbstractNodeFieldExport
  MODULE SUBROUTINE anf_Export(obj, hdf5, group)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE anf_Export
END INTERFACE AbstractNodeFieldExport

!----------------------------------------------------------------------------
!                                                       WriteData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data in vrkfile

! INTERFACE AbstractNodeWriteData
!   MODULE SUBROUTINE anf_WriteData_vtk(obj, vtk, group)
!     CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
!     TYPE(VTKFile_), INTENT(INOUT) :: vtk
!     CHARACTER(*), INTENT(IN) :: group
!   END SUBROUTINE anf_WriteData_vtk
! END INTERFACE AbstractNodeWriteData

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: Returns the pointer to a fortran real vector stored inside realVec

INTERFACE AbstractNodeFieldGetPointer
  MODULE FUNCTION anf_GetPointer(obj) RESULT(ans)
    CLASS(AbstractNodeField_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION anf_GetPointer
END INTERFACE AbstractNodeFieldGetPointer

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

!----------------------------------------------------------------------------
!                                                          GetSingle@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE
  MODULE SUBROUTINE anf_GetSingle(obj, indx, VALUE)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE anf_GetSingle
END INTERFACE

INTERFACE AbstractNodeFieldGetSingle
  MODULE PROCEDURE anf_GetSingle
END INTERFACE AbstractNodeFieldGetSingle

!----------------------------------------------------------------------------
!                                                                       Size
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
!
!----------------------------------------------------------------------------

END MODULE AbstractNodeField_Class
