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
USE Basetype
USE RealVector_Method
USE DOF_Method
USE AbstractField_Class
USE FPL, ONLY: ParameterList_
USE Domain_Class, ONLY: DomainPointer_, Domain_
USE HDF5File_Class, ONLY: HDF5File_
USE VTKFile_Class, ONLY: VTKFile_
USE ExceptionHandler_Class, ONLY: e
USE AbstractBC_Class, ONLY: AbstractBC_
USE DirichletBC_Class, ONLY: DirichletBCPointer_, DirichletBC_
USE UserFunction_Class, ONLY: UserFunction_
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
PUBLIC :: AbstractNodeFieldGetFEVariable
PUBLIC :: NodeFieldsWriteData

CHARACTER(*), PARAMETER :: modName = "AbstractNodeField_Class"
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
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate an instance of AbstrtactNodeField
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate an instance of AbstrtactNodeField
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate an instance of AbstrtactNodeField
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data stored inside

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of AbstractNodeField
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import AbstractNodeField from HDF5File_
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export AbstractNodeField to HDF5File_
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk => obj_WriteData1_vtk
  PROCEDURE, NOPASS :: WriteData_vtk_ptrs_vec =>  &
    & obj_WriteData2_vtk
  GENERIC, PUBLIC :: WriteData => WriteData_vtk_ptrs_vec
  !! Export data in VTKformat

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! GetPointer to the fortran vector stored inside the realvec
  !! This function should be called for Native engine only
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Returns the length of data stored inside the fortran vector
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => obj_Norm2
  !! Returns the L2 norm
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
  !! Get single entry
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get Finite Element variable
  PROCEDURE, PUBLIC, PASS(obj) :: GetPhysicalNames => obj_GetPhysicalNames
  !! Get physical names
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalPhysicalVars =>  &
    & obj_GetTotalPhysicalVars
  !! Get total physical variables
  PROCEDURE, PUBLIC, PASS(obj) :: GetSpaceCompo => obj_GetSpaceCompo
  !! Get GetSpaceCompo
  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeCompo => obj_GetTimeCompo
  !! Get the time components
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeLoc1 => obj_GetNodeLoc1
  !! Get location of global node number
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeLoc2 => obj_GetNodeLoc2
  !! Get location of global node number from AbstractBC
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeLoc3 => obj_GetNodeLoc3
  !! Get location of global node number from DirichletBCPointer
  GENERIC, PUBLIC :: GetNodeLoc => GetNodeLoc1, GetNodeLoc2,  &
    & GetNodeLoc3
  !! Generic method for getting location of nodes

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  !! Set single entry
  PROCEDURE, PUBLIC, PASS(obj) :: SetByFunction => obj_SetByFunction
  !! Set by user function
  GENERIC, PUBLIC :: Set => SetByFunction

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, PASS(obj) :: ApplyDirichletBC1 => obj_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: ApplyDirichletBC2 => obj_ApplyDirichletBC2
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    & ApplyDirichletBC2

  ! GET:
  ! @BlasMethods
  PROCEDURE, PUBLIC, PASS(obj) :: AXPY => obj_AXPY
  PROCEDURE, PUBLIC, PASS(obj) :: SCAL => obj_SCAL
  PROCEDURE, PUBLIC, PASS(obj) :: COPY => obj_Copy

END TYPE AbstractNodeField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractNodeFieldPointer_
  CLASS(AbstractNodeField_), POINTER :: ptr => NULL()
END TYPE AbstractNodeFieldPointer_

!----------------------------------------------------------------------------
!                                               CheckError@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE AbstractNodeFieldCheckError(obj)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
  END SUBROUTINE AbstractNodeFieldCheckError
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate the field by reading param and given domain

INTERFACE AbstractNodeFieldInitiate
  MODULE SUBROUTINE obj_Initiate1(obj, param, dom)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate1
END INTERFACE AbstractNodeFieldInitiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
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

INTERFACE AbstractNodeFieldInitiate2
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE AbstractNodeFieldInitiate2

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: Initiates AbstractNodeField_ from parameters and domain

INTERFACE AbstractNodeFieldInitiate
  MODULE SUBROUTINE obj_Initiate3(obj, param, dom)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE obj_Initiate3
END INTERFACE AbstractNodeFieldInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Oct 2021
! summary: Deallocates data in [[AbstractNodeField_]]

INTERFACE AbstractNodeFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractNodeFieldDeallocate

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Display the content of AbstractNodeField

INTERFACE AbstractNodeFieldDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE AbstractNodeFieldDisplay

!----------------------------------------------------------------------------
!                                                         IMPORT@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Import data into HDF5File_

INTERFACE AbstractNodeFieldImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom, domains)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE obj_Import
END INTERFACE AbstractNodeFieldImport

!----------------------------------------------------------------------------
!                                                         Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data into HDF5File_

INTERFACE AbstractNodeFieldExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE AbstractNodeFieldExport

!----------------------------------------------------------------------------
!                                                       WriteData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data in vtkfile

INTERFACE AbstractNodeFieldWriteData
  MODULE SUBROUTINE obj_WriteData1_vtk(obj, vtk)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
  END SUBROUTINE obj_WriteData1_vtk
END INTERFACE AbstractNodeFieldWriteData

!----------------------------------------------------------------------------
!                                                       WriteData@IOMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-21
! summary:  Export data in vtkfile

INTERFACE AbstractNodeFieldWriteData
  MODULE SUBROUTINE obj_WriteData2_vtk(obj, vtk)
    CLASS(AbstractNodeFieldPointer_), INTENT(INOUT) :: obj(:)
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
  END SUBROUTINE obj_WriteData2_vtk
END INTERFACE AbstractNodeFieldWriteData

INTERFACE NodeFieldsWriteData
  MODULE PROCEDURE obj_WriteData2_vtk
END INTERFACE NodeFieldsWriteData

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: Returns the pointer to a fortran real vector stored inside realVec

INTERFACE AbstractNodeFieldGetPointer
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(AbstractNodeField_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointer
END INTERFACE AbstractNodeFieldGetPointer

!----------------------------------------------------------------------------
!                                                          Norm2@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Jan 2022
! summary: This function returns NORM2

INTERFACE
  MODULE FUNCTION obj_Norm2(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-25
! summary:  Set parameters of AbstractNodeField_

INTERFACE AbstractNodeFieldSetParam
  MODULE SUBROUTINE obj_SetParam(obj, dof_tPhysicalVars,  &
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
  END SUBROUTINE obj_SetParam
END INTERFACE AbstractNodeFieldSetParam

!----------------------------------------------------------------------------
!                                                       SetSingle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE AbstractNodeFieldSetSingle
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, &
    & addContribution)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetSingle
END INTERFACE AbstractNodeFieldSetSingle

!----------------------------------------------------------------------------
!                                                   SetByFunction@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction(obj, func, times, ivar, idof,  &
    & spaceCompo, timeCompo)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: func
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE obj_SetByFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE AbstractNodeFieldGetSingle
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE obj_GetSingle
END INTERFACE AbstractNodeFieldGetSingle

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE AbstractNodeFieldGetFEVariable
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, VALUE, ivar)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE obj_GetFeVariable
END INTERFACE AbstractNodeFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                GetPhysicalNames@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns the names of physical variables

INTERFACE
  MODULE SUBROUTINE obj_GetPhysicalNames(obj, ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(INOUT) :: ans(:)
  END SUBROUTINE obj_GetPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalPhysicalVars@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-03
! summary:  Returns the total number of physical variables

INTERFACE
  MODULE FUNCTION obj_GetTotalPhysicalVars(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalPhysicalVars
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetSpaceCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns space components

INTERFACE
  MODULE FUNCTION obj_GetSpaceCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
      !! Total number of physical variables
      !! This can be obtained from GetTotalPhysicalVars method
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetSpaceCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTimeCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns Time components

INTERFACE
  MODULE FUNCTION obj_GetTimeCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTimeCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Size@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: This function returns the size of the field

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode

INTERFACE
  MODULE FUNCTION obj_GetNodeLoc1(obj, globalNode, ivar, spaceCompo,  &
    & timeCompo) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical varibale number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! list of space components
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! list of time components
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeLoc1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE FUNCTION obj_GetNodeLoc2(obj, dbc, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    CLASS(AbstractBC_), INTENT(INOUT) :: dbc
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeLoc2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE FUNCTION obj_GetNodeLoc3(obj, dbc, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeLoc3
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times, ivar, extField)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times, ivar, extField)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-17
! summary:  y = y + s * x

INTERFACE
  MODULE SUBROUTINE obj_AXPY(obj, x, scale)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_AXPY
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-17
! summary:  y = y + s * x

INTERFACE
  MODULE SUBROUTINE obj_SCAL(obj, scale)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_SCAL
END INTERFACE

!----------------------------------------------------------------------------
!                                                         COPY@BlasMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Copy obj=obj2

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractNodeField_Class
