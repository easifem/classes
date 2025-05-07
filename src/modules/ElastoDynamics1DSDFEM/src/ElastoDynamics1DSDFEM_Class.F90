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

MODULE ElastoDynamics1DSDFEM_Class
USE Abstract1DSDFEM_Class, ONLY: Abstract1DSDFEM_
USE GlobalData, ONLY: DFP, I4B, LGT

USE FPL, ONLY: ParameterList_

USE TxtFile_Class, ONLY: TxtFile_

USE ExceptionHandler_Class, ONLY: e

USE String_Class, ONLY: String

USE CSVFile_Class, ONLY: CSVFile_

USE SDAlgorithms, ONLY: SDAlgoParam_

PRIVATE

PUBLIC :: ElastoDynamics1DSDFEM_

CHARACTER(*), PARAMETER :: modName = 'ElastoDynamics1DSDFEM_Class'
CHARACTER(*), PARAMETER :: prefix = "ElastoDyanmics1DSDFEM"
CHARACTER(*), PARAMETER :: default_result_dir = "./results"
CHARACTER(*), PARAMETER :: default_filename = "ElastoDyanmics1DSDFEM"
CHARACTER(*), PARAMETER :: default_baseInterpolationForSpace = "LAGR"
CHARACTER(*), PARAMETER :: default_timeIntegrationScheme = "NEWM"
CHARACTER(*), PARAMETER :: default_baseTypeForSpace = "Monomial"
CHARACTER(*), PARAMETER :: default_ipTypeForSpace = "Equidistance"
CHARACTER(*), PARAMETER :: default_quadTypeForSpace = "GaussLegendre"
INTEGER(I4B), PARAMETER :: MAX_ORDER_SPACE = 10
INTEGER(I4B), PARAMETER :: default_verbosity = 0

!----------------------------------------------------------------------------
!                                                   ElastoDynamics1DSDFEM_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-19
! update: 2025-02-11
! summary:  Define ElastoDynamics1DSDFEM_

TYPE, EXTENDS(Abstract1DSDFEM_) :: ElastoDynamics1DSDFEM_

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate

  PROCEDURE, PUBLIC, PASS(obj) :: Run => obj_Run
  !! Debug mode

END TYPE ElastoDynamics1DSDFEM_

!----------------------------------------------------------------------------
!                              -                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-02
! summary:  Initiate by param

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(ElastoDynamics1DSDFEM_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Debug@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-08-05
! summary:  Debug mode

INTERFACE
  MODULE SUBROUTINE obj_Run(obj)
    CLASS(ElastoDynamics1DSDFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Run
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ElastoDynamics1DSDFEM_Class
