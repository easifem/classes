! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE ElasticityOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: ElasticityOpt_
PUBLIC :: TypeElasticityOpt

CHARACTER(*), PARAMETER :: modName = "ElasticityOpt_Class"

!----------------------------------------------------------------------------
!                                                              ElasticityOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary:  Options for elasticity models

TYPE :: ElasticityOpt_
  INTEGER(I4B) :: isotropic = 1
  INTEGER(I4B) :: anisotropic = 2
  INTEGER(I4B) :: orthotropic = 3
  INTEGER(I4B) :: transIsotropic = 4
  INTEGER(I4B) :: size_c_plane_stress = 3
  INTEGER(I4B) :: size_c_plane_strain = 3
  CHARACTER(3) :: isotropic_char = "ISO"
  CHARACTER(5) :: anisotropic_char = "ANISO"
  CHARACTER(5) :: orthotropic_char = "ORTHO"
  CHARACTER(5) :: transIsotropic_char = "TRANS"

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => obj_ToNumber
    !! Convert name of elasticity type to number
  PROCEDURE, PUBLIC, PASS(obj) :: ToString => obj_ToString
    !! Convert integer elasticity name to string
END TYPE ElasticityOpt_

!----------------------------------------------------------------------------
!                                                         TypeElasticityOpt
!----------------------------------------------------------------------------

TYPE(ElasticityOpt_), PARAMETER :: TypeElasticityOpt = ElasticityOpt_()

!----------------------------------------------------------------------------
!                                                                   ToNumber
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns the elasticity number

INTERFACE
  MODULE FUNCTION obj_ToNumber(obj, name) RESULT(ans)
    CLASS(ElasticityOpt_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_ToNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    ToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Returns the elasticity number

INTERFACE
  MODULE FUNCTION obj_ToString(obj, name) RESULT(ans)
    CLASS(ElasticityOpt_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_ToString
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ElasticityOpt_Class
