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

MODULE DomainUtility
USE BaseType
USE BaseMethod
USE Mesh_Class
USE Domain_Class
USE DomainConnectivity_Class
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "DomainUtility"

INTERFACE Initiate
  MODULE PROCEDURE InitiateDomainPointer1
END INTERFACE Initiate

PUBLIC :: Initiate

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-02
! summary: Initiate domain pointer from file names
!
!# Introduction
!
!- Initiate domain pointer from the file names.
!- Create HDF5 file object, open it, read data and close those files.
!- Please allocate obj before using it
!- The size of `obj` should be same as the size of filename.

SUBROUTINE InitiateDomainPointer1(domains, filename, group)
  CLASS(DomainPointer_), INTENT(INOUT) :: domains(:)
  ! domain pointers to be created
  TYPE(String), INTENT(IN) :: filename(:)
  ! filenames for domain
  TYPE(String), OPTIONAL, INTENT(IN) :: group(:)
  ! Path (address) in filename
  !
  ! Internal variables
  !
  CHARACTER(*), PARAMETER :: myName = "InitiateDomainPointer1"
  TYPE(HDF5File_) :: domainFile
  INTEGER(I4B) :: ii, n
  !
  ! check
  !
  IF (SIZE(filename) .NE. SIZE(domains)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'SIZE of domains should be same as the size of filename')
  END IF
  !
  ! check
  !
  IF (PRESENT(group)) THEN
    IF (SIZE(group) .NE. SIZE(filename)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
        & 'SIZE of group should be same as the size of filename')
    END IF
  END IF
  !
  ! check
  !
  n = SIZE(filename)
  !
  DO ii = 1, n
    IF (ASSOCIATED(domains(ii)%ptr)) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
        & 'domains( '//tostring(ii)//' ) is already associated.')
    END IF
  END DO
  !
  ! compute
  !
  DO ii = 1, SIZE(filename)
    CALL domainFile%Initiate(filename=filename(ii)%chars(), MODE="READ")
    CALL domainFile%OPEN()
    IF (PRESENT(group)) THEN
      domains(ii)%ptr => Domain_Pointer(hdf5=domainFile, &
        & group=group(ii)%chars())
    ELSE
      domains(ii)%ptr => Domain_Pointer(hdf5=domainFile, group="")
    END IF
    CALL domainFile%DEALLOCATE()
  END DO
  !
END SUBROUTINE InitiateDomainPointer1

END MODULE DomainUtility
