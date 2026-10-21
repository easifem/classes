!> author: Vikas Sharma, Ph. D.
! summary: This example show how to convert the domain data from [[MSHFile_]]
!          format to [[HDF5File]] format.

PROGRAM main
    USE easifemBase
    USE easifemClasses
    IMPLICIT NONE
    TYPE(MSHFile_) :: mshFile
    TYPE(HDF5File_) :: hdf5File

    CALL mshFile%Initiate("./mesh.msh", STATUS="OLD", ACTION="READ")
    CALL mshFile%Open()
    CALL mshFile%Read()

    CALL hdf5File%Initiate("./mesh.h5", MODE="NEW")
    CALL hdf5File%Open()

    CALL mshFile%Export(hdf5=hdf5File, group="")

    CALL hdf5File%Deallocate()
    CALL mshFile%Deallocate()
END PROGRAM main
