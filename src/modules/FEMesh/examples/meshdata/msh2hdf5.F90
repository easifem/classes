PROGRAM main
USE GlobalData
USE MSHFIle_Class
USE HDF5File_Class
USE Display_Method

IMPLICIT NONE

CHARACTER(*), PARAMETER :: title = "very_small_quad4_mesh"
INTEGER(I4B) :: ierr
TYPE(MSHFile_) :: mshFile
TYPE(HDF5File_) :: hdf5file

CALL Display("flag 1")

CALL mshFile%Initiate(filename=title//'.msh', STATUS="OLD", ACTION="READ")

CALL Display("flag 2")

CALL mshFile%OPEN()
CALL mshFile%READ()

CALL Display("flag 2")

CALL hdf5file%Initiate(title//'.h5', MODE="NEW")
CALL hdf5file%OPEN()

CALL Display("flag 3")

CALL mshFile%Export(hdf5=hdf5file, group="")

CALL Display("flag 4")

CALL mshFile%DEALLOCATE()
CALL hdf5file%DEALLOCATE()

END PROGRAM main
