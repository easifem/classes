!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing Export and Import of MeshSelection_ with HDF5File_

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BaseType, ONLY: math => TypeMathOpt
USE BoundingBox_Method, ONLY: BoundingBox_Initiate => Initiate
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE MeshSelection_Class, ONLY: MeshSelection_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./hdf5/Export_test_1.hdf5"
TYPE(MeshSelection_) :: obj, obj2
TYPE(HDF5File_) :: hdf5file
INTEGER(I4B) :: ans(8), tsize

CALL e%SetQuietMode(math%yes)

CALL obj%Initiate()
CALL obj%Add(dim=0, meshID=[1, 2])
CALL obj%Add(dim=1, meshID=[5])
CALL obj%Add(dim=2, meshID=[3, 8])
CALL obj%Add(dim=3, meshID=[4])
CALL obj%Add(dim=2, elemNum=[20, 18])
CALL obj%Add(dim=1, nodeNum=[5, 4])
CALL obj%Add(nodeNum=[9])
CALL obj%Set()

! Test 1: Export writes the flags and the lists
BLOCK
  CALL hdf5file%Initiate(filename=filename, mode="NEW")
  CALL hdf5file%OPEN()
  CALL obj%Export(hdf5=hdf5file, group="selection")
  CALL OK(hdf5file%PathExists("selection/isSelectionByMeshID"), &
          "Export: flag")
  CALL OK(hdf5file%PathExists("selection/SurfaceMeshID"), "Export: meshID")
  CALL OK(hdf5file%PathExists("selection/SurfaceElemNum"), "Export: elemNum")
  CALL OK(hdf5file%PathExists("selection/NodeNum"), "Export: nodeNum")
  CALL OK(.NOT. hdf5file%PathExists("selection/VolumeElemNum"), &
          "Export: skips empty lists")
  CALL hdf5file%DEALLOCATE()
END BLOCK

! Test 2: Import returns what Export wrote
BLOCK
  CALL hdf5file%Initiate(filename=filename, mode="READ")
  CALL hdf5file%OPEN()
  CALL obj2%IMPORT(hdf5=hdf5file, group="selection")
  CALL hdf5file%DEALLOCATE()

  CALL OK(obj2%IsInitiated(), "Import: IsInitiated")
  CALL OK(obj2%IsSelectionByMeshID(), "Import: meshID flag")
  CALL OK(obj2%IsSelectionByElemNum(), "Import: elemNum flag")
  CALL OK(obj2%IsSelectionByNodeNum(), "Import: nodeNum flag")
  CALL OK(.NOT. obj2%IsSelectionByBox(), "Import: box flag")
  CALL OK(ALL(obj2%GetMeshID(dim=0) .EQ. [1, 2]), "Import: meshID dim=0")
  CALL OK(ALL(obj2%GetMeshID(dim=1) .EQ. [5]), "Import: meshID dim=1")
  CALL OK(ALL(obj2%GetMeshID(dim=2) .EQ. [3, 8]), "Import: meshID dim=2")
  CALL OK(ALL(obj2%GetMeshID(dim=3) .EQ. [4]), "Import: meshID dim=3")

  CALL obj2%GetElemNum(dim=2, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "Import: size of elemNum")
  CALL OK(ALL(ans(1:tsize) .EQ. [18, 20]), "Import: elemNum")

  CALL obj2%GetNodeNum(ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 3, "Import: size of nodeNum")
  CALL OK(ALL(ans(1:tsize) .EQ. [4, 5, 9]), "Import: nodeNum")
END BLOCK

! Test 3: Export skips an instance which is not initiated
BLOCK
  TYPE(MeshSelection_) :: empty

  CALL hdf5file%Initiate(filename=filename, mode="READ")
  CALL hdf5file%OPEN()
  CALL empty%Export(hdf5=hdf5file, group="empty")
  CALL OK(.NOT. hdf5file%PathExists("empty"), "Export: not initiated")
  CALL hdf5file%DEALLOCATE()
END BLOCK

! Test 4: the box flag is written, the boxes are not
BLOCK
  TYPE(BoundingBox_) :: box(1)
  TYPE(MeshSelection_) :: withBox, back

  CALL BoundingBox_Initiate(box(1), nsd=2_I4B, &
                            lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, &
                                 0.0_DFP, 0.0_DFP])
  CALL withBox%Initiate()
  CALL withBox%Add(dim=2, box=box)

  CALL hdf5file%Initiate(filename=filename, mode="OVERWRITE")
  CALL hdf5file%OPEN()
  CALL withBox%Export(hdf5=hdf5file, group="box")
  CALL hdf5file%DEALLOCATE()

  CALL hdf5file%Initiate(filename=filename, mode="READ")
  CALL hdf5file%OPEN()
  CALL back%IMPORT(hdf5=hdf5file, group="box")
  CALL hdf5file%DEALLOCATE()
  CALL OK(back%IsSelectionByBox(), "Import: box flag is restored")
END BLOCK

CALL obj%DEALLOCATE()
CALL obj2%DEALLOCATE()
END PROGRAM main
