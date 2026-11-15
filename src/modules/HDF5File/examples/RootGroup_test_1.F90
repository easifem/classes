!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: The root group "/" of an HDF5 file
!
! The root group always exists: PathExists("/") and IsGroup("/") are
! true, Ls("/") lists the top-level objects, and attributes can be
! written to and read from "/" (file-level metadata). Paths with double
! slashes are accepted.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "RootGroup_test_1.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String), ALLOCATABLE :: objs(:)
  TYPE(String) :: s1
  INTEGER(I4B) :: version

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%Mkdir("/mesh")
  CALL obj%WRITE(dsetname="/mesh/x", vals=1.0_DFP)

  CALL OK(obj%PathExists("/"), "PathExists('/')")
  CALL OK(obj%IsGroup("/"), "IsGroup('/')")
  CALL OK(obj%PathExists("//mesh//x"), "path with double slashes")
  CALL OK(.NOT. obj%PathExists("/none"), "missing path")

  CALL obj%Ls(path="/", objs=objs)
  CALL OK(ALLOCATED(objs), "Ls('/'): allocated")
  IF (ALLOCATED(objs)) THEN
    CALL OK(SIZE(objs) .EQ. 1, "Ls('/'): one object")
    CALL OK(objs(1)%chars() .EQ. "mesh", "Ls('/'): name")
  END IF

  ! file-level metadata as attributes of the root group
  CALL obj%WriteAttribute(obj_name="/", attr_name="format", &
                          attr_val="easifem")
  CALL obj%WriteAttribute(obj_name="/", attr_name="version", attr_val=2)
  CALL OK(obj%AttributeExists("/", "format"), "attribute of '/' exists")
  CALL obj%ReadAttribute(obj_name="/", attr_name="format", attr_val=s1)
  CALL obj%ReadAttribute(obj_name="/", attr_name="version", &
                         attr_val=version)
  CALL OK(s1%chars() .EQ. "easifem", "String attribute of '/'")
  CALL OK(version .EQ. 2, "integer attribute of '/'")

  CALL obj%DEALLOCATE()
END PROGRAM main
