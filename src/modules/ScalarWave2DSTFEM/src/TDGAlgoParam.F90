MODULE TDGAlgoParam
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: QuadraturePoint_, &
                    ElemshapeData_, &
                    poly => TypePolynomialOpt, &
                    qp => TypeQuadratureOpt, &
                    ip => TypeInterpolationOpt, &
                    CSRMatrix_, &
                    DOF_, &
                    RealVector_, &
                    iface_SpaceTimeFunction, &
                    iface_1DFunction, &
                    FEVariable_
USE tomlf, ONLY: toml_table, &
                 toml_serialize, &
                 toml_get => get_value, &
                 toml_stat, toml_array, &
                 toml_len => len
USE FPL, ONLY: ParameterList_
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display, ToString
USE String_Class, ONLY: String
USE UserFunction_Class, ONLY: UserFunction_
USE ReallocateUtility, ONLY: Reallocate
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE CSVFile_Class, ONLY: CSVFile_

PRIVATE

PUBLIC :: TDGAlgoParam_
PUBLIC :: ElementDataImportFromToml

CHARACTER(*), PARAMETER :: modName = 'ScalarWave2DSTFEM_Class'
CHARACTER(*), PARAMETER :: default_result_dir = "./results"
CHARACTER(*), PARAMETER :: default_filename = "ScalarWave2DSTFEM"
CHARACTER(*), PARAMETER :: default_baseInterpolation = "LAGR"
CHARACTER(*), PARAMETER :: default_timeIntegrationScheme = "NEWM"
CHARACTER(*), PARAMETER :: default_baseType = "Monomial"
CHARACTER(*), PARAMETER :: default_ipType = "Equidistance"
CHARACTER(*), PARAMETER :: default_quadType = "GaussLegendre"
CHARACTER(*), PARAMETER :: default_engineName = "NATIVE_SERIAL"
INTEGER(I4B), PARAMETER :: MAX_ORDER_TIME = 10
INTEGER(I4B), PARAMETER :: default_verbosity = 0
INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B), PARAMETER :: spaceCompo = 1
INTEGER(I4B), PARAMETER :: timeCompo = 1

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

#ifdef DEBUG_VER
LOGICAL(LGT), PARAMETER :: debug = .TRUE.
#else
LOGICAL(LGT), PARAMETER :: debug = .FALSE.
#endif

INTERFACE elementdataimportfromtoml
  MODULE PROCEDURE ElementDataImportFromToml_real
  MODULE PROCEDURE ElementDataImportFromToml_int
END INTERFACE ElementDataImportFromToml

!----------------------------------------------------------------------------
!                                                   ScalarWave2DSTFEM_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Define TDGAlgoParam_

TYPE :: TDGAlgoParam_
  CHARACTER(2) :: baseContinuity = "H1"
  CHARACTER(4) :: baseInterpolation = "LAGR"
  INTEGER(I4B) :: baseType = poly%monomial
  INTEGER(I4B) :: ipType = ip%Equidistance
  INTEGER(I4B) :: quadType = qp%GaussLegendre

  INTEGER(I4B) :: nnt = 0
  REAL(DFP) :: tij(1, MAX_ORDER_TIME + 1)
  REAL(DFP) :: currentTime = 0.0_DFP
  REAL(DFP) :: timeDomain(2)
  !! current time
  INTEGER(I4B) :: currentTimeStep = 1
  !! current time step
  INTEGER(I4B) :: totalTimeElements = 0
  INTEGER(I4B) :: totalTimeNodes = 0

  INTEGER(I4B), ALLOCATABLE :: order(:)
  !! time order of each element
  REAL(DFP), ALLOCATABLE :: elemLength(:)
  !! length of each time element
  !! the size should be totalTimeElements
  INTEGER(I4B) :: maxOrder = 0

  TYPE(QuadraturePoint_) :: quad
  !! Quadrature points in time

  TYPE(ElemshapeData_) :: linElemsd
  !! linearElement shape data for time

  TYPE(ElemshapeData_) :: elemsd
  !! Element shape data for time

  REAL(DFP) :: timeShapeFuncBndy(MAX_ORDER_TIME + 1, 2)
  !! Value of time shape function at theta = -1

  REAL(DFP) :: timeShapeFuncGradBndy(MAX_ORDER_TIME + 1, 2)
  !! Value of time shape function at theta = -1

  REAL(DFP) :: ct(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! Ct matrix, see notes

  REAL(DFP) :: mt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! mass matrix in time

  REAL(DFP) :: mtplus(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! mass like matrix (computed at -1)

  REAL(DFP) :: kt_tilda(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! space-time displacement matrix

  REAL(DFP) :: bt(MAX_ORDER_TIME + 1, 2 * MAX_ORDER_TIME + 2)
  !! bt matrix, see notes

  REAL(DFP) :: bt_right(MAX_ORDER_TIME + 1)
  !! bt at theta +1

  REAL(DFP) :: wt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  REAL(DFP) :: wmt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! transpose of wt*mt
  !! needed to make bt
  !! this matrix is made when we call GetWt

  REAL(DFP) :: at(MAX_ORDER_TIME + 1)
  !! At matrix

  REAL(DFP) :: at_right
  !! At matrix at +1

  REAL(DFP) :: tat(MAX_ORDER_TIME + 1)
  !! integral of shape function of time times at

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  PROCEDURE, PUBLIC, PASS(obj) :: SetTij => obj_SetTij

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuad => obj_SetQuad
  !! Set quadrature points for time

  PROCEDURE, PUBLIC, PASS(obj) :: SetElemsd => obj_SetElemsd
  !! Set element shape data for time

  PROCEDURE, PUBLIC, PASS(obj) :: GetMt => obj_GetMt
  !! Get Mt matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetMtPlus => obj_GetMtPlus
  !! Get MtPlus matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetCt => obj_GetCt
  !! Get Ct matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetWt => obj_GetWt
  !! Get Wt matrix
  !! It needs Ct, Mt, Mtplus

  PROCEDURE, PUBLIC, PASS(obj) :: GetAt => obj_GetAt
  !! Get At matrix
  !! it needs wt

  PROCEDURE, PUBLIC, PASS(obj) :: GetBt => obj_GetBt
  !! Get Bt matrix
  !! It needs Wt

  PROCEDURE, PUBLIC, PASS(obj) :: GetKt_Tilda => obj_GetKt_Tilda
  !! Get Kt_tilda matrix

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml => obj_ImportFromToml1

END TYPE TDGAlgoParam_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-29
! summary:  Display TDGAlgoParam_ information

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-29
! summary:  Import TDGAlgoParam_ from toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Set TDGAlgoParam_

INTERFACE
  MODULE SUBROUTINE obj_Set(obj, timeElemNum)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-26
! summary:  Set TDGAlgoParam_

INTERFACE
  MODULE SUBROUTINE obj_SetTij(obj, timeElemNum)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
  END SUBROUTINE obj_SetTij
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Set quadrature points for time

INTERFACE
  MODULE SUBROUTINE obj_SetQuad(obj, timeElemNum)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
  END SUBROUTINE obj_SetQuad
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetElemsd@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Set element shape data for time

INTERFACE
  MODULE SUBROUTINE obj_SetElemsd(obj, timeElemNum, tij)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
    REAL(DFP), INTENT(IN) :: tij(1, 2)
  END SUBROUTINE obj_SetElemsd
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetCt@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Get Ct matrix

INTERFACE
  MODULE SUBROUTINE obj_GetCt(obj, ans, nrow, ncol)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetCt
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetMt@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Get Mt matrix

INTERFACE
  MODULE SUBROUTINE obj_GetMt(obj, ans, nrow, ncol)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMt
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetMtPlus@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Get MtPlus matrix

INTERFACE
  MODULE SUBROUTINE obj_GetMtPlus(obj, ans, nrow, ncol)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMtPlus
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetAt@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Get At matrix

INTERFACE
  MODULE SUBROUTINE obj_GetAt(obj)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_GetAt
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetBt@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Get Bt matrix

INTERFACE
  MODULE SUBROUTINE obj_GetBt(obj)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_GetBt
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetWt@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Get Wt matrix

INTERFACE
  MODULE SUBROUTINE obj_GetWt(obj, ans, nrow, ncol)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetWt
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetKt_Tilda@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-25
! summary:  Get Kt_tilda

INTERFACE
  MODULE SUBROUTINE obj_GetKt_Tilda(obj, ans, nrow, ncol)
    CLASS(TDGAlgoParam_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetKt_Tilda
END INTERFACE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ElementDataImportFromToml_real(table, key, val, telem, isfound)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:)
  INTEGER(I4B), INTENT(IN) :: telem
  LOGICAL(LGT), INTENT(INOUT) :: isfound

  CHARACTER(*), PARAMETER :: myName = "ElementDataImportFromToml()"
  TYPE(toml_array), POINTER :: array
  INTEGER(I4B) :: stat, origin, ii, tsize, ncol, &
                  iostat
  LOGICAL(LGT) :: isOk
  REAL(DFP) :: temp
  TYPE(String) :: filename, ext
  TYPE(TxtFile_) :: atxtfile
  TYPE(CSVFile_) :: acsvfile
  CHARACTER(512) :: iomsg
  INTEGER(I4B), ALLOCATABLE :: intvec1(:), intvec2(:)
  REAL(DFP), ALLOCATABLE :: realvec(:)

  IF (debug) CALL Display(myName//key)

  CALL toml_get(table, key, array, origin=origin, stat=stat, &
                requested=.FALSE.)

  IF (ASSOCIATED(array)) THEN
    tsize = toml_len(array)
    CALL Reallocate(val, tsize)
    DO ii = 1, tsize
      CALL toml_get(array, ii, val(ii))
    END DO
    NULLIFY (array)
    isfound = .TRUE.
    RETURN
  END IF

  CALL toml_get(table, key, temp, origin=origin, stat=stat)

  IF (stat .EQ. toml_stat%success) THEN
    CALL Reallocate(val, 1)
    val(1) = temp
    isfound = .TRUE.
    RETURN
  END IF

  CALL toml_get(table, key, filename%raw, origin=origin, stat=stat)

  IF (stat .EQ. toml_stat%success) THEN
    ext = filename%extension()
    SELECT CASE (ext%chars())
    CASE (".txt")
      CALL atxtfile%Initiate(filename=filename%Chars(), &
                             action="READ", status="OLD")
      CALL atxtfile%OPEN()
      CALL atxtfile%READ(val=val, iostat=iostat, iomsg=iomsg)

      isok = iostat .NE. 0 .AND. (.NOT. atxtfile%isEOF())
      IF (isok) THEN
        STOP "error occur "
        filename = ""
        RETURN
      END IF

      CALL atxtfile%DEALLOCATE()
      filename = ""
      isfound = .TRUE.
      RETURN
    CASE (".csv")
      CALL acsvfile%Initiate(filename=filename%Chars(), &
                             action="READ", status="OLD", &
                             delimiter=",")
      CALL acsvfile%OPEN()
      CALL acsvfile%SetHeaderIndx(1)
      CALL acsvfile%READ()
      ncol = acsvfile%Getncols()
      SELECT CASE (ncol)
      CASE (1) ! only value

        CALL acsvfile%get(1, val=realvec) ! value
        isOk = ALLOCATED(realvec)
        CALL AssertError_(isok, myname, &
                          "value column does not have data")
        CALL reallocate(val, 1)
        val(1) = realvec(1)

      CASE (2) ! element number and value

        CALL acsvfile%get(1, val=intvec1) ! element
        CALL acsvfile%get(2, val=realvec) ! value

        isok = ALLOCATED(intvec1) .AND. ALLOCATED(realvec)
        CALL AssertError_(isok, myname, &
                          "element column or value column "// &
                          " does not have data")
        isok = SIZE(intvec1) .EQ. telem
        CALL AssertError_(isok, myname, "the size of elemen column "// &
                          " should be the same as the number of elements")
        CALL reallocate(val, telem)
        DO ii = 1, telem
          val(intvec1(ii)) = realvec(ii)
        END DO

      CASE (3) ! start, end, value

        CALL acsvfile%get(1, val=intvec1) ! start
        CALL acsvfile%get(2, val=intvec2) ! end
        CALL acsvfile%get(3, val=realvec) ! value
        isok = ALLOCATED(intvec1) .AND. ALLOCATED(realvec) &
               .AND. ALLOCATED(intvec2)
        CALL AssertError_(isok, myname, &
                          "start column, end column or value column"// &
                          " does not have data")
        isok = MINVAL(intvec1) .EQ. one
        CALL AssertError_(isok, myname, "start must have 1 as a component")
        isok = MAXVAL(intvec2) .EQ. telem
        CALL AssertError_(isok, myname, "end must have the integer"// &
                          " which is the same as the number of elements")

        CALL reallocate(val, telem)
        DO ii = 1, SIZE(intvec1)
          val(intvec1(ii):intvec2(ii)) = realvec(ii)
        END DO

      CASE default
        CALL AssertError_(.FALSE., myname, &
                          "wrong number of columns in csv file ")
      END SELECT

      CALL acsvfile%DEALLOCATE()
      filename = ""
      isfound = .TRUE.
      RETURN
    END SELECT
  END IF

  isfound = .FALSE.

END SUBROUTINE ElementDataImportFromToml_real

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ElementDataImportFromToml_int(table, key, val, telem, isfound)
  TYPE(toml_table), INTENT(INOUT) :: table
  CHARACTER(*), INTENT(IN) :: key
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: val(:)
  INTEGER(I4B), INTENT(IN) :: telem
  LOGICAL(LGT), INTENT(INOUT) :: isfound

  CHARACTER(*), PARAMETER :: myName = "ElementDataImportFromToml()"
  TYPE(toml_array), POINTER :: array
  INTEGER(I4B) :: stat, origin, ii, tsize, ncol, &
                  iostat, temp
  LOGICAL(LGT) :: isOk
  TYPE(String) :: filename, ext
  TYPE(TxtFile_) :: atxtfile
  TYPE(CSVFile_) :: acsvfile
  CHARACTER(512) :: iomsg
  INTEGER(I4B), ALLOCATABLE :: intvec1(:), intvec2(:), intvec3(:)

  IF (debug) CALL Display(myName//key)

  CALL toml_get(table, key, array, origin=origin, stat=stat, &
                requested=.FALSE.)

  IF (ASSOCIATED(array)) THEN
    tsize = toml_len(array)
    CALL Reallocate(val, tsize)
    DO ii = 1, tsize
      CALL toml_get(array, ii, val(ii))
    END DO
    NULLIFY (array)
    isfound = .TRUE.
    RETURN
  END IF

  CALL toml_get(table, key, temp, origin=origin, stat=stat)

  IF (stat .EQ. toml_stat%success) THEN
    CALL Reallocate(val, 1)
    val(1) = temp
    isfound = .TRUE.
    RETURN
  END IF

  CALL toml_get(table, key, filename%raw, origin=origin, stat=stat)

  IF (stat .EQ. toml_stat%success) THEN
    ext = filename%extension()
    SELECT CASE (ext%chars())
    CASE (".txt")
      CALL atxtfile%Initiate(filename=filename%Chars(), &
                             action="READ", status="OLD")
      CALL atxtfile%OPEN()
      CALL atxtfile%READ(val=val, iostat=iostat, iomsg=iomsg)

      isok = iostat .NE. 0 .AND. (.NOT. atxtfile%isEOF())
      IF (isok) THEN
        STOP "error occur "
        filename = ""
        RETURN
      END IF

      CALL atxtfile%DEALLOCATE()
      filename = ""
      isfound = .TRUE.
      RETURN
    CASE (".csv")
      CALL acsvfile%Initiate(filename=filename%Chars(), &
                             action="READ", status="OLD", &
                             delimiter=",")
      CALL acsvfile%OPEN()
      CALL acsvfile%SetHeaderIndx(1)
      CALL acsvfile%READ()
      ncol = acsvfile%Getncols()
      SELECT CASE (ncol)
      CASE (1) ! only value

        CALL acsvfile%get(1, val=intvec3) ! value
        isOk = ALLOCATED(intvec3)
        CALL AssertError_(isok, myname, &
                          "value column does not have data")
        CALL reallocate(val, 1)
        val(1) = intvec3(1)

      CASE (2) ! element number and value

        CALL acsvfile%get(1, val=intvec1) ! element
        CALL acsvfile%get(2, val=intvec3) ! value

        isok = ALLOCATED(intvec1) .AND. ALLOCATED(intvec3)
        CALL AssertError_(isok, myname, &
                          "element column or value column "// &
                          " does not have data")
        isok = SIZE(intvec1) .EQ. telem
        CALL AssertError_(isok, myname, "the size of elemen column "// &
                          " should be the same as the number of elements")
        CALL reallocate(val, telem)
        DO ii = 1, telem
          val(intvec1(ii)) = intvec3(ii)
        END DO

      CASE (3) ! start, end, value

        CALL acsvfile%get(1, val=intvec1) ! start
        CALL acsvfile%get(2, val=intvec2) ! end
        CALL acsvfile%get(3, val=intvec3) ! value
        isok = ALLOCATED(intvec1) .AND. ALLOCATED(intvec3) &
               .AND. ALLOCATED(intvec2)
        CALL AssertError_(isok, myname, &
                          "start column, end column or value column"// &
                          " does not have data")
        isok = MINVAL(intvec1) .EQ. one
        CALL AssertError_(isok, myname, "start must have 1 as a component")
        isok = MAXVAL(intvec2) .EQ. telem
        CALL AssertError_(isok, myname, "end must have the integer"// &
                          " which is the same as the number of elements")

        CALL reallocate(val, telem)
        DO ii = 1, SIZE(intvec1)
          val(intvec1(ii):intvec2(ii)) = intvec3(ii)
        END DO

      CASE default
        CALL AssertError_(.FALSE., myname, &
                          "wrong number of columns in csv file ")
      END SELECT

      CALL acsvfile%DEALLOCATE()
      filename = ""
      isfound = .TRUE.
      RETURN
    END SELECT
  END IF

  isfound = .FALSE.

END SUBROUTINE ElementDataImportFromToml_int

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE AssertError_(a, myName, msg)
  LOGICAL(LGT), INTENT(IN) :: a
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: msg

  IF (.NOT. a) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INTERNAL ERROR] :: '//msg)
    RETURN
  END IF

END SUBROUTINE AssertError_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TDGAlgoParam
