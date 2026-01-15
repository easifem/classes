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

SUBMODULE(EquationParser_Class) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: cImmed = 1, &
                           cNeg = 2, &
                           cAdd = 3, &
                           cSub = 4, &
                           cMul = 5, &
                           cDiv = 6, &
                           cPow = 7, &
                           cAbs = 8, &
                           cExp = 9, &
                           cLog10 = 10, &
                           cLog = 11, &
                           cSqrt = 12, &
                           cSinh = 13, &
                           cCosh = 14, &
                           cTanh = 15, &
                           cSin = 16, &
                           cCos = 17, &
                           cTan = 18, &
                           cAsin = 19, &
                           cAcos = 20, &
                           cAtan = 21, &
                           VarBegin = 22

CHARACTER(LEN=1), DIMENSION(cAdd:cPow), PARAMETER :: OPS = ['+', &
                                                            '-', &
                                                            '*', &
                                                            '/', &
                                                            '^']

CHARACTER(LEN=5), DIMENSION(cAbs:cAtan), PARAMETER :: FUNCS = ['abs  ', &
                                                               'exp  ', &
                                                               'log10', &
                                                               'log  ', &
                                                               'sqrt ', &
                                                               'sinh ', &
                                                               'cosh ', &
                                                               'tanh ', &
                                                               'sin  ', &
                                                               'cos  ', &
                                                               'tan  ', &
                                                               'asin ', &
                                                               'acos ', &
                                                               'atan ']

INTEGER(I4B), PARAMETER :: tFuncs = SIZE(FUNCS)

INTEGER(I4B), DIMENSION(cAbs:cAtan), PARAMETER :: LEN_FUNCS = [3, &
                                                               3, &
                                                               5, &
                                                               3, &
                                                               4, &
                                                               4, &
                                                               4, &
                                                               4, &
                                                               3, &
                                                               3, &
                                                               3, &
                                                               4, &
                                                               4, &
                                                               4]

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%ByteCode)
IF (isok) DEALLOCATE (obj%ByteCode)

obj%ByteCodeSize = 0

isok = ALLOCATED(obj%Immed)
IF (isok) DEALLOCATE (obj%Immed)

obj%ImmedSize = 0

isok = ALLOCATED(obj%Stack)
IF (isok) DEALLOCATE (obj%Stack)

obj%StackSize = 0
obj%StackPtr = 0

obj%funcString = ''
obj%funcStringOrig = ''

isok = ALLOCATED(obj%variableNames)
IF (isok) DEALLOCATE (obj%variableNames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%funcString = funcStr
obj%funcStringOrig = funcStr

tsize = SIZE(Var)

ALLOCATE (obj%variableNames(tsize))

obj%variableNames(1:tsize) = var(1:tsize)

CALL obj%parse()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Constructor
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Constructor1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ans%Initiate(funcStr=funcStr, var=var)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                      EquationParser_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Constructor_1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)
CALL ans%Initiate(funcStr=funcStr, var=var)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%byteCodeSize, msg="byteCodeSize: ", unitNo=unitNo)
CALL Display(obj%immedSize, msg="immedSize: ", unitNo=unitNo)
CALL Display(obj%stackSize, msg="stackSize: ", unitNo=unitNo)
CALL Display(obj%stackPtr, msg="stackPtr: ", unitNo=unitNo)
CALL Display(obj%funcString, msg="funcString: ", unitNo=unitNo)
CALL Display(obj%funcStringOrig, msg="funcStringOrig: ", unitNo=unitNo)

isok = ALLOCATED(obj%byteCode)
CALL Display(isok, msg="byteCode ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%byteCode)
  CALL Display(tsize, msg="byteCode SIZE: ", unitNo=unitNo)
  CALL Display(obj%byteCode, msg="byteCode: ", unitNo=unitNo)
END IF

isok = ALLOCATED(obj%immed)
CALL Display(isok, msg="immed ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%immed)
  CALL Display(tsize, msg="immed SIZE: ", unitNo=unitNo)
  CALL Display(obj%immed, msg="immed: ", unitNo=unitNo)
END IF

isok = ALLOCATED(obj%stack)
CALL Display(isok, msg="stack ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%stack)
  CALL Display(tsize, msg="stack SIZE: ", unitNo=unitNo)
  CALL Display(obj%stack, msg="stack: ", unitNo=unitNo)
END IF

isok = ALLOCATED(obj%variableNames)
CALL Display(isok, msg="variableNames ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%variableNames)
  CALL Display(tsize, msg="variableNames SIZE: ", unitNo=unitNo)
  DO ii = 1, tsize
    CALL Display(TRIM(obj%variableNames(ii)), &
                 msg="variableNames("//ToString(ii)//"): ", unitNo=unitNo)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                       Parse
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Parse
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Parse()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Replace('**', '^ ', obj%funcString)
! Exponent into 1-Char. format
CALL RemoveSpaces(obj%funcString)
! Condense function string

CALL obj%CheckSyntax()

CALL obj%Compile()
! Compile into bytecode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Parse

!----------------------------------------------------------------------------
!                                                                  Evaluate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Evaluate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Evaluate()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: IP, DP, SP
! Instruction, data, and Stack pointer
INTEGER(I4B) :: evalErrType
LOGICAL(LGT) :: iszero

DP = 1
SP = 0
evalErrType = 0

DO IP = 1, obj%byteCodeSize

  SELECT CASE (obj%byteCode(IP))

  CASE (cImmed)
    SP = SP + 1; obj%stack(SP) = obj%Immed(DP); DP = DP + 1

  CASE (cNeg)
    obj%stack(SP) = -obj%stack(SP)

  CASE (cAdd)
    obj%stack(SP - 1) = obj%stack(SP - 1) + obj%stack(SP); SP = SP - 1

  CASE (cSub)
    obj%stack(SP - 1) = obj%stack(SP - 1) - obj%stack(SP); SP = SP - 1

  CASE (cMul)
    obj%stack(SP - 1) = obj%stack(SP - 1) * obj%stack(SP); SP = SP - 1

  CASE (cDiv)

    iszero = obj%stack(SP) == math%zero

    IF (iszero) THEN
      evalErrType = 1
      ans = math%zero
      EXIT
    END IF
    obj%stack(SP - 1) = obj%stack(SP - 1) / obj%stack(SP); SP = SP - 1

  CASE (cPow)
    obj%stack(SP - 1) = obj%stack(SP - 1)**obj%stack(SP); SP = SP - 1

  CASE (cAbs)
    obj%stack(SP) = ABS(obj%stack(SP))

  CASE (cExp)
    obj%stack(SP) = EXP(obj%stack(SP))

  CASE (cLog10)

    IF (obj%stack(SP) <= math%zero) THEN
      evalErrType = 3
      ans = math%zero
      EXIT
    END IF
    obj%stack(SP) = LOG10(obj%stack(SP))

  CASE (cLog)

    IF (obj%stack(SP) <= math%zero) THEN
      evalErrType = 3
      ans = math%zero
      EXIT
    END IF
    obj%stack(SP) = LOG(obj%stack(SP))

  CASE (cSqrt)

    IF (obj%stack(SP) < math%zero) THEN
      evalErrType = 3
      ans = math%zero
      EXIT
    END IF
    obj%stack(SP) = SQRT(obj%stack(SP))

  CASE (cSinh); obj%stack(SP) = SINH(obj%stack(SP))

  CASE (cCosh); obj%stack(SP) = COSH(obj%stack(SP))

  CASE (cTanh); obj%stack(SP) = TANH(obj%stack(SP))

  CASE (cSin); obj%stack(SP) = SIN(obj%stack(SP))

  CASE (cCos); obj%stack(SP) = COS(obj%stack(SP))

  CASE (cTan); obj%stack(SP) = TAN(obj%stack(SP))

  CASE (cAsin)

    IF ((obj%stack(SP) < math%minus_one) .OR. &
        (obj%stack(SP) > math%one)) THEN
      evalErrType = 4
      ans = math%zero
      EXIT
    END IF
    obj%stack(SP) = ASIN(obj%stack(SP))

  CASE (cAcos)
    IF ((obj%stack(SP) < math%minus_one) .OR. &
        (obj%stack(SP) > math%one)) THEN
      evalErrType = 4
      ans = math%zero
      EXIT
    END IF
    obj%stack(SP) = ACOS(obj%stack(SP))

  CASE (cAtan); obj%stack(SP) = ATAN(obj%stack(SP))

  CASE DEFAULT
    SP = SP + 1
    obj%stack(SP) = val(obj%ByteCode(IP) - VarBegin + 1)

  END SELECT

END DO

#ifdef DEBUG_VER
isok = evalErrType == 0
CALL AssertError1(isok, myName, &
                  EvalErrMsg(evalErrType))
#endif

ans = obj%Stack(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Evaluate

!----------------------------------------------------------------------------
!                                                              CheckSyntax
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckSyntax
CHARACTER(*), PARAMETER :: myName = "obj_CheckSyntax()"
INTEGER(I4B) :: n, parCount, j, ib, in, lFunc
CHARACTER(LEN=1) :: c
REAL(DFP) :: r
LOGICAL :: err, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

j = 1
parCount = 0
lFunc = LEN_TRIM(obj%funcString)

step: DO

  isok = j <= lFunc
  CALL AssertError1(isok, myName, &
                    'Error in syntax of function string: '//CHAR_LF// &
                    obj%funcStringOrig)

  c = obj%funcString(j:j)

  ! Check for valid operand (must appear)
  IF (c == '-' .OR. c == '+') THEN
    ! Check for leading - or +
    j = j + 1

    isok = j <= lFunc
    CALL AssertError1( &
      isok, myName, &
      'Error in syntax of function string: Missing operand'//CHAR_LF// &
      obj%funcStringOrig)

    c = obj%funcString(j:j)

    err = ANY(c == OPS)
    CALL AssertError1( &
      .NOT. err, myName, &
      'Error in syntax of function string: Multiple consequtive&
      &operators'//CHAR_LF// &
      obj%funcStringOrig)

  END IF

  n = MathFunctionIndex(obj%funcString(j:))
  IF (n > 0) THEN
    ! Check for math function
    ! j = j + LEN_TRIM(funcs(n))
    j = j + LEN_FUNCS(n)

    err = j > lFunc
    CALL AssertError1( &
      .NOT. err, myName, &
      'Error in syntax of function string: &
      &Missing function argument after "'//funcs(n)//'" '//CHAR_LF// &
      obj%funcStringOrig)

    c = obj%funcString(j:j)

    err = c /= '('
    CALL AssertError1( &
      .NOT. err, myName, &
      'Error in syntax of function string: &
      &Missing opening parenthesis after "'//funcs(n)//'" '//CHAR_LF// &
      obj%funcStringOrig)

  END IF

  isok = c == '('
  IF (isok) THEN
    ! Check for opening parenthesis
    parCount = parCount + 1
    j = j + 1
    CYCLE step
  END IF

  isok = SCAN(c, '0123456789.') > 0
  IF (isok) THEN
    ! Check for number
    r = RealNum(obj%funcString(j:), ib, in, err)

    CALL AssertError1( &
      .NOT. err, myName, &
      'Error in syntax of function string: '//CHAR_LF// &
      'Invalid number format: '//obj%funcString(j + ib - 1:j + in - 2)// &
      CHAR_LF//obj%funcStringOrig)

    j = j + in - 1
    IF (j > lFunc) EXIT
    c = obj%funcString(j:j)

  ELSE

    ! Check for variable
    n = VariableIndex(obj%funcString(j:), obj%variableNames, ib, in)

    err = n .EQ. 0
    CALL AssertError1( &
      .NOT. err, myName, &
      'Error in syntax of function string: '//CHAR_LF// &
      'Invalid element: '//obj%funcString(j + ib - 1:j + in - 2)// &
      CHAR_LF//obj%funcStringOrig)

    j = j + in - 1
    IF (j > lFunc) EXIT
    c = obj%funcString(j:j)
  END IF

  DO WHILE (c == ')')
    ! Check for closing parenthesis
    parCount = parCount - 1

    err = parCount < 0
    CALL AssertError1( &
      .NOT. err, myName, &
     'Error in syntax of function string: Mismatched parenthesis'//CHAR_LF// &
      obj%funcStringOrig)

    err = obj%funcString(j - 1:j - 1) == '('
    CALL AssertError1( &
      .NOT. err, myName, &
      'Error in syntax of function string: '//CHAR_LF// &
      'Empty parentheses'//CHAR_LF//obj%funcStringOrig)

    j = j + 1
    IF (j > lFunc) EXIT
    c = obj%funcString(j:j)
  END DO

  ! Now, we have a legal operand: A legal operator or end of string must follow
  IF (j > lFunc) EXIT
  IF (ANY(c == Ops)) THEN
    ! Check for multiple operators

    err = j + 1 > lFunc
    CALL AssertError1( &
      .NOT. err, myName, &
      'Error in syntax of function string: '// &
      CHAR_LF//obj%funcStringOrig)

    err = ANY(obj%funcString(j + 1:j + 1) == Ops)
    CALL AssertError1( &
      .NOT. err, myName, &
      'Error in syntax of function string: Multiple operators'//CHAR_LF// &
      obj%funcStringOrig)

  ELSE
    ! Check for next operand
    CALL AssertError1( &
      math%no, myName, &
      'Error in syntax of function string: Missing operator'//CHAR_LF// &
      obj%funcStringOrig)

  END IF

  ! Now, we have an operand and an operator:
  ! the next loop will check for another
  ! operand (must appear)
  j = j + 1
END DO step

err = parCount > 0
CALL AssertError1( &
  .NOT. err, myName, &
  'Error in syntax of function string: Mismatched parenthesis'//CHAR_LF// &
  obj%funcStringOrig)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CheckSyntax

!----------------------------------------------------------------------------
!                                                                     Compile
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Compile
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Compile()"
#endif

INTEGER(I4B) :: istat, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%ByteCode)
IF (isok) THEN
  DEALLOCATE (obj%ByteCode, obj%Immed, obj%Stack)
END IF

obj%ByteCodeSize = 0
obj%ImmedSize = 0
obj%StackSize = 0
obj%StackPtr = 0

tsize = LEN_TRIM(obj%funcString)
CALL obj%CompileSubstr(1, tsize)
! Compile string to determine size

ALLOCATE (obj%ByteCode(obj%ByteCodeSize), &
          obj%Immed(obj%ImmedSize), &
          obj%Stack(obj%StackSize), &
          STAT=istat)

#ifdef DEBUG_VER
isok = istat .EQ. 0
CALL AssertError1(isok, myName, &
                  "*** Parser error: Memmory allocation for byte code failed")
#endif

obj%ByteCodeSize = 0
obj%ImmedSize = 0
obj%StackSize = 0
obj%StackPtr = 0
CALL obj%CompileSubstr(1, tsize)
! Compile string into bytecode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Compile

!----------------------------------------------------------------------------
!                                                             AddCompiledByte
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddCompiledByte
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddCompiledByte()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%ByteCodeSize = obj%ByteCodeSize + 1
isok = ALLOCATED(obj%ByteCode)
IF (isok) obj%ByteCode(obj%ByteCodeSize) = b

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddCompiledByte

!----------------------------------------------------------------------------
!                                                              MathItemIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MathItemIndex
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MathItemIndex()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0

isok = SCAN(obj%funcString(b:b), '0123456789.') > 0

IF (isok) THEN
  ! Check for begin of a number
  obj%ImmedSize = obj%ImmedSize + 1
  isok = ALLOCATED(obj%Immed)
  IF (isok) obj%Immed(obj%ImmedSize) = RealNum(obj%funcString(b:l))
  ans = cImmed
ELSE
  ! Check for a variable
  ans = VariableIndex(obj%funcString(b:l), obj%variableNames)
  isok = ans > 0
  IF (isok) ans = VarBegin + ans - 1_I4B
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_MathItemIndex

!----------------------------------------------------------------------------
!                                                              CompileSubstr
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CompileSubstr
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CompileSubstr()"
#endif

CHARACTER(LEN=*), PARAMETER :: calpha = 'abcdefghijklmnopqrstuvwxyz'// &
                               'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
INTEGER(I4B) :: n, b2, j, k, io
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Check for special cases of substring
isok = obj%funcString(b:b) == '+'
IF (isok) THEN
  ! Case 1: funcString(b:e) = '+...'
  ! WRITE(*,*)'1. funcString(b:e) = "+..."'
  CALL obj%CompileSubstr(b + 1, l)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = CompletelyEnclosed(obj%funcString, b, l)
IF (isok) THEN
  ! Case 2: funcString(b:e) = '(...)'
  ! WRITE(*,*)'2. funcString(b:e) = "(...)"'
  CALL obj%CompileSubstr(b + 1, l - 1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = SCAN(obj%funcString(b:b), calpha) > 0
IF (isok) THEN
  n = MathFunctionIndex(obj%funcString(b:l))

  isok = n > 0
  IF (isok) THEN
    b2 = b + INDEX(obj%funcString(b:l), '(') - 1

    isok = CompletelyEnclosed(obj%funcString, b2, l)
    IF (isok) THEN
      ! Case 3: funcString(b:e) = 'fcn(...)'
      ! WRITE(*,*)'3. funcString(b:e) = "fcn(...)"'
      CALL obj%CompileSubstr(b2 + 1, l - 1)
      CALL obj%AddCompiledByte(n)

#ifdef DEBUG_VER
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                              '[END] ')
#endif
      RETURN

    END IF
  END IF

END IF

isok = obj%funcString(b:b) == '-'
IF (isok) THEN

  isok = CompletelyEnclosed(obj%funcString, b + 1, l)
  IF (isok) THEN
    ! Case 4: obj%funcString(b:e) = '-(...)'
    ! WRITE(*,*)'4. obj%funcString(b:e) = "-(...)"'
    CALL obj%CompileSubstr(b + 2, l - 1)
    CALL obj%AddCompiledByte(cNeg)

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  isok = SCAN(obj%funcString(b + 1:b + 1), calpha) > 0
  IF (isok) THEN
    n = MathFunctionIndex(obj%funcString(b + 1:l))

    isok = n > 0
    IF (isok) THEN

      b2 = b + INDEX(obj%funcString(b + 1:l), '(')

      isok = CompletelyEnclosed(obj%funcString, b2, l)
      IF (isok) THEN
        ! Case 5: obj%funcString(b:e) = '-fcn(...)'
        ! WRITE(*,*)'5. obj%funcString(b:e) = "-fcn(...)"'
        CALL obj%CompileSubstr(b2 + 1, l - 1); 
        CALL obj%AddCompiledByte(n)
        CALL obj%AddCompiledByte(cNeg)

#ifdef DEBUG_VER
        CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                '[END] ')
#endif
        RETURN
      END IF
    END IF

  END IF
END IF

! Check for operator in substring: check only base level (k=0),
! exclude expr. in ()
DO io = cAdd, cPow ! Increasing priority +-*/^
  k = 0
  DO j = l, b, -1

    IF (obj%funcString(j:j) == ')') THEN
      k = k + 1
    ELSEIF (obj%funcString(j:j) == '(') THEN
      k = k - 1
    END IF

    isok = k == 0 &
           .AND. (obj%funcString(j:j) == Ops(io)) &
           .AND. (IsBinaryOp(j, obj%funcString))
    IF (isok) THEN

      isok = ANY(obj%funcString(j:j) == Ops(cMul:cPow)) &
             .AND. obj%funcString(b:b) == '-'
      IF (isok) THEN
        ! Case 6: obj%funcString(b:e) = '-...Op...' with Op > -
        ! WRITE(*,*)'6. obj%funcString(b:e) = "-...Op..." with Op > -'
        CALL obj%CompileSubstr(b + 1, l)
        CALL obj%AddCompiledByte(cNeg)

#ifdef DEBUG_VER
        CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                '[END] ')
#endif

        RETURN

      ELSE

        ! Case 7: obj%funcString(b:e) = '...BinOp...'
        ! WRITE(*,*)'7. Binary operator',obj%funcString(j:j)
        CALL obj%CompileSubstr(b, j - 1)
        CALL obj%CompileSubstr(j + 1, l)
        CALL obj%AddCompiledByte(OperatorIndex(Ops(io)))
        obj%StackPtr = obj%StackPtr - 1

#ifdef DEBUG_VER
        CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                '[END] ')
#endif

        RETURN
      END IF
    END IF
  END DO
END DO

! Check for remaining items, i.e. variables or explicit numbers
b2 = b

IF (obj%funcString(b:b) == '-') b2 = b2 + 1

n = obj%MathItemIndex(b2, l)

!   WRITE(*,*)'8. AddCompiledByte ',n
CALL obj%AddCompiledByte(n)
obj%StackPtr = obj%StackPtr + 1
IF (obj%StackPtr > obj%StackSize) obj%StackSize = obj%StackSize + 1

IF (b2 > b) CALL obj%AddCompiledByte(cNeg)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CompileSubstr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Return error message
FUNCTION EvalErrMsg(EvalErrType) RESULT(msg)
  INTEGER, INTENT(in) :: EvalErrType
  CHARACTER(LEN=*), DIMENSION(4), PARAMETER :: &
    m = ['Division by zero                ', &
         'Argument of SQRT negative       ', &
         'Argument of LOG negative        ', &
         'Argument of ASIN or ACOS illegal']
  CHARACTER(32) :: msg

  LOGICAL(LGT) :: isok

  isok = (EvalErrType < 1) .OR. (EvalErrType > SIZE(m))
  IF (isok) THEN
    msg = ''
  ELSE
    msg = m(EvalErrType)
  END IF
END FUNCTION EvalErrMsg

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Return operator index
FUNCTION OperatorIndex(c) RESULT(n)
  CHARACTER(LEN=1), INTENT(in) :: c
  INTEGER(I4B) :: n, j

  n = 0

  DO j = cAdd, cPow
    IF (c == Ops(j)) THEN
      n = j
      EXIT
    END IF
  END DO

END FUNCTION OperatorIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION MathFunctionIndex(str) RESULT(n)
  ! Return index of math function beginnig at 1st position of string str
  CHARACTER(LEN=*), INTENT(in) :: str

  INTEGER(I4B) :: n, j, k
  CHARACTER(tFuncs) :: fun

  n = 0

  DO j = cAbs, cAtan ! Check all math functions
    k = MIN(LEN_FUNCS(j), LEN(str))
    CALL LowCase(str(1:k), fun)
    IF (fun == funcs(j)) THEN
      ! Compare lower case letters
      n = j
      ! Found a matching function
      EXIT
    END IF
  END DO
END FUNCTION MathFunctionIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Return index of variable at begin of string str
! (returns 0 if no variable found)
FUNCTION VariableIndex(str, Var, ibegin, inext) RESULT(n)
  CHARACTER(LEN=*), INTENT(in) :: str
  ! String
  CHARACTER(LEN=*), DIMENSION(:), INTENT(in) :: Var
  ! Array with variable names
  INTEGER(I4B) :: n
  ! Index of variable
  INTEGER, OPTIONAL, INTENT(out) :: ibegin, inext
  ! Start position of variable name
  ! Position of character after name
  INTEGER :: j, ib, in, lstr

  n = 0
  lstr = LEN_TRIM(str)
  IF (lstr > 0) THEN
    DO ib = 1, lstr ! Search for first character in str
      IF (str(ib:ib) /= ' ') EXIT ! When lstr>0 at least 1 char in str
    END DO
    DO in = ib, lstr ! Search for name terminators
      IF (SCAN(str(in:in), '+-*/^) ') > 0) EXIT
    END DO
    DO j = 1, SIZE(Var)
      IF (str(ib:in - 1) == Var(j)) THEN
        n = INT(j, I4B) ! Variable name found
        EXIT
      END IF
    END DO
  END IF
  IF (PRESENT(ibegin)) ibegin = ib
  IF (PRESENT(inext)) inext = in
END FUNCTION VariableIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove Spaces from string, remember positions of characters in old string
SUBROUTINE RemoveSpaces(str)
  CHARACTER(LEN=*), INTENT(inout) :: str

  INTEGER :: k, lstr

  lstr = LEN_TRIM(str)

  k = 1

  DO WHILE (str(k:lstr) /= ' ')
    IF (str(k:k) == ' ') THEN
      str(k:lstr) = str(k + 1:lstr)//' ' ! Move 1 character to left
      k = k - 1
    END IF
    k = k + 1
  END DO
END SUBROUTINE RemoveSpaces

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Replace ALL appearances of character set ca in string str by character
! set cb
SUBROUTINE Replace(ca, cb, str)
  CHARACTER(*), INTENT(IN) :: ca
  CHARACTER(*), INTENT(IN) :: cb
  ! LEN(ca) must be LEN(cb)
  CHARACTER(LEN=*), INTENT(INOUT) :: str

  INTEGER :: j, lca

  lca = LEN(ca)

  DO j = 1, LEN_TRIM(str) - lca + 1
    IF (str(j:j + lca - 1) == ca) str(j:j + lca - 1) = cb
  END DO
END SUBROUTINE Replace

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Check if function substring F(b:e) is completely enclosed by a pair of
! parenthesis
FUNCTION CompletelyEnclosed(F, b, l) RESULT(res)
  CHARACTER(LEN=*), INTENT(in) :: F
  ! Function substring
  INTEGER(I4B), INTENT(in) :: b, l
  ! First and last pos. of substring

  LOGICAL :: res
  INTEGER :: j, k

  res = .FALSE.

  IF (F(b:b) == '(' .AND. F(l:l) == ')') THEN
    k = 0
    DO j = b + 1, l - 1
      IF (F(j:j) == '(') THEN
        k = k + 1
      ELSEIF (F(j:j) == ')') THEN
        k = k - 1
      END IF
      IF (k < 0) EXIT
    END DO
    IF (k == 0) res = .TRUE. ! All opened parenthesis closed
  END IF
END FUNCTION CompletelyEnclosed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Check if operator F(j:j) in string F is binary operator
! Special cases already covered elsewhere:
! (that is corrected in v1.1)
! - operator character F(j:j) is first character of string (j=1)
FUNCTION IsBinaryOp(j, F) RESULT(res)
  INTEGER, INTENT(in) :: j ! Position of Operator
  CHARACTER(LEN=*), INTENT(in) :: F ! String
  LOGICAL :: res ! Result

  ! internal variables
  INTEGER :: k
  LOGICAL :: Dflag, Pflag

  res = .TRUE.

  IF (F(j:j) == '+' .OR. F(j:j) == '-') THEN
    ! Plus or minus sign:
    IF (j == 1) THEN
      ! - leading unary operator ?
      res = .FALSE.
    ELSEIF (SCAN(F(j - 1:j - 1), '+-*/^(') > 0) THEN
      ! - other unary operator ?
      res = .FALSE.
    ELSEIF (SCAN(F(j + 1:j + 1), '0123456789') > 0 .AND. & ! - in exponent of real number ?
            SCAN(F(j - 1:j - 1), 'eEdD') > 0) THEN
      Dflag = .FALSE.; Pflag = .FALSE.
      k = j - 1
      DO WHILE (k > 1) !   step to the left in mantissa
        k = k - 1
        IF (SCAN(F(k:k), '0123456789') > 0) THEN
          Dflag = .TRUE.
        ELSEIF (F(k:k) == '.') THEN
          IF (Pflag) THEN
            EXIT !   * EXIT: 2nd appearance of '.'
          ELSE
            Pflag = .TRUE. !   * mark 1st appearance of '.'
          END IF
        ELSE
          EXIT !   * all other characters
        END IF
      END DO
      IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k), '+-*/^(') > 0)) res = .FALSE.
    END IF
  END IF
END FUNCTION IsBinaryOp

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get real number from string - Format:
! [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
FUNCTION RealNum(str, ibegin, inext, error) RESULT(res)
  CHARACTER(LEN=*), INTENT(IN) :: str ! String
  REAL(DFP) :: res ! Real number
  INTEGER, OPTIONAL, INTENT(OUT) :: ibegin, & ! Start position of real number
                                    inext ! 1st character after real number
  LOGICAL, OPTIONAL, INTENT(OUT) :: error ! Error flag

  INTEGER :: ib, in, istat
  LOGICAL :: Bflag, & ! .T. at begin of number in str
             InMan, & ! .T. in mantissa of number
             Pflag, & ! .T. after 1st '.' encountered
             Eflag, & ! .T. at exponent identifier 'eEdD'
             InExp, & ! .T. in exponent of number
             DInMan, & ! .T. if at least 1 digit in mant.
             DInExp, & ! .T. if at least 1 digit in exp.
             err ! Local error flag

  Bflag = .TRUE.; InMan = .FALSE.; Pflag = .FALSE.; Eflag = .FALSE.
  InExp = .FALSE.
  DInMan = .FALSE.; DInExp = .FALSE.
  ib = 1
  in = 1
  DO WHILE (in <= LEN_TRIM(str))
    SELECT CASE (str(in:in))
    CASE (' ') ! Only leading blanks permitted
      ib = ib + 1
      IF (InMan .OR. Eflag .OR. InExp) EXIT
    CASE ('+', '-') ! Permitted only
      IF (Bflag) THEN
        InMan = .TRUE.; Bflag = .FALSE. ! - at beginning of mantissa
      ELSEIF (Eflag) THEN
        InExp = .TRUE.; Eflag = .FALSE. ! - at beginning of exponent
      ELSE
        EXIT ! - otherwise STOP
      END IF
    CASE ('0':'9') ! Mark
      IF (Bflag) THEN
        InMan = .TRUE.; Bflag = .FALSE. ! - beginning of mantissa
      ELSEIF (Eflag) THEN
        InExp = .TRUE.; Eflag = .FALSE. ! - beginning of exponent
      END IF
      IF (InMan) DInMan = .TRUE. ! Mantissa contains digit
      IF (InExp) DInExp = .TRUE. ! Exponent contains digit
    CASE ('.')
      IF (Bflag) THEN
        Pflag = .TRUE. ! - mark 1st appearance of '.'
        InMan = .TRUE.; Bflag = .FALSE. !   mark beginning of mantissa
      ELSEIF (InMan .AND. .NOT. Pflag) THEN
        Pflag = .TRUE. ! - mark 1st appearance of '.'
      ELSE
        EXIT ! - otherwise STOP
      END IF
    CASE ('e', 'E', 'd', 'D') ! Permitted only
      IF (InMan) THEN
        Eflag = .TRUE.; InMan = .FALSE. ! - following mantissa
      ELSE
        EXIT ! - otherwise STOP
      END IF
    CASE DEFAULT
      EXIT ! STOP at all other characters
    END SELECT
    in = in + 1
  END DO
  err = (ib > in-1) .OR. (.NOT.DInMan) .OR. ((Eflag.OR.InExp).AND..NOT.DInExp)
  IF (err) THEN
    res = math%zero
  ELSE
    READ (str(ib:in - 1), *, IOSTAT=istat) res
    err = istat /= 0
  END IF
  IF (PRESENT(ibegin)) ibegin = ib
  IF (PRESENT(inext)) inext = in
  IF (PRESENT(error)) error = err
END FUNCTION RealNum

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Transform upper case letters in str1 into lower case letters, result is str2
SUBROUTINE LowCase(str1, str2)
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(in) :: str1
  CHARACTER(LEN=*), INTENT(out) :: str2
  INTEGER :: j, k
  CHARACTER(LEN=*), PARAMETER :: lc = 'abcdefghijklmnopqrstuvwxyz'
  CHARACTER(LEN=*), PARAMETER :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

  str2 = str1

  DO j = 1, LEN_TRIM(str1)
    k = INDEX(uc, str1(j:j))
    IF (k > 0) str2(j:j) = lc(k:k)
  END DO
END SUBROUTINE LowCase

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
