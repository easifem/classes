!> Utility module containing miscellaneous tools that don't
!> quite fit anywhere else.
MODULE EasyPlplot_Utilities
USE GlobalData, ONLY: wp => DFP
IMPLICIT NONE
PRIVATE

!> Return a 2-vector comprising the minimum and maximum values of an array
INTERFACE mixval
  MODULE PROCEDURE mixval_1
  MODULE PROCEDURE mixval_2
  MODULE PROCEDURE mixval_3
END INTERFACE

!> Return a the maximum-minumum values of an array
INTERFACE span
  MODULE PROCEDURE span_1
  MODULE PROCEDURE span_2
  MODULE PROCEDURE span_3
END INTERFACE

!> Reduce an array to one dimension
INTERFACE flatten
  MODULE PROCEDURE flatten_2
  MODULE PROCEDURE flatten_3
END INTERFACE

PUBLIC :: mixval
PUBLIC :: span
PUBLIC :: linspace

PUBLIC :: startsWith
PUBLIC :: endsWith

PUBLIC :: meshGridX
PUBLIC :: meshGridY

PUBLIC :: randomNormal
PUBLIC :: randomUniform
PUBLIC :: mean
PUBLIC :: stdev

PUBLIC :: flatten

PUBLIC :: colorize
PUBLIC :: int2char
PUBLIC :: real2char

PUBLIC :: showProgress

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return [hi,low] for an array
FUNCTION mixval_1(x) RESULT(b)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! Array to find extrema in
  REAL(wp), DIMENSION(2) :: b

  b = [MINVAL(x), MAXVAL(x)]
END FUNCTION mixval_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return [hi,low] for an array
FUNCTION mixval_2(x) RESULT(b)
  REAL(wp), DIMENSION(:, :), INTENT(in) :: x
  !! Array to find extrema in
  REAL(wp), DIMENSION(2) :: b

  b = [MINVAL(x), MAXVAL(x)]
END FUNCTION mixval_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return [hi,low] for an array
FUNCTION mixval_3(x) RESULT(b)
  REAL(wp), DIMENSION(:, :, :), INTENT(in) :: x
  !! Array to find extrema in
  REAL(wp), DIMENSION(2) :: b

  b = [MINVAL(x), MAXVAL(x)]
END FUNCTION mixval_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return hi-low for an array
FUNCTION span_1(x) RESULT(o)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! Array to find span in
  REAL(wp) :: o

  o = MAXVAL(x) - MINVAL(x)
END FUNCTION span_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return hi-low for an array
FUNCTION span_2(x) RESULT(o)
  REAL(wp), DIMENSION(:, :), INTENT(in) :: x
  !! Array to find span in
  REAL(wp) :: o

  o = MAXVAL(x) - MINVAL(x)
END FUNCTION span_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return hi-low for an array
FUNCTION span_3(x) RESULT(o)
  REAL(wp), DIMENSION(:, :, :), INTENT(in) :: x
  !! Array to find span in
  REAL(wp) :: o

  o = MAXVAL(x) - MINVAL(x)
END FUNCTION span_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return an array of evenly-spaced values
FUNCTION linspace(l, h, N) RESULT(o)
  REAL(wp), INTENT(in) :: l
  !! Low-bound for values
  REAL(wp), INTENT(in) :: h
  !! High-bound for values
  INTEGER, INTENT(in), OPTIONAL :: N
  !! Number of values (default 20)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: o

  INTEGER :: Nl, i

  Nl = 20
  IF (PRESENT(N)) Nl = N

  o = [((h - l) * REAL(i - 1, wp) / REAL(Nl - 1, wp) + l, i=1, Nl)]
END FUNCTION linspace

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Test if text starts with str
FUNCTION startsWith(text, str) RESULT(o)
  CHARACTER(*), INTENT(in) :: text !! Text to search
  CHARACTER(*), INTENT(in) :: str !! String to look for
  LOGICAL :: o
  INTEGER :: k

  k = LEN(str)
  o = text(1:k) == str
END FUNCTION startsWith

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Test if text ends with str
FUNCTION endsWith(text, str) RESULT(o)
  CHARACTER(*), INTENT(in) :: text
  !! Text to search
  CHARACTER(*), INTENT(in) :: str
  !! String to look for
  LOGICAL :: o
  INTEGER :: k

  k = LEN(text)
  o = text(k - LEN(str) + 1:k) == str
END FUNCTION endsWith

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return a sample from an approximate normal distribution
!> with a mean of \(\mu=0\) and a standard deviation of
!> \(\sigma=1\). In this approximate distribution, \(x\in[-6,6]\).
FUNCTION randomNormal() RESULT(o)
  REAL(wp) :: o
  REAL(wp), DIMENSION(12) :: x

  CALL RANDOM_NUMBER(x)
  o = SUM(x) - 6.0_WP
END FUNCTION randomNormal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return a sample from a uniform distribution
!> in the range \(x\in[-1,1]\).
FUNCTION randomUniform() RESULT(o)
  REAL(wp) :: o

  CALL RANDOM_NUMBER(o)
  o = o * 2.0_WP - 1.0_WP
END FUNCTION randomUniform

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Convert a 2d array to 1d
FUNCTION flatten_2(A) RESULT(o)
  REAL(wp), DIMENSION(:, :), INTENT(in) :: A
  !! Array to convert
  REAL(wp), DIMENSION(:), ALLOCATABLE :: o

  o = RESHAPE(A, [SIZE(A)])
END FUNCTION flatten_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Convert a 3d array to 1d
FUNCTION flatten_3(A) RESULT(o)
  REAL(wp), DIMENSION(:, :, :), INTENT(in) :: A
  !! Array to convert
  REAL(wp), DIMENSION(:), ALLOCATABLE :: o

  o = RESHAPE(A, [SIZE(A)])
END FUNCTION flatten_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Construct a 2d array of X values from a structured grid
FUNCTION meshGridX(x, y) RESULT(o)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-positions in grid
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-positions in grid
  REAL(wp), DIMENSION(:, :), ALLOCATABLE :: o

  INTEGER :: Nx, Ny
  INTEGER :: i, j

  Nx = SIZE(x)
  Ny = SIZE(y)

  ALLOCATE (o(Nx, Ny))

  DO CONCURRENT(i=1:Nx, j=1:Ny)
    o(i, j) = x(i)
  END DO
END FUNCTION meshGridX

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Construct a 2d array of Y values from a structured grid
FUNCTION meshGridY(x, y) RESULT(o)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-positions in grid
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-positions in grid
  REAL(wp), DIMENSION(:, :), ALLOCATABLE :: o

  INTEGER :: Nx, Ny
  INTEGER :: i, j

  Nx = SIZE(x)
  Ny = SIZE(y)

  ALLOCATE (o(Nx, Ny))

  DO CONCURRENT(i=1:Nx, j=1:Ny)
    o(i, j) = y(j)
  END DO
END FUNCTION meshGridY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add terminal format codes to coloize a string
FUNCTION colorize(s, c) RESULT(o)
  CHARACTER(*), INTENT(in) :: s
  !! String to colorize
  INTEGER, DIMENSION(3) :: c ! c in [0,5]
  !! Color code in [r,g,b] where \(r,g,b\in[0,5]\)
  CHARACTER(:), ALLOCATABLE :: o

  CHARACTER(1), PARAMETER :: CR = ACHAR(13)
  CHARACTER(1), PARAMETER :: ESC = ACHAR(27)

  CHARACTER(20) :: pre
  CHARACTER(3) :: cb

  WRITE (cb, '(1I3)') 36 * c(1) + 6 * c(2) + c(3) + 16
  pre = ESC//'[38;5;'//TRIM(ADJUSTL(cb))//'m'
  o = TRIM(pre)//s//ESC//'[0m'
END FUNCTION colorize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Convert a real to a character
PURE FUNCTION real2char(a, f, l) RESULT(o)
  REAL(wp), INTENT(in) :: a
  !! Real value to convert
  CHARACTER(*), OPTIONAL, INTENT(in) :: f
  !! Format of result
  INTEGER, OPTIONAL, INTENT(in) :: l
  !! Length of result
  CHARACTER(:), ALLOCATABLE :: o

  CHARACTER(128) :: buf

  IF (PRESENT(l)) THEN
    ALLOCATE (CHARACTER(l) :: o)
    IF (PRESENT(f)) THEN
      WRITE (o, '('//f//')') a
    ELSE
      WRITE (o, *) a
    END IF
  ELSE
    IF (PRESENT(f)) THEN
      WRITE (buf, '('//f//')') a
    ELSE
      WRITE (buf, *) a
    END IF
    o = TRIM(ADJUSTL(buf))
  END IF
END FUNCTION real2char

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Convert an integer to a character
PURE FUNCTION int2char(a, f, l) RESULT(o)
  INTEGER, INTENT(in) :: a
  !! Integer value to convert
  CHARACTER(*), OPTIONAL, INTENT(in) :: f
  !! Format of result
  INTEGER, OPTIONAL, INTENT(in) :: l
  !! Length of result
  CHARACTER(:), ALLOCATABLE :: o

  CHARACTER(128) :: buf

  IF (PRESENT(l)) THEN
    ALLOCATE (CHARACTER(l) :: o)
    IF (PRESENT(f)) THEN
      WRITE (o, '('//f//')') a
    ELSE
      WRITE (o, *) a
    END IF
  ELSE
    IF (PRESENT(f)) THEN
      WRITE (buf, '('//f//')') a
    ELSE
      WRITE (buf, *) a
    END IF
    o = TRIM(ADJUSTL(buf))
  END IF
END FUNCTION int2char

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Show a progress bar with a message
SUBROUTINE showProgress(m, p)
  CHARACTER(*), INTENT(in) :: m
  !! Message to show
  REAL(wp), INTENT(in) :: p
  !! Progress level \(p\in[0,1]\)

  REAL(wp) :: r
  REAL(wp), SAVE :: po
  INTEGER :: N, k

  N = 40

  IF (p <= 0.0_WP) THEN
    po = p
  END IF
  IF (p - po < 0.05 .AND. p < 1.0_WP) THEN
    RETURN
  ELSE
    po = p
  END IF

  WRITE (*, '(1A)', advance='no') ACHAR(13)//colorize(m//' [', [5, 5, 0])
  DO k = 1, N
    r = REAL(k - 1, wp) / REAL(N - 1, wp)
    IF (r <= p) THEN
      WRITE (*, '(1A)', advance='no') colorize('=', cmap(r, [0.0_WP, 1.0_WP]))
    ELSE
      WRITE (*, '(1A)', advance='no') colorize(' ', [0, 0, 0])
    END IF
  END DO
  WRITE (*, '(1A,1A,1X,1A)', advance='no') colorize('] ', [5, 5, 0]), &
    & colorize(real2char(100.0_WP * p, '1F5.1'), &
    & cmap(p, [0.0_WP, 1.0_WP])), &
    & colorize('%', [5, 5, 0])
  IF (p >= 1.0_WP) WRITE (*, '(1A)') ''
  FLUSH (6)
END SUBROUTINE showProgress

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Sample a color from a cool-warm colormap for colorize
FUNCTION cmap(v, r) RESULT(c)
  REAL(wp), INTENT(in) :: v
  !! Value to sample
  REAL(wp), DIMENSION(2), INTENT(in) :: r
  !! Range to sample from
  INTEGER, DIMENSION(3) :: c

  INTEGER :: s

  IF (v < SUM(r) / 2.0_WP) THEN
    s = NINT((v - r(1)) / (SUM(r) / 2.0_WP - r(1)) * 5.0_WP)
    c = [s, s, 5]
  ELSE
    s = 5 - NINT((v - SUM(r) / 2.0_WP) / (r(2) - SUM(r) / 2.0_WP) * 5.0_WP)
    c = [5, s, s]
  END IF
END FUNCTION cmap

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Compute the arithmetic mean of an array
FUNCTION mean(d) RESULT(o)
  REAL(wp), DIMENSION(:), INTENT(in) :: d
  REAL(wp) :: o

  o = SUM(d) / REAL(SIZE(d), wp)
END FUNCTION mean

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Compute the standard deviation of an array
FUNCTION stdev(d) RESULT(o)
  REAL(wp), DIMENSION(:), INTENT(in) :: d
  REAL(wp) :: o

  o = SQRT(SUM((d - mean(d))**2) / REAL(SIZE(d) - 1, wp))
END FUNCTION stdev

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EasyPlplot_Utilities
