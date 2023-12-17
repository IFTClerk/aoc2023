MODULE UTILS
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: ISA
  PUBLIC :: ISN
  PUBLIC :: ISW
  PUBLIC :: NLINES
  PUBLIC :: READMAT
  PUBLIC :: PRINTMAT
  PUBLIC :: SWAP
  PUBLIC :: RESIZE
  PUBLIC :: PUSHONE
  PUBLIC :: PUSHARR
  PUBLIC :: COUNTSTR

  INTERFACE READMAT
     PROCEDURE READMAT_I, READMAT_C
  END INTERFACE READMAT

  INTERFACE PRINTMAT
     PROCEDURE PRINTMAT_I, PRINTMAT_C
  END INTERFACE PRINTMAT

  INTERFACE SWAP
     PROCEDURE SWAP_I, SWAP_C
  END INTERFACE SWAP

  INTERFACE RESIZE
     PROCEDURE RESIZE_I, RESIZE_C
  END INTERFACE RESIZE

  INTERFACE PUSHONE
     PROCEDURE PUSHONE_I, PUSHONE_C
  END INTERFACE PUSHONE

  INTERFACE PUSHARR
     PROCEDURE PUSHARR_I, PUSHARR_C
  END INTERFACE PUSHARR

CONTAINS

ELEMENTAL LOGICAL FUNCTION ISA(c)
  CHARACTER, INTENT(in) :: c

  isa = ('a'.LE.c .AND. c.LE.'z') .OR. ('A'.LE.c .AND. c.LE.'Z')
END FUNCTION ISA

ELEMENTAL LOGICAL FUNCTION ISN(c)
  CHARACTER, INTENT(in) :: c

  isn = '0'.LE.c .AND. c.LE.'9'
END FUNCTION ISN

ELEMENTAL LOGICAL FUNCTION ISW(c)
  CHARACTER, INTENT(in) :: c

  isw = c.EQ.CHAR(9) .OR. c.EQ.' '
END FUNCTION ISW

PURE INTEGER FUNCTION COUNTSTR(str, sub) RESULT(n)
  CHARACTER(LEN=*), INTENT(IN) :: str, sub
  INTEGER p, xp

  n = 0
  IF(LEN(sub).EQ.0) RETURN
  p = 1
  DO
     xp = INDEX(str(p:), sub)
     IF (xp.EQ.0) RETURN
     n = n + 1
     p = p + xp + LEN(sub) - 1
  END DO
END FUNCTION COUNTSTR

INTEGER FUNCTION NLINES(iu)
  INTEGER ios, iu

  nlines=0
  DO
     READ(iu, *, IOSTAT=ios)
     IF (ios.EQ.IOSTAT_END) EXIT
     nlines = nlines + 1
  END DO
  REWIND(iu)
END FUNCTION NLINES

SUBROUTINE READMAT_C(iu, mat, nr, nc)
  CHARACTER(LEN=256) l
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: mat
  INTEGER iu, ios, nr, nc, i, j

  nr = NLINES(iu)
  READ(iu, "(A)", IOSTAT=ios) l
  nc = LEN_TRIM(l)
  BACKSPACE(iu)

  ALLOCATE(mat(nr, nc))

  DO j=1,nr
     READ(iu, "(*(A))") (mat(j,i), i=1,nc)
  END DO
END SUBROUTINE READMAT_C

SUBROUTINE READMAT_I(iu, mat, nr, nc)
  CHARACTER(LEN=256) l
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: mat
  INTEGER iu, ios, nr, nc, i, j

  nr = NLINES(iu)
  READ(iu, "(A)", IOSTAT=ios) l
  nc = LEN_TRIM(l)
  BACKSPACE(iu)

  ALLOCATE(mat(nr, nc))

  DO j=1,nr
     READ(iu, "(*(I1))") (mat(j,i), i=1,nc)
  END DO
END SUBROUTINE READMAT_I

SUBROUTINE PRINTMAT_I(mtx)
  INTEGER, DIMENSION(:,:) :: mtx
  INTEGER i, nrow, ncol

  nrow = SIZE(mtx, DIM=1)
  ncol = SIZE(mtx, DIM=2)
  DO i = 1,ncol
     WRITE(6, '(1X,*(I0,1X))') mtx(i,:)
  END DO
END SUBROUTINE PRINTMAT_I

SUBROUTINE PRINTMAT_C(mtx)
  CHARACTER(LEN=*), DIMENSION(:,:) :: mtx
  INTEGER i, nrow, ncol

  nrow = SIZE(mtx, DIM=1)
  ncol = SIZE(mtx, DIM=2)
  DO i = 1,ncol
     WRITE(6, '(1X,*(A,1X))') mtx(i,:)
  END DO
END SUBROUTINE PRINTMAT_C

ELEMENTAL SUBROUTINE SWAP_I(x, y)
  INTEGER, INTENT(INOUT) :: x, y
  INTEGER t

  t = x
  x = y
  y = t
END SUBROUTINE SWAP_I

ELEMENTAL SUBROUTINE SWAP_C(x, y)
  CHARACTER(LEN=*), INTENT(INOUT) :: x, y
  CHARACTER(LEN=LEN(x)) t

  t = x
  x = y
  y = t
END SUBROUTINE SWAP_C

SUBROUTINE RESIZE_I(arr, n)
  INTEGER, DIMENSION(:), ALLOCATABLE :: arr, temp
  INTEGER n, s

  ALLOCATE(temp(n))
  ! temp = 0
  s = MIN(n, SIZE(arr))
  temp(1:s) = arr(1:s)
  CALL MOVE_ALLOC(temp, arr)
END SUBROUTINE RESIZE_I

SUBROUTINE RESIZE_C(arr, n)
  CHARACTER(LEN=*), DIMENSION(:), ALLOCATABLE :: arr
  CHARACTER(LEN=LEN(arr)), DIMENSION(:), ALLOCATABLE :: temp
  INTEGER n, s

  ALLOCATE(temp(n))
  ! temp = 0
  s = MIN(n, SIZE(arr))
  temp(1:s) = arr(1:s)
  CALL MOVE_ALLOC(temp, arr)
END SUBROUTINE RESIZE_C

SUBROUTINE PUSHONE_I(arr, val)
  INTEGER, DIMENSION(:), ALLOCATABLE :: arr
  INTEGER val
  INTEGER s

  s = SIZE(arr)+1
  CALL RESIZE(arr, s)
  arr(s) = val
END SUBROUTINE PUSHONE_I

SUBROUTINE PUSHONE_C(arr, val)
  CHARACTER(LEN=*), DIMENSION(:), ALLOCATABLE :: arr
  CHARACTER(LEN=*) val
  INTEGER s

  s = SIZE(arr)+1
  CALL RESIZE(arr, s)
  arr(s) = val
END SUBROUTINE PUSHONE_C

SUBROUTINE PUSHARR_I(arr, varr)
  INTEGER, DIMENSION(:), ALLOCATABLE :: arr
  INTEGER, DIMENSION(:) :: varr
  INTEGER sa, sv

  sa = SIZE(arr)
  sv = SIZE(varr)
  CALL RESIZE(arr, sa + sv)
  arr(sa+1:sa+sv) = varr(1:sv)
END SUBROUTINE PUSHARR_I

SUBROUTINE PUSHARR_C(arr, varr)
  CHARACTER(LEN=*), DIMENSION(:), ALLOCATABLE :: arr
  CHARACTER(LEN=*), DIMENSION(:) :: varr
  INTEGER sa, sv

  sa = SIZE(arr)
  sv = SIZE(varr)
  CALL RESIZE(arr, sa + sv)
  arr(sa+1:sa+sv) = varr(1:sv)
END SUBROUTINE PUSHARR_C

END MODULE UTILS
