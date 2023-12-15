MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  TYPE :: BOX
     CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE :: labs
     INTEGER, DIMENSION(:), ALLOCATABLE :: fs
  END TYPE BOX
CONTAINS

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER(LEN=50000) buf
  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE :: seq
  CHARACTER(LEN=16) w
  INTEGER s, v, ns, i, j

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  READ(10, '(A)') buf
  ns = COUNTSTR(buf, ',')+1
  BACKSPACE(10)
  ALLOCATE(seq(ns))
  READ(10, *) seq
  CLOSE(10)

  DO i=1,ns
     w = seq(i)
     v = 0
     DO j=1,LEN_TRIM(w)
        v = MODULO((v + IACHAR(w(j:j))) * 17, 256)
     END DO
     s = s + v
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE RMLENS(bx, lab)
  TYPE(BOX) bx
  CHARACTER(LEN=16) lab
  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE :: labs
  INTEGER, DIMENSION(:), ALLOCATABLE :: fs
  INTEGER il
  LOGICAL, DIMENSION(:), ALLOCATABLE :: msk

  labs = bx%labs
  il = FINDLOC(labs, lab, DIM=1)
  IF (il.EQ.0) THEN
     ! cant find label to remove
     RETURN
  ELSE IF (il.GT.0) THEN
     ALLOCATE(msk(SIZE(labs)))
     msk = .TRUE.
     msk(il) = .FALSE.

     ! pack into new arrays with masked entry removed
     fs = bx%fs
     labs = PACK(labs, msk)
     fs = PACK(fs, msk)

     ! move updated arrays back
     CALL MOVE_ALLOC(labs, bx%labs)
     CALL MOVE_ALLOC(fs, bx%fs)
  END IF
END SUBROUTINE RMLENS

SUBROUTINE MVLENS(bx, lab, f)
  TYPE(BOX) bx
  CHARACTER(LEN=16) lab
  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE :: labs
  INTEGER, DIMENSION(:), ALLOCATABLE :: fs
  INTEGER il, nl, f

  nl = SIZE(bx%labs)
  il = FINDLOC(bx%labs, lab, DIM=1)
  IF (il.EQ.0) THEN
     ! insert lens
     ALLOCATE(labs(nl+1))
     labs(1:nl) = bx%labs
     labs(nl+1) = lab
     ALLOCATE(fs(nl+1))
     fs(1:nl) = bx%fs
     fs(nl+1) = f

     ! move updated arrays back
     CALL MOVE_ALLOC(labs, bx%labs)
     CALL MOVE_ALLOC(fs, bx%fs)
  ELSE IF (il.GT.0) THEN
     ! swap focal length
     bx%fs(il) = f
  END IF
END SUBROUTINE MVLENS

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER(LEN=50000) buf
  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE :: seq
  CHARACTER(LEN=16) w, ww
  INTEGER s, v, ns, i, j, ie, im, ii, f
  LOGICAL rm
  TYPE(box), DIMENSION(256) :: bxs
  TYPE(box) :: bx

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  READ(10, '(A)') buf
  ns = COUNTSTR(buf, ',')+1
  BACKSPACE(10)
  ALLOCATE(seq(ns))
  READ(10, *) seq
  CLOSE(10)

  ! initialise boxes
  DO i=1,256
     ALLOCATE(bxs(i)%labs(0))
     ALLOCATE(bxs(i)%fs(0))
  END DO

  DO i=1,ns
     w = seq(i)
     ! are we moving or are we removing?
     im = INDEX(TRIM(w), '-')
     ie = INDEX(TRIM(w), '=')
     ii = MAX(im, ie)
     rm = im.GT.ie

     v = 0
     ! why the hell do I have to do this
     ww = w(1:ii-1)
     ! find box number
     DO j=1,LEN_TRIM(ww)
        v = MODULO((v + IACHAR(ww(j:j))) * 17, 256)
     END DO
     v = v + 1 ! only stupid people index from 0

     IF (rm) THEN
        CALL RMLENS(bxs(v), ww)
     ELSE
        ! thank god this is only one digit long
        READ(w(ii+1:ii+1), '(I1)') f
        CALL MVLENS(bxs(v), ww, f)
     END IF
  END DO

  ! finally find total power
  DO i=1,256
     bx = bxs(i)
     IF (SIZE(bx%labs).GT.0) THEN
        s = s + i * DOT_PRODUCT([(j,j=1,SIZE(bx%fs))], bx%fs)
     END IF
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD

  ! CALL PART1()
  CALL PART2()
END PROGRAM MAIN
