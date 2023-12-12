MODULE MOD
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
CONTAINS

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

RECURSIVE FUNCTION SOLVE(sprs, sgrp) RESULT(r)
  CHARACTER, DIMENSION(:) :: sprs
  CHARACTER c, nc
  INTEGER, DIMENSION(:) :: sgrp
  INTEGER g, i, ub
  INTEGER(KIND=INT64) r
  ! LOGICAL, DIMENSION(SIZE(sprs)) :: msk

  r = 0
  ! check whether we have enough space left
  IF (COUNT(sprs.EQ.'#' .OR. sprs.EQ.'?').LT.SUM(sgrp)) RETURN
  ! begin solving
  g = sgrp(1)
  ub = SIZE(sprs) - g + 1
  ! loop until upper bound of where solution can lie
  i = 0
  DO
     i = i + 1
     IF (i.GT.ub) EXIT

     ! msk = .FALSE.
     c = sprs(i)
     IF (c.EQ.'.') CYCLE
     IF (c.EQ.'#' .OR. c.EQ.'?') THEN
        ! prevent OOB error
        IF ((i+g).GT.SIZE(sprs)) THEN
           nc = '.'
        ELSE
           nc = sprs(i+g)
        END IF
        IF (ALL(sprs(i:i+g-1).EQ.'#' .OR. sprs(i:i+g-1).EQ.'?') .AND. &
             nc.NE.'#') THEN
           ! msk(i:i+g-1) = .TRUE.
           ! PRINT *, MERGE('#', sprs, msk)
           ! potential solution
           IF (SIZE(sgrp).EQ.1) THEN
              ! check whether we have too many # left
              IF (COUNT(sprs(i+g+1:).EQ.'#').GT.0) THEN
                 CONTINUE
              ELSE
                 ! PRINT *, "+1"
                 r = r + 1
              END IF
           ELSE
              r = r + SOLVE(sprs(i+g+1:), sgrp(2:))
           END IF
        END IF
        ! look no further if already fully filled
        IF (c.EQ.'#') THEN
           RETURN
        END IF
     END IF
  END DO
END FUNCTION SOLVE

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*256
  CHARACTER, DIMENSION(:), ALLOCATABLE :: sprs
  INTEGER ios, i, ws, nc
  INTEGER(KIND=INT64) s, j
  INTEGER, DIMENSION(:), ALLOCATABLE :: sgrp

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')

  i = 0
  DO
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     i = i + 1

     nc = COUNTSTR(TRIM(l), ',')
     ws = INDEX(l, ' ')
     ALLOCATE(sgrp(nc+1))
     ALLOCATE(sprs(ws-1))
     READ(l(1:ws-1), '(*(A))') sprs
     READ(l(ws+1:), *) sgrp

     j = SOLVE(sprs, sgrp)
     WRITE(6, '(1X, I0, 1X, I0)') i, j
     s = s + j

     DEALLOCATE(sgrp)
     DEALLOCATE(sprs)
  END DO

  CLOSE(10)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2(ln)
  IMPLICIT NONE
  CHARACTER l*256
  CHARACTER, DIMENSION(:), ALLOCATABLE :: sprs, bigsprs
  INTEGER ios, i, ws, nc, ln
  INTEGER(KIND=INT64) s, j
  INTEGER, DIMENSION(:), ALLOCATABLE :: sgrp, bigsgrp
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: cache

  s = 0
  ALLOCATE(cache(256,256))
  cache = -1
  OPEN(10, FILE=fin, STATUS='OLD')

  i = 0
  DO
     i = i + 1
     IF (i.LT.ln) THEN
        READ(10, '(A)')
     ELSE
        EXIT
     END IF
  END DO

  READ(10, '(A)') l
  ! IF (ios.EQ.IOSTAT_END) EXIT
  CLOSE(10)

  nc = COUNTSTR(TRIM(l), ',')
  ws = INDEX(l, ' ')
  ALLOCATE(sgrp(nc+1))
  ALLOCATE(sprs(ws-1))
  READ(l(1:ws-1), '(*(A))') sprs
  READ(l(ws+1:), *) sgrp

  ! God I fucking hate this
  ALLOCATE(bigsgrp(5*SIZE(sgrp)))
  ALLOCATE(bigsprs(5*SIZE(sprs)+5))

  CALL PUSHONEC(sprs, '?')

  bigsgrp = PACK(SPREAD(sgrp, 2, 5), .TRUE.)
  bigsprs = PACK(SPREAD(sprs, 2, 5), .TRUE.)
  CALL RESIZEC(bigsprs, SIZE(bigsprs)-1)

  j = SOLVE(bigsprs, bigsgrp)
  s = s + j

  DEALLOCATE(sgrp)
  DEALLOCATE(sprs)
  DEALLOCATE(bigsgrp)
  DEALLOCATE(bigsprs)

  WRITE(6,*) s
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD
  CHARACTER(len=16) lc
  INTEGER ln

  ! CALL PART1()
  ! I am cheating in part 2 by making it run
  ! on the university batch system then summing all the outputs
  CALL GET_COMMAND_ARGUMENT(1, lc)
  IF (LEN_TRIM(lc).EQ.0) STOP
  READ(lc, '(I4)') ln
  CALL PART2(ln)

END PROGRAM MAIN
