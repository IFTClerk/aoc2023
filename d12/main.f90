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
     ! WRITE(6, '(1X, I0, 1X, I0)') i, j
     s = s + j

     DEALLOCATE(sgrp)
     DEALLOCATE(sprs)
  END DO

  CLOSE(10)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

! In this function instead of straight passing the sliced array
! we pass it the big array and the slicing indices
! then make it look up in the cache whether we already have the
! answer or not
! If not, then slice the array according to how we would have in
! part 1 and run the same routine
RECURSIVE FUNCTION SOLVE2(sprs, sgrp, cache, np, ng) RESULT(r)
  CHARACTER, DIMENSION(:) :: sprs
  CHARACTER, DIMENSION(:), ALLOCATABLE :: tsprs
  CHARACTER c, nc
  INTEGER, DIMENSION(:) :: sgrp
  INTEGER, DIMENSION(:), ALLOCATABLE :: tsgrp
  INTEGER g, i, ub, np, ng
  INTEGER(KIND=INT64) r
  INTEGER(KIND=INT64), DIMENSION(:,:), ALLOCATABLE :: cache

  r = 0
  ! check in cache
  IF (cache(np, ng).NE.-1) THEN
     r = cache(np, ng)
     RETURN
  END IF
  tsprs = sprs(np:)
  tsgrp = sgrp(ng:)
  ! check whether we have enough space left
  IF (COUNT(tsprs.EQ.'#' .OR. tsprs.EQ.'?').LT.SUM(tsgrp)) THEN
     cache(np, ng) = 0
     RETURN
  END IF
  ! begin solving
  g = tsgrp(1)
  ub = SIZE(tsprs) - g + 1
  ! loop until upper bound of where solution can lie
  i = 0
  DO
     i = i + 1
     IF (i.GT.ub) EXIT

     c = tsprs(i)
     IF (c.EQ.'.') CYCLE
     IF (c.EQ.'#' .OR. c.EQ.'?') THEN
        ! prevent OOB error
        IF ((i+g).GT.SIZE(tsprs)) THEN
           nc = '.'
        ELSE
           nc = tsprs(i+g)
        END IF
        IF (ALL(tsprs(i:i+g-1).EQ.'#' .OR. tsprs(i:i+g-1).EQ.'?') .AND. &
             nc.NE.'#') THEN
           ! potential solution
           IF (SIZE(tsgrp).EQ.1) THEN
              ! check whether we have too many # left
              IF (COUNT(tsprs(i+g+1:).EQ.'#').GT.0) THEN
                 CONTINUE
              ELSE
                 r = r + 1
              END IF
           ELSE
              r = r + SOLVE2(sprs, sgrp, cache, np+i+g, ng+1)
           END IF
        END IF
        ! look no further if already fully filled
        IF (c.EQ.'#') THEN
           cache(np, ng) = r
           RETURN
        END IF
     END IF
  END DO

  cache(np, ng) = r
END FUNCTION SOLVE2

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER l*256
  CHARACTER, DIMENSION(:), ALLOCATABLE :: sprs, bigsprs
  INTEGER ios, i, ws, nc
  INTEGER(KIND=INT64) s, j
  INTEGER, DIMENSION(:), ALLOCATABLE :: sgrp, bigsgrp
  INTEGER(KIND=INT64), DIMENSION(:,:), ALLOCATABLE :: cache

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

     ! God I fucking hate this
     ALLOCATE(bigsgrp(5*SIZE(sgrp)))
     ALLOCATE(bigsprs(5*SIZE(sprs)+5))

     CALL PUSHONEC(sprs, '?')

     bigsgrp = PACK(SPREAD(sgrp, 2, 5), .TRUE.)
     bigsprs = PACK(SPREAD(sprs, 2, 5), .TRUE.)
     CALL RESIZEC(bigsprs, SIZE(bigsprs)-1)

     ALLOCATE(cache(SIZE(bigsprs)+10,SIZE(bigsgrp)))
     cache = -1

     j = SOLVE2(bigsprs, bigsgrp, cache, 1, 1)
     ! WRITE(6, '(1X, I0, 1X, I0)') i, j
     s = s + j

     DEALLOCATE(sgrp)
     DEALLOCATE(sprs)
     DEALLOCATE(bigsgrp)
     DEALLOCATE(bigsprs)
     DEALLOCATE(cache)
  END DO

  CLOSE(10)

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
