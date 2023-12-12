MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! INTEGER, PARAMETER :: nwin=5, nscr=8
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  INTEGER, PARAMETER :: nwin=10, nscr=25
CONTAINS

SUBROUTINE COUNTCARDS(lu, nm)
  CHARACTER l*256
  INTEGER ios, ic, ib, i, j , lu
  INTEGER win(nwin), scr(nscr), nm

  READ(lu, "(A)", IOSTAT=ios) l
  IF (ios.EQ.IOSTAT_END) RETURN

  ic = SCAN(l, ":")
  ib = SCAN(l, "|")
  READ(l(ic+1:), *) win
  READ(l(ib+1:), *) scr

  ! how many did we win?
  nm = 0
  DO i=1,nwin
     DO j=1,nscr
        IF (win(i).EQ.scr(j)) THEN
           nm = nm + 1
        END IF
     END DO
  END DO

END SUBROUTINE COUNTCARDS

SUBROUTINE PART1()
  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: cw(:)
  INTEGER s, ncw, i

  ncw = NLINES(fin)
  ALLOCATE(cw(ncw))

  OPEN(10, FILE=fin, STATUS='OLD')

  DO i=1,ncw
     CALL COUNTCARDS(10, cw(i))
  END DO

  CLOSE(10)

  s = SUM(2**(cw-1))

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: cw(:), xcw(:)
  INTEGER s, ncw, i, j

  ncw = NLINES(fin)
  ALLOCATE(cw(ncw))
  ALLOCATE(xcw(ncw))

  OPEN(10, FILE=fin, STATUS='OLD')

  DO i=1,ncw
     CALL COUNTCARDS(10, cw(i))
  END DO

  CLOSE(10)

  xcw = 0
  DO i=1,ncw
     IF (cw(i).GT.0) THEN
        DO j=1,MIN(cw(i),ncw-i)
           xcw(i+j) = xcw(i+j) + 1 + xcw(i)
        END DO
     END IF
  END DO

  s = SUM(xcw)+ncw

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
  ! CALL PART2()
END PROGRAM MAIN
