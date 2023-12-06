MODULE MOD
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! INTEGER, PARAMETER :: nraces = 3
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  INTEGER, PARAMETER :: nraces = 4
CONTAINS

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER(LEN=10) gbg
  INTEGER, DIMENSION(nraces) :: time, dist
  INTEGER, DIMENSION(:), ALLOCATABLE :: thold
  INTEGER s, i, tmax, dmin

  OPEN(10, FILE=fin, STATUS='OLD')
  s = 1

  READ(10, *) gbg, time
  READ(10, *) gbg, dist

  CLOSE(10)

  DO i=1,nraces
     tmax = time(i)
     dmin = dist(i)

     ALLOCATE(thold(tmax))
     thold = [(i, i=1,tmax)]
     s = s * COUNT(((tmax-thold)*thold).GT.dmin)
     DEALLOCATE(thold)
  END DO


  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  INTEGER(KIND=INT64), DIMENSION(:), ALLOCATABLE :: thold
  INTEGER s
  INTEGER(KIND=INT64) i, tmax, dmin

  OPEN(10, FILE=fin, STATUS='OLD')
  s = 1

  READ(10, '(10X, BN, I30)') tmax
  READ(10, '(10X, BN, I30)') dmin

  CLOSE(10)

  ALLOCATE(thold(tmax))
  thold = [(i, i=1,tmax)]
  s = s * COUNT(((tmax-thold)*thold).GT.dmin)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
  CALL PART2()
END PROGRAM MAIN
