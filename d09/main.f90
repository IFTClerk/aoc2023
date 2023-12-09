MODULE MOD
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! INTEGER, PARAMETER :: ns = 6
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  INTEGER, PARAMETER :: ns = 21
CONTAINS

SUBROUTINE EXTRAPOLATE(seq, finn, firn)
  INTEGER, DIMENSION(ns) :: seq, finn, firn
  INTEGER, DIMENSION(:), ALLOCATABLE :: diff, odiff
  INTEGER i, j

  finn = 0
  finn(ns) = seq(ns)
  firn = 0
  firn(ns) = seq(1)
  DO i=ns,1,-1
     IF (ALLOCATED(odiff)) DEALLOCATE(odiff)
     ALLOCATE(odiff(i))
     IF (i.EQ.ns) THEN
        odiff = seq
     ELSE
        odiff = diff
     END IF

     IF (ALLOCATED(diff)) DEALLOCATE(diff)
     ALLOCATE(diff(i-1))

     FORALL(j=1:i-1) diff(j) = odiff(j+1) - odiff(j)
     IF ((i-1).GT.0) THEN
        finn(i-1) = diff(i-1)
        firn(i-1) = diff(1)
     END IF
     IF (ALL(diff.EQ.0)) EXIT
  END DO
END SUBROUTINE EXTRAPOLATE

SUBROUTINE PART1()
  IMPLICIT NONE
  INTEGER, DIMENSION(ns) :: seq, finn, firn
  INTEGER ios, s

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')

  DO
     READ(10, *, IOSTAT=ios) seq
     IF (ios.EQ.IOSTAT_END) EXIT

     CALL EXTRAPOLATE(seq, finn, firn)
     s = s + SUM(finn)
  END DO

  CLOSE(10)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2
  IMPLICIT NONE
  INTEGER, DIMENSION(ns) :: seq, finn, firn
  INTEGER ios, s

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')

  DO
     READ(10, *, IOSTAT=ios) seq
     IF (ios.EQ.IOSTAT_END) EXIT

     ! smarter than original solution
     seq = seq(ns:1:-1)
     CALL EXTRAPOLATE(seq, finn, firn)
     s = s + SUM(finn)
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
