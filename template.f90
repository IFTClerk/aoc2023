MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "input.txt"
CONTAINS

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*256
  INTEGER ios, sum

  OPEN(10, FILE=fin, STATUS='OLD')
  sum = 0

  DO
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT

     WRITE(6,*) l
  END DO

  CLOSE(10)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", sum
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
END PROGRAM MAIN
