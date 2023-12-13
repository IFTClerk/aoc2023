MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
CONTAINS

SUBROUTINE NEXTPAT(nr)
  CHARACTER(LEN=256) l
  INTEGER ios, nr, i

  nr = 0
  DO
     READ(10, "(A)", IOSTAT=ios) l
     IF (LEN_TRIM(l).EQ.0) EXIT
     IF (ios.EQ.IOSTAT_END) EXIT
     nr = nr + 1
  END DO
  DO i=1,nr+1
     BACKSPACE(10)
  END DO
END SUBROUTINE NEXTPAT

SUBROUTINE READPAT(pat, nr, nc)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: pat
  CHARACTER(LEN=256) l
  INTEGER ios, nr, nc, i, j

  CALL NEXTPAT(nr)
  READ(10, "(A)", IOSTAT=ios) l
  nc = LEN_TRIM(l)
  BACKSPACE(10)

  IF (ALLOCATED(pat)) DEALLOCATE(pat)
  ALLOCATE(pat(nr, nc))

  DO j=1,nr
     READ(10, "(*(A))") (pat(j,i), i=1,nc)
  END DO
END SUBROUTINE READPAT

SUBROUTINE FINDREFL(pat, nr, nc, lh, lv)
  CHARACTER, DIMENSION(:,:) :: pat
  INTEGER lh, lv, nr, nc, i, lr

  ! use transpose and run the same routine twice if you feel like a smartass
  ! I'm not going to
  IF (lv.EQ.0) THEN
     ! find vertical line
     DO i=1,nc-1
        lr = MIN(i, nc-i)-1
        IF (ALL(pat(:,i:i-lr:-1).EQ.pat(:,i+1:i+1+lr))) THEN
           ! PRINT *, "FOUND V REFLECTION", i
           lv = i
           RETURN
        END IF
     END DO
  END IF

  IF (lh.EQ.0) THEN
     ! find hotizontal line
     DO i=1,nr-1
        lr = MIN(i, nr-i)-1
        IF (ALL(pat(i:i-lr:-1,:).EQ.pat(i+1:i+1+lr,:))) THEN
           ! PRINT *, "FOUND H REFLECTION", i
           lh = i
           RETURN
        END IF
     END DO
  END IF
END SUBROUTINE FINDREFL

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: pat
  INTEGER ios, s, nr, nc, lh, lv

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')

  DO
     CALL READPAT(pat, nr, nc)

     lh = 0
     lv = 0
     CALL FINDREFL(pat, nr, nc, lh, lv)

     s = s + lv + 100*lh

     READ(10, *, IOSTAT=ios)
     IF (ios.EQ.IOSTAT_END) EXIT
  END DO

  CLOSE(10)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE FINDREFL2(pat, nr, nc, lh, lv)
  CHARACTER, DIMENSION(:,:) :: pat
  INTEGER lh, lv, nr, nc, i, lr

  IF (lv.EQ.0) THEN
     ! find vertical line
     DO i=1,nc-1
        lr = MIN(i, nc-i)-1
        IF (COUNT(pat(:,i:i-lr:-1).NE.pat(:,i+1:i+1+lr)).EQ.1) THEN
           ! PRINT *, "FOUND NEW V REFLECTION", i
           lv = i
           RETURN
        END IF
     END DO
  END IF

  IF (lh.EQ.0) THEN
     ! find hotizontal line
     DO i=1,nr-1
        lr = MIN(i, nr-i)-1
        IF (COUNT(pat(i:i-lr:-1,:).NE.pat(i+1:i+1+lr,:)).EQ.1) THEN
           ! PRINT *, "FOUND NEW H REFLECTION", i
           lh = i
           RETURN
        END IF
     END DO
  END IF
END SUBROUTINE FINDREFL2

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: pat
  INTEGER ios, s, nr, nc, lh, lv

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')

  DO
     CALL READPAT(pat, nr, nc)

     lh = 0
     lv = 0
     CALL FINDREFL2(pat, nr, nc, lh, lv)

     s = s + lv + 100*lh

     READ(10, *, IOSTAT=ios)
     IF (ios.EQ.IOSTAT_END) EXIT
  END DO

  CLOSE(10)

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
