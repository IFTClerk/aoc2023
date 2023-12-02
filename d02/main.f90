MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : IOSTAT_END
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
CONTAINS

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*256, col
  INTEGER ios, sum, is, ic, inxt, gid, ef
  INTEGER ncol

  OPEN(10, FILE=fin, STATUS='OLD')
  sum = 0

  DO
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT

     ! find colon
     ic = SCAN(l, ':')
     READ(l(6:ic-1), *) gid

     ! ditch the start
     l = l(ic+1:)
     ic = 0

     ! get semicolon or comma
     ef = 0
     ncol = 0
     DO
        is = SCAN(l, ';')
        ic = SCAN(l, ',')
        IF (is.GT.0 .AND. ic.GT.0) THEN
           ! take closest next entry
           inxt = MIN(is, ic)
        ELSEIF (is.EQ.0 .NEQV. ic.EQ.0) THEN
           ! either one is zero
           ! take the nonzero entry
           inxt = is + ic
        ELSE
           ! both are zero
           ! end of line
           inxt = LEN_TRIM(l)
           ! set exit flag
           ef = 1
        END IF

        READ(l(:inxt), *) ncol, col
        ! are we legal?
        IF (col.EQ.'r' .AND. ncol.GT.12) THEN
           gid = 0
        ELSEIF (col.EQ.'g' .AND. ncol.GT.13) THEN
           gid = 0
        ELSEIF (col.EQ.'b' .AND. ncol.GT.14) THEN
           gid = 0
        END IF

        ! chop off that bit we just did
        l = l(inxt+1:)
        ! break if we are at the end
        IF (ef.EQ.1) EXIT
     END DO

     ! WRITE(6,*) gid
     sum = sum + gid
  END DO

  CLOSE(10)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1 sum", sum
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER l*256, col
  INTEGER ios, sum, is, ic, inxt, ef
  INTEGER ncol, nr, ng, nb, p

  OPEN(11, FILE=fin, STATUS='OLD')
  sum = 0

  DO !for each game
     READ(11, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT

     ! find colon
     ic = SCAN(l, ':')
     ! ditch the start
     l = l(ic+1:)
     ic = 0

     ! get semicolon or comma
     ef = 0
     nr = 0
     ng = 0
     nb = 0
     DO
        is = SCAN(l, ';')
        ic = SCAN(l, ',')
        IF (is.GT.0 .AND. ic.GT.0) THEN
           ! take closest next entry
           inxt = MIN(is, ic)
        ELSEIF (is.EQ.0 .NEQV. ic.EQ.0) THEN
           ! either one is zero
           ! take the nonzero entry
           inxt = is + ic
        ELSE
           ! both are zero
           ! end of line
           inxt = LEN_TRIM(l)
           ! set exit flag
           ef = 1
        END IF

        READ(l(:inxt), *) ncol, col
        ! compare
        IF (col.EQ.'r') THEN
           nr = MAX(ncol, nr)
        ELSEIF (col.EQ.'g') THEN
           ng = MAX(ncol, ng)
        ELSEIF (col.EQ.'b') THEN
           nb = MAX(ncol, nb)
        END IF

        ! break if we are at the end
        IF (ef.EQ.1) EXIT
        ! chop off that bit we just did
        l = l(inxt+1:)
     END DO

     ! WRITE(6,*) nr, ng, nb
     p = nr * ng * nb
     sum = sum + p
  END DO

  CLOSE(11)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2 sum", sum
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD

  ! CALL PART1()
  CALL PART2()
END PROGRAM MAIN
