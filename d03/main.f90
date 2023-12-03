MODULE MOD
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
CONTAINS

SUBROUTINE MAKESCHEM(scm, nr, nc)
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: scm
  CHARACTER*256 l
  INTEGER ios, nr, nc, nl

  OPEN(UNIT=50, FILE=fin, STATUS='old')

  READ(50, '(A)', IOSTAT=ios) l
  IF (ios.EQ.IOSTAT_END) nc=0
  nc = LEN_TRIM(l)
  CLOSE(50)

  nr = NLINES(fin)

  IF (nr.EQ.0 .OR. nc.EQ.0) THEN
     WRITE(6,*) "Unable to find valid schematic in ", fin
  END IF

  ALLOCATE(scm(0:nr+1, 0:nc+1))
  scm(:,:) = '.'

  OPEN(51, FILE=fin, STATUS='OLD')

  ! PRINT *, scm(0,:)
  DO nl=1,nr
     READ(51, "(*(A))", IOSTAT=ios) scm(nl, 1:nc)
     IF (ios.EQ.IOSTAT_END) EXIT
     ! PRINT *, scm(nl,:)
  END DO
  CLOSE(51)
  ! PRINT *, scm(nr+1,:)

END SUBROUTINE

ELEMENTAL LOGICAL FUNCTION ISS(c)
  CHARACTER, INTENT(in) :: c

  iss = (.NOT.ISN(c)) .AND. c.NE.'.'
END FUNCTION ISS

INTEGER FUNCTION LOOKAHEAD(row, n)
  CHARACTER, DIMENSION(*) :: row
  INTEGER n

  lookahead = 0
  DO
     IF (ISN(row(n+lookahead+1))) THEN
        lookahead = lookahead + 1
     ELSE
        EXIT
     END IF
  END DO
END FUNCTION LOOKAHEAD

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: scm
  CHARACTER c
  CHARACTER*64 b
  INTEGER nr, nc
  INTEGER ios, sum, ni, nj, nk, nf, s

  CALL MAKESCHEM(scm, nr, nc)
  scm = TRANSPOSE(scm)

  sum = 0
  DO nj=1,nr
     ni = 1
     DO
        c = scm(ni, nj)
        IF (ISN(c)) THEN
           nf = LOOKAHEAD(scm(1:, nj), ni)
           ! WRITE(6,*) nf
           IF (ANY(ISS(scm(ni-1:ni+nf+1,nj-1:nj+1)))) THEN
              b = ''
              DO nk=0,nf
                 b = TRIM(b) // scm(ni+nk, nj)
              END DO
              READ(b, *, IOSTAT=ios) s
              IF (ios.GT.0) THEN
                 WRITE(6,*) "Tried to read ", b, " but failed"
              END IF
              WRITE(6,*) s
              sum = sum + s
           END IF
           ni = ni + nf
        END IF
        ni = ni + 1
        IF (ni.GT.nc) EXIT
     END DO
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", sum
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
END PROGRAM MAIN
