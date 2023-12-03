MODULE MOD
  USE UTILS
  IMPLICIT NONE

  INTEGER, PARAMETER :: long = SELECTED_INT_KIND(16)
  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "input.txt"
  CHARACTER*(*), PARAMETER :: fin = "Hxtu.txt"
CONTAINS

SUBROUTINE MAKESCHEM(scm, nr, nc)
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: scm
  CHARACTER*8192 l
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
           ! Find where this number ends
           nf = LOOKAHEAD(scm(1:, nj), ni)
           IF (ANY(ISS(scm(ni-1:ni+nf+1,nj-1:nj+1)))) THEN
              ! if any symbol around this number, read it
              b = ''
              DO nk=0,nf
                 b = TRIM(b) // scm(ni+nk, nj)
              END DO
              READ(b, *, IOSTAT=ios) s
              IF (ios.GT.0) THEN
                 WRITE(6,*) "Tried to read ", b, " but failed"
              END IF
              ! WRITE(6,*) s

              sum = sum + s
           END IF
           ! skip the numbers we already did
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

INTEGER FUNCTION ADJN(adj)
  LOGICAL, DIMENSION(3,3) :: adj
  adjn = 0
  ! numbers in line
  adjn = adjn + COUNT(adj(:,2))
  ! numbers above and below
  IF (adj(2,1)) THEN
     adjn = adjn + 1
  ELSE
     adjn = adjn + COUNT(adj(:,1))
  END IF
  IF (adj(2,3)) THEN
     adjn = adjn + 1
  ELSE
     adjn = adjn + COUNT(adj(:,3))
  END IF

END FUNCTION ADJN

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: scm
  LOGICAL, DIMENSION(3,3) :: adj
  CHARACTER c
  CHARACTER*64 b
  INTEGER nr, nc
  INTEGER ios, t, s
  INTEGER ni, nj, nn, nii, njj
  INTEGER nf, nb, nk
  INTEGER(KIND=long) :: sum

  sum = 0

  CALL MAKESCHEM(scm, nr, nc)
  scm = TRANSPOSE(scm)

  DO nj=1,nr
     DO ni=1,nc
        c = scm(ni, nj)
        IF (c.EQ.'*') THEN
           adj = ISN(scm(ni-1:ni+1,nj-1:nj+1))
           nn = ADJN(adj)
           ! continue if not exactly 2 numbers
           IF (nn.NE.2) CYCLE

           ! now we can read those numbers
           ! reduce the top and bottom rows
           IF (adj(2,1)) adj(:,1) = (/ .FALSE., .TRUE., .FALSE. /)
           IF (adj(2,3)) adj(:,3) = (/ .FALSE., .TRUE., .FALSE. /)

           s = 1
           DO njj=1,3
              DO nii=1,3
                 IF (adj(nii,njj)) THEN
                    ! find the whole number
                    nf = LOOKAHEAD(scm(ni+(nii-2):,nj+(njj-2)), 1)
                    nb = LOOKAHEAD(scm(ni+(nii-2):0:-1,nj+(njj-2)), 1)

                    ! read number
                    b = ''
                    DO nk=0,nf+nb
                       b = TRIM(b) // scm((ni+nii-2)-nb+nk, nj+njj-2)
                    END DO
                    READ(b, *, IOSTAT=ios) t
                    IF (ios.GT.0) THEN
                       WRITE(6,*) "Tried to read ", TRIM(b), " but failed"
                    END IF

                    s = s * t
                 END IF
              END DO
           END DO
           ! WRITE(6,*) s
           sum = sum + s
        END IF
     END DO
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", sum
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD

  ! CALL PART1()
  CALL PART2()
END PROGRAM MAIN
