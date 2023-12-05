MODULE MOD
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! INTEGER, PARAMETER :: nseeds=4
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  INTEGER, PARAMETER :: nseeds=20
CONTAINS

INTEGER FUNCTION NEXTBLOCK(lu)
  CHARACTER l*256
  INTEGER lu, nl, i, ios

  nextblock = 0
  DO
     READ(lu, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     IF (LEN(TRIM(l)).EQ.0) EXIT
     nextblock = nextblock + 1
  END DO

  ! rewind
  DO i=1,nextblock+1
     BACKSPACE(lu)
  END DO

END FUNCTION NEXTBLOCK

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*256
  INTEGER(KIND=int64) seeds(nseeds), loc(nseeds)
  INTEGER ios, ic, nb, i, j
  INTEGER(KIND=int64) s
  INTEGER(KIND=int64), DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER(KIND=int64), DIMENSION(:), ALLOCATABLE :: srcs, srcd, dest
  LOGICAL, DIMENSION(:), ALLOCATABLE :: insrc

  OPEN(10, FILE=fin, STATUS='OLD')
  s = 0

  ! Read in seeds
  READ(10, "(A)", IOSTAT=ios) l
  IF (ios.EQ.IOSTAT_END) WRITE(6,*) "Failed to read seeds"
  ic = SCAN(l, ":")
  READ(l(ic+1:), *) seeds
  loc = seeds

  DO
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT

     ! start of new map
     IF (INDEX(l,":").GT.0) THEN
        nb = NEXTBLOCK(10)
     ELSE
        CYCLE
     END IF

     ! read in new map
     ALLOCATE(map(nb, 3))
     ALLOCATE(srcs(nb))
     ALLOCATE(srcd(nb))
     ALLOCATE(dest(nb))
     ALLOCATE(insrc(nb))
     DO i=1,nb
        READ(10,*) map(i,:)
     END DO

     srcs = map(:,2)
     srcd = map(:,2) + map(:,3)
     dest = map(:,1)
     DO i=1,nseeds
        ! find where the seed belongs in the intervals
        insrc = (srcs.LE.loc(i) .AND. srcd.GT.loc(i))
        DO j=1,nb
           IF (insrc(j)) THEN
              ! offset by correct amount
              loc(i) = dest(j) + loc(i) - srcs(j)
           END IF
        END DO
     END DO

     ! PRINT*, loc

     DEALLOCATE(map)
     DEALLOCATE(srcs)
     DEALLOCATE(srcd)
     DEALLOCATE(dest)
     DEALLOCATE(insrc)
  END DO

  CLOSE(10)

  s = MINVAL(loc)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
END PROGRAM MAIN
