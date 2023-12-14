MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
CONTAINS

SUBROUTINE READMAP(map, nr, nc)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  CHARACTER(LEN=256) l
  INTEGER ios, nr, nc, i, j

  OPEN(10, FILE=fin, STATUS='OLD')
  nr = NLINES(10)
  READ(10, "(A)", IOSTAT=ios) l
  nc = LEN_TRIM(l)
  BACKSPACE(10)

  ALLOCATE(map(nr, nc))

  DO j=1,nr
     READ(10, "(*(A))") (map(j,i), i=1,nc)
  END DO

  CLOSE(10)
END SUBROUTINE READMAP

INTEGER FUNCTION CLOAD(col, nr)
  CHARACTER, DIMENSION(:) :: col
  INTEGER nr, i, pl

  cload = 0
  pl = nr
  DO i=1,nr
     SELECT CASE (col(i))
        CASE ('.')
           CYCLE
        CASE ('#')
           pl = nr - i
        CASE ('O')
           cload = cload + pl
           pl = pl - 1
     END SELECT
  END DO
END FUNCTION CLOAD

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER s, nr, nc, i

  s = 0
  CALL READMAP(map, nr, nc)
  ! CALL PRINTMTX(MAP)

  DO i=1,nc
     s = s + CLOAD(map(:,i),nr)
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

! ok fine I'll implement your stupid tilt
SUBROUTINE TILT(col, nr)
  CHARACTER, DIMENSION(:), ALLOCATABLE :: col
  CHARACTER, DIMENSION(:), ALLOCATABLE :: nucol
  INTEGER nr, i, free

  ALLOCATE(nucol(nr))
  nucol = '.'
  free = 1
  DO i=1,nr
     SELECT CASE (col(i))
        CASE ('.')
           CYCLE
        CASE ('#')
           nucol(i) = '#'
           free = i + 1
        CASE ('O')
           nucol(free) = 'O'
           free = free + 1
     END SELECT
  END DO
  CALL MOVE_ALLOC(nucol, col)
END SUBROUTINE TILT

SUBROUTINE ROT(brain, nr, nc)
  CHARACTER, DIMENSION(nr,nc) :: brain ! otherwise known as map
  INTEGER nr, nc

  ! rotate clockwise = (vertical reflection) o (transpose)
  brain = TRANSPOSE(brain)
  brain(:,1:nc) = brain(:,nc:1:-1)
END SUBROUTINE

INTEGER FUNCTION DLOAD(col, nr)
  CHARACTER, DIMENSION(:) :: col
  INTEGER nr, i

  dload = 0
  DO i=1,nr
     IF (col(i).EQ.'O') THEN
        dload = dload + (nr+1-i)
     END IF
  END DO
END FUNCTION DLOAD

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  CHARACTER, DIMENSION(:), ALLOCATABLE :: col
  CHARACTER, DIMENSION(:,:,:), ALLOCATABLE :: maps
  INTEGER s, nr, nc, i, r, c, d, m

  s = 0
  CALL READMAP(map, nr, nc)
  ! CALL PRINTMTX(MAP)

  ALLOCATE(maps(0:100000,nr,nc))
  maps(0,:,:) = map
  c = 0 ! cycle 0; cycle# = c
  cycle: DO
     c = c+1
     DO r=1,4
        DO i=1,nc
           col = map(:,i)
           CALL TILT(col, nr)
           map(:,i) = col
           DEALLOCATE(col)
        END DO
        CALL ROT(map, nr, nc)
     END DO

     DO d=0,c-1
        IF (ALL(maps(d,:,:).EQ.map)) THEN
           PRINT *, "FOUND CYCLE:", c-d, "starting at", d
           EXIT cycle
        ELSE
           maps(c,:,:) = map
        END IF
     END DO
  END DO cycle

  m = INT(MODULO(1000000000_int64 - d, c-d), KIND=INT32)

  map = maps(d,:,:)
  DO c=1,m
     ! cycle m more times
     DO r=1,4
        DO i=1,nc
           col = map(:,i)
           CALL TILT(col, nr)
           map(:,i) = col
           DEALLOCATE(col)
        END DO
        CALL ROT(map, nr, nc)
     END DO
  END DO

  ! PRINT *, "after:"
  ! CALL PRINTMTX(map)

  DO i=1,nc
     ! eric didn't want me to find out the tilted load
     ! so I had to write another function
     s = s + DLOAD(map(:,i),nr)
  END DO

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

