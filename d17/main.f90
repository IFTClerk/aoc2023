MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  ! input is 141x141
  INTEGER, DIMENSION(2), PARAMETER :: nor=[-1,0], sou=[1,0], est=[0,1], wst=[0,-1]
  INTEGER, DIMENSION(2,4), PARAMETER :: dirs=RESHAPE([nor,sou,est,wst],[2,4])

  TYPE :: HEAD
     INTEGER, DIMENSION(:,:), ALLOCATABLE :: steps
     INTEGER, DIMENSION(:), ALLOCATABLE :: dists
  END TYPE HEAD
CONTAINS

SUBROUTINE PUSHCOL(mat, col)
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: mat, tmp
  INTEGER col(2), ns

  ns = SIZE(mat,DIM=2)
  ALLOCATE(tmp(2,ns+1))
  tmp(:,:ns) = mat
  tmp(:,ns+1) = col
  CALL MOVE_ALLOC(tmp, mat)
END SUBROUTINE PUSHCOL

INTEGER FUNCTION LOCCOL(mat, col)
  INTEGER, DIMENSION(:,:) :: mat
  INTEGER col(SIZE(mat,DIM=1)), i

  loccol = 0
  DO i=1,SIZE(mat,DIM=2)
     IF (ALL(mat(:,i).EQ.col)) THEN
        loccol = i
        RETURN
     END IF
  END DO
END FUNCTION LOCCOL

SUBROUTINE DIJKSTRA(map, nr, nc, hmap, p2)
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: map
    INTEGER, INTENT(IN) :: nr, nc
    INTEGER i, j, k, dd, dst, prop, c
    INTEGER cur(2), nxt(2), dir(2), xdir(2), stp(2)
    TYPE(HEAD), TARGET, DIMENSION(:,:), ALLOCATABLE :: hmap
    TYPE(HEAD), POINTER :: h, hx
    LOGICAL p2
    LOGICAL, DIMENSION(nr, nc) :: dirty

    IF (ALLOCATED(hmap)) DEALLOCATE(hmap)
    ALLOCATE(hmap(nr, nc))
    DO i=1,nc
       DO j=1,nr
          ALLOCATE(hmap(j,i)%steps(2,0))
          ALLOCATE(hmap(j,i)%dists(1))
          ! I don't have infinities but I have YUUUGE numbers
          hmap(j,i)%dists(1) = HUGE(dst)
       END DO
    END DO

    ! nothing is dirty
    dirty = .FALSE.

    ! mark starting point as dirty
    dirty(1,1) = .TRUE.
    hmap(1,1)%dists(1) = 0
    ! can go anywhere from start
    CALL PUSHCOL(hmap(1,1)%steps,[0,0])
    c = 0
    DO
       IF (.NOT.ANY(dirty)) EXIT

       ! find dirty spot and unmark dirty
       cur = FINDLOC(dirty,.TRUE.)
       dirty(cur(1),cur(2)) = .FALSE.

       ! loop over all possible paths to this point
       h => hmap(cur(1),cur(2))
       DO j=1,SIZE(h%steps,DIM=2)
          stp = h%steps(:,j)
          dst = h%dists(j)
          DO i=1,4 ! 4 cardinal directions
             dir = dirs(:,i)
             dd = DOT_PRODUCT(dir, stp)
             IF (.NOT.p2) THEN
                ! if going backwards or forwards for >3 times
                IF (dd.LT.0 .OR. dd.GE.3) CYCLE
             ELSE IF (p2) THEN
                ! part 2 extra solution
                ! forwards for >10 times
                IF (dd.LT.0 .OR. dd.GE.10) CYCLE
                IF (c.GT.0) THEN
                   ! after first move, if have not been moving straight
                   ! for 4 steps we cannot turn
                   IF (dd.EQ.0 .AND. SUM(ABS(stp)).LT.4) CYCLE
                END IF
             END IF

             nxt = cur + dir
             IF ((nxt(1).LT.1 .OR. nxt(1).GT.nr) .OR. &
                  (nxt(2).LT.1 .OR. nxt(2).GT.nc)) CYCLE

             xdir = dir * (dd + 1)
             prop = dst + map(nxt(1),nxt(2))
             hx => hmap(nxt(1),nxt(2))
             k = LOCCOL(hx%steps,xdir)
             IF (k.EQ.0) THEN
                ! add path if doesn't exist yet at node
                CALL PUSHCOL(hx%steps,xdir)
                IF (hx%dists(1).EQ.HUGE(prop)) THEN
                   hx%dists(1) = prop
                ELSE
                   CALL PUSHONE(hx%dists,prop)
                END IF
                ! mark dirty again
                dirty(nxt(1),nxt(2)) = .TRUE.
             ELSE
                ! replace if we have a better distance
                IF (prop.LT.hx%dists(k)) THEN
                   hx%dists(k) = prop
                   ! mark dirty again
                   dirty(nxt(1),nxt(2)) = .TRUE.
                END IF
             END IF
          END DO
       END DO
       ! hack to skip for first loop
       IF (c.EQ.0) c = 1
    END DO
END SUBROUTINE DIJKSTRA

SUBROUTINE PART1()
  IMPLICIT NONE
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER s, nr, nc
  TYPE(HEAD), DIMENSION(:,:), ALLOCATABLE :: hmap

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READMAT(10, map, nr, nc)
  CLOSE(10)

  CALL DIJKSTRA(map, nr, nc, hmap, .FALSE.)
  s = MINVAL(hmap(nr,nc)%dists)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER s, nr, nc
  TYPE(HEAD), DIMENSION(:,:), ALLOCATABLE :: hmap

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READMAT(10, map, nr, nc)
  CLOSE(10)

  CALL DIJKSTRA(map, nr, nc, hmap, .TRUE.)
  s = MINVAL(hmap(nr,nc)%dists)

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
