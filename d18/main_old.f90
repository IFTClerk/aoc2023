MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  INTEGER, DIMENSION(2), PARAMETER :: UP=[-1,0],DN=[1,0],LT=[0,-1],RT=[0,1]
  INTEGER, DIMENSION(2,4), PARAMETER :: DIRS=RESHAPE([UP,DN,LT,RT],[2,4])
CONTAINS

! SUBROUTINE PUSHCOL(mat, col)
!   INTEGER, DIMENSION(:,:), ALLOCATABLE :: mat, tmp
!   INTEGER col(2), ns

!   ns = SIZE(mat,DIM=2)
!   ALLOCATE(tmp(2,ns+1))
!   tmp(:,:ns) = mat
!   tmp(:,ns+1) = col
!   CALL MOVE_ALLOC(tmp, mat)
! END SUBROUTINE PUSHCOL

SUBROUTINE DIG(map, dir, dis, sr, sc)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: map
  CHARACTER, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: dir
  CHARACTER r
  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: dis
  INTEGER, DIMENSION(:), ALLOCATABLE :: locs
  INTEGER pos(2), mov(2), ni, i, j, s, ml, mr, mu, md, lb(2)
  INTEGER, INTENT(OUT) :: sr, sc

  ni = SIZE(dir)
  IF (SIZE(dis).NE.ni) THEN
     PRINT *, "Instruction length mismatch"
     RETURN
  END IF
  ! current position
  pos = [0,0]
  map(pos(1),pos(2)) = '#'

  DO i=1,ni
     r = dir(i)
     s = dis(i)
     SELECT CASE (r)
        CASE ('U')
           mov = UP
        CASE ('D')
           mov = DN
        CASE ('L')
           mov = LT
        CASE ('R')
           mov = RT
     END SELECT
     DO j=1,s
        pos = pos + mov
        map(pos(1),pos(2)) = '#'
     END DO
  END DO

  locs = FINDLOC(map, '#', DIM=1)
  mu = MINVAL(locs, locs.NE.0)
  locs = FINDLOC(map, '#', DIM=1, BACK=.TRUE.)
  md = MAXVAL(locs, locs.NE.0)

  locs = FINDLOC(map, '#', DIM=2)
  ml = MINVAL(locs, locs.NE.0)
  locs = FINDLOC(map, '#', DIM=2, BACK=.TRUE.)
  mr = MAXVAL(locs, locs.NE.0)

  lb = lbound(map) - 1
  map = map(lb(1)+mu:lb(1)+md,lb(2)+ml:lb(2)+mr)
  sr = 1-lb(1)-mu
  sc = 1-lb(2)-ml
END SUBROUTINE DIG

SUBROUTINE FILL(fmap, sr, sc)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: fmap
  INTEGER i, j, sr, sc, pos(2), nxt(2)
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: q

  ! IF (fmap(sr, sc).NE.'#') THEN
  !    PRINT *, "Starting location is not a hole"
  !    RETURN
  ! END IF
  PRINT *, "Filling from ", sr, sc

  ALLOCATE(q(SIZE(fmap,DIM=1),SIZE(fmap,DIM=2)))
  q = .FALSE.
  q(sr,sc) = .TRUE.

  DO
     IF (.NOT.ANY(q)) EXIT
     ! this function is a O(N^2) loop, haha
     pos = FINDLOC(q, .TRUE.)
     fmap(pos(1),pos(2)) = '#'
     q(pos(1),pos(2)) = .FALSE.
     DO i=1,4
        nxt = pos+DIRS(:,i)
        IF (fmap(nxt(1),nxt(2)).EQ.'.') THEN
           q(nxt(1),nxt(2)) = .TRUE.
        END IF
     END DO
  END DO

END SUBROUTINE FILL

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*256
  CHARACTER, DIMENSION(:), ALLOCATABLE :: dir
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map, fmap
  INTEGER ios, s, nl, i, nrp, nrn, ncp, ncn, sr, sc
  INTEGER, DIMENSION(:), ALLOCATABLE :: dis

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  nl = NLINES(10)
  ALLOCATE(dir(nl))
  ALLOCATE(dis(nl))
  DO
     READ(10, *, IOSTAT=ios) (dir(i), dis(i), l, i=1,nl)
     IF (ios.EQ.IOSTAT_END) EXIT
  END DO
  CLOSE(10)

  ! find out max distance in each direction
  nrp = SUM(dis, dir.EQ.'D')
  nrn = SUM(dis, dir.EQ.'U')
  ncp = SUM(dis, dir.EQ.'R')
  ncn = SUM(dis, dir.EQ.'L')
  ALLOCATE(map(-nrn:nrp,-ncn:ncp))
  map = '.'

  ! after this the map is indexed like a normal person would
  CALL DIG(map, dir, dis, sr, sc)
  ! CALL PRINTMAT(map)

  fmap = map
  ! CALL PRINTMAT(fmap)
  ! hard coded, whatever
  CALL FILL(fmap, sr+1, sc+1)

  s = COUNT(fmap.EQ.'#')

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
END PROGRAM MAIN
