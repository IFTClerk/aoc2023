MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  INTEGER, PARAMETER :: NSTEP=26501365
  INTEGER, DIMENSION(2), PARAMETER :: nor=[-1,0], sou=[1,0], est=[0,1], wst=[0,-1]
  INTEGER, DIMENSION(2,4), PARAMETER :: dirs=RESHAPE([nor,sou,est,wst],[2,4])
CONTAINS

SUBROUTINE STEP(map, nr, nc, locs)
  INTEGER, INTENT(IN) :: nr, nc
  CHARACTER, DIMENSION(nr,nc), INTENT(IN) :: map
  INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: locs
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: tmp
  INTEGER nl, ni, nn, i, j, loc(2), nxt(2), nlocs(2,4)

  nl = SIZE(locs,DIM=2)
  ALLOCATE(tmp(2,0))
  DO i=1,nl
     ni = SIZE(tmp,DIM=2)
     loc = locs(:,1)
     locs = locs(:,2:)
     nn = 0
     DO j=1,4
        nxt = loc + dirs(:,j)
        IF (ANY(tmp(1,:).EQ.nxt(1) .AND. tmp(2,:).EQ.nxt(2))) CYCLE
        IF (nxt(1).LT.1 .OR. nxt(2).LT.1 .OR. &
             nxt(1).GT.nr .OR. nxt(2).GT.nc) CYCLE
        IF (map(nxt(1),nxt(2)).EQ.'#') CYCLE
        nn = nn + 1
        nlocs(:,nn) = nxt
     END DO
     tmp = RESHAPE([tmp, nlocs],[2,ni+nn])
  END DO
  CALL MOVE_ALLOC(tmp, locs)
END SUBROUTINE STEP

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: locs
  INTEGER s, nr, nc, i

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READMAT(10, map, nr, nc)
  CLOSE(10)
  ! CALL PRINTMAT(map)
  ALLOCATE(locs(2,1))
  locs(:,1) = FINDLOC(map, 'S')
  map(locs(1,1), locs(2,1)) = '.'
  DO i=1,64
     CALL STEP(map, nr, nc, locs)
  END DO
  s = SIZE(locs,DIM=2)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE STEP2(map, nr, nc, sloc, ns, odd, evn)
  INTEGER, INTENT(IN) :: nr, nc
  CHARACTER, DIMENSION(nr,nc), INTENT(IN) :: map
  LOGICAL, DIMENSION(nr,nc), INTENT(OUT) :: odd, evn
  INTEGER, INTENT(IN) :: sloc(2), ns
  LOGICAL, DIMENSION(nr,nc) :: msk
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: q, tmp
  INTEGER i, j, loc(2), nxt(2), nn

  msk = map.EQ.'#'
  odd = .FALSE.
  evn = .FALSE.
  ALLOCATE(q(2,1))
  q(:,1) = sloc
  evn(sloc(1),sloc(2)) = .TRUE. ! zero is even
  msk(sloc(1),sloc(2)) = .TRUE.
  DO i=1,ns
     nn = 0
     ALLOCATE(tmp(2,0))
     DO
        IF (SIZE(q).EQ.0) EXIT
        loc = q(:,1)
        q = q(:,2:)
        DO j=1,4
           nxt = loc + dirs(:,j)
           IF (nxt(1).LT.1 .OR. nxt(2).LT.1 .OR. &
                nxt(1).GT.nr .OR. nxt(2).GT.nc) CYCLE
           IF (map(nxt(1),nxt(2)).EQ.'#') CYCLE
           IF (msk(nxt(1),nxt(2))) CYCLE ! visited
           ! add to even or odd list
           IF (.EVEN.i) THEN
              evn(nxt(1),nxt(2)) = .TRUE.
           ELSE
              odd(nxt(1),nxt(2)) = .TRUE.
           END IF
           nn = nn + 1
           ! push into tmp
           tmp = RESHAPE([tmp,nxt],[2,nn])
           msk(nxt(1),nxt(2)) = .TRUE.
        END DO
     END DO
     CALL MOVE_ALLOC(tmp, q)
  END DO
END SUBROUTINE STEP2

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER nr, nc, np, nl, i, sloc(2)
  INTEGER(KIND=INT64) sodd, sevn, scar(4), sint(4), scnr(4)
  INTEGER(KIND=INT64) s
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: odd, evn, evn2, tevn

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READMAT(10, map, nr, nc)
  CLOSE(10)
  ! CALL PRINTMAT(map)
  nl = (NSTEP-nc/2)/nc
  sloc = FINDLOC(map, 'S')
  map(sloc(1),sloc(2)) = '.'
  ALLOCATE(odd(nr,nc))
  ALLOCATE(evn(nr,nc))
  ALLOCATE(evn2(nr,nc))
  ALLOCATE(tevn(nr,nc))
  CALL STEP2(map, nr, nc, sloc, nr*2, odd, evn)
  sodd = COUNT(odd)
  sevn = COUNT(evn)

  ! we take the even entries here because when I search from the edges,
  ! the final layout of the grid will be the ones where the elf ends up
  ! on the edge (odd number of steps from the start)
  ! step through nr-1 steps since the first step is moving onto the grid
  ! north
  sloc = [1,nc/2+1]
  CALL STEP2(map, nr, nc, sloc, nr-1, odd, evn)
  scar(1) = COUNT(evn)
  tevn = evn
  ! east
  sloc = [nr/2+1,nc]
  CALL STEP2(map, nr, nc, sloc, nc-1, odd, evn2)
  sint(1) = COUNT(evn.OR.evn2)
  scar(2) = COUNT(evn2)
  ! south
  sloc = [nr,nc/2+1]
  CALL STEP2(map, nr, nc, sloc, nr-1, odd, evn)
  sint(2) = COUNT(evn.OR.evn2)
  scar(3) = COUNT(evn)
  ! west
  sloc = [nr/2+1,1]
  CALL STEP2(map, nr, nc, sloc, nc-1, odd, evn2)
  sint(3) = COUNT(evn.OR.evn2)
  scar(4) = COUNT(evn2)
  sint(4) = COUNT(tevn.OR.evn2)
  ! similar to above, take the even entries since those will be where
  ! the elf ends up at the end of the steps
  ! corners
  np = nc - nc/2 - 2
  sloc = [1,1]
  CALL STEP2(map, nr, nc, sloc, np, odd, evn)
  scnr(1) = COUNT(evn)
  sloc = [1,nc]
  CALL STEP2(map, nr, nc, sloc, np, odd, evn)
  scnr(2) = COUNT(evn)
  sloc = [nr,1]
  CALL STEP2(map, nr, nc, sloc, np, odd, evn)
  scnr(3) = COUNT(evn)
  sloc = [nr,nc]
  CALL STEP2(map, nr, nc, sloc, np, odd, evn)
  scnr(4) = COUNT(evn)

  s = sodd
  DO i=1,nl-1
     IF (.EVEN.(i+1)) THEN
        s = s + i*4*sevn
     ELSE
        s = s + i*4*sodd
     END IF
  END DO
  s = s + SUM(scar) + (i-1)*SUM(sint) + i*SUM(scnr)

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
