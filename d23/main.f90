MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  INTEGER, PARAMETER :: sloc(2)=[1,2]
  INTEGER, DIMENSION(2), PARAMETER :: nor=[-1,0], sou=[1,0], est=[0,1], wst=[0,-1]
  INTEGER, DIMENSION(2,4), PARAMETER :: dirs=RESHAPE([nor,sou,est,wst],[2,4])
CONTAINS

LOGICAL FUNCTION BOUNDED(rc, nr, nc)
  INTEGER, INTENT(IN) :: rc(2), nr, nc

  bounded = .TRUE.
  IF (ANY(rc.LT.1)) bounded = .FALSE.
  IF (rc(1).GT.nr) bounded = .FALSE.
  IF (rc(2).GT.nc) bounded = .FALSE.
END FUNCTION BOUNDED

SUBROUTINE READADJ(adj, nn, map, nr, nc, isx)
  CHARACTER, DIMENSION(nr,nc), INTENT(IN) :: map
  INTEGER, INTENT(IN) :: nr, nc, nn
  INTEGER i, j, k, n, m, loc(2), nbr(2)
  INTEGER, DIMENSION(2,nn) :: ntom
  INTEGER, DIMENSION(nn,nn), INTENT(OUT) :: adj
  LOGICAL, DIMENSION(nn) :: isx

  adj = 0
  n = 0
  isx = .TRUE.
  DO j=1,nc
     DO i=1,nr
        IF (map(i,j).EQ.'#') CYCLE
        n = n + 1
        ntom(:,n) = [i,j]
     END DO
  END DO
  IF (n.NE.nn) WRITE(6,*) "Did not find the right number of nodes", n, nn
  DO i=1,n
     loc = ntom(:,i)
     ! check neighbours
     DO j=1,4
        IF (map(loc(1),loc(2)).EQ.'v' .AND. j.NE.2) CYCLE
        IF (map(loc(1),loc(2)).EQ.'>' .AND. j.NE.3) CYCLE
        nbr = loc + dirs(:,j)
        IF (.NOT.BOUNDED(nbr, nr, nc)) CYCLE
        IF (map(nbr(1),nbr(2)).EQ.'.') isx(i) = .FALSE.
        IF (map(nbr(1),nbr(2)).EQ.'v' .AND. j.EQ.1) CYCLE
        IF (map(nbr(1),nbr(2)).EQ.'>' .AND. j.EQ.4) CYCLE
        ! add neighbour to adj mat
        m = FINDLOC([(ALL(ntom(:,k).EQ.nbr), k=1,nn)], .TRUE., DIM=1)
        IF (m.GT.0) adj(i,m) = 1
     END DO
  END DO
  ! CALL PRINTMAT(TRANSPOSE(ntom))
END SUBROUTINE READADJ

SUBROUTINE READADJ2(adj, nn, map, nr, nc, isx)
  CHARACTER, DIMENSION(nr,nc), INTENT(IN) :: map
  INTEGER, INTENT(IN) :: nr, nc, nn
  INTEGER i, j, k, n, m, loc(2), nbr(2)
  INTEGER, DIMENSION(2,nn) :: ntom
  INTEGER, DIMENSION(nn,nn), INTENT(OUT) :: adj
  LOGICAL, DIMENSION(nn) :: isx

  adj = 0
  n = 0
  isx = .TRUE.
  DO j=1,nc
     DO i=1,nr
        IF (map(i,j).EQ.'#') CYCLE
        n = n + 1
        ntom(:,n) = [i,j]
     END DO
  END DO
  IF (n.NE.nn) WRITE(6,*) "Did not find the right number of nodes", n, nn
  DO i=1,n
     loc = ntom(:,i)
     ! check neighbours
     DO j=1,4
        nbr = loc + dirs(:,j)
        IF (.NOT.BOUNDED(nbr, nr, nc)) CYCLE
        IF (map(nbr(1),nbr(2)).EQ.'.') isx(i) = .FALSE.
        ! add neighbour to adj mat
        m = FINDLOC([(ALL(ntom(:,k).EQ.nbr), k=1,nn)], .TRUE., DIM=1)
        IF (m.GT.0) adj(i,m) = 1
     END DO
  END DO
END SUBROUTINE READADJ2

SUBROUTINE FINDXROADS(adj, nn, xroads, isx, nx)
  INTEGER, DIMENSION(nn,nn), TARGET, INTENT(IN) :: adj
  INTEGER, INTENT(IN) :: nn
  INTEGER, INTENT(INOUT) :: nx
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: xroads
  INTEGER, DIMENSION(:), POINTER :: ca
  INTEGER, DIMENSION(:), ALLOCATABLE :: q, toq
  INTEGER i, cur, nt, ns, fro
  INTEGER, DIMENSION(nn) :: ln
  LOGICAL, DIMENSION(nn), INTENT(IN) :: isx
  LOGICAL, DIMENSION(nn) :: seen

  ln = [(i, i=1,nn)]
  ALLOCATE(q(1))
  q(1) = 1
  nx = 0
  seen = .FALSE.
  DO
     IF (SIZE(q).EQ.0) EXIT
     cur = q(1)
     q = q(2:)
     fro = cur
     ns = 0
     DO
        ns = ns + 1
        ca => adj(cur,:)
        IF (.NOT.isx(cur)) seen(cur)=.TRUE.
        toq = PACK(ln, ca.EQ.1 .AND. .NOT.seen)
        nt = SIZE(toq)
        IF (nt.EQ.0) EXIT ! end node
        IF (isx(cur)) EXIT
        cur = toq(1)
     END DO
     ! is a crossroad/end
     nx = nx + 1
     xroads = RESHAPE([xroads, [fro, cur, ns-1]], [3,nx])
     DO i=1,nt
        IF (ANY(toq(i).EQ.q)) CYCLE
        q = [q, toq(i)]
     END DO
  END DO
END SUBROUTINE FINDXROADS

SUBROUTINE FINDXROADS2(adj, nn, xroads, isx, nx)
  INTEGER, DIMENSION(nn,nn), TARGET, INTENT(IN) :: adj
  INTEGER, INTENT(IN) :: nn
  INTEGER, INTENT(INOUT) :: nx
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: xroads
  INTEGER, DIMENSION(:), POINTER :: ca, cb
  INTEGER, DIMENSION(:), ALLOCATABLE :: q, toq
  INTEGER i, j, cur, nt, ns, fro, is, os
  INTEGER, DIMENSION(nn) :: ln
  LOGICAL, DIMENSION(nn), INTENT(IN) :: isx
  LOGICAL, DIMENSION(nn) :: seen

  ln = [(i, i=1,nn)]
  ALLOCATE(q(1))
  q(1) = 1
  nx = 0
  seen = .FALSE.
  DO
     IF (SIZE(q).EQ.0) EXIT
     cur = q(1)
     q = q(2:)
     fro = cur
     ns = 0
     DO
        ns = ns + 1
        ca => adj(cur,:)
        ! cb => adj(:,cur)
        IF (.NOT.isx(cur)) seen(cur)=.TRUE.
        toq = PACK(ln, ca.EQ.1 .AND. .NOT.seen)
        nt = SIZE(toq)
        IF (nt.EQ.0) EXIT ! end node
        IF (ns.EQ.1) THEN
           toq = PACK(toq, [(.NOT.isx(toq(j)), j=1,nt)])
        END IF
        IF (isx(cur)) EXIT
        cur = toq(1)
     END DO
     nx = nx + 1
     xroads = RESHAPE([xroads, [fro, cur, ns-1]], [3,nx])
     DO i=1,nt
        IF (ANY(toq(i).EQ.q)) CYCLE
        q = [q, toq(i)]
     END DO
  END DO
END SUBROUTINE FINDXROADS2

RECURSIVE SUBROUTINE DFS(adj, nn, xroads, nx, sn, nstep, cstep)!, seen)
  INTEGER, INTENT(IN) :: nn, sn, nx
  INTEGER, INTENT(INOUT) :: cstep
  INTEGER, DIMENSION(3,nx) :: xroads
  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: nstep
  INTEGER, DIMENSION(nn,nn), TARGET, INTENT(IN) :: adj
  INTEGER, DIMENSION(:), POINTER :: ca
  INTEGER, DIMENSION(:), ALLOCATABLE :: nxt
  INTEGER i, j, cur, cs, ln(nn)
  ! LOGICAL, DIMENSION(nn) :: seen, se

  ln = [(i, i=1,nn)]
  cur = sn
  cs = cstep
  DO i=1,nx
     IF (xroads(1,i).EQ.cur) THEN
        ! print *, xroads(:,i)
        cs = cs + xroads(3,i)
        IF (xroads(2,i).EQ.nn) THEN
           nstep = [nstep, cs]
           RETURN
        END IF
     ELSE
        CYCLE
     END IF
     ! find next points
     ca => adj(xroads(2,i),:)
     ! IF (SUM(ca).GT.2) PRINT *, "Somehow crossroad ", i, " has more than 2 exits"
     cs = cs + 1 ! stepping onto next tile
     nxt = PACK(ln, ca.EQ.1)
     DO j=1,SIZE(nxt)
        ! PRINT *, "next: ", nxt(j)
        CALL DFS(adj, nn, xroads, nx, nxt(j), nstep, cs)
     END DO
  END DO
END SUBROUTINE DFS

RECURSIVE SUBROUTINE DFS2(adj, nn, sn, nstep, cstep, seen)
  INTEGER, INTENT(IN) :: nn, sn
  INTEGER, INTENT(INOUT) :: cstep
  ! INTEGER, DIMENSION(3,nx) :: xroads
  INTEGER, INTENT(INOUT) :: nstep
  INTEGER, DIMENSION(nn,nn), TARGET, INTENT(IN) :: adj
  INTEGER, DIMENSION(:), POINTER :: ca
  INTEGER, DIMENSION(:), ALLOCATABLE :: nxt
  INTEGER i, j, cur, cs, ln(nn)
  LOGICAL, DIMENSION(nn) :: seen, se

  IF (sn.EQ.nn) THEN
     IF (cstep.GT.nstep) THEN
        nstep = cstep
        PRINT *, cstep
     END IF
     RETURN
  END IF
  ln = [(i, i=1,nn)]
  cur = sn
  ca => adj(cur,:)
  nxt = PACK(ln, (ca.EQ.1 .AND. .NOT.seen))
  DO j=1,SIZE(nxt)
     cs = cstep
     cs = cs + 1
     se = seen
     se(cur) = .TRUE.
     ! PRINT *, "next: ", nxt(j)
     CALL DFS2(adj, nn, nxt(j), nstep, cs, se)
  END DO
END SUBROUTINE DFS2

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER s, nr, nc, nn, nx, cstep
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: adj
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: xroads
  INTEGER, DIMENSION(:), ALLOCATABLE :: nstep
  LOGICAL, DIMENSION(:), ALLOCATABLE :: isx

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READMAT(10, map, nr, nc)
  CLOSE(10)
  ! CALL PRINTMAT(map)

  nn = COUNT(map.EQ.'.' .OR. map.EQ.'v' .OR. map.EQ.'>')
  ALLOCATE(adj(nn,nn))
  ALLOCATE(isx(nn))
  CALL READADJ(adj, nn, map, nr, nc, isx)
  ! adj(i,j) means connection from i to j
  ! CALL PRINTMAT(adj)

  ALLOCATE(xroads(3,0))
  CALL FINDXROADS(adj, nn, xroads, isx, nx)
  ! CALL PRINTMAT(xroads)

  ALLOCATE(nstep(0))
  cstep = 0
  CALL DFS(adj, nn, xroads, nx, 1, nstep, cstep)
  ! PRINT *, nstep
  s = MAXVAL(nstep)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE XTOADJ(xroads, nx, adj, isx, nn, xadj, nnx)
  INTEGER, INTENT(IN) :: nx, nn, nnx
  INTEGER, DIMENSION(3,nx), INTENT(IN) :: xroads
  INTEGER, DIMENSION(nnx,nnx), INTENT(OUT) :: xadj
  INTEGER, DIMENSION(nn,nn), INTENT(IN) :: adj
  INTEGER, DIMENSION(nnx) :: ntox
  INTEGER i, x, cx(3), nf, nt
  LOGICAL, DIMENSION(nn), INTENT(IN) :: isx

  xadj = 0
  ntox(1) = 1
  ntox(nnx) = nn
  x = 2
  DO i=1,nn
     IF (isx(i)) THEN
        ntox(x) = i
        x = x + 1
     END IF
  END DO
  IF (x.NE.nnx) PRINT *, "Not enough nodes found: ", x, nnx
  DO i=1,nx
     cx = xroads(:,i)
     IF (cx(1).EQ.1) THEN
        nf = 1
     ELSE
        x = FINDLOC(adj(:,cx(1)).EQ.1 .AND. isx, .TRUE., DIM=1)
        nf = FINDLOC(ntox, x, DIM=1)
        IF (nf.EQ.0) PRINT *, "Did not find ", nf
        cx(3) = cx(3) + 1
     END IF
     nt = FINDLOC(ntox, cx(2), DIM=1)
     xadj(nf,nt) = cx(3)
  END DO
END SUBROUTINE XTOADJ

RECURSIVE SUBROUTINE DFS3(adj, nnx, sn, nstep, cstep, seen)
  INTEGER, INTENT(IN) :: nnx, sn
  INTEGER, INTENT(INOUT) :: cstep
  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: nstep
  INTEGER, DIMENSION(nnx,nnx), TARGET, INTENT(IN) :: adj
  INTEGER, DIMENSION(:), POINTER :: ca
  INTEGER, DIMENSION(:), ALLOCATABLE :: nxt
  INTEGER i, j, cur, cs, ln(nnx)
  LOGICAL, DIMENSION(nnx) :: seen, se

  IF (sn.EQ.nnx) THEN
     nstep = [nstep, cstep]
     ! PRINT *, maxval(nstep)
     RETURN
  END IF
  ln = [(i, i=1,nnx)]
  cur = sn
  ca => adj(cur,:)
  nxt = PACK(ln, (ca.GT.0 .AND. .NOT.seen))
  IF (ANY(nxt.EQ.nnx)) nxt = [nnx]
  DO j=1,SIZE(nxt)
     cs = cstep
     cs = cs + adj(cur,nxt(j))
     se = seen
     se(cur) = .TRUE.
     CALL DFS3(adj, nnx, nxt(j), nstep, cs, se)
  END DO
END SUBROUTINE DFS3

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER s, nr, nc, nn, nx, nnx, cstep
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: adj
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: xadj
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: xroads
  INTEGER, DIMENSION(:), ALLOCATABLE :: nstep
  LOGICAL, DIMENSION(:), ALLOCATABLE :: isx
  LOGICAL, DIMENSION(:), ALLOCATABLE :: seen

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READMAT(10, map, nr, nc)
  CLOSE(10)
  ! CALL PRINTMAT(map)

  nn = COUNT(map.EQ.'.' .OR. map.EQ.'v' .OR. map.EQ.'>')
  ALLOCATE(adj(nn,nn))
  ALLOCATE(isx(nn))
  CALL READADJ(adj, nn, map, nr, nc, isx)
  ! adj(i,j) means connection from i to j
  ! CALL PRINTMAT(adj)

  ALLOCATE(xroads(3,0))
  CALL FINDXROADS(adj, nn, xroads, isx, nx)
  ! CALL PRINTMAT(xroads)

  nnx = COUNT(isx) + 2
  ALLOCATE(xadj(nnx,nnx))
  CALL XTOADJ(xroads, nx, adj, isx, nn, xadj, nnx)
  xadj = MERGE(xadj, TRANSPOSE(xadj), xadj.GT.0)
  ! CALL PRINTMAT(xadj)

  ALLOCATE(nstep(0))
  ALLOCATE(seen(nnx))
  seen = .FALSE.
  cstep = 0
  CALL DFS3(xadj, nnx, 1, nstep, cstep, seen)
  ! PRINT *, nstep
  s = MAXVAL(nstep)

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
