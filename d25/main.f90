MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  TYPE :: PAIR
     INTEGER :: left
     INTEGER :: right
  END TYPE PAIR

  INTERFACE OPERATOR (.EQ.)
     PROCEDURE PAIR_EQ
  END INTERFACE OPERATOR (.EQ.)
CONTAINS

ELEMENTAL LOGICAL FUNCTION PAIR_EQ(p1, p2) RESULT(eq)
  TYPE(PAIR), INTENT(IN) :: p1, p2

  IF (p1%left.EQ.p2%left) THEN
     eq = p1%right.EQ.p2%right
  ELSE IF (p1%left.EQ.p2%right) THEN
     eq = p1%right.EQ.p2%left
  ELSE
     eq = .FALSE.
  END IF
END FUNCTION PAIR_EQ

PURE LOGICAL FUNCTION PAIR_X(p1, p2) RESULT(eq)
  TYPE(PAIR), INTENT(IN) :: p1, p2

  IF (p1%left.EQ.p2%left) THEN
     eq = .TRUE.
     RETURN
  END IF
  IF (p1%left.EQ.p2%right) THEN
     eq = .TRUE.
     RETURN
  END IF
  IF (p1%right.EQ.p2%left) THEN
     eq = .TRUE.
     RETURN
  END IF
  IF (p1%right.EQ.p2%right) THEN
     eq = .TRUE.
     RETURN
  END IF
  eq = .FALSE.
END FUNCTION PAIR_X

SUBROUTINE READINPUT(lu, itoc, nc, adj)
  CHARACTER(LEN=3), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: itoc
  CHARACTER l*256, comp*3
  INTEGER, INTENT(IN) :: lu
  INTEGER, INTENT(OUT) :: nc
  INTEGER i, j, ios, nl, fro, to, offset
  INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: adj

  ALLOCATE(itoc(0))
  ! find all component names
  nl = 0
  DO
     READ(lu, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     nl = nl + 1

     i = INDEX(l, ":")
     l = l(:i-1) // l(i+1:)

     i=1
     DO
        IF (i.GT.LEN_TRIM(l)) EXIT
        comp = l(i:i+2)
        i = i + 4

        IF (FINDLOC(itoc, comp, DIM=1).GT.0) CYCLE
        CALL PUSHONE(itoc, comp)
     END DO
  END DO
  REWIND(lu)
  nc = SIZE(itoc)
  ALLOCATE(adj(nc,nc))
  DO i=1,nl
     READ(lu, "(A)", IOSTAT=ios) l

     fro = FINDLOC(itoc, l(1:3), DIM=1)
     offset = INDEX(l, ":")
     ! the first nonwhitespace char is 2 after the colon
     j=offset+2
     DO
        IF (j.GT.LEN_TRIM(l)) EXIT
        comp = l(j:j+2)
        j = j + 4

        to = FINDLOC(itoc, comp, DIM=1)
        IF (to.EQ.0) PRINT *, "Did not find ", comp
        adj(fro,to) = 1
        adj(to,fro) = 1
     END DO
  END DO
END SUBROUTINE READINPUT

FUNCTION FINDLOWS(arr, n) RESULT(res)
  INTEGER, DIMENSION(:), INTENT(IN) :: arr
  INTEGER, INTENT(IN) :: n
  INTEGER, DIMENSION(n) :: res
  INTEGER sa, m, i
  LOGICAL, DIMENSION(SIZE(arr)) :: msk

  res = 0
  sa = SIZE(arr)
  msk = .TRUE.
  DO i=1,n
     m = MINLOC(arr, DIM=1, MASK=msk)
     msk(m) = .FALSE.
     res(i) = m
  END DO
END FUNCTION FINDLOWS

SUBROUTINE FINDPAIRS(adj, nc, pairs)
  INTEGER, DIMENSION(nc,nc), TARGET, INTENT(IN) :: adj
  INTEGER, INTENT(IN) :: nc
  INTEGER, DIMENSION(:), POINTER :: ca, cb
  INTEGER i, j
  TYPE(PAIR), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: pairs
  TYPE(PAIR) p

  ALLOCATE(pairs(0))
  DO i=1,nc
     ca => adj(:,i)
     DO j=1,nc
        IF (ca(j).EQ.0) CYCLE
        cb => adj(:,j)
        ! no common neighbours, potential bridge
        IF (.NOT.ANY(ca.EQ.1 .AND. cb.EQ.1)) THEN
           p%left = i
           p%right = j
           IF (ANY(p.EQ.pairs)) CYCLE
           pairs = [pairs, p]
        END IF
     END DO
  END DO
END SUBROUTINE FINDPAIRS

SUBROUTINE GROUPSIZE(adj, nc, pairs, np, gs)
  INTEGER, INTENT(IN) :: nc, np
  INTEGER, DIMENSION(nc,nc), TARGET, INTENT(IN) :: adj
  INTEGER, DIMENSION(nc,nc) :: adj2
  INTEGER, INTENT(OUT) :: gs
  INTEGER i,j,k
  TYPE(PAIR), DIMENSION(:), ALLOCATABLE :: pairs
  TYPE(PAIR) p1,p2,p3

  DO i=1,np-2
     p1 = pairs(i)
     DO j=i+1,np-1
        p2 = pairs(j)
        IF (PAIR_X(p1,p2)) CYCLE
        DO k=j+1,np
           p3 = pairs(k)
           IF (PAIR_X(p1,p3).OR.PAIR_X(p2,p3)) CYCLE
           ! disconnecting
           adj2 = adj
           adj2(p1%left,p1%right) = 0
           adj2(p1%right,p1%left) = 0
           adj2(p2%left,p2%right) = 0
           adj2(p2%right,p2%left) = 0
           adj2(p3%left,p3%right) = 0
           adj2(p3%right,p3%left) = 0
           CALL BFSBLOC(adj2, nc, gs)
           IF (gs.NE.nc) RETURN
        END DO
     END DO
  END DO
END SUBROUTINE GROUPSIZE

SUBROUTINE BFSBLOC(adj, nc, s)
  INTEGER, INTENT(IN) :: nc
  INTEGER, DIMENSION(nc,nc), TARGET, INTENT(IN) :: adj
  INTEGER, INTENT(OUT) :: s
  INTEGER, DIMENSION(:), POINTER :: ca
  INTEGER j, cur, nxt
  INTEGER, DIMENSION(:), ALLOCATABLE :: q
  LOGICAL, DIMENSION(nc) :: seen

  ALLOCATE(q(1))
  seen = .FALSE.
  q = 1
  seen(1) = .TRUE.
  s = 0
  DO
     IF (SIZE(q).EQ.0) EXIT
     cur = q(1)
     q = q(2:)
     ca => adj(:,cur)
     s = s + 1
     DO j=1,nc
        IF (ca(j).EQ.0 .OR. seen(j)) CYCLE
        nxt = j
        q = [q, nxt]
        seen(j) = .TRUE.
     END DO
  END DO
END SUBROUTINE BFSBLOC

SUBROUTINE BFSDIST(adj, nc, fro, to, d)
  INTEGER, INTENT(IN) :: nc, fro, to
  INTEGER, DIMENSION(nc,nc), TARGET, INTENT(IN) :: adj
  INTEGER, INTENT(OUT) :: d
  INTEGER, DIMENSION(:), POINTER :: ca
  INTEGER i, j, cur, nxt
  INTEGER, DIMENSION(:), ALLOCATABLE :: q
  LOGICAL, DIMENSION(nc) :: seen

  ALLOCATE(q(1))
  seen = .FALSE.
  q = fro
  seen(fro) = .TRUE.
  d = 0
  DO
     IF (SIZE(q).EQ.0) EXIT
     cur = q(1)
     IF (cur.EQ.to) RETURN
     q = q(2:)
     ca => adj(:,cur)
     d = d + 1
     DO j=1,nc
        IF (ca(j).EQ.0 .OR. seen(j)) CYCLE
        nxt = j
        q = [q, nxt]
        seen(j) = .TRUE.
     END DO
  END DO
END SUBROUTINE BFSDIST

SUBROUTINE FINDDIFF(adj, nc, fro, to, dd)
  INTEGER, INTENT(IN) :: nc, fro, to
  INTEGER, DIMENSION(nc,nc), INTENT(IN) :: adj
  INTEGER, INTENT(OUT) :: dd
  INTEGER, DIMENSION(nc,nc) :: adj2
  INTEGER d1, d2

  CALL BFSDIST(adj, nc, fro, to, d1)
  adj2 = adj
  adj2(fro,to) = 0
  adj2(to,fro) = 0
  CALL BFSDIST(adj2, nc, fro, to, d2)
  dd = d2 - d1
END SUBROUTINE FINDDIFF

SUBROUTINE SORT(ps, ds)
  TYPE(PAIR), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: ps
  TYPE(PAIR), DIMENSION(:), ALLOCATABLE :: tps
  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: ds
  INTEGER, DIMENSION(:), ALLOCATABLE :: tds
  INTEGER i, mai
  LOGICAL, DIMENSION(SIZE(ps)) :: msk

  msk = .TRUE.
  ALLOCATE(tps(SIZE(ps)))
  ALLOCATE(tds(SIZE(ps)))
  DO i=1,SIZE(ps)
     mai = MAXLOC(ds, MASK=msk, DIM=1)
     tps(i) = ps(mai)
     tds(i) = ds(mai)
     msk(mai) = .FALSE.
  END DO
  CALL MOVE_ALLOC(tps, ps)
  CALL MOVE_ALLOC(tds, ds)
END SUBROUTINE SORT

SUBROUTINE PART1
  IMPLICIT NONE
  CHARACTER(LEN=3), DIMENSION(:), ALLOCATABLE :: itoc
  INTEGER s, nc, ports(6), i, j, k, np
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: adj
  INTEGER, DIMENSION(:), ALLOCATABLE :: dp
  ! INTEGER, DIMENSION(:), ALLOCATABLE :: pairs
  TYPE(PAIR), DIMENSION(:), ALLOCATABLE :: pairs

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READINPUT(10, itoc, nc, adj)
  CLOSE(10)
  ! WRITE(6,'(*(A,1X))') itoc
  ! CALL PRINTMAT(adj)

  CALL FINDPAIRS(adj, nc, pairs)
  np = SIZE(pairs)
  ! PRINT *, pairs%left
  ! PRINT *, pairs%right

  ALLOCATE(dp(np))
  DO i=1,np
     CALL FINDDIFF(adj, nc, pairs(i)%left, pairs(i)%right, dp(i))
  END DO
  ! PRINT *, dp

  CALL SORT(pairs, dp)
  ! PRINT *, pairs%left
  ! PRINT *, pairs%right
  ! PRINT *, dp

  CALL GROUPSIZE(adj, nc, pairs, np, s)
  s = s * (nc-s)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
END PROGRAM MAIN
