MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  INTEGER, PARAMETER :: NLEN=2
  INTEGER, PARAMETER :: NPUSH=1000

  TYPE :: MPTR
     TYPE(MDL), POINTER :: ptr=>NULL()
  END TYPE MPTR

  TYPE :: MDL
     CHARACTER(LEN=NLEN) name
     LOGICAL :: pulse=.FALSE.
     TYPE(MPTR), DIMENSION(:), ALLOCATABLE :: conn
     ! flipflop stuff
     LOGICAL :: isflp=.FALSE.
     LOGICAL :: state=.FALSE.
     ! conjunction stuff
     LOGICAL :: iscnj=.FALSE.
     INTEGER, DIMENSION(:), ALLOCATABLE :: pid
  END TYPE MDL

  CHARACTER(LEN=NLEN), DIMENSION(:), ALLOCATABLE :: mnames
  TYPE(MDL), DIMENSION(:), ALLOCATABLE, TARGET :: mods
  TYPE(MPTR), DIMENSION(:), ALLOCATABLE :: q
CONTAINS

SUBROUTINE READINPUT(lu)
  CHARACTER l*256
  CHARACTER(LEN=NLEN), DIMENSION(:), ALLOCATABLE :: conns
  INTEGER, INTENT(IN) :: lu
  INTEGER ios, i, j, nm, nf, nc, nn, ic

  nm = NLINES(lu)
  ALLOCATE(mnames(nm))
  ALLOCATE(mods(nm))
  nf = 0
  nc = 0
  DO i=1,nm
     READ(lu, "(A)", IOSTAT=ios) l
     READ(l(2:), "(A)") mnames(i)
     IF (l(1:1).EQ.'%') THEN
        mods(i) = MDL(mnames(i), isflp=.TRUE.)
        nf = nf + 1
     ELSE IF (l(1:1).EQ.'&') THEN
        mods(i) = MDL(mnames(i), iscnj=.TRUE.)
        nc = nc + 1
     ELSE
        mods(i) = MDL(mnames(i))
     END IF
  END DO
  REWIND(lu)
  ! build connections
  DO i=1,nm
     READ(lu, "(A)", IOSTAT=ios) l
     j = INDEX(l, '>')
     nn = COUNTSTR(l,',') + 1
     ALLOCATE(conns(nn))
     ALLOCATE(mods(i)%conn(nn))
     READ(l(j+1:), *) conns
     ! PRINT *, mnames(i), " ", mods(i)%isflp, mods(i)%iscnj, ": ", conns
     DO j=1,nn
        ic = FINDLOC(mnames, conns(j), DIM=1)
        IF (ic.EQ.0) CYCLE
        mods(i)%conn(j)%ptr => mods(ic)
        ! reciprocal connection
        IF (mods(ic)%iscnj) THEN
           IF (.NOT.ALLOCATED(mods(ic)%pid)) ALLOCATE(mods(ic)%pid(0))
           mods(ic)%pid = [mods(ic)%pid, i]
        END IF
     END DO
     DEALLOCATE(conns)
  END DO
END SUBROUTINE READINPUT

SUBROUTINE UPDATE(mp, nhi, nlo)
  TYPE(MPTR) :: mp
  TYPE(MDL), POINTER :: md
  INTEGER i, j, nc, ncc, nhi, nlo
  LOGICAL, DIMENSION(:), ALLOCATABLE :: pmem
  TYPE(MPTR), DIMENSION(:), ALLOCATABLE :: toq

  md => mp%ptr
  ! PRINT *, "processing ", md%name
  nc = SIZE(md%conn)
  IF (md%isflp) THEN
     md%state = .NOT.md%state
     md%pulse = md%state
  ELSE IF (md%iscnj) THEN
     pmem = [(mods(md%pid(j))%pulse, j=1,SIZE(md%pid))]
     md%pulse = .NOT.ALL(pmem)
  ELSE
     CONTINUE
  END IF

  ncc = 0
  DO i=1,nc
     IF (.NOT.ASSOCIATED(md%conn(i)%ptr)) CYCLE
     IF (.NOT.md%pulse) THEN
        ncc = ncc + 1
     ELSE
        IF (md%conn(i)%ptr%isflp) CYCLE
        ncc = ncc + 1
     END IF
  END DO
  ALLOCATE(toq(ncc))
  j = 1
  DO i=1,nc
     IF (.NOT.ASSOCIATED(md%conn(i)%ptr)) CYCLE
     IF (md%conn(i)%ptr%isflp .AND. md%pulse) CYCLE
     toq(j)%ptr => md%conn(i)%ptr
     j = j + 1
     ! PRINT *, "sending ", md%pulse, "pulse to ", md%conn(i)%ptr%name
  END DO
  IF (md%pulse) THEN
     ! high pulse
     nhi = nhi + nc
  ELSE
     ! low pulse
     nlo = nlo + nc
  END IF
  IF (SIZE(toq).GT.0) q = [q, toq]
END SUBROUTINE UPDATE

SUBROUTINE POPONE(fro, to)
  TYPE(MPTR), DIMENSION(:), ALLOCATABLE :: fro, tmp
  TYPE(MPTR) to

  IF (SIZE(fro).EQ.0) RETURN
  ALLOCATE(tmp(SIZE(fro)-1))
  to = fro(1)
  IF (SIZE(fro).GT.1) THEN
     tmp(:) = fro(2:)
  END IF
  CALL MOVE_ALLOC(tmp, fro)
END SUBROUTINE POPONE

SUBROUTINE PART1()
  IMPLICIT NONE
  TYPE(MPTR) mp
  INTEGER i, ls, nhi, nlo, nm
  INTEGER(KIND=INT64) s, nhis, nlos

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READINPUT(10)
  CLOSE(10)
  nm = SIZE(mods)
  ! because of how my strings are read "broadcast" becomes "ro"
  ls = FINDLOC(mnames, 'ro', DIM=1)

  nhis = 0
  nlos = 0
  DO i=1,1000
     nhi = 0
     nlo = 1 ! button sends one at the start
     IF (ALLOCATED(q)) DEALLOCATE(q)
     ALLOCATE(q(1))
     q(1)%ptr => mods(ls)
     DO
        IF (SIZE(q).EQ.0) EXIT
        CALL POPONE(q, mp)
        CALL UPDATE(mp, nhi, nlo)
     END DO
     nhis = nhis+nhi
     nlos = nlos+nlo
  END DO
  s = nhis * nlos

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE GETFLPS(flps)
  INTEGER, DIMENSION(:), ALLOCATABLE :: flps
  INTEGER i, j, nf, lh, rh
  LOGICAL, DIMENSION(:), ALLOCATABLE :: msk

  outer: DO
     nf = SIZE(flps)
     msk = [(mods(flps(i))%iscnj, i=1,nf)]
     IF (ALL(.NOT.msk)) RETURN
     DO i=1,nf
        IF (.NOT.msk(i)) CYCLE
        flps = [flps(:i-1), mods(flps(i))%pid, flps(i+1:)]
        CYCLE outer
     END DO
  END DO outer
END SUBROUTINE GETFLPS

SUBROUTINE PART2A()
  IMPLICIT NONE
  CHARACTER l*256
  TYPE(MPTR) mp
  CHARACTER(LEN=NLEN), DIMENSION(:), ALLOCATABLE :: cnjn
  INTEGER, DIMENSION(:), ALLOCATABLE :: cnjs, cyc
  INTEGER i, ls, lrx, nc, nhi, nlo, nm, nf
  INTEGER(KIND=INT64) s
  LOGICAL, DIMENSION(:), ALLOCATABLE :: msk

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READINPUT(10)
  REWIND(10)
  lrx = 0
  DO
     lrx = lrx + 1
     READ(10, '(A)') l
     i = INDEX(l,'rx')
     IF (i.GT.0) EXIT
  END DO
  CLOSE(10)
  nm = SIZE(mods)
  cnjs = mods(lrx)%pid
  ! we now have 4 conjunctions
  nf = SIZE(cnjs)
  cnjs = [(mods(cnjs(i))%pid, i=1,nf)]
  cnjn = [(mods(cnjs(i))%name, i=1,nf)]
  ALLOCATE(msk(nf))
  ALLOCATE(cyc(nf))
  msk = .TRUE.
  cyc = 0
  ! because of how my strings are read "broadcast" becomes "ro"
  ls = FINDLOC(mnames, 'ro', DIM=1)
  nc = 0
  DO
     nhi = 0
     nlo = 1
     IF (ALLOCATED(q)) DEALLOCATE(q)
     ALLOCATE(q(1))
     q(1)%ptr => mods(ls)
     nc = nc + 1
     DO
        IF (SIZE(q).EQ.0) EXIT
        CALL POPONE(q, mp)
        CALL UPDATE(mp, nhi, nlo)
        DO i=1,nf
           IF (.NOT.msk(i)) CYCLE
           IF (mp%ptr%name.EQ.cnjn(i) .AND. .NOT.mp%ptr%pulse) THEN
              cyc(i) = nc
              msk(i) = .FALSE.
           END IF
        END DO
     END DO
     IF (ALL(.NOT.msk)) EXIT
  END DO
  s = PRODUCT(INT(cyc, KIND=INT64))

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2A

! Alternate solution by following flip flop counters
SUBROUTINE PART2B()
  IMPLICIT NONE
  CHARACTER l*256
  TYPE(MPTR) cur, nxt
  TYPE(MPTR), DIMENSION(:), ALLOCATABLE :: flps
  INTEGER i, j, ls, lrx, nc, nm, nf, pow
  INTEGER(KIND=INT64), DIMENSION(:), ALLOCATABLE :: facs
  INTEGER(KIND=INT64) s
  LOGICAL cont

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READINPUT(10)
  REWIND(10)
  lrx = 0
  DO
     lrx = lrx + 1
     READ(10, '(A)') l
     i = INDEX(l,'rx')
     IF (i.GT.0) EXIT
  END DO
  CLOSE(10)
  nm = SIZE(mods)
  ! because of how my strings are read "broadcast" becomes "ro"
  ls = FINDLOC(mnames, 'ro', DIM=1)
  flps = mods(ls)%conn
  nf = SIZE(flps)
  ALLOCATE(facs(nf))
  facs = 0
  nums: DO i=1,nf
     pow = 0
     cur = flps(i)
     DO
        nc = SIZE(cur%ptr%conn)
        cont = .FALSE.
        DO j=1,nc
           IF (cur%ptr%conn(j)%ptr%iscnj) facs(i) = facs(i) + 2**pow
           IF (cur%ptr%conn(j)%ptr%isflp) THEN
              nxt = cur%ptr%conn(j)
              cont = .TRUE.
           END IF
        END DO
        IF (.NOT.cont) CYCLE nums
        cur = nxt
        pow = pow + 1
     END DO
  END DO nums
  s = PRODUCT(facs)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2B

END MODULE MOD

PROGRAM MAIN
  USE MOD

  ! CALL PART1()
  CALL PART2A()
  ! CALL PART2B()
END PROGRAM MAIN
