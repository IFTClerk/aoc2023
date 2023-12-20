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
  INTEGER i, ls, lc, nhi, nlo, nm
  INTEGER(KIND=INT64) s, nhis, nlos

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READINPUT(10)
  CLOSE(10)
  nm = SIZE(mods)
  ! because of how my strings are read "broadcast" becomes "ro"
  ls = FINDLOC(mnames, 'ro', DIM=1)

  lc = 0
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
     lc = lc + 1
  END DO
  s = nhis * nlos

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  TYPE(MPTR) mp
  INTEGER i, ls, lc, nhi, nlo, nm
  INTEGER(KIND=INT64) s, nhis, nlos

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READINPUT(10)
  CLOSE(10)
  nm = SIZE(mods)
  ! because of how my strings are read "broadcast" becomes "ro"
  ls = FINDLOC(mnames, 'ro', DIM=1)

  lc = 0
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
     lc = lc + 1
  END DO
  s = nhis * nlos

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
