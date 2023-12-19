MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  CHARACTER(LEN=3), DIMENSION(:), ALLOCATABLE :: fnames

  TYPE :: WFLOW
     CHARACTER(LEN=3) name
     TYPE(RULE), DIMENSION(:), ALLOCATABLE :: rules
  END TYPE WFLOW

  TYPE :: RULE
     INTEGER idx
     CHARACTER op
     INTEGER val
     TYPE(WFLOW), POINTER :: dest => NULL()
  END TYPE RULE

  ! for part 2
  TYPE :: PART
     INTEGER, DIMENSION(4) :: start
     INTEGER, DIMENSION(4) :: end
     LOGICAL :: acc=.FALSE.
     LOGICAL :: cont=.TRUE.
  END TYPE PART
CONTAINS

SUBROUTINE INIT(flo, name)
  TYPE(WFLOW), POINTER :: flo
  CHARACTER(LEN=*) name

  flo%name = name
  ALLOCATE(flo%rules(0))
END SUBROUTINE INIT

SUBROUTINE ADDRULE(flo, i, o, v, d)
  TYPE(WFLOW), POINTER, INTENT(INOUT) :: flo
  TYPE(WFLOW), POINTER, INTENT(IN) :: d
  TYPE(RULE), DIMENSION(:), ALLOCATABLE :: tmp
  TYPE(RULE) rul
  CHARACTER, INTENT(IN) :: o
  INTEGER, INTENT(IN) :: i, v
  INTEGER nr

  rul%idx = i
  rul%op = o
  rul%val = v
  rul%dest => d

  nr = SIZE(flo%rules)
  ALLOCATE(tmp(nr+1))
  tmp(:nr) = flo%rules
  tmp(nr+1) = rul
  CALL MOVE_ALLOC(tmp, flo%rules)
ENDSUBROUTINE ADDRULE

SUBROUTINE PRINTFLOW(flo)
  TYPE(WFLOW), INTENT(IN) :: flo
  TYPE(RULE) rul
  INTEGER nr, i

  nr = SIZE(flo%rules)
  PRINT *, "Workflow ", flo%name
  DO i=1,nr
     rul = flo%rules(i)
     PRINT '(1X,I1,A1,I0,A1,A3)', rul%idx, rul%op, rul%val, ':', rul%dest%name
  END DO
END SUBROUTINE PRINTFLOW

SUBROUTINE READINPUT(lu, flows, parts)
  CHARACTER l*256
  CHARACTER op, dest*3, name*3
  INTEGER lu, ios, lc, nw, np, nl, nc, ncc, nr, i, j
  INTEGER idx, val, iwf
  INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: parts
  TYPE(WFLOW), TARGET, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: flows
  TYPE(WFLOW), POINTER :: flo

  lc = 0
  DO
     READ(lu, '(A)', IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     IF (LEN_TRIM(l).EQ.0) nw = lc
     lc = lc + 1
  END DO
  nl = lc
  np = nl - 1 - nw
  REWIND(lu)

  ! this is where the un-fun begins
  ALLOCATE(flows(nw+2))
  IF (ALLOCATED(fnames)) DEALLOCATE(fnames)
  ALLOCATE(fnames(nw+2))
  fnames(nw+1)='A'
  fnames(nw+2)='R'
  ! read workflows names
  DO i=1,nw
     READ(lu, '(A)', IOSTAT=ios) l
     nc = INDEX(l, '{')
     READ(l(1:nc-1), '(A)') fnames(i)
  END DO
  ! initialise all of them
  DO i=1,nw+2
     flo => flows(i)
     CALL INIT(flo, fnames(i))
  END DO
  REWIND(lu)
  DO i=1,nw
     READ(lu, '(A)', IOSTAT=ios) l
     nr = COUNTSTR(l, ',')
     nc = INDEX(l, '{')
     READ(l(1:nc-1), '(A)') name
     iwf = FINDLOC(fnames, name, DIM=1)
     flo => flows(iwf)
     ! remove braces
     l = l(nc+1:LEN_TRIM(l)-1)
     DO j=1,nr
        nc = INDEX(l, ',')
        ncc = INDEX(l, ':')
        SELECT CASE (l(1:1))
           CASE ('x')
              l(1:1) = '1'
           CASE ('m')
              l(1:1) = '2'
           CASE ('a')
              l(1:1) = '3'
           CASE ('s')
              l(1:1) = '4'
        END SELECT
        READ(l(1:ncc-1), '(I1,A1,I4)') idx, op, val
        READ(l(ncc+1:nc-1), '(A)') dest
        iwf = FINDLOC(fnames, dest, DIM=1)
        CALL ADDRULE(flo, idx, op, val, flows(iwf))
        l = l(nc+1:)
     END DO
     ! final bit
     READ(l, '(A)') dest
     iwf = FINDLOC(fnames, dest, DIM=1)
     CALL ADDRULE(flo, 1, '>', -1, flows(iwf))
  END DO
  ! skip empty line between two blocks
  READ(lu, *)
  ! read parts
  ALLOCATE(parts(4,np))
  DO i=1,np
     READ(lu, '(A)', IOSTAT=ios) l
     ! remove stupid characters
     DO j=1,LEN_TRIM(l)
        IF (.NOT.ISN(l(j:j))) l(j:j)=' '
     END DO
     READ(l,*) parts(:,i)
  END DO
END SUBROUTINE READINPUT

RECURSIVE FUNCTION PROCESS(part, flo) RESULT(res)
  INTEGER, DIMENSION(4) :: part
  INTEGER nr, i
  LOGICAL res
  TYPE(WFLOW) flo
  TYPE(RULE) rul

  IF (flo%name.EQ.'A') THEN
     res = .TRUE.
     RETURN
  ELSE IF(flo%name.EQ.'R') THEN
     res = .FALSE.
     RETURN
  END IF
  res = .FALSE.
  ! PRINT *, "Workflow ", flo%name
  nr = SIZE(flo%rules)
  DO i=1,nr
     rul = flo%rules(i)
     IF (rul%op.EQ.'<') THEN
        IF (part(rul%idx).LT.rul%val) THEN
           res = PROCESS(part, rul%dest)
           RETURN
        END IF
     ELSE IF (rul%op.EQ.'>') THEN
        IF (part(rul%idx).GT.rul%val) THEN
           res = PROCESS(part, rul%dest)
           RETURN
        END IF
     ELSE
        PRINT *, "Unknown operation ", rul%op
     END IF
  END DO
END FUNCTION PROCESS

SUBROUTINE PART1()
  IMPLICIT NONE
  INTEGER s, in, np, i
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: parts
  TYPE(WFLOW), DIMENSION(:), ALLOCATABLE :: flows
  LOGICAL, DIMENSION(:), ALLOCATABLE :: msk

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READINPUT(10, flows, parts)
  CLOSE(10)
  ! CALL PRINTFLOW(flows(1))
  in = FINDLOC(fnames,'in',DIM=1)
  np = SIZE(parts,DIM=2)
  ALLOCATE(msk(np))
  DO i=1,np
     msk(i) = PROCESS(parts(:,i), flows(in))
  END DO

  s = SUM(SUM(parts,DIM=1),MASK=msk)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE SPLITPART(parts, np, idx, at)
  TYPE(PART), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: parts
  TYPE(PART), DIMENSION(:), ALLOCATABLE :: tmp, tsplt
  TYPE(PART) p1,p2
  INTEGER, INTENT(IN) :: idx, at
  INTEGER i, np, nx
  INTEGER, DIMENSION(SIZE(parts)) :: sts, ens
  LOGICAL, DIMENSION(SIZE(parts)) :: msk

  np = SIZE(parts)
  msk = [(parts(i)%start(idx).LT.at .AND. parts(i)%end(idx).GE.at &
       .AND. parts(i)%cont, i=1,np)]
  nx = COUNT(msk)
  ALLOCATE(tmp(np+nx))
  tmp(1:np-nx) = PACK(parts, .NOT.msk)  ! don't split these
  tsplt = PACK(parts, msk)              ! split these
  DO i=1,nx
     p1 = tsplt(i)
     ! IF (p1%acc) PRINT *, "BAD: Tried to split finished part!"
     p2 = p1
     p1%end(idx) = at - 1
     p2%start(idx) = at
     tmp(np-nx+i*2-1:np-nx+i*2) = [p1, p2]
  END DO
  CALL MOVE_ALLOC(tmp, parts)
  np = np + nx
END SUBROUTINE SPLITPART

RECURSIVE SUBROUTINE PROCESS2(parts, flo)
  TYPE(PART), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: parts
  TYPE(PART), DIMENSION(:), ALLOCATABLE :: tmp
  INTEGER np, nr, i, j
  TYPE(WFLOW), INTENT(IN) :: flo
  TYPE(RULE) rul
  LOGICAL, DIMENSION(:), ALLOCATABLE :: msk

  np = SIZE(parts)
  IF (flo%name.EQ.'A') THEN
     DO i=1,np
        parts(i)%acc = .TRUE.
        parts(i)%cont = .FALSE.
     END DO
     RETURN
  ELSE IF(flo%name.EQ.'R') THEN
     DO i=1,np
        parts(i)%cont = .FALSE.
     END DO
     RETURN
  END IF
  nr = SIZE(flo%rules)
  DO i=1,nr
     np = SIZE(parts)
     rul = flo%rules(i)
     IF (rul%op.EQ.'<') THEN
        CALL SPLITPART(parts, np, rul%idx, rul%val)
        ! find ones that need to be sent to detroit
        msk = [(parts(j)%end(rul%idx).LT.rul%val .AND. parts(j)%cont, j=1,np)]
        tmp = PACK(parts, msk)          ! these go to detroit
        parts = PACK(parts, .NOT.msk)   ! these go through
        CALL PROCESS2(tmp, rul%dest)    ! to detroit
        parts = [parts, tmp]            ! reunited
     ELSE IF (rul%op.EQ.'>') THEN
        CALL SPLITPART(parts, np, rul%idx, rul%val+1)
        msk = [(parts(j)%start(rul%idx).GT.rul%val .AND. parts(j)%cont, j=1,np)]
        tmp = PACK(parts, msk)
        parts = PACK(parts, .NOT.msk)
        CALL PROCESS2(tmp, rul%dest)
        parts = [parts, tmp]
     ELSE
        PRINT *, "Unknown operation ", rul%op
     END IF
  END DO
  IF (ANY([(parts(i)%cont, i=1,np)])) PRINT *, "BAD: Has unfinished part at end"
END SUBROUTINE PROCESS2

SUBROUTINE PART2()
  IMPLICIT NONE
  INTEGER(KIND=INT64) s
  INTEGER in, i
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: junk
  TYPE(PART), DIMENSION(:), ALLOCATABLE :: parts
  TYPE(WFLOW), DIMENSION(:), ALLOCATABLE :: flows

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READINPUT(10, flows, junk)
  CLOSE(10)
  in = FINDLOC(fnames,'in',DIM=1)
  ALLOCATE(parts(1))
  parts(1)%start(:) = 1
  parts(1)%end(:) = 4000

  CALL PROCESS2(parts, flows(in))
  DO i=1,SIZE(parts)
     IF (parts(i)%acc) THEN
        s = s + PRODUCT(INT(parts(i)%end - parts(i)%start + 1, KIND=INT64))
     END IF
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
  CALL PART2()
END PROGRAM MAIN
