MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  CHARACTER, DIMENSION(2) :: mir=['/','\']
  CHARACTER, DIMENSION(2) :: spl=['|','-']
  INTEGER, DIMENSION(2,2) :: md1=RESHAPE([1,0,0,1], [2,2])
  INTEGER, DIMENSION(2,2) :: md2=RESHAPE([0,1,1,0], [2,2])
  INTEGER, DIMENSION(2,2) :: sd=RESHAPE([0,1,1,0], [2,2])
  INTEGER, DIMENSION(2,2) :: rot=RESHAPE([0,-1,1,0], [2,2])
CONTAINS

RECURSIVE SUBROUTINE PATH(map, sloc, dir, omap, mem)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map, omap
  CHARACTER ct
  INTEGER i, loc(2), dir(2), sloc(2), tloc(2), tdir(2), sm
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: mem, tem

  loc = sloc
  DO
     loc = loc + dir
     IF (ANY(loc.GT.SHAPE(map).OR.loc.LT.[1,1])) EXIT ! break out if we are outside the map
     omap(loc(1), loc(2)) = '#'
     ct = map(loc(1), loc(2))
     SELECT CASE (ct)
        CASE ('.')
           CYCLE
        CASE ('/','\')
           dir = (ABS(DOT_PRODUCT(dir, PACK(md1(:,FINDLOC(mir,ct)),.TRUE.))) &
                - ABS(DOT_PRODUCT(dir, PACK(md2(:,FINDLOC(mir,ct)),.TRUE.)))) &
                * MATMUL(rot,dir)
        CASE ('|','-')
           IF (DOT_PRODUCT(dir, PACK(sd(:,FINDLOC(spl,ct)),.TRUE.)).NE.0) THEN
              dir = MATMUL(rot,dir)
              tloc = loc
              tdir = - dir

              ! check memory
              sm = SIZE(mem,DIM=2)
              DO i=1,sm
                 IF (ALL(mem(:,i).EQ.loc)) THEN
                    RETURN
                 END IF
              END DO
              ALLOCATE(tem(2,sm+1))
              tem(:,1:sm) = mem
              tem(:,sm+1)=[tloc(1), tloc(2)]
              CALL MOVE_ALLOC(tem, mem)

              ! otherwise recursively call
              CALL PATH(map, tloc, tdir, omap, mem)
           ELSE
              CYCLE
           END IF
     END SELECT
  END DO
END SUBROUTINE PATH

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map, omap
  INTEGER s, nr, nc
  INTEGER dir(2), sloc(2)
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: mem

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READMAT(10, map, nr, nc)
  CLOSE(10)
  ! CALL PRINTMAT(map)

  dir = [0,1] ! start heading to the right
  sloc = [1,0]
  ALLOCATE(mem(2,0))
  omap = map
  CALL PATH(map, sloc, dir, omap, mem)

  ! CALL PRINTMAT(omap)
  s = COUNT(omap.EQ.'#')

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map, omap
  INTEGER s, nr, nc, i, j
  INTEGER dir(2), sloc(2)
  INTEGER, DIMENSION(:), ALLOCATABLE :: ngs
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: mem

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READMAT(10, map, nr, nc)
  CLOSE(10)
  ! CALL PRINTMAT(map)

  ALLOCATE(ngs(0))

  PRINT *, "Searching rows"
  DO i=1,nr
     dir = [0,1]
     sloc = [i,0]
     ALLOCATE(mem(2,0))
     omap = map
     CALL PATH(map, sloc, dir, omap, mem)
     CALL PUSHONE(ngs, COUNT(omap.EQ.'#'))
     DEALLOCATE(mem)

     dir = [0,-1]
     sloc = [i,nc+1]
     ALLOCATE(mem(2,0))
     omap = map
     CALL PATH(map, sloc, dir, omap, mem)
     CALL PUSHONE(ngs, COUNT(omap.EQ.'#'))
     DEALLOCATE(mem)
  END DO

  PRINT *, "Searching columns"
  DO j=1,nc
     dir = [1,0]
     sloc = [0,j]
     ALLOCATE(mem(2,0))
     omap = map
     CALL PATH(map, sloc, dir, omap, mem)
     CALL PUSHONE(ngs, COUNT(omap.EQ.'#'))
     DEALLOCATE(mem)

     dir = [-1,0]
     sloc = [nr+1,j]
     ALLOCATE(mem(2,0))
     omap = map
     CALL PATH(map, sloc, dir, omap, mem)
     CALL PUSHONE(ngs, COUNT(omap.EQ.'#'))
     DEALLOCATE(mem)
  END DO
  ! CALL PRINTMAT(omap)
  s = MAXVAL(ngs)

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
