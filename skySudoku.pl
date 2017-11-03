:- use_module(library(clpfd)).
:- use_module(library(lists)).

%the predicate to solve skysudokus
skysudoku(ss(Size,Clues), SS):-
  L is Size*Size,
  length(SS, L),
  enforceLengthAndDomain(SS, L),
  alldistinct(SS),
  applyClues(SS, Clues),
  applyLabeling(SS).

% visnum(+L, ?K): the number of elements in list L that are visible from the left is K.
% It can be assumed that L is proper (i.e., not open-ended). The elements of L as well as K are
% FD-variables or integers. visnum/2 should not perform labeling nor create any choice points.
visnum([A|T], K) :-
  K1 is K - 1,
  visnum(T, A, K1).
visnum([], _, 0).
visnum([A|T], Max, K) :-
  A #> Max #<=> K0,
  K0 #=> NMax #= A,
  #\ K0 #=> NMax #= Max,
  K1 #= K - K0,
  visnum(T, NMax, K1).

% visnumR(+L, ?K): the number of elements in list L that are visible from the right is K.
visnumRight(List, K) :-
  reverse(List, RList),
  visnum(RList, K).

%visnumRowWest is called when the clue is on the West
visnumRowWest([R|_Rs], 1, K) :-
  !, visnum(R,K).
visnumRowWest([_R|Rs], Row, K) :-
  Row > 1,
  NRow is Row - 1,
  visnumRowWest(Rs, NRow, K).

%visnumRowEast is called when the clue is on the East
visnumRowEast([R|_Rs],1,K):-
  !, visnumRight(R,K).
visnumRowEast([_R|Rs], Row, K):-
  Row > 1,
  NRow is Row - 1,
  visnumRowEast(Rs, NRow, K).

%visnumColNorth is called when the clue is on the North
visnumColNorth(Grid, Col, K) :-
  transpose(Grid, Dirg),
  visnumRowWest(Dirg, Col, K).

%visnumColSouth is called when the clue is on the South
visnumColSouth(Grid, Col, K) :-
  transpose(Grid, Dirg),
  visnumRowEast(Dirg, Col, K).

%alldistinct and alldistinctHelper is just a shortcut to make sure that the grid (Rs) is a valid sudoku
alldistinct(Rs) :-
  compile(Rs,Cs,Ss),
  alldistinctHelper(Rs),
  alldistinctHelper(Cs),
  alldistinctHelper(Ss).

alldistinctHelper([]) :- !.
alldistinctHelper([R|Rs]):-
  all_distinct(R),
  alldistinctHelper(Rs).

%Square is a subgrid of Grid of a given Size with its top left corner
%at Row and Col
extract(Grid, Row, Col, Size, Square) :-
  NRow is Row - 1,
  NCol is Col - 1, % the functionality requires 0-base counting, but the rest of the project uses 1-base counting
  sublist(Grid, Rs, NRow, Size, _),
  helper(Rs, NCol, Size, Cs),
  append(Cs, Square).

%helper helps extract get the correct elements
% from the Rows lists to create the subgrid
helper([], _, _, []).
helper([R|Rs], Col, Size, [C|Cs]) :-
  sublist(R, C, Col, Size, _), helper(Rs, Col, Size, Cs).

%subgrids gets a list of the subgrids in Grid
subgrids(Grid, Counter, Size, [S|Ss]):-
    Counter =< Size*Size,
    !,
    serialToRC(Counter, Size, Row, Col),
    extract(Grid, Row, Col, Size, S),
    Counter1 is Counter + 1,
    subgrids(Grid, Counter1, Size, Ss).
subgrids(_, C, S, []) :- C > S.

%serialToRc converts a subgrids Serial # to its Row and Column locaiton
serialToRC(Serial, Size, Row, Column) :-
  Serial0 is Serial-1,
  Column is mod(Serial0, Size)*Size + 1,
  Row is div(Serial0, Size)*Size + 1.

%compile Columns is the list of columns in Grid, and Subgrids is the list of Subgrids in Grid
compile(Grid, Columns, Subgrids):-
  transpose(Grid, Columns),
  length(Grid, L),
  Size is round(sqrt(L)),
  subgrids(Grid, 1, Size, Subgrids).

%The term g(N,R,C) represents a “given number” clue: number N is known to occupy the field in row R column C.
%clue(?Grid, +g(N,R,C)) puts it there
clue([[N|_]|_], g(N,1,1)):-
  !.
clue([[_C|Cs]|_Rs], g(N, 1, Col)):-
  Col > 1,
  !,
  NCol is Col - 1,
  clue([Cs|_Rs], g(N, 1, NCol)).
clue([_R|Rs], g(N,Row,Col)):-
  Row > 1,
  !,
  NRow is Row - 1,
  clue(Rs, g(N,NRow,Col)).

%The term v(V,Dir,RC) represents a “visible count” clue. Here, Dir specifies the
%direction from which the visible buildings are counted. Dir is one of the atoms
% n, e, s, w (standing for north, east, south, west). RC is a row or column number
% to which the clue applies. V is the count of the buildings visible from the direction
% Dir in row or column RC.
%clue(?Grid, +v(K,Dir,RC)) applys the clue
clue(Grid, v(K, n, RC)):-
  visnumColNorth(Grid, RC, K).

clue(Grid, v(K, s, RC)):-
  visnumColSouth(Grid, RC, K).

clue(Grid, v(K, e, RC)):-
  visnumRowEast(Grid, RC, K).

clue(Grid, v(K, w, RC)):-
  visnumRowWest(Grid, RC, K).

%appliesClues(?Grid, +Clues) applies all of Clues to Grid.
applyClues(_, []):- !.
applyClues(Grid, [C|Cs]):-
  !,
  clue(Grid, C),
  applyClues(Grid, Cs).

%this helper predicate makes sure each Row is the correct length with the correct domain
enforceLengthAndDomain([], _).
enforceLengthAndDomain([H|T], Length) :-
  length(H,Length),
  domain(H,1,Length),
  enforceLengthAndDomain(T, Length).

%this simply applies labeling to all the Rows
applyLabeling([]).
applyLabeling([H|T]):-
  labeling([ffc, bisect], H),
  applyLabeling(T).
