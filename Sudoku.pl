:- use_module(library(clpfd), []).
:- use_module(library(lists)).

%noRepeats succeeds if the input List contains only
%one occurence of each of its elements
noRepeats([]).
noRepeats([H|T]) :- (H>0 -> \+(member(H, T))), noRepeats(T).

%noRepeatsLL succeeds if noRepeats succeeds on each
%of the input's (a list of lists) elements
noRepeatsLL([]).
noRepeatsLL([H|T]) :-
  (noRepeats(H) -> noRepeatsLL(T);
  fail).

%%consistent succeeds if the rows and columns of the
%%Grid have no repeat elements
%%I had to use clpfd because normal transpose wasn't
%% working correctly. I hope this is okay
consistent([]).
consistent(X) :-
  (clpfd:transpose(X,Y)), noRepeatsLL(X), noRepeatsLL(Y), checkSubs(X).


% chop, sublist, and split are all taken from previous homework assignments
chop(_, [], []).
chop(N, List, [LLH|LLT]):-
  length(List, M),
  M > 0,
  (N > M -> K is M; K is N),
  split(K, List, LLH, B),
  chop(K, B, LLT).

split(0, List, [], List).
split(N, [H|T], [H|Ft], B) :-
  N > 0,
  N0 is N-1,
  split(N0, T, Ft, B).

% sublist(Whole, Part, Before, Length, After) :-
% 	append(BeforePart, PartAndAfterPart, Whole),
% 	length(BeforePart, Before),
% 	append(Part, AfterPart, PartAndAfterPart),
% 	length(Part, Length),
% 	length(AfterPart, After).

%Square is a subgrid of Grid of a given Size with its top left corner
%at Row and Col
extract(Grid, Row, Col, Size, Square) :-
  sublist(Grid, Rs, Row, Size, _),
  helper(Rs, Col, Size, Cs),
  flatten2(Cs, Square).

%helper helps extract get the correct elements
% from the Rows lists to create the subgrid
helper([], _, _, []).
helper([R|Rs], Col, Size, [C|Cs]) :-
  sublist(R, C, Col, Size, _), helper(Rs, Col, Size, Cs).

%repSub succeeds if the subgrid at index Serial has no repeats
repSub(Grid, Size, Serial) :-
  serial(Serial, Size, Row, Col), extract(Grid, Row, Col, Size, Square),
  noRepeats(Square).

%serial takes an index for a subgrid and returns its Row and Col position
serial(Serial, Size, Row, Col) :-
  Row is div(Serial, Size)*Size, Col is mod(Serial, Size)*Size.

%checkSubs checks all the subgrids in Grid for consistency
checkSubs(Grid) :-
  length(Grid, L), Size is integer(sqrt(L)), checkSubs(Grid, 0, Size).

checkSubs(Grid, Counter, Size) :-
  length(Grid, L), Counter < L, repSub(Grid, Size, Counter).


%fillRow replaces all the zeroes in a list with a random number between 1
% and the length of the row
fillRow([],_,[]).
fillRow(Row, Size, Out) :-
  random_between(1,Size,X),
  (member(X, Row) -> fillRow(Row, Size, Out);
  fillRow(X, Size, Row, Out)).
fillRow(X, Size, [0|T], [X|T2]):-
  !, fillRow(T, Size, T2).
fillRow(_, Size, [H|T], [H|T2]) :-
  fillRow(T, Size, T2).

%fillRows calls fillRow on each row in a Grid
fillRows([],_,[]).
fillRows([R|Rs], Size, [O|Os]) :-
  fillRow(R, Size, O), fillRows(Rs, Size, Os).

flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

%sudoku0 attempts to solve the sudoku represented by Grid by generate-and-check
sudoku0(Grid, Out) :-
  length(Grid, Size), fillRows(Grid, Size, Filled),
   (consistent(Filled) -> Out = Filled;
   sudoku0(Grid, Out)).
