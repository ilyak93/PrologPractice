
sum([], 0).
sum([N|NS], Total) :- sum(NS, Rest), Total is N + Rest.

drop([], _, []) :- !.
drop([X | Xs], 0, [X| Xs]) :- !.
drop([_ | Xs], N, Res) :- N1 is N-1, drop(Xs,N1,Res).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

/*
range(S, N, []) :- S > N,!, true.
range(S, N, [S | Ss]) :-  S1 is S+1, range(S1, N, Ss).

needSpace(0,0).
needSpace(X,1) :- X =\= 0.

head([], 0).
head([T | _], T).

ifNegativeFix(X, Y) :- X =< 0, Y = 0.
ifNegativeFix(X, Y) :- X > 0, Y = X.

rangeFFix(S,F, NewF) :- F < S, NewF = S.
rangeFFix(S,F, F) :- F >= S.

getRest([],[]) :- !.
getRest([_ | Hs], Hs).

removeDuplicates([],[]).
removeDuplicates([H],[H]).
removeDuplicates([H ,H| T], List) :- removeDuplicates( [H|T], List).
removeDuplicates([H,Y | T], [H|T1]):- Y \= H,removeDuplicates( [Y|T], T1).



nextRange([[]])

combNext([X | Xs], R) :- 
    combNext(Xs, Res), nextRange(X, range), listToDoubleList(range, DbRange),
    multiplyLists([X], DbRange, Mult), my_flatten(Mult, PP), append([PP], Res, R).

    

locateAux([], _, _, _, []) :- !.
locateAux(_, _, _, [], []) :- !.
locateAux([X|Xs], S, N, [R | Rs], CurLinePos) :- 
        sum(Xs,Sum), length(Xs, L), L1 is L - 1, ifNegativeFix(L1,L2), needSpace(L, Sp), 
        S1 is R + X + Sp, F is S1 + (N - (1 + X + Sum + L2 ) ), rangeFFix(S1,F, NewF),
        range(S1, NewF, PossiblePositions), N1 is N - (X + Sp),
        locateAux(Xs, R, N1, PossiblePositions, Res1),
        head(Rs, H), N2 is (N - (H - R)), ifNegativeFix(N2, N3),
        locateAux([X | Xs], S , N3, Rs , Res2),
        append(Res1, Res2, Tmp),
        append([S | [R]], Tmp, CurLinePos), !.


locateAux([], _, _, _, []) :- !.
locateAux(_, _, _, [], []) :- !.
locateAux([X|Xs], S, N, [R | Rs], CurLinePos) :- 
        N1 is N-1,
        print([X | Xs]), print(' '), print(R), print(' '), print(N1), print(' '), print(Rs), print(' '),print(Res1), print('\n'),
        locateAux([X | Xs], R, N1, Rs, Res1),
        sum(Xs,Sum), length(Xs, L), L1 is L - 1, ifNegativeFix(L1,L2), needSpace(L, Sp), 
        S1 is R + X + Sp, F is S1 + (N - (R + X + Sum + L2 ) ), rangeFFix(S1,F, NewF),
        range(S1, NewF, PossiblePositions), N2 is N - (X + Sp),
        print(Xs), print(' '), print(S1), print(' '), print(N2), print(' '), print(PossiblePositions), print(' '),print(Res2), print('\n'),
         head(Rs, H), N2 is (N - (H - R)), ifNegativeFix(N2, N3), 
        locateAux([X | Xs], S , N3, Rs , Res2), 
        locateAux(Xs, S1 , N2, PossiblePositions, Res2),
        append(Res1, Res2, Tmp),
        append([R], Tmp, CurLinePos), print(CurLinePos), print('\n'), !.
      

 
findAllPLsOfStart([], _, _, _, 0, []).
findAllPLsOfStart(_, _, 0, _, 0, []).
findAllPLsOfStart([S | Xs], SeqSize, N, S, Ind, [CurSeq | Res]) :- 
    take(SeqSize, [S | Xs], CurSeq), drop([S | Xs], SeqSize, ConList), N1 is N-1, 
    findAllPLsOfStart(ConList, SeqSize, N1, S, I, Res), Ind is I + SeqSize, !.
findAllPLsOfStart([_ | Xs], SeqSize, N, S, Ind, PLs) :-
    findAllPLsOfStart(Xs, SeqSize, N, S, I, PLs), Ind is I+1, !.
    
  

findAllPLs([], _, _, _, []).
findAllPLs([Sq | Sqs],[Start | Starts], SeqSize, N, PLs) :- 
    findAllPLsOfStart([Sq | Sqs], SeqSize, N, Start, Ind, CurPLs), N1 is N-1,
    drop([Sq | Sqs], Ind, NewSqs),
    findAllPLs(NewSqs , Starts, SeqSize, N1, Res), append(CurPLs, Res, PLs), !.
    


listToDoubleList([X | []], [[X]]) :- !.
listToDoubleList([X | Xs], [[X] | Rest]) :- listToDoubleList(Xs, Rest), !.



    
locate([], _, []) :- !.
locate(_, 0, []) :- !.
locate([X | []], N, PosLine) :- 
    F is (N - X + 1), range(1, F, Res), 
    listToDoubleList(Res, DList), !, member(PosLine, DList).
locate([X | Xs], N, PosLine) :- 
    sum([X | Xs],Sum), length([X | Xs], L), L1 is L - 1, C is Sum+L1, C =<N, 
    F is 1 + (N - (Sum + L1 ) ), range(1, F, PossiblePositions),
    locateAux([X|Xs], 1, N, PossiblePositions, PosLineTmp),
    length([X | Xs], SeqSize),
    length(PossiblePositions, NumOfSeqs), removeDuplicates(PosLineTmp, PosLineTmpNoDup), 
    findAllPLs(PosLineTmpNoDup, PossiblePositions, SeqSize, NumOfSeqs, AllPLs), member(PosLine, AllPLs).
    
    

locateSecond([X | Xs], N, PosLine) :- locate()

    

getAllLines([], _, []).     
getAllLines([X | Xs], N, [CurLine | Res]) :- 
        locateSecond(X, N, CurLine), member(_,CurLine),	getAllLines(Xs, N, Res), !.

      
multiplyLists([[X]], [[X]]).
multiplyLists([[X|Xs]], [[X]|T]) :-
    multiplyLists([Xs], T).
multiplyLists([E|Es], R) :-
    multiplyLists(Es, R1),
    multiplyList(E, R1, R).

multiplyList([], _, []).
multiplyList([E|Es], L, Ls) :-
    multiplyList(Es, L, LL),
    multiplyElement(E, L, LL, Ls).

multiplyElement(_, [], A, A).
multiplyElement(X, [Y|Ys], A, [[X|Y]|T]) :-
    multiplyElement(X, Ys, A, T).
    
locateAll([], [], _).
locateAll(_, [], 0).
locateAll([X | Xs], PosLineAll, N) :- getAllLines([X | Xs], N, AllLines), multiplyLists(AllLines, AllPLsLines), member(PosLineAll, AllPLsLines).


locateAllSecond([], [], _).
locateAllSecond(_, [], 0).
locateAllSecond([X | Xs], PosLineAll, N) :- getAllLines([X | Xs], N, AllLines), multiplyLists(AllLines, PosLineAll), !.
    

comb(Xs,Ys,PXYs) :- comb1(Xs,Ys,PXYs).

comb1([],_,[]).
comb1([X|Xs],Ys,PXYs0) :-
    comb1(Ys,X,PXYs0,PXYs1),
    comb1(Xs,Ys,PXYs1).
comb1([], _, A, A).
comb1([Y|Ys], X, [[X, Y]|D], E) :-
    comb1(Ys, X, D, E).
    
append_pairs([],[]).
append_pairs([[X,Y | _] | Ps], [[X | Y] | L]) :- append_pairs(Ps, L).

    
locate_all_aux([X | []],S, N, [R | Rs], CurLinePos) :- locate_all_aux([X | Xs], R, N, Rs, CurLinePos) 
      
locate_all_aux([], _, _, _, [[]]) :- !.
locate_all_aux(_, _, _, [], [[]]) :- !.
locate_all_aux([X | Xs],S, N, [R | Rs], CurLinePos) :-
        print([X | Xs]), print(' '), print(R), print(' '), print(N), print(' '), print(Res1), print(' '), print('\n'),
        locate_all_aux(Xs, R, N, X, Res1),
        take(1,R, S1),
        print(S1), print('\n'),
        locate_all_aux([X | Xs], S1 , N, Rs , Res2),
        comb(Res1, Res2, C), append_pairs(C, T), comb(R, T, C2), 
        append_pairs(C2, CurLinePos),
        print(CurLinePos), print('\n'),        !.
        
        getAllLines([[2,2],[2,3]],6,[X|Xs]),take(1,X,S), locate_all_aux([X|Xs],S,6,X,CurLine). 
        
try([],[]).
try([L|Ls],[M|Ms]):-
    member(M,L),
    try(Ls,Ms).

all(L,All) :- findall(M, try(L,M), All).

*/

range([N], N, N).
range([Low|Tail], Low, High) :- Low < High, LesLow is Low + 1, range(Tail, LesLow, High).

inRange(Low, Low, High) :- Low =< High.
inRange(I, Low, High) :- Low < High, LessLow is Low + 1, inRange(I, LessLow, High).

findS([], _, StartS, L, StartS, L1) :- L1 is L-1.
findS([S | Ss], PrevS, StartS, L, StartS, L1) :- P1 is PrevS+1, L1 is L-1, S =\= P1, !.
findS([S | Ss], PrevS, StartS, L, Start, Len) :- P1 is PrevS+1, S =:= P1, L1 is L+1, findS(Ss, S, StartS, L1, Start, Len).

drop([], _, []) :- !.
drop([X | Xs], 0, [X| Xs]) :- !.
drop([_ | Xs], N, Res) :- N1 is N-1, drop(Xs,N1,Res).
    
combinate(Line, Size, Combo) :-
 sum(Line, Sum),
 length(Line, Length),
 CortegeLength is Sum + Length - 1,
 combinate_supp(Line, 1, CortegeLength, Size, [], Combo).

combinate_supp([0], _, _, _, _, []).
combinate_supp([Number], Start, _, Size, PreCombo, Combo) :-
 End is Size - Number + 1,
 inRange(First, Start, End),
 Last is First + Number - 1,
 range(EndCombo, First, Last),
 append(PreCombo, EndCombo, Combo).
combinate_supp([Number|Tail], Start, CortegeLength, Size, PreCombo, Combo) :-
 End is Size - CortegeLength + 1,
 inRange(First, Start, End),
 Last is First + Number - 1,
 range(EndCombo, First, Last),
 NewCortegeLength is CortegeLength - Number - 1,
 NewStart is Last + 2,
 append(PreCombo, EndCombo, LowCombo),
 combinate_supp(Tail, NewStart, NewCortegeLength, Size, LowCombo, Combo).
 
 
locateAux([], []).
locateAux([X | Xs], [Start | Res1]) :- 
    X1 is X-1, findS([X | Xs], X1, X, 1, Start, Len), drop([X | Xs], Len, Rest), 
    /* print(Start), print(' '), print(Len), print(' '), print(Rest), print('\n'), */                                
    locateAux(Rest, Res1), !.

locate(Line, Size, Res) :- combinate(Line, Size, Res1), locateAux(Res1, Res).


getAllLines([], _, []).     
getAllLines([X | Xs], N, [CurLine | Res]) :- 
        setof(Res,locate(X, N, Res),CurLine), member(_,CurLine), getAllLines(Xs, N, Res), !.
  
multiplyLists([[X]], [[X]]).
multiplyLists([[X|Xs]], [[X]|T]) :-
    multiplyLists([Xs], T).
multiplyLists([E|Es], R) :-
    multiplyLists(Es, R1),
    multiplyList(E, R1, R).

multiplyList([], _, []).
multiplyList([E|Es], L, Ls) :-
    multiplyList(Es, L, LL),
    multiplyElement(E, L, LL, Ls).

multiplyElement(_, [], A, A).
multiplyElement(X, [Y|Ys], A, [[X|Y]|T]) :-
    multiplyElement(X, Ys, A, T).
    
checkMem([_ | _]).

getAllLines([], [], _).
getAllLines(_, [], 0).
getAllLines([X | Xs], [Res | PosLineAll], N) :- locate(X, N, Res), checkMem(Res), getAllLines(Xs, PosLineAll, N).

locateAll(List, PosAll, N) :- getAllLines(List, PosAll,N). %print(AllLines), print('\n'), multiplyLists(AllLines, PosAll).
    
fill_char(0, _, []) :- !, true.
fill_char(N, C, [C | List]) :- N1 is N-1, fill_char(N1,C,List).

/* append([R], Tmp, CurLinePos),  */

vecLine2BitLineAux([],[],N, LastP, LastC, BitLine) :- 
    SpCnt is N - (LastP+LastC-1),
    fill_char(SpCnt, ' ', Spaces),
    fill_char(LastC, '*', Stars),
    append(Stars, Spaces,BitLine).
    
vecLine2BitLineAux([C | Cs],[P | Ps], N, LastP, LastC, BitLine) :-
    vecLine2BitLineAux(Cs,Ps, N, P, C, Res),
    SpCnt is P-(LastP+LastC),
    fill_char(SpCnt, ' ', Spaces),
    fill_char(LastC, '*', Stars),
    append(Stars, Spaces, Tmp),
    append(Tmp, Res, BitLine).

correct(X, 1) :- X =< 0, !.
correct(X, X) :- X > 0, !.
    
vecLine2BitLine([C | Cs], [P | Ps], N, BitLine) :- 
    sum([C | Cs] ,S), S =< N,
    vecLine2BitLineAux([C | Cs], [P | Ps], N, 1, 0, BitLine).
    
    
    /* vecLine2BitLineAux([1,2,1],[2,5,9],12, 1,0,BitLine). */ 
    
    
vecLineAll2BitLineAll([LenLine | []],[PosLine | []], N , [BitLine]) :- vecLine2BitLine(LenLine,PosLine, N, BitLine), !.

vecLineAll2BitLineAll([LenLine | LLs],[PosLine | PLs], N , [BitLine|Res]) :- 
    vecLineAll2BitLineAll(LLs, PLs, N, Res),
    vecLine2BitLine(LenLine,PosLine, N, BitLine).
     
    /*
    vecLineAll2BitLineAll([[3],[4],[2],[1,1],[1,1]], [[1],[1],[4],[1,5],[1,5]], 5, B
    vecLineAll2BitLineAll([[1,2,1],[1,2,1]],[[2,5,9],[2,5,9]],12,BitLine).
    */
    
    


	
       
mapHead([], []).
mapHead([[X|_]|YS], [X|R]) :-
mapHead(YS, R).

mapTail([], []).
mapTail([[_|XS]|YS], [XS|R]) :-
mapTail(YS, R).

trans([],[]).
trans([[]|XS],R) :- trans(XS,R),!.
trans(Matrix, [H|T1]) :- 
mapHead(Matrix,H),
mapTail(Matrix,T),
trans(T,T1),!.
trans([[X|_]|YS],[X|R]) :- trans(YS,R),!.

%solve([[3],[4],[2],[1,1],[1,1]],[[2,2],[2],[2],[2],[3]],5,Sol).
%solve([[1,1],[2],[1,1],[2]],[[1],[2],[1],[2]],4,Sol).
%solve([[2],[3],[2]],[[2],[3],[2]],3,Sol).
%solve([[2],[2]],[[2],[2]],2,Sol).
%setof(X5,solve([[4],[2,3],[5,2],[2,5],[6],[1,1],[1,1],[2]],[[2],[4],[3,1,2],[1,3,1],[5,1],[2,2,2],[4],[2]],8,X5),O5).
%solve([[4],[2,3],[5,2],[2,5],[6],[1,1],[1,1],[2]],[[2],[4],[3,1,2],[1,3,1],[5,1],[2,2,2],[4],[2]],8,Sol).

solve(_, _, 0, []).
solve(RowLenAll, ColLenAll, N, Sol) :-
locateAll(RowLenAll, RowLocated, N),

checkLegal(RowLenAll, RowLocated, N, ColLenAll , Sol).


/*
solve([[1],[2,1],[2,2,2],[2,1,1],[2,1],[1,2,1],[3,2],[4,2]],[[2,1],[3,2],[4],[2,2],[2,1],[2,2,1],[1,2],[3,2]],8,Sol).
solve([[1],[5],[1,3],[3,2],[1,1],[3],[1],[3]],[[1],[1,1],[3,1],[2,5],[3,1],[1,1],[3],[1]] ,8,Sol).
solve([[5],[7],[1,1,4],[4,2],[3,2],[1,2],[1,2],[2]],[[2],[2,2,1],[6],[2,2],[4],[3,2],[7],[4]],8,X9).
*/



checkLegal(_, [], _, _, _).
checkLegal(RowLenAll, X, N, ColLenAll, X) :-
vecLineAll2BitLineAll(RowLenAll, X, N, BitLine),
trans(BitLine, TransBitLine),
isBitLineLegalAll(TransBitLine, ColLenAll).

	
isBitLineLegal([], []).
isBitLineLegal([], [0]).
isBitLineLegal(BitLine, LenLine) :-
length(BitLine, L),
isBitLineLegal_AUX(BitLine, L, [], RetLen),
sublist(RetLen, LenLine),
sublist(LenLine, RetLen),!.


sublist(Xs, Ys) :-
append(As, Bs, Ys),
append(Xs, Cs, Bs),!.

isBitLineLegal_AUX([], _, Build, Ret) :-
append(Build, [], Ret).
isBitLineLegal_AUX([' '|XS], L, Build, Ret) :-
L1 is L-1,
isBitLineLegal_AUX(XS, L1, Build, Ret).
isBitLineLegal_AUX(['*'|XS], L, Build, Ret) :-
starCount(['*'|XS], L, 0, NumStarFound),
L1 is L-NumStarFound,
drop(['*'|XS], NumStarFound,DroppedBitLine),
append(Build, [NumStarFound], NewBuild),
isBitLineLegal_AUX(DroppedBitLine, L1,NewBuild, Ret).

starCount([], _, Count, NumStarFound) :-
NumStarFound is Count.
starCount(['*'|XS], L, Count, NumStarFound) :-
Count1 is Count+1,
L1 is L-1,
starCount(XS, L1, Count1, NumStarFound).
starCount([' '|_], _, Count, NumStarFound) :-
starCount([] , _,Count, NumStarFound).


isBitLineLegalAll([],[]).
isBitLineLegalAll([],_).
isBitLineLegalAll(_,[]).
isBitLineLegalAll([X|XS],[Y|YS]) :-
isBitLineLegal(X,Y),
isBitLineLegalAll(XS,YS),!.


/*
 vecLineAll2BitLineAll([[3],[4],[2],[1,1],[1,1]], [[1],[1],[4],[1,5],[1,5]], 5, B),
 trans(B,BT),
 isBitLineLegalAll(BT, [[2,2],[2],[2],[2],[3]]).
 
  vecLineAll2BitLineAll([[3],[4],[2],[1,1],[1,1]], [[1],[1],[4],[1,5],[1,5]], 5, B).
   solve([[1,1],[2],[1]],[[2],[1],[1,1]],3,Sol).
*/

headUpToL([], _, []).
headUpToL(_, 0, []).
headUpToL([X|_], 1, [X]).
headUpToL([X|XS], L, [X|R]) :-
L1 is L-1,
headUpToL(XS,L1,R).

cutFirstL([], _, []).
cutFirstL([X|XS], 0, [X|XS]).
cutFirstL([_|XS], 1, XS).
cutFirstL([_|XS], L, XR) :-
L1 is L-1,
cutFirstL(XS, L1, XR).

my_blockify([], _, []).
my_blockify(L, N, [B1|R]) :-
headUpToL(L,N,B1),
cutFirstL(L,N,B2),
my_blockify(B2, N, R),!.

my_flatten([], []).
my_flatten([[]|XS],Flatten) :- my_flatten(XS,Flatten).
my_flatten([[X|XS]|XT], Flatten) :-
my_flatten([X|XS],Flatten1),
my_flatten(XT,Flatten2),
append(Flatten1,Flatten2, Flatten),!.
my_flatten([X|XS],[X|Flatten]) :- flatten(XS,Flatten).