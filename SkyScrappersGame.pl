
non_correlating_pair([], []).
non_correlating_pair([X|_], [X|_]) :- !, fail.
non_correlating_pair([_|Xs], [_|Ys])  :- non_correlating_pair(Xs, Ys).

non_correlating_lists([], _).
non_correlating_lists([X|_], X) :- !, fail.
non_correlating_lists([X|Xs], Y) :- non_correlating_pair(X, Y), non_correlating_lists(Xs, Y).

range(0, []) :- !, true.
range(N, _) :- N<0, !, fail.
range(N, [N|Ns]) :-  N1 is N-1, range(N1, Ns).

% 1-concrete, 2-concrete/variable
filter_ascending([], []) :- !, true.
filter_ascending([X], [X]) :- !, true.
filter_ascending([X1|[X2|Xs]], Ys) :- X1>=X2, filter_ascending([X1|Xs],Ys), !.
filter_ascending([X1|[X2|Xs]], [X1|Ys]) :- X1<X2, filter_ascending([X2|Xs],Ys).

% 1-concrete, 2-concrete/variable
can_see(Xs, N) :- filter_ascending(Xs, AscSeq), length(AscSeq, N).


accRev([H|T],A,R):-  accRev(T,[H|A],R). 
accRev([],A,A).

reverse([],[]).
reverse([X|Xs], Rev) :- accRev([X|Xs], [], Rev). %reverse(Xs, RevXs), append(RevXs, [X], Rev). 

left_constraint(L, nop, P) :- permutation(L,P).
left_constraint(L, N, P) :- not(N=nop) ,permutation(L,P), can_see(P,N).

height_constraints(Range, L, R, P) :- left_constraint(Range, L, P), reverse(P, RP), left_constraint(Range, R, RP).

no_same_heights([]) :- !, true.
no_same_heights([X|Xs]) :- member(X,Xs), !, fail.
no_same_heights([X|Xs]) :- not(member(X,Xs)), no_same_heights(Xs).

no_same_heights_2d([]) :- !, true.
no_same_heights_2d([[]]) :- !, true.
no_same_heights_2d([R|Rs]) :- no_same_heights(R), no_same_heights_2d(Rs).
 
all_heads([],[]).
all_heads([[]],[[]]).
all_heads([[Head|_]|Rows], [Head|Heads]) :- all_heads(Rows, Heads).

all_tails([],[]).
all_tails([[]],[[]]).
all_tails([[_|Tail]|Rows], [Tail|Tails]) :- all_tails(Rows, Tails).

transpose([],[]).
transpose([[]],[[]]).
transpose([[X]],[[X]]).
transpose([[]|_], []).
transpose([R|Rs], [C|Cs]) :- all_heads([R|Rs], C), all_tails([R|Rs], Ts), transpose(Ts, Cs), !.

skyscrapers_rows(_,[],_,[]).
skyscrapers_rows(Range,[L-R|LRs],_,[P|Ps]) :- height_constraints(Range, L, R, P), skyscrapers_rows(Range, LRs, [], Ps),
		transpose([P|Ps], Tr), no_same_heights_2d(Tr).	 

skyscrapers(HorizontalConstraints, VerticalConstraints, Sol) :- length(HorizontalConstraints, L), range(L, R),
	skyscrapers_rows(R, HorizontalConstraints, [], Sol), transpose(TSol, Sol),
	skyscrapers_rows(R, VerticalConstraints, [], TSol), !, true.
