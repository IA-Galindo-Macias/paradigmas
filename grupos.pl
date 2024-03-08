% Union e interseccion

%% elemento(X,[X|_]).
%% elemento(X,[_|R]):-elemento(X,R).

%% union([],L,L).
%% union([X|R],W,L):- elemento(X,W),union(R,W,L),!.
%% union([X|R],W,[X|Q]):-union(R,W,Q).

%% inters([],_,[]).
%% inters([X|R],W,L):- not(elemento(X,W)), inters(R,W,L),!.
%% inters([X|R],W,[X|Q]):- inters(R,W,Q).
%% split_last([X|[]], [], X).
%% split_last([X|R], [X|Q], T) :- split_last(R, Q, T).

%% append([], []).
%% append([[]|T], L) :- append(T, L).
%% append([[L|R]|T], [L|Q]) :- append([R|T], Q).

%% nlista([A|AR], [B|BR], [C|CR], [[A,B,C], [LA,LB,LC], R]) :-
%%     split_last(AR, AH, LA),
%%     split_last(BR, BH, LB),
%%     split_last(CR, CH, LC),
%%     append([AH,BH,CH], H),
%%     sort(H, R).

% ------------------------------------------------------

%% append([], []).
%% append([[]|T], L) :- append(T, L).
%% append([[L|R]|T], [L|Q]) :- append([R|T], Q).

%% split_last([X|[]], [], X).
%% split_last([X|R], [X|Q], T) :- split_last(R, Q, T).

%% nlista([A|AR], [B|BR], [C|CR], W) :-
%%     split_last(AR, AH, LA),
%%     split_last(BR, BH, LB),
%%     split_last(CR, CH, LC),
%%     append([AH,BH,CH], H),
%%     sort(H, R),
%%     W = [[A,B,C], [LA,LB,LC], R].


peligro(paja, ganso).
peligro(ganso, lobo).

%% mov(izquiero, derecho, direccion).


%% Granjero, Trigo, Pato, Lobo 

%% state([G,T,P,L], [_,_,_,_]).
%% state([_,T,_,L], [G,P,_,_]).
%% state([G,T,_,L], [_,P,_,_]).

%% state([_,_,_,L], [G,P,T,_]).
%% state([G,_,P,L], [_,_,T,_]).

%% state([_,T,_,_], [G,P,L,_]).
%% state([G,T,P,_], [_,_,L,_]).

%% state([_,_,P,_], [G,_,L,T]).
%% state([G,_,P,_], [_,_,L,T]).
%% state([_,_,P,_], [G,P,L,T]).



%% mov(state([G,T,P,L], [_,_,_,_]), pasar(G,P), state([_,T,_,L], [G,P,_,_])).
%% mov(state([_,T,_,L], [G,P,_,_]), regresar(G), state([G,T,_,L], [_,P,_,_])).

%% %% flujo 1
%% mov(state([G,T,_,L], [_,P,_,_]), pasar(G,T), state([_,_,_,L], [G,P,T,_])).
%% mov(state([_,_,_,L], [G,P,T,_]), regresar(G), state([G,_,P,L], [_,_,T,_])).
%% mov(state([G,_,P,L], [_,_,T,_]), pasar(G,L), state([_,_,P,_], [G,_,T,L])).

%% %% flujo 2
%% mov(state([G,T,_,L], [_,P,_,_]), pasar(G,L),state([_,T,_,_], [G,P,L,_])).
%% mov(state([_,T,_,_], [G,P,L,_]), regresar(G), state([G,T,P,_], [_,_,L,_])).
%% mov(state([G,T,P,_], [_,_,L,_]), pasar(G,T),  state([_,_,P,_], [G,_,T,L])).


%% mov(state([_,_,P,_], [G,_,T,L]), regresar(G), state([G,_,P,_], [_,_,L,T])).
%% mov(state([G,_,P,_], [_,_,L,T]), pasar(G,P), state([_,_,_,_], [G,P,L,T])).


%% resolve(state([_,_,_,_], [G,P,L,T])).
%% resolve(State1) :- mov(State1, _, State2), resolve(State2).


mov(state([g,t,p,l], []), pasar(g,p), state([t,l], [g,p])) :- write("pasar pato"), nl.
mov(state([t,l], [g,p]), regresar(g), state([g,t,l], [p])) :- write("regresar"), nl.

%% flujo 1
mov(state([g,t,l], [p]), pasar(g,t), state([l], [g,p,t])) :- write("pasar trigo"), nl.
mov(state([l], [g,p,t]), regresar(g,p), state([g,p,l], [t])) :- write("regresar pato"), nl.
mov(state([g,p,l], [t]), pasar(g,l), state([p], [g,t,l])) :- write("pasar lobo"), nl.

%% flujo 2
mov(state([g,t,l], [p]), pasar(g,l),state([t], [g,p,l])) :- write("pasar lobo"), nl.
mov(state([t], [g,p,l]), regresar(g,p), state([g,t,p], [l])) :- write("regresar pato"), nl.
mov(state([g,t,p], [l]), pasar(g,t),  state([p], [g,t,l])) :- write("pasar trigo"), nl.


mov(state([p], [g,t,l]), regresar(g), state([g,p], [l,t])) :- write("regresar"), nl.
mov(state([g,p], [l,t]), pasar(g,p), state([], [g,p,l,t])) :- write("pasar pato"), nl.


resolve(state([], [g,p,l,t])).
resolve(State1) :- mov(State1,_,State2), resolve(State2).
