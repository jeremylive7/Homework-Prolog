

solve_dfs(Estado,_,[]) :- final_state(Estado).

solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),
      update(Estado,Movida,Estado2),
      legal(Estado2),
      not(member(Estado2,Historia)),
      solve_dfs(Estado2,[Estado2|Historia],Movidas).

test_dfs(Problema,Movidas) :-
      initial_state(Problema,Estado),
      solve_dfs(Estado,[Estado],Movidas).

initial_state(part, part(10,16,no,0,30)).
final_state(part(Pos,Obs,_,_,_)):-Pos is Obs + 1. % partícula justo a la derecha
move(part(Pos,_,_,_,Fin),1) :- Pos < Fin.
move(part(Pos,_,_,Ini,_),(-1)) :- Pos > Ini.
move(part(Pos,_,no,_,Fin),5) :- Pos + 5 =< Fin.
move(part(Pos,_,no,Ini,_),(-5)) :- Pos - 5 >=  Ini.
update(part(Pos,Obs,S,I,F),N, part(Pos2,Obs,S2,I,F)) :-
                        Pos2 is Pos + N,update_salto(S,N,S2).
update_salto(S,N,S)   :- (N=1;N=(-1)).
update_salto(no,N,si) :- (N=5;N=(-5)).
legal(part(Pos,Obs,_,I,F)) :- Pos >= I, Pos =< F, Pos \= Obs.

