
solve_dfs(Estado,_,[]) :- final_state(Estado).

solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),               % generar una nueva Movida
      update(Estado,Movida,Estado2),     % calcula nuevo estado usando Movida
      legal(Estado2),                    % nuevo estado debe ser legal
      insert_sort00(Estado2,Estado22),
      not(member(Estado22,Historia)),     % debe ser primera vez que se llega al nuevo estado
      solve_dfs(Estado22,[Estado22|Historia],Movidas).   % continuar a partir de nuevo estado

test_dfs(Problema,Movidas) :-
      initial_state(Problema,Estado),      % obtener un Estado inicial dado Problema
      solve_dfs(Estado,[Estado],Movidas).  % inicia resolución desde Estado

%Caso#1
%initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)],[],28,0,1)).
%final_state(ptm(der,[],[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)],28,28,1)).

%Caso#2
%initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)],[],21,0,2)).
%final_state(ptm(der,[],[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15)],21,21,2)).

%Caso#3
%initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)],[],42,0,3)).
%final_state(ptm(der,[],[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio, 20)],42,42,3)).

%Caso#4
initial_state(ptm,ptm(izq,[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio,20)],[],30,0,4)).
final_state(ptm(der,[],[(alberto, 1),  (beatriz, 2),  (carlos, 5),  (dora, 10),  (emilio, 15), (julio,20)],30,30,4)).


move(ptm(der,_,D,_,_,_),Carga):-select(Carga,D,C2).

move(ptm(izq,I,_,_,_,1),Carga):-member(Carga1,I),member(Carga2,I),Carga1\=Carga2,Carga=[Carga1,Carga2].

move(ptm(izq,I,_,_,_,2),Carga):-select2(Carga,I,C2).

move(ptm(izq,I,_,_,_,3),Carga):-member(Carga1,I),member(Carga2,I),Carga1\=Carga2,Carga=[Carga1,Carga2].

move(ptm(izq,[I1,I2],_,_,_,4),Carga):-Carga=[I1,I2],!.
move(ptm(izq,I,_,_,_,4),Carga):-select2(Carga,I,C2).

update(ptm(B,I,D,TT,TA,M),Carga,ptm(B1,I1,D1,TT,TAA,M)):-
      update_Bote(B,B1),                   
      update_margenes(Carga,B,I,D,I1,D1,TA,TAA,M).   

update_Bote(izq,der).
update_Bote(der,izq).

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA,1):-
      select1(Carga,I,I1),       
      insert1(Carga,D,D1),        
      sumarTiempo1(Carga,TA,TAA).

update_margenes(Carga,der,I,D,I1,D1,TA,TAA,1):-
      select(Carga,D,D1),    
      insert(Carga,I,I1),      
      sumarTiempo(Carga,TA,TAA).

update_margenes(Carga,der,I,D,I1,D1,TA,TAA,2):-
      select(Carga,D,D1),    
      insert22(Carga,I,I1),      
      sumarTiempo(Carga,TA,TAA).

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA,2):-
      select2(Carga,I,I1),       
      insert2(Carga,D,D1),        
      sumarTiempo2(Carga,TA,TAA).

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA,3):-
      select1(Carga,I,I1),       
      insert33(Carga,D,D1),        
      sumarTiempo1(Carga,TA,TAA).

update_margenes(Carga,der,I,D,I1,D1,TA,TAA,3):-
      select(Carga,D,D1),    
      insert3(Carga,I,I1),      
      sumarTiempo(Carga,TA,TAA).

update_margenes(Carga,izq,[I1,I2],D,I11,D1,TA,TAA,4):-
      select1(Carga,[I1,I2],I11),       
      insert44(Carga,D,D1),        
      sumarTiempo1(Carga,TA,TAA),!.

update_margenes(Carga,izq,I,D,I1,D1,TA,TAA,4):-
      select2(Carga,I,I1),       
      insert444(Carga,D,D1),        
      sumarTiempo2(Carga,TA,TAA).

update_margenes(Carga,der,I,D,I1,D1,TA,TAA,4):-
      select(Carga,D,D1),    
      insert4(Carga,I,I1),      
      sumarTiempo(Carga,TA,TAA).

sumarTiempo((U,X),TA,TAA):-TAA is TA+X.
sumarTiempo1([X,(U,Y)],TA,TAA):-TAA is TA+Y.
sumarTiempo2([X,U,(O,Y)],TA,TAA):-TAA is TA+Y.

insert(X,[Y|Ys],[X,Y|Ys]):-precedes11(X,[Y|Ys]).
insert(X,[Y|Ys],[Y|Zs]):-precedes11(X,Y),insert(X,Ys,Zs). 
insert(X,[],[X]).                        

insert1([X,Z],[Y|Ys],[X,Z,Y|Ys]):-precedes1([X,Z],[Y|Ys]).
insert1([X,Z],[Y|Ys],[Y|Zs]):-precedes1([X,Z],[Y|Ys]),insert1([X,Z],Ys,Zs).  
insert1([X,Z],[],[X,Z]).      

insert2([X,Z,U],[Y|Ys],[X,Z,U,Y|Ys]):-precedes2([X,Z,U],[Y|Ys]).
insert2([X,Z,U],[Y|Ys],[Y|Zs]):-precedes2([X,Z,U],[Y|Ys]),insert2([X,Z,U],Ys,Zs).  
insert2([X,Z,U],[],[X,Z,U]).         

insert22(X,[Y|Ys],[X,Y|Ys]):-precedes22(X,[Y|Ys]).
insert22(X,[Y|Ys],[Y|Zs]):-precedes22(X,Y),insert22(X,Ys,Zs). 
insert22(X,[],[X]).     

insert3(X,[Y|Ys],[X,Y|Ys]):-precedes3(X,[Y|Ys]).
insert3(X,[Y|Ys],[Y|Zs]):-precedes3(X,Y),insert3(X,Ys,Zs). 
insert3(X,[],[X]).                        

insert33([X,Z],[Y|Ys],[X,Z,Y|Ys]):-precedes33([X,Z],[Y|Ys]).
insert33([X,Z],[Y|Ys],[Y|Zs]):-precedes33([X,Z],[Y|Ys]),insert33([X,Z],Ys,Zs).  
insert33([X,Z],[],[X,Z]).     

insert4(X,[Y|Ys],[X,Y|Ys]):-precedes4(X,[Y|Ys]).
insert4(X,[Y|Ys],[Y|Zs]):-precedes4(X,Y),insert4(X,Ys,Zs). 
insert4(X,[],[X]).   

insert44([X,Z],[Y|Ys],[X,Z,Y|Ys]):-precedes44([X,Z],[Y|Ys]).
insert44([X,Z],[Y|Ys],[Y|Zs]):-precedes44([X,Z],[Y|Ys]),insert44([X,Z],Ys,Zs).  
insert44([X,Z],[],[X,Z]). 

insert444([X,Z,U],[Y|Ys],[X,Z,U,Y|Ys]):-precedes44([X,Z,U],[Y|Ys]).
insert444([X,Z,U],[Y|Ys],[Y|Zs]):-precedes44([X,Z,U],[Y|Ys]),insert444([X,Z,U],Ys,Zs).  
insert444([X,Z,U],[],[X,Z,U]).     

select(X,[X|Xs],Xs).                         
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).     

select1([X,Z],[X,Z|Xs],Xs).                        
select1([X,Z],[Y|Ys],[Y|Zs]):-select1([X,Z],Ys,Zs).  

select2([X,Z,U],[X,Z,U|Xs],Xs).                        
select2([X,Z,U],[Y|Ys],[Y|Zs]):-select2([X,Z,U],Ys,Zs).  

precedes11((alberto,1),[(carlos,5),(dora,10),(emilio,15)]).
precedes11((beatriz,2),[(alberto,1),(carlos,5)]).
precedes11((alberto,1),[(carlos,5)]).
precedes1([(alberto,1),(beatriz,2)],[]).
precedes1([(dora,10),(emilio,15)],[(beatriz,2)]).
precedes1([(alberto,1),(beatriz,2)],[(dora,10),(emilio,15)]).
precedes1([(alberto,1),(carlos,5)],[(beatriz,2),(dora,10),(emilio,15)]).

precedes22((alberto,1),[(dora,10),(emilio,15)]).
precedes2([(alberto,1),(beatriz,2),(carlos,5)],[]).
precedes2([(alberto,1),(dora,10),(emilio,15)],[(beatriz,2),(carlos,5)]).

precedes3((alberto,1),[(carlos,5),(dora,10),(emilio,15),(julio,20)]).
precedes3((beatriz,2),[(alberto,1),(carlos,5),(dora,10)]).
precedes3((alberto,1),[(carlos,5),(dora,10)]).
precedes3((beatriz,2),[(alberto,1)]).
precedes33([(alberto,1),(beatriz,2)],[]).
precedes33([(emilio,15),(julio,20)],[(beatriz,2)]).
precedes33([(alberto,1),(beatriz,2)],[(emilio,15),(julio,20)]).
precedes33([(carlos,5),(dora,10)],[(beatriz,2),(emilio,15),(julio,20)]).
precedes33([(alberto,1),(beatriz,2)],[(carlos,5),(dora,10),(emilio,15),(julio,20)]).

precedes4((alberto,1),[(dora,10),(emilio,15),(julio,20)]).
precedes4((beatriz,2),[(alberto,1)]).
precedes44([(alberto,1),(beatriz,2),(carlos,5)],[]).
precedes44([(dora,10),(emilio,15),(julio,20)],[(beatriz,2),(carlos,5)]).
precedes44([(alberto,1),(beatriz,2)],[(carlos,5),(dora,10),(emilio,15),(julio,20)]).

legal(ptm(izq,_,D,TT,TA,M)):-not(ilegal(TT,TA)). 
legal(ptm(der,I,_,TT,TA,M)):-not(ilegal(TT,TA)).

ilegal(TT,TA):-TA>TT.

insert_sort00(ptm(izq,I,D,TT,TA,M),L):-insert_sort2(I,I2),L=ptm(izq,I2,D,TT,TA,M).
insert_sort00(ptm(der,I,D,TT,TA,M),L):-insert_sort2(D,D2),L=ptm(der,I,D2,TT,TA,M).

insert_sort2(List,Sorted):-i_sort2(List,[],Sorted).
i_sort2([],Acc,Acc).
i_sort2([(O,X)|T],Acc,Sorted):-insert22((O,X),Acc,NAcc),i_sort2(T,NAcc,Sorted).
   
insert22((O,X),[(P,Y)|T],[(P,Y)|NT]):-X>Y,insert22((O,X),T,NT).
insert22((O,X),[(P,Y)|T],[(O,X),(P,Y)|T]):-X=<Y.
insert22((O,X),[],[(O,X)]).