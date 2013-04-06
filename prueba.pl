% Predicado que genera las filas en la sopa de letras

generarFilas(Alfabeto,N,Fila):-
  L = [],
  agregarElem(Alfabeto,L,N,ListaFila),
  Fila = ListaFila.

% Predicado que agrega los elementos a una fila

agregarElem(_,L,N,ListaFila):-
  length(L,Long),
  Long >= N,
  L = ListaFila,
  !.
  
agregarElem(Alfabeto,L,N,ListaFila):-
  randomElem(Alfabeto,Elem),
  write('elemento  '),write(Elem),nl,
  write('lista  '),write(L),nl,
  Fila = [Elem|L],
  write('fila  '),write(Fila),nl,
  agregarElem(Alfabeto,Fila,N,ListaFila).
  
  
  
% Predicado que agrega los elementos a una fila

generarFila(_,L,N,ListaFila):-
  length(L,Long),
  Long >= N,
  L = ListaFila,
  !.
  
generarFila(E,L,N,ListaFila):-
  Fila = [E|L],
  write('fila  '),write(Fila),nl,
  generarFila(Fila,N,ListaFila).
  
 [m,a,p,u,o,r,i,s,n,'3']. 
 