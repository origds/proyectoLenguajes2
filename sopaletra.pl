%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Proyecto 2. Prolog - Sopa de Letras
%% Realizado por:
%% Oriana Gomez 09-10336
%% Carla Urrea 09-11215
%% Fecha: 22/03/13
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CARGAR LISTA PALABRA

% Predicado para cargar las listas de palabras

cargarListaPalabra(Archivo, Alfabeto,Palabras):-
  see(Archivo),
  read(Palabras),
  seen,
  transformaEnAscii(Palabras,Alfabeto),
  !.

% Predicado para llevar las listas y el alfabeto a Ascii

transformaEnAscii([],_).
transformaEnAscii([C|R], Alf):-
  atom_codes(T,Alf),
  atom_codes(T,AlfabetoAscii),
  atom_chars(C,Temp),
  atom_codes(X,Temp),
  atom_codes(X,CabezaAscii),
  comparar(CabezaAscii,AlfabetoAscii),
  transformaEnAscii(R,Alf),
  !.

% Predicado para comparar que las palabras estan formadas por
% el alfabeto

comparar([],_).
comparar([C|R],Alf):-
  member(C,Alf),
  comparar(R,Alf),
  !.


% Predicado que arma las sopas de letras

armarSopa(Tam,Sopa) :-
  length(Fila, Tam), list_to_set(Fila, ['$']),
  length(Sopa, Tam), list_to_set(Sopa, [Fila]).
  
% Predicado que reemplaza el elemento en una posicion de la fila
% por otro

reemplazar([_ | Resto], 0, Elem, [Elem | Resto]).
reemplazar([C | R], Pos, Elem, [C | F]) :-
  Pos > 0,
  Pos1 is Pos - 1,
  reemplazar(R, Pos1, Elem, F),
  !.

% Predicado que reemplaza todos los elementos requeridos e una fila dada

reemplazarEnFila([], FilaTmp, _, NuevaFila):-
  FilaTmp = NuevaFila.
reemplazarEnFila([PrimeraLetra|RestoLetras], Fila, Pos, NuevaFila) :-
  Pos >= 0,
  length(Fila, N), 
  Pos < N,
  verificarRepeticion(PrimeraLetra,Fila,Pos),
  reemplazar(Fila, Pos, PrimeraLetra, FilaTmp),                             
  Pos1 is Pos + 1,
  reemplazarEnFila(RestoLetras, FilaTmp, Pos1, NuevaFila),
  !.

%Predicado enDiagonal. 
%Se encarga de meter una palabra en la sopa de letras
%En forma diagonal y hacia abajo

enDiagonal(_, _, [], SopaFinal, SopaFinal).
  
enDiagonal(PosFila, PosCol, [Letra | Resto], SopaLetras, R) :-
  nth0(PosFila, SopaLetras, Fila),
  verificarRepeticion(Letra,Fila, PosCol),                            %Quiero la fila de la posicion PosFil
  reemplazar(Fila, PosCol, Letra, FilaTmp),
  reemplazar(SopaLetras, PosFila, FilaTmp, SopaLetrasAct),
  Fil1 is PosFila + 1,                                   
  Col1 is PosCol + 1,                                       
  enDiagonal(Fil1, Col1, Resto, SopaLetrasAct, R),
  !.

%Predicado que anade las palabras en forma diagonal "invertida"
%NOTA: Revisar el False que da enDiagonalInv y no AddDiagInv
enDiagonalInv(_, _, [], SopaFinal, SopaFinal).
enDiagonalInv(PosFila, PosCol, [Letras | Resto], SopaLetras, R) :-  
  nth0(PosFila, SopaLetras, Fila),                            %Quiero la fila de la posicion PosFil
  verificarRepeticion(Letras,Fila, PosCol),
  reemplazar(Fila, PosCol, Letras, FilaTmp),
  reemplazar(SopaLetras, PosFila, FilaTmp, SopaLetrasAct),
  Fil1 is PosFila + 1,                                   
  Col1 is PosCol - 1,                   
  enDiagonalInv(Fil1, Col1, Resto, SopaLetrasAct, R),
  !.

% Predicado que añade las palabras en forma diagonal

addDiagonal(Palabra, SopaLetras, Result) :- 
 atom_chars(Palabra, Letras),
 length(Letras, Long), 
 length(SopaLetras, Tam), 
 Long =< Tam,
 between(1, Tam, F), 
 PosFila is F - 1,
 between(1, Tam, C), 
 PosCol is C - 1,
 enDiagonal(PosFila, PosCol, Letras, SopaLetras, Result). 

% Predicado que añade las paabras en diagonal al reves

addDiagonalReverse(Palabra, SopaLetras, Result) :- 
 atom_chars(Palabra, Letras),
 reverse(Letras, Reverse),
 length(Letras, Long), 
 length(SopaLetras, Tam), 
 Long =< Tam,
 between(1, Tam, F), 
 PosFila is F - 1,
 between(1, Tam, C), 
 PosCol is C - 1,
 enDiagonal(PosFila, PosCol, Reverse, SopaLetras, Result).

% Predicado que añade las palabras en diagonal invertido

addDiagonalInv(Palabra, SopaLetras, Result) :- 
 atom_chars(Palabra, Letras),
 length(Letras, Long), 
 length(SopaLetras, Tam), 
 Long =< Tam,
 between(1, Tam, F), 
 PosFila is F - 1,
 between(1, Tam, C), 
 PosCol is C - 1,
 enDiagonalInv(PosFila, PosCol, Letras, SopaLetras, Result).
 
% Predicado que añade las palabras en diagonal invertido al reves 

addDiagonalInvReverse(Palabra, SopaLetras, Result) :- 
 atom_chars(Palabra, Letras),
 reverse(Letras,Reverse),
 length(Letras, Long), 
 length(SopaLetras, Tam), 
 Long =< Tam,
 between(1, Tam, F), 
 PosFila is F - 1,
 between(1, Tam, C), 
 PosCol is C - 1,
 enDiagonalInv(PosFila, PosCol, Reverse, SopaLetras, Result).

% Predicado que añade las palabras en forma horizontal 

addHorizontal(Palabra, SopaLetras, Result) :-
  length(SopaLetras, Tam),
  between(1,Tam,PosFila),
  X is PosFila-1,
  between(1,Tam,PosCol),
  Y is PosCol-1,
  nth0(X, SopaLetras, Fila),                         %Obtengo la Sublista (Fila) en la que agregare la palabra
  atom_chars(Palabra, Letras),               		   %Guardo la palabra como una lista de atomos en Tmp
  length(Letras, Long),                              %Obtengo la longitud de la palabra
  Long =< Tam,                                       %Verifico que la palabra quepa en el tablero
  reemplazarEnFila(Letras, Fila, Y, FilaNueva),
  reemplazar(SopaLetras,X,FilaNueva,SopaLetrasAct),
  Result = SopaLetrasAct.
  
addHorizontalReverse(Palabra, SopaLetras, Result) :-
  length(SopaLetras, Tam),
  between(1,Tam,PosFila),
  X is PosFila-1,
  between(1,Tam,PosCol),
  Y is PosCol-1,
  nth0(X, SopaLetras, Fila),                       %Obtengo la Sublista (Fila) en la que agregare la palabra
  atom_chars(Palabra, Letras),               %Guardo la palabra como una lista de atomos en Tmp
  reverse(Letras,Reverse),
  length(Reverse, Long),                              %Obtengo la longitud de la palabra
  Long =< Tam,                                       %Verifico que la palabra quepa en el tablero
  reemplazarEnFila(Reverse, Fila, Y, FilaNueva),
  reemplazar(SopaLetras,X,FilaNueva,SopaLetrasAct),
  Result = SopaLetrasAct.

% Predicado para agregar palabras en forma vertical.

addVertical(Palabra, SopaLetras, Result):-
  trasponer(SopaLetras,Sopa),
  addHorizontal(Palabra,Sopa,Aux),
  trasponer(Aux,Result).
  
addVerticalReverse(Palabra, SopaLetras, Result):-
  trasponer(SopaLetras,Sopa),
  addHorizontalReverse(Palabra,Sopa,Aux),
  trasponer(Aux,Result).
  
% Predicado para verificar que no se inserten 2 palabras en el mismo sitio

verificarRepeticion(Letra,Fila,PosCol) :-
  atom_codes(Letra,X),
  nth0(PosCol,Fila,Laletra),
  atom_codes(Laletra,Y),
  X =:= Y,
  !.

verificarRepeticion(_,Fila,PosCol) :-
  nth0(PosCol,Fila,Letra),
  atom_codes(Letra,Y),
  Y =:= 36.
  
% Predicado que rellena con elementos random las posiciones vacias de la matriz.

rellenarEspacios(Alfabeto,Sopa,SopaLlena):-
  estaVacia(Sopa,F,C),
  !,
  randomElem(Alfabeto, E),
  cambiaSopa(Sopa,F,C,E,SopaCambiada),
  rellenarEspacios(Alfabeto,SopaCambiada,SopaLlena).
rellenarEspacios(_,SopaLlena,SopaLlena).
  
cambiaSopa(Sopa, F, C, E, SopaCambiada) :-
  nth0(F,Sopa,Fila),
  reemplazar(Fila,C,E,NuevaFila),
  reemplazar(Sopa,F,NuevaFila,SopaCambiada).
  
estaVacia(Sopa, F, C):-
  indiceValido(Sopa,F),
  indiceValido(Sopa,C),
  nth0(F, Sopa, Fila),
  nth0(C, Fila, '$').
  
indiceValido(Sopa, Indice) :- 
  length(Sopa, N),
  Tam is N-1,
  between(0, Tam, Indice).
  
% Predicado que saca la traspuesta de una matriz

trasponer([[]|_], []) :- !.
trasponer([[I|Is]|Rs], [Col|MT]) :-
  primeraCol([[I|Is]|Rs], Col, [Is|NRs]),
  trasponer([Is|NRs], MT).

primeraCol([], [], []).
primeraCol([[]|_], [], []).
primeraCol([[I|Is]|Rs], [I|Col], [Is|Rest]) :-
  primeraCol(Rs, Col, Rest).
 
%% Predicado que genera un elemento random del alfabeto

randomElem(Lista, Elem) :-
  length(Lista, Long),
  random(0, Long, Pos),
  nth0(Pos, Lista, Elem).
  
%% SOPA LETRA

% Predicado que devuelve todas las posibles sopas de letras
%Nota: La posicion en la que estaran las palabras en la sopa
%de letras vendran dadas por: Horizontal = 0, HorizontalReverse = 1,
%Vertical = 2, VerticalReverse = 3, Diagonal = 4, DiagonalReverse = 5,
%DiagonalInv = 6, DiagonalInvReverse = 7.

sopaLetra(Alfabeto,Tam,Aceptadas,Rechazadas):-
  armarSopa(Tam,Sopa),
  crearSopaDeLetras(Sopa,Alfabeto,Tam,Aceptadas,SopaLetras),
  rellenarEspacios(Alfabeto,SopaLetras,SopaFinal),
  not(esRechazada(SopaFinal,Rechazadas)),
  nl,
  mostrarSopa(SopaFinal).

% Predicado que triunfa cuando una de las palabras esta rechazada en la sopa.

esRechazada(_,[]).

esRechazada(Sopa,[Rechazada|Resto]):-
  atom_chars(Rechazada,Letras),
  rechazar(Letras,Sopa),
  esRechazada(Sopa,Resto),
  !.
  
esRechazada(Sopa,[Rechazada|Resto]):-
  atom_chars(Rechazada,Letras),
  reverse(Letras,Reverse),
  rechazar(Reverse,Sopa),
  esRechazada(Sopa,Resto),
  !.

rechazar(Palabra,Sopa):-
  rechazadaHoriz(Palabra,Sopa).

rechazar(Palabra,Sopa):-
  rechazadaVertical(Palabra,Sopa).
  
% Predicado que crea la sopa de letras con las palabras aceptadas
  
crearSopaDeLetras(SopaFinal,_,_,[], SopaFinal).
crearSopaDeLetras(Sopa,Alfabeto,Tam, [Palabra | Resto] , SopaFinal):-
  Predicados = ['addHorizontal', 'addHorizontalReverse', 'addVertical', 
              'addVerticalReverse', 'addDiagonal', 'addDiagonalReverse',
              'addDiagonalInv', 'addDiagonalInvReverse'],
  member(Llamada,Predicados),
  Lambda=.. [Llamada,Palabra,Sopa,Result],
  call(Lambda),
  crearSopaDeLetras(Result,Alfabeto,Tam,Resto,SopaFinal).

% Predicado que verifica las palabras rechazadas en la sopa horizontalmente

rechazadaHoriz([],_).
rechazadaHoriz(Letras, Sopa) :- 
  length(Sopa, Tam),
  between(1, Tam, PosFila),
  X is PosFila -1,
  between(1,Tam,PosCol),
  Y is PosCol-1, 
  indiceValido(Sopa, X),
  indiceValido(Sopa, Y),
  nth0(X, Sopa, Fila),
  comparar(Y,Letras,Fila),
  !,
  rechazadaHoriz(_, Sopa),
  !.

rechazadaVertical([],_).
rechazadaVertical(Letras, Sopa) :-
  trasponer(Sopa, SopaNueva),
  rechazadaHoriz(Letras, SopaNueva).

rechazadaDiagonal([],_).
rechazadaDiagonal(Letras,Sopa) :-
  length(Sopa, Tam),
  between(1, Tam, PosFila),
  X is PosFila -1,
  between(1,Tam,PosCol),
  Y is PosCol-1, 
  indiceValido(Sopa, X),
  indiceValido(Sopa, Y),
  comparaDiagonal(X,Y,Letras,Sopa),
  !,
  rechazadaDiagonal(_, Sopa),
  !.

rechazadaDiagSec([],_).
rechazadaDiagSec(Letras,Sopa) :-
  length(Sopa, Tam),
  between(1, Tam, PosFila),
  X is PosFila -1,
  between(1,Tam,PosCol),
  Y is PosCol-1, 
  indiceValido(Sopa, X),
  indiceValido(Sopa, Y),
  comparaDiagonalSec(X,Y,Letras,Sopa),
  !,
  rechazadaDiagSec(_, Sopa),
  !.


%Predicado comparaDiagonal. Triunfa si la palabra esta en la
%diagonal de la posicion que se le indique. 
comparaDiagonalSec(_,_,[], _).
comparaDiagonalSec(PosFila, PosCol, [Letra | Resto], Sopa) :-
  nth0(PosFila, Sopa, Fila), %obtengo la fila que quiero revisar
  nth0(PosCol, Fila, Letra1),  %Obtengo el elemento de la fila que me interesa.
  Letra = Letra1,
  Fil is PosFila + 1,
  Col is PosCol - 1,
  comparaDiagonalSec(Fil, Col, Resto, Sopa).

%Predicado comparaDiagonal. Triunfa si la palabra esta en la
%diagonal de la posicion que se le indique. 
comparaDiagonal(_,_,[], _).
comparaDiagonal(PosFila, PosCol, [Letra | Resto], Sopa) :-
  nth0(PosFila, Sopa, Fila), %obtengo la fila que quiero revisar
  nth0(PosCol, Fila, Letra1),  %Obtengo el elemento de la fila que me interesa.
  Letra = Letra1,
  Fil is PosFila + 1,
  Col is PosCol + 1,
  comparaDiagonal(Fil, Col, Resto, Sopa).


%Predicado que compara los elementos de una lista con los de otra, triunfa 
%cuando los elementos de la primera lista se encuentran todos en la segunda
%en el mismo orden.
comparar(_, [], _).
comparar(PosCol, [Letra | Resto], Fila) :-
  nth0(PosCol, Fila, Letra1),
  Letra = Letra1,
  Pos is PosCol+1,
  comparar(Pos, Resto, Fila). 

%% MOSTRAR SOPA.
  
% Predicado que muestra las sopas de letra en pantalla

mostrarSopa([C|R]):-
  separar(C),
  mostrarSopa(R),
  !.
mostrarSopa([]).

% Predicado que imprime las lineas de la sopa de letras

separar([C|R]):-
  write(C),
  write(' '),
  separar(R),
  !.
separar([]):- 
  nl.
separar(A):-
  write(A),
  nl.

% Predicado que agrega el quiero mas .

quieroMas :-
  nl, write('Quieres mas?'), nl,
  read(Mas),
  Mas \= mas. 

%% GENERADOR SOPA

% Predicado Principal: generadorSopa

generadorSopa:-
  write('GENERADOR DE SOPAS DE LETRAS'),
  nl,nl,
  write('Nota: Debe terminar con punto todo lo que ingrese por consola.'),
  nl,nl,
  write('Indique el tamaño de la sopa de letras:'),
  nl,
  read(Tam),
  write('Indique el afabeto a utilizar:'),
  nl,
  read(Alfabeto),
  write('Indique el nombre de archivo de palabras aceptadas:'),
  nl,
  read(ArchivoA),
  write('Indique el nombre de archivo de palabras rechazadas:'),
  nl,
  read(ArchivoR),
  cargarListaPalabra(ArchivoA,Alfabeto,Aceptadas),
  cargarListaPalabra(ArchivoR,Alfabeto,Rechazadas),
  nl, write('Estas son las palabras aceptadas: '), write(Aceptadas), nl,
  nl, write('Estas son las palabras rechazadas: '), write(Rechazadas), nl,nl,
  sopaLetra(Alfabeto,Tam,Aceptadas,Rechazadas),
  quieroMas,
  !,
  nl, write('Se acabo!, no quisiste mas sopas de letras :( '), nl.
