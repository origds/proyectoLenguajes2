%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Proyecto 2. Prolog - Sopa de Letras
%% Realizado por:
%% Oriana Gomez 09-10336
%% Carla Urrea 09-11215
%% Fecha: 22/03/13
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Funcion para cargar las listas de palabras

cargarListaPalabra(Archivo, Alfabeto):-
see(Archivo),
read(Lista),
seen,
transformaEnAscii(Lista,Alfabeto),
!.

% Funcion para llevar las listas y el alfabeto a Ascii

transformaEnAscii([],_).

% Caso 1. Alfabeto de numeros

transformaEnAscii([C|R],Alf):-
Lista = [],
numberAscii(Alf,Lista,AlfabetoAscii),
name(C, CabezaAscii),
comparar(CabezaAscii,AlfabetoAscii),
transformaEnAscii(R,Alf),
!.

% Caso 2. Alfabeto de letras

transformaEnAscii([C|R], Alf):-
atom_codes(Temp,Alf),
name(Temp, AlfabetoAscii),
name(C, CabezaAscii),
comparar(CabezaAscii,AlfabetoAscii),
transformaEnAscii(R,Alf),
!.

% Funcion para convertir un alfabeto numerico en Ascii

numberAscii([],L,AlfabetoAscii):-
AlfabetoAscii = L.

numberAscii([AlfC|AlfR],L,AlfabetoAscii):-
number(AlfC),
name(AlfC,Temp),
append(L,Temp,X),
numberAscii(AlfR,X,AlfabetoAscii).

% Funcion para comparar que las palabras estan formadas por
% el alfabeto

comparar([],_).

comparar([C|R],Alf):-
member(C,Alf),
comparar(R,Alf),
!.

% Funcion que devuelve todas las posibles sopas de letras

%sopaLetra(Alfabeto,Tam,Aceptadas,Rechazadas):-

% Funcion que muestra las sopas de letra en pantalla

mostrarSopa([C|R]):-
separar(C),
mostrarSopa(R),
!.

mostrarSopa([]).

% Funcion que imprime las lineas de la sopa de letras

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