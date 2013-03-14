%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Proyecto 2 - Sopa de Letras
%% Realizado por:
%% Oriana Gomez 09-10336
%% Carla Urrea 09-11215
%% Fecha: 22/03/13
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cargarListaPalabra(Archivo, Alfabeto):-
see(Archivo),
read(Lista),
seen,
comparar(Lista,Alfabeto).

comparar(Lista, Alfabeto):-
atom_codes(Ascii,Alfabeto),
display(get(Lista)),
X == get(Lista),
X \== 91,
X \== 93,
X \== 46,
X \== 44,
member(X,Ascii).
