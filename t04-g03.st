"Tarea 4. Smalltalk"
"Realizado por: Oriana Gomez 09-10336
                Carla Urrea 09-11215"
"Fecha de modificacion: 08/04/13"

"Implementacion. Pare 1."

"Pregunta 1. Extienda OrderedCollection, tal que toda instancia
de la clase responda al mensaje subCollectionFrom:to:"

OrderedCollection extend [
    subCollectionFrom: ini to: fin [
      |nuevo i|
      i:= ini.
      nuevo:= OrderedCollection new.
      ini to: fin do: [:x | nuevo addLast: (self at: x)].
      ^nuevo 
    ]
]

|a|
a:= OrderedCollection new.
a addLast: 4.
a addLast: 0.
a addLast: 3.
a addLast: -2.
a addLast: -1. 
a addLast: 5. 
a addLast: 1. 
a printNl.
(a subCollectionFrom: 5 to: 7) printNl.



"Pregunta 2. Extienda OrderedCollection, tal que toda instancia de la clase responda al mensaje filter:"

OrderedCollection extend [
  filter: condicion [
    | aux i nuevo tam seCumple|
    aux := OrderedCollection new.
    tam := self size.
    i := 1.
    [i <= tam] whileTrue: [
      seCumple := condicion value: (self at: i).      
      seCumple ifTrue: [
        aux addLast: (self at: i).
      ].
      i := i +1
    ].
    ^aux
  ]
]

|nueva|
nueva := OrderedCollection new: 5.
nueva addLast: 1.
nueva addLast: 2.
nueva addLast: -3.
nueva addLast: 0.
nueva addLast: -5.
aux := nueva filter: [:x | x  == -5 ].
aux printNl.

"Pregunta 3. Extienda la clase Array para implementar el método cuyo mensaje es esCuadradoMagicoNormal."
