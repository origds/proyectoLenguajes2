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

"Pregunta 2. Extienda OrderedCollection, tal que toda instancia
de la clase responda al mensaje filter:"

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

"Pregunta 3. Extienda la clase Array para implementar el método
cuyo mensaje es esCuadradoMagicoNormal."


"Implementacion. Parte 2"

"Pregunta 1. Implementacion Clase Coleccion"

Object subclass: Coleccion [

  | coleccion |
   
  init [
    coleccion := OrderedCollection new.
  ] 
  
  getColeccion [
    ^coleccion.
  ]
  
].

Coleccion class extend [

  inicializar [
    |obj|
    obj := super new.
    obj init.
    ^obj
  ]
  
].

Coleccion extend [

  printOn: stream [
    stream nextPutAll: 'Coleccion: '.
    coleccion printOn: stream.
  ]
  
].

"Pregunta 2. Clase Pila implementada como Coleccion"

Coleccion subclass: #Pila.

Pila class extend [
  
  | pila |
  
  new [
    pila:= super inicializar.
    ^pila
  ]
  
].

Pila extend [

  tamano [
    |tam|
    tam:= (super getColeccion) size.
    ^tam
  ]

  empilar: elem [
    (super getColeccion) addFirst: elem.
  ]
  
  tope [
    |tam elemento|
    tam:= self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'La pila se encuentra vacia.')
    ].
    elemento:= (super getColeccion) first.
    ^elemento
  ]  
  
  desempilar [
    |tam elemento|
    tam:= self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'La pila se encuentra vacia. No puede desempilar ningun elemento.')
    ].
    elemento:= self tope.
    (super getColeccion) removeFirst.
    ^elemento    
  ]
  
  desempilarHasta: bloque [
    |tam elemento|
    tam:= self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'La pila se encuentra vacia. No puede desempilar ningun elemento.')
    ].
    1 to: tam do: 
      [ :x | (bloque value: (self tope)) ifTrue: [self desempilar] ]
  ]
  
].

"Pregunta 3. Clase Cola implementada como Coleccion"

Coleccion subclass: #Cola.

Cola class extend [
  
  | cola |
  
  new [
    cola:= super inicializar.
    ^cola
  ]
  
].

Cola extend [

  tamano [
    |tam|
    tam:= (super getColeccion) size.
    ^tam
  ]

  encolar: elem [
    (super getColeccion) addLast: elem.
  ]

  proximo [
    |tam elemento|
    tam:= self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'Error: La cola se encuentra vacia')
    ].
    elemento:= (super getColeccion) first.
    ^elemento
  ]

        desencolar [
                |tam elemento|
                tam := self tamano.
    (tam = 0) ifTrue: [
      ^(self error: 'Error: La cola se encuentra vacia')
    ].
    elemento := (super getColeccion) removeFirst.
    ^elemento
        ] 

].

"Pregunta 4. Implementacion de Cola Memoria"

Cola subclass: ColaMemoria [
        |desencolados|

        inicializar [
                desencolados := Set new.
        ]
]

ColaMemoria class extend [
        
        new[
                ^(super new inicializar)
        ]

]

ColaMemoria extend [
        tamano [
                ^(super tamano)
        ]

        proximo [
                ^(super proximo)
        ]

        encolar: elemento [
                ^(super encolar: elemento)
        ]

        desencolar [
                |elemento|
                elemento := super desencolar.
                desencolados add: elemento.
                ^elemento
        ]

        obtenerDesencolados [
                ^desencolados
        ]
].
