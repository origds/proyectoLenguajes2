"Tarea 4. Smalltalk"
"Realizado por: Oriana Gomez 09-10336
                Carla Urrea 09-11215"
"Fecha de modificacion: 08/04/13"

"Implementacion. Pare 1."

"Pregunta 1. Extienda OrderedCollection, tal que toda instancia
de la clase responda al mensaje subCollectionFrom:to:"

OrderedCollection extend [
    subCollectionFrom: ini to: fin [
      nuevo:= OrderedCollection new.
      [ini<=fin] whileTrue: [
        nuevo at: ini put: (self at: ini)
        ini:= ini+1.
      ]
      return ^.
    ]
]

"Pregunta 2. Extienda OrderedCollection, tal que toda instancia de la clase responda al mensaje filter:"

"Pregunta 3. Extienda la clase Array para implementar el método cuyo mensaje es esCuadradoMagicoNormal."
