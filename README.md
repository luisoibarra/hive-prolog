# Hive

Implementación del juego Hive en Prolog.

- Luis Ernesto Ibarra Vázquez C411
- Luis Enrique Dalmau Coopat C411

## Correr juego

### Requerimientos

- Tener de alguna forma **SwiProlog**
- Tener las dependencias de Python descritas en el **requirements.txt**

#### Opcional
- Tener docker instalado en caso de no tener **SwiProlog** en la computadora

### Jugar

#### Interfaz visual

Los *requirements.txt* tienen que estar cumplidos, lo que se puede hacer ejecutando **install_requirements.sh**.

Para jugar con interfaz visual ejectar el script **play.sh** estando en la carpeta *hive-python-visual*.

#### Correr servidor de Prolog

##### Docker

Crear imagen de Docker:

1. Abrir consola en la carpeta del proyecto
2. En la consola ejecutar `./play_docker.sh`.
   - Este paso le descargará la imagen de SwiProlog del repositorio de la UCLV `docker.uclv.cu/swipl:8.2.4` si no la tiene en la computadora.
     - En caso de ya tener una imagen de prolog en el sistema cambiar el `FROM <Nombre de la imagen>` en el Dockerfile.
   - Luego correrá el container asociado a la imagen del proyecto
3. En el container ejecutar `main.`

##### Sin Docker

1. En la consola ejecutar `swipl main.pl` o `swipl -o main.out -c main.pl;./main.out`
2. Luego ejecutar `main.`

#### A tener en cuenta

Una vez se tenga el visual y el servidor corriendo se debe seleccionar al menos un jugador Humano al iniciar el juego y verificar que el puerto `9001` no está siendo usado por otro programa en `127.0.0.1` ya que es por aquí por donde se hace la comunicación entre el visual y la lógica del juego.

## Implementación

El proyecto propuso diferentes dificultades:

- Tener una forma extensible de comunicarse con el usuario en dependencia de la interfaz que se quiera usar consola, http u alguna otra.
- Tener una manera sencilla y extensible de representar las reglas del juego.
- Tener una manera extensible de representar a los jugadores independientemente de que si es humano o no.
- Realizar un jugador inteligente autónomo que sea capaz de poder jugar contra un humano.

Lo primero a tener en cuenta son las principales estructuras que se usaron en la implementación

### Estructuras

1. Básicas

  - piece(PosX, PosY, PlayerOwner, \[PieceType,Height\])
    - PosX, PosY: Coordenadas en el grid hexagonal
    - PlayerOwner: white, black, etc.
    - PieceType: queen, beetle, spider, cricket, etc.
    - Height: Altura de la  pieza en el tablero
  - pieces_info(PlayerOwner, PiecesTypesLeftToSet)
    - PlayerOwner: white, black, etc.
    - PiecesTypesLeftToSet: Lista de tipos de piezas, ej: \[queen, spider\]
  - game(Board,CurrentTurnPlayer,\[PlayersPiecesToSet,GameHistory,CurrentTurn\])
    - Board: Lista de `piece`
    - CurrentTurnPlayer: white, black, etc.
    - PlayersPiecesToSet: Lista de `pieces_info`
    - CurrentTurn: entero indicando el turno

2. Acciones hechas por jugadores

  - set_play(PositionSelectedPieceToSet, PosX, PosY)
    - PositionSelectedPieceToSet: Index en PiecesTypesLeftToSet de `pieces_info` de la ficha que se quiere poner
    - PosX, PosY: Posición de la ficha
  - move_play(PosX, PosY, NewPosX, NewPosY, ExtraArgs)
    - PosX, PosY: Posición de la ficha en el tablero a mover
    - NewPosX, NewPosY: Nueva posición de la ficha
    - ExtraArgs: Argumentos para cualquier otro tipo de jugada

3. Flujo de juego

  - step(Action, Game, Feedback, Status)
    - Action: Movimiento que se hizo en el paso ej: `set_play`, `move_play`.
    - Game: Instancia del `game` generada en el paso.
    - Feedback: String con un mensaje sobre el estado del paso
    - Status: Indica el estado del paso ej: invalid, continue, over, tie.
  - feedback_info(PlayerColor, StartGameStateUserFeedback, EndGameStateUserFeedback)
    - StartGameStateUserFeedback: Functor a llamar para mostrar y actualizar el estado del juego en el cliente
    - EndGameStateUserFeedback: Functor a llamar para mostrar y actualizar el estado del juego luego de que se haya hecho una jugada

4. Configuración

  - player_info(CurrentPlayer, \[PlayerFunctor\])
    - CurrentPlayer: white, black, etc.
    - PlayerFunctor: Functor a una función que devuelve la acción del jugador
  - game_config(FeedbackPlayerInfoList, ExtraPlayerInfoList)
    - FeedbackPlayerInfoList: Lista que contiene `feedback_info` con los functors a llamar para la actualización del estado de los usuarios
    - ExtraPlayerInfoList: Lista que contiene `extra_info` sobre la configuración de los usuarios.
  - extra_info(Player, ExtraConfigInfo)
    - ExtraConfigInfo: Lista que contiene información adicional sobre el jugador necesaria para hacer los updates. Por ejemplo se usa para guardar la dirección de los jugadores HTTP conteniendo un `http_player_config(Host, Port, BasePath)`.

### Interfaz extensible

Este problema fue resuelto mediante el uso de functors. En la configuración por defecto del juego se guarda el functor que se utilizará para la interacción con el usuario. Lo anterior se puede observar en `run_console.pl` en el hecho *interface_functor* el cual tiene como valor asignado el functor usado para la comunicación con el usuario en términos de configuración del juego, por ejemplo que tipo de juego quiere jugar o que tipo de jugador quiere ser.

La interacción del jugador con la interfaz también es modelada mediante functors, lo cual permite abstraer la acción que se quiere hacer de cómo se hace, permitiendo una gran versatilidad. Las acciones que hacen los jugadores al interactar con la interfaz son las de mostrar el estado actual del juego y mostrar el resultado luego de un movimiento. Esta asignación de jugador a acción se puede ver en `run_console.pl` en los hechos *select_player*, los cuales tienen asignados los functors que realizarán las acciones correspondientes, aquí se crearon 3 tipos de jugadores: *Human*, *AI* y *Random*.

Una vez se tiene toda esta configuración se tiene un mismo código para correr el juego independientemente de cuál sea esta, permitiendo poder agregar o cambiar solamente los functors y manteniendo la lógica del flujo del juego.

### Lógica del juego

Básicamente el juego consiste en dos acciones posibles, poner una pieza y mover una pieza. Estas acciones se expresan mediante las cláusulas `add_piece` en `add_piece_rules.pl` y `move` en `move_piece_rules.pl` respectivamente. Las otras reglas más generales se van manteniendo a medida de que se hacen las jugadas, tales como la conectitud del tablero y la existencia de una reina del color que desea mover entre otras. Al estar estas acciones en solo dos cláusulas permite que agregar más reglas relacionadas con ellas sean tarea sencilla y también provee una manera simple de añadir los diferentes tipos de bichos presentes en el juego. Los nuevos bichos se pueden agregar al definir cómo se mueven y agregar el correspondiente `bug_movement_functor` en `bug_movement.pl`.

### Flujo del juego

El flujo del juego empieza en la cláusula `init_game` en donde se configuran las características del juego como el tipo de juego que se va a jugar y el tipo de jugadores que se va a elegir. Una vez elegido esto se genera un ciclo por un llamado recursivo de cola de la cláusula `run_game`. En esta cláusula ocurre todo lo relacionado con las acciones de los jugadores, la actualización de la interfaz con la representación actual del juego, etc.

### Jugador Inteligente

Para la creación del jugador inteligente se recurrió al uso del algoritmo minimax usando el método de poda `alpha-beta` este puede ser encotrado en `minimax.pl`. La función de decisión de la acción a tomar por este jugador es `ai_player` que se encuentra en `players.pl`.

Para la función de utilidad de dicho algoritmo se tuvieron en cuenta los siguientes criterios:

- Cantidad de movimientos que se pueden hacer por jugador
- Cantidad de fichas alrededor de la reina
- Cantidad de fichas que se pueden mover
- Cantidad de lugares en donde se puede poner fichas
- Tipo de fichas en el tablero

Estos criterios luego son normalizados y multiplicados por un peso que depende del turno en donde se encuentre el juego, de esta manera se pueden potenciar ciertos movimientos al principio o ya en el medio o largo juego. La función puede ser encontrada en `utility_function.pl`.

### Test

A medida que se aumentó la complejidad del proyecto se hicieron pequeños scripts para probar las diferentes mecánicas del juego que se estaban implementando:

- `test_boards.pl`: Probar movimientos y el añadido de piezas. Para correrlos cargar el archivo y ejecutar `run_board_test().`
- `test_games.pl`: Simulaciones de juegos con todas las reglas. Para correrlos cargar el archivo y ejecutar `run_game_tests().`


### Interfaz HTTP Prolog

Para poder conectar la lógica del juego en Prolog a cualquier cliente se creó una interfaz HTTP. Para esto se hace un POST al host (default *127.0.0.1*) con el puerto establecido (default *9001*) al path `player_{COLOR DEL JUGADOR}`. Por ejemplo un pseudo HEADER podría ser `POST http://127.0.0.1:9001/player_white`.

Cuerpos del POST:

1. Llamado actualizando el estado:

- Se enviará un JSON con el siguiente formato

``` json
// Representación de una instancia de juego (game). Se usará esta plantilla luego.
{
  "turn":1,
  "board":[
    {
      "x":2,
      "y":3,
      "color":"white",
      "type":"queen",
      "height":0
    },
    {
      "x":3,
      "y":3,
      "color":"black",
      "type":"queen",
      "height":0
    },
    {
      "x":4,
      "y":3,
      "color":"white",
      "type":"ant",
      "height":0
    }
  ],
  "player":"black",
  "remaining_pieces":[
    {
      "player":"white",
      "pieces":["ant"]
    },
    {
      "player":"black",
      "pieces":["ant", "ant"]
    }
  ]
}

```

2. Llamado para recibir la acción a jugar
   - Se enviará un JSON con el siguiente formato

```json

{
  "action":"play",
  "player":"white"
}

```

- Se espera recibir un JSON con alguno de los siguientes formatos

```json
// En caso de que se ponga una ficha
// Representación de una acción (action) de juego. Se usará esta plantilla luego.
{
  "type":"set",
  "final_x":2,
  "final_y":3,
  "piece_index":0
}

```

```json
// En caso de que se mueva una ficha
// Representación de una acción (action) de juego. Se usará esta plantilla luego.
{
  "type":"move",
  "final_x":3,
  "final_y":3,
  "from_x":2,
  "from_y":3,
  "args": [] // Argumentos para algún otro tipo de jugadas, por ejemplo la jugada especial del pillbug
}

```

3. Llamado para informar sobre el feedback de la jugada
   - Se enviará un JSON con el siguiente formato

```json

{
  "game":"Instancia <game>",
  "action":"Instancia <action>",
  "feedback":"Game Over",
  "status": "tie" // status Puede ser alguno de estos valores: [invalid, continue, tie, over]
}

```

4. Selección de opciones de configuración
    - Se enviará un JSON con el siguiente formato

```json

{
  "header": "Header de la pregunta",
  "read_header": "Subheader, Seleccione un jugador:",
  "labels": ["Opcion1", "Opcion2"], // Etiquetas que representan las opciones
  "options": ["1","2"] // Lo que se devuelve en la respuesta en dependencia de lo que se seleccione
}

```

- Se espera recibir un JSON en respuesta con el siguiente formato

```json
{
  "answer": "1" // option correspondiente al label seleccionado
}
```

### Interfaz visual

La interfaz visual se creó mediante la unión de los paquetes **FastAPI** y **pygame**. Con el primero se crearon los endpoints y los modelos necesarios para la comunicación con la lógica del juego respetando la interfaz explicada anteriormente. Con el segundo se crearon los visuales necesarios para representar el juego así como la interacción entre el usuario y el juego a través de clicks. 

Se adaptó la idea de un tablero hexagonal público de pygame ajustando las coordenadas correctas para nuestro tablero hexagonal. De ahí proviene el código de la carpeta hexmap.

Con **pygame** obtuvimos información de que clicks eran presionados y dentro de la módulo *visual.py* se crearon los eventos que se deben manejar para el juego.

Se crearon las clases **Render** (que hereda de **pygame.Surface**) y sus herederas **RenderGrid** y  **RenderUnits**

La clase **RenderPieces**, pese a no heredar de **Render**, se creó para poder manejar las piezas del juego.

Todas las clases de **Render** se dedican a dibujar sobre la ventana creada por **pygame**.

Para las piezas se creó una clase **Piece** la cual posee una posición, un tipo y un color.

Para las unidades se creó una clase **Unit** la cual posee un label, una imagen, una propiedad que indica el turno, una para saber si está seleccionada y otra para saber si es el objetivo de un efecto.

#### Movimientos

- Poner ficha:
  1. Seleccionar el tipo de ficha que se quiere poner
  2. Seleccionar una casilla válida en donde ponerla
- Mover ficha:
  1. Seleccionar la ficha a mover
  2. Seleccionar la casilla válida hacia donde moverla
- Movimiento especial:
  1. Seleccionar la ficha a hacer el movimiento especial
  2. Click derecho sobre la ficha en la que se va a hacer el movimiento especial
  3. Seleccionar la casilla hacia donde se dirije el movimiento especial

El movimiento especial se aplica sobre los pillbugs o sobre los mosquitos adyacentes a uno. 

#### Detalles sobre jugabilidad

- En caso de trabarse el juego probar realizar una jugada cualquiera.
- En caso de que al dar click en una opción esta no se seleccione probar darle click de nuevo.

## Bibliografía

- https://www.redblobgames.com/grids/hexagons/ Se usará para las posiciones de los hexágonos estilo offset coordinates odd-q de la página .
- Artificial Inteligence: A Modern Approach, Capítulo 5 pág 161
