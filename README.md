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

### Estructuras

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
- set_play(PositionSelectedPieceToSet, PosX, PosY)
  - PositionSelectedPieceToSet: Index en PiecesTypesLeftToSet de `pieces_info` de la ficha que se quiere poner
  - PosX, PosY: Posición de la ficha
- move_play(PosX, PosY, NewPosX, NewPosY)
  - PosX, PosY: Posición de la ficha en el tablero a mover
  - NewPosX, NewPosYÑ Nueva posición de la ficha
- step(Action, Game, Feedback, Status)
  - Action: Movimiento que se hizo en el paso ej: `set_play`, `move_play`.
  - Game: Instancia del `game` generada en el paso.
  - Feedback: String con un mensaje sobre el estado del paso
  - Status: Indica el estado del paso ej: invalid, continue, over, tie.
- game_config(FeedbackPlayerInfoList, ExtraPlayerInfoList)
  - FeedbackPlayerInfoList: Lista que contiene `feedback_info` con los functors a llamar para la actualización del estado de los usuarios
  - ExtraPlayerInfoList: Lista que contiene `extra_info` sobre la configuración de los usuarios.
- feedback_info(PlayerColor, StartGameStateUserFeedback, EndGameStateUserFeedback)
  - StartGameStateUserFeedback: Functor a llamar para mostrar y actualizar el estado del juego en el cliente
  - EndGameStateUserFeedback: Functor a llamar para mostrar y actualizar el estado del juego luego de que se haya hecho una jugada
- extra_info(Player, ExtraConfigInfo)
  - ExtraConfigInfo: Lista que contiene información adicional sobre el jugador necesaria para hacer los updates. Por ejemplo se usa para guardar la dirección de los jugadores HTTP conteniendo un `http_player_config(Host, Port, BasePath)`.
- player(CurrentPlayer, \[PlayerFunctor\])
  - CurrentPlayer: white, black, etc.
  - PlayerFunctor: Functor a una función que devuelve la acción del jugador

### Estrategia

#### Generales

- Cantidad de moviemientos que se pueden hacer por jugador
- Cantidad de fichas alrededor de la reina
- Cantidad de fichas que se pueden mover
- Cantidad de lugares en donde se puede poner fichas
- Tipo de fichas en el tablero

#### Poner piezas

- Las Piezas tienen una ganancia por ponerlas que puede variar de acuerdo el juego
  - Al principio se potencian las fichas que son menos potentes en moviemiento ya que la primera ficha que se pone generalmente se mantiene ahí. Por ejemplo la araña
  - La reina se potencia para colocarla en el 3ro y 4to turno

### Test

Los testing en `test_boards.pl` son para probar movimientos y el añadido de piezas con más flexibilidad de que se tiene en el juego original. Para correrlos cargar el archivo y ejecutar `run_board_test().`

Los testing en `test_games.pl` son simulaciones de juegos con todas las reglas. Para correrlos cargar el archivo y ejecutar `run_game_tests().`

### Interfaz HTTP Prolog

Se hace un POST al host con el puerto establecido al path `player_{COLOR DEL JUGADOR}`. Por ejemplo un pseudo HEADER podría ser `POST http://localhost:9001/player_white`.

Cuerpos del POST:

1. Llamado actualizando el estado:

- Se enviará un JSON con el siguiente formato

``` json
// Representación de una instancia de juego. Se usará esta plantilla luego.
game =
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
// Representación de una acción de juego. Se usará esta plantilla luego.
action=
{
  "type":"set",
  "final_x":2,
  "final_y":3,
  "piece_index":0
}

```

```json
// En caso de que se mueva una ficha
// Representación de una acción de juego. Se usará esta plantilla luego.
action=
{
  "type":"move",
  "final_x":3,
  "final_y":3,
  "from_x":2,
  "from_y":3
}

```

3. Llamado para informar sobre el feedback de la jugada
   - Se enviará un JSON con el siguiente formato

```json

{
  "game":<game>,
  "action":<action>,
  "feedback":"Game Over",
  "status": "tie"
}
// status Puede ser alguno de estos valores: [invalid, continue, tie, over]
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

## Bibliografía

- https://www.redblobgames.com/grids/hexagons/ Se usará para las posiciones de los hexágonos estilo offset coordinates odd-q de la página .
- Artificial Inteligence: A Modern Approach, Capítulo 5 pág 161
