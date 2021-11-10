# Hive

Implementación del juego Hive en Prolog.

## Jugar

### Docker

Crear imagen de Docker:

1. Abrir consola en la carpeta del proyecto
2. En la consola ejecutar `docker build -t swipl-hive:v1 .` Notar el punto al final.
   - Este paso le descargará la imagen de SwiProlog del repositorio de la UCLV `docker.uclv.cu/swipl:8.2.4`.
   - En caso de ya tener una imagen de prolog en el sistema cambiar el `FROM <Nombre de la imagen>` en el Dockerfile.

Correr container con el proyecto:

1. En la consola ejecutar `docker run --rm -it --net=host swipl-hive:v1 ./main.out`
2. Luego ejecutar `main().`

### Sin Docker

1. En la consola ejecutar `swipl main.pl` o `swipl -o main.out -c main.pl;./main.out`
2. Luego ejecutar `main().`

## Estructuras

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
  - CurrentTurn: entero
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

## Test

Los testing en `test_boards.pl` son para probar movimientos y el añadido de piezas con más flexibilidad de que se tiene en el juego original. Para correrlos cargar el archivo y ejecutar `run_board_test().`

Los testing en `test_games.pl` son simulaciones de juegos con todas las reglas. Para correrlos cargar el archivo y ejecutar `run_game_tests().`

## Bibliografía

Se usará para las posiciones de los hexágonos estilo offset coordinates odd-q de la página https://www.redblobgames.com/grids/hexagons/.

Artificial Inteligence: A Modern Approach, Capítulo 5 pág 161

## TODO

- Hacer que el feedback se pueda dar desde dentro del juego, una opción puede ser codificar el feedback para que luego se pueda decidir el mensaje más fácil.
- Organizar módulos por carpetas

## Interfaz HTTP

Se hace un POST al host con el puerto establecido al path `player_{COLOR DEL JUGADOR}`. Por ejemplo un pseudo HEADER podría ser `POST http://localhost:9000/player_white`.

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

## Pasos restantes

- [x] Ver cómo se puede organizar mejor el proyecto en módulos
- [x] Movimientos de fichas
- [x] Jugar en consola entre personas
- [ ] AI
- [ ] UI
