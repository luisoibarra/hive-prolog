# Hive

Implementación del juego Hive en Prolog.

## Jugar

1. En la consola ejecutar `swipl run_console.pl`
2. Luego ejecutar `init_game().`

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
- game_config(ShowGameStateFunc, ShowGameFeedbackFunc)
  - ShowGameStateFunc: Functor a una función que muestra el estado de juego
  - ShowGameFeedbackFunc: Functor a una función que muestra el estado del juego inmediato a una jugada
- player(CurrentPlayer, \[PlayerFunctor\])
  - CurrentPlayer: white, black, etc.
  - PlayerFunctor: Functor a una función que devuelve la acción del jugador

## Test

Los testing en `test_boards.pl` son para probar movimientos y el añadido de piezas con más flexibilidad de que se tiene en el juego original. Para correrlos cargar el archivo y ejecutar `run_board_test().`

Los testing en `test_games.pl` son simulaciones de juegos con todas las reglas. Para correrlos cargar el archivo y ejecutar `run_game_tests().`

## Bibliografía

Se usará para las posiciones de los hexágonos estilo offset coordinates odd-q de la página https://www.redblobgames.com/grids/hexagons/.

Artificial Inteligence: A Modern Approach, Capítulo 5 pág 161

## TODO:
- Hacer que el feedback se pueda dar desde dentro del juego, una opción puede ser codificar el feedback para que luego se pueda decidir el mensaje más fácil.
- Organizar módulos por carpetas

## Pasos restantes

- [x] Ver cómo se puede organizar mejor el proyecto en módulos
- [x] Movimientos de fichas
- [x] Jugar en consola entre personas
- [ ] IA
- [ ] UI
