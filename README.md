# Hive

Implementación del juego Hive en Prolog.

## Jugar

1. En la consola ejecutar `swipl run_console.pl`
2. Luego ejecutar `init_game().`

## Test

Los testing en `test_boards.pl` son para probar movimientos y el añadido de piezas con más flexibilidad de que se tiene en el juego original. Para correrlos cargar el archivo y ejecutar `run_board_test().`

Los testing en `test_games.pl` son simulaciones de juegos con todas las reglas. Para correrlos cargar el archivo y ejecutar `run_game_tests().`

## Bibliografía

Se usará para las posiciones de los hexágonos estilo offset coordinates odd-q de la página https://www.redblobgames.com/grids/hexagons/.

Artificial Inteligence: A Modern Approach, Capítulo 5 pág 161

## Pasos restantes

- [x] Ver cómo se puede organizar mejor el proyecto en módulos
- [x] Movimientos de fichas
- [x] Jugar en consola entre personas
- [ ] IA
- [ ] UI

TODO:
- Hacer que el feedback se pueda dar desde dentro del juego, una opción puede ser codificar el feedback para que luego se pueda decidir el mensaje más fácil.
- Cambiar la estructura de game(_,_,_,_) para que admita varios jugadores facilmente poniendo una lista de jugadores con sus piezas a poner en el último argumento de primera posicion en vez de tener las listas de piezas pestas sin agrupar, ya que esto difculta el agrego de varios jugadores.
- Organizar módulos por carpetas