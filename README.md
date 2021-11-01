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
Hay errores a la hora de colocar piezas, me coloco una hormiga encima de una reina.
