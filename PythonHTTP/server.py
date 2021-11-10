from typing import List, Union
from fastapi import FastAPI
from pydantic import BaseModel
from pydantic.types import OptionalInt
import visual

app = FastAPI()

class Piece(BaseModel):
    x:int
    y:int
    color:str
    type:str
    height:int

class RemainingPiece(BaseModel):
    player:str
    pieces:List[str]

class Game(BaseModel):
    turn:int
    board: List[Piece]
    player:str
    remaining_pieces:List[RemainingPiece]

class ActionRequest(BaseModel):
    action:str
    player:str
    
class Action(BaseModel):
    type:str
    final_x:int
    final_y:int
    piece_index:OptionalInt
    from_x:OptionalInt
    from_y:OptionalInt
    
class GameFeedback(BaseModel):
    game:Game
    action:Action
    feedback:str
    status:str
        
@app.get("/")
async def root():
    return {"message": "Hello World"}

# @app.post("/player_white")
# def player_white():
#     p = input("Escribe p")
#     q = input("Escribe q")
#     return {"p":p, "q":q}


@app.post("/player_{player}")
def player(player:str, param: Union[GameFeedback, Game, ActionRequest]):
    print(player)    
    if isinstance(param, Game):
        print("Aqui es donde el juego se debe de actualizar")
        print(param)
    if isinstance(param, ActionRequest):
        print("Haz una jugada")
        play = input("1: Set, 2: Move: ")
        if play == "1":
            index = input("index de la pieza a colocar: ")
            pos_x = input("Pos x: ")
            pos_y = input("Pos y: ")
            return Action(type="set",final_x=int(pos_x),final_y=int(pos_y), piece_index=int(index))
        if play == "2":
            src_pos_x = input("Desde Pos x: ")
            src_pos_y = input("Desde Pos y: ")
            dest_pos_x = input("Para Pos x: ")
            dest_pos_y = input("Para Pos y: ")
            return Action(type="move", from_x=int(src_pos_x), from_y=int(src_pos_y),final_x=int(dest_pos_x),final_y=int(dest_pos_y))
    if isinstance(param, GameFeedback):
        print("Aqui es donde el juego muestra el update despues de una jugada")
        print(param)
