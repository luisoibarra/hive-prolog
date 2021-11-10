from typing import List
from pydantic import BaseModel
from pydantic.types import OptionalInt

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
      