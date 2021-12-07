from typing import List, Optional
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
    
    @staticmethod
    def empty() -> 'Game':
        return Game(turn=0, board=[], player="", remaining_pieces=[])

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
    args:Optional[List[int]]
    
class GameFeedback(BaseModel):
    game:Game
    action:Action
    feedback:str
    status:str

class Question(BaseModel):
    header:str
    read_header:str
    labels:List[str]
    options:List[str]

class QuestionResponse(BaseModel):
    answer:str