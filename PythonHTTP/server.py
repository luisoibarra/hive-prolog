from typing import List, Optional, Union
from fastapi import FastAPI
from models import *
import visual
import time

app = FastAPI()

  
def console_action_input() -> Optional[Action]:
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
    return None

def visual_action_input() -> Optional[Action]:
    while not visual.action_to_perform:
        time.sleep(.3)
    action = visual.action_to_perform
    visual.action_to_perform = None
    return action

def update_visual_game_instance(game: Game):
    visual.game_instance = game

def update_visual_play_feedback(feedback: GameFeedback):
    update_visual_game_instance(feedback.game)
    visual.play_feedback = feedback.feedback
    visual.play_status = feedback.status

def console_question_answer(question:Question) -> QuestionResponse:
    print(question.header)
    for option, label in zip(question.options, question.labels):
        print(f"{option}: {label}")
    answer = None
    while True:
        print(question.header)
        answer = input('>> ')
        if not answer in question.options:
            print("Invalid option")
        else:
            break
    return QuestionResponse(answer = answer)

def visual_question_answer(question:Question) -> QuestionResponse:
    raise NotImplementedError # TODO

@app.post("/player_{player}")
async def player(player:str, param: Union[GameFeedback, Game, ActionRequest, Question]):
    print(player)    
    if isinstance(param, Game):
        print("Instancia de juego:")
        print(param)
        update_visual_game_instance(param)
    if isinstance(param, ActionRequest):
        action = None
        while not action:
            # action = console_action_input()
            action = visual_action_input()
        return action
    if isinstance(param, GameFeedback):
        print("Feedback de jugada")
        print(param)
        update_visual_play_feedback(param)
    if isinstance(param, Question):
        answer = console_question_answer(param)
        # answer = visual_question_answer(param)
        return answer
