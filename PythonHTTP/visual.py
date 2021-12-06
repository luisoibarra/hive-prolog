from abc import ABCMeta, abstractmethod
import time
import pygame
import math
from hexmap.Map import Grid
from models import Game, Action, Question, QuestionResponse, RemainingPiece
import os

SQRT3 = math.sqrt(3)

pygame.init()


radius = 32
pieceRadius = radius / 2
turn = 1
window=None

# String that returns the game with info about the last play
play_feedback:str = None
# String that gives information about the state of the play and game, can be [continue, invalid, tie, over]
play_status:str = None
# Rendered game instance 
game_instance:Game = Game.empty()
# Action to perform 
action_to_perform:Action = None
# Question to ask the user LUISO CHECKEA LOS COMENTARIOS QUE DIGAN LUISO
question:Question = None
# Response to the current question
question_response: QuestionResponse = None


BLACK = (0, 0, 0)
GRAY = (180, 180, 180)
WHITE = (255, 255, 255)
ORANGE = pygame.Color('orange')

# Fonts
OPEN_SANS ="assets/fonts/OpenSans-Regular.ttf"
smallFont = pygame.font.Font(OPEN_SANS, 16)
mediumFont = pygame.font.Font(OPEN_SANS, 20)
mediumFontPieces = pygame.font.Font(OPEN_SANS, 28)
largeFont = pygame.font.Font(OPEN_SANS, 40)


# Pieces
PIECES = []

ALL_BLACK_PIECES = []
ALL_WHITE_PIECES = []

PIECES_ON_GRID = []

# PIECES_IMAGES = [queen,spider,ant,beetle,cricket]

CLICKED_PIECES_ON_HAND = []

CLICKED_PIECE_ON_GRID = None

BLACKPIECES_AMOUNT = [1 for _ in range(len(PIECES))]

WHITEPIECES_AMOUNT = [1 for _ in range(len(PIECES))]


class Render(pygame.Surface):

    __metaclass__ = ABCMeta

    def __init__(self, map, radius=16, *args, **keywords):
        self.map = map
        self.radius = radius

        # Colors for the map
        self.GRID_COLOR = pygame.Color(50, 50, 50)

        super(Render, self).__init__(
            (self.width, self.height), *args, **keywords)

        self.cell = [(.5 * self.radius, 0),
                     (1.5 * self.radius, 0),
                     (2 * self.radius, SQRT3 / 2 * self.radius),
                     (1.5 * self.radius, SQRT3 * self.radius),
                     (.5 * self.radius, SQRT3 * self.radius),
                     (0, SQRT3 / 2 * self.radius)
                     ]

    @property
    def width(self):
        return math.ceil(self.map.cols / 2.0) *2 * self.radius + \
            math.floor(self.map.cols / 2.0) * self.radius + 32

    @property
    def height(self):
        return (self.map.rows + .5) * self.radius * SQRT3 + 1

    def get_surface(self, window):
        """
        Returns a subsurface corresponding to the surface, hopefully with trim_cell wrapped around the blit method.
        """
        width = 2 * self.radius
        height = self.radius * SQRT3
        row, col = window

        top = (row * height + (height / 2 if col % 2 == 1 else 0))
        left = 1.5 * self.radius * col

        left = left + self.radius*2
        top = top + self.radius*2


        surface = pygame.Rect(left , top , width, height)
        return surface

    # Draw methods
    @abstractmethod
    def draw(self):
        """
        An abstract base method for various render objects to call to paint 
        themselves.  If called via super, it fills the screen with the colorkey,
        if the colorkey is not set, it sets the colorkey to magenta (#FF00FF)
        and fills this surface. 
        """
        color = self.get_colorkey()
        if not color:
            magenta = pygame.Color(255, 0, 255)
            self.set_colorkey(magenta)
            color = magenta
        self.fill(color)

    # Identify cell
    def get_cell(self, position):
        """
        Identify the cell clicked in terms of row and column
        """
        x, y = position
        x,y = x - self.radius*2, y - self.radius*2

        # Identify the square grid the click is in.
        row = math.floor(y / (SQRT3 * self.radius))
        col = math.floor(x / (1.5 * self.radius))

        # Determine if cell outside cell centered in this grid.
        x = x - col * 1.5 * self.radius
        y = y - row * SQRT3 * self.radius


        # Correct row and col for boundaries of a hex grid
        if col % 2 == 0:
            if y < SQRT3 * self.radius / 2 and x < .5 * self.radius and \
                    y < SQRT3 * self.radius / 2 - x:
                row, col = row - 1, col - 1
            elif y > SQRT3 * self.radius / 2 and x < .5 * self.radius and \
                    y > SQRT3 * self.radius / 2 + x:
                row, col = row, col - 1
        else:
            if x < .5 * self.radius and abs(y - SQRT3 * self.radius / 2) < SQRT3 * self.radius / 2 - x:
                row, col = row , col -1
            elif y < SQRT3 * self.radius / 2:
                row, col = row - 1, col

        return (row, col) if self.map.valid_cell((row, col)) else None

    def fit_window(self, window):
        width = window.get_width()
        height = window.get_height()
        top = max(height - self.height, 0)
        left = max(width - self.width, 0)
        return (top, left)

class RenderUnits(Render):
    """
    A premade render object that will automatically draw the Units from the map 
     
    """

    def __init__(self, map, *args, **keywords):
        super(RenderUnits, self).__init__(map, *args, **keywords)
        if not hasattr(self.map, 'units'):
            self.map.units = Grid()

    def draw(self):
        """
        Calls unit.paint for all units on self.map
        """
        super(RenderUnits, self).draw()
        units = self.map.units

        for position, unit in units.items():
            surface = self.get_surface(position)
            unit.paint(surface,unit.playerBlack)

class RenderPieces:
    """
    A premade render object that will automatically draw the available pieces to place in the board down at the bottom of the window
    """
    def __init__(self,radius, pieces,playerBlack) -> None:
        self.radius = radius
        self.pieces = pieces
        self.height = radius
        self.playerBlack = playerBlack

    def draw(self, window):
        """Draw the pieces at the bottom of the window"""
        width = window.get_width()
        height = window.get_height()
        if len(self.pieces) == 0:
            width_of_piece = width
        else:
            width_of_piece = width / len(self.pieces)
        height_of_piece = self.radius * SQRT3

        if self.playerBlack:
            location = height - height_of_piece
        else:
            location = 0

        for i, piece in enumerate(self.pieces):
            pieceRect = pygame.Rect(i * width_of_piece, location, width_of_piece, height_of_piece)
            count = 0  # ADDED ADDED ADDED ADDED ADDED
            if(turn % 2) == self.playerBlack:
                clicked =  CLICKED_PIECES_ON_HAND[i]
                if(turn % 2):
                    if BLACKPIECES_AMOUNT[i] == 0:
                        continue
                    else:
                        count = BLACKPIECES_AMOUNT[i]
                    
                else:
                    if WHITEPIECES_AMOUNT[i] == 0:
                        continue
                    else:
                        count = WHITEPIECES_AMOUNT[i]
            else:
                clicked = 0
                
            piece.paint(pieceRect,self.playerBlack,clicked,count)

    def get_cell(self, position,window):
        """
        Identify the cell clicked in terms of row and column
        """
        x, y = position

        # Identify the piece grid the click is in.
        col = math.floor((x /window.get_width())*len(self.pieces))
        if PIECES:
            return PIECES[col],col
        else:
            return None, col
            
class RenderGrid(Render):
    def draw(self):
        """
        Draws a hex grid, based on the map object, onto this Surface
        """
        super(RenderGrid, self).draw()
        # A point list describing a single cell, based on the radius of each hex

        for col in range(self.map.cols):
            # Alternate the offset of the cells based on column
            offset = self.radius * SQRT3 / 2 if col % 2 else 0
            for row in range(self.map.rows):
                # Calculate the offset of the cell
                top = offset + SQRT3 * row * self.radius
                left = 1.5 * col * self.radius
                # Create a point list containing the offset cell
                points = [(x + left, y + top) for (x, y) in self.cell]
                # Draw the polygon onto the surface
                pygame.draw.polygon(self, self.GRID_COLOR, points, 1)


def run():
    from hexmap.Map import Map, MapUnit
    import sys

    class Unit(MapUnit):
        def __init__(self, grid,label,image,playerBlack):
            super().__init__(grid)
            self.label = label
            self.image = image
            self.playerBlack = playerBlack
            self.selected = False


        def paint(self, unitRect,playerBlack):
            color2 = GRAY
            if playerBlack:
                color1 = BLACK
            else:
                color1 = WHITE
            radius = unitRect.width/ 2
            pieceText = mediumFontPieces.render(f"{self.label}", True, color1)
            pieceTextRect = unitRect
            center = pieceTextRect.center
            center=(center[0]+ (radius/1.5),center[1]+(radius/4))
            pieceTextRect.center = center
            if self.selected:
                center = (pieceTextRect.center[0]-radius/2,pieceTextRect.center[1]-radius/4)
                pygame.draw.circle(window, color2, center, radius/1.5)
            window.blit(pieceText, pieceTextRect)

    class Piece:
    

        def __init__(self,label) -> None:
            self.label = label

        def paint(self, pieceRect,playerBlack,clicked,count):
            if playerBlack:
                color1=WHITE
                color2=BLACK
                
            else:
                color1=BLACK
                color2=WHITE

            if clicked:
                color2 = GRAY

            pieceText = smallFont.render(
                f"{self.label} : {count}", True, color1)
            pieceTextRect = pieceText.get_rect()
            pieceTextRect.center = pieceRect.center
            pygame.draw.rect(window, color2, pieceRect)
            window.blit(pieceText, pieceTextRect)

    def update_variables(game_instance: Game,map:Map):
        global PIECES
        global BLACKPIECES_AMOUNT
        global WHITEPIECES_AMOUNT
        global turn
        global PIECES_ON_GRID
        global CLICKED_PIECES_ON_HAND

        turn = game_instance.turn - 1

        global ALL_BLACK_PIECES 
        global ALL_WHITE_PIECES 

        

        for pieces in game_instance.remaining_pieces:
            if pieces.player.lower() == "white":
                ALL_WHITE_PIECES = pieces.pieces
            else:
                ALL_BLACK_PIECES = pieces.pieces
                
        PIECES = list(dict.fromkeys(ALL_BLACK_PIECES).keys())
        PIECES_ON_GRID = [string.upper()[0] for string in PIECES]
        CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]

        BLACKPIECES_AMOUNT = [ALL_BLACK_PIECES.count(piece) for piece in PIECES]
        WHITEPIECES_AMOUNT = [ALL_WHITE_PIECES.count(piece) for piece in PIECES]
        
        smallFont = pygame.font.Font(OPEN_SANS, 20 - len(PIECES)) # LUISO AJUSTA AQUI EL TAMAÑO DE LA FUENTE
        piecesBlack = RenderPieces(pieceRadius, [Piece(piece) for piece in PIECES], playerBlack=True)
        piecesWhite = RenderPieces(pieceRadius, [Piece(piece) for piece in PIECES], playerBlack=False)

        return smallFont, piecesBlack, piecesWhite

    def fill_map(game_instance: Game, map: Map):

        board = game_instance.board
        board.sort(key = lambda x: (x.x,x.y,x.height))

        map.units.clear()

        for piece in game_instance.board:
            cell =  (piece.y,piece.x)
            heigth = piece.height
            label = piece.type[0].upper()
            for i in range(heigth):
                label += "*"
            player = 1 if piece.color == "black" else 0
            map.units[cell] = Unit(map, label, None, player)

    global window
    global turn
    
    global game_instance
    global action_to_perform
    global play_feedback
    global play_status
    global question_response
    global question

    global smallFont
    
    global PIECES 

    global PIECES_ON_GRID 

    # global PIECES_IMAGES 

    global CLICKED_PIECES_ON_HAND

    global CLICKED_PIECE_ON_GRID 

    global BLACKPIECES_AMOUNT 

    global WHITEPIECES_AMOUNT 


    m = Map((8, 10))

    while not game_instance:
        time.sleep(.5)
    
    # ADDED ADDED ADDED ADDED ADDED
    
    global ALL_BLACK_PIECES
    global ALL_WHITE_PIECES

    # for pieces in game_instance.remaining_pieces:
    #     if pieces.player.lower() == "white":
    #         ALL_WHITE_PIECES = pieces.pieces
    #     else:
    #         ALL_BLACK_PIECES = pieces.pieces
    
    # PIECES = list(dict.fromkeys(ALL_BLACK_PIECES).keys())
    # PIECES_ON_GRID = [string.upper()[0] for string in PIECES]
    # CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]


    smallFont, piecesBlack, piecesWhite = update_variables(game_instance,m)
    fill_map(game_instance,m)

    grid = RenderGrid(m, radius)
    units = RenderUnits(m, radius)

    print(m.ascii())

    try:
        
        
        fpsClock = pygame.time.Clock()
        window = pygame.display.set_mode((800,600), 1)
        from pygame.locals import QUIT, MOUSEBUTTONDOWN
        width = window.get_width()
        height = window.get_height()

        

        #Leave it running until exit
        while True:
             
            for event in pygame.event.get():
                if event.type == QUIT:
                    pygame.quit()
                    sys.exit()
                if event.type == MOUSEBUTTONDOWN:
                    if event.pos[1]<radius*2:
                        print("Clicked on white piece")
                        whitePiece,i = piecesWhite.get_cell(event.pos, window)
                        print(whitePiece,i)
                        if whitePiece is not None:
                            if(turn % 2) == 0 and WHITEPIECES_AMOUNT[i] != 0:
                                if not CLICKED_PIECES_ON_HAND[i] and sum(CLICKED_PIECES_ON_HAND) >= 1:
                                    CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]
                                CLICKED_PIECES_ON_HAND[i] = not CLICKED_PIECES_ON_HAND[i]

                        
                    elif event.pos[1]>height-pieceRadius - 8:
                        print("Clicked on black piece")
                        blackPiece,i = piecesBlack.get_cell(event.pos, window)
                        print(blackPiece,i)
                        if blackPiece is not None:
                            if(turn % 2) == 1 and BLACKPIECES_AMOUNT[i]!=0:
                                if not CLICKED_PIECES_ON_HAND[i] and sum(CLICKED_PIECES_ON_HAND) >= 1:
                                    CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]
                                CLICKED_PIECES_ON_HAND[i] = not CLICKED_PIECES_ON_HAND[i]
                    else:
                        print("Clicked on grid")
                        cell = units.get_cell(event.pos)
                        print(cell)
                        if cell:
                            if sum(CLICKED_PIECES_ON_HAND)==1:
                                CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]
                                
                                #################################
                                # PLACE A PIECE ON THE GRID
                                #################################
                                if m.units[cell] is None:
                                    #m.units[cell] = Unit(m, PIECES[i][0].upper() , None,(turn % 2))
                                
                                    if(turn % 2):
                                        #BLACKPIECES_AMOUNT[i]+=-1
                                        piece_index = ALL_BLACK_PIECES.index(blackPiece)
                                    else:
                                        #WHITEPIECES_AMOUNT[i]+=-1
                                        piece_index = ALL_WHITE_PIECES.index(whitePiece)

                                    print(piece_index)

                                    action_to_perform = Action(type = "set",
                                                    final_x = cell[1],
                                                    final_y = cell[0],
                                                    piece_index = piece_index)
                                    
                            

                                ################################

                            elif sum(CLICKED_PIECES_ON_HAND)==0:
                                unit = m.units.get(cell, None)                            
                                if unit:
                                    if unit.playerBlack ==(turn % 2):
                                        if not unit.selected and sum([x.selected for x in m.units.values()]) >= 1:
                                            for index, x in m.units.items():
                                                if x.selected:
                                                    if x==unit:
                                                        unit.selected = not unit.selected
                                                    else:
                                                        from_x, from_y = index

                                                        action_to_perform = Action(type="move",
                                                                                final_x=cell[1],
                                                                                final_y=cell[0],
                                                                                from_x=from_y,
                                                                                from_y=from_x)

                                                        x.selected = False
                                        else :
                                            unit.selected = not unit.selected
                                        
                                    else:
                                        for index, x in m.units.items():
                                            if x.selected:
                                                from_x, from_y = index

                                                action_to_perform = Action(type="move",
                                                                        final_x=cell[1],
                                                                        final_y=cell[0],
                                                                        from_x=from_y,
                                                                        from_y=from_x)

                                            x.selected = False

                                else:
                                    for index , x in m.units.items():
                                        if x.selected:
                                            from_x,from_y = index

                                            action_to_perform = Action(type="move",
                                                            final_x=cell[1],
                                                            final_y=cell[0],
                                                            from_x=from_y,
                                                            from_y = from_x)

                                        x.selected = False
                            
                            else:
                                raise Exception("Multiple clicked pieces")
            

            ##############################################################
            # RENDER WINDOW BASE COLOR
            ##############################################################
            window.fill(ORANGE)

            ##############################################################
            

            ##############################################################
            # RENDER GRID
            ##############################################################
            grid.draw()

            ##############################################################


            ##############################################################
            # RENDER UNITS
            ##############################################################
            units.draw()

            ##############################################################


            ##############################################################
            # RENDER PLAYER TURN
            ##############################################################

            if(turn % 2):
                piecesBlack.draw(window)
                turnColor = BLACK
                
            else:
                piecesWhite.draw(window)
                turnColor = WHITE
            playerText = "Player Black" if(turn % 2) else "Player White"
            turnText = mediumFont.render(
                f"{playerText}", True, turnColor)

            turnTextRect = turnText.get_rect()
            turnRect = pygame.Rect(width - radius*7, radius*2.5,turnTextRect.width,turnTextRect.height)
            turnTextRect.center = turnRect.center
            pygame.draw.rect(window, ORANGE, turnRect)
            window.blit(turnText, turnTextRect)

            ##############################################################


            ##############################################################
            # RENDER FEEDBACK 
            ##############################################################

            if play_feedback:
                feedbackText = mediumFont.render(
                    f"{play_feedback.upper()}", True, turnColor)


                feedbackTextRect = feedbackText.get_rect()
                feedbackRect = pygame.Rect(
                    width - radius*7, radius*3.5, feedbackTextRect.width, feedbackTextRect.height)
                feedbackTextRect.center = feedbackRect.center
                pygame.draw.rect(window, ORANGE, feedbackRect)
                window.blit(feedbackText, feedbackTextRect)
            
            ##############################################################

            
            ##############################################################
            # RENDER QUESTION CHOICES BUTTONS
            ##############################################################

            if question:
                # print buttons for each choice
                question_headerText = mediumFont.render(question.header, True, BLACK)
                question_headerTextRect = question_headerText.get_rect()
                question_headerRect = pygame.Rect(
                        width - radius*7, radius*3.5 + 2*question_headerTextRect.height+10, question_headerTextRect.width, question_headerTextRect.height)
                question_headerTextRect.center = question_headerRect.center
                pygame.draw.rect(window, WHITE, question_headerRect)
                window.blit(question_headerText, question_headerTextRect)
                #pygame.display.flip()

                # select header
                select_headerText = mediumFont.render(question.read_header, True, BLACK)
                select_headerTextRect = select_headerText.get_rect()

                select_headerRect = pygame.Rect(
                    width - radius*7, radius*3.5 + (4)*select_headerTextRect.height + 10, select_headerTextRect.width, select_headerTextRect.height)

                select_headerTextRect.center = select_headerRect.center
                pygame.draw.rect(window, WHITE, select_headerRect)
                window.blit(select_headerText, select_headerTextRect)
                
                for i, (choice, label) in enumerate(zip(question.options, question.labels)):
                    # choice button
                    choiceText = mediumFont.render(label, True, BLACK)
                    choiceTextRect = choiceText.get_rect()

                    choiceRect = pygame.Rect(
                        width - radius*7, radius*3.5 + (i+6)*choiceTextRect.height + 10, choiceTextRect.width, choiceTextRect.height)
                    
                    choiceTextRect.center = choiceRect.center
                    pygame.draw.rect(window, WHITE, choiceRect)
                    window.blit(choiceText, choiceTextRect)

                    # Check if choice button clicked
                    click, _, _ = pygame.mouse.get_pressed()
                    if click == 1:
                        mouse = pygame.mouse.get_pos()
                        if choiceRect.collidepoint(mouse):
                            # choice selected 
                            question_response = QuestionResponse(answer=choice)
                            time.sleep(0.3)

                    #pygame.display.flip()

            ##############################################################


           
            window.blit(grid, (radius*2  ,radius*2 ))
            window.blit(units, (radius*2 , radius*2 ))
            if game_instance:
                print("Game instance exist")
                smallFont, piecesBlack, piecesWhite = update_variables(game_instance,m)
                fill_map(game_instance,m)
                game_instance = None
                # action_to_perform = "Something"
            
            pygame.display.update()
            fpsClock.tick(10)
    finally:
        pygame.quit()


if __name__ == '__main__':
    run()
else:
    from threading import Thread
    t = Thread(target=run)
    t.start()
