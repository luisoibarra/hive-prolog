from abc import ABCMeta, abstractmethod
import pygame
import math
from hexmap.Map import Grid
import os

SQRT3 = math.sqrt(3)

pygame.init()

radius = 32
pieceRadius = radius / 2
turn = 1


BLACK = (0, 0, 0)
GRAY = (180, 180, 180)
WHITE = (255, 255, 255)

# Fonts
OPEN_SANS ="assets/fonts/OpenSans-Regular.ttf"
smallFont = pygame.font.Font(OPEN_SANS, 20)
mediumFont = pygame.font.Font(OPEN_SANS, 28)
largeFont = pygame.font.Font(OPEN_SANS, 40)

# Add images
# flag = pygame.image.load(os.path.join("assets/images/flag.png"))
# flag = pygame.transform.scale(flag, (radius, radius * 2))
# mine = pygame.image.load("assets/images/mine.png")
# mine = pygame.transform.scale(mine, (radius*2, radius*2))
queen = pygame.image.load(os.path.join("assets/images/queen.png"))
queen = pygame.transform.scale(queen, (int(radius*SQRT3), radius * 2))
spider = pygame.image.load(os.path.join("assets/images/spider.png"))
spider = pygame.transform.scale(spider, (int(radius*SQRT3), radius * 2))
ant = pygame.image.load(os.path.join("assets/images/ant.png"))
ant = pygame.transform.scale(ant, (int(radius*SQRT3), radius * 2))
beetle = pygame.image.load(os.path.join("assets/images/beetle.png"))
beetle = pygame.transform.scale(beetle, (int(radius*SQRT3), radius * 2))
cricket = pygame.image.load(os.path.join("assets/images/cricket.png"))
cricket = pygame.transform.scale(cricket, (int(radius*SQRT3), radius * 2))


# Pieces
PIECES = ['Queen', 'Spider', 'Ant', 'Beetle', 'Cricket']

PIECES_ON_GRID = ["Q", "S", "A", "B", "C"]

PIECES_IMAGES = [queen,spider,ant,beetle,cricket]

CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]

CLICKED_PIECE_ON_GRID = None

BLACKPIECES = [3,3,3,3,3]

WHITEPIECES = [3,3,3,3,3]


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
                row, col = row - 1, col - 1
            elif y < SQRT3 * self.radius / 2:
                row, col = row - 1, col

        return (row, col) if self.map.valid_cell((row, col)) else None

    def fit_window(self, window):
       top = max(window.get_height() - self.height, 0)
       left = max(window.get_width() - self.width, 0)
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
        width_of_piece = window.get_width() / len(self.pieces)
        height_of_piece = self.radius * SQRT3

        if self.playerBlack:
            location = window.get_height() - height_of_piece
        else:
            location = 0

        for i, piece in enumerate(self.pieces):
            pieceRect = pygame.Rect(i * width_of_piece, location, width_of_piece, height_of_piece)
            
            if turn == self.playerBlack:
                clicked =  CLICKED_PIECES_ON_HAND[i]
                if turn:
                    if WHITEPIECES[i] == 0:
                        continue
                    else:
                        count = WHITEPIECES[i]
                else:

                    if BLACKPIECES[i]==0:
                        continue
                    else:
                        count = BLACKPIECES[i]
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

        return PIECES[col],col

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


class RenderFog(Render):

    OBSCURED = pygame.Color(00, 00, 00, 255)
    SEEN = pygame.Color(00, 00, 00, 100)
    VISIBLE = pygame.Color(00, 00, 00, 0)

    def __init__(self, map, *args, **keywords):

        super(RenderFog, self).__init__(
            map, *args, flags=pygame.SRCALPHA, **keywords)
        if not hasattr(self.map, 'fog'):
            self.map.fog = Grid(default=self.OBSCURED)

    def draw(self):

        #Some constants for the math
        height = self.radius * SQRT3
        width = 1.5 * self.radius
        offset = height / 2

        for cell in self.map.cells():
            row, col = cell
            surface = self.get_cell(cell)

            # Calculate the position of the cell
            top = row * height - offset * col
            left = width * col

            #Determine the points that corresponds with
            points = [(x + left, y + top) for (x, y) in self.cell]
            # Draw the polygon onto the surface
            pygame.draw.polygon(self, self.map.fog[cell], points, 0)


def trim_cell(surface):
    pass


if __name__ == '__main__':
    from hexmap.Map import Map, MapUnit
    import sys



    class Unit(MapUnit):
        def __init__(self, grid,label,image,playerBlack):
            super().__init__(grid)
            self.label = label
            self.image = image
            self.playerBlack = playerBlack


        def paint(self, unitRect,playerBlack):
            if playerBlack:
                color2 = WHITE
                color1 = BLACK
            else:
                color2 = BLACK
                color1 = WHITE
            radius = unitRect.width/ 2
            pieceText = mediumFont.render(f"{self.label}", True, color1)
            pieceTextRect = unitRect
            # center = surface.get_rect().center
            center = pieceTextRect.center
            center=(center[0]+ (radius/1.5),center[1]+(radius/4))
            pieceTextRect.center = center
            # pygame.draw.circle(surface, color2, (radius, int(
            #     SQRT3 / 2 * radius)), int(radius - radius * .3))
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

    

    m = Map((6, 8))
    pieces = [Piece(piece) for piece in PIECES]
    piecesBlack = RenderPieces(pieceRadius, pieces, playerBlack=True)
    piecesWhite = RenderPieces(pieceRadius, pieces, playerBlack=False)
    grid = RenderGrid(m, radius)
    units = RenderUnits(m, radius)
    
    #fog = RenderFog(m, radius=32)

    # m.units[(0, 0)] = Unit(m, 'R', True)
    # m.units[(3, 2)] = Unit(m)
    # m.units[(5, 3)] = Unit(m)
    # m.units[(5, 4)] = Unit(m)

    # for cell in m.spread((3, 2), radius=2):
    #     m.fog[cell] = fog.SEEN

    # for cell in m.spread((3, 2)):
    #     m.fog[cell] = fog.VISIBLE

    print(m.ascii())

    try:
        
        fpsClock = pygame.time.Clock()

        window = pygame.display.set_mode((640, 480), 1)
        from pygame.locals import QUIT, MOUSEBUTTONDOWN

        

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
                        print(whitePiece)
                        
                        if turn == 0:
                            CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]
                            CLICKED_PIECES_ON_HAND[i] = 1

                        
                    elif event.pos[1]>window.get_height()-pieceRadius - 8:
                        print("Clicked on black piece")
                        blackPiece,i = piecesBlack.get_cell(event.pos, window)
                        print(blackPiece)
                        
                        if turn == 1:
                            CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]
                            CLICKED_PIECES_ON_HAND[i] = 1
                    else:
                        print("Clicked on grid")
                        cell = units.get_cell(event.pos)
                        print(cell)
                        if cell:
                            if sum(CLICKED_PIECES_ON_HAND)==1:
                                CLICKED_PIECES_ON_HAND = [0 for _ in range(len(PIECES))]
                                
                                #################################
                                # PLACE A PIECE 
                                m.units[cell] = Unit(m, PIECES_ON_GRID[i] , PIECES_IMAGES[i], turn)
                                if turn:
                                    WHITEPIECES[i]+=-1
                                else:
                                    BLACKPIECES[i]+=-1
                                    
                                turn = (turn + 1) % 2


                                ################################

                            elif sum(CLICKED_PIECES_ON_HAND)==0:

                                ###############################
                                # MAYBE FOR SELECTING A PIECE TO MOVE OR 
                                # SELECTING THE DESTINATION GRID FOR A PLACED PIECE TO MOVE FOR
                                if CLICKED_PIECE_ON_GRID:
                                    if CLICKED_PIECE_ON_GRID == cell:
                                        CLICKED_PIECE_ON_GRID = None
                                    else:
                                        # Move()
                                        pass
                                else:
                                    CLICKED_PIECE_ON_GRID = cell

                                ###############################
                                pass
                            
                            else:
                                raise Exception("Multiple clicked pieces")


            window.fill(pygame.Color('orange'))
            grid.draw()
            units.draw()
            if turn:
                piecesBlack.draw(window)
                
            else:
                piecesWhite.draw(window)

            #fog.draw()
            window.blit(grid, (radius*2  ,radius*2 ))
            window.blit(units, (radius*2 , radius*2 ))
            #window.blit(fog, (0, 0))
            pygame.display.update()
            fpsClock.tick(10)
    finally:
        pygame.quit()
