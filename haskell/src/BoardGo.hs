{-|
    esse modulo exporta to os componentes usados pelo jogo Go
-}
module BoardGo(
    Point(Point),
    Stone(Black, White, Ko, Empty),
    getOppositeStone,
    Move(Pass, Move),
    getPiece,
    GameStatus(Alive, Dead, Over),
    GameType(Machine, Player),
    Game(Game),
    createGame,
    killGame,
    seekBoard,
    removeKo,
    removePiece,
    playPass,
    validMove,
    playMove,
    finishGame,
    getWinner
) where

import Data.Map as Map
import Data.List as List
import System.Random
import System.IO.Unsafe



-- | Esse tipo dedado representa um "ponto" no board (Ex: a interseção de duas linhas)
data Point = Point Int Int -- ^ O construtor do ponto, as coordenadas x e y
           deriving (Ord, Eq)

-- | Esse tipo de dado representa a "pedra" do jogo Go
data Stone = Black -- ^ para representar a pedra Preta
           | White -- ^ para representar a pedra Branca
           | Ko -- ^ 'Ko' Uma pedra especial que nao aparece no board. Colocado 
           | Empty -- ^ Representa o ponto vazio no tabuleiro
           deriving (Eq, Show)

-- | 'getOppositeStone' retorna a pedra oposta dada uma pedra especifica
getOppositeStone :: Stone -> Stone
getOppositeStone stone | stone == Black = White
                      | stone == White = Black
                      | otherwise = Empty

-- | Esse tipo de dado representa o 'Move' que pode ser realizado no tabuleiro pelo jogador 
data Move = Pass Stone -- ^ Jogador com essa 'Stone' pa
          | Move Point Stone -- ^ Jogador com essa 'Stone' joga no determinado 'Point'
          deriving (Eq)

-- | 'getPiece' retorna a "pedra" especifica para o movimento,(ex: se um jogador passar ou jogar)
getPiece :: Move -> Stone
getPiece (Pass st) = st
getPiece (Move  _ st) = st

-- | Esse tipo define o jogo 'Game'
data Game = Game {
    board :: Map Point Stone, -- ^ mapa do ponto a pedra
    lastMove :: Move, -- ^ o ultimo movimento jogado no jogo
    boardSize :: Int, -- ^ o tamanho do tabuleiro sendo possivel de 9 13 19
    scoreBlack :: Int, -- ^ pontuação da pedra preta
    scoreWhite :: Int, -- ^ pontuação da pedra branca
    status :: GameStatus, -- ^ O status do game,se ele está rodando ou acabou
    gameType :: GameType -- ^ O tipo do jogo,contra bot ou contra player
}

data GameType = Machine  -- ^ jogo contra a maquina
                | Player -- ^ jogo contra um jogador
                deriving (Eq)

-- | 'GameStatus' representa o estado do jogo se ele está vivo,morto ou terminou
data GameStatus = Alive -- ^ Jogo está executando
                | Dead -- ^ Jogo acabou e resultado final é contado
                | Over -- ^ Jogo acabou e resultado final é exibido
                deriving (Eq)

-- | cria o tabuleiro levando em conta o tamanho e o jogo
createGame :: Int -> Int -> Game
createGame size gameTypeInt = Game{
    board = addPieces (Map.empty) points,
    lastMove = Move (Point (-1) (-1)) White,
    boardSize = size,
    scoreBlack = 0,
    scoreWhite = 0,
    status = Alive,
    gameType = getGameTypeFromInt gameTypeInt
} where points = [(Point x y) | x <- [1..size], y <- [1..size]]

-- | adiciona um mapa vazio em cima do tabuleiro com peças "vazias"
addPieces :: (Map Point Stone) -> [Point] -> (Map Point Stone)
addPieces m [] = m
addPieces m (x:xs) = addPieces (Map.insert x Empty m) xs

-- | 'killGame' muda o status do jogo de 'Alive' para 'Dead' e aumenta a pontuação do 'Black'
-- se 'White' foi o ultimo a passar 'Pass'.
killGame :: Game -> Game
killGame game@(Game m lm s sb sw Alive gameType) = Game m lm s (sb+1) sw Dead gameType
killGame game = game

-- | 'removeKo' remove a peça ko do 'game'
removeKo :: Game -> Game
removeKo game@(Game m lm s b w status gameType) = (Game newBoard lm s b w status gameType)
    where pointsWithKo = List.filter (\(p, s) -> s == Ko) (assocs m)
          newBoard = if pointsWithKo == [] then m else addPiece m (fst (pointsWithKo !! 0)) Empty

-- | 'addPiece' adiona um elemento do tipo t no mapa
addPiece :: (Map Point t) -> Point -> t -> (Map Point t)
addPiece m point stone = Map.insert point stone (Map.delete point m)

-- | 'removePiece' remove uma pedra do jogo
removePiece :: (Map Point Stone) -> Point -> (Map Point Stone)
removePiece m point = addPiece m point Empty

-- | 'playPass' adiciona o comando 'Pass' movido pela 'Stone' no 'Game'
playPass :: Game -> Stone -> Game
playPass game@(Game board lm s sb sw status gameType) stone = Game {
    board = board,
    lastMove = Pass stone,
    boardSize = s,
    scoreBlack = newsb,
    scoreWhite = newsw,
    status = status,
    gameType = gameType
} where
    -- | acrescenta a pontuação da peca oposta
    newsb = if stone == Black then sb else (sb+1)
    newsw = if stone == White then sw else (sw+1)

-- | 'playMove' adiciona um 'Move' para  o 'Point' pela a 'Stone' no 'Game'.
-- apos a fazer uma jogada, tambem remove a pontuação da pedra oposta caso ela esteja presente
playMove :: Game -> Point -> Stone -> Game
playMove game@(Game board lm s sb sw status gameType) point stone =
    let ostone = getOppositeStone stone
        stoneColor = getPiece lm
        gamePassed = removeGroups Game {
            board = addPiece board point stone,
            lastMove = Move point stone,
            boardSize = s,
            scoreBlack = sb,
            scoreWhite = sw,
            status = status,
            gameType = gameType
            } point ostone
    in if gameType == Machine && stoneColor == White 
        then
            machinePlay gamePassed
        else gamePassed

-- | 'removeGroups' remove pedras capturadas do tipo 'Stone' do 'Game' comecando pelo
-- 'Point' e movendo para todas as quatro direcoes
removeGroups :: Game -> Point -> Stone -> Game
removeGroups game@(Game board lm s sb sw status gameType) point@(Point x y) stone
    -- Se o numero de pontos = 1 capturados e um entao checa se  Ko e formado e adiciona Ko no tabuleiro se neccesario
    | length pointsToBeRemoved == 1 && isKo = updateScore (addKo (removeStones game pointsToBeRemoved) (pointsToBeRemoved !! 0)) 1 stone'
    | otherwise = updateScore (removeStones game pointsToBeRemoved) (length pointsToBeRemoved) stone'
    where (up, down, left, right) = getAdj point
          removeUp = removeDead up stone game
          removeDown = removeDead down stone game
          removeLeft = removeDead left stone game
          removeRight = removeDead right stone game
          pointsToBeRemoved = removeUp ++ removeDown ++ removeLeft ++ removeRight
          point' = if length removeUp == 1 then up else if length removeDown == 1 then down else if length removeLeft == 1 then left else right
          (up', down', left', right') = getAdj point'
          stone' = getOppositeStone stone
          isKo = length ((removeDead up' stone' game) ++ (removeDead down' stone' game) ++ (removeDead left' stone' game) ++ (removeDead right' stone' game)) == 1

-- | 'getAdj' pega os pontos adjacentes ao 'point' passado
getAdj :: Point -> (Point, Point, Point, Point)
getAdj (Point x y) = (Point x (y+1), Point x (y-1), Point (x-1) y, Point (x+1) y)

-- | 'removeDead' retorna o grupo morto de 'Stone' no 'Game' começando do 'Point'
removeDead :: Point -> Stone -> Game -> [Maybe Point]
removeDead point stone game | checkIfTrapped game point stone = removablePoints
                            | otherwise = []
                            where removablePoints = (findTrappedGroup game point stone [])

-- | 'updateScore' atualiza o placar da 'Stone' pelo 'Int' passado
updateScore :: Game -> Int -> Stone -> Game
updateScore game@(Game m lm s b w status gameType) p st
    | st == Black = (Game m lm s (b+p) w status gameType)
    | otherwise = (Game m lm s b (w+p) status gameType)

-- | 'addKo' adiciona 'Ko' ao jogo a partir de tal ponto
addKo :: Game -> (Maybe Point) -> Game
addKo game@(Game m lm s b w status gameType) (Just p) = (Game (addPiece m p Ko) lm s b w status gameType)

-- | 'removeStones' dada uma lista de pontos remove todas a pecas do tabuleiro passadas
removeStones :: Game -> [Maybe Point] -> Game
removeStones game@(Game m lm _ _ _ status gameType) [] = game
removeStones game@(Game m lm s b w status gameType) (Nothing:xs) = game
removeStones game@(Game m lm s b w status gameType) ((Just p):xs) = removeStones (Game (removePiece m p) lm s b w status gameType) xs

-- | 'seekBoard' retorna uma 'Stone' em um 'Point' no 'Game'
seekBoard :: Game -> Point -> Stone
seekBoard (Game m _ _ _ _ _ _) p = case Map.lookup p m of
    Just stone -> stone
    Nothing -> Empty

-- | essa data representa se uma peca pertence ao tipo black white ou nenhum
data Status = Seen | Unseen | SeenW | SeenB | None deriving (Eq)

-- | 'seekMap' retorna 'Status' do 'Point' do mapa passado
seekMap :: (Map Point Status) -> Point -> Status
seekMap m p = case Map.lookup p m of
    Just s -> s
    Nothing -> Unseen

-- | 'findTrappedGroup' acha um grupo de pecas presas em um jogo
-- comecando de um ponto e indo para todas as direções
findTrappedGroup :: Game -> Point -> Stone -> [Maybe Point] -> [Maybe Point]
findTrappedGroup game@(Game m move@(Move pt st) boardSize _ _ _ _) point@(Point x y) stone seenPoints
    | x < 1 || x > boardSize || y < 1 || y > boardSize  = seenPoints
    | elem (pure point) seenPoints = seenPoints
    | seekBoard game point == Empty = Nothing:seenPoints
    | seekBoard game point == Ko = Nothing:seenPoints
    | seekBoard game point /= stone = seenPoints
    | otherwise = findTrappedGroup game left stone
        $ findTrappedGroup game right stone
        $ findTrappedGroup game up stone
        $ findTrappedGroup game down stone ((pure point):seenPoints)
    where up = Point x (y+1)
          down = Point x (y-1)
          right = Point (x+1) y
          left = Point (x-1) y

-- | 'findTerritory' acha um territorio a partir de um ponto no jogo
-- comecando de um ponto e indo para as quatro direcoes
findTerritory :: Game -> Point -> Stone -> ((Map Point Status), [Maybe Point]) -> ((Map Point Status), [Maybe Point])
findTerritory game@(Game _ _ boardSize _ _ _ _) point@(Point x y) stone (m, points)
    | x < 1 || x > boardSize || y < 1 || y > boardSize = (m, points) -- Se o ponto está fora do mapa
    | elem (pure point) points = (m, points) -- Se o ponto já foi visitado
    | seekBoard game point == getOppositeStone stone = (m, Nothing:points) -- Se esse ponto tem pedra do outro jogador
    | seekBoard game point == stone = (m, points) -- Se esse ponto contém alguma pedra
    | otherwise = findTerritory game left stone
        $ findTerritory game right stone
        $ findTerritory game up stone
        $ findTerritory game down stone ((Map.insert point Seen m), ((pure point):points)) -- De outra forma recupera todas as pedras adjacentes
    where (up, down, left, right) = getAdj point

-- | 'findTerritories' acha os territorios tanto do black quanto do white em um jogo
findTerritories :: Game -> Point -> (Map Point Status) -> (Map Point Status)
findTerritories game point m
    | seekBoard game point /= Empty && seekBoard game point /= Ko = addPiece m point None -- se um ponto e Ko ou nenhum
    | seekMap m point == SeenW = m -- se pertence ao territorio do White
    | seekMap m point == SeenB = m -- se pertence ao territorio do Black
    | seekMap m point == None = m -- se foi visto e nao pertence a ninguem
    | otherwise = if elem Nothing (snd tw) then
                      if elem Nothing (snd tb) then setInMap m (snd tb) None
                      else setInMap m (snd tb) SeenB
                  else setInMap m (snd tw) SeenW
    where tb = findTerritory game point Black (m, [])
          tw = findTerritory game point White (m, [])

-- | 'setInMap' seta o status de um ponto no mapa
setInMap :: (Map Point Status) -> [Maybe Point] -> Status -> (Map Point Status)
setInMap m [] st = m
setInMap m (point:points) st | point /= Nothing = setInMap (addPiece m (purify point) st) points st
                             | otherwise = setInMap m points st

-- | 'findAllTerritoriesOfPoints' calcula todos os territorios dada uma lista de pontos
findAllTerritoriesOfPoints :: Game -> [Point] -> (Map Point Status) -> (Map Point Status)
findAllTerritoriesOfPoints game [] m = m
findAllTerritoriesOfPoints game (point:points) m = findAllTerritoriesOfPoints game points (findTerritories game point m)

-- | 'findTerritories' cria uma lista de pontos e acha todos os territorios dessa lista do jogo
findAllTerritories :: Game -> (Map Point Status)
findAllTerritories game@(Game _ _ boardSize _ _ _ _) = findAllTerritoriesOfPoints game points Map.empty
    where points = [(Point x y) | x <- [1..boardSize], y <- [1..boardSize]]

-- | 'finishGame' acha todos territorios capturados no 'Game' e atualiza a pontuacao de dada 'Stone'
finishGame :: Game -> Game
finishGame game@(Game m lm s sb sw status gameType) = Game m lm s sb' sw' Over gameType
    where sb' = sb + countSeenB t s
          sw' = sw + countSeenW t s
          t = findAllTerritories game

-- | 'countSeenB' conta os pontos que pertencem a peca 'Black'
countSeenB :: (Map Point Status) -> Int -> Int
countSeenB m size | l == size*size = 0 -- Se o tabuleiro esta vazio entao nao pode ser um territorio
                  | otherwise = l
                  where l = length $ List.filter (\(p,s) -> s == SeenB) (assocs m)

-- | 'countSeenW' conta os pontos que pertencem a peca 'White'
countSeenW :: (Map Point Status) -> Int -> Int
countSeenW m size | l == size*size = 0 -- Se o tabuleiro esta vazio entao nao pode ser um territorio
                  | otherwise = l
                  where l = length $ List.filter (\(p,s) -> s == SeenW) (assocs m)
-- | 'purify' muda do tipo Maybe Type para Type
purify :: Maybe a -> a
purify (Just a) = a

-- | 'validMove' checa se dado um movimento de um 'Point' para dada pedra 'Stone' e valida ou nao
validMove :: Game -> Point -> Stone -> Bool
validMove game@(Game m lm s _ _ _ _) p@(Point x y) st | x < 1 || x > s || y < 1 || y > s = False
    | seekBoard game p /= Empty = False -- Se o ponto nao e vazio
    | not $ checkIfTrapped game1 p st = True -- se o ponto nao esta capturado entao o movimento e valido
    -- se ponto esta capurado entao checa se enquanto se coloca uma peca se a peca oposta sera capturada ou nao 
    | (seekBoard game up == ostone) && (checkIfTrapped game1 up ostone) = True
    | (seekBoard game down == ostone) && (checkIfTrapped game1 down ostone) = True
    | (seekBoard game left == ostone) && (checkIfTrapped game1 left ostone) = True
    | (seekBoard game right == ostone) && (checkIfTrapped game1 right ostone) = True
    | otherwise = False
    where game1 = playMove game p st
          ostone = getOppositeStone st
          up = Point x (y+1)
          down = Point x (y-1)
          right = Point (x+1) y
          left = Point (x-1) y

-- | 'checkIfTrapped' checa se uma 'Stone' esta capturada ou nao
checkIfTrapped :: Game -> Point -> Stone -> Bool
checkIfTrapped game p st = not $ elem Nothing (findTrappedGroup game p st [])

-- | 'checkIfNothing' checa se 'Maybe Point' eh 'Nothing'
checkIfNothing :: (Maybe Point) -> Bool
checkIfNothing Nothing = True
checkIfNothing (Just point) = False

-- | 'getWinner' declara o vencedor dependendo da pontuacao dos dois
getWinner :: Game -> String
getWinner game@(Game _ _ _ sb sw _ _)
    | sb > sw = "Black wins." -- Se placar de 'Black' for maior que 'White'
    | sw > sb = "White wins." -- Se placar de 'White' for maior que 'Black'
    | otherwise = "Game Draws" -- Se está empatado

getGameTypeFromInt :: Int -> GameType
getGameTypeFromInt number
    | number == 1 = Player
    | number == 2 = Machine

-- | 'machinePlay' modo de jogo onde se ativa a ia para jogar contra a maquina
machinePlay :: Game -> Game
machinePlay game = playMove game point White
    where point = getAnRandomEmptyPoint game

getAnRandomEmptyPoint :: Game -> Point
getAnRandomEmptyPoint game = point
    where point = Point getAnRandom getAnRandom

-- if validMove game point stone then point
--     else getAnRandomEmptyPoint game point stone

getAnRandom :: Int
-- getAnRandom = randomNumber where randomNumber = getIntFromIo randomRIO (1,9)
getAnRandom = unsafePerformIO (getStdRandom (randomR (1,9)))

{- getAnotherRandom :: Int -> IO
getAnotherRandom n = do
    r <- randomRIO(1,9)
    putStrLn r
    return r -}

-- getAnIntFromIo :: IO -> Int
-- getAnIntFromIo io = inte
--     where inte = read io
