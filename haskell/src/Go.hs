{-| 
    esse modulo tem apenas 'playGo' que chama o 
    usuario para perguntar o tamanho do tabuleiro, entao cria uma janela com o jogo.
-}
module Go(playGo) where

import Data.Map
import BoardGo
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | 'PlayGo' funcoes que perguntamo tamanho do tabuleiro, cria um 'Game' object e comeca a janela do jogo
playGo :: IO ()
playGo = do
    putStrLn "Por favor digite o tamanho do tabuleiro - 9 13 ou 19"
    sizeString <- getLine
    let size = read sizeString
    -- Se o tamanho do tabuleiro for invalido,pergunta denovo
    if size /= 9 && size /= 13 && size /= 19 then playGo else do
        putStrLn $ "Digite 1 para PvP."
        putStrLn $ "Digite 2 para PvC"
        gameTypeString <- getLine
        let gameType = read gameTypeString
        let game = createGame size gameType
        let window = InWindow "Go Window" (20 * size + 180, 20 * size +240) (10, 10)
        play window (dark yellow) 0 game render handleEvent (\_ y -> y)

-- | 'handleEvent' lida com os cliques do mouse e do tabuleiro
handleEvent :: Event -> Game -> Game
-- ^ se dado um ponto clicado no jogo em um jogo 'alive', entao checa se o movimento e valido e joga
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game _ lm s _ _ Alive _)
    | validMove game p st = playMove (removeKo game) p st
    where p = BoardGo.Point (round ((x + 10*(fromIntegral s+1))/20)) (round ((-1*y + 10*(fromIntegral s+1))/20))
          st = getOppositeStoneFromLastMove lm
-- ^ Se p e pressionado no teclado em um jogo alive entao passa
handleEvent (EventKey (Char 'p') Down _ _) game@(Game _ lm s _ _ Alive _)
    | lm == Pass Black = killGame game
    | otherwise = playPass (removeKo game) st
    where st = getOppositeStoneFromLastMove lm
-- ^ If clicked on point on dead game then take the point as part of hopeless string
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game m lm s sb sw Dead gameType) = Game (removePiece m (BoardGo.Point x' y')) lm s sb' sw' Dead gameType
    where x' = round ((x + 10*(fromIntegral s+1))/20)
          y' = round ((-1*y + 10*(fromIntegral s+1))/20)
          sb' = if seekBoard game (BoardGo.Point x' y') == White then sb+1 else sb
          sw' = if seekBoard game (BoardGo.Point x' y') == Black then sw+1 else sw
-- ^ Se pressionar e em um grupo morto acaba o jogo
handleEvent (EventKey (Char 'e') Down _ _) game@(Game _ lm s _ _ Dead _) = finishGame game
-- ^ Ignora outros eventos
handleEvent _ game = game

-- | Essa função retorna o movimento oposto ao da pedra
getOppositeStoneFromLastMove :: Move -> Stone
getOppositeStoneFromLastMove (Move _ st) = getOppositeStone st
getOppositeStoneFromLastMove (Pass st) = getOppositeStone st

-- | Função 'render' gera quadro que vai ser exibido na janela
render :: Game -> Picture
render game@(Game m lm s sb sw status gameType) = pictures [ (pictures gameHorizontalLines), (pictures gameVerticalLines), (pictures stones),  scoreBlack, scoreWhite, statusText]
    where -- Linhas do tabuleiro
          gameHorizontalLines = [line [(fromIntegral (10 - 10*s),fromIntegral (10*s - 10 - 20*x)), (fromIntegral (20*s - 10 - 10*s),fromIntegral (10*s - 10 - 20*x))] | x <- [0..(s-1)]]
          gameVerticalLines = [line [(fromIntegral (10*s - 10 - 20*x),fromIntegral (10 - 10*s)), (fromIntegral (10*s - 10 - 20*x),fromIntegral (-10*s + 20*s - 10))] | x <- [0..(s-1)]]
          -- Todas as pedras no tabuleiro
          stones = [translate (fromIntegral (x*20 - 10*s-10)) (fromIntegral (10*s - y*20 + 10)) $ color c $ circleSolid 8 | (BoardGo.Point x y, st) <- (assocs m) , c <- [black,white], ((st == Black && c == black) ||  (st == White && c == white))]
          -- Placar das pedras pretas
          scoreBlack = translate 0 (fromIntegral (10*s + 40)) $ scale 0.1 0.1 $ text $ "Black's Score : " ++ (show sb)
          -- Placar das pedras brancas
          scoreWhite = translate 0 (fromIntegral (10*s + 60)) $ scale 0.1 0.1 $ text $ "White's Score : " ++ (show sw)
          -- Mostra turno se o jogo está vivo, instruções de captura se o jogo está morto e vencedor se o jogo acabou.
          statusText = if status == Alive then translate (fromIntegral (-10*s)) (fromIntegral (-10*s - 60)) $ scale 0.2 0.2 $ text $ (show $ getOppositeStone (getPiece lm)) ++ "'s turn"
                       else if status == Dead then translate (fromIntegral (-10*s)) (fromIntegral (-10*s - 60)) $ scale 0.1 0.1 $ text "Click on hopeless stones and enter e"
                       else translate (fromIntegral (-10*s)) (fromIntegral (-10*s - 60)) $ scale 0.2 0.2 $ text $ getWinner game
