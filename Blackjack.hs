-- fc59886,fc59808
module Blackjack (
    Baralho,
    converte,
    tamanho,
    EstadoJogo,
    inicializa,
    creditos,
    baralho,
    terminado,
    Estrategia,
    sempreStand,
    sempreHit,
    minhaEstrategia,
    simulaRonda,
    simulaJogo
) where
    
import BaralhosExemplo

-- A

type Baralho = [String]

-- A1

converte :: [String] -> Baralho
converte x = x :: Baralho

-- A2

tamanho :: Baralho -> Int
tamanho = length

-- B

data EstadoJogo = EstadoJogo {
    baralho :: Baralho,
    maoJogador :: [String],
    maoCasa :: [String],
    creditos :: Int,
    aposta :: Int,
    status :: String
} deriving (Eq)

-- B1, B2, B3

inicializa :: Baralho -> EstadoJogo
inicializa x = EstadoJogo {
    baralho = x,
    maoJogador = [],
    maoCasa = [],
    creditos = 100,
    aposta = 0,
    status = "em jogo"
}
-- B4

terminado :: EstadoJogo -> Bool
terminado x = semCreditos x || menosDeVinteCartas x

semCreditos :: EstadoJogo -> Bool
semCreditos x = creditos x <= 0

menosDeVinteCartas :: EstadoJogo -> Bool
menosDeVinteCartas x = tamanho (baralho x) <= 20

-- B5

instance Show EstadoJogo where
    show x = "jogador: " ++ show (maoJogador x) ++ "\ncasa: " ++ show (maoCasa x) ++ "\ncreditos: " ++ show (creditos x) ++ "\naposta: " ++ show (aposta x) ++ "\nstatus: " ++ show (status x)

-- C

data Estrategia = Estrategia {
    estrategiaCreditos :: Int,
    decisao :: EstadoJogo -> String,
    descricao :: String
}

-- C1

charInt :: Char -> Int
charInt x = read [x] :: Int

valorCarta :: String -> [Int]
valorCarta (x:_)
    | x == 'A' = [1, 11]
    | x `elem` ['T', 'J', 'Q', 'K'] = [10]
    | otherwise = [charInt x]

valorMao :: Baralho -> [Int]
valorMao [] = [0]
valorMao (x:xs) = [x+y | x <- valorCarta x, y <- valorMao xs]

menorValorMao :: Baralho -> Int
menorValorMao x = minimum (valorMao x)

maiorValorMao :: Baralho -> Int
maiorValorMao x = maximum (valorMao x)

sempreStand :: Estrategia
sempreStand = Estrategia {
    estrategiaCreditos = 5,
    decisao = \_ -> "stand",
    descricao = "sempre stand"
}

sempreHit :: Estrategia
sempreHit = Estrategia {
    estrategiaCreditos = 5,
    decisao = \x -> if menorValorMao (maoJogador x) < 21 then "hit" else "stand",
    descricao = "sempre hit"
}

{-
A nossa estratégia é apostar 10 créditos e fazer hit 
enquanto o menor valor da mão for menor que 15, caso contrário, fazer stand.
-}
minhaEstrategia :: Estrategia
minhaEstrategia = Estrategia {
    estrategiaCreditos = 10,
    decisao = \x -> if menorValorMao (maoJogador x) < 15 then "hit" else "stand",
    descricao = "hit enquanto o menor valor da mão for menor que 15, caso contrário, fazer stand"
}

-- C2

simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda x y =
    if jogadorRebentou j then derrotaJogador j
    else compararValores $ simulaJogadaCasa j
    where j = simulaJogadaJogador x $ apostarCreditos x $ distribuirCartas y

distribuirCartas :: EstadoJogo -> EstadoJogo
distribuirCartas x = x {
    baralho = drop 4 (baralho x),
    maoJogador = take 2 (baralho x),
    maoCasa = take 2 (drop 2 (baralho x))
}

apostarCreditos :: Estrategia -> EstadoJogo -> EstadoJogo
apostarCreditos x y = y {
    creditos = creditos y - estrategiaCreditos x,
    aposta = estrategiaCreditos x
}

simulaJogadaJogador :: Estrategia -> EstadoJogo -> EstadoJogo
simulaJogadaJogador x y
    | jogadorRebentou y = y
    | maoEvinteUm (maoJogador y) = y
    | decisao x y == "stand" = y
    | decisao x y == "hit" = simulaJogadaJogador x (hitJogador y)
    | otherwise = y

maoEvinteUm :: [String] -> Bool
maoEvinteUm x = 21 `elem` (valorMao x)

hitJogador :: EstadoJogo -> EstadoJogo
hitJogador x = x {
    baralho = tail (baralho x),
    maoJogador = maoJogador x ++ [head . baralho $ x]
}

simulaJogadaCasa :: EstadoJogo -> EstadoJogo
simulaJogadaCasa x
    | casaRebentou x = x
    | maoEvinteUm (maoCasa x) = x
    | maiorValorMenorIgualQue21 (maoCasa x) >= 17 = x
    | menorValorMao (maoCasa x) < 17 = simulaJogadaCasa (hitCasa x)
    | otherwise = x

hitCasa :: EstadoJogo -> EstadoJogo
hitCasa x = x {
    baralho = tail (baralho x),
    maoCasa = maoCasa x ++ [head . baralho $ x]
}

jogadorRebentou :: EstadoJogo -> Bool
jogadorRebentou x = rebentou (maoJogador x)

casaRebentou :: EstadoJogo -> Bool
casaRebentou x = rebentou (maoCasa x)

rebentou :: [String] -> Bool
rebentou x = menorValorMao x > 21

compararValores :: EstadoJogo -> EstadoJogo
compararValores x
    | menorValorMao (maoJogador x) > 21 = derrotaJogador x
    | menorValorMao (maoCasa x) > 21 = vitoriaJogador x
    | maiorValorMenorIgualQue21 (maoJogador x) > maiorValorMenorIgualQue21 (maoCasa x) = vitoriaJogador x
    | maiorValorMenorIgualQue21 (maoJogador x) < maiorValorMenorIgualQue21 (maoCasa x) = derrotaJogador x
    | otherwise = empate x

vitoriaJogador :: EstadoJogo -> EstadoJogo
vitoriaJogador x = x { 
    maoJogador = [],
    maoCasa = [],
    creditos = creditos x + (aposta x) * 2,
    aposta = 0,
    status = "vitoria do jogador"
}

derrotaJogador :: EstadoJogo -> EstadoJogo
derrotaJogador x = x {
    maoJogador = [],
    maoCasa = [],
    creditos = creditos x,
    aposta = 0,
    status = "derrota do jogador"
}

empate :: EstadoJogo -> EstadoJogo 
empate x = x { 
    maoJogador = [],
    maoCasa = [],
    creditos = creditos x + aposta x,
    aposta = 0,
    status = "empate"
}

maiorValorMenorIgualQue21 :: [String] -> Int
maiorValorMenorIgualQue21 xs = maximum (filter (<= 21) (valorMao xs))

-- C3

simulaJogo :: Estrategia -> Baralho -> Int
simulaJogo x y
    | terminado i = creditos i
    | otherwise = creditos (simulaJogo' x i)
    where i = inicializa y

simulaJogo' :: Estrategia -> EstadoJogo -> EstadoJogo
simulaJogo' x y
    | terminado ronda = ronda
    | otherwise = simulaJogo' x ronda
    where ronda = simulaRonda x y

-- C4

instance Show Estrategia where
    show x = "Estrategia: apostar sempre " ++ show (estrategiaCreditos x) ++ " créditos, fazer " ++ show (descricao x)
