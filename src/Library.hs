module Library where
import PdePreludat
import GHC.Num (Num)

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

--Punto 1

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {velocidad = 10, precision = precisionJugador habilidad * 2 , altura = 0}

madera :: Palo
madera habilidad  = UnTiro {velocidad = 100, precision = precisionJugador habilidad / 2, altura = 5}

hierro :: Number -> Palo
hierro n habilidad  = UnTiro {velocidad = n * fuerzaJugador habilidad, precision = precisionJugador habilidad / n, altura = (n-3) `max` 0}

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

--Punto 2

golpe :: Palo -> Jugador -> Tiro
golpe palo = palo . habilidad

--Punto 3

data Obstaculo = UnObstaculo {
    puedeSuperar :: Tiro -> Bool,
    efectoObstaculo :: Tiro -> Tiro
} deriving Show

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiro 
    |puedeSuperar obstaculo tiro = efectoObstaculo obstaculo tiro
    |otherwise = anularTiro tiro

tunel :: Obstaculo
tunel = UnObstaculo superaTunel efectoTunel

superaTunel :: Tiro -> Bool
superaTunel tiro = precision tiro > 90 && vaAlRasDelSuelo tiro
efectoTunel :: Tiro -> Tiro
efectoTunel tiro = tiro {velocidad = velocidad tiro *3, precision = 100, altura = 0}

laguna :: Number -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && altura tiro >= 1 && altura tiro <= 5
efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna largo tiro = tiro {velocidad = velocidad tiro , precision = precision tiro, altura = altura tiro / largo}

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = velocidad tiro >= 5 && velocidad tiro <= 20 && precision tiro > 95 && vaAlRasDelSuelo tiro
efectoHoyo :: Tiro -> Tiro
efectoHoyo = anularTiro

anularTiro :: Tiro -> Tiro
anularTiro tiro = tiro {velocidad = 0, precision = 0, altura = 0}

vaAlRasDelSuelo :: Tiro -> Bool
vaAlRasDelSuelo tiro = altura tiro == 0

--Punto 4

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe palo jugador)

cantidadObstaculosSuperados :: [Obstaculo] -> Tiro -> Number
cantidadObstaculosSuperados listaObstaculos tiro = length (takeWhile (flip puedeSuperar tiro) listaObstaculos)

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador listaObstaculos = maximoSegun (cantidadObstaculosSuperados listaObstaculos . flip golpe jugador ) palos

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b
