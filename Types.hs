{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

--identificadores 
newtype DisciplinaID = DisciplinaID Text deriving (Show, Eq, Ord)
newtype PPCID = PPCID Text deriving (Show, Eq, Ord)

-- os tipos de dados que representam as entidades do sistema
data EstadoDisciplina
    = Concluida
    | EmAndamento
    | Pendente
    | NaoInformada
    | Reprovada
    | Dispensada
    deriving (Show, Eq)

--regra
data Disciplina = Disciplina
    { idDisciplina :: DisciplinaID
    , nome :: Text
    , cargaHoraria :: Int
    , preRequisitos :: [DisciplinaID] --aresta
    } deriving (Show, Eq)

--toda a trajetŕoia q é feita por aluno
data Trajetoria = Trajetoria
    {ppcBase :: PPCID
    , semestreRef :: Int
    , estadoDisciplinas :: Map DisciplinaID EstadoDisciplina
    } deriving (Show, Eq)


--teste

--sem pre requisitos
d1 :: Disciplina
d1 = Disciplina (DisciplinaID "BCC001") "Fundamentos da Computação" 60 []

d2 :: Disciplina
d2 = Disciplina (DisciplinaID "BCC002") "Paradigmas da Computação" 30 [DisciplinaID "BCC001"]

d3 :: Disciplina
d3 = Disciplina (DisciplinaID "BCC003") "Programação Lógica e Funcional" 30 [DisciplinaID "BCC002"]

miniPPC :: [Disciplina]
miniPPC = [d1, d2, d3]

-- MATRIZ CORROMPIDA PARA TESTES DE GRAFO
dErro1 :: Disciplina
dErro1 = Disciplina (DisciplinaID "ERR1") "Materia A" 60 [DisciplinaID "ERR2"] -- A exige B

dErro2 :: Disciplina
dErro2 = Disciplina (DisciplinaID "ERR2") "Materia B" 60 [DisciplinaID "ERR3"] -- B exige C

dErro3 :: Disciplina
dErro3 = Disciplina (DisciplinaID "ERR3") "Materia C" 60 [DisciplinaID "ERR1"] -- C exige A (CICLO!)

matrizComCiclo :: [Disciplina]
matrizComCiclo = [dErro1, dErro2, dErro3]

minhaTrajetoria :: Trajetoria
minhaTrajetoria = Trajetoria
    {   ppcBase = PPCID "PPC001"
    ,     semestreRef = 2
    , estadoDisciplinas = Map.fromList
        [ (DisciplinaID "BCC001", Concluida)
        , (DisciplinaID "BCC002", Concluida)
        , (DisciplinaID "BCC003", Pendente)
        ]
    }



--o types é o vocabulário do meu código, como uma planta baixa de uma casa
--aqui eu ensino ao compilador quais são as regras fundamentais para meu sistema, antes mesmo de qualquer lógica
--na programação funcional é a técnica de desenvolvimento guiado por tipos, que basicamente diz que se eu definir os tipos de forma perfeita é quase impossivel escrever um código que faça algo errado depois
