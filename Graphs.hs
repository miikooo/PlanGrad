--gráfico direcionado aciclico dag

module Graph where

import Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

--tranformas o ppc num diciionarios para buscas mais rápidas
--chave matéria, o valor é a lista de pré requisitos
type Grafo = Map DisciplinaID [DisciplinaID]

construirGrafo :: [Disciplina] -> Grafo
construirGrafo matriz =
    --usa list comprehension para montar os pares de chave e valor
    Map.fromList [(idDisciplina d, preRequisitos d) | d <- matriz]

--recebe o grafo, faz um rastro e a materia atual, e vai ver se tem ciclo
buscaCiclo :: Grafo -> [DisciplinaID] -> DisciplinaID -> Bool
buscaCiclo grafo caminhoAtual idAtual 
    --pisei e já tem rastro meu? ent avisa que é ciclo
    | idAtual `elem` caminhoAtual = True
    |otherwise =  -- olhar pré requisitos
        case Map.lookup idAtual grafo of  
            Nothing -> False --não tem pré requisitos, não tem ciclo

            --se achar matéria se olha os pré requisitos
            Just preReqs ->
                --jogamos o rastro, se adiciona a matéria atual na lista de caminhos
                let novoCaminho = idAtual : caminhoAtual
                --a função any verifica se algum dos pré requisitos gera um ciclo
                in any (\req -> buscaCiclo grafo novoCaminho req) preReqs

--testa todas as matérias do ppc, se alguma delas gera ciclo ai barra tudo
temCiclo :: [Disciplina] -> Bool
temCiclo matriz =
    let grafo = construirGrafo matriz
        todasAsMaterias = map idDisciplina matriz
    in any (\idDisc -> buscaCiclo grafo [] idDisc) todasAsMaterias

--rece 4 coisas, o grafo, uma disciplina paraa visitar, um conjunto set das matérias visitadas e a lista acumulada com a ordem final, devolve as 3 e 4 atualizados
dfsTopoSort :: Grafo -> DisciplinaID -> (Set DisciplinaID, [DisciplinaID]) -> (Set DisciplinaID, [DisciplinaID])
dfsTopoSort grafo idAtual (visitados, ordemAcumulada)
    --se ja visitei, ignoro e devolvo
    | idAtual `Set.member` visitados = (visitados, ordemAcumulada)
    |otherwise = 
        let preReqs = Map.findWithDefault [] idAtual grafo
            --marca que esta visitando agora
            visitadosAgora = Set.insert idAtual visitados
            --foldl aplica a função dfsTopoSort em cada pŕe requisito e repassa o estado se esta visitado 
            (visitadosDepois, ordemDepois) = foldl (\acc req -> dfsTopoSort grafo req acc) (visitadosAgora, ordemAcumulada) preReqs
        --lista final
        in(visitadosDepois, ordemDepois ++ [idAtual])

--ordena uma lista qualquer de matérias respeitando a dependencia do curriculo
ordenacaoTopologica :: [Disciplina] -> [DisciplinaID]
ordenacaoTopologica matriz =
    let grafo = construirGrafo matriz
        todasAsMaterias = map idDisciplina matriz
        --começa o dfs vazio e uma o fodl para percorrer todas as meterias do curso
        (_, ordemFinal) = foldl (\acc disc -> dfsTopoSort grafo disc acc) (Set.empty, []) todasAsMaterias
    in ordemFinal
 
