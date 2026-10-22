--lógica de elegibilidade
--regra de negócio: uma disciplina é elegível se todos os seus pré-requisitos foram concluídos

module Eligibility where
import Types
import qualified Data.Map as Map

--minha função auxiliar que verifica o status de somente uma matéria
--vai procurar o ID da disciplina no dicionário de trajetórioa
foiCumprida :: DisciplinaID -> Trajetoria -> Bool
foiCumprida idDisc traj = 
--map.lookup procura a chave no dicionário. pode achar Just ou Nothing
    case Map.lookup idDisc (estadoDisciplinas traj) of
        Just Concluida -> True
        Just Dispensada -> True
        _ -> False --qualquer outro da falso

--função principaa
--ve se todos os pŕe requisitos passam da função auxiliar
estaLiberada :: Disciplina -> Trajetoria -> Bool
estaLiberada disciplina traj =
    --all verifica se uma condição é verdadeira para todos os itens de um elemento
     all (\reqId -> foiCumprida reqId traj) (preRequisitos disciplina)

--função auxiliar que descobre o status exato de uma materia
statusAtual :: DisciplinaID -> Trajetoria -> Maybe EstadoDisciplina
statusAtual idDisc traj = 
    Map.lookup idDisc (estadoDisciplinas traj)

--regra de negócio, posso me matricular hoje?
podeCursar :: Disciplina -> Trajetoria -> Bool
podeCursar disciplina traj =
    let status = statusAtual (idDisciplina disciplina) traj
    --so conta se for concluida ou dispensada
        jaTerminou = status == Just Concluida || status == Just Dispensada
        --esta sendo feita agr?
        estaFazendo = status == Just EmAndamento
        --os prereqs tão ok?
        preReqsOk = estaLiberada disciplina traj
    in
        not jaTerminou && not estaFazendo && preReqsOk

disciplinasElegiveis :: [Disciplina] -> Trajetoria -> [Disciplina]
disciplinasElegiveis matriz traj =
    --filter vai testa e retorna com a resposta
    filter (\disc -> podeCursar disc traj) matriz

