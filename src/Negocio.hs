module Negocio
  ( inserirPassageiro, inserirPassageiroDados, listarPassageiros, proximoId
  , inserirCompanhia, inserirCompanhiaNome, listarCompanhias
  , listarVoos, inserirVoo, inserirVooDados, buscarCompanhiaPorId
  , buscarPassageiroPorId, buscarVooPorId
  , listarReservas
  , criarReserva
  , confirmarReserva
  , cancelarReserva
  , listarReservasPorPassageiro
  , listarReservasPorVoo
  ) where


import Tipos
import Data.Char (isSpace)

-- | Remove espaços do início e fim
-- | Remove espaços em branco no início e no fim de uma string.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Gera próximo ID de uma lista (maior + 1, começa em 1)
--   dado um campo que fornece o ID atual.
-- Se a lista estiver vazia, retorna 1.
-- Caso contrário, retorna (máximo dos IDs + 1).
proximoId :: [a] -> (a -> Int) -> Int
proximoId xs f =
  if null xs then 1 else maximum (map f xs) + 1

-- ====== PASSAGEIROS ======

-- | Lista todos os passageiros (útil para o menu)
listarPassageiros :: Sistema -> [Passageiro]
listarPassageiros = passageiros

-- | Inserção com struct pronto (ignora o id passado e gera um novo)
inserirPassageiro :: Passageiro -> Sistema -> Either String Sistema
inserirPassageiro p sys =
  inserirPassageiroDados (nome p) (documento p) sys

-- | Inserção a partir de nome/documento (validações simples)
inserirPassageiroDados :: String -> String -> Sistema -> Either String Sistema
inserirPassageiroDados nomeRaw docRaw sys =
  let nome' = trim nomeRaw
      doc'  = trim docRaw
  in
    if null nome'
       then Left "Nome não pode ser vazio."
    else if null doc'
       then Left "Documento não pode ser vazio."
    else if any (\x -> documento x == doc') (passageiros sys)
       then Left "Já existe um passageiro com esse documento."
    else
      let novoId = proximoId (passageiros sys) idPassageiro
          novoP  = Passageiro novoId nome' doc'
      in Right sys { passageiros = passageiros sys ++ [novoP] }


-- ====== COMPANHIAS ======

-- | Lista todas as companhias (para o menu)
listarCompanhias :: Sistema -> [Companhia]
listarCompanhias = companhias

-- | Inserção com struct (ignora id passado e gera um novo)
inserirCompanhia :: Companhia -> Sistema -> Either String Sistema
inserirCompanhia c sys = inserirCompanhiaNome (nomeCompanhia c) sys

-- | Inserção a partir do nome (validações simples)
inserirCompanhiaNome :: String -> Sistema -> Either String Sistema
inserirCompanhiaNome nomeRaw sys =
  let nome' = trim nomeRaw
  in if null nome'
        then Left "Nome da companhia não pode ser vazio."
     else if any (\x -> nomeCompanhia x == nome') (companhias sys)
        then Left "Já existe uma companhia com esse nome."
     else
       let novoId = proximoId (companhias sys) idCompanhia
           novaC  = Companhia novoId nome'
       in Right sys { companhias = companhias sys ++ [novaC] }

-- ====== UTILS COM COMPANHIAS ======
buscarCompanhiaPorId :: Int -> Sistema -> Maybe Companhia
buscarCompanhiaPorId cid sys = case filter (\c -> idCompanhia c == cid) (companhias sys) of
  (c:_) -> Just c
  _     -> Nothing

-- ====== VOOS ======

-- | Lista todos os voos (para o menu)
listarVoos :: Sistema -> [Voo]
listarVoos = voos

-- | Inserção com struct pronto (ignora id passado e gera novo).
--   Valida se a companhia existe.
inserirVoo :: Voo -> Sistema -> Either String Sistema
inserirVoo v sys = inserirVooDados (origem v) (destino v) (horario v) (idComp v) sys

-- | Inserção a partir dos campos (com validações simples)
inserirVooDados :: String -> String -> String -> Int -> Sistema -> Either String Sistema
inserirVooDados origemRaw destinoRaw horarioRaw idCompanhiaRef sys =
  let o = trim origemRaw
      d = trim destinoRaw
      h = trim horarioRaw
  in
    if null o then Left "Origem não pode ser vazia."
    else if null d then Left "Destino não pode ser vazio."
    else if null h then Left "Horário não pode ser vazio."
    else case buscarCompanhiaPorId idCompanhiaRef sys of
      Nothing -> Left "Companhia informada não existe."
      Just _  ->
        let novoId = proximoId (voos sys) idVoo
            novoV  = Voo novoId o d h idCompanhiaRef
        in Right sys { voos = voos sys ++ [novoV] }

-- ====== UTILS: BUSCAS ======
buscarPassageiroPorId :: Int -> Sistema -> Maybe Passageiro
buscarPassageiroPorId pid sys = case filter (\p -> idPassageiro p == pid) (passageiros sys) of
  (p:_) -> Just p
  _     -> Nothing

buscarVooPorId :: Int -> Sistema -> Maybe Voo
buscarVooPorId vid sys = case filter (\v -> idVoo v == vid) (voos sys) of
  (v:_) -> Just v
  _     -> Nothing



-- ====== RESERVAS ======

-- | Lista todas as reservas
listarReservas :: Sistema -> [Reserva]
listarReservas = reservas

-- | Lista reservas de um passageiro específico
listarReservasPorPassageiro :: Int -> Sistema -> [Reserva]
listarReservasPorPassageiro pid sys =
  filter (\r -> idPass r == pid) (reservas sys)

-- | Lista reservas de um voo específico
listarReservasPorVoo :: Int -> Sistema -> [Reserva]
listarReservasPorVoo vid sys =
  filter (\r -> idVooRef r == vid) (reservas sys)

-- | Cria uma reserva Pendente para (passageiro, voo)
-- Regras:
--  - passageiro e voo devem existir
--  - não permitir reserva duplicada (mesmo passageiro + mesmo voo) em status não-cancelado
criarReserva :: Int -> Int -> Sistema -> Either String Sistema
criarReserva pid vid sys = do
  case (buscarPassageiroPorId pid sys, buscarVooPorId vid sys) of
    (Nothing, _) -> Left "Passageiro inexistente."
    (_, Nothing) -> Left "Voo inexistente."
    (Just _, Just _) ->
      let conflito = any (\r -> idPass r == pid && idVooRef r == vid && status r /= Cancelada) (reservas sys)
      in if conflito
           then Left "Já existe uma reserva ativa para este passageiro neste voo."
           else
             let novoId = proximoId (reservas sys) idReserva
                 novaR  = Reserva novoId pid vid Pendente
             in Right sys { reservas = reservas sys ++ [novaR] }

-- | Confirma uma reserva Pendente
confirmarReserva :: Int -> Sistema -> Either String Sistema
confirmarReserva rid sys =
  case break (\r -> idReserva r == rid) (reservas sys) of
    (_, []) -> Left "Reserva não encontrada."
    (antes, r:depois) ->
      case status r of
        Pendente   ->
          let r' = r { status = Confirmada }
          in Right sys { reservas = antes ++ (r':depois) }
        Confirmada -> Left "Reserva já está confirmada."
        Cancelada  -> Left "Não é possível confirmar uma reserva cancelada."

-- | Cancela uma reserva (Pendente ou Confirmada -> Cancelada)
cancelarReserva :: Int -> Sistema -> Either String Sistema
cancelarReserva rid sys =
  case break (\r -> idReserva r == rid) (reservas sys) of
    (_, []) -> Left "Reserva não encontrada."
    (antes, r:depois) ->
      case status r of
        Cancelada  -> Left "Reserva já está cancelada."
        _          ->
          let r' = r { status = Cancelada }
          in Right sys { reservas = antes ++ (r':depois) }

