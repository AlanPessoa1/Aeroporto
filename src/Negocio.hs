-- Este módulo é o “cérebro” do sistema Aeroporto.
-- Ele contém toda a lógica funcional relacionada a:
--   - criação e validação de passageiros, companhias e voos
--   - buscas (consultas por critérios)
--   - manipulação de reservas (criar, confirmar, cancelar, listar)
--
-- O Menus.hs apenas chama as funções deste módulo — ele não decide
-- regras de negócio, apenas exibe os resultados. Isso segue o princípio
-- da separação de responsabilidades (UI ≠ lógica).
--
-- Todas as funções são puras, exceto quando retornam `Either String Sistema`,
-- o que permite indicar erros de validação sem causar exceções.
module Negocio
  ( inserirPassageiro, inserirPassageiroDados, listarPassageiros, proximoId
  , inserirCompanhia, inserirCompanhiaNome, listarCompanhias
  , editarCompanhia, excluirCompanhia
  , listarVoos, inserirVoo, inserirVooDados, buscarCompanhiaPorId
  , editarVoo, excluirVoo
  , buscarPassageiroPorId, buscarVooPorId
  , buscarVoosPorOrigem, buscarVoosPorDestino, buscarVoosPorRota
  , listarReservas
  , criarReserva
  , confirmarReserva
  , cancelarReserva
  , listarReservasPorPassageiro
  , listarReservasPorVoo
  , listarReservasPorStatus
  , inserirUsuario
  , listarUsuarios
  , editarUsuario
  , excluirUsuario
  , buscarUsuarioPorId
  ) where

import Tipos
import Data.Char (isSpace, toLower)
import Data.List (isInfixOf)


-- ===============================================================
--                   FUNÇÕES GENÉRICAS DE APOIO
-- ===============================================================

-- | Remove espaços em branco no início e no fim de uma string.
--   Essa função é útil para limpar entradas do usuário,
--   evitando erros de validação por espaços desnecessários.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


-- | Calcula o próximo ID para uma nova entidade (genérico).
--   Recebe uma lista de elementos e uma função que extrai o ID atual.
--   Retorna o maior ID + 1, ou 1 se a lista estiver vazia.
--
--   Exemplo:
--     proximoId [Passageiro 1 "A" "123", Passageiro 2 "B" "456"] idPassageiro
--     → 3
--
--   Essa abordagem garante unicidade dos identificadores sem depender
--   de estado global ou banco de dados.
proximoId :: [a] -> (a -> Int) -> Int
proximoId xs f =
  if null xs then 1 else maximum (map f xs) + 1


-- ===============================================================
--                         PASSAGEIROS
-- ===============================================================

-- | Retorna todos os passageiros do sistema.
--   É usada, por exemplo, em menus de listagem e relatórios.
listarPassageiros :: Sistema -> [Passageiro]
listarPassageiros = passageiros


-- | Insere um passageiro já estruturado (com nome e identificador),
--   mas ignora o ID recebido e gera um novo ID automaticamente.
inserirPassageiro :: Passageiro -> Sistema -> Either String Sistema
inserirPassageiro p sys =
  inserirPassageiroDados (nome p) (identificador p) sys


-- | Insere um novo passageiro a partir de nome e identificador.
--   Valida campos obrigatórios e impede duplicação de identificadores.
inserirPassageiroDados :: String -> String -> Sistema -> Either String Sistema
inserirPassageiroDados nomeRaw idRaw sys =
  let nome' = trim nomeRaw
      id'   = trim idRaw
  in
    if null nome'
       then Left "Nome não pode ser vazio."
    else if null id'
       then Left "Identificador não pode ser vazio."
    else if any (\x -> identificador x == id') (passageiros sys)
       then Left "Já existe um passageiro com esse identificador."
    else
      let novoId = proximoId (passageiros sys) idPassageiro
          novoP  = Passageiro novoId nome' id'
      in Right sys { passageiros = passageiros sys ++ [novoP] }


-- ===============================================================
--                         COMPANHIAS
-- ===============================================================

-- | Retorna todas as companhias aéreas cadastradas.
listarCompanhias :: Sistema -> [Companhia]
listarCompanhias = companhias


-- | Insere uma companhia a partir de uma estrutura já existente.
inserirCompanhia :: Companhia -> Sistema -> Either String Sistema
inserirCompanhia c sys = inserirCompanhiaNome (nomeCompanhia c) sys


-- | Insere uma companhia aérea apenas com o nome.
--   Realiza validação básica e impede duplicidade.
--   Valida limite máximo de companhias (5).
inserirCompanhiaNome :: String -> Sistema -> Either String Sistema
inserirCompanhiaNome nomeRaw sys =
  let nome' = trim nomeRaw
  in if length (companhias sys) >= 5
        then Left "Limite de companhias atingido (maximo: 5)."
     else if null nome'
        then Left "Nome da companhia não pode ser vazio."
     else if any (\x -> nomeCompanhia x == nome') (companhias sys)
        then Left "Já existe uma companhia com esse nome."
     else
       let novoId = proximoId (companhias sys) idCompanhia
           novaC  = Companhia novoId nome'
       in Right sys { companhias = companhias sys ++ [novaC] }


-- | Busca uma companhia pelo seu ID (retorna Maybe).
--   Retorna `Just Companhia` se encontrada, ou `Nothing` se não.
buscarCompanhiaPorId :: Int -> Sistema -> Maybe Companhia
buscarCompanhiaPorId cid sys =
  case filter (\c -> idCompanhia c == cid) (companhias sys) of
    (c:_) -> Just c
    _     -> Nothing

-- | Edita o nome de uma companhia existente
editarCompanhia :: Int -> String -> Sistema -> Either String Sistema
editarCompanhia cid novoNomeRaw sys =
  let novoNome = trim novoNomeRaw
  in if null novoNome
        then Left "Nome da companhia não pode ser vazio."
     else if any (\c -> nomeCompanhia c == novoNome && idCompanhia c /= cid) (companhias sys)
        then Left "Já existe uma companhia com esse nome."
     else case break (\c -> idCompanhia c == cid) (companhias sys) of
       (_, []) -> Left "Companhia não encontrada."
       (antes, c:depois) ->
         let c' = c { nomeCompanhia = novoNome }
         in Right sys { companhias = antes ++ (c':depois) }

-- | Exclui uma companhia (apenas se não houver voos associados)
excluirCompanhia :: Int -> Sistema -> Either String Sistema
excluirCompanhia cid sys =
  case buscarCompanhiaPorId cid sys of
    Nothing -> Left "Companhia não encontrada."
    Just _ ->
      let voosAssociados = filter (\v -> idComp v == cid) (voos sys)
      in if not (null voosAssociados)
           then Left "Não é possível excluir companhia com voos cadastrados."
           else Right sys { companhias = filter (\c -> idCompanhia c /= cid) (companhias sys) }


-- ===============================================================
--                         VOOS
-- ===============================================================

-- | Lista todos os voos disponíveis no sistema.
listarVoos :: Sistema -> [Voo]
listarVoos = voos


-- | Insere um voo já pronto (estrutura completa),
--   mas força a geração de um novo ID para manter a consistência.
--   Essa função chama 'inserirVooDados', que faz as validações.
inserirVoo :: Voo -> Sistema -> Either String Sistema
inserirVoo v sys =
  inserirVooDados (origem v) (destino v) (horario v) (idComp v) (capacidade v) sys


-- | Insere um voo a partir de seus campos básicos.
--   Verifica se a companhia existe antes de cadastrar.
--   Valida limite máximo de voos (20).
inserirVooDados :: String -> String -> String -> Int -> Int -> Sistema -> Either String Sistema
inserirVooDados origemRaw destinoRaw horarioRaw idCompanhiaRef cap sys =
  let o = trim origemRaw
      d = trim destinoRaw
      h = trim horarioRaw
  in
    if length (voos sys) >= 20
       then Left "Limite de voos atingido (maximo: 20)."
    else if null o then Left "Origem não pode ser vazia."
    else if null d then Left "Destino não pode ser vazio."
    else if null h then Left "Horário não pode ser vazio."
    else if cap <= 0 then Left "Capacidade deve ser maior que zero."
    else case buscarCompanhiaPorId idCompanhiaRef sys of
      Nothing -> Left "Companhia informada não existe."
      Just _  ->
        let novoId = proximoId (voos sys) idVoo
            novoV  = Voo novoId o d h idCompanhiaRef cap
        in Right sys { voos = voos sys ++ [novoV] }


-- ===============================================================
--                         BUSCAS AUXILIARES
-- ===============================================================

-- | Busca um passageiro pelo ID (retorna Maybe Passageiro).
buscarPassageiroPorId :: Int -> Sistema -> Maybe Passageiro
buscarPassageiroPorId pid sys =
  case filter (\p -> idPassageiro p == pid) (passageiros sys) of
    (p:_) -> Just p
    _     -> Nothing

-- | Busca um voo pelo ID (retorna Maybe Voo).
buscarVooPorId :: Int -> Sistema -> Maybe Voo
buscarVooPorId vid sys =
  case filter (\v -> idVoo v == vid) (voos sys) of
    (v:_) -> Just v
    _     -> Nothing

-- | Edita um voo existente
editarVoo :: Int -> String -> String -> String -> Int -> Int -> Sistema -> Either String Sistema
editarVoo vid origemRaw destinoRaw horarioRaw idCompanhiaRef cap sys =
  let o = trim origemRaw
      d = trim destinoRaw
      h = trim horarioRaw
  in
    if null o then Left "Origem não pode ser vazia."
    else if null d then Left "Destino não pode ser vazio."
    else if null h then Left "Horário não pode ser vazio."
    else if cap <= 0 then Left "Capacidade deve ser maior que zero."
    else case buscarCompanhiaPorId idCompanhiaRef sys of
      Nothing -> Left "Companhia informada não existe."
      Just _ ->
        case break (\v -> idVoo v == vid) (voos sys) of
          (_, []) -> Left "Voo não encontrado."
          (antes, v:depois) ->
            let v' = v { origem = o, destino = d, horario = h, idComp = idCompanhiaRef, capacidade = cap }
            in Right sys { voos = antes ++ (v':depois) }

-- | Exclui um voo (apenas se não houver reservas confirmadas)
excluirVoo :: Int -> Sistema -> Either String Sistema
excluirVoo vid sys =
  case buscarVooPorId vid sys of
    Nothing -> Left "Voo não encontrado."
    Just _ ->
      let reservasConfirmadas = filter (\r -> idVooRef r == vid && status r == Confirmada) (reservas sys)
      in if not (null reservasConfirmadas)
           then Left "Não é possível excluir voo com reservas confirmadas."
           else Right sys { voos = filter (\v -> idVoo v /= vid) (voos sys) }


-- ===============================================================
--                         BUSCAS DE VOOS
-- ===============================================================
-- As funções abaixo permitem pesquisar voos por origem, destino
-- ou uma combinação dos dois. Elas são usadas no menu de busca.
-- Todas as comparações ignoram diferenças de maiúsculas/minúsculas
-- e aceitam correspondências parciais (substrings).

-- | Busca voos pela origem informada.
buscarVoosPorOrigem :: String -> Sistema -> [Voo]
buscarVoosPorOrigem origemBusca sys =
  let origemLower = map toLower (trim origemBusca)
  in filter (\v -> origemLower `isInfixOf` map toLower (origem v)) (voos sys)

-- | Busca voos pelo destino informado.
buscarVoosPorDestino :: String -> Sistema -> [Voo]
buscarVoosPorDestino destinoBusca sys =
  let destinoLower = map toLower (trim destinoBusca)
  in filter (\v -> destinoLower `isInfixOf` map toLower (destino v)) (voos sys)

-- | Busca voos pela combinação origem + destino (rota).
buscarVoosPorRota :: String -> String -> Sistema -> [Voo]
buscarVoosPorRota origemBusca destinoBusca sys =
  let origemLower  = map toLower (trim origemBusca)
      destinoLower = map toLower (trim destinoBusca)
  in filter (\v ->
       origemLower  `isInfixOf` map toLower (origem v) &&
       destinoLower `isInfixOf` map toLower (destino v)
     ) (voos sys)

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

-- | Lista reservas por status específico
listarReservasPorStatus :: StatusReserva -> Sistema -> [Reserva]
listarReservasPorStatus st sys =
  filter (\r -> status r == st) (reservas sys)

-- | Cria uma reserva Confirmada para (passageiro, voo)
-- Regras:
--  - passageiro e voo devem existir
--  - não permitir reserva duplicada (mesmo passageiro + mesmo voo) em status não-cancelado
--  - verificar se voo tem capacidade disponível
criarReserva :: Int -> Int -> Sistema -> Either String Sistema
criarReserva pid vid sys = do
  case (buscarPassageiroPorId pid sys, buscarVooPorId vid sys) of
    (Nothing, _) -> Left "Passageiro inexistente."
    (_, Nothing) -> Left "Voo inexistente."
    (Just _, Just voo) ->
      let conflito = any (\r -> idPass r == pid && idVooRef r == vid && status r /= Cancelada) (reservas sys)
      in if conflito
           then Left "Já existe uma reserva ativa para este passageiro neste voo."
           else
             let reservasConfirmadas = filter (\r -> idVooRef r == vid && status r == Confirmada) (reservas sys)
                 vagasOcupadas = length reservasConfirmadas
             in if vagasOcupadas >= capacidade voo
                  then Left $ "Voo lotado! Capacidade: " ++ show (capacidade voo) ++ " assentos."
                  else
                    let novoId = proximoId (reservas sys) idReserva
                        novaR  = Reserva novoId pid vid Confirmada  -- Já cria como Confirmada
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

-- ====== USUARIOS ======

-- | Lista todos os usuarios
listarUsuarios :: Sistema -> [Usuario]
listarUsuarios = usuarios

-- | Insere um novo usuario (para registro)
-- Valida campos obrigatorios e impede duplicacao de email e nomeUsuario
-- Se for UsuarioComum, cria automaticamente um passageiro vinculado
inserirUsuario :: String -> String -> String -> Sistema -> Either String Sistema
inserirUsuario nomeRaw emailRaw senhaRaw sys =
  let nome'  = trim nomeRaw
      email' = trim emailRaw
      senha' = trim senhaRaw
  in
    if null nome'
       then Left "Nome de usuário não pode ser vazio."
    else if null email'
       then Left "Email não pode ser vazio."
    else if null senha'
       then Left "Senha não pode ser vazia."
    else if any (\u -> emailUsuario u == email') (usuarios sys)
       then Left "Já existe um usuário com esse email."
    else if any (\u -> nomeUsuario u == nome') (usuarios sys)
       then Left "Já existe um usuário com esse nome."
    else
      let novoIdUsuario = proximoId (usuarios sys) idUsuario
          novoU  = Usuario novoIdUsuario nome' email' senha' UsuarioComum
          -- Criar automaticamente um passageiro para o usuário comum (email como identificador)
          novoIdPassageiro = proximoId (passageiros sys) idPassageiro
          novoP = Passageiro novoIdPassageiro nome' email'
          sys' = sys { usuarios = usuarios sys ++ [novoU]
                     , passageiros = passageiros sys ++ [novoP] }
      in Right sys'

-- | Busca um usuario pelo ID
buscarUsuarioPorId :: Int -> Sistema -> Maybe Usuario
buscarUsuarioPorId uid sys =
  case filter (\u -> idUsuario u == uid) (usuarios sys) of
    (u:_) -> Just u
    _     -> Nothing

-- | Edita um usuario (nome, email, senha)
editarUsuario :: Int -> String -> String -> String -> Sistema -> Either String Sistema
editarUsuario uid nomeRaw emailRaw senhaRaw sys =
  let nome'  = trim nomeRaw
      email' = trim emailRaw
      senha' = trim senhaRaw
  in
    if null nome'
       then Left "Nome de usuário não pode ser vazio."
    else if null email'
       then Left "Email não pode ser vazio."
    else if null senha'
       then Left "Senha não pode ser vazia."
    else if any (\u -> emailUsuario u == email' && idUsuario u /= uid) (usuarios sys)
       then Left "Já existe um usuário com esse email."
    else if any (\u -> nomeUsuario u == nome' && idUsuario u /= uid) (usuarios sys)
       then Left "Já existe um usuário com esse nome."
    else case break (\u -> idUsuario u == uid) (usuarios sys) of
      (_, []) -> Left "Usuário não encontrado."
      (antes, u:depois) ->
        let u' = u { nomeUsuario = nome', emailUsuario = email', senhaUsuario = senha' }
        in Right sys { usuarios = antes ++ (u':depois) }

-- | Exclui um usuario (admin pode excluir outros usuarios)
excluirUsuario :: Int -> Sistema -> Either String Sistema
excluirUsuario uid sys =
  case buscarUsuarioPorId uid sys of
    Nothing -> Left "Usuário não encontrado."
    Just _ -> Right sys { usuarios = filter (\u -> idUsuario u /= uid) (usuarios sys) }

