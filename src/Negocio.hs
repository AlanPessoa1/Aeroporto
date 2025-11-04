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
  , listarVoos, inserirVoo, inserirVooDados, buscarCompanhiaPorId
  , buscarPassageiroPorId, buscarVooPorId
  , buscarVoosPorOrigem, buscarVoosPorDestino, buscarVoosPorRota
  , listarReservas
  , criarReserva
  , confirmarReserva
  , cancelarReserva
  , listarReservasPorPassageiro
  , listarReservasPorVoo
  , listarReservasPorStatus
  -- Funcoes de usuarios (autenticacao e registro)
  , registrarUsuario
  , autenticarUsuario
  , buscarUsuarioPorEmail
  , buscarUsuarioPorId
  -- Funcoes UPDATE de usuarios
  , atualizarNomeUsuario
  , atualizarEmailUsuario
  , atualizarSenhaUsuario
  -- Funcoes DELETE
  , deletarUsuario
  , deletarPassageiro
  , deletarCompanhia
  , deletarVoo
  ) where

import Tipos
import Data.Char (isSpace, toLower)
import Data.List (isInfixOf)


-- Funcoes genericas de apoio

-- Remove espacos em branco no inicio e no fim de uma string.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


-- Calcula o proximo ID para uma nova entidade.
-- Retorna o maior ID + 1, ou 1 se a lista estiver vazia.
proximoId :: [a] -> (a -> Int) -> Int
proximoId xs f =
  if null xs then 1 else maximum (map f xs) + 1


-- Usuarios - Autenticacao e registro

-- Busca um usuario pelo email (retorna Maybe Usuario).
buscarUsuarioPorEmail :: String -> Sistema -> Maybe Usuario
buscarUsuarioPorEmail email sys =
  case filter (\u -> emailUsuario u == email) (usuarios sys) of
    (u:_) -> Just u
    _     -> Nothing


-- Registra um novo usuario no sistema.
-- Valida: nome, email e senha nao podem ser vazios. Email deve ser unico.
registrarUsuario :: String -> String -> String -> TipoUsuario -> Sistema -> Either String Sistema
registrarUsuario nomeRaw emailRaw senhaRaw tipo sys =
  let nome'  = trim nomeRaw
      email' = trim emailRaw
      senha' = trim senhaRaw
  in
    -- Validações
    if null nome'
      then Left "Nome não pode ser vazio."
    else if null email'
      then Left "Email não pode ser vazio."
    else if null senha'
      then Left "Senha não pode ser vazia."
    -- Verifica se email já está cadastrado
    else if any (\u -> emailUsuario u == email') (usuarios sys)
      then Left "Já existe um usuário com este email."
    else
      -- Cria novo usuário com ID incremental
      let novoId = proximoId (usuarios sys) idUsuario
          novoUsuario = Usuario novoId nome' email' senha' tipo
      in Right sys { usuarios = usuarios sys ++ [novoUsuario] }


-- Autentica um usuario com email e senha.
autenticarUsuario :: String -> String -> Sistema -> Maybe Usuario
autenticarUsuario email senha sys =
  case buscarUsuarioPorEmail email sys of
    Nothing -> Nothing  -- Email não encontrado
    Just u  ->
      -- Verifica se a senha está correta
      if senhaUsuario u == senha
        then Just u
        else Nothing


-- Busca um usuario pelo ID.
buscarUsuarioPorId :: Int -> Sistema -> Maybe Usuario
buscarUsuarioPorId uid sys =
  case filter (\u -> idUsuario u == uid) (usuarios sys) of
    (u:_) -> Just u
    _     -> Nothing


-- Atualiza o nome de um usuario.
atualizarNomeUsuario :: Int -> String -> Sistema -> Either String Sistema
atualizarNomeUsuario uid novoNomeRaw sys =
  let novoNome = trim novoNomeRaw
  in
    if null novoNome
      then Left "Nome não pode ser vazio."
    else case break (\u -> idUsuario u == uid) (usuarios sys) of
      (_, []) -> Left "Usuario não encontrado."
      (antes, u:depois) ->
        let usuarioAtualizado = u { nomeUsuario = novoNome }
            novosUsuarios = antes ++ (usuarioAtualizado : depois)
        in Right sys { usuarios = novosUsuarios }


-- Atualiza o email de um usuario.
atualizarEmailUsuario :: Int -> String -> Sistema -> Either String Sistema
atualizarEmailUsuario uid novoEmailRaw sys =
  let novoEmail = trim novoEmailRaw
  in
    if null novoEmail
      then Left "Email não pode ser vazio."
    else if any (\u -> emailUsuario u == novoEmail && idUsuario u /= uid) (usuarios sys)
      then Left "Já existe um usuário com este email."
    else case break (\u -> idUsuario u == uid) (usuarios sys) of
      (_, []) -> Left "Usuario não encontrado."
      (antes, u:depois) ->
        let usuarioAtualizado = u { emailUsuario = novoEmail }
            novosUsuarios = antes ++ (usuarioAtualizado : depois)
        in Right sys { usuarios = novosUsuarios }


-- Atualiza a senha de um usuario.
atualizarSenhaUsuario :: Int -> String -> Sistema -> Either String Sistema
atualizarSenhaUsuario uid novaSenhaRaw sys =
  let novaSenha = trim novaSenhaRaw
  in
    if null novaSenha
      then Left "Senha não pode ser vazia."
    else case break (\u -> idUsuario u == uid) (usuarios sys) of
      (_, []) -> Left "Usuario não encontrado."
      (antes, u:depois) ->
        let usuarioAtualizado = u { senhaUsuario = novaSenha }
            novosUsuarios = antes ++ (usuarioAtualizado : depois)
        in Right sys { usuarios = novosUsuarios }


-- Deleta um usuario do sistema.
deletarUsuario :: Int -> Sistema -> Either String Sistema
deletarUsuario uid sys =
  let usuariosFiltrados = filter (\u -> idUsuario u /= uid) (usuarios sys)
  in
    if length usuariosFiltrados == length (usuarios sys)
      then Left "Usuario não encontrado."
      else Right sys { usuarios = usuariosFiltrados }


-- Retorna todos os passageiros do sistema.
listarPassageiros :: Sistema -> [Passageiro]
listarPassageiros = passageiros


-- Insere um passageiro (gera novo ID automaticamente).
inserirPassageiro :: Passageiro -> Sistema -> Either String Sistema
inserirPassageiro p sys =
  inserirPassageiroDados (nome p) (documento p) sys


-- Insere um novo passageiro a partir de nome e documento.
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


-- Deleta um passageiro (valida se tem reservas).
deletarPassageiro :: Int -> Sistema -> Either String Sistema
deletarPassageiro pid sys =
  -- Verifica se o passageiro existe
  case filter (\p -> idPassageiro p == pid) (passageiros sys) of
    [] -> Left "Passageiro não encontrado."
    _  ->
      -- Verifica se tem reservas
      let temReservas = any (\r -> idPass r == pid) (reservas sys)
      in if temReservas
           then Left "Não é possível excluir passageiro com reservas. Cancele as reservas primeiro."
           else
             let passageirosFiltrados = filter (\p -> idPassageiro p /= pid) (passageiros sys)
             in Right sys { passageiros = passageirosFiltrados }


-- Retorna todas as companhias aereas cadastradas.
listarCompanhias :: Sistema -> [Companhia]
listarCompanhias = companhias


-- Insere uma companhia a partir de uma estrutura ja existente.
inserirCompanhia :: Companhia -> Sistema -> Either String Sistema
inserirCompanhia c sys = inserirCompanhiaNome (nomeCompanhia c) sys


-- Insere uma companhia aerea apenas com o nome.
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


-- Busca uma companhia pelo seu ID.
buscarCompanhiaPorId :: Int -> Sistema -> Maybe Companhia
buscarCompanhiaPorId cid sys =
  case filter (\c -> idCompanhia c == cid) (companhias sys) of
    (c:_) -> Just c
    _     -> Nothing


-- Deleta uma companhia (valida se tem voos).
deletarCompanhia :: Int -> Sistema -> Either String Sistema
deletarCompanhia cid sys =
  -- Verifica se a companhia existe
  case filter (\c -> idCompanhia c == cid) (companhias sys) of
    [] -> Left "Companhia não encontrada."
    _  ->
      -- Verifica se tem voos
      let temVoos = any (\v -> idComp v == cid) (voos sys)
      in if temVoos
           then Left "Não é possível excluir companhia com voos cadastrados. Delete os voos primeiro."
           else
             let companhiasFiltradas = filter (\c -> idCompanhia c /= cid) (companhias sys)
             in Right sys { companhias = companhiasFiltradas }


-- Lista todos os voos disponiveis no sistema.
listarVoos :: Sistema -> [Voo]
listarVoos = voos


-- Insere um voo (gera novo ID automaticamente).
inserirVoo :: Voo -> Sistema -> Either String Sistema
inserirVoo v sys =
  inserirVooDados (origem v) (destino v) (horario v) (idComp v) sys


-- Insere um voo a partir de seus campos basicos.
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


-- Deleta um voo (valida se tem reservas).
deletarVoo :: Int -> Sistema -> Either String Sistema
deletarVoo vid sys =
  -- Verifica se o voo existe
  case filter (\v -> idVoo v == vid) (voos sys) of
    [] -> Left "Voo não encontrado."
    _  ->
      -- Verifica se tem reservas
      let temReservas = any (\r -> idVooRef r == vid) (reservas sys)
      in if temReservas
           then Left "Não é possível excluir voo com reservas cadastradas. Cancele as reservas primeiro."
           else
             let voosFiltrados = filter (\v -> idVoo v /= vid) (voos sys)
             in Right sys { voos = voosFiltrados }


-- Busca um passageiro pelo ID.
buscarPassageiroPorId :: Int -> Sistema -> Maybe Passageiro
buscarPassageiroPorId pid sys =
  case filter (\p -> idPassageiro p == pid) (passageiros sys) of
    (p:_) -> Just p
    _     -> Nothing

-- Busca um voo pelo ID.
buscarVooPorId :: Int -> Sistema -> Maybe Voo
buscarVooPorId vid sys =
  case filter (\v -> idVoo v == vid) (voos sys) of
    (v:_) -> Just v
    _     -> Nothing


-- Busca voos pela origem informada.
buscarVoosPorOrigem :: String -> Sistema -> [Voo]
buscarVoosPorOrigem origemBusca sys =
  let origemLower = map toLower (trim origemBusca)
  in filter (\v -> origemLower `isInfixOf` map toLower (origem v)) (voos sys)

-- Busca voos pelo destino informado.
buscarVoosPorDestino :: String -> Sistema -> [Voo]
buscarVoosPorDestino destinoBusca sys =
  let destinoLower = map toLower (trim destinoBusca)
  in filter (\v -> destinoLower `isInfixOf` map toLower (destino v)) (voos sys)

-- Busca voos pela combinacao origem + destino (rota).
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

