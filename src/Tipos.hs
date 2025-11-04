module Tipos where

-- Tipos de Usuários
-- TipoUsuario define os níveis de acesso no sistema
data TipoUsuario = Administrador | UsuarioComum
  deriving (Show, Read, Eq)

-- Usuario armazena as informações de autenticação e identificação
-- Inclui: ID único, nome, email (usado para login), senha e tipo de usuário
data Usuario = Usuario
  { idUsuario   :: Int
  , nomeUsuario :: String
  , emailUsuario :: String  -- Email para login (deve ser único)
  , senhaUsuario :: String  -- Senha para autenticação (em texto simples por simplicidade)
  , tipoUsuario :: TipoUsuario
  } deriving (Show, Read, Eq) 

-- Passageiro
data Passageiro = Passageiro
  { idPassageiro :: Int
  , nome         :: String
  , documento    :: String
  } deriving (Show, Read, Eq)

-- Companhia
data Companhia = Companhia
  { idCompanhia   :: Int
  , nomeCompanhia :: String
  } deriving (Show, Read, Eq)

-- Voo
data Voo = Voo
  { idVoo    :: Int
  , origem   :: String
  , destino  :: String
  , horario  :: String
  , idComp   :: Int
  } deriving (Show, Read, Eq)

-- Status de uma reserva
data StatusReserva = Pendente | Confirmada | Cancelada
  deriving (Show, Read, Eq)

-- Reserva
data Reserva = Reserva
  { idReserva :: Int
  , idPass    :: Int
  , idVooRef  :: Int
  , status    :: StatusReserva
  } deriving (Show, Read, Eq)

-- Estado do sistema em memória
-- Sistema armazena todas as entidades do sistema em listas
data Sistema = Sistema
  { usuarios    :: [Usuario]    -- Lista de usuários cadastrados
  , passageiros :: [Passageiro]
  , companhias  :: [Companhia]
  , voos        :: [Voo]
  , reservas    :: [Reserva]
  } deriving (Show, Read, Eq)

-- Sistema vazio inicial (sem nenhum dado)
sistemaVazio :: Sistema
sistemaVazio = Sistema [] [] [] [] []




