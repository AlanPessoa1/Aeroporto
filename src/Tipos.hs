module Tipos where

-- ======================================================
-- Tipos de Usuário e Estrutura de Login
-- ======================================================

data TipoUsuario
  = Administrador
  | UsuarioComum
  deriving (Show, Read, Eq)

data Usuario = Usuario
  { idUsuario   :: Int
  , nomeUsuario :: String
  , emailUsuario :: String
  , senhaUsuario :: String
  , tipoUsuario :: TipoUsuario
  } deriving (Show, Read, Eq)

-- ======================================================
-- Passageiro
-- ======================================================

data Passageiro = Passageiro
  { idPassageiro  :: Int
  , nome          :: String
  , identificador :: String  -- Email, CPF, RG, etc.
  } deriving (Show, Read, Eq)

-- ======================================================
-- Companhia
-- ======================================================

data Companhia = Companhia
  { idCompanhia   :: Int
  , nomeCompanhia :: String
  } deriving (Show, Read, Eq)

-- ======================================================
-- Voo
-- ======================================================

data Voo = Voo
  { idVoo      :: Int
  , origem     :: String
  , destino    :: String
  , horario    :: String
  , idComp     :: Int
  , capacidade :: Int
  } deriving (Show, Read, Eq)

-- ======================================================
-- Reserva
-- ======================================================

data StatusReserva
  = Pendente
  | Confirmada
  | Cancelada
  deriving (Show, Read, Eq)

data Reserva = Reserva
  { idReserva :: Int
  , idPass    :: Int
  , idVooRef  :: Int
  , status    :: StatusReserva
  } deriving (Show, Read, Eq)

-- ======================================================
-- Sistema
-- ======================================================

data Sistema = Sistema
  { usuarios    :: [Usuario]
  , passageiros :: [Passageiro]
  , companhias  :: [Companhia]
  , voos        :: [Voo]
  , reservas    :: [Reserva]
  } deriving (Show, Read, Eq)

sistemaVazio :: Sistema
sistemaVazio = Sistema [] [] [] [] []




