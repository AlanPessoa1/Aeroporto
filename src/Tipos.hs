module Tipos where

 -- Tipos de Usuários
 data TipoUsuario = Administrador | UsuarioComum
   deriving (Show, Read, Eq) 

  data Usuario = Usuario
   { idUsuario   :: Int -- ver se tem algum jeito de fazer isso automaticamente
   , nomeUsuario :: String
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
data Sistema = Sistema
  { passageiros :: [Passageiro]
  , companhias  :: [Companhia]
  , voos        :: [Voo]
  , reservas    :: [Reserva]
  } deriving (Show, Read, Eq)

sistemaVazio :: Sistema
sistemaVazio = Sistema [] [] [] []




