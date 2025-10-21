module Reservas
  ( criarReserva
  , listarReservas
  , cancelarReserva
  , confirmarReserva
  ) where

import Tipos

-- Função auxiliar para gerar um novo ID incremental
novoIdReserva :: [Reserva] -> Int
novoIdReserva [] = 1
novoIdReserva rs = maximum (map idReserva rs) + 1

-- Criar uma nova reserva
criarReserva :: Sistema -> IO Sistema
criarReserva sys = do
  putStrLn "\n=== Criar Reserva ==="
  putStrLn "Informe o ID do passageiro:"
  idP <- readLn
  putStrLn "Informe o ID do voo:"
  idV <- readLn

  let passageiroExiste = any ((== idP) . idPassageiro) (passageiros sys)
      vooExiste        = any ((== idV) . idVoo) (voos sys)

  if not passageiroExiste
    then do
      putStrLn "❌ Passageiro não encontrado."
      return sys
    else if not vooExiste
      then do
        putStrLn "❌ Voo não encontrado."
        return sys
      else do
        let novaId = novoIdReserva (reservas sys)
            novaReserva = Reserva novaId idP idV Pendente
        putStrLn "✅ Reserva criada com sucesso (status: Pendente)."
        return sys { reservas = novaReserva : reservas sys }

-- Listar todas as reservas
listarReservas :: Sistema -> IO ()
listarReservas sys = do
  putStrLn "\n=== Lista de Reservas ==="
  if null (reservas sys)
    then putStrLn "Nenhuma reserva registrada."
    else mapM_ exibirReserva (reservas sys)
  where
    exibirReserva r = do
      putStrLn $ "ID: " ++ show (idReserva r)
              ++ " | Passageiro: " ++ show (idPass r)
              ++ " | Voo: " ++ show (idVooRef r)
              ++ " | Status: " ++ show (status r)

-- Confirmar uma reserva
confirmarReserva :: Sistema -> IO Sistema
confirmarReserva sys = do
  putStrLn "\nInforme o ID da reserva para confirmar:"
  rid <- readLn
  case atualizarStatusReserva rid Confirmada sys of
    Nothing -> do
      putStrLn "❌ Reserva não encontrada."
      return sys
    Just novoSys -> do
      putStrLn "✅ Reserva confirmada!"
      return novoSys

-- Cancelar uma reserva
cancelarReserva :: Sistema -> IO Sistema
cancelarReserva sys = do
  putStrLn "\nInforme o ID da reserva para cancelar:"
  rid <- readLn
  case atualizarStatusReserva rid Cancelada sys of
    Nothing -> do
      putStrLn "❌ Reserva não encontrada."
      return sys
    Just novoSys -> do
      putStrLn "✅ Reserva cancelada!"
      return novoSys

-- Função auxiliar para atualizar o status de uma reserva
atualizarStatusR
