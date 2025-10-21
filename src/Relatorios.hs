{-|
Module      : Relatorios
Description : Modulo de geracao de relatorios do sistema de aeroporto
Stability   : experimental

Este modulo implementa a geracao de relatorios seguindo principios funcionais:
- Funcoes puras para calcular estatisticas e gerar strings
- Funcoes IO apenas para imprimir e obter data/hora
- Funcoes reutilizaveis para formatacao (cabecalho, rodape, separadores)
-}

module Relatorios
  ( -- * Funcoes de formatacao reutilizaveis
    gerarCabecalho
  , gerarRodape
  , separadorLinha
  , separadorDuplo
    -- * Funcoes puras de estatisticas
  , EstatisticasGerais(..)
  , calcularEstatisticasGerais
    -- * Funcoes IO de impressao de relatorios
  , imprimirRelatorioEstatisticasGerais
  , imprimirRelatorioPassageiros
  , imprimirRelatorioCompanhias
  , imprimirRelatorioVoos
  , imprimirRelatorioReservas
  ) where

import Tipos
import Negocio (buscarCompanhiaPorId)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Printf (printf)

-- ====== TIPOS PARA RELATORIOS ======

-- | Estrutura para estatisticas gerais do sistema
data EstatisticasGerais = EstatisticasGerais
  { totalPassageiros :: Int
  , totalCompanhias  :: Int
  , totalVoos        :: Int
  , totalReservas    :: Int
  , reservasPendentes   :: Int
  , reservasConfirmadas :: Int
  , reservasCanceladas  :: Int
  } deriving (Show, Eq)

-- ====== FUNCOES PURAS DE CALCULO ======

-- | Calcula estatisticas gerais do sistema (funcao pura)
calcularEstatisticasGerais :: Sistema -> EstatisticasGerais
calcularEstatisticasGerais sys = EstatisticasGerais
  { totalPassageiros = length (passageiros sys)
  , totalCompanhias  = length (companhias sys)
  , totalVoos        = length (voos sys)
  , totalReservas    = length (reservas sys)
  , reservasPendentes   = contarPorStatus Pendente (reservas sys)
  , reservasConfirmadas = contarPorStatus Confirmada (reservas sys)
  , reservasCanceladas  = contarPorStatus Cancelada (reservas sys)
  }

-- | Conta reservas por status (funcao auxiliar pura)
contarPorStatus :: StatusReserva -> [Reserva] -> Int
contarPorStatus st = length . filter (\r -> status r == st)

-- ====== FUNCOES DE FORMATACAO (PURAS) ======

-- | Gera string do separador duplo
separadorDuplo :: String
separadorDuplo = replicate 60 '='

-- | Gera string do separador simples
separadorLinha :: String
separadorLinha = replicate 60 '-'

-- | Gera string do titulo centralizado
gerarTitulo :: String -> String
gerarTitulo titulo =
  let largura = 60
      espacos = (largura - length titulo) `div` 2
  in replicate espacos ' ' ++ titulo

-- | Gera string formatada para item de relatorio
formatarItem :: String -> String -> String
formatarItem label valor = printf "  %-30s: %s" label valor

-- | Gera string formatada para item numerico
formatarItemNum :: String -> Int -> String
formatarItemNum label num = formatarItem label (show num)

-- ====== FUNCOES IO DE FORMATACAO ======

-- | Imprime cabecalho do relatorio com titulo
gerarCabecalho :: String -> IO ()
gerarCabecalho tituloRelatorio = do
  putStrLn ""
  putStrLn separadorDuplo
  putStrLn (gerarTitulo tituloRelatorio)
  putStrLn separadorDuplo
  putStrLn ""

-- | Imprime rodape do relatorio com data e informacoes do sistema
gerarRodape :: IO ()
gerarRodape = do
  agora <- getCurrentTime
  let dataHora = formatTime defaultTimeLocale "%d/%m/%Y %H:%M:%S" agora
  putStrLn ""
  putStrLn separadorLinha
  putStrLn $ formatarItem "Relatorio gerado em" dataHora
  putStrLn $ formatarItem "Sistema" "Aeroporto v0.1.0"
  putStrLn "  Relatorio gerado automaticamente pelo Sistema Aeroporto"
  putStrLn separadorDuplo
  putStrLn ""

-- ====== RELATORIO: ESTATISTICAS GERAIS ======

-- | Gera string do corpo do relatorio de estatisticas gerais (funcao pura)
gerarCorpoEstatisticasGerais :: EstatisticasGerais -> String
gerarCorpoEstatisticasGerais stats = unlines
  [ "RESUMO DO SISTEMA:"
  , ""
  , formatarItemNum "Total de Passageiros" (totalPassageiros stats)
  , formatarItemNum "Total de Companhias" (totalCompanhias stats)
  , formatarItemNum "Total de Voos" (totalVoos stats)
  , formatarItemNum "Total de Reservas" (totalReservas stats)
  , ""
  , "DETALHAMENTO DE RESERVAS:"
  , ""
  , formatarItemNum "Reservas Pendentes" (reservasPendentes stats)
  , formatarItemNum "Reservas Confirmadas" (reservasConfirmadas stats)
  , formatarItemNum "Reservas Canceladas" (reservasCanceladas stats)
  ]

-- | Imprime relatorio completo de estatisticas gerais (funcao IO)
imprimirRelatorioEstatisticasGerais :: Sistema -> IO ()
imprimirRelatorioEstatisticasGerais sys = do
  let stats = calcularEstatisticasGerais sys
  gerarCabecalho "RELATORIO DE ESTATISTICAS GERAIS"
  putStr (gerarCorpoEstatisticasGerais stats)
  gerarRodape

-- ====== RELATORIO: PASSAGEIROS ======

-- | Imprime relatorio de passageiros com suas reservas
imprimirRelatorioPassageiros :: Sistema -> IO ()
imprimirRelatorioPassageiros sys = do
  let passList = passageiros sys
      resList = reservas sys

  gerarCabecalho "RELATORIO DE PASSAGEIROS"

  if null passList
    then putStrLn "  Nenhum passageiro cadastrado no sistema.\n"
    else do
      putStrLn "LISTA DE PASSAGEIROS:\n"
      imprimirListaPassageiros resList passList

      putStrLn ""
      putStrLn separadorDuplo
      putStrLn "RESUMO:\n"

      let totalPass = length passList
          passComReservas = filter (\p -> temReserva (idPassageiro p) resList) passList
          passSemReservas = filter (\p -> not (temReserva (idPassageiro p) resList)) passList

      putStrLn $ formatarItemNum "Total de Passageiros" totalPass
      putStrLn $ formatarItemNum "Passageiros com Reservas" (length passComReservas)
      putStrLn $ formatarItemNum "Passageiros sem Reservas" (length passSemReservas)

      if null passSemReservas
        then return ()
        else do
          putStrLn ""
          putStrLn "PASSAGEIROS SEM RESERVAS:"
          putStrLn ""
          imprimirPassageirosSemReservas passSemReservas

  gerarRodape
  where
    temReserva pid rs = any (\r -> idPass r == pid) rs

-- | Imprime lista de passageiros (auxiliar recursiva)
imprimirListaPassageiros :: [Reserva] -> [Passageiro] -> IO ()
imprimirListaPassageiros _ [] = return ()
imprimirListaPassageiros resList (p:ps) = do
  imprimirPassageiroComReservas resList p
  imprimirListaPassageiros resList ps

-- | Imprime passageiros sem reservas (auxiliar recursiva)
imprimirPassageirosSemReservas :: [Passageiro] -> IO ()
imprimirPassageirosSemReservas [] = return ()
imprimirPassageirosSemReservas (p:ps) = do
  putStrLn $ formatarItem ("  ID " ++ show (idPassageiro p)) (nome p)
  imprimirPassageirosSemReservas ps

-- | Imprime dados de um passageiro com suas reservas
imprimirPassageiroComReservas :: [Reserva] -> Passageiro -> IO ()
imprimirPassageiroComReservas resList p = do
  let pid = idPassageiro p
      resPass = filter (\r -> idPass r == pid) resList
      numPendentes = contarPorStatus Pendente resPass
      numConfirmadas = contarPorStatus Confirmada resPass
      numCanceladas = contarPorStatus Cancelada resPass
      totalRes = length resPass

  putStrLn separadorLinha
  putStrLn $ formatarItem "ID" (show pid)
  putStrLn $ formatarItem "Nome" (nome p)
  putStrLn $ formatarItem "Documento" (documento p)
  putStrLn ""
  putStrLn "  RESERVAS:"
  putStrLn $ formatarItemNum "  Total" totalRes
  putStrLn $ formatarItemNum "  Pendentes" numPendentes
  putStrLn $ formatarItemNum "  Confirmadas" numConfirmadas
  putStrLn $ formatarItemNum "  Canceladas" numCanceladas
  putStrLn ""

-- ====== RELATORIO: COMPANHIAS ======

-- | Imprime relatorio de companhias com seus voos
imprimirRelatorioCompanhias :: Sistema -> IO ()
imprimirRelatorioCompanhias sys = do
  let compList = companhias sys
      voosList = voos sys

  gerarCabecalho "RELATORIO DE COMPANHIAS"

  if null compList
    then putStrLn "  Nenhuma companhia cadastrada no sistema.\n"
    else do
      putStrLn "LISTA DE COMPANHIAS:\n"
      imprimirListaCompanhias voosList compList

      putStrLn ""
      putStrLn separadorDuplo
      putStrLn "RESUMO:\n"

      let totalComp = length compList
          compComVoos = filter (\c -> temVoo (idCompanhia c) voosList) compList
          compSemVoos = filter (\c -> not (temVoo (idCompanhia c) voosList)) compList

      putStrLn $ formatarItemNum "Total de Companhias" totalComp
      putStrLn $ formatarItemNum "Companhias com Voos" (length compComVoos)
      putStrLn $ formatarItemNum "Companhias sem Voos" (length compSemVoos)

      if null compSemVoos
        then return ()
        else do
          putStrLn ""
          putStrLn "COMPANHIAS SEM VOOS:"
          putStrLn ""
          imprimirCompanhiasSemVoos compSemVoos

  gerarRodape
  where
    temVoo cid vs = any (\v -> idComp v == cid) vs

-- | Imprime lista de companhias (auxiliar recursiva)
imprimirListaCompanhias :: [Voo] -> [Companhia] -> IO ()
imprimirListaCompanhias _ [] = return ()
imprimirListaCompanhias voosList (c:cs) = do
  imprimirCompanhiaComVoos voosList c
  imprimirListaCompanhias voosList cs

-- | Imprime companhias sem voos (auxiliar recursiva)
imprimirCompanhiasSemVoos :: [Companhia] -> IO ()
imprimirCompanhiasSemVoos [] = return ()
imprimirCompanhiasSemVoos (c:cs) = do
  putStrLn $ formatarItem ("  ID " ++ show (idCompanhia c)) (nomeCompanhia c)
  imprimirCompanhiasSemVoos cs

-- | Imprime dados de uma companhia com seus voos
imprimirCompanhiaComVoos :: [Voo] -> Companhia -> IO ()
imprimirCompanhiaComVoos voosList c = do
  let cid = idCompanhia c
      voosComp = filter (\v -> idComp v == cid) voosList
      totalVoos = length voosComp

  putStrLn separadorLinha
  putStrLn $ formatarItem "ID" (show cid)
  putStrLn $ formatarItem "Nome" (nomeCompanhia c)
  putStrLn ""
  putStrLn "  VOOS:"
  putStrLn $ formatarItemNum "  Total de Voos" totalVoos
  putStrLn ""

-- ====== RELATORIO: VOOS ======

-- | Imprime relatorio de voos com reservas e distribuicao por companhia
imprimirRelatorioVoos :: Sistema -> IO ()
imprimirRelatorioVoos sys = do
  let voosList = voos sys
      resList = reservas sys
      compList = companhias sys

  gerarCabecalho "RELATORIO DE VOOS"

  if null voosList
    then putStrLn "  Nenhum voo cadastrado no sistema.\n"
    else do
      putStrLn "LISTA DE VOOS:\n"
      imprimirListaVoos sys resList voosList

      putStrLn ""
      putStrLn separadorDuplo
      putStrLn "RESUMO:\n"

      let totalVoos = length voosList
          voosComReservas = filter (\v -> temReserva (idVoo v) resList) voosList
          voosSemReservas = filter (\v -> not (temReserva (idVoo v) resList)) voosList

      putStrLn $ formatarItemNum "Total de Voos" totalVoos
      putStrLn $ formatarItemNum "Voos com Reservas" (length voosComReservas)
      putStrLn $ formatarItemNum "Voos sem Reservas" (length voosSemReservas)

      putStrLn ""
      putStrLn "DISTRIBUICAO POR COMPANHIA:"
      putStrLn ""
      imprimirDistribuicaoVoosPorCompanhia voosList compList

      if null voosSemReservas
        then return ()
        else do
          putStrLn ""
          putStrLn "VOOS SEM RESERVAS:"
          putStrLn ""
          imprimirVoosSemReservas sys voosSemReservas

  gerarRodape
  where
    temReserva vid rs = any (\r -> idVooRef r == vid) rs

-- | Imprime lista de voos (auxiliar recursiva)
imprimirListaVoos :: Sistema -> [Reserva] -> [Voo] -> IO ()
imprimirListaVoos _ _ [] = return ()
imprimirListaVoos sys resList (v:vs) = do
  imprimirVooComReservas sys resList v
  imprimirListaVoos sys resList vs

-- | Imprime dados de um voo com suas reservas
imprimirVooComReservas :: Sistema -> [Reserva] -> Voo -> IO ()
imprimirVooComReservas sys resList v = do
  let vid = idVoo v
      resVoo = filter (\r -> idVooRef r == vid) resList
      totalRes = length resVoo
      compNome = case buscarCompanhiaPorId (idComp v) sys of
                   Just c  -> nomeCompanhia c
                   Nothing -> "(desconhecida)"

  putStrLn separadorLinha
  putStrLn $ formatarItem "ID" (show vid)
  putStrLn $ formatarItem "Rota" (origem v ++ " -> " ++ destino v)
  putStrLn $ formatarItem "Horario" (horario v)
  putStrLn $ formatarItem "Companhia" compNome
  putStrLn ""
  putStrLn "  RESERVAS:"
  putStrLn $ formatarItemNum "  Total de Reservas" totalRes
  putStrLn ""

-- | Imprime distribuicao de voos por companhia (auxiliar recursiva)
imprimirDistribuicaoVoosPorCompanhia :: [Voo] -> [Companhia] -> IO ()
imprimirDistribuicaoVoosPorCompanhia _ [] = return ()
imprimirDistribuicaoVoosPorCompanhia voosList (c:cs) = do
  let voosComp = filter (\v -> idComp v == idCompanhia c) voosList
  putStrLn $ formatarItemNum ("  " ++ nomeCompanhia c) (length voosComp)
  imprimirDistribuicaoVoosPorCompanhia voosList cs

-- | Imprime voos sem reservas (auxiliar recursiva)
imprimirVoosSemReservas :: Sistema -> [Voo] -> IO ()
imprimirVoosSemReservas _ [] = return ()
imprimirVoosSemReservas sys (v:vs) = do
  let compNome = case buscarCompanhiaPorId (idComp v) sys of
                   Just c  -> nomeCompanhia c
                   Nothing -> "(desconhecida)"
  putStrLn $ formatarItem ("  ID " ++ show (idVoo v)) (origem v ++ " -> " ++ destino v ++ " (" ++ compNome ++ ")")
  imprimirVoosSemReservas sys vs

-- ====== RELATORIO: RESERVAS ======

-- | Imprime relatorio detalhado de reservas com estatisticas
imprimirRelatorioReservas :: Sistema -> IO ()
imprimirRelatorioReservas sys = do
  let resList = reservas sys

  gerarCabecalho "RELATORIO DE RESERVAS"

  if null resList
    then putStrLn "  Nenhuma reserva cadastrada no sistema.\n"
    else do
      let totalRes = length resList
          numPendentes = contarPorStatus Pendente resList
          numConfirmadas = contarPorStatus Confirmada resList
          numCanceladas = contarPorStatus Cancelada resList

          taxaConfirmacao :: Double
          taxaConfirmacao = if totalRes > 0
                              then (fromIntegral numConfirmadas * 100.0) / fromIntegral totalRes
                              else 0.0
          taxaCancelamento :: Double
          taxaCancelamento = if totalRes > 0
                               then (fromIntegral numCanceladas * 100.0) / fromIntegral totalRes
                               else 0.0

      putStrLn "RESUMO GERAL:\n"
      putStrLn $ formatarItemNum "Total de Reservas" totalRes
      putStrLn ""
      putStrLn "RESERVAS POR STATUS:\n"
      putStrLn $ formatarItemNum "Pendentes" numPendentes
      putStrLn $ formatarItemNum "Confirmadas" numConfirmadas
      putStrLn $ formatarItemNum "Canceladas" numCanceladas
      putStrLn ""
      putStrLn "TAXAS:\n"
      putStrLn $ formatarItem "Taxa de Confirmacao" (printf "%.2f%%" taxaConfirmacao)
      putStrLn $ formatarItem "Taxa de Cancelamento" (printf "%.2f%%" taxaCancelamento)

  gerarRodape