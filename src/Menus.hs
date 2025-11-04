-- | Módulo responsável por gerenciar toda a interface textual do sistema.
-- Ele apresenta menus, recebe entradas do usuário e chama as funções da
-- camada de negócio (definidas em Negocio.hs) e de relatórios.
-- Assim, o Menus.hs funciona como o "controlador" principal do programa.
module Menus (menuPrincipal, menuAutenticacao) where

import Tipos
import Negocio
import Relatorios
import Reservas

-- ====== AUTENTICACAO (LOGIN E REGISTRO) ======

-- | Menu inicial de autenticação
-- Oferece 3 opções: Usuário Comum, Administrador ou Sair
-- Retorna o usuário autenticado e o sistema atualizado
menuAutenticacao :: Sistema -> IO (Usuario, Sistema)
menuAutenticacao sys = do
  putStrLn ""
  putStrLn "========== Bem-vindo ao Sistema Aeroporto =========="
  putStrLn "1) Acesso Usuario Comum (Login/Registro)"
  putStrLn "2) Acesso Administrador (Login)"
  putStrLn "3) Sair"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> menuUsuarioComum sys          -- Menu para usuário comum
    "2" -> menuAdministrador sys         -- Menu para administrador
    "3" -> do
      putStrLn "Ate logo!"
      error "Saindo do sistema..."      -- Encerra o programa
    _   -> do
      putStrLn "Opcao invalida."
      menuAutenticacao sys

-- | Menu específico para usuário comum
-- Permite login ou registro de novo usuário comum
menuUsuarioComum :: Sistema -> IO (Usuario, Sistema)
menuUsuarioComum sys = do
  putStrLn ""
  putStrLn "===== ACESSO USUARIO COMUM ====="
  putStrLn "1) Login"
  putStrLn "2) Registrar nova conta"
  putStrLn "3) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoLoginComum sys
    "2" -> acaoRegistroComum sys
    "3" -> menuAutenticacao sys
    _   -> do
      putStrLn "Opcao invalida."
      menuUsuarioComum sys

-- | Menu específico para administrador
-- Apenas permite login (admin não se registra pelo sistema)
menuAdministrador :: Sistema -> IO (Usuario, Sistema)
menuAdministrador sys = do
  putStrLn ""
  putStrLn "===== ACESSO ADMINISTRADOR ====="
  putStrLn "Email e senha do administrador:"
  putStr "Email: "
  email <- getLine
  putStr "Senha: "
  senha <- getLine

  case autenticarUsuario email senha sys of
    Just usuario ->
      if tipoUsuario usuario == Administrador
        then do
          putStrLn $ "Bem-vindo, " ++ nomeUsuario usuario ++ "!"
          return (usuario, sys)
        else do
          putStrLn "[ERRO] Este usuario nao e administrador."
          menuAutenticacao sys
    Nothing -> do
      putStrLn "[ERRO] Email ou senha incorretos."
      menuAutenticacao sys

-- | Ação de login para usuário comum
-- Autentica e verifica se é realmente um usuário comum
acaoLoginComum :: Sistema -> IO (Usuario, Sistema)
acaoLoginComum sys = do
  putStrLn ""
  putStrLn "===== LOGIN USUARIO COMUM ====="
  putStr "Email: "
  email <- getLine
  putStr "Senha: "
  senha <- getLine

  case autenticarUsuario email senha sys of
    Just usuario ->
      if tipoUsuario usuario == UsuarioComum
        then do
          putStrLn $ "Bem-vindo, " ++ nomeUsuario usuario ++ "!"
          return (usuario, sys)
        else do
          putStrLn "[ERRO] Este usuario e administrador. Use o acesso de administrador."
          menuUsuarioComum sys
    Nothing -> do
      putStrLn "[ERRO] Email ou senha incorretos."
      menuUsuarioComum sys

-- | Ação de registro de novo usuário comum
-- Cria automaticamente como UsuarioComum (não permite escolher tipo)
acaoRegistroComum :: Sistema -> IO (Usuario, Sistema)
acaoRegistroComum sys = do
  putStrLn ""
  putStrLn "===== REGISTRO DE NOVO USUARIO COMUM ====="
  putStr "Nome completo: "
  nome <- getLine
  putStr "Email: "
  email <- getLine
  putStr "Senha: "
  senha <- getLine

  -- Registra sempre como UsuarioComum
  case registrarUsuario nome email senha UsuarioComum sys of
    Left erro -> do
      putStrLn $ "[ERRO] " ++ erro
      menuUsuarioComum sys
    Right sys' -> do
      case buscarUsuarioPorEmail email sys' of
        Just usuario -> do
          putStrLn "Usuario registrado com sucesso!"
          putStrLn $ "Bem-vindo, " ++ nomeUsuario usuario ++ "!"
          return (usuario, sys')
        Nothing -> do
          putStrLn "[ERRO] Erro ao recuperar usuario registrado."
          menuUsuarioComum sys

-- Limpa a tela do console (apenas visual)
limparTela :: IO ()
limparTela = putStrLn "\n\n\n"  -- Simples espaçamento (funciona em qualquer terminal)

-- Menu principal do sistema
menuPrincipal :: Usuario -> Sistema -> IO Sistema
menuPrincipal usuario sys = do
  putStrLn ""
  putStrLn "========== Aeroporto.hs =========="

  -- Verifica o tipo de usuário para exibir menu apropriado
  if tipoUsuario usuario == Administrador
      then do
        putStrLn "1) Passageiros"
        putStrLn "2) Companhias"
        putStrLn "3) Voos"
        putStrLn "4) Reservas"
        putStrLn "5) Relatorios (Administrador)"
        putStrLn "6) Gerenciar Sistema (DELETE)"
        putStrLn "7) Salvar e Sair"
      else do
        putStrLn "1) Passageiros (Listar)"
        putStrLn "2) Companhias (Listar)"
        putStrLn "3) Voos (Listar/Buscar)"
        putStrLn "4) Reservas"
        putStrLn "5) Minha Conta (Atualizar perfil)"
        putStrLn "6) Salvar e Sair"

  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> if tipoUsuario usuario == Administrador
             then menuPassageiros sys >>= menuPrincipal usuario
             else menuPassageirosComum sys >>= menuPrincipal usuario
    "2" -> if tipoUsuario usuario == Administrador
             then menuCompanhias sys >>= menuPrincipal usuario
             else menuCompanhiasComum sys >>= menuPrincipal usuario
    "3" -> if tipoUsuario usuario == Administrador
             then menuVoos sys >>= menuPrincipal usuario
             else menuVoosComum sys >>= menuPrincipal usuario
    "4" -> menuReservas sys >>= menuPrincipal usuario
    "5" -> if tipoUsuario usuario == Administrador
             then menuRelatorios sys >>= menuPrincipal usuario
             else menuMinhaConta usuario sys >>= menuPrincipal usuario
    "6" -> if tipoUsuario usuario == Administrador
             then menuGerenciarSistema sys >>= menuPrincipal usuario
             else putStrLn "Saindo..." >> pure sys
    "7" -> if tipoUsuario usuario == Administrador
             then putStrLn "Saindo..." >> pure sys
             else putStrLn "Opcao invalida." >> menuPrincipal usuario sys
    _   -> putStrLn "Opcao invalida." >> menuPrincipal usuario sys


--menuPrincipal :: Sistema -> IO Sistema
--menuPrincipal sys = do
  --putStrLn ""
  --putStrLn "========== Aeroporto.hs =========="
  --putStrLn "1) Passageiros"
  --putStrLn "2) Companhias"
  --putStrLn "3) Voos"
  --putStrLn "4) Reservas"
  --putStrLn "5) Relatorios"
  --putStrLn "6) Salvar e Sair"
  --putStr   "Escolha: "
  --opc <- getLine
  --case opc of
    --"1" -> menuPassageiros sys >>= menuPrincipal
    --"2" -> menuCompanhias sys >>= menuPrincipal
    --"3" -> menuVoos sys >>= menuPrincipal
    --"4" -> menuReservas sys >>= menuPrincipal   
    --"5" -> menuRelatorios sys >>= menuPrincipal
    --"6" -> putStrLn "Saindo..." >> pure sys
    --_   -> putStrLn "Opcao invalida." >> menuPrincipal sys

-- ====== HELPER ======

readIntSafe :: String -> Maybe Int
readIntSafe s = case reads s of
  [(n,"")] -> Just n
  _        -> Nothing

-- ===============================================================
--                     SUBMENU: RESERVAS
-- ===============================================================
-- Este menu é responsável por todo o gerenciamento das reservas,
-- desde a criação até a consulta por diferentes critérios.
-- Ele interage com as funções da camada de negócio (Negocio.hs),
-- garantindo que as operações sejam consistentes e seguras.
-- O fluxo segue o padrão de leitura -> validação -> execução -> feedback.

menuReservas :: Sistema -> IO Sistema
menuReservas sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Reservas ----"
  putStrLn "1) Criar"
  putStrLn "2) Confirmar"
  putStrLn "3) Cancelar"
  putStrLn "4) Listar todas"
  putStrLn "5) Listar por Passageiro"
  putStrLn "6) Listar por Voo"
  putStrLn "7) Listar por Status"
  putStrLn "8) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    -- Cada opção chama uma função específica de ação,
    -- e ao final retorna ao próprio menu (recursão interativa).
    "1" -> acaoCriarReserva sys >>= menuReservas
    "2" -> acaoConfirmarReserva sys >>= menuReservas
    "3" -> acaoCancelarReserva sys >>= menuReservas
    "4" -> acaoListarReservas sys >> menuReservas sys
    "5" -> acaoListarReservasPorPassageiro sys >> menuReservas sys
    "6" -> acaoListarReservasPorVoo sys >> menuReservas sys
    "7" -> menuListarReservasPorStatus sys >> menuReservas sys
    "8" -> pure sys
    _   -> putStrLn "Opcao invalida." >> menuReservas sys


-- ===============================================================
--                    AÇÃO: CRIAR RESERVA
-- ===============================================================
-- Cria uma nova reserva com base nos IDs do passageiro e do voo.
-- O status inicial da reserva é "Pendente".
-- São realizadas verificações de validade dos IDs e regras de negócio.
acaoCriarReserva :: Sistema -> IO Sistema
acaoCriarReserva sys = do
  putStr   "ID Passageiro: "
  sPid <- getLine
  putStr   "ID Voo: "
  sVid <- getLine
  case (readIntSafe sPid, readIntSafe sVid) of
    (Just pid, Just vid) ->
      -- Chama a função de negócio 'criarReserva' (Negocio.hs)
      case criarReserva pid vid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva criada (status: Pendente)." >> pure sys'
    _ -> putStrLn "[ERRO] IDs inválidos." >> pure sys


-- ===============================================================
--                    AÇÃO: CONFIRMAR RESERVA
-- ===============================================================
-- Atualiza o status de uma reserva para "Confirmada".
-- O ID da reserva é validado antes da alteração.
acaoConfirmarReserva :: Sistema -> IO Sistema
acaoConfirmarReserva sys = do
  putStr   "ID da Reserva: "
  sRid <- getLine
  case readIntSafe sRid of
    Just rid ->
      case confirmarReserva rid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva confirmada." >> pure sys'
    _ -> putStrLn "[ERRO] ID inválido." >> pure sys


-- ===============================================================
--                    AÇÃO: CANCELAR RESERVA
-- ===============================================================
-- Altera o status da reserva para "Cancelada".
-- Essa função é essencial para controle de disponibilidade de voos.
acaoCancelarReserva :: Sistema -> IO Sistema
acaoCancelarReserva sys = do
  putStr   "ID da Reserva: "
  sRid <- getLine
  case readIntSafe sRid of
    Just rid ->
      case cancelarReserva rid sys of
        Left err   -> putStrLn ("[ERRO] " ++ err) >> pure sys
        Right sys' -> putStrLn "[OK] Reserva cancelada." >> pure sys'
    _ -> putStrLn "[ERRO] ID inválido." >> pure sys


-- ===============================================================
--                   LISTAGEM DE RESERVAS
-- ===============================================================
-- As funções abaixo exibem reservas segundo diferentes critérios:
-- todas, por passageiro, por voo e por status.
-- A função auxiliar 'printReserva' formata a exibição.

-- | Lista todas as reservas existentes.
acaoListarReservas :: Sistema -> IO ()
acaoListarReservas sys = do
  let rs = listarReservas sys
  if null rs
    then putStrLn "(vazio)"
    else mapM_ (printReserva sys) rs


-- | Lista reservas associadas a um determinado passageiro.
acaoListarReservasPorPassageiro :: Sistema -> IO ()
acaoListarReservasPorPassageiro sys = do
  putStr   "ID Passageiro: "
  sPid <- getLine
  case readIntSafe sPid of
    Just pid -> do
      let rs = listarReservasPorPassageiro pid sys
      if null rs
        then putStrLn "(vazio)"
        else mapM_ (printReserva sys) rs
    _ -> putStrLn "[ERRO] ID inválido."


-- | Lista reservas vinculadas a um voo específico.
acaoListarReservasPorVoo :: Sistema -> IO ()
acaoListarReservasPorVoo sys = do
  putStr   "ID Voo: "
  sVid <- getLine
  case readIntSafe sVid of
    Just vid -> do
      let rs = listarReservasPorVoo vid sys
      if null rs
        then putStrLn "(vazio)"
        else mapM_ (printReserva sys) rs
    _ -> putStrLn "[ERRO] ID inválido."


-- ===============================================================
--             LISTAGEM DE RESERVAS POR STATUS
-- ===============================================================
-- Este submenu permite filtrar as reservas por seu status:
-- Pendentes, Confirmadas ou Canceladas.
-- É útil para controle administrativo e geração de relatórios.
menuListarReservasPorStatus :: Sistema -> IO ()
menuListarReservasPorStatus sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Listar por Status ----"
  putStrLn "1) Pendentes"
  putStrLn "2) Confirmadas"
  putStrLn "3) Canceladas"
  putStrLn "4) Voltar"
  putStr   "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoListarReservasPorStatus Pendente sys >> menuListarReservasPorStatus sys
    "2" -> acaoListarReservasPorStatus Confirmada sys >> menuListarReservasPorStatus sys
    "3" -> acaoListarReservasPorStatus Cancelada sys >> menuListarReservasPorStatus sys
    "4" -> pure ()
    _   -> putStrLn "Opcao invalida." >> menuListarReservasPorStatus sys


-- | Lista reservas conforme o status informado.
acaoListarReservasPorStatus :: StatusReserva -> Sistema -> IO ()
acaoListarReservasPorStatus st sys = do
  let rs = listarReservasPorStatus st sys
  if null rs
    then putStrLn "(nenhuma reserva encontrada)"
    else do
      putStrLn ""
      putStrLn ("Encontradas " ++ show (length rs)
                ++ " reserva(s) com status " ++ show st ++ ":")
      mapM_ (printReserva sys) rs


-- ===============================================================
--             EXIBIÇÃO DETALHADA DE UMA RESERVA
-- ===============================================================
-- A função abaixo formata os dados de uma reserva de forma legível,
-- mostrando informações do passageiro, voo (rota e horário)
-- e o status atual.
printReserva :: Sistema -> Reserva -> IO ()
printReserva sys r = do
  let pn = maybe "(?)" nome (buscarPassageiroPorId (idPass r) sys)
      v  = buscarVooPorId (idVooRef r) sys
      rota = maybe "(voo?)"
                  (\vv -> origem vv ++ " -> " ++ destino vv ++ " @ " ++ horario vv)
                  v
  putStrLn $ "- ID " ++ show (idReserva r)
          ++ " | Passageiro: " ++ pn
          ++ " | Voo: " ++ rota
          ++ " | Status: " ++ show (status r)


-- ===============================================================
--                    SUBMENU: RELATÓRIOS
-- ===============================================================
-- Este submenu permite gerar relatórios administrativos sobre
-- os dados armazenados. Cada opção chama uma função do módulo
-- Relatorios.hs, que imprime as informações de forma formatada.
-- São apenas funções IO (sem alteração de estado do sistema).
menuRelatorios :: Sistema -> IO Sistema
menuRelatorios sys = do
  limparTela
  putStrLn "\n---- Relatórios ----"
  putStrLn "1) Estatísticas gerais"
  putStrLn "2) Passageiros"
  putStrLn "3) Companhias"
  putStrLn "4) Voos"
  putStrLn "5) Reservas"
  putStrLn "6) Voltar"
  putStr "Escolha: "
  op <- getLine
  case op of
    "1" -> imprimirRelatorioEstatisticasGerais sys >> menuRelatorios sys
    "2" -> imprimirRelatorioPassageiros sys >> menuRelatorios sys
    "3" -> imprimirRelatorioCompanhias sys >> menuRelatorios sys
    "4" -> imprimirRelatorioVoos sys >> menuRelatorios sys
    "5" -> imprimirRelatorioReservas sys >> menuRelatorios sys
    "6" -> return sys
    _   -> putStrLn "Opção inválida!" >> menuRelatorios sys


-- ===============================================================
--                    SUBMENU: PASSAGEIROS
-- ===============================================================
-- Menu para gerenciar passageiros (cadastrar, listar, etc)
menuPassageiros :: Sistema -> IO Sistema
menuPassageiros sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Passageiros ----"
  putStrLn "1) Cadastrar"
  putStrLn "2) Listar"
  putStrLn "3) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCadastrarPassageiro sys >>= menuPassageiros
    "2" -> acaoListarPassageiros sys >> menuPassageiros sys
    "3" -> return sys
    _   -> putStrLn "Opcao invalida." >> menuPassageiros sys

-- Cadastra um novo passageiro
acaoCadastrarPassageiro :: Sistema -> IO Sistema
acaoCadastrarPassageiro sys = do
  putStr "Nome: "
  nome <- getLine
  putStr "Documento: "
  doc <- getLine
  case inserirPassageiroDados nome doc sys of
    Left erro  -> putStrLn ("[ERRO] " ++ erro) >> return sys
    Right sys' -> putStrLn "[OK] Passageiro cadastrado." >> return sys'

-- Lista todos os passageiros
acaoListarPassageiros :: Sistema -> IO ()
acaoListarPassageiros sys = do
  let ps = listarPassageiros sys
  if null ps
    then putStrLn "(vazio)"
    else mapM_ printPassageiro ps
  where
    printPassageiro p = putStrLn $ "ID: " ++ show (idPassageiro p)
                                ++ " | Nome: " ++ nome p
                                ++ " | Doc: " ++ documento p


-- ===============================================================
--                    SUBMENU: COMPANHIAS
-- ===============================================================
-- Menu para gerenciar companhias aéreas
menuCompanhias :: Sistema -> IO Sistema
menuCompanhias sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Companhias ----"
  putStrLn "1) Cadastrar"
  putStrLn "2) Listar"
  putStrLn "3) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCadastrarCompanhia sys >>= menuCompanhias
    "2" -> acaoListarCompanhias sys >> menuCompanhias sys
    "3" -> return sys
    _   -> putStrLn "Opcao invalida." >> menuCompanhias sys

-- Cadastra uma nova companhia
acaoCadastrarCompanhia :: Sistema -> IO Sistema
acaoCadastrarCompanhia sys = do
  putStr "Nome da Companhia: "
  nome <- getLine
  case inserirCompanhiaNome nome sys of
    Left erro  -> putStrLn ("[ERRO] " ++ erro) >> return sys
    Right sys' -> putStrLn "[OK] Companhia cadastrada." >> return sys'

-- Lista todas as companhias
acaoListarCompanhias :: Sistema -> IO ()
acaoListarCompanhias sys = do
  let cs = listarCompanhias sys
  if null cs
    then putStrLn "(vazio)"
    else mapM_ printCompanhia cs
  where
    printCompanhia c = putStrLn $ "ID: " ++ show (idCompanhia c)
                                ++ " | Nome: " ++ nomeCompanhia c


-- ===============================================================
--                    SUBMENU: VOOS
-- ===============================================================
-- Menu para gerenciar voos
menuVoos :: Sistema -> IO Sistema
menuVoos sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Voos ----"
  putStrLn "1) Cadastrar"
  putStrLn "2) Listar"
  putStrLn "3) Buscar"
  putStrLn "4) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoCadastrarVoo sys >>= menuVoos
    "2" -> acaoListarVoos sys >> menuVoos sys
    "3" -> menuBuscarVoos sys >> menuVoos sys
    "4" -> return sys
    _   -> putStrLn "Opcao invalida." >> menuVoos sys

-- Cadastra um novo voo
acaoCadastrarVoo :: Sistema -> IO Sistema
acaoCadastrarVoo sys = do
  putStr "Origem: "
  origem <- getLine
  putStr "Destino: "
  destino <- getLine
  putStr "Horario: "
  horario <- getLine
  putStr "ID da Companhia: "
  idCompInput <- getLine
  case readIntSafe idCompInput of
    Just idComp ->
      case inserirVooDados origem destino horario idComp sys of
        Left erro  -> putStrLn ("[ERRO] " ++ erro) >> return sys
        Right sys' -> putStrLn "[OK] Voo cadastrado." >> return sys'
    Nothing -> putStrLn "[ERRO] ID invalido." >> return sys

-- Lista todos os voos
acaoListarVoos :: Sistema -> IO ()
acaoListarVoos sys = do
  let vs = listarVoos sys
  if null vs
    then putStrLn "(vazio)"
    else mapM_ (printVoo sys) vs

-- Menu de busca de voos
menuBuscarVoos :: Sistema -> IO ()
menuBuscarVoos sys = do
  putStrLn ""
  putStrLn "---- Buscar Voos ----"
  putStrLn "1) Por Origem"
  putStrLn "2) Por Destino"
  putStrLn "3) Por Rota (Origem + Destino)"
  putStrLn "4) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> do
      putStr "Origem: "
      origem <- getLine
      let vs = buscarVoosPorOrigem origem sys
      if null vs
        then putStrLn "(nenhum voo encontrado)"
        else mapM_ (printVoo sys) vs
      menuBuscarVoos sys
    "2" -> do
      putStr "Destino: "
      destino <- getLine
      let vs = buscarVoosPorDestino destino sys
      if null vs
        then putStrLn "(nenhum voo encontrado)"
        else mapM_ (printVoo sys) vs
      menuBuscarVoos sys
    "3" -> do
      putStr "Origem: "
      origem <- getLine
      putStr "Destino: "
      destino <- getLine
      let vs = buscarVoosPorRota origem destino sys
      if null vs
        then putStrLn "(nenhum voo encontrado)"
        else mapM_ (printVoo sys) vs
      menuBuscarVoos sys
    "4" -> return ()
    _   -> putStrLn "Opcao invalida." >> menuBuscarVoos sys

-- Imprime informações de um voo
printVoo :: Sistema -> Voo -> IO ()
printVoo sys v = do
  let compNome = case buscarCompanhiaPorId (idComp v) sys of
                   Just c  -> nomeCompanhia c
                   Nothing -> "(desconhecida)"
  putStrLn $ "ID: " ++ show (idVoo v)
          ++ " | " ++ origem v ++ " -> " ++ destino v
          ++ " | Horario: " ++ horario v
          ++ " | Cia: " ++ compNome


-- Menu para usuario comum atualizar seu proprio perfil
menuMinhaConta :: Usuario -> Sistema -> IO Sistema
menuMinhaConta usuario sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Minha Conta ----"
  putStrLn $ "Nome: " ++ nomeUsuario usuario
  putStrLn $ "Email: " ++ emailUsuario usuario
  putStrLn ""
  putStrLn "1) Atualizar Nome"
  putStrLn "2) Atualizar Email"
  putStrLn "3) Atualizar Senha"
  putStrLn "4) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoAtualizarNome usuario sys
    "2" -> acaoAtualizarEmail usuario sys
    "3" -> acaoAtualizarSenha usuario sys
    "4" -> return sys
    _   -> putStrLn "Opcao invalida." >> menuMinhaConta usuario sys

-- Atualiza o nome do usuário
acaoAtualizarNome :: Usuario -> Sistema -> IO Sistema
acaoAtualizarNome usuario sys = do
  putStr "Novo nome: "
  novoNome <- getLine
  case atualizarNomeUsuario (idUsuario usuario) novoNome sys of
    Left erro  -> do
      putStrLn $ "[ERRO] " ++ erro
      menuMinhaConta usuario sys
    Right sys' -> do
      putStrLn "[OK] Nome atualizado com sucesso."
      -- Busca o usuario atualizado para mostrar as mudanças
      case buscarUsuarioPorId (idUsuario usuario) sys' of
        Just usuarioAtualizado -> menuMinhaConta usuarioAtualizado sys'
        Nothing -> menuMinhaConta usuario sys'

-- Atualiza o email do usuário
acaoAtualizarEmail :: Usuario -> Sistema -> IO Sistema
acaoAtualizarEmail usuario sys = do
  putStr "Novo email: "
  novoEmail <- getLine
  case atualizarEmailUsuario (idUsuario usuario) novoEmail sys of
    Left erro  -> do
      putStrLn $ "[ERRO] " ++ erro
      menuMinhaConta usuario sys
    Right sys' -> do
      putStrLn "[OK] Email atualizado com sucesso."
      case buscarUsuarioPorId (idUsuario usuario) sys' of
        Just usuarioAtualizado -> menuMinhaConta usuarioAtualizado sys'
        Nothing -> menuMinhaConta usuario sys'

-- Atualiza a senha do usuário
acaoAtualizarSenha :: Usuario -> Sistema -> IO Sistema
acaoAtualizarSenha usuario sys = do
  putStr "Nova senha: "
  novaSenha <- getLine
  case atualizarSenhaUsuario (idUsuario usuario) novaSenha sys of
    Left erro  -> do
      putStrLn $ "[ERRO] " ++ erro
      menuMinhaConta usuario sys
    Right sys' -> do
      putStrLn "[OK] Senha atualizada com sucesso."
      menuMinhaConta usuario sys'


-- Menu para administrador deletar entidades do sistema
menuGerenciarSistema :: Sistema -> IO Sistema
menuGerenciarSistema sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Gerenciar Sistema (DELETE) ----"
  putStrLn "1) Deletar Usuario"
  putStrLn "2) Deletar Passageiro"
  putStrLn "3) Deletar Companhia"
  putStrLn "4) Deletar Voo"
  putStrLn "5) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoDeletarUsuario sys >>= menuGerenciarSistema
    "2" -> acaoDeletarPassageiro sys >>= menuGerenciarSistema
    "3" -> acaoDeletarCompanhia sys >>= menuGerenciarSistema
    "4" -> acaoDeletarVoo sys >>= menuGerenciarSistema
    "5" -> return sys
    _   -> putStrLn "Opcao invalida." >> menuGerenciarSistema sys

-- Deleta um usuário do sistema
acaoDeletarUsuario :: Sistema -> IO Sistema
acaoDeletarUsuario sys = do
  putStrLn ""
  putStrLn "=== Usuarios Cadastrados ==="
  let us = usuarios sys
  if null us
    then putStrLn "(vazio)"
    else mapM_ printUsuario us
  putStrLn ""
  putStr "ID do Usuario a deletar (0 para cancelar): "
  sUid <- getLine
  case readIntSafe sUid of
    Just 0   -> return sys
    Just uid ->
      case deletarUsuario uid sys of
        Left erro  -> putStrLn ("[ERRO] " ++ erro) >> return sys
        Right sys' -> putStrLn "[OK] Usuario deletado com sucesso." >> return sys'
    Nothing -> putStrLn "[ERRO] ID invalido." >> return sys
  where
    printUsuario u = putStrLn $ "ID: " ++ show (idUsuario u)
                              ++ " | Nome: " ++ nomeUsuario u
                              ++ " | Email: " ++ emailUsuario u
                              ++ " | Tipo: " ++ show (tipoUsuario u)

-- Deleta um passageiro do sistema
acaoDeletarPassageiro :: Sistema -> IO Sistema
acaoDeletarPassageiro sys = do
  putStrLn ""
  acaoListarPassageiros sys
  putStrLn ""
  putStr "ID do Passageiro a deletar (0 para cancelar): "
  sPid <- getLine
  case readIntSafe sPid of
    Just 0   -> return sys
    Just pid ->
      case deletarPassageiro pid sys of
        Left erro  -> putStrLn ("[ERRO] " ++ erro) >> return sys
        Right sys' -> putStrLn "[OK] Passageiro deletado com sucesso." >> return sys'
    Nothing -> putStrLn "[ERRO] ID invalido." >> return sys

-- Deleta uma companhia do sistema
acaoDeletarCompanhia :: Sistema -> IO Sistema
acaoDeletarCompanhia sys = do
  putStrLn ""
  acaoListarCompanhias sys
  putStrLn ""
  putStr "ID da Companhia a deletar (0 para cancelar): "
  sCid <- getLine
  case readIntSafe sCid of
    Just 0   -> return sys
    Just cid ->
      case deletarCompanhia cid sys of
        Left erro  -> putStrLn ("[ERRO] " ++ erro) >> return sys
        Right sys' -> putStrLn "[OK] Companhia deletada com sucesso." >> return sys'
    Nothing -> putStrLn "[ERRO] ID invalido." >> return sys

-- Deleta um voo do sistema
acaoDeletarVoo :: Sistema -> IO Sistema
acaoDeletarVoo sys = do
  putStrLn ""
  acaoListarVoos sys
  putStrLn ""
  putStr "ID do Voo a deletar (0 para cancelar): "
  sVid <- getLine
  case readIntSafe sVid of
    Just 0   -> return sys
    Just vid ->
      case deletarVoo vid sys of
        Left erro  -> putStrLn ("[ERRO] " ++ erro) >> return sys
        Right sys' -> putStrLn "[OK] Voo deletado com sucesso." >> return sys'
    Nothing -> putStrLn "[ERRO] ID invalido." >> return sys


-- Menu de passageiros para usuario comum (apenas listagem)
menuPassageirosComum :: Sistema -> IO Sistema
menuPassageirosComum sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Passageiros ----"
  acaoListarPassageiros sys
  putStrLn ""
  putStrLn "Pressione ENTER para voltar..."
  _ <- getLine
  return sys

-- Menu de companhias para usuario comum (apenas listagem)
menuCompanhiasComum :: Sistema -> IO Sistema
menuCompanhiasComum sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Companhias ----"
  acaoListarCompanhias sys
  putStrLn ""
  putStrLn "Pressione ENTER para voltar..."
  _ <- getLine
  return sys

-- Menu de voos para usuario comum (listagem e busca, sem cadastro)
menuVoosComum :: Sistema -> IO Sistema
menuVoosComum sys = do
  limparTela
  putStrLn ""
  putStrLn "---- Voos ----"
  putStrLn "1) Listar todos"
  putStrLn "2) Buscar"
  putStrLn "3) Voltar"
  putStr "Escolha: "
  opc <- getLine
  case opc of
    "1" -> acaoListarVoos sys >> putStrLn "" >> putStrLn "Pressione ENTER..." >> getLine >> menuVoosComum sys
    "2" -> menuBuscarVoos sys >> menuVoosComum sys
    "3" -> return sys
    _   -> putStrLn "Opcao invalida." >> menuVoosComum sys


