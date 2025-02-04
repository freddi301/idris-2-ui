module Demo27

import Network.Socket

%default total


setupListenSocket : HasIO io => io (Maybe Socket)
setupListenSocket = do
  putStrLn "Creating listen socket"
  Right listen_socket <- socket AF_INET Stream 0
    | Left error => do
      putStrLn "Error creating server socket \{show error}"
      pure Nothing
  putStrLn "Created listen socket"
  putStrLn "Binding listen socket"
  0 <- bind listen_socket (Just $ Hostname "localhost") 3008
    | error => do
      putStrLn "Error binding server socket \{show error}"
      close listen_socket
      pure Nothing
  putStrLn "Bound listen socket"
  putStrLn "Listening on listen socket"
  0 <- listen listen_socket
    | error => do
      putStrLn "Error listening on server socket \{show error}"
      close listen_socket
      pure Nothing
  putStrLn "Listening on listen socket"
  pure $ Just listen_socket

answerRequest : HasIO io => Socket -> io ()
answerRequest listen_socket = do
  putStrLn "Accepting connection"
  Right (accept_socket, accept_socket_address) <- accept listen_socket
    | Left error => do
      putStrLn "Error accepting connection \{show error}"
      pure ()
  putStrLn "Accepted connection"
  putStrLn "Receiving request"
  Right (request, request_length) <- recv accept_socket 100
    | Left error => do
      putStrLn "Error receiving request \{show error}"
      close accept_socket
      pure ()
  putStrLn "Received request \{show request_length}"
  putStrLn request
  let content = "<h1>Hello</h1>"
  let response =
    """
    HTTP/1.1 200 OK
    Content-Type: text/html; charset=UTF-8
    Content-Length: \{show $ String.length content}

    \{content}
    """
  putStrLn "Sending response"
  Right result <- send accept_socket response
    | Left error => do
      putStrLn "Error sending response \{show error}"
      close accept_socket
      pure ()
  putStrLn "Response sent \{show result}"
  close accept_socket

covering
answerRequestRecursively : HasIO io => Socket -> io ()
answerRequestRecursively listen_socket = do
  answerRequest listen_socket
  answerRequestRecursively listen_socket

covering
main : IO ()
main = do
  putStrLn "Setting up listen socket"
  Just listen_socket <- setupListenSocket
    | Nothing => do
      putStrLn "Could not create listen socket"
      pure ()
  putStrLn "Setup listen socket successfull"
  answerRequest listen_socket
  close listen_socket


data Markup : (clientRequest : Type) -> (serverRequest : Type) -> Type where
  Text : String -> Markup clientRequest serverRequest
  Block : List (Markup clientRequest serverRequest) -> Markup clientRequest serverRequest
  Action : (Either clientRequest serverRequest) -> (Markup clientRequest serverRequest) -> Markup clientRequest serverRequest

interface Application (clientState : Type) (clientRequest : Type) (serverState : Type) (serverRequest : Type) where
  handleClient : clientState -> serverState -> clientRequest -> (clientState, Markup clientRequest serverRequest)
  handleServer : clientState -> serverState -> serverRequest -> (serverState, Markup clientRequest serverRequest)

record MyClientState where
  toBe : Int

record MyServerState where
  actual : Int

data MyClientRequest : Type where
  Inc : MyClientRequest
  Dec : MyClientRequest

data MyServerRequest : Type where
  Index : MyServerRequest
  Confirm : MyServerRequest

renderIndex : MyClientState -> MyServerState -> Markup MyClientRequest MyServerRequest
renderIndex clientState serverState =
  Block [
    Text "Actual: \{show serverState.actual}",
    Text "To be: \{show clientState.toBe}",
    Action (Left Inc) $ Text "Inc",
    Action (Left Dec) $ Text "Dec",
    Action (Right Confirm) $ Text "Confirm"
  ]

Application MyClientState MyClientRequest MyServerState MyServerRequest where
  handleClient clientState serverState Inc =
    let clientState = { toBe := clientState.toBe + 1 } clientState in
    (clientState, renderIndex clientState serverState)
  handleClient clientState serverState Dec =
    let clientState = { toBe := clientState.toBe - 1 } clientState in
    (clientState, renderIndex clientState serverState)
  handleServer clientState serverState Index =
    (serverState, renderIndex clientState serverState)
  handleServer clientState serverState Confirm =
    let serverState = { actual := clientState.toBe } serverState in
    (serverState, renderIndex clientState serverState)