port module WebSocket exposing (listen, receive)


port listen : String -> Cmd msg


port receive : (String -> msg) -> Sub msg
