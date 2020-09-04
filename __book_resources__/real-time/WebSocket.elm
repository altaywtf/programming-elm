-- START:module
port module WebSocket exposing (listen, receive)
-- END:module


-- START:ports
port listen : String -> Cmd msg


port receive : (String -> msg) -> Sub msg
-- END:ports
