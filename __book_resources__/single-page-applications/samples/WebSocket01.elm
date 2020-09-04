port module WebSocket exposing (close, listen, receive)


port listen : String -> Cmd msg


-- START:close
port close : () -> Cmd msg
-- END:close


port receive : (String -> msg) -> Sub msg
