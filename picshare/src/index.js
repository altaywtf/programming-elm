import "./main.css";
import { Elm } from "./Picshare.elm";
import * as serviceWorker from "./serviceWorker";

const app = Elm.Picshare.init({
  node: document.getElementById("root"),
});

function listen(url) {
  const socket = new WebSocket(url);

  socket.onmessage = function (event) {
    app.ports.receive.send(event.data);
  };
}

app.ports.listen.subscribe(listen);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
