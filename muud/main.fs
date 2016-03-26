module Muud

open System.Net
open System.Net.Sockets

let listen () =
  let address = IPAddress.Any
  let port = 9999
  let listener = new TcpListener(address, port)
  listener.Start()

  let rec loop () =
    printf "Waiting for connection\n"
    let client = listener.AcceptTcpClient ()
    printf "Got connection\n"

  loop ()


let main () =
  printf "Hello world!\n"
  listen ()

main ()
