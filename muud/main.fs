module Muud

open System.Net
open System.Net.Sockets

let handleClient (client : TcpClient) =
  let stream = client.GetStream()
  let bufflen = 256
  let bytes : byte array = Array.zeroCreate bufflen
  let rec loop () =
    let i = stream.Read(bytes, 0, bufflen)
    if i = 0 then
      ()
    else
      let str = System.Text.Encoding.ASCII.GetString(bytes, 0, i)
      printf "Got %s\n" str
      let returnMessage = Array.rev bytes.[0..i-1]
      stream.Write(returnMessage, 0, i)
      printf "Wrote message back: '%A'\n" returnMessage
  loop ()


let listen () =
  let address = IPAddress.Any
  let port = 9999
  let listener = new TcpListener(address, port)
  listener.Start()

  let rec loop () =
    printf "Waiting for connection\n"
    let client = listener.AcceptTcpClient ()
    printf "Got connection\n"
    handleClient client

  loop ()


let main () =
  printf "Hello world!\n"
  listen ()

main ()