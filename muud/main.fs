module Muud

open System.Net
open System.Net.Sockets


let handleClient (client : TcpClient) = async {
  printf "Handling client...\n"
  let stream = client.GetStream()
  let bufflen = 256
  let bytes : byte array = Array.zeroCreate bufflen
  let rec loop () = 
    printf "Getting input from client\n"
    let i = stream.Read(bytes, 0, bufflen)
    printf "Got %d bytes\n" i
    if i <> 0 then
      let str = System.Text.Encoding.ASCII.GetString(bytes, 0, i)
      printf "Got %s\n" str
      let returnMessage = Array.rev bytes.[0..i-1]
      stream.Write(returnMessage, 0, i)
      printf "Wrote message back: '%A'\n" returnMessage
      loop ()
    else
      // XXX: Detecting that the client has closed the
      // connection is a little opaque...
      printf "Client disconnected??? %A\n" stream.CanRead
  loop ()
  }

let rec serverLoop (listener:TcpListener) =
  printf "Waiting for connection\n"
  let client = listener.AcceptTcpClient ()
  printf "Got connection\n"
  let t = 
    handleClient client
    |> Async.StartChildAsTask
    |> Async.RunSynchronously
  serverLoop listener


let listen () =
  let address = IPAddress.Any
  let port = 9999
  let listener = new TcpListener(address, port)
  listener.Start()
  serverLoop listener 

let main () =
  printf "Hello world!\n"
  listen ()

main ()
