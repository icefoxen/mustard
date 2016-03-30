module Muud

open System.Collections.Generic

open System.Net
open System.Net.Sockets

// So let's make a proper talk server.
// What one client types out will be echoed
// to all of them.
//
// So 'handle client' involves async reading
// from the client until we get \n, then 
// giving that string to some handler function.
// The handler is then in charge of writing in
// out to everything.
// So we need some shared state.  It'll be 
// exciting to see how that jibes with threads.
// Firing an event saying "we got stuff" is also
// an option; we'll work on that later.
//
// 

type TalkerServer() = 
  let Clients = HashSet<TalkerClient> ()

  member this.StartListening address port =
    let rec loop (listener:TcpListener) =
      printf "Waiting for connection\n"
      let tcpclient = listener.AcceptTcpClient ()
      printf "Got connection\n"
      let tc = TalkerClient tcpclient
      this.AddClient tc |> ignore
      tc.MessageLoop this |> Async.Start
      loop listener

    let listener = TcpListener (address,port)
    listener.Start ()
    loop listener


  member this.AddClient c =
    printf "Adding client %A\n" c
    Clients.Add(c)

  member this.RemoveClient c =
    printf "Removing client %A\n" c
    Clients.Remove(c)

  member this.ReceiveMessage str =
    this.SendToClients str

  member this.SendToClients (str:string) =
    printf "Sending to clients: %A\n" str
    printf "We have %d clients\n" Clients.Count
    //let bytes = System.Text.Encoding.ASCII.GetBytes(str)
    for client in Clients do
      printf "Sending to client %A\n" client
      client.SendMessage str |> ignore
      //let stream = client.GetStream()
      //stream.Write(bytes, 0, bytes.Length)
      printf "Sent to client %A\n" client


and TalkerClient (tcpclient:TcpClient) =
  let client = tcpclient
  let stream = tcpclient.GetStream ()

  member this.SendMessage (str:string) =
    let bytes : byte array = System.Text.Encoding.ASCII.GetBytes(str)
    stream.WriteAsync(bytes, 0, bytes.Length)

  member this.IsClientConnected () =
    true

  member this.MessageLoop (server:TalkerServer) = async {
    let bufflen = 1024
    let bytes : byte array = Array.zeroCreate bufflen
    let rec loop () = 
      printf "Getting input from client\n"
      let i = stream.Read(bytes, 0, bufflen)
      printf "Got %d bytes\n" i
      if i <> 0 then
        let str = System.Text.Encoding.ASCII.GetString(bytes, 0, i)
        server.ReceiveMessage str
        //printf "Got %s\n" str
        //stream.Write(returnMessage, 0, i)
        //printf "Wrote message back: '%A'\n" returnMessage
        loop ()
      else
        // XXX: Detecting that the client has closed the
        // connection is a little opaque...
        // This is entirely wrong, unfortunately.
        // Also the client stream needs to be closed.
        printf "Client disconnected??? %A %A %A\n" stream.CanRead client.Available client.Connected
        server.RemoveClient this  |> ignore
    loop ()
    }


let rec serverLoop (server:TalkerServer) (listener:TcpListener) =
  printf "Waiting for connection\n"
  let tcpclient = listener.AcceptTcpClient ()
  printf "Got connection\n"
  let tc = new TalkerClient(tcpclient)
  server.AddClient tc |> ignore
  tc.MessageLoop server |> Async.Start
  serverLoop server listener


let listen () =
  let address = IPAddress.Any
  let port = 9999
  let server = TalkerServer ()
  server.StartListening address port

let main () =
  printf "Hello world!\n"
  listen ()

main ()
