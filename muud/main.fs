module Muud

open System.Collections.Generic

open System
open System.Net
open System.Net.Sockets
open System.Threading.Tasks

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
      tc.MessageLoop this |> ignore
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
    for client in Clients do
      printf "Sending to client %A\n" client
      client.SendMessage str |> ignore
      printf "Sent to client %A\n" client


and TalkerClient (tcpclient:TcpClient) =
  let client = tcpclient
  let stream = tcpclient.GetStream ()
  let bufflen = 1024
  let buffer : byte array = Array.zeroCreate bufflen


  let handleMessage (this:TalkerClient) (server:TalkerServer) (task : Task<int>) = 
    let i = task.Result

    printf "Got %d bytes\n" i
    if i <> 0 then
      let str = System.Text.Encoding.ASCII.GetString(buffer, 0, i)
      server.ReceiveMessage str
      this.MessageLoop server
    else
      // XXX: Detecting that the client has closed the
      // connection is a little opaque...
      // This is entirely wrong, unfortunately.
      // Also the client stream needs to be closed.
      printf "Client disconnected??? %A %A %A\n" stream.CanRead client.Available client.Connected
      server.RemoveClient this  |> ignore

  member this.SendMessage (str:string) =
    let bytes : byte array = System.Text.Encoding.ASCII.GetBytes(str)
    stream.WriteAsync(bytes, 0, bytes.Length)

  member this.IsClientConnected () =
    true


  member this.MessageLoop (server:TalkerServer) = 
    let rec loop () = 
      printf "Getting input from client\n"
      //let i = stream.Read(bytes, 0, bufflen)
      let task = stream.ReadAsync(buffer, 0, bufflen)
      let cont : Action<Task<int>> = Action<Task<int>>(handleMessage this server)
      task.ContinueWith(cont)
    loop () |> ignore


let listen () =
  let address = IPAddress.Any
  let port = 9999
  let server = TalkerServer ()
  server.StartListening address port

let main () =
  printf "Hello world!\n"
  listen ()

main ()
