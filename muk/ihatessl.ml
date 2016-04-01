(* ALL RIGHT.  Let's abstract, bitches.
 * We're going to have a list of net objects.  Each one will have an index, a
 * network connection, and eventually a reference to the player/whatever that it
 * is connected to.
 * Roight.
 *
 * XXX: We might want to make the sockets non-blocking.  We might also need
 * either a way to register callbacks on socket operations (on_recv, on_close,
 * etc), or a way to check if a socket is still open.
 *)


let certfile = "cert.pem"
and privkey = "privkey.pem"
and passphrase = "@X!^kX*#MDmErkh"


let nconn = 16;;


type yetAnotherSocketType =
   PlainSock of Unix.file_descr
 | SSLSock of Ssl.socket
;;

(* Plaintext server socket, SSL socket, client sockets. *)
type connectionSet = Unix.file_descr * Ssl.socket * (yetAnotherSocketType list)

let getFileDescr = function
   PlainSock( fd ) -> fd
 | SSLSock( s ) -> Ssl.file_descr_of_socket s
;;


let newPlainServer port =
   let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
   let addr = Unix.ADDR_INET( Unix.inet_addr_loopback, port ) in
   Unix.setsockopt s Unix.SO_REUSEADDR true;
   Unix.bind s addr;
   Unix.listen s nconn;
   PlainSock( s )
;;

let newSSLServer port =
   let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
   let context = Ssl.create_context TLSv1 Server_context in
   Ssl.set_password_callback context (fun _ -> passphrase);;
   Ssl.use_certificate context certfile privkey;

   Unix.setsockopt s Unix.SO_REUSEADDR true;
   Unix.bind s addr;
   Unix.listen s nconn;

   SSLSock( Ssl.embed_socket s context )
;;

(* Returns "" when the socket is closed *)
let yetAnotherRead sock buflen =
   let buf = String.make buflen '\111' in
   match sock with
      PlainSock( fd ) ->
         let size = Unix.recv fd buf 0 buflen [] in
         String.sub buf 0 size
    | SSLSock( s ) -> 
         let size = Ssl.read s buf 0 buflen in
         String.sub buf 0 size
;;

let yetAnotherWrite sock str =
   match sock with
      PlainSock( fd ) ->
         Unix.send fd str 0 (String.length str) []
    | SSLSock( s ) ->
         Ssl.write s str 0 (String.length str)
;;

let yetAnotherClose c message =
   yetAnotherWrite s message;
   match s with
      PlainSock( fd ) ->
         Unix.shutdown fd Unix.SHUTDOWN_ALL;
    | SSLSock( s ) ->
         (* Should this be shutdown or shutdown_connection?  Who knows? *)
         Ssl.shutdown_connection s 
;;


let newConnectionSet port sslport =
   (newPlainServer port, newSSLServer sslport, [])
;;

let closeConnectionSet cs message =
   let ps, ssls, c = cs in
   Unix.shutdown s Unix.SHUTDOWN_ALL;
   Ssl.shutdown ssls;
   List.iter (fun x -> yetAnotherclose x message) c
;;

let closeConnection cs sock message =
   let s, c = cs in
   ignore (Unix.send sock message 0 (String.length message) []);
   Unix.shutdown sock Unix.SHUTDOWN_ALL;
   let activesocks = List.filter ((<>) sock) c in
   (s, activesocks)
;;


(* OKAY.  We now have the infrastructure to do all the shit we need.
 * This function takes a connection set and selects upon all the connections.
 * When it gets a new connection, it creates a new connection object in the
 * connection set.  When it gets new data on an existing connection, it
 * adds it to the list of returns.  If the client closes the socket, it 
 * removes it from the list.
 *
 * It returns a (connectionset, (file_descr, string) list) tuple, where the
 * connectionset is the new connection set (with new connections and such), and
 * the list is a list of (socket, message) pairs.
 * 
 * We might want to make the timeout on this non-infinate at some point,
 * so we can loop through and do other stuff while waiting for new network
 * thingies.
 *)


let doWonderfulThings cs buflen =
   let serversock, sslsock, othersocks = cs in
   let sockets = List.map getFileDescr (serversock :: sslsock :: othersocks) in
   let readready, _, exceptready = 
      (* timeout = 0? *)
      Unix.select sockets [] [] (0.0001) in

   let rec handleRead socks socklist reads =
      match socks with
         [] -> (socklist, reads)
       | hd :: tl when hd = List.nth sockets 0 ->
             let ns, _ = Unix.accept hd in
             handleRead tl (PlainSock( ns ) :: socklist) reads
       | hd :: tl when hd = List.nth sockets 1 ->
       | hd :: tl ->
             let result = yetAnotherRead hd buflen in
             if result = "" then (* client has closed socket. *) (
                let activesocks = List.filter ((<>) hd) socklist in
                yetAnotherClose 
                Unix.shutdown hd Unix.SHUTDOWN_ALL;
                handleRead tl activesocks reads
             )
             else 
                let msg = String.sub buf 0 size in
                handleRead tl socklist ((hd, msg) :: reads)
   in
   let socklist, reads = handleRead readready othersocks [] in

   (*
   Printf.printf "Returning %d reads\n" (List.length reads);
   if (List.length exceptready) > 0 then
      Printf.printf "Yow!  %d socket exceptions!\n" (List.length exceptready);
      *)

   ((serversock, socklist), reads)

;; 
