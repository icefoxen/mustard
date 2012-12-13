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

(* The lone fd is the server socket *)
type connectionSet = Unix.file_descr * (Unix.file_descr list)


let newConnectionSet port =
   let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
   let addr = Unix.ADDR_INET( Unix.inet_addr_loopback, port ) in
   Unix.setsockopt s Unix.SO_REUSEADDR true;
   Unix.bind s addr;
   Unix.listen s 16;
   (s, [])
;;

let closeConnectionSet cs message =
   let s, c = cs in
   Unix.shutdown s Unix.SHUTDOWN_ALL;
   let f sock = 
      ignore (Unix.send sock message 0 (String.length message) []);
      Unix.shutdown sock Unix.SHUTDOWN_ALL;
   in
   List.iter f c
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
   let serversock, othersocks = cs in
   let readready, _, exceptready = 
      Unix.select (serversock :: othersocks) [] [] (-1.0) in
         (*(serversock :: othersocks) (-1.0) in*)

   let rec handleRead socks socklist reads =
      match socks with
         [] -> (socklist, reads)
       | hd :: tl when hd = serversock ->
             let ns, _ = Unix.accept hd in
             handleRead tl (ns :: socklist) reads
       | hd :: tl ->
             let buf = String.make buflen '\111' in
             let size = Unix.recv hd buf 0 buflen [] in
             if size = 0 then (* client has closed socket. *) (
                let activesocks = List.filter ((<>) hd) socklist in
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
