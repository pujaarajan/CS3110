open Order

(*************************************************************)
(* signature for the services we provide for the crawler     *)
(*************************************************************)
module type CRAWLER_SERVICES =
  sig
    (* links are used to describe a web address *)
    type link = { host : string ;  (* e.g., "www.cs.cornell.edu" *)
                  port : int ;     (* e.g., 80 *)
                  path : string    (* e.g., "/~clarkson/index.php" *)
                }

    val string_of_link : link -> string
    val href_of_link: link -> string
    val link_compare : link -> link -> order

    (* pages are used to describe the contents of web pages *)
    type page = { url : link ;          (* see above *)
                  links : link list ;   (* all of the links on the page *)
                  words : string list   (* all of the words on the page *)
                }

    val string_of_page : page -> string
    (* given the link, returns the page -- should specify what
     * exceptions get raised. *)
    val get_page : link -> page option

    (* the initial link to be used by the crawler *)
    val initial_link : link
    (* The root directory of the server. "" if crawling the web,
    (dirname initial_link) otherwise *)
    val root_dir : string
    (* the number of (distinct) pages the crawler should process *)
    val num_pages_to_search : int
    (* the port on which to listen for query requests *)
    val server_port : int
  end

(*************************************************************)
(* implementation of the crawler services                    *)
(*************************************************************)
module CrawlerServices : CRAWLER_SERVICES =
  struct
    (* set this flag to true to make the crawler really talk to
     * the internet.  Otherwise, it will treat the urls you give
     * it as local file paths and ignore the host and port. *)
    let crawl_internet = false

    (* links are used to describe a web address *)
    type link = { host : string ;  (* e.g., "www.cs.cornell.edu" *)
                  port : int ;     (* e.g., 80 *)
                  path : string    (* e.g., "/~clarkson/index.php" *)
                }

    let string_of_link l =
      if l.host = "" then "file://" ^ l.path
      else if l.port = 80 then
        l.host ^ ":" ^ l.path
      else
        l.host ^ ":" ^ (string_of_int l.port) ^ l.path


    let link_compare (x:link) (y:link) : order =
      match string_compare x.host y.host with
        | Eq ->
            (match int_compare x.port y.port with
               | Eq -> (match string_compare x.path y.path with
                          | Eq -> Eq
                          | ans -> ans)
               | ans -> ans)
        | ans -> ans


    (* pages are used to describe the contents of web pages *)
    type page = { url : link ;
                  links : link list ;
                  words : string list
                }

    let string_of_page p =
      (* truncate a string to at most 100 characters *)
      let trunc s =
        if String.length s > 100 then
          (String.sub s 0 100) ^ "..."
        else s in
      (* concatenate the links and truncate them *)
      let ls = trunc (String.concat ";"
                        (List.map string_of_link p.links)) in
      (* concatenate the words and truncate them *)
      let ws = trunc (String.concat ";" p.words) in
        "page(" ^ (string_of_link p.url) ^
          "; Links: " ^ ls ^ "; Words: " ^ ws ^ ")"

    (* parse a url, breaking it into a host, port number (default 80) and
     * path (default "") using a regular expression.
     *)
    let http_re =
      Str.regexp "http://\\([^/:]*\\)\\(:\\([0-9]+\\)\\)?\\(/\\([^#]*\\)\\)?"

    let parse_url url =
      if Str.string_match http_re url 0 then
        let host = Str.matched_group 1 url in
        let port =
          try int_of_string(Str.matched_group 3 url) with Not_found -> 80 in
        let init_path = try Str.matched_group 5 url with Not_found -> "" in
        let path = "/" ^ init_path in
          {host = host; port = port; path = path}
      else raise Not_found

    (* The response from the web-server will have a bunch of headers
     * on it separated from the actual data by two newlines (or two
     * carriage-returns/line-feeds.)  This finds those two spaces and
     * strips off all the headers. *)
    let strip_headers page =
      let rec find_two_newlines i =
        if i+2 < String.length page then
          match String.sub page i 2 with
            | "\n\n" -> Some (i+2)
            | "\r\n" ->
                if i+4 < String.length page then
                  (match String.sub page (i+2) 2 with
                     | "\r\n" -> Some (i+4)
                     | _ -> find_two_newlines (i+1))
                else None
            | _ -> find_two_newlines (i+1)
        else None
      in
        match find_two_newlines 0 with
          | None -> page
          | Some i -> String.sub page i (String.length page - i)


    let buf_len = 4096
    let buf = Bytes.create buf_len   (* yuck, not thread safe *)

    let rec receive_message fd contents =
      let len = Unix.recv fd buf 0 buf_len [] in
        if len = 0 then String.concat "" (List.rev contents)
        else receive_message fd ((String.sub buf 0 len)::contents)


    (* inet_fetch_url "www.cs.cornell.edu" 80 "/foo.html" should
     * return as a string the message response from the web server
     * www.cs.cornell.edu:80 for path "/foo.html".  This is using
     * the more expensive HTTP 1.0 style protocol where a socket is
     * opened and closed for each request.
     *)
    let inet_fetch_url link : string option =
      let host_addr =
        try Unix.inet_addr_of_string link.host
        with Failure _ ->
          (Unix.gethostbyname link.host).Unix.h_addr_list.(0) in
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let _ = Unix.connect fd (Unix.ADDR_INET (host_addr,link.port)) in
      let msg =
        "GET "^link.path^" HTTP/1.1\r\nHost: "^link.host^":"^
          (string_of_int link.port)^"Connection: close\r\n\r\n" in
      let i = Unix.send fd msg 0 (String.length msg) [] in
        if i = -1 then raise (Failure "Unix.send failed") else
          let result = receive_message fd [] in
            Unix.close fd ; Some (strip_headers result)

    (* file_fetch_url "www.cs.cornell.edu" 80 "/foo.html" should
     * return as a string the contents of the file in
     * "/foo.html".  *)
    let buf_len = 4096
    let buf = Bytes.create buf_len   (* yuck, not thread safe *)

    let file_fetch_url link : string option =
      let chan = open_in link.path in
      let rec receive_message chan contents =
        let len = input chan buf 0 buf_len in
          if len = 0 then String.concat "" (List.rev contents)
          else receive_message chan ((String.sub buf 0 len)::contents) in
      let result = receive_message chan [] in
        close_in chan ; Some result

    (* we return a blank page unless the link is a txt, text, htm, html,
     * ml, or mli file *)
    let valid_suffixes =
      [ ""; ".html"; ".html"; ".txt"; ".text"; ".ml"; ".mli" ]

    let fetch_url link : string option =
      try
        if (List.exists (Filename.check_suffix link.path)
              valid_suffixes) then
          if crawl_internet then inet_fetch_url link
          else file_fetch_url link
        else None
      with _ -> None

    let href_re = Str.regexp_case_fold "href *= *\"\\([^#\"]*\\)[^\"]*\""
    let absolute_path_re = Str.regexp "/[.]*"
    let javascript_re = Str.regexp "javascript:[.]*"
    let parent_path_re = Str.regexp "\\.\\./.*"

    (* Convert an href from the page into a link -- if the href
     * starts with "http" then parse it to get the host, port, and
     * path.  Otherwise, we assume it's on this host.  If there's
     * no leading "/" on the href, we prepend the source_link's
     * path.  I've filtered out the javascript links -- we may need to
     * filter out other links as well...
     *)
    let link_of_string source_link link_string =
      try Some (parse_url link_string)
      with Not_found ->
        if Str.string_match absolute_path_re link_string 0 then
          Some {host=source_link.host;
                port=source_link.port; path=link_string}
        else if Str.string_match javascript_re link_string 0 then
          None
        else
          (* need to watch out for "../path" -- should probably also
           * watch out for ./path. *)
          let rec glue_paths p s =
            if Str.string_match parent_path_re s 0 then
              glue_paths (Filename.dirname p)
                (String.sub s 3 (String.length s - 3))
            else p ^ "/" ^ s in
          let new_path =
            glue_paths (Filename.dirname source_link.path) link_string in
          Some {host=source_link.host; port=source_link.port;
                path=new_path}

    (* get_links page returns a list of URLs as links that occurred within
     * href's on the page.
     *)
    let get_links source_link page =
      let rec loop pos links =
        try
          let _ = Str.search_forward href_re page pos in
          let link_string = Str.matched_group 1 page in
          let newpos = Str.match_end() in
            match link_of_string source_link link_string with
              | None -> loop newpos links
              | Some l -> loop newpos (l::links)
        with
            Not_found -> List.rev links
      in
        loop 0 []

    (* get_words returns a list of words that occur on the page.
     * For our purposes, words only contain alphabetic characters.
     * If we try to use regexps here, we'll get a stack overflow,
     * so this is coded by hand.
     *)
    let get_words page =
      let len = String.length page in
      let rec loop pos words =
        let rec find_end i =
          if i < len then
            let c = String.get page (pos+i) in
              if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then
                find_end (i+1)
              else i
          else 0 in
          if pos < len then
            let e = find_end 0 in
              if e = 0 then loop (pos+1) words else
                loop (pos+e) ((String.sub page pos e)::words)
          else words
      in loop 0 []

    let get_page link =
      match fetch_url link with
          None -> None
        | Some page ->
            let links = get_links link page in
            let words = get_words page in
              Some {url = link ; links = links ; words = words}

    (* Read the command line arguments and return the
     * port number which Moogle should use for serving,
     * the number of pages to index, and the root url. *)
    let (server_port, num_pages_to_search, root_url) =
      let args = Sys.argv in
        try
          let port = int_of_string(Array.get args 1) in
          let num = int_of_string(Array.get args 2) in
          let root_url = Array.get args 3 in
            (port, num, root_url)
        with
            exn -> (Printf.printf
                      "usage: %s <port> <num-pages> <root-url>\n"
                      (Array.get args 0) ;
                    exit 1)

    let initial_link =
      try parse_url root_url
      with Not_found ->
        match link_of_string {host=""; port=80; path=""} root_url with
          | None ->
              (Printf.printf
                 "Please specify a url or path to a starting file\n" ;
               exit 1)
          | Some l -> l

    let root_dir =
      if initial_link.host = ""
      then Filename.dirname initial_link.path
      else ""

    (* Return a link that should work in an href.
     * If the link is a file in the root_dir, return just the
     * relative path.  *)
    let href_of_link l =
      let suffix str start =
        let len = String.length str in
        String.sub str start (len - start)
      in
      if l.host = "" then
        let root_regexp = Str.regexp_string root_dir in
          if Str.string_partial_match root_regexp l.path 0
          then suffix l.path (Str.match_end ())
          else "file://" ^ l.path
      else if l.port = 80 then
        l.host ^ ":" ^ l.path
      else
        l.host ^ ":" ^ (string_of_int l.port) ^ l.path
  end
