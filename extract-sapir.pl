%%% server libraries
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
%%% process library
:- use_module(library(filesex)).

:- [relation].
:- ['patterns-sapir'].

% set up server
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- http_handler(root('info/name'), info_name, []).
info_name(_Request) :-
        format('Content-type: text/plain~n~n', []),
        format('Prolog-Sapir~n').

:- http_handler(root('info/description'), info_description, []).
info_description(_Request) :-
        format('Content-type: text/plain~n~n', []),
        format('Prolog Extraction from Sapir dependencies~n').

:- http_handler(root('info/version'), info_version, []).
info_version(_Request) :-
        format('Content-type: text/plain~n~n', []),
        format('2014-03-14~n').

:- http_handler(root(.), request, []).
request(Request) :-
        memberchk(method(post), Request), !,
        http_read_data(Request, Data, [to(atom)]),
        format('Content-type: text/plain~n~n', []),
        extract(Data).
request(_Request) :-
        format('Content-type: text/plain~n~n', []),
        format('No sentence found in the input~n').

% process string
extract(Text) :-
        sapir(Text, Deps),
        turtle(Deps, Turtle),
        write_tmp_file(Turtle, File),
        rdf_load(File, [format(turtle)]),
        findall(_,relation,_),
        rdf_unload(File),
        delete_file(File).

% call sapir service
sapir(Text, Reply) :-
        URL = 'http://ari.allenai.org:8003/dep',
        http_post(URL, form([Text]), Reply, []).

% convert to turtle using script
turtle(Deps, Ttl) :-
        process_create(path(sh),
                       ['sapir2ttl.sh'],
                       [stdin(pipe(Stdin)),
                        stdout(pipe(Stdout)) ]),
        write(Stdin, Deps),
        close(Stdin),
        read_stream_to_codes(Stdout,TtlCodes),
        atom_codes(Ttl,TtlCodes).

write_tmp_file(Text, File) :-
        tmp_file_stream(utf8, File, Stream),
        write(Stream, Text),
        close(Stream).

