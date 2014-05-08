%%% server libraries
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
%%% json libraries
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
%%% process library
:- use_module(library(filesex)).
:- use_module(library(jpl)).

:- [relation].
:- ['patterns-stanford'].

% set up server
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- http_handler(root('info/name'), info_name, []).
info_name(_Request) :-
        format('Content-type: text/plain~n~n', []),
        format('Prolog-CoreNLP~n').

:- http_handler(root('info/description'), info_description, []).
info_description(_Request) :-
        format('Content-type: text/plain~n~n', []),
        format('Prolog Extraction from Stanford CoreNLP dependencies~n').

:- http_handler(root('info/version'), info_version, []).
info_version(_Request) :-
        format('Content-type: text/plain~n~n', []),
        format('2014-05-05~n').

:- http_handler(root(.), request, []).
request(Request) :- % json from decomposer
        memberchk(method(post), Request),
        catch( http_read_json(Request, Json), _Error, fail ), !,
        json_to_prolog(Json, Data),
        Data = json(Params),
        member(question=Question,Params),
        member(focus=Focus,Params),
        format('Content-type: text/plain~n~n', []),
        extract(Question,Focus).
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
        corenlp_jsonrpc(Text, Xml),
        turtle(Xml, Turtle),
        write_tmp_file(Turtle, File),
        rdf_load(File, [format(turtle)]),
        findall(_,relation,_),
        rdf_unload(File),
        delete_file(File).
% process string as question
extract(Text,Focus) :-
        corenlp_jsonrpc(Text, Xml),
        turtle(Xml, Turtle),
        write_tmp_file(Turtle, File),
        rdf_load(File, [format(turtle)]),
        findall(_,(question(Focus,Inf),write(Inf)),_),
        rdf_unload(File),
        delete_file(File).

% call corenlp jsonrpc service
corenlp_jsonrpc(Text, Xml) :-
        URL = 'http://extraction.allenai.org:9621',
        Command = json([jsonrpc='2.0', id=1, method=raw_parse, params=[Text]]),
        http_post(URL, json(Command), Json, []),
        atom_json_term(Json, JsonTerm, []),
        JsonTerm = json([jsonrpc=_, result=Xml, id=_]).

% convert corenlp output to turtle using xsltproc
turtle(Xml, Ttl) :-
        process_create(path('xsltproc'),
                       ['--stringparam', 'filename', '',
                        'CoreNLP-to-ttl.xsl',
                        '-'],
                       [stdin(pipe(Stdin)),
                        stdout(pipe(Stdout)) ]),
        write(Stdin, Xml),
        close(Stdin),
        read_stream_to_codes(Stdout,TtlCodes),
        atom_codes(Ttl,TtlCodes).

write_tmp_file(Text, File) :-
        tmp_file_stream(utf8, File, Stream),
        write(Stream, Text),
        close(Stream).
